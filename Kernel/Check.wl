(* ::Package:: *)

(* ==================== checking a published site ====================

   Everything is requested the way a reader's browser asks for it: anonymously, without the
   kernel's cloud session. A page whose notebook answers 403 is not public; one that answers 404
   is gone, which for a Paclet Repository site means the paclet was updated and the site has to
   be published again from a new catalog. *)

anonymousStatus[url_String] := With[{response = URLRead[HTTPRequest[url], {"StatusCode", "Headers"}, Authentication -> None, TimeConstraint -> 120]},
    If[ AssociationQ[response],
        <|"Status" -> response["StatusCode"], "Type" -> Lookup[toAssociation[response["Headers"]], "content-type", Lookup[toAssociation[response["Headers"]], "Content-Type", ""]]|>,
        <|"Status" -> Missing["NoResponse"], "Type" -> ""|>
    ]
];

anonymousBody[url_String] := With[{response = URLRead[HTTPRequest[url], {"StatusCode", "Body"}, Authentication -> None, TimeConstraint -> 120]},
    If[AssociationQ[response] && response["StatusCode"] === 200, response["Body"], Missing["NotRead", url]]
];

staticRenderURL[notebook_String] := StringReplace[notebook, "/obj/" -> "/statichtml/", 1] <> "?maxwaitmillis=8000&width=900";

check[what_String, url_String, ok_?BooleanQ, detail_] := <|"Check" -> what, "URL" -> url, "OK" -> ok, "Detail" -> detail|>;

(* the pages to check: the root guide, then the rest in catalog order, up to n *)
checkedPages[catalog_Association, All] := Keys[catalog["Pages"]];
checkedPages[catalog_Association, n_Integer] := Take[
    Select[DeleteDuplicates[Prepend[Keys[catalog["Pages"]], catalog["RootGuide"]]], KeyExistsQ[catalog["Pages"], #] &],
    UpTo[n]
];

(* Each check is a function and its arguments, so that a check that fails can run again: a site
   checked right after it is published may have pages the cloud has not rendered yet. *)

(* the shell and the host page *)
shellCheck[url_String, site_String] := With[{body = anonymousBody[url]},
    check[If[url === site, "Site", "Host page"], url,
        StringQ[body] && StringContainsQ[body, "id=\"pp-config\""],
        If[StringQ[body], "public, " <> IntegerString[StringLength[body]] <> " characters", "not readable anonymously"]]
];

(* the anonymous statuses of many URLs, sixteen at a time, with a line every ten blocks when monitored *)
anonymousStatuses[urls_List] := Module[{done = 0},
    Catenate @ Map[
        Function[block, With[{responses = URLRead[HTTPRequest /@ block, {"StatusCode", "Headers"}, Authentication -> None, TimeConstraint -> 120]},
            done += Length[block];
            If[ Length[urls] > 16 && (Mod[done, 160] < Length[block] || done === Length[urls]),
                monitor["check", "pages: " <> ToString[done] <> " of " <> ToString[Length[urls]] <> " requested"]];
            Map[responseStatus, If[ListQ[responses], responses, ConstantArray[responses, Length[block]]]]
        ]],
        Partition[urls, UpTo[16]]
    ]
];

responseStatus[response_Association] := <|"Status" -> response["StatusCode"],
    "Type" -> Lookup[toAssociation[response["Headers"]], "content-type", Lookup[toAssociation[response["Headers"]], "Content-Type", ""]]|>;
responseStatus[_] := <|"Status" -> Missing["NoResponse"], "Type" -> ""|>;

(* the checks run: the pages' requests together, the others one by one *)
runChecks[checks_List] := With[{urls = DeleteDuplicates[checkURL /@ Select[checks, batchedCheckQ]]},
    With[{statuses = AssociationThread[urls -> anonymousStatuses[urls]]},
        Map[If[batchedCheckQ[#], Append[#, statuses[checkURL[#]]], #] &, checks] // Map[runCheck]
    ]
];
batchedCheckQ[{pageCheck | htmlCheck, ___}] := True;
batchedCheckQ[_] := False;
checkURL[{pageCheck, _, notebook_String, ___}] := staticRenderURL[notebook];
checkURL[{htmlCheck, _, url_String, ___}] := url;

(* a notebook a page shows, through the cloud's static render *)
pageCheck[id_String, notebook_String] := pageCheck[id, notebook, anonymousStatus[staticRenderURL[notebook]]];
pageCheck[id_String, notebook_String, status_Association] := (
    check["Page " <> id, notebook, status["Status"] === 200,
        Replace[status["Status"], {200 -> "rendered", 403 -> "not public", 404 -> "not found: publish the site again from a new catalog", other_ :> "HTTP " <> TextString[other]}]]
);

(* the page a reader opens from the resource page or the repository *)
htmlCheck[id_String, url_String] := htmlCheck[id, url, anonymousStatus[url]];
htmlCheck[id_String, url_String, status_Association] :=
    check["Page HTML " <> id, url, status["Status"] === 200,
        Replace[status["Status"], {200 -> "served", 403 -> "not public", 404 -> "not found: deploy the resource again", other_ :> "HTTP " <> TextString[other]}]];

(* a Paclet Repository page still shows the notebook the catalog names *)
catalogCheck[url_String, notebook_String] := With[{html = fetchText[url]},
    check["Catalog current", url, StringQ[html] && repositoryNotebook[html] === notebook,
        Which[
            ! StringQ[html], "not readable",
            repositoryNotebook[html] === notebook, "the catalog matches the published pages",
            True, "the published notebooks changed: publish the site again from a new catalog"
        ]]
];

(* the feedback: the form public, Me answering a stranger with null, the team pages closed *)
formCheck[directory_String] := With[{form = anonymousStatus[directory <> "/Form"]},
    check["Feedback form", directory <> "/Form", form["Status"] === 200,
        If[form["Status"] === 200, "public", "HTTP " <> TextString[form["Status"]]]]
];
meCheck[directory_String] := With[{me = anonymousBody[directory <> "/Me"]},
    check["Feedback Me", directory <> "/Me", StringQ[me] && StringContainsQ[StringDelete[me, Whitespace], "\"email\":null"],
        If[StringQ[me], "answers " <> StringTrim[me], "not readable anonymously"]]
];
closedCheck[directory_String, endpoint_String] := With[{status = anonymousStatus[directory <> "/" <> endpoint]},
    check["Feedback " <> endpoint, directory <> "/" <> endpoint, status["Status"] =!= 200,
        If[status["Status"] === 200, "OPEN to anonymous readers", "closed to anonymous readers (HTTP " <> TextString[status["Status"]] <> ")"]]
];

(* An example entry sent as the site's Feedback panel sends it, with an image attached: the form
   answers with its thank-you page, and the entry and its attachment are stored. The entry is then
   set "not a defect", so the team's list keeps it out of the open ones. *)
sentCheck[name_String, directory_String, site_String] := Module[
    {marker = "An example entry sent by CheckPacletPage, " <> CreateUUID[], image, response, entry},
    image = FileNameJoin[{$TemporaryDirectory, CreateUUID["check-"] <> ".png"}];
    Export[image, Image[ConstantArray[{0.85, 0.2, 0.1}, {8, 8}]], "PNG"];
    response = URLRead[
        HTTPRequest[directory <> "/Form", <|"Method" -> "POST", "Headers" -> {"Referer" -> site <> "Page"},
            "Body" -> {"Feedback" -> marker, "Page" -> "check", "Title" -> "CheckPacletPage", "View" -> "check", "Attachment1" -> File[image]}|>],
        {"StatusCode", "Body"}, Authentication -> None, TimeConstraint -> 120];
    DeleteFile[image];
    entry = If[ AssociationQ[response] && response["StatusCode"] === 200 && StringContainsQ[response["Body"], "pp-feedback-thanks"],
        SelectFirst[Replace[Normal[PacletPageFeedback[name]], Except[_List] -> {}], Lookup[#, "Feedback", ""] === marker &, None],
        None
    ];
    If[ AssociationQ[entry], SetPacletPageFeedbackStatus[name, TextString[entry["ID"]], "not a defect", "an example entry sent by CheckPacletPage"]];
    check["Feedback sent", directory <> "/Form", AssociationQ[entry] && StringQ[entry["Attachments"]] && entry["Attachments"] =!= "",
        Which[
            ! AssociationQ[response], "no answer",
            response["StatusCode"] =!= 200 || ! StringContainsQ[response["Body"], "pp-feedback-thanks"], "the form did not thank: HTTP " <> TextString[response["StatusCode"]],
            ! AssociationQ[entry], "thanked, but the entry is not in the feedback",
            ! StringQ[entry["Attachments"]] || entry["Attachments"] === "", "stored without its attachment",
            True, "stored with its attachment, and set \"not a defect\""
        ]]
];

runCheck[{f_, arguments___}] := f[arguments];

waitSeconds[seconds_?NumericQ] := Max[0, seconds];
waitSeconds[time_Quantity] := Max[0, QuantityMagnitude[UnitConvert[time, "Seconds"]]];
waitSeconds[_] := 0;

Options[CheckPacletPage] = {"Pages" -> 10, "Feedback" -> True, "Wait" -> 0, "Monitor" -> False};

CheckPacletPage[catalog_, name_, opts : OptionsPattern[]] := Block[{$deployMonitor = TrueQ[$deployMonitor] || TrueQ[OptionValue["Monitor"]]}, Module[
    {site, directory, root, checks, results, deadline},
    Which[
        ! catalogQ[catalog], Message[PacletPageFiles::catalog, catalog]; Return[$Failed, Module],
        ! StringQ[name] || ! (siteNameQ[name] || StringStartsQ[name, "https://"]), Message[DeployPacletPage::name, name]; Return[$Failed, Module],
        ! StringStartsQ[name, "https://"] && ! TrueQ[$CloudConnected], Message[DeployPacletPage::cloud]; Return[$Failed, Module]
    ];
    site = siteURL[name];
    directory = site <> "Feedback";
    root = catalog["RootGuide"];
    checks = Join[
        {{shellCheck, site, site}, {shellCheck, site <> "Page", site}},
        {pageCheck, #, catalog["Pages"][#]["Notebook"]} & /@ checkedPages[catalog, OptionValue["Pages"]],
        Cases[checkedPages[catalog, OptionValue["Pages"]], id_ /; StringQ[catalog["Pages"][id]["HTML"]] :> {htmlCheck, id, catalog["Pages"][id]["HTML"]}],
        If[ catalog["Kind"] === "Repository" && KeyExistsQ[catalog["Pages"], root],
            {{catalogCheck, catalog["Pages"][root]["HTML"], catalog["Pages"][root]["Notebook"]}},
            {}
        ],
        If[ MatchQ[OptionValue["Feedback"], True | "Send"],
            Join[{{formCheck, directory}, {meCheck, directory}}, {closedCheck, directory, #} & /@ {"All", "File", "SetStatus"}],
            {}
        ],
        If[ OptionValue["Feedback"] === "Send", {{sentCheck, name, directory, site}}, {}]
    ];
    monitor["check", ToString[Length[checks]] <> " checks"];
    results = runChecks[checks];
    (* the checks that failed run again, every 10 seconds, until they pass or the wait is over *)
    deadline = AbsoluteTime[] + waitSeconds[OptionValue["Wait"]];
    While[ ! AllTrue[results, #["OK"] &] && AbsoluteTime[] < deadline,
        Pause[Min[10, deadline - AbsoluteTime[]]];
        With[{again = Flatten[Position[results, _?(! TrueQ[#["OK"]] &), {1}, Heads -> False]]},
            monitor["check", ToString[Length[again]] <> " checks again"];
            results[[again]] = runChecks[checks[[again]]]
        ]
    ];
    monitor["check", ToString[Count[results, _?(TrueQ[#["OK"]] &)]] <> " of " <> ToString[Length[results]] <> " passed"];
    <|"OK" -> AllTrue[results, #["OK"] &], "Site" -> site, "Failed" -> Select[results, ! #["OK"] &], "Checks" -> Dataset[results]|>
]];
