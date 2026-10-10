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

Options[CheckPacletPage] = {"Pages" -> 10, "Feedback" -> True};

CheckPacletPage[catalog_, name_, opts : OptionsPattern[]] := Module[{site, page, results, ids, directory},
    Which[
        ! catalogQ[catalog], Message[PacletPageFiles::catalog, catalog]; Return[$Failed, Module],
        ! StringQ[name] || ! (siteNameQ[name] || StringStartsQ[name, "https://"]), Message[DeployPacletPage::name, name]; Return[$Failed, Module],
        ! StringStartsQ[name, "https://"] && ! TrueQ[$CloudConnected], Message[DeployPacletPage::cloud]; Return[$Failed, Module]
    ];
    site = siteURL[name];
    page = site <> "Page";
    results = {};

    (* the shell and the host page *)
    Scan[
        Function[url,
            With[{body = anonymousBody[url]},
                AppendTo[results, check[If[url === site, "Site", "Host page"], url,
                    StringQ[body] && StringContainsQ[body, "id=\"pp-config\""],
                    If[StringQ[body], "public, " <> IntegerString[StringLength[body]] <> " characters", "not readable anonymously"]]]
            ]
        ],
        {site, page}
    ];

    (* the notebooks the pages show, through the cloud's static render *)
    ids = checkedPages[catalog, OptionValue["Pages"]];
    Scan[
        Function[id,
            With[{status = anonymousStatus[staticRenderURL[catalog["Pages"][id]["Notebook"]]]},
                AppendTo[results, check["Page " <> id, catalog["Pages"][id]["Notebook"], status["Status"] === 200,
                    Replace[status["Status"], {200 -> "rendered", 403 -> "not public", 404 -> "not found: publish the site again from a new catalog", other_ :> "HTTP " <> TextString[other]}]]]
            ]
        ],
        ids
    ];

    (* a Paclet Repository page still shows the notebook the catalog names *)
    If[ catalog["Kind"] === "Repository" && KeyExistsQ[catalog["Pages"], catalog["RootGuide"]],
        With[{html = fetchText[catalog["Pages"][catalog["RootGuide"]]["HTML"]]},
            AppendTo[results, check["Catalog current", catalog["Pages"][catalog["RootGuide"]]["HTML"],
                StringQ[html] && repositoryNotebook[html] === catalog["Pages"][catalog["RootGuide"]]["Notebook"],
                If[StringQ[html] && repositoryNotebook[html] =!= catalog["Pages"][catalog["RootGuide"]]["Notebook"],
                    "the published notebooks changed: publish the site again from a new catalog", "the catalog matches the published pages"]]]
        ]
    ];

    (* the feedback: the form public, Me answering a stranger with null, the team pages closed *)
    If[ TrueQ[OptionValue["Feedback"]],
        directory = site <> "Feedback";
        With[{form = anonymousStatus[directory <> "/Form"], me = anonymousBody[directory <> "/Me"]},
            AppendTo[results, check["Feedback form", directory <> "/Form", form["Status"] === 200,
                If[form["Status"] === 200, "public", "HTTP " <> TextString[form["Status"]]]]];
            AppendTo[results, check["Feedback Me", directory <> "/Me", StringQ[me] && StringContainsQ[StringDelete[me, Whitespace], "\"email\":null"],
                If[StringQ[me], "answers " <> StringTrim[me], "not readable anonymously"]]]
        ];
        Scan[
            Function[endpoint,
                With[{status = anonymousStatus[directory <> "/" <> endpoint]},
                    AppendTo[results, check["Feedback " <> endpoint, directory <> "/" <> endpoint, status["Status"] =!= 200,
                        If[status["Status"] === 200, "OPEN to anonymous readers", "closed to anonymous readers (HTTP " <> TextString[status["Status"]] <> ")"]]]
                ]
            ],
            {"All", "File", "SetStatus"}
        ]
    ];

    <|"OK" -> AllTrue[results, #["OK"] &], "Site" -> site, "Checks" -> Dataset[results]|>
];
