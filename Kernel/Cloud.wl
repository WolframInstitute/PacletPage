(* ::Package:: *)

(* ==================== the account and the site's objects ====================

   A site named name lives in the connected account as sibling objects:
     <name>          the shell, a public HTML page
     <name>Page      the host page the shell frames, public
     <name>Feedback/ the feedback form and the team's pages (Feedback.wl)
     <name>Source/   the authoring notebooks, when the site publishes them
     <name>SourceManifest   private: what <name>Source holds, by content hash
   Cleanup only ever deletes objects strictly beneath the directory it works on, so a sibling
   whose name starts with the same characters is never touched. *)

siteNameQ[name_String] := StringMatchQ[name, RegularExpression["[A-Za-z0-9_-]+(/[A-Za-z0-9_-]+)*"]];
siteNameQ[_] := False;

accountBase[] := First[$CloudRootDirectory];

(* the URL of the site name: a name in the connected account, or the URL of a site *)
siteURL[name_String] := If[StringStartsQ[name, "https://" | "http://"], StringTrim[name, "/"], accountBase[] <> "/" <> name];

(* the connected Wolfram ID; $WolframID is set once the session has made a cloud request *)
connectedAccount[] := (If[! StringQ[$WolframID], First[$CloudRootDirectory]]; $WolframID);

(* whether the connected account may publish: connected, known, and as account when one is named *)
publishingAccountQ[head_Symbol, account_] := Which[
    ! TrueQ[$CloudConnected] || ! StringQ[connectedAccount[]], Message[head::cloud]; False,
    StringQ[account] && ToLowerCase[account] =!= ToLowerCase[connectedAccount[]],
        Message[head::account, connectedAccount[], account]; False,
    True, True
];

strictlyBeneathQ[root_String, url_String] := StringStartsQ[url, StringTrim[root, "/"] <> "/"];

(* every object strictly beneath the cloud directory root, deleted; root itself stays *)
cleanBeneath[root_String] := Module[{objects},
    objects = If[TrueQ[DirectoryQ[CloudObject[root]]], CloudObjects[CloudObject[root]], {}];
    Scan[
        Function[object,
            If[ strictlyBeneathQ[root, First[object]],
                If[TrueQ[DirectoryQ[object]], cleanBeneath[First[object]]];
                DeleteObject[object]
            ]
        ],
        objects
    ]
];

(* text as an HTML page at url, UTF-8 and served as text/html *)
exportPage[html_String, url_String, permissions_] := CloudExport[html, {"Text", "text/html"}, CloudObject[url],
    CharacterEncoding -> "UTF-8", Permissions -> permissions];

(* ==================== the authoring notebooks ==================== *)

$sourceKinds = {{{"Guides"}, "guide"}, {{"ReferencePages", "Symbols"}, "ref"}, {{"Tutorials"}, "tutorial"}};

(* {file, "<kind>/<Name>.nb"} for each authoring notebook of a directory holding
   Documentation/English, or the English directory itself; one level deep per kind, since the
   site addresses pages by kind and name alone. A paclet directory's ResourceDefinition.nb is the
   source of the definition page, resource/<short name>. *)
sourceNotebooks[dir_String, short_String] := Join[
    sourceNotebooks[dir],
    With[{definition = FileNameJoin[{dir, "ResourceDefinition.nb"}]},
        If[FileExistsQ[definition], {{definition, "resource/" <> short <> ".nb"}}, {}]
    ]
];

sourceNotebooks[dir_String] := Module[{root},
    root = SelectFirst[{FileNameJoin[{dir, "Documentation", "English"}], dir},
        AnyTrue[$sourceKinds, Function[kind, DirectoryQ[FileNameJoin[Prepend[First[kind], #]]]]] &, None];
    If[ root === None, Return[{}, Module]];
    Catenate @ Map[
        Function[kind,
            With[{sub = FileNameJoin[Prepend[First[kind], root]]},
                If[DirectoryQ[sub], Map[{#, Last[kind] <> "/" <> FileNameTake[#]} &, FileNames["*.nb", sub]], {}]
            ]
        ],
        $sourceKinds
    ]
];

(* serially, or over the parallel kernels when every one acts as the account this kernel does: a
   parallel kernel is connected as the machine's saved login, which another session may have
   changed, and a write as another account is refused *)
mapBlocks[f_, list_List] := If[ Length[Kernels[]] > 0 && parallelSameAccountQ[],
    ParallelMap[f, list, DistributedContexts -> Automatic],
    Map[f, list]
];

(* the account a kernel acts as: $WolframID is None until the kernel's first cloud call *)
cloudAccount[] := (
    If[ TrueQ[$CloudConnected] && ! StringQ[$WolframID], CloudObject["x"]];
    If[ TrueQ[$CloudConnected] && StringQ[$WolframID], $WolframID, None]
);
kernelAccounts[] := If[ Length[Kernels[]] > 0,
    ParallelEvaluate[
        If[ TrueQ[$CloudConnected] && ! StringQ[$WolframID], CloudObject["x"]];
        If[ TrueQ[$CloudConnected] && StringQ[$WolframID], $WolframID, None]
    ],
    {}
];
sameAccountQ[accounts_List, account_String] := accounts =!= {} && AllTrue[accounts, # === account &];

(* whether the parallel kernels act as this kernel's account; when they do not, a monitored run
   says so, once for each set of accounts *)
$accountsNoted = {};
parallelSameAccountQ[] := With[{accounts = kernelAccounts[], account = cloudAccount[]},
    sameAccountQ[accounts, account] || (
        If[ ! MemberQ[$accountsNoted, {accounts, account}],
            AppendTo[$accountsNoted, {accounts, account}];
            monitor["cloud", "the parallel kernels act as " <> StringRiffle[ToString /@ DeleteDuplicates[accounts], ", "] <>
                ", not " <> ToString[account] <> ": cloud writes go from this kernel"]
        ];
        False
    )
];
sameAccountQ[_, _] := False;

(* Uploads the notebooks whose content changed since the manifest recorded them, deletes the ones
   no longer there, and records only what landed, so a failed upload is retried next time. The
   uploads go in blocks, with a line and the manifest written after each, so that a publish stopped
   midway uploads again only what it had not. *)
publishSource[dir_String, sourceURL_String, manifestURL_String, permissions_, short_String, all_ : False] := Module[
    {tasks, manifest, hashes, changed, retired, done = <||>, failed = {}, removed},
    tasks = sourceNotebooks[dir, short];
    If[ tasks === {}, Message[DeployPacletPage::source, dir]; Return[$Failed, Module]];
    manifest = If[ ! TrueQ[all] && TrueQ[FileExistsQ[CloudObject[manifestURL]]], CloudGet[CloudObject[manifestURL]], <||>];
    If[! AssociationQ[manifest], manifest = <||>];
    hashes = Association @ Map[Last[#] -> FileHash[First[#], "SHA256", "HexString"] &, tasks];
    changed = Select[tasks, Lookup[manifest, Last[#]] =!= hashes[Last[#]] &];
    retired = Complement[Keys[manifest], Keys[hashes]];
    monitor["site", "source notebooks: " <> ToString[Length[changed]] <> " of " <> ToString[Length[tasks]] <> " to upload"];
    Scan[
        Function[block, With[
            {results = Association @ mapBlocks[
                Function[task,
                    Last[task] -> MatchQ[
                        CopyFile[First[task], CloudObject[sourceURL <> "/" <> Last[task], Permissions -> permissions], OverwriteTarget -> True],
                        _CloudObject
                    ]
                ],
                block
            ]},
            failed = Join[failed, Keys[Select[results, ! TrueQ[#] &]]];
            done = Join[done, KeyTake[hashes, Keys[Select[results, TrueQ]]]];
            CloudPut[Join[KeyDrop[manifest, failed], done], CloudObject[manifestURL], Permissions -> "Private"];
            If[ Length[changed] > Length[block],
                monitor["site", "source notebooks: " <> ToString[Length[done] + Length[failed]] <> " of " <> ToString[Length[changed]] <> " uploaded"]]
        ]],
        Partition[changed, UpTo[50]]
    ];
    removed = Select[retired,
        Function[path,
            With[{url = sourceURL <> "/" <> path},
                strictlyBeneathQ[sourceURL, url] && (! TrueQ[FileExistsQ[CloudObject[url]]] || MatchQ[DeleteObject[CloudObject[url]], Null])
            ]
        ]
    ];
    CloudPut[
        Join[KeyDrop[manifest, Join[failed, removed]], KeyDrop[hashes, failed]],
        CloudObject[manifestURL],
        Permissions -> "Private"
    ];
    <|
        "URL" -> sourceURL,
        "Notebooks" -> Length[tasks],
        "Uploaded" -> Length[changed] - Length[failed],
        "Removed" -> Length[removed],
        "Failed" -> failed
    |>
];

(* ==================== publishing the site ==================== *)

Options[DeployPacletPage] = Join[
    {
        "Feedback" -> True,
        "FeedbackOptions" -> {},
        "Source" -> None,
        "Upload" -> Automatic,
        "Account" -> Automatic,
        "Monitor" -> False,
        Permissions -> "Public"
    },
    FilterRules[Options[PacletPageFiles], Except["SiteURL" | "PageURL" | "FeedbackURL" | "SourceURL"]]
];

DeployPacletPage::name = "`1` is not a site name: use letters, digits, \"_\" and \"-\", with \"/\" between directories.";

DeployPacletPage[catalog_, name_, opts : OptionsPattern[]] := Block[{$deployMonitor = TrueQ[$deployMonitor] || TrueQ[OptionValue["Monitor"]]}, Module[
    {site, page, feedback, source, files, result, title, mail, feedbackOptions},
    Which[
        ! catalogQ[catalog], Message[PacletPageFiles::catalog, catalog]; Return[$Failed, Module],
        ! siteNameQ[name], Message[DeployPacletPage::name, name]; Return[$Failed, Module],
        ! publishingAccountQ[DeployPacletPage, OptionValue["Account"]], Return[$Failed, Module]
    ];
    site = siteURL[name];
    page = site <> "Page";
    title = Replace[OptionValue["Title"], Automatic -> catalog["Title"]];
    {mail, feedbackOptions} = sharedFeedbackEmail[OptionValue["FeedbackEmail"], OptionValue["FeedbackOptions"]];
    feedback = If[ TrueQ[OptionValue["Feedback"]],
        DeployPacletPageFeedback[name, Sequence @@ feedbackOptions, "Title" -> title, "Account" -> OptionValue["Account"]],
        None
    ];
    If[ feedback === $Failed, Return[$Failed, Module]];
    source = Replace[OptionValue["Source"], {
        None -> None,
        dir_String :> publishSource[dir, site <> "Source", site <> "SourceManifest", OptionValue[Permissions], Last[StringSplit[catalog["Name"], "/"]], OptionValue["Upload"] === All],
        other_ :> (Message[DeployPacletPage::source, other]; $Failed)
    }];
    If[ source === $Failed, Return[$Failed, Module]];
    files = PacletPageFiles[catalog,
        "SiteURL" -> site,
        "PageURL" -> page,
        "FeedbackURL" -> If[AssociationQ[feedback], feedback["Form"], ""],
        "SourceURL" -> If[AssociationQ[source], source["URL"], ""],
        "FeedbackEmail" -> mail,
        Sequence @@ FilterRules[{opts}, Options[PacletPageFiles]]
    ];
    If[ ! AssociationQ[files], Return[$Failed, Module]];
    (* the host page first, so the shell never frames a page that is not there yet *)
    Do[
        result = exportPage[files[First[pair]], Last[pair], OptionValue[Permissions]];
        If[ ! MatchQ[result, _CloudObject], Message[DeployPacletPage::export, Last[pair], result]; Return[$Failed, Module]],
        {pair, {{"page.html", page}, {"index.html", site}}}
    ];
    <|
        "Site" -> site,
        "Page" -> page,
        "Feedback" -> feedback,
        "Source" -> source,
        "Pages" -> Length[catalog["Pages"]]
    |>
]];

(* One email setting: an address given to the site's email fallback ("FeedbackEmail") or to the
   feedback pages ("Email" of "FeedbackOptions") is the other's too *)
sharedFeedbackEmail[siteMail_, feedbackOptions_List] := With[
    {given = Lookup[Association[Cases[Flatten[feedbackOptions], _Rule | _RuleDelayed]], "Email", None]},
    {
        If[ MatchQ[siteMail, None | ""] && StringQ[feedbackAddress[given]], given, siteMail],
        If[ MatchQ[given, None | ""] && StringQ[feedbackAddress[siteMail]], Append[feedbackOptions, "Email" -> feedbackAddress[siteMail]], feedbackOptions]
    }
];

(* ==================== permissions ==================== *)

(* the directory tree beneath dir, dir first: one listing per directory, of its directories only *)
cloudDirectories[dir_CloudObject] := Prepend[Catenate[cloudDirectories /@ CloudObjects[dir, "Directory"]], dir];

Options[SetPacletPagePermissions] = {"Objects" -> "Directories"};

SetPacletPagePermissions[dir_String, permissions_, opts : OptionsPattern[]] :=
    SetPacletPagePermissions[CloudObject[dir], permissions, opts];

SetPacletPagePermissions[dir_CloudObject, permissions_, opts : OptionsPattern[]] := Module[{directories, objects, results},
    If[ ! TrueQ[DirectoryQ[dir]], Message[SetPacletPagePermissions::dir, First[dir]]; Return[$Failed, Module]];
    directories = cloudDirectories[dir];
    objects = If[ OptionValue["Objects"] === All,
        Join[directories, Catenate[Complement[CloudObjects[#], CloudObjects[#, "Directory"]] & /@ directories]],
        directories
    ];
    results = mapBlocks[First[#] -> MatchQ[SetPermissions[#, permissions], _CloudObject | {___}] &, objects];
    <|
        "Directories" -> Length[directories],
        "Objects" -> Length[objects],
        "Failed" -> Keys[Select[Association[results], ! TrueQ[#] &]]
    |>
];
