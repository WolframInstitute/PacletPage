(* ::Package:: *)

(* ==================== a site without the cloud ====================

   BuildPacletPageSite renders a paclet's documentation notebooks to HTML on this computer, as the
   documentation center shows them, and writes the site around them: the same shell, with its
   rails, its filter and its frame. Nothing is read from the cloud or written to it, so the site
   previews documentation before anything is published, and needs no account. *)

Options[BuildPacletPageSite] = {
    "Target" -> Automatic,
    "Pages" -> All,
    "UseCache" -> True,
    "Parallel" -> False,
    "TimeConstraint" -> 300,
    "Title" -> Automatic,
    "Subtitle" -> Automatic,
    "Accent" -> "#dd1100",
    "Filter" -> True,
    "SymbolsRail" -> True,
    "ResourceLink" -> True,
    "VersionLabel" -> False,
    "Footer" -> False,
    "LayoutControls" -> True,
    "DarkMode" -> Automatic,
    "SymbolIndex" -> Automatic,
    "Monitor" -> False
};

BuildPacletPageSite::dir = "`1` is not a paclet directory with documentation notebooks.";
BuildPacletPageSite::render = "`1` pages did not render: `2`.";
BuildPacletPageSite::pages = "`1` are not pages of the paclet's documentation.";

BuildPacletPageSite[dir_String, opts : OptionsPattern[]] := Block[{$deployMonitor = TrueQ[$deployMonitor] || TrueQ[OptionValue["Monitor"]]}, Module[
    {paclet, local, catalog, target, stage, cache, wanted, keys, cachedQ, hits, misses, results, failed, files},
    local = If[FileExistsQ[FileNameJoin[{dir, "PacletInfo.wl"}]] || FileExistsQ[FileNameJoin[{dir, "PacletInfo.m"}]], localDocumentation[dir], <||>];
    If[ local === <||>, Message[BuildPacletPageSite::dir, dir]; Return[$Failed, Module]];
    paclet = PacletObject[File[dir]];
    catalog = localCatalog[paclet["Name"], local];
    target = Replace[OptionValue["Target"], Automatic :> FileNameJoin[{dir, "build", "site"}]];
    stage = target <> ".new";
    cache = FileNameJoin[{dir, "build", "html"}];
    (* "Pages" -> {id, ...} renders those pages again and takes the rest from the cache *)
    wanted = Replace[OptionValue["Pages"], {All -> Keys[local], ids_List :> ids, id_String :> {id}}];
    If[ ! SubsetQ[Keys[local], wanted], Message[BuildPacletPageSite::pages, Complement[wanted, Keys[local]]]; Return[$Failed, Module]];
    If[ DirectoryQ[stage], DeleteDirectory[stage, DeleteContents -> True]];
    CreateDirectory[stage, CreateIntermediateDirectories -> True];
    If[ ! DirectoryQ[cache], CreateDirectory[cache, CreateIntermediateDirectories -> True]];
    keys = AssociationMap[renderKey[local[#]] &, Keys[local]];
    cachedQ = Function[id, TrueQ[OptionValue["UseCache"]] && DirectoryQ[FileNameJoin[{cache, keys[id]}]]];
    misses = If[OptionValue["Pages"] === All, Select[Keys[local], ! cachedQ[#] &], wanted];
    hits = Select[Complement[Keys[local], misses], cachedQ];
    (* the shared stylesheets, scripts and fonts come with any render: with every page cached, from
       the cache *)
    If[ misses === {} && ! DirectoryQ[FileNameJoin[{cache, "assets"}]], misses = Take[Keys[local], 1]; hits = Rest[hits]];
    monitor["site", ToString[Length[hits]] <> " pages rendered before, " <> ToString[Length[misses]] <> " to render"];
    results = renderPages[dir, paclet, KeyTake[local, misses], stage, OptionValue["Parallel"], OptionValue["TimeConstraint"]];
    failed = Keys[Select[results, ! TrueQ[#] &]];
    If[ failed =!= {}, Message[BuildPacletPageSite::render, Length[failed], Take[failed, UpTo[10]]]];
    (* each fresh render enters the cache, and every cached page joins the site *)
    Scan[storeRender[stage, #, FileNameJoin[{cache, keys[#]}]] &, Select[misses, TrueQ[results[#]] &]];
    Scan[takeRender[FileNameJoin[{cache, keys[#]}], stage, #] &, hits];
    If[ misses =!= {} && DirectoryQ[FileNameJoin[{stage, "common"}]],
        If[DirectoryQ[FileNameJoin[{cache, "assets"}]], DeleteDirectory[FileNameJoin[{cache, "assets"}], DeleteContents -> True]];
        CreateDirectory[FileNameJoin[{cache, "assets"}]];
        Scan[CopyDirectory[#, FileNameJoin[{cache, "assets", FileNameTake[#]}]] &, Select[FileNames["*", stage], DirectoryQ[#] && FileNameTake[#] =!= "html" &]],
        Scan[CopyDirectory[#, FileNameJoin[{stage, FileNameTake[#]}]] &, Select[FileNames["*", FileNameJoin[{cache, "assets"}]], DirectoryQ]]
    ];
    (* the renders that are no page of the paclet's any more leave the cache *)
    Scan[DeleteDirectory[#, DeleteContents -> True] &,
        Select[FileNames["*", cache], DirectoryQ[#] && FileNameTake[#] =!= "assets" && ! MemberQ[Values[keys], FileNameTake[#]] &]];
    siteLayout[stage];
    (* the paclet's version, which "VersionLabel" -> True shows *)
    files = PacletPageFiles[Append[catalog, "Version" -> paclet["Version"]], "PageURL" -> "",
        Sequence @@ (# -> OptionValue[#] & /@ {"Title", "Subtitle", "Accent", "Filter", "SymbolsRail", "ResourceLink", "VersionLabel", "Footer", "LayoutControls", "DarkMode", "SymbolIndex"})];
    If[ ! AssociationQ[files], Return[$Failed, Module]];
    Export[FileNameJoin[{stage, "index.html"}], files["index.html"], "Text", CharacterEncoding -> "UTF-8"];
    (* the site is replaced whole, so a server showing it never shows half a site *)
    If[ DirectoryQ[target], DeleteDirectory[target, DeleteContents -> True]];
    RenameDirectory[stage, target];
    <|
        "Site" -> FileNameJoin[{target, "index.html"}],
        "Pages" -> Length[local],
        "Rendered" -> Length[misses] - Length[failed],
        "Cached" -> Length[hits],
        "Failed" -> failed
    |>
]];

(* The catalog of a paclet's own documentation notebooks: each page rendered at
   Documentation/<kind>/<name>.html beside the site *)
localCatalog[name_String, local_Association] := Module[{short = Last[StringSplit[name, "/"]], joined = StringDelete[name, "/"], pages, root},
    pages = localPages[name, local];
    root = SelectFirst[
        Join[{"guide/" <> short, "guide/" <> joined}, SortBy[Select[Keys[pages], StringStartsQ[#, "guide/"] &], guideKey[pages[#]] &]],
        StringQ[#] && KeyExistsQ[pages, #] &,
        None
    ];
    <|
        "Name" -> name,
        "Title" -> If[StringQ[root], pages[root]["Title"], short],
        "Kind" -> "Local",
        "Resource" -> "",
        "DocBase" -> "Documentation/",
        "RootGuide" -> root,
        "Pages" -> pages
    |>
];

(* The catalog's pages, each read once: a page whose notebook has the size the catalog shipped
   beside the notebooks records takes the shipped entry, as a fresh install's pages do; one whose
   notebook has the size and date this user's cache records takes the cached entry; the others are
   read and enter the cache. So a paclet's catalog opens in about a second when its build shipped
   one, and after its first reading otherwise, in any kernel. *)
localPages[name_String, local_Association] := Module[
    {shipped = shippedCatalog[name, local], cacheFile = catalogCacheFile[name, local], cache, fresh = <||>, stamp, root, pages},
    cache = If[StringQ[cacheFile], Replace[readCatalogJSON[cacheFile], Except[_Association] -> <||>], <||>];
    root = If[local === <||>, None, documentationRoot[First[Values[local]]]];
    stamp[file_] := {FileByteCount[file], Round[10^6 AbsoluteTime[FileDate[file]]]};
    pages = Association @ KeyValueMap[
        Function[{id, file}, id -> With[{entry = If[StringQ[root], Lookup[shipped, relativePath[root, file], None], None]},
            If[ AssociationQ[entry] && entry["Size"] === FileByteCount[file] && AssociationQ[entry["Page"]], entry["Page"],
                With[{cached = Lookup[cache, file, None], now = stamp[file]},
                    If[ AssociationQ[cached] && cached["Stamp"] === now && AssociationQ[cached["Page"]], cached["Page"],
                        With[{page = localPage[name, id, file]}, fresh[file] = <|"Stamp" -> now, "Page" -> page|>; page]]]]]],
        local];
    If[ StringQ[cacheFile] && (fresh =!= <||> || Complement[Keys[cache], Values[local]] =!= {}),
        writeCatalogCache[cacheFile, Join[KeyTake[cache, Values[local]], fresh]]];
    pages
];

(* the form of a catalog's entries, which a shipped catalog and a cache are kept in: JSON, as reading
   it makes only data, where WXF evaluates the expressions it reads, and a shipped catalog comes with
   a paclet from anywhere *)
$catalogFormat = 2;
$shippedCatalogName = "PacletPageCatalog.json";

(* a catalog file's data, or None; a file broken by hand reports why, and the notebooks are read *)
readCatalogJSON[file_String] := If[FileExistsQ[file], Replace[Import[file, "RawJSON"], Except[_Association] -> None], None];

(* the documentation directory a notebook of it is in: the one above its Guides, Tutorials or
   ReferencePages *)
documentationRoot[file_String] := With[{parts = FileNameSplit[file]},
    Replace[Position[parts, "Guides" | "Tutorials" | "ReferencePages"], {{___, {n_}} :> FileNameJoin[Take[parts, n - 1]], _ -> None}]];

(* the catalog a build ships beside a paclet's notebooks, by each notebook's path there: its size and
   its entry, or none where it is of another paclet or form *)
shippedCatalog[name_String, local_Association] := With[{root = If[local === <||>, None, documentationRoot[First[Values[local]]]]},
    If[ ! StringQ[root] || ! FileExistsQ[FileNameJoin[{root, $shippedCatalogName}]], <||>,
        Replace[readCatalogJSON[FileNameJoin[{root, $shippedCatalogName}]], {
            shipped_Association /; Lookup[shipped, "Format", None] === $catalogFormat && Lookup[shipped, "Name", None] === name && AssociationQ[shipped["Pages"]] :> shipped["Pages"],
            _ -> <||>}]]];

(* The catalog of a built paclet's notebooks shipped beside them, so that an install of the paclet
   opens its catalog without reading them: the file written, or None. *)
shipCatalog[builtRoot_String, name_String] := Module[{local = localDocumentation[builtRoot], root},
    root = If[local === <||>, None, documentationRoot[First[Values[local]]]];
    If[ ! StringQ[root], Return[None, Module]];
    Export[FileNameJoin[{root, $shippedCatalogName}], <|"Format" -> $catalogFormat, "Name" -> name,
        "Pages" -> Association[KeyValueMap[relativePath[root, #2] -> <|"Size" -> FileByteCount[#2], "Page" -> localPage[name, #1, #2]|> &, local]]|>, "RawJSON", "Compact" -> True]
];

(* this user's cache of a paclet's catalog: one file for the paclet's name and its notebooks' directory *)
catalogCacheFile[name_String, local_Association] := If[ local === <||>, None,
    FileNameJoin[{$UserBaseDirectory, "ApplicationData", "WolframInstitute", "PacletPage", "Catalogs",
        StringReplace[name, "/" -> "__"] <> "-" <> StringTake[digest[{name, $catalogFormat, DirectoryName[DirectoryName[First[Values[local]]]]}], 16] <> ".json"}]
];

(* the cache written to a file of its own beside it and moved into place, so a kernel that reads it
   never reads half of it *)
writeCatalogCache[file_String, cache_Association] := With[{dir = DirectoryName[file], partial = file <> "." <> CreateUUID[] <> ".part"},
    If[ ! DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    If[ StringQ[Export[partial, cache, "RawJSON", "Compact" -> True]], RenameFile[partial, file, OverwriteTarget -> True]]
];

(* A page of the catalog. A guide's children and a tutorial's state are read from its cells; any
   other page, a symbol's above all, takes its title and keywords from its notebook's metadata, read
   without its cells, and is read whole only where the file holds no metadata to read. *)
localPage[name_String, id_String, file_String] := With[{meta = If[MemberQ[{"guide", "tutorial"}, pageKind[id]], None, fileMetadata[file]]},
    If[ AssociationQ[meta],
        <|"Kind" -> pageKind[id], "Name" -> pageName[id], "Title" -> Replace[meta["Title"], _Missing -> pageName[id]], "Keywords" -> meta["Keywords"],
          "Guides" -> {}, "HTML" -> "Documentation/" <> id <> ".html", "Notebook" -> ""|>,
        With[{nb = localNotebook[file]},
            Join[
                <|"Kind" -> pageKind[id], "Name" -> pageName[id], "Title" -> localTitle[nb, pageName[id]], "Keywords" -> {}, "Guides" -> {},
                  "HTML" -> "Documentation/" <> id <> ".html", "Notebook" -> ""|>,
                If[MatchQ[nb, _Notebook], <|"Keywords" -> notebookMetadata[nb]["Keywords"]|>, <||>],
                If[pageKind[id] === "tutorial", <|"Stub" -> placeholderQ[nb], "Overview" -> overviewNotebookQ[nb]|>, <||>],
                If[pageKind[id] === "guide" && MatchQ[nb, _Notebook], <|"Guides" -> DeleteCases[guideStructure[name, nb]["Children"], id]|>, <||>]
            ]
        ]
    ]
];

(* an authoring notebook's title: its symbol's name, the guide's or the tutorial's title *)
localTitle[nb_Notebook, name_String] := FirstCase[nb, Cell[c_, "ObjectName" | "GuideTitle" | "Title" | "TechNoteTitle", ___] :> cellText[c], name, Infinity];
localTitle[_, name_String] := name;

(* what a page's render is made from: its notebook, and the versions that render it, the PacletPage
   running rather than the newest installed *)
renderKey[file_String] := digest[{FileHash[file, "SHA256", "HexString"], $VersionNumber, $pacletPageVersion}];

(* where a page's render lands: html/<kind>/<name>.html, and its images in html/<kind>/Files/<name>.en *)
renderPage[stage_String, id_String] := FileNameJoin[{stage, "html", pageKind[id], pageName[id] <> ".html"}];
renderFiles[stage_String, id_String] := FileNameJoin[{stage, "html", pageKind[id], "Files", pageName[id] <> ".en"}];

mergeRender[from_String, stage_String, id_String] := (
    If[! DirectoryQ[DirectoryName[renderPage[stage, id]]], CreateDirectory[DirectoryName[renderPage[stage, id]], CreateIntermediateDirectories -> True]];
    CopyFile[renderPage[from, id], renderPage[stage, id], OverwriteTarget -> True];
    If[ DirectoryQ[renderFiles[from, id]], copyInto[renderFiles[from, id], renderFiles[stage, id]]];
);

(* a directory copied to to, replacing what is there, its parent made when it is missing *)
copyInto[from_String, to_String] := (
    If[DirectoryQ[to], DeleteDirectory[to, DeleteContents -> True]];
    If[! DirectoryQ[DirectoryName[to]], CreateDirectory[DirectoryName[to], CreateIntermediateDirectories -> True]];
    CopyDirectory[from, to]
);

storeRender[stage_String, id_String, entry_String] := (
    If[DirectoryQ[entry], DeleteDirectory[entry, DeleteContents -> True]];
    CreateDirectory[entry, CreateIntermediateDirectories -> True];
    CopyFile[renderPage[stage, id], FileNameJoin[{entry, "page.html"}]];
    If[DirectoryQ[renderFiles[stage, id]], CopyDirectory[renderFiles[stage, id], FileNameJoin[{entry, "Files"}]]];
);

takeRender[entry_String, stage_String, id_String] := (
    If[! DirectoryQ[DirectoryName[renderPage[stage, id]]], CreateDirectory[DirectoryName[renderPage[stage, id]], CreateIntermediateDirectories -> True]];
    CopyFile[FileNameJoin[{entry, "page.html"}], renderPage[stage, id], OverwriteTarget -> True];
    If[DirectoryQ[FileNameJoin[{entry, "Files"}]], copyInto[FileNameJoin[{entry, "Files"}], renderFiles[stage, id]]];
);

(* The pages rendered in separate kernel processes, each with a front end of its own and a time
   limit a page, and each into a directory of its own, merged into stage after: True or the
   failure, by page. A process records each page as it renders; one stopped before its end leaves
   its unrendered pages to one more process each. *)
renderPages[_, _, pages_Association, _, _, _] /; pages === <||> := <||>;
renderPages[dir_String, paclet_PacletObject, pages_Association, stage_String, parallel_, limit_] := Module[
    {n = Max[1, pageProcesses[parallel]], work = CreateDirectory[], runs, again, retries, results},
    runs = renderRuns[dir, paclet, pages, n, limit, work, "share"];
    runKernelScripts[runs, n, "site: render shares"];
    again = Catenate[Complement[#["Pages"], Keys[Replace[If[FileExistsQ[#["Record"]], Get[#["Record"]], <||>], Except[_Association] -> <||>]]] & /@ runs];
    retries = If[ again === {}, {},
        monitor["site", ToString[Length[again]] <> " pages again, in new processes"];
        renderRuns[dir, paclet, KeyTake[pages, again], n, limit, work, "again"]
    ];
    If[ retries =!= {}, runKernelScripts[retries, n, "site: pages again"]];
    runs = Join[runs, retries];
    (* each process's pages, from its own directory, and its shared files when stage has none yet *)
    results = Join @@ Map[
        Function[run, With[{record = Replace[If[FileExistsQ[run["Record"]], Get[run["Record"]], <||>], Except[_Association] -> <||>]},
            Scan[mergeRender[run["Out"], stage, #] &, Keys[Select[record, TrueQ]]];
            If[ DirectoryQ[run["Out"]],
                Scan[
                    If[ ! DirectoryQ[FileNameJoin[{stage, FileNameTake[#]}]], CopyDirectory[#, FileNameJoin[{stage, FileNameTake[#]}]]] &,
                    Select[FileNames["*", run["Out"]], DirectoryQ[#] && FileNameTake[#] =!= "html" &]
                ]
            ];
            record
        ]],
        runs
    ];
    DeleteDirectory[work, DeleteContents -> True];
    Join[AssociationMap[Missing["NotRendered"] &, Keys[pages]], KeyTake[results, Keys[pages]]]
];

(* the shares of pages and their processes, each share's record written as its pages render; a
   share whose record has not changed for a page's time and two minutes has stalled *)
renderRuns[dir_String, paclet_PacletObject, pages_Association, n_Integer, limit_, work_String, kind_String] := With[
    {shares = Partition[Keys[pages], UpTo[Max[1, Min[25, Ceiling[Length[pages] / (4 n)]]]]]},
    MapIndexed[
        Function[{ids, index}, With[{base = FileNameJoin[{work, kind <> "-" <> IntegerString[First[index]]}]},
            Export[base <> ".wl", renderShareScript[FileNameJoin[{dir, "Documentation"}], base, KeyTake[pages, ids], documentationLinkBase[paclet], limit, base <> ".record.wl", base <> ".log"], "Text"];
            <|"Script" -> base <> ".wl", "Record" -> base <> ".record.wl", "Out" -> base, "Pages" -> ids, "Deadline" -> limit * Length[ids] + 120, "Stall" -> limit + 120,
              "Label" -> "site: render " <> kind <> " " <> IntegerString[First[index]] <> " of " <> IntegerString[Length[shares]] <> " (" <> IntegerString[Length[ids]] <> " pages)"|>
        ]],
        shares
    ]
];

(* a process's script: each page rendered, as the documentation center shows it, within the limit,
   and whether it rendered recorded by page as soon as it has *)
renderShareScript[docsDir_String, out_String, pages_Association, linkBase_, limit_, recordOut_String, log_String] := StringJoin[
    "$ProgressReporting = False;\n",
    "$Output = $Messages = {OpenWrite[", wlString[log], "]};\n",
    "Needs[\"DocumentationBuild`\"];\n",
    "pages = <|", StringRiffle[KeyValueMap[wlString[#1] <> " -> " <> wlString[#2] &, pages], ", "], "|>;\n",
    "record = <||>;\n",
    "KeyValueMap[\n",
    "    (record[#1] = MatchQ[TimeConstrained[UsingFrontEnd @ DocumentationBuild`DocumentationBuildHTML[", wlString[docsDir], ", ", wlString[out], ",\n",
    "        \"SingleFile\" -> #2, \"CompleteHTMLQ\" -> True, \"LinkBase\" -> ", If[StringQ[linkBase], wlString[linkBase], "None"], "], ", ToString[limit], ", $TimedOut], _Success];\n",
    "     Put[record, ", wlString[recordOut], "]) &,\n",
    "    pages];\n",
    $endScriptCode
];

(* The pages under Documentation/, as the shell asks for them. The renders name a stylesheet
   directory the renderer does not write under that name, which it gets too; and a script of the
   renderer's bundle that holds lines of a build tool's log, which stop it running, is without
   them. *)
siteLayout[stage_String] := (
    Scan[repairScript, Select[FileNames["*.js", stage, Infinity], StringContainsQ[ReadString[#], "org.owasp.dependencycheck"] &]];
    If[ DirectoryQ[FileNameJoin[{stage, "html"}]],
        If[DirectoryQ[FileNameJoin[{stage, "Documentation"}]], DeleteDirectory[FileNameJoin[{stage, "Documentation"}], DeleteContents -> True]];
        RenameDirectory[FileNameJoin[{stage, "html"}], FileNameJoin[{stage, "Documentation"}]]
    ];
    With[{assets = FileNameJoin[{stage, "language-assets"}], named = FileNameJoin[{stage, "language-assets-v2"}]},
        If[DirectoryQ[assets] && ! DirectoryQ[named], CopyDirectory[assets, named]]
    ];
);

repairScript[file_String] := Export[file,
    StringReplace[ReadString[file], RegularExpression["(?m)^(\\d{4}-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d,\\d+ org\\.owasp\\..*|DEBUG - Exit code: \\d+)\\n?"] -> ""],
    "Text"
];
