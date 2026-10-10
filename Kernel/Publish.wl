(* ::Package:: *)

(* ==================== building and publishing a paclet's documentation ====================

   One pipeline for every paclet, from markdown to a published site:
     BuildPacletDocumentation   docs/**/*.md -> Documentation/English authoring notebooks, with
                                MarkdownToNotebook; ResourceDefinition.md -> ResourceDefinition.nb
     DeployPacletResource       a fresh PacletBuild (which builds the documentation pages), checked
                                to hold every page, then the paclet resource deployed to the cloud
     SubmitPacletResource       the same fresh build, submitted to the Paclet Repository
     PublishPacletPage          the documentation built, the resource deployed, the site around it
                                published and checked, and the paclet submitted on request *)

(* ==================== the converter ==================== *)

$converterURL = "https://www.wolframcloud.com/obj/nikm/DeployedResources/Function/MarkdownToNotebook";

(* the AISkills checkout this paclet sits in, when it does: its converter and WolframParser *)
$checkoutDirectory = FileNameJoin[{ParentDirectory[$packageDirectory], "..", "..", ".."}];

(* The converter, as an association: "Function", the function to call; "Load", held code that
   gives it on a parallel kernel; "Distribute", held code naming what the parallel kernels get as
   distributed definitions, or None; "ID", what tells its versions apart in the page cache.
   Automatic is MarkdownToNotebook: the one loaded already, else the checkout's file (with the
   checkout's WolframParser registered first, for TeX), else the published function. The file
   defines its symbols in the current context, so it is read in Global`; the code that loads it
   names the function by its string, which keeps a parallel map from distributing it. *)
converterLoad[file_String] := With[{path = file},
    Hold[Block[{$Context = "Global`", $ContextPath = {"System`", "Global`"}}, Get[path]]; Symbol["Global`MarkdownToNotebook"]]
];

converterSpec[Automatic] := converterSpec[Automatic] = Which[
    DownValues[Global`MarkdownToNotebook] =!= {},
        <|
            "Function" -> Global`MarkdownToNotebook, "Load" -> Hold[Symbol["Global`MarkdownToNotebook"]],
            "Distribute" -> Hold[Global`MarkdownToNotebook],
            "ID" -> Hash[{DownValues[Global`MarkdownToNotebook], Options[Global`MarkdownToNotebook]}, "SHA256", "HexString"]
        |>,
    FileExistsQ[FileNameJoin[{$checkoutDirectory, "MarkdownToNotebook.wl"}]],
        If[ DirectoryQ[FileNameJoin[{$checkoutDirectory, "examples", "Paclet", "WolframParser"}]],
            PacletDirectoryLoad[FileNameJoin[{$checkoutDirectory, "examples", "Paclet", "WolframParser"}]]
        ];
        converterSpec[FileNameJoin[{$checkoutDirectory, "MarkdownToNotebook.wl"}]],
    True,
        converterSpec[$converterURL]
];
converterSpec[file_String] /; FileExistsQ[file] := converterSpec[file] = <|
    "Function" -> ReleaseHold[converterLoad[file]], "Load" -> converterLoad[file], "Distribute" -> None,
    "ID" -> FileHash[file, "SHA256", "HexString"]
|>;
(* a published function is used through its ResourceObject: a bare ResourceFunction[url] fails
   on parallel kernels *)
converterSpec[url_String] := converterSpec[url] = With[{u = url}, <|
    "Function" -> ResourceFunction[ResourceObject[u]], "Load" -> Hold[ResourceFunction[ResourceObject[u]]], "Distribute" -> None,
    "ID" -> u
|>];
converterSpec[f_] := <|"Function" -> f, "Load" -> Hold[f], "Distribute" -> Hold[f], "ID" -> Hash[f, "SHA256", "HexString"]|>;

converter[spec_] := converterSpec[spec]["Function"];

(* ==================== building ==================== *)

(* the directories of the documentation, under Documentation/English, by page template *)
$templateDirectories = <|
    "Symbol" -> {"ReferencePages", "Symbols"}, "Guide" -> {"Guides"}, "TechNote" -> {"Tutorials"},
    "Overview" -> {"Tutorials"}, "Format" -> {"ReferencePages", "Formats"},
    "ServiceConnection" -> {"ReferencePages", "Services"}, "Device" -> {"ReferencePages", "Devices"},
    "Interpreter" -> {"ReferencePages", "Interpreters"}, "Entity" -> {"ReferencePages", "Entities"},
    "Character" -> {"ReferencePages", "Characters"}, "Message" -> {"ReferencePages", "Messages"},
    "Program" -> {"ReferencePages", "Programs"}, "Workflow" -> {"Workflows"}, "WorkflowGuide" -> {"WorkflowGuides"}
|>;

(* the notebook a page's markdown becomes: its template's directory, named by the last segment of
   its URI (which DocumentationBuild resolves the page by), else by its file *)
pageNotebook[target_String, template_String, meta_Association, source_String] := FileNameJoin[Join[
    {target}, Lookup[$templateDirectories, template, {"Other"}],
    {Replace[Lookup[meta, "URI", None], {uri_String :> Last[StringSplit[uri, "/"]], _ :> FileBaseName[source]}] <> ".nb"}
]];

(* a markdown page's front matter, read without converting the page: the "Key: value" lines
   between its first two "---" lines, as the converter reads them *)
frontMatterRules[line_String] := StringCases[line,
    StartOfString ~~ WhitespaceCharacter ... ~~ key : (LetterCharacter ~~ WordCharacter ...) ~~ WhitespaceCharacter ... ~~ ":" ~~ value___ ~~ EndOfString :>
        (key -> StringTrim[StringTrim[value], "\"" | "'"])
];

frontMatter[file_String] := Module[{lines = StringSplit[Import[file, "Text", CharacterEncoding -> "UTF-8"], "\n"], close},
    If[ lines === {} || StringTrim[First[lines]] =!= "---", Return[<||>, Module]];
    close = SelectFirst[Range[2, Length[lines]], StringTrim[lines[[#]]] === "---" &, 0];
    If[ close === 0, <||>, Association[Flatten[frontMatterRules /@ lines[[2 ;; close - 1]]]]]
];

(* a file's path below a directory, which stays the same wherever the directory is checked out *)
relativePath[base_String, file_String] := With[{b = FileNameSplit[ExpandFileName[base]], f = FileNameSplit[ExpandFileName[file]]},
    If[ Length[f] > Length[b] && Take[f, Length[b]] === b, FileNameJoin[Drop[f, Length[b]]], file]
];

(* a hash of the paclet's code (its PacletInfo, Kernel/ and Assets/): the documentation's examples
   are evaluated afresh when it changes, since the converter caches each example's result by its
   code alone; the PacletInfo carries the version an example may show *)
codeHash[dir_String] := Hash[
    Map[
        {StringDrop[#, StringLength[dir]], FileHash[#, "SHA256"]} &,
        Sort @ Join[
            Select[FileNameJoin[{dir, #}] & /@ {"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ],
            Select[FileNames["*", Select[FileNameJoin[{dir, #}] & /@ {"Kernel", "Assets"}, DirectoryQ], Infinity], FileType[#] === File &]
        ]
    ],
    "SHA256", "HexString"
];
codeHash[dirs_List] := Hash[codeHash /@ dirs, "SHA256", "HexString"];

(* the paclet directories this kernel has loaded *)
pacletDirectories[] := Select[Replace[PacletDirectoryLoad[], Except[_List] -> {}], StringQ[#] && DirectoryQ[#] &];

(* the code a paclet's examples run: the paclet's own and its dependencies', given as directories
   or paclet names, by default the ones its PacletInfo declares, wherever they are found *)
dependencyName[name_String] := name;
dependencyName[name_String -> _] := name;
dependencyName[_] := Nothing;

dependencyDirectory[location_String] /; DirectoryQ[location] := location;
dependencyDirectory[name_String] := Replace[PacletFind[name], {{paclet_PacletObject, ___} :> paclet["Location"], _ -> Nothing}];

exampleCode[dir_String, dependencies_] := codeHash @ DeleteDuplicates @ Prepend[
    Map[dependencyDirectory, Replace[dependencies, {
        Automatic :> Map[dependencyName, Replace[PacletObject[File[dir]]["Dependencies"], Except[_List] -> {}]],
        Except[_List] -> {}
    }]],
    dir
];

(* the TeX parser the converter loads, which a parallel kernel loads from where this kernel found
   it: a parallel kernel's paclet catalog predates an install this kernel made *)
parserDirectories[] := Replace[PacletFind["Wolfram/Parser"], {{paclet_PacletObject, ___} :> {paclet["Location"]}, _ -> {}}];

(* The parallel kernels a build converts on: Automatic, the kernels launched already; True, as
   many as the processors and the memory allow (a kernel converting pages takes about a
   gigabyte); n, at least n. *)
buildKernels[False | None] := {};
buildKernels[Automatic] := Kernels[];
buildKernels[True] := buildKernels[Max[2, Min[$ProcessorCount, Floor[MemoryAvailable[] / 1.*^9]]]];
buildKernels[n_Integer] /; n < 2 := {};
buildKernels[n_Integer] := (If[ Length[Kernels[]] < n, LaunchKernels[n - Length[Kernels[]]]]; Kernels[]);

(* One page converted into its work file, under a time limit: "OK", "Result". A converter that
   wrote its notebook and then hung still counts, when the notebook it wrote is whole. *)
(* a converted page, with the sections the converter reports: a page that resets its context at
   each heading names, with MarkdownToNotebook::section, what a section uses from an earlier one,
   since those names come out unevaluated without a message of their own *)
convertPage[convert_, task_Association, limit_] := Module[{work = task["Work"], run, result},
    If[ FileExistsQ[work], DeleteFile[work]];
    run = EvaluationData[TimeConstrained[convert[task["File"], work, "UseCache" -> task["UseCache"], Sequence @@ task["Options"]], limit, $TimedOut]];
    result = run["Result"];
    <|
        "OK" -> Which[
            FailureQ[result], False,
            result === $TimedOut, FileExistsQ[work] && SyntaxQ[ReadString[work]],
            True, FileExistsQ[work]
        ],
        "Result" -> If[FailureQ[result] || result === $TimedOut, result, Null],
        "Unevaluated" -> sectionReports[run["MessagesExpressions"]]
    |>
];

sectionReports[messages_List] := Cases[messages,
    Hold[Message[MessageName[_, "section"], _, heading_String, names_List]] :> <|"Heading" -> heading, "Symbols" -> names|>];
sectionReports[_] := {};

convertHere[spec_Association, setup_Association][task_Association] := With[{done = convertPage[spec["Function"], task, setup["TimeConstraint"]]},
    If[ setup["Monitor"], Print[If[done["OK"], "converted ", "failed "], task["Page"]]];
    done
];

(* the conversion a parallel kernel runs, preparing the kernel the first time: a kernel the pool
   restarts after a crash keeps the distributed definitions, and nothing else *)
workerFunction[setup_Association] := With[
    {dirs = setup["Directories"], context = setup["Context"], load = setup["Load"], limit = setup["TimeConstraint"], monitor = setup["Monitor"]},
    Function[task,
        If[ ! TrueQ[$workerReady],
            Scan[PacletDirectoryLoad, dirs];
            If[ StringQ[context], Needs[context]];
            $workerConverter = ReleaseHold[load];
            $workerReady = True
        ];
        With[{done = convertPage[$workerConverter, task, limit]},
            If[ monitor, Print[If[done["OK"], "converted ", "failed "], task["Page"]]];
            done
        ]
    ]
];

(* The pages converted, in order: one by one in this kernel, or on the parallel kernels after the
   first, which this kernel converts so that whatever the converter installs is in place before
   they look for it; a page that fails on a parallel kernel is tried once more here. *)
convertTasks[{}, _, _, _] := {};
convertTasks[tasks_List, spec_, setup_, {}] := Map[convertHere[spec, setup], tasks];
convertTasks[{task_}, spec_, setup_, _] := {convertHere[spec, setup][task]};
convertTasks[tasks_List, spec_, setup_, kernels_List] := Module[{first, rest},
    first = convertHere[spec, setup][First[tasks]];
    DistributeDefinitions[convertPage];
    If[ MatchQ[spec["Distribute"], _Hold], DistributeDefinitions @@ spec["Distribute"]];
    rest = ParallelMap[
        workerFunction[Append[setup, "Directories" -> DeleteDuplicates[Join[setup["Directories"], parserDirectories[]]]]],
        Rest[tasks],
        Method -> "FinestGrained"
    ];
    rest = MapThread[
        If[ TrueQ[Lookup[Replace[#2, Except[_Association] -> <||>], "OK", False]], #2, convertHere[spec, setup][#1]] &,
        {Rest[tasks], rest}
    ];
    Prepend[rest, first]
];

(* the page cache's key: everything a page's notebook depends on. The guides' titles are in it
   because every page's Related Guides links show them. *)
$pageCacheVersion = "1";
pageKey[task_Association, converterID_, code_String, guides_List] := Hash[
    {$pageCacheVersion, task["Page"], FileHash[task["File"], "SHA256"], task["Options"], converterID, code, guides},
    "SHA256", "HexString"
];

(* "EvaluateSeparator": the converter's, one value for every page or an association by template;
   by default None for tutorials and Overview pages, whose later sections use what earlier ones
   define, and for every other page the converter's own default: each section, and each example
   between ---, in a clean context;
   "ConverterOptions": options for every MarkdownToNotebook call, such as "MathFont";
   "Files": All, or the markdown files (or a string pattern for their names) to build *)
(* a tutorial or an Overview page is one narrative: its later sections use what its earlier ones
   define, so it runs in one context *)
$separatorByTemplate = <|"TechNote" -> None, "Overview" -> None|>;

Options[BuildPacletDocumentation] = {
    "Source" -> Automatic,
    "Target" -> Automatic,
    "Files" -> All,
    "EvaluateSeparator" -> Automatic,
    "ConverterOptions" -> {},
    "UseCache" -> Automatic,
    "Definition" -> Automatic,
    "Converter" -> Automatic,
    "Prune" -> False,
    "Parallel" -> Automatic,
    "TimeConstraint" -> 1800,
    "CacheDirectory" -> Automatic,
    "Dependencies" -> Automatic,
    "Monitor" -> False
};

BuildPacletDocumentation::dir = "`1` is not a paclet directory: it has no PacletInfo.wl.";
BuildPacletDocumentation::source = "`1` has no markdown documentation.";
BuildPacletDocumentation::page = "`1` did not become a notebook: `2`.";
BuildPacletDocumentation::nodefinition = "No markdown definition was found, as ResourceDefinition.md in `1` or a Template: Paclet page under `2`, so `3` was not rebuilt; a deploy would publish it as it is. Give \"Definition\", or \"Definition\" -> None to build without one.";

BuildPacletDocumentation[dir_String, opts : OptionsPattern[]] := Module[
    {
        paclet, source, target, spec, cache, pagesDir, workDir, manifestFile, evaluated, code, useCache, separator,
        all, files, metas, guides, definitionSource, tasks, misses, kernelsBefore, kernels, setup, results, outcomes,
        ok, failed, pruned, pageFiles, skipped, unevaluated
    },
    If[ ! FileExistsQ[FileNameJoin[{dir, "PacletInfo.wl"}]] && ! FileExistsQ[FileNameJoin[{dir, "PacletInfo.m"}]],
        Message[BuildPacletDocumentation::dir, dir]; Return[$Failed, Module]];
    source = Replace[OptionValue["Source"], Automatic -> FileNameJoin[{dir, "docs"}]];
    target = Replace[OptionValue["Target"], Automatic -> FileNameJoin[{dir, "Documentation", "English"}]];
    all = Sort @ FileNames["*.md", source, Infinity];
    files = Sort @ Replace[OptionValue["Files"], {
        All :> all,
        list_List :> Select[list, FileExistsQ],
        pattern_ :> Select[all, StringMatchQ[FileNameTake[#], pattern] &]
    }];
    If[ files === {}, Message[BuildPacletDocumentation::source, source]; Return[$Failed, Module]];
    (* the paclet itself, for the examples *)
    PacletDirectoryLoad[dir];
    paclet = PacletObject[File[dir]];
    If[ StringQ[paclet["PrimaryContext"]], Needs[paclet["PrimaryContext"]]];
    spec = converterSpec[OptionValue["Converter"]];
    (* the cache: each page's notebook under a key of everything it depends on, and the code each
       page's examples were last evaluated with, since the converter caches an example's result by
       its code alone *)
    cache = Replace[OptionValue["CacheDirectory"], Automatic -> FileNameJoin[{dir, "build", "cache"}]];
    pagesDir = FileNameJoin[{cache, "pages"}];
    workDir = FileNameJoin[{cache, "work"}];
    If[ DirectoryQ[workDir], DeleteDirectory[workDir, DeleteContents -> True]];
    Scan[If[! DirectoryQ[#], CreateDirectory[#, CreateIntermediateDirectories -> True]] &, {pagesDir, workDir}];
    manifestFile = FileNameJoin[{cache, "evaluated.wl"}];
    evaluated = Replace[If[FileExistsQ[manifestFile], Get[manifestFile], <||>], Except[_Association] -> <||>];
    useCache = OptionValue["UseCache"] =!= False;
    code = exampleCode[dir, OptionValue["Dependencies"]];
    separator[template_] := Replace[OptionValue["EvaluateSeparator"], {
        Automatic :> Lookup[$separatorByTemplate, template, Automatic],
        byTemplate_Association :> Lookup[byTemplate, template, Lookup[$separatorByTemplate, template, Automatic]]
    }];
    metas = AssociationMap[frontMatter, Union[all, files]];
    (* a page is markdown whose front matter names a page template; anything else under the source,
       such as notes for the authors or the definition, is not one *)
    pageFiles = Select[files, KeyExistsQ[$templateDirectories, Lookup[metas[#], "Template", None]] &];
    skipped = Complement[files, pageFiles];
    guides = Sort @ KeyValueMap[
        If[ Lookup[#2, "Template", ""] === "Guide", {FileBaseName[#1], Lookup[#2, "Title", None]}, Nothing] &,
        KeyTake[metas, all]
    ];
    tasks = Map[
        Function[file, With[{template = Lookup[metas[file], "Template", "Default"]},
            <|
                "File" -> file, "Page" -> relativePath[source, file],
                "Out" -> pageNotebook[target, template, metas[file], file],
                "Options" -> Join[{"EvaluateSeparator" -> separator[template]}, OptionValue["ConverterOptions"]]
            |>
        ]],
        pageFiles
    ];
    (* the definition notebook the paclet resource is deployed from: by default from
       ResourceDefinition.md beside PacletInfo.wl, else in the source, else the source's
       Template: Paclet page *)
    definitionSource = Replace[OptionValue["Definition"], {
        Automatic :> SelectFirst[
            Join[
                {FileNameJoin[{dir, "ResourceDefinition.md"}], FileNameJoin[{source, "ResourceDefinition.md"}]},
                Select[all, Lookup[metas[#], "Template", None] === "Paclet" &]
            ],
            FileExistsQ,
            None
        ],
        None -> None
    }];
    If[ OptionValue["Definition"] === Automatic && definitionSource === None && FileExistsQ[FileNameJoin[{dir, "ResourceDefinition.nb"}]],
        Message[BuildPacletDocumentation::nodefinition, dir, source, FileNameJoin[{dir, "ResourceDefinition.nb"}]]
    ];
    If[ StringQ[definitionSource] && FileExistsQ[definitionSource],
        AppendTo[tasks, <|
            "File" -> definitionSource, "Page" -> "ResourceDefinition", "Out" -> FileNameJoin[{dir, "ResourceDefinition.nb"}],
            "Options" -> Join[{"EvaluateSeparator" -> separator["Paclet"]}, OptionValue["ConverterOptions"]]
        |>]
    ];
    tasks = Map[
        Function[task, With[{key = pageKey[task, spec["ID"], code, guides]},
            Join[task, <|
                "Entry" -> FileNameJoin[{pagesDir, key <> ".nb"}],
                "Work" -> FileNameJoin[{workDir, key <> ".nb"}],
                "Cached" -> useCache && FileExistsQ[FileNameJoin[{pagesDir, key <> ".nb"}]],
                "UseCache" -> useCache && Lookup[evaluated, task["Page"], None] === code
            |>]
        ]],
        tasks
    ];
    misses = Select[tasks, ! #Cached &];
    kernelsBefore = Kernels[];
    kernels = If[ Length[misses] > 1, buildKernels[OptionValue["Parallel"]], {}];
    setup = <|
        "Directories" -> DeleteDuplicates[Append[pacletDirectories[], dir]], "Context" -> paclet["PrimaryContext"], "Load" -> spec["Load"],
        "TimeConstraint" -> OptionValue["TimeConstraint"], "Monitor" -> TrueQ[OptionValue["Monitor"]]
    |>;
    results = AssociationThread[Lookup[misses, "Entry", {}], convertTasks[misses, spec, setup, kernels]];
    If[ kernels =!= {}, CloseKernels[Complement[Kernels[], kernelsBefore]]];
    (* each converted page enters the cache; every page that has its notebook is copied to it *)
    outcomes = Map[
        Function[task, Which[
            task["Cached"], "Cached",
            TrueQ[results[task["Entry"]]["OK"]],
                CopyFile[task["Work"], task["Entry"], OverwriteTarget -> True];
                "Converted",
            True,
                Message[BuildPacletDocumentation::page, task["File"], results[task["Entry"]]["Result"]];
                "Failed"
        ]],
        tasks
    ];
    ok = Pick[tasks, outcomes, "Cached" | "Converted"];
    Scan[
        Function[task,
            If[ ! DirectoryQ[DirectoryName[task["Out"]]], CreateDirectory[DirectoryName[task["Out"]], CreateIntermediateDirectories -> True]];
            CopyFile[task["Entry"], task["Out"], OverwriteTarget -> True]
        ],
        ok
    ];
    (* a page with names left unevaluated stays out of the cache, so the next build converts it
       again and reports it until it is fixed *)
    unevaluated = DeleteCases[
        Association @ Map[#["File"] -> Lookup[Lookup[results, #["Entry"], <||>], "Unevaluated", {}] &, Pick[tasks, outcomes, "Converted"]],
        {}
    ];
    Scan[If[KeyExistsQ[unevaluated, #["File"]] && FileExistsQ[#["Entry"]], DeleteFile[#["Entry"]]] &, Pick[tasks, outcomes, "Converted"]];
    evaluated = Join[evaluated, AssociationMap[code &, Lookup[ok, "Page", {}]]];
    failed = Lookup[Pick[tasks, outcomes, "Failed"], "File", {}];
    DeleteDirectory[workDir, DeleteContents -> True];
    (* after a build of every page, the cache keeps what this build used *)
    If[ OptionValue["Files"] === All,
        Scan[DeleteFile, Complement[FileNames["*.nb", pagesDir], Lookup[tasks, "Entry"]]];
        evaluated = KeyTake[evaluated, Lookup[tasks, "Page"]]
    ];
    Put[evaluated, manifestFile];
    (* the notebooks of pages whose markdown is gone: only after a whole build with no failure,
       since a page that did not convert keeps its last notebook *)
    pruned = If[ TrueQ[OptionValue["Prune"]] && OptionValue["Files"] === All && failed === {},
        With[{stale = Complement[FileNames["*.nb", target, Infinity], Lookup[Select[ok, #Page =!= "ResourceDefinition" &], "Out", {}]]},
            Scan[DeleteFile, stale]; stale],
        {}
    ];
    <|
        "Notebooks" -> Lookup[Select[ok, #Page =!= "ResourceDefinition" &], "Out", {}],
        "Definition" -> Replace[Lookup[Select[ok, #Page === "ResourceDefinition" &], "Out", {}], {{out_} :> out, _ -> None}],
        "Failed" -> failed,
        "Converted" -> Lookup[Pick[tasks, outcomes, "Converted"], "File", {}],
        "Cached" -> Lookup[Pick[tasks, outcomes, "Cached"], "File", {}],
        "Evaluated" -> Lookup[Select[Pick[tasks, outcomes, "Converted"], ! #UseCache &], "File", {}],
        "Skipped" -> skipped,
        "Pruned" -> pruned,
        "Kernels" -> Length[kernels],
        "Unevaluated" -> unevaluated
    |>
];

(* ==================== deploying the paclet resource ==================== *)

(* Fixes to the stock deploy (PacletResource`), installed once per session:
   - the resource page's Details section goes through htmlFragmentExport, which nothing defines;
   - some accounts' subscriptions reject the AppearanceElements option, whatever its value; a
     probe decides, the option is dropped where the cloud rejects it, and the deploy reports the
     answer as "AppearanceElements";
   - each notebook's render is warmed by a one-second request that a render rarely beats; only the
     main guide and the examples, which the resource page inlines, are warmed, until they render;
   - the resource page, main guide and example notebooks are deleted before a copy that
     overwrites them anyway; the deletions go, so they stay in place while they are replaced.
     The documentation directory is copied whole by a copy that cannot overwrite, so it is still
     deleted first. *)
$deployFixesInstalled = False;
$appearanceElements = None;

deployFunctions[] := {
    PacletResource`Private`cloudExportPacletResourceContent,
    PacletResource`Private`cloudDeployPacletWebFile,
    PacletResource`Private`cloudDeployNB
};

patchDeployFunctions[rule_, skip_ : Function[False]] := Scan[
    Function[f, Unprotect[f]; DownValues[f] = Map[If[TrueQ[skip[#]], #, # /. rule] &, DownValues[f]]],
    deployFunctions[]
];

(* one definition, guarded inside: a literal installDeployFixes[] := Null beside a conditional
   rule would always be tried first, and the fixes would never run *)
installDeployFixes[] := If[ ! TrueQ[$deployFixesInstalled], Module[{probe = CloudObject["PacletPageAppearanceProbe.html"]},
    Needs["PacletResource`"];
    ResourceSystemClient`Private`loadDeployResourceShingle[];
    If[ DownValues[DeployedResourceShingle`Private`htmlFragmentExport] === {} && OwnValues[DeployedResourceShingle`Private`htmlFragmentExport] === {},
        DeployedResourceShingle`Private`htmlFragmentExport[s_String] := s
    ];
    (* where the subscription refuses the option, the probe's deploy says so with
       CloudDeploy::appearancenotsup: that message is the probe's answer, not a failure, so it is
       kept off the output, the one purpose of this Quiet, and reported in the deploy's result *)
    $appearanceElements = Quiet[
        Check[MatchQ[CloudDeploy["probe", probe, AppearanceElements -> None], _CloudObject], False, CloudDeploy::appearancenotsup],
        CloudDeploy::appearancenotsup
    ];
    If[ ! $appearanceElements,
        Unprotect[PacletResource`Private`$embeddedOptions];
        PacletResource`Private`$embeddedOptions = Sequence[];
        patchDeployFunctions[HoldPattern[AppearanceElements -> _] :> Sequence[]]
    ];
    If[TrueQ[FileExistsQ[probe]], DeleteObject[probe]];
    If[ DownValues[PacletResource`Private`prefetchURL] =!= {},
        Unprotect[PacletResource`Private`prefetchURL];
        PacletResource`Private`prefetchURL[url_] := (
            If[ StringEndsQ[urlString[url], "MainGuidePage.nb" | "-ExampleNotebook_embed.nb"], warmStaticRender[urlString[url]]];
            URLBuild[URLParse[url]]
        )
    ];
    patchDeployFunctions[HoldPattern[Quiet[DeleteObject[_], ___]] :> Null, ! FreeQ[#, CopyDirectory] &];
    $deployFixesInstalled = True
]];

(* The resource page is a delayed page that inlines the static renders of the main guide and the
   examples, reading each with a one-second wait, and serves those parts empty, cached for ten
   minutes, until both read; then it replaces itself with the finished page. The stock deploy warms
   every notebook it deploys with a one-second request, which a render rarely beats and which costs
   a large paclet an hour; only these two are warmed, until their renders are there, and the page
   is requested once after the deploy, so that it finishes itself. *)
urlString[url_String] := url;
urlString[CloudObject[url_String, ___]] := url;
urlString[URL[url_String]] := url;
urlString[other_] := ToString[other];

warmStaticRender[url_String] := With[{static = PacletResource`Private`staticHTMLURL[url]},
    Do[
        If[ With[{response = URLRead[static, TimeConstraint -> 30]}, response["StatusCode"] === 200 && StringLength[response["Body"]] > 0], Break[]];
        Pause[3],
        {10}
    ]
];

(* The hero image of the resource page. The definition notebook's hero is the first output in its
   cell group tagged "HeroImage"; the headless scrape does not capture it, so the deploy attaches
   it as an image (a file path would resolve to nothing). *)
$heroImage = None;
$heroPatchInstalled = False;

installHeroPatch[] := If[ ! TrueQ[$heroPatchInstalled],
    Unprotect[PacletResource`Private`deployExtraPacletWebFiles];
    PrependTo[
        DownValues[PacletResource`Private`deployExtraPacletWebFiles],
        HoldPattern[PacletResource`Private`deployExtraPacletWebFiles[
            id_, as_?(AssociationQ[#] && KeyExistsQ[#, "PacletFile"] && ! KeyExistsQ[#, "HeroImage"] && $heroImage =!= None &), root_, opts___]] :>
            PacletResource`Private`deployExtraPacletWebFiles[id, Append[as, "HeroImage" -> $heroImage], root, opts]
    ];
    $heroPatchInstalled = True
];

heroImageQ[expr_] := ImageQ[expr] || MatchQ[expr, _Graphics | _Graphics3D];

definitionHero[definition_String] := Module[{group, boxes},
    group = FirstCase[Import[definition, "NB"],
        CellGroupData[{Cell[__, CellTags -> tags_ /; MemberQ[Flatten[{tags}], "HeroImage"], ___], rest___}, ___] :> {rest}, None, Infinity];
    boxes = If[group === None, None, FirstCase[group, Cell[BoxData[b_], "Output", ___] :> b, None, Infinity]];
    Replace[If[boxes === None, None, MakeExpression[boxes, StandardForm]], {HoldComplete[e_?heroImageQ] :> e, _ -> None}]
];

heroImage[Automatic, definition_String] := definitionHero[definition];
heroImage[None, _] := None;
heroImage[file_String, _] /; FileExistsQ[file] := Replace[Import[file], Except[_?heroImageQ] -> None];
heroImage[image_?heroImageQ, _] := image;
heroImage[_, _] := None;

(* the built archive, where PacletInstall can find it: <dir>/<Publisher>__<Name>.paclet, the
   latest, and <dir>/<Publisher>__<Name>-<version>.paclet *)
publishArchive[archive_String, dir_String, name_String, permissions_, commit_] := With[
    {latest = StringTrim[dir, "/"] <> "/" <> StringReplace[name, "/" -> "__"] <> ".paclet",
     versioned = StringTrim[dir, "/"] <> "/" <> FileNameTake[archive],
     meta = If[StringQ[commit], <|"Commit" -> commit|>, <||>]},
    (* the commit is set after the copy, which replacing an object does not carry over *)
    Map[
        With[{copied = CopyFile[archive, CloudObject[#, Permissions -> permissions], OverwriteTarget -> True]},
            If[MatchQ[copied, _CloudObject] && meta =!= <||>, SetOptions[copied, MetaInformation -> meta]];
            copied
        ] &,
        {versioned, latest}
    ] // Replace[{{v_CloudObject, l_CloudObject} :> <|"Latest" -> First[l], "Version" -> First[v]|>, other_ :> other}]
];

(* A dry run writes nothing to the cloud, so without a cloud session it runs offline: no account
   check, no version gate (which reads the published archive) and no deploy location. With a
   session it checks everything a deploy would. *)
offlineDryRunQ[dryRun_] := TrueQ[dryRun] && ! TrueQ[$CloudConnected];

(* the documentation pages a built paclet archive holds *)
archivePages[archive_String] := Select[Import[archive, {"ZIP", "FileNames"}], StringMatchQ[#, ___ ~~ "/Documentation/English/" ~~ ___ ~~ ".nb"] &];

(* ==================== checking the built archive ==================== *)

(* the paclet directories this kernel has loaded, the paclet's dependencies among them, which a
   fresh kernel loads too; never the paclet's own source, which would stand in for its archive *)
loadedPacletDirectories[source_String] := With[{name = pacletNames[source]},
    Select[pacletDirectories[], ExpandFileName[#] =!= ExpandFileName[source] && Intersection[pacletNames[#], name] === {} &]
];

(* the names of the paclets in a directory, or just beneath it, as PacletDirectoryLoad finds them;
   another copy of the paclet being built never reaches a fresh kernel, which would load it instead
   of the archive *)
pacletNames[dir_String] := DeleteDuplicates @ Map[
    PacletObject[File[DirectoryName[#]]]["Name"] &,
    FileNames["PacletInfo.wl" | "PacletInfo.m", dir, 2]
];

dependencyLoadCode[dirs_List] := StringJoin[Map["PacletDirectoryLoad[" <> wlString[#] <> "];\n" &, dirs]];

(* the kernel this one runs, for a fresh process *)
kernelExecutable[] := SelectFirst[
    FileNameJoin[Prepend[#, $InstallationDirectory]] & /@ {{"MacOS", "WolframKernel"}, {"Executables", "WolframKernel"}, {"Executables", "WolframKernel.exe"}},
    FileExistsQ, None];

(* The archive installed alone, in a fresh kernel: extracted to a temporary directory, its primary
   context loaded without a message, from there, and at the archive's version. The kernel that
   publishes has the source paclet loaded, so it cannot tell. *)
archiveLoadScript[archive_String, context_String, dependencies_List : {}] := StringJoin[
    "$ProgressReporting = False;\n",
    dependencyLoadCode[dependencies],
    "dir = CreateDirectory[];\n",
    "location = Replace[ExtractPacletArchive[", wlString[archive], ", dir], Except[_String?DirectoryQ] :> First[Select[FileNames[\"*\", dir], DirectoryQ], dir]];\n",
    "PacletDirectoryLoad[location];\n",
    "load = EvaluationData[Needs[", wlString[context], "]];\n",
    "file = FindFile[", wlString[context], "];\n",
    "report = <|\"Loaded\" -> MemberQ[$Packages, ", wlString[context], "], \"Messages\" -> load[\"MessagesText\"], ",
        "\"FromArchive\" -> StringQ[file] && StringStartsQ[file, location], \"Version\" -> PacletObject[File[location]][\"Version\"], ",
        "\"Symbols\" -> Length[Names[", wlString[context <> "*"], "]], ",
        "\"SymbolNames\" -> Sort[Map[", wlString[context], " <> Last[StringSplit[#, \"`\"]] &, Names[", wlString[context <> "*"], "]]]|>;\n",
    "WriteString[\"stdout\", \"PACLETPAGE-LOAD \" <> ExportString[report, \"RawJSON\", \"Compact\" -> True] <> \"\\n\"];\n",
    "DeleteDirectory[dir, DeleteContents -> True];\n"
];

archiveLoadCheck[archive_String, version_String, context_String, dependencies_List : {}] := Module[{kernel = kernelExecutable[], script, run, line, report},
    If[ kernel === None, Return[<|"OK" -> False, "Reason" -> "no kernel executable under " <> $InstallationDirectory|>, Module]];
    script = FileNameJoin[{$TemporaryDirectory, CreateUUID["pacletpage-load-"] <> ".wl"}];
    Export[script, archiveLoadScript[archive, context, dependencies], "Text"];
    run = RunProcess[{kernel, "-noprompt", "-script", script}];
    DeleteFile[script];
    line = If[AssociationQ[run], SelectFirst[StringSplit[run["StandardOutput"], "\n"], StringStartsQ[#, "PACLETPAGE-LOAD "] &, None], None];
    report = If[StringQ[line], ImportString[StringDrop[line, 16], "RawJSON"], <|"Reason" -> "the fresh kernel gave no report: " <> If[AssociationQ[run], StringTake[run["StandardError"] <> run["StandardOutput"], UpTo[500]], ToString[run]]|>];
    If[ ! AssociationQ[report], report = <|"Reason" -> "the fresh kernel's report could not be read"|>];
    (* a kernel may report two of its own packages defining one symbol (SystemSymbolQ::shdw in
       15.0); a shadowing message that names none of the paclet's contexts is not the paclet's *)
    If[ ListQ[report["Messages"]],
        report = Join[report, <|
            "Messages" -> Select[report["Messages"], ! foreignShadowQ[#, context] &],
            "IgnoredMessages" -> Select[report["Messages"], foreignShadowQ[#, context] &]
        |>]
    ];
    Append[report, "OK" -> And[TrueQ[report["Loaded"]], report["Messages"] === {}, TrueQ[report["FromArchive"]], report["Version"] === version]]
];

foreignShadowQ[text_String, context_String] := StringContainsQ[text, "::shdw"] && ! StringContainsQ[text, StringDrop[context, -1]];
foreignShadowQ[_, _] := False;

(* The commit a paclet directory is checked out at, or None outside a git repository. A paclet with
   changes not committed is marked +dirty, with a hash of its sources, so that two different
   uncommitted states of one version do not pass for the same release. *)
gitCommit[dir_String] := Module[{head, status},
    head = RunProcess[{"git", "-C", dir, "rev-parse", "HEAD"}];
    If[ ! AssociationQ[head] || head["ExitCode"] =!= 0, Return[None, Module]];
    status = RunProcess[{"git", "-C", dir, "status", "--porcelain", "--", "."}];
    StringTrim[head["StandardOutput"]] <> If[ AssociationQ[status] && StringTrim[status["StandardOutput"]] =!= "",
        "+dirty." <> StringTake[sourceHash[dir], 12],
        ""
    ]
];

(* a hash of a paclet's sources: every file in its directory but what a build makes *)
sourceHash[dir_String] := Hash[
    Map[
        {relativePath[dir, #], FileHash[#, "SHA256"]} &,
        Sort @ Select[FileNames["*", dir, Infinity],
            FileType[#] === File && ! StringMatchQ[relativePath[dir, #], ("build" | "Documentation" | ".git") ~~ $PathnameSeparator ~~ ___] &&
                ! StringEndsQ[#, ".paclet"] && FileNameTake[#] =!= "ResourceDefinition.nb" &]
    ],
    "SHA256", "HexString"
];

(* DocumentationBuild draws a PageDelimiter between a reference page's Details and its Examples,
   whose section draws its own top rule: a double line. The delimiter goes, the section's rule stays. *)
singleRuleBeforeExamples[Notebook[cells_List, opts___]] := Notebook[
    SequenceReplace[cells, {
        Cell[_, "PageDelimiter", ___],
        examples : (Cell[CellGroupData[{Cell[_, "PrimaryExamplesSection", ___], ___}, ___], ___] | Cell[_, "PrimaryExamplesSection", ___])
    } :> examples],
    opts
];
singleRuleBeforeExamples[other_] := other;

(* the built pages under a built paclet directory tidied in place, where they need it: the number
   tidied. A page tidied before has no delimiter left, and is only read. *)
tidyBuiltPages[builtRoot_String] := Count[
    Map[
        Function[file, StringContainsQ[ReadString[file], "\"PageDelimiter\""] && With[{notebook = Get[file]},
            With[{tidy = singleRuleBeforeExamples[notebook]}, tidy =!= notebook && StringQ[Export[file, tidy, "NB"]]]
        ]],
        FileNames["*.nb", FileNameJoin[{builtRoot, "Documentation"}], Infinity]
    ],
    True
];

(* PacletBuild in a fresh kernel: the build's own documentation step loads packages and issues
   messages that belong to it, not to the caller, and a fresh kernel builds what is on disk, not
   what the caller has loaded. Its documentation step is the stock one, followed by the tidying of
   the pages it built, before PacletBuild hashes them into the manifest and packs them. *)
pacletBuildScript[source_String, buildDir_String, dependencies_List : {}] := StringJoin[
    "$ProgressReporting = False;\n",
    dependencyLoadCode[dependencies],
    "PacletDirectoryLoad[", wlString[ParentDirectory[$packageDirectory]], "];\n",
    "Needs[\"WolframInstitute`PacletPage`\"];\n",
    "Needs[\"PacletTools`\"];\n",
    "handlers = PacletTools`$PacletExtensionHandlers;\n",
    "With[{stock = handlers[\"Documentation\", \"Build\"]},\n",
    "    handlers[\"Documentation\", \"Build\"] = Function[{extension, root, built, files},\n",
    "        With[{done = stock[extension, root, built, files]}, WolframInstitute`PacletPage`Private`tidyBuiltPages[built]; done]]];\n",
    "result = PacletTools`PacletBuild[", wlString[source], ", ", wlString[buildDir], ", PacletTools`PacletExtensionHandlers -> handlers];\n",
    "report = If[MatchQ[result, _Success], <|\"Archive\" -> result[\"PacletArchive\"]|>, <|\"Failure\" -> ToString[result, InputForm]|>];\n",
    "WriteString[\"stdout\", \"PACLETPAGE-BUILD \" <> ExportString[report, \"RawJSON\", \"Compact\" -> True] <> \"\\n\"];\n"
];

freshKernelBuild[source_String, buildDir_String, dependencies_List : {}] := Module[{kernel = kernelExecutable[], script, run, line},
    If[ kernel === None, Return[<|"Failure" -> "no kernel executable under " <> $InstallationDirectory|>, Module]];
    script = FileNameJoin[{$TemporaryDirectory, CreateUUID["pacletpage-build-"] <> ".wl"}];
    Export[script, pacletBuildScript[source, buildDir, dependencies], "Text"];
    run = RunProcess[{kernel, "-noprompt", "-script", script}];
    DeleteFile[script];
    line = If[AssociationQ[run], SelectFirst[StringSplit[run["StandardOutput"], "\n"], StringStartsQ[#, "PACLETPAGE-BUILD "] &, None], None];
    Replace[If[StringQ[line], ImportString[StringDrop[line, 17], "RawJSON"], None], Except[_Association] :>
        <|"Failure" -> "the build's kernel gave no report: " <> If[AssociationQ[run], StringTake[run["StandardError"] <> run["StandardOutput"], UpTo[800]], ToString[run]]|>]
];

(* the MetaInformation of a cloud object, None when there is no such object *)
cloudMeta[object_CloudObject] := If[ TrueQ[FileExistsQ[object]],
    Association[Select[Flatten[{Lookup[Options[object, MetaInformation], MetaInformation, {}]}], MatchQ[_Rule]]],
    None
];

(* the commit a published archive records, Missing when it records none, None when there is no
   such archive *)
publishedCommit[url_String] := Replace[cloudMeta[CloudObject[url]], meta_Association :> Lookup[meta, "Commit", Missing["NoCommit"]]];

(* the commit the deployed resource records for this version: None when no resource is deployed,
   or another version is, Missing when it records no commit for it *)
deployedCommit[location_CloudObject, version_String] := Replace[cloudMeta[location], {
    meta_Association /; Lookup[meta, "PacletVersion", None] === version :> Lookup[meta, "PacletCommit", Missing["NoCommit"]],
    _ -> None
}];

(* the release the deployed resource records, beside the MetaInformation its deploy writes *)
recordRelease[location_CloudObject, version_String, commit_] := With[
    {meta = Select[Flatten[{Lookup[Options[location, MetaInformation], MetaInformation, {}]}], MatchQ[_Rule]]},
    SetOptions[location, MetaInformation -> Join[
        DeleteCases[meta, ("PacletVersion" | "PacletCommit") -> _],
        {"PacletVersion" -> version},
        If[StringQ[commit], {"PacletCommit" -> commit}, {}]
    ]]
];

(* The version gate: a version already published from another commit, or from no recorded commit,
   is refused, since what installs as that version would silently change. True when the release
   may go ahead. *)
releaseAllowedQ[head_Symbol, what_String, published_, commit_] := Which[
    published === None, True,
    MissingQ[published], Message[head::nocommit, what]; False,
    commit === None, Message[head::unknowncommit, what, published]; False,
    published =!= commit, Message[head::commit, what, published, commit]; False,
    True, True
];

(* ==================== the documentation pages, built on parallel kernels ==================== *)

(* PacletBuild keeps the pages it builds in build/<Name>/Documentation, and a record of each one's
   authoring notebook in build/incremental/<Name>; it builds only the pages whose authoring
   notebook is new, changed, failed before, or lost its page. *)
builtPagesRecord[buildDir_String, prefix_String] := FileNameJoin[{buildDir, "incremental", prefix, "DocumentationBuildNotebooksIncrementalData.wl"}];

readRecord[file_String] := Replace[If[FileExistsQ[file], Get[file], <||>], Except[_Association] -> <||>];

(* the authoring notebooks of a documentation directory, by their path below it *)
authoringPages[docsDir_String] := Map[relativePath[docsDir, #] &, FileNames["*.nb", FileNameJoin[{docsDir, "English"}], Infinity]];

(* the pages PacletBuild would build *)
changedPages[docsDir_String, builtDocsDir_String, record_Association] := Select[authoringPages[docsDir],
    Function[page, With[{entry = Lookup[record, page, None]},
        ! AssociationQ[entry] || FailureQ[Lookup[entry, "Result", $Failed]] ||
            Lookup[entry, "FileHash", None] =!= FileHash[FileNameJoin[{docsDir, page}], "SHA256", "Base64Encoding"] ||
            ! FileExistsQ[FileNameJoin[{builtDocsDir, page}]]
    ]]
];

(* the built pages whose authoring notebook is gone, which PacletBuild would pack still *)
pruneBuiltPages[docsDir_String, builtDocsDir_String, recordFile_String] := Module[{stale},
    stale = Select[
        Map[relativePath[builtDocsDir, #] &, FileNames["*.nb", FileNameJoin[{builtDocsDir, "English"}], Infinity]],
        ! FileExistsQ[FileNameJoin[{docsDir, #}]] &
    ];
    Scan[DeleteFile[FileNameJoin[{builtDocsDir, #}]] &, stale];
    If[ stale =!= {} && FileExistsQ[recordFile], Put[KeyDrop[readRecord[recordFile], stale], recordFile]];
    stale
];

documentationLinkBase[paclet_PacletObject] := Replace[
    Cases[Replace[paclet["Extensions"], Except[_List] -> {}], {"Documentation", rules___} :> Lookup[Association[Cases[{rules}, _Rule]], "LinkBase", Missing[]]],
    {{base_String, ___} :> base, _ :> paclet["Name"]}
];

(* The script a page-building process runs: the stock documentation build over its share of the
   changed pages, without the search indexes, writing the record entries PacletBuild keeps for them
   to a file. Its output goes to a log, so that nothing waits on a full pipe. *)
pageShareScript[source_String, docsDir_String, builtDocsDir_String, pages_List, recordOut_String, log_String, dependencies_List] := StringJoin[
    "$ProgressReporting = False;\n",
    "$Output = $Messages = {OpenWrite[", wlString[log], "]};\n",
    dependencyLoadCode[dependencies],
    "PacletDirectoryLoad[", wlString[source], "];\n",
    "Needs[\"PacletTools`\"];\n",
    "Needs[\"DocumentationBuild`\"];\n",
    "pages = {", StringRiffle[wlString /@ pages, ", "], "};\n",
    "paclet = PacletObject[File[", wlString[source], "]];\n",
    "inputDir = ExpandFileName[", wlString[docsDir], "];\n",
    "result = UsingFrontEnd @ DocumentationBuild`DocumentationBuildNotebooks[inputDir, ", wlString[builtDocsDir], ", MemberQ[pages, #] &,\n",
    "    \"BuildSearchIndex\" -> False, \"SuccessThreshold\" -> -1, \"LinkBase\" -> ", wlString[documentationLinkBase[PacletObject[File[source]]]], ",\n",
    "    \"CitationFunction\" -> PacletTools`PacletDocumentationBuild`Private`makePacletCitationFunction[paclet],\n",
    "    \"TargetWolframVersionNumber\" -> PacletTools`PacletDocumentationBuild`Private`determinePacletDocumentationTargetVersionNumber[paclet]];\n",
    "processed = Replace[result, {built : (_Success | _Association) :> built[\"ProcessedFiles\"], _ -> <||>}];\n",
    "Put[Association[KeyValueMap[\n",
    "    With[{page = FileNameDrop[#1, Length[FileNameSplit[inputDir]]]}, If[MemberQ[pages, page], page -> <|\"Result\" -> #2, \"FileHash\" -> FileHash[#1, \"SHA256\", \"Base64Encoding\"]|>, Nothing]] &,\n",
    "    Replace[processed, Except[_Association] -> <||>]]], ", wlString[recordOut], "];\n"
];

(* Scripts run as separate kernel processes, at most n at a time, each one killed past its
   deadline: "Finished" or "TimedOut" by script. A process may start parallel kernels of its own,
   which a parallel kernel may not, and the stock documentation build does when it installs a
   format's paclet. *)
runKernelScripts[runs_List, n_Integer] := Module[{kernel = kernelExecutable[], queue = runs, running = {}, finished = <||>},
    If[ kernel === None, Return[AssociationMap["NoKernel" &, Lookup[runs, "Script", {}]], Module]];
    While[ queue =!= {} || running =!= {},
        While[ Length[running] < n && queue =!= {},
            AppendTo[running, Append[First[queue], {"Process" -> StartProcess[{kernel, "-noprompt", "-script", First[queue]["Script"]}], "Start" -> AbsoluteTime[]}]];
            queue = Rest[queue]
        ];
        (* what a process or its front end writes is read and dropped, so that no pipe fills *)
        Scan[If[MatchQ[#["Process"], _ProcessObject], drainProcess[#["Process"]]] &, running];
        running = Select[running, Function[run, Which[
            ! MatchQ[run["Process"], _ProcessObject] || ProcessStatus[run["Process"]] =!= "Running",
                finished[run["Script"]] = "Finished"; False,
            AbsoluteTime[] - run["Start"] > run["Deadline"],
                KillProcess[run["Process"]]; finished[run["Script"]] = "TimedOut"; False,
            True, True
        ]]];
        If[ running =!= {}, Pause[0.5]]
    ];
    finished
];

drainProcess[process_ProcessObject] := (
    ReadString[ProcessConnection[process, "StandardOutput"], EndOfBuffer];
    ReadString[ProcessConnection[process, "StandardError"], EndOfBuffer];
);

(* The changed pages, built in n processes and entered in PacletBuild's record, so that its own
   build builds only what is left (the pages that failed here, if any), makes the indexes over every
   page and packs. The shares are small, so that the processes stay busy to the end. The number of
   pages built here. *)
buildPagesInParallel[source_String, docsDir_String, builtDocsDir_String, recordFile_String, changed_List, n_Integer, limit_, dependencies_List] := Module[
    {work, shares, runs, finished, record},
    If[ changed === {} || n < 2, Return[0, Module]];
    work = CreateDirectory[];
    shares = Partition[changed, UpTo[Max[1, Min[25, Ceiling[Length[changed] / (4 n)]]]]];
    If[ ! DirectoryQ[builtDocsDir], CreateDirectory[builtDocsDir, CreateIntermediateDirectories -> True]];
    runs = MapIndexed[
        Function[{pages, index}, With[{base = FileNameJoin[{work, "share-" <> IntegerString[First[index]]}]},
            Export[base <> ".wl", pageShareScript[source, docsDir, builtDocsDir, pages, base <> "-record.wl", base <> ".log", dependencies], "Text"];
            <|"Script" -> base <> ".wl", "Record" -> base <> "-record.wl", "Deadline" -> limit * Length[pages] + 120|>
        ]],
        shares
    ];
    finished = runKernelScripts[runs, n];
    record = Join[readRecord[recordFile], Association[Select[Map[readRecord[#["Record"]] &, runs], AssociationQ]]];
    If[ ! DirectoryQ[DirectoryName[recordFile]], CreateDirectory[DirectoryName[recordFile], CreateIntermediateDirectories -> True]];
    Put[record, recordFile];
    DeleteDirectory[work, DeleteContents -> True];
    Count[Lookup[record, changed, None], KeyValuePattern["Result" -> Except[_?FailureQ]]]
];

(* the number of page-building processes: True, as many as the processors and the memory allow
   (a process with its front end takes about two gigabytes); n, n of them *)
pageProcesses[True] := Max[2, Min[$ProcessorCount, Floor[MemoryAvailable[] / 2.*^9]]];
pageProcesses[n_Integer] := n;
pageProcesses[_] := 0;

(* ==================== the archive ==================== *)

(* The archive a deploy or a submission packs, built afresh into build/, since the scrape packs
   whatever archive it finds there. PacletBuild builds the documentation from authoring notebooks;
   a paclet whose Documentation already holds built pages is built from a staged copy whose
   Documentation is docSource, the directory of their authoring notebooks (holding English/),
   because a 15.1 PacletBuild rebuilding built pages finds no URI in them and packs none. A
   PacletBuild that fails on the pages still succeeds, so the archive must hold a page for every
   authoring notebook.
   With "Incremental", the pages PacletBuild built last time are kept, less those whose authoring
   notebook is gone, and only changed pages are built again; with "Parallel", the changed pages are
   built on parallel kernels first. *)
freshArchive[head_Symbol, dir_String, name_String, docSource_, loadCheck_ : True, pageOptions_Association : <||>] := Module[
    {buildDir, prefix, recordFile, builtDocsDir, authoring, stageRoot, source, built, pages, load, dependencies, processes, changed, parallel, pruned,
     excludedQ, links, missing, declared},
    (* the files the archive leaves out; a symbolic link that leads outside the paclet must be one *)
    excludedQ = Function[path, AnyTrue[Flatten[{Lookup[pageOptions, "Exclude", {}]}], StringMatchQ[path, #] &]];
    links = outsideLinks[dir];
    If[ ! AllTrue[links, excludedQ], Message[head::links, Select[links, ! excludedQ[#] &]]; Return[$Failed, Module]];
    buildDir = FileNameJoin[{dir, "build"}];
    prefix = StringReplace[name, "/" -> "__"];
    recordFile = builtPagesRecord[buildDir, prefix];
    builtDocsDir = FileNameJoin[{buildDir, prefix, "Documentation"}];
    authoring = FileNames["*.nb", Replace[docSource, Automatic -> FileNameJoin[{dir, "Documentation"}]], Infinity];
    (* the archives of earlier builds, which the scrape would pack; without "Incremental", the pages
       built last time too *)
    Scan[DeleteFile, FileNames[prefix ~~ ("" | ("-" ~~ ___)) ~~ ".paclet", buildDir]];
    If[ ! TrueQ[Lookup[pageOptions, "Incremental", False]],
        If[ DirectoryQ[FileNameJoin[{buildDir, prefix}]], DeleteDirectory[FileNameJoin[{buildDir, prefix}], DeleteContents -> True]];
        If[ FileExistsQ[recordFile], DeleteFile[recordFile]]
    ];
    dependencies = loadedPacletDirectories[dir];
    (* PacletBuild builds the paclet directory itself, or a staged copy of it: without the files
       left out, and with docSource as its Documentation *)
    source = If[ docSource === Automatic && Lookup[pageOptions, "Exclude", {}] === {},
        dir,
        stageRoot = CreateDirectory[];
        With[{stage = CreateDirectory[FileNameJoin[{stageRoot, FileNameTake[dir]}]]},
            copyTree[dir, stage, "", Function[path, path === "build" || (docSource =!= Automatic && path === "Documentation") || excludedQ[path]]];
            If[ docSource =!= Automatic, CopyDirectory[docSource, FileNameJoin[{stage, "Documentation"}]]];
            stage
        ]
    ];
    pruned = pruneBuiltPages[FileNameJoin[{source, "Documentation"}], builtDocsDir, recordFile];
    changed = changedPages[FileNameJoin[{source, "Documentation"}], builtDocsDir, readRecord[recordFile]];
    processes = If[ Length[changed] > 1, pageProcesses[Lookup[pageOptions, "Parallel", False]], 0];
    parallel = buildPagesInParallel[source, FileNameJoin[{source, "Documentation"}], builtDocsDir, recordFile, changed, processes, Lookup[pageOptions, "TimeConstraint", 900], dependencies];
    built = freshKernelBuild[source, buildDir, dependencies];
    If[ source =!= dir, DeleteDirectory[stageRoot, DeleteContents -> True]];
    If[ ! StringQ[built["Archive"]] || ! FileExistsQ[built["Archive"]], Message[head::build, built["Failure"]]; Return[$Failed, Module]];
    pages = archivePages[built["Archive"]];
    missing = Complement[
        Map[relativePath[Replace[docSource, Automatic -> FileNameJoin[{dir, "Documentation"}]], #] &, authoring],
        Map[Last[StringSplit[#, "/Documentation/", 2]] &, pages]
    ];
    If[ missing =!= {}, Message[head::pages, Length[authoring] - Length[missing], Length[authoring], Take[missing, UpTo[10]]]; Return[$Failed, Module]];
    load = If[ TrueQ[loadCheck],
        With[{paclet = PacletObject[File[dir]]}, archiveLoadCheck[built["Archive"], paclet["Version"], paclet["PrimaryContext"], dependencies]],
        None
    ];
    If[ AssociationQ[load] && ! TrueQ[load["OK"]], Message[head::load, KeyDrop[load, "OK"]]; Return[$Failed, Module]];
    (* a paclet that declares LibraryLink libraries ships some; a check of the caller's has the last word *)
    With[{roots = libraryRoots[PacletObject[File[dir]]], files = Import[built["Archive"], {"ZIP", "FileNames"}]},
        If[ roots =!= {} && ! AnyTrue[files, Function[file, AnyTrue[roots, StringContainsQ[file, "/" <> # <> "/"] && ! StringEndsQ[file, "/"] &]]],
            Message[head::library, roots]; Return[$Failed, Module]]
    ];
    With[{check = Lookup[pageOptions, "ArchiveCheck", None]},
        If[ check =!= None && ! TrueQ[check[built["Archive"]]], Message[head::archivecheck, check]; Return[$Failed, Module]]
    ];
    (* the symbols the paclet declares, which the resource page's examples link to: by default the
       public ones the fresh kernel found, for a paclet that declares none *)
    declared = Replace[Lookup[pageOptions, "Symbols", Automatic], {
        Automatic :> If[ AssociationQ[load] && ListQ[load["SymbolNames"]] && load["SymbolNames"] =!= {},
            declareSymbols[built["Archive"], FileNameJoin[{buildDir, prefix}], load["SymbolNames"], False], None],
        names : {__String} :> declareSymbols[built["Archive"], FileNameJoin[{buildDir, prefix}], names, True],
        _ -> None
    }];
    <|"Archive" -> built["Archive"], "Pages" -> Length[pages], "Load" -> load, "DeclaredSymbols" -> declared, "ArchiveSize" -> Quantity[FileByteCount[built["Archive"]], "Bytes"],
      "Documentation" -> <|"Changed" -> Length[changed], "Pruned" -> Length[pruned], "Processes" -> processes, "BuiltInParallel" -> parallel|>|>
];

(* The archive repacked with the given symbols as its Kernel extension's "Symbols", which the
   resource page's examples are linked by (PacletSymbol), unless it declares some and replace is
   False. The built PacletInfo.wl is changed, never the paclet's own. The number of symbols
   declared, or None. *)
declareSymbols[archive_String, builtRoot_String, names_List, replace_] := Module[
    {infoFile = FileNameJoin[{builtRoot, "PacletInfo.wl"}], info, kernel, text, work, packed},
    info = FirstCase[Import[infoFile, "HeldExpressions"], HoldComplete[PacletObject[assoc_Association]] :> assoc, None];
    If[ ! AssociationQ[info], Return[None, Module]];
    kernel = FirstCase[Lookup[info, "Extensions", {}], {"Kernel", rules___} :> {rules}, None];
    If[ kernel === None || (! TrueQ[replace] && KeyExistsQ[Association[Cases[kernel, _Rule]], "Symbols"]), Return[None, Module]];
    info["Extensions"] = Replace[info["Extensions"], {"Kernel", rules___} :> Join[{"Kernel"}, DeleteCases[{rules}, "Symbols" -> _], {"Symbols" -> names}], {1}];
    text = "PacletObject[" <> ToString[info, InputForm] <> "]\n";
    If[ ! SyntaxQ[text], Return[None, Module]];
    Export[infoFile, text, "Text"];
    work = CreateDirectory[];
    packed = CreatePacletArchive[builtRoot, work];
    If[ StringQ[packed] && FileExistsQ[packed], CopyFile[packed, archive, OverwriteTarget -> True]];
    DeleteDirectory[work, DeleteContents -> True];
    If[ StringQ[packed], Length[names], None]
];

(* the directories of the LibraryLink libraries a paclet declares *)
libraryRoots[paclet_PacletObject] := DeleteDuplicates @ Cases[
    Replace[paclet["Extensions"], Except[_List] -> {}],
    {"LibraryLink", rules___} :> Lookup[Association[Cases[{rules}, _Rule]], "Root", "LibraryResources"]
];

(* The symbolic links in a paclet directory, outside build/, that lead outside it, by their path
   within it: PacletBuild would pack what they lead to, such as a runtime the paclet links to from a
   toolchain on this computer. They are found with find and realpath, where those exist. *)
outsideLinks[dir_String] := Module[{root = realPath[dir], find},
    find = RunProcess[{"find", dir, "-type", "l"}];
    If[ ! AssociationQ[find] || find["ExitCode"] =!= 0, Return[{}, Module]];
    Select[
        Map[relativePath[dir, #] &, StringSplit[find["StandardOutput"], "\n"]],
        ! StringStartsQ[#, "build" <> $PathnameSeparator] && ! StringStartsQ[realPath[FileNameJoin[{dir, #}]], root <> $PathnameSeparator] &
    ]
];

realPath[path_String] := With[{run = RunProcess[{"realpath", path}]},
    If[ AssociationQ[run] && run["ExitCode"] === 0, StringTrim[run["StandardOutput"]], path]
];

(* a directory copied file by file, without the paths (relative to it) skip says to leave out *)
copyTree[from_String, to_String, path_String, skip_] := Scan[
    Function[entry, With[{relative = If[path === "", FileNameTake[entry], FileNameJoin[{path, FileNameTake[entry]}]]},
        Which[
            skip[relative], Null,
            DirectoryQ[entry], CreateDirectory[FileNameJoin[{to, relative}]]; copyTree[from, to, relative, skip],
            True, CopyFile[entry, FileNameJoin[{to, relative}]]
        ]
    ]],
    FileNames["*", FileNameJoin[{from, path}]]
];

(* the documentation and the definition notebook a deploy or a submission needs *)
readyToPublishQ[head_Symbol, dir_String, docSource_] := Or[
    FileExistsQ[FileNameJoin[{dir, "ResourceDefinition.nb"}]] &&
        FileNames["*.nb", Replace[docSource, Automatic -> FileNameJoin[{dir, "Documentation"}]], Infinity] =!= {},
    Message[head::nodef, dir]; False
];

loadResourceTools[] := (
    Needs["DefinitionNotebookClient`"];
    Needs["PacletResource`DefinitionNotebook`"];
    Needs["ResourceSystemClient`"];
    (* the scrape computes the documentation's paths with PacletTools`, which a headless kernel
       does not load by itself; without it a $Failed lands in the scraped file listing *)
    Needs["PacletTools`"]
);

Options[DeployPacletResource] = {
    "Account" -> Automatic,
    "DryRun" -> False,
    "DocumentationSource" -> Automatic,
    "HeroImage" -> Automatic,
    "Archive" -> None,
    "Commit" -> Automatic,
    "Republish" -> False,
    "LoadCheck" -> True,
    "Incremental" -> False,
    "Parallel" -> False,
    "TimeConstraint" -> 900,
    "Exclude" -> {},
    "ArchiveCheck" -> None,
    "Symbols" -> Automatic,
    Permissions -> "Public"
};

DeployPacletResource::nodef = "`1` has no ResourceDefinition.nb or no documentation: build it first with BuildPacletDocumentation.";
DeployPacletResource::build = "PacletBuild failed: `1`.";
DeployPacletResource::pages = "The built archive holds `1` of the `2` documentation pages; nothing was deployed. Missing: `3`.";
DeployPacletResource::library = "PacletInfo.wl declares LibraryLink libraries, and the archive holds none under `1`; nothing was deployed.";
DeployPacletResource::archivecheck = "The archive check `1` did not give True; nothing was deployed.";
DeployPacletResource::links = "These symbolic links lead outside the paclet, and the archive would hold what they lead to: `1`; nothing was deployed. Remove them, or leave them out with \"Exclude\".";
DeployPacletResource::scrape = "Scraping the resource from `1` failed: `2`.";
DeployPacletResource::packed = "The scrape packed `1`, not the fresh build `2`; nothing was deployed.";
DeployPacletResource::deploy = "Deploying the resource failed: `1`.";
DeployPacletResource::cloud = "Not connected to the Wolfram Cloud.";
DeployPacletResource::account = "Connected to the Wolfram Cloud as `1`, not as `2`; connect as `2` to publish this resource.";
DeployPacletResource::archive = "The archive could not be published to `1`: `2`.";
DeployPacletResource::load = "The built archive does not load cleanly in a fresh kernel: `1`; nothing was deployed.";
DeployPacletResource::commit = "`1` is already published from commit `2`, not `3`; nothing was deployed. Bump the version, or use \"Republish\" -> True to replace it.";
DeployPacletResource::unknowncommit = "`1` is already published from commit `2`, and this paclet's commit is unknown (not a git checkout); nothing was deployed. Pass \"Commit\", or use \"Republish\" -> True.";
DeployPacletResource::nocommit = "`1` is already published and records no commit; nothing was deployed. Use \"Republish\" -> True to replace it.";

DeployPacletResource[dir_String, opts : OptionsPattern[]] := Catch[Module[
    {definition, paclet, name, built, resource, location, hero, deployed, archive, commit, versioned, offline, target},
    definition = FileNameJoin[{dir, "ResourceDefinition.nb"}];
    If[ ! readyToPublishQ[DeployPacletResource, dir, OptionValue["DocumentationSource"]], Throw[$Failed, "deploy"]];
    offline = offlineDryRunQ[OptionValue["DryRun"]];
    If[ ! offline && ! publishingAccountQ[DeployPacletResource, OptionValue["Account"]], Throw[$Failed, "deploy"]];
    PacletDirectoryLoad[dir];
    paclet = PacletObject[File[dir]];
    name = paclet["Name"];
    loadResourceTools[];
    (* the fixes probe the cloud; a dry run writes nothing there *)
    If[ ! TrueQ[OptionValue["DryRun"]], installDeployFixes[]; installHeroPatch[]];
    (* the version gate, before anything is built: on the deployed resource, which records the
       version and commit of its release, and on the versioned archive *)
    commit = Replace[OptionValue["Commit"], Automatic :> gitCommit[dir]];
    versioned = Replace[OptionValue["Archive"], {
        dirName_String :> StringTrim[dirName, "/"] <> "/" <> StringReplace[name, "/" -> "__"] <> "-" <> paclet["Version"] <> ".paclet",
        _ -> None
    }];
    target = If[ offline, None,
        ResourceSystemClient`Private`loadDeployResourceShingle[];
        Replace[DeployedResourceShingle`Private`defaultCloudObjectLocation["Paclet", name], Except[_CloudObject] :> None]
    ];
    If[ ! offline && ! TrueQ[OptionValue["Republish"]],
        If[ MatchQ[target, _CloudObject] && ! releaseAllowedQ[DeployPacletResource, paclet["Version"] <> " at " <> First[target], deployedCommit[target, paclet["Version"]], commit],
            Throw[$Failed, "deploy"]];
        If[ StringQ[versioned] && ! releaseAllowedQ[DeployPacletResource, versioned, publishedCommit[versioned], commit],
            Throw[$Failed, "deploy"]]
    ];
    built = freshArchive[DeployPacletResource, dir, name, OptionValue["DocumentationSource"], OptionValue["LoadCheck"],
        <|"Incremental" -> OptionValue["Incremental"], "Parallel" -> OptionValue["Parallel"], "TimeConstraint" -> OptionValue["TimeConstraint"], "Exclude" -> OptionValue["Exclude"], "ArchiveCheck" -> OptionValue["ArchiveCheck"], "Symbols" -> OptionValue["Symbols"]|>];
    If[ built === $Failed, Throw[$Failed, "deploy"]];
    (* where the definition notebook's Deploy button puts a resource:
       <account>/DeployedResources/Paclet/<Publisher>/<Name>. A freshly opened definition notebook
       resolves its paclet only after PacletObject and ScrapePacletDirectory have each run once.
       A definition notebook built from markdown has no file-manager cell listing PacletInfo.wl,
       which the scrape reports as an error; the paclet is attached from build/.
       The scrape takes the notebook's data, its examples among them, and the deploy runs with the
       notebook open: the examples are a notebook the resource page is made from, which the front
       end holds only while the definition notebook is open. The scrape reads the notebook's hints
       from the front end's input notebook, which a headless front end does not have; it is the
       definition notebook. The scrape makes a ResourceObject, which needs a cloud session; an
       offline dry run stops before it. *)
    hero = heroImage[OptionValue["HeroImage"], definition];
    {resource, location, deployed} = Replace[
        If[ offline, {None, None, None}, UsingFrontEnd @ Module[{notebook, scraped, versioned, where, packed, done},
            notebook = NotebookOpen[definition, Visible -> False];
            PacletObject[name];
            PacletResource`DefinitionNotebook`ScrapePacletDirectory[notebook];
            scraped = With[{inputNotebook = Symbol["DefinitionNotebookClient`PackageScope`inputNotebook"]},
                Block[{inputNotebook = Function[notebook]},
                    DefinitionNotebookClient`ScrapeResource["Paclet", notebook, Interactive -> False, "FailureCondition" -> None, "IncludeNotebookData" -> True]
                ]
            ];
            (* a deploy, unlike a submission, gives the resource no version; it is the paclet's, in
               the resource system's record of the resource and in the object itself *)
            versioned = If[ MatchQ[scraped, _ResourceObject] && ! StringQ[scraped["Version"]],
                ResourceSystemClient`Private`setResourceInfo[scraped["UUID"], <|"Version" -> paclet["Version"]|>];
                ResourceObject[Append[First[scraped], "Version" -> paclet["Version"]]],
                scraped
            ];
            where = If[ MatchQ[scraped, _ResourceObject],
                ResourceSystemClient`Private`loadDeployResourceShingle[];
                Replace[DeployedResourceShingle`Private`defaultCloudObjectLocation["Paclet", scraped["Name"]], Except[_CloudObject] :> None],
                None
            ];
            packed = If[ MatchQ[versioned, _ResourceObject], Replace[Lookup[First[versioned], "PacletFile", None], File[file_] :> file], None];
            done = Which[
                ! MatchQ[versioned, _ResourceObject] || where === None,
                    Message[DeployPacletResource::scrape, definition, scraped]; $Failed,
                ! StringQ[packed] || realPath[packed] =!= realPath[built["Archive"]],
                    Message[DeployPacletResource::packed, packed, built["Archive"]]; $Failed,
                TrueQ[OptionValue["DryRun"]],
                    {versioned, where, None},
                True,
                    {versioned, where, Block[{$heroImage = hero}, CloudDeploy[versioned, where, Permissions -> OptionValue[Permissions]]]}
            ];
            (* the deploy reads the notebook's data, so the notebook closes after it *)
            NotebookClose[notebook];
            done
        ]],
        $Failed :> Throw[$Failed, "deploy"]
    ];
    If[ deployed =!= None && ! MatchQ[deployed, _CloudObject], Message[DeployPacletResource::deploy, deployed]; Throw[$Failed, "deploy"]];
    If[ deployed =!= None, recordRelease[location, paclet["Version"], commit]; URLRead[First[location] <> "/", TimeConstraint -> 60]];
    archive = Replace[OptionValue["Archive"], {
        None -> None,
        dirName_String /; TrueQ[OptionValue["DryRun"]] :> None,
        dirName_String :> With[{published = publishArchive[built["Archive"], dirName, name, OptionValue[Permissions], commit]},
            If[AssociationQ[published], published, Message[DeployPacletResource::archive, dirName, published]; Throw[$Failed, "deploy"]]
        ]
    }];
    <|
        "Resource" -> If[location === None, None, First[location]],
        "Account" -> If[offline, None, connectedAccount[]],
        "Deployed" -> deployed =!= None,
        "Archive" -> built["Archive"],
        "Pages" -> built["Pages"],
        "Documentation" -> built["Documentation"],
        "DeclaredSymbols" -> built["DeclaredSymbols"],
        "HeroImage" -> hero =!= None,
        "Install" -> archive,
        "ArchiveSize" -> built["ArchiveSize"],
        "Version" -> paclet["Version"],
        "Commit" -> commit,
        "Load" -> built["Load"],
        "AppearanceElements" -> $appearanceElements
    |>
], "deploy"];

(* ==================== submitting to the Paclet Repository ==================== *)

Options[SubmitPacletResource] = {
    "Account" -> Automatic, "DryRun" -> False, "DocumentationSource" -> Automatic, "LoadCheck" -> True,
    "Incremental" -> False, "Parallel" -> False, "TimeConstraint" -> 900, "Exclude" -> {}, "ArchiveCheck" -> None, "Symbols" -> Automatic
};

SubmitPacletResource::nodef = DeployPacletResource::nodef;
SubmitPacletResource::build = DeployPacletResource::build;
SubmitPacletResource::pages = DeployPacletResource::pages;
SubmitPacletResource::links = DeployPacletResource::links;
SubmitPacletResource::packed = "The scrape packed `1`, not the fresh build `2`; nothing was submitted.";
SubmitPacletResource::library = DeployPacletResource::library;
SubmitPacletResource::archivecheck = DeployPacletResource::archivecheck;
SubmitPacletResource::published = "The Paclet Repository has `1` at version `2` already, not older than `3`; nothing was submitted. Raise the version in PacletInfo.wl.";
SubmitPacletResource::cloud = DeployPacletResource::cloud;
SubmitPacletResource::account = DeployPacletResource::account;
SubmitPacletResource::load = "The built archive does not load cleanly in a fresh kernel: `1`; nothing was submitted.";
SubmitPacletResource::check = "The definition notebook has blocking issues: `1`; nothing was submitted.";
SubmitPacletResource::version = "The scrape packed `1`, not version `2` of the paclet; nothing was submitted.";
SubmitPacletResource::submit = "The submission did not succeed: `1`.";
SubmitPacletResource::apierr = "The Paclet Repository answered HTTP `1` from `2`: `3`";

(* The repository's answers to a rejected request, which the client reports as "Unknown error",
   shown as a message; the definition never matches, so the stock handler still runs. *)
$apiErrorsShown = False;
showAPIErrors[] := If[ ! TrueQ[$apiErrorsShown],
    ResourceSystemClient`Private`handleError[head_, endpoint_, code_, chars_, params_, rbase_] /;
        (Message[SubmitPacletResource::apierr, code, endpoint, StringTake[StringJoin[FromCharacterCode /@ Flatten[{chars}]], UpTo[1500]]]; False) := Null;
    $apiErrorsShown = True
];

(* SubmitRepository[notebook] asks for the version in a dialog, which a kernel without a front end
   cannot show; the same submission is the notebook scraped into a ResourceObject and handed to
   ResourceSubmit with the version and publisher of PacletInfo.wl. *)
SubmitPacletResource[dir_String, opts : OptionsPattern[]] := Catch[Module[
    {definition, paclet, version, publisher, issues, built, result, offline},
    definition = FileNameJoin[{dir, "ResourceDefinition.nb"}];
    If[ ! readyToPublishQ[SubmitPacletResource, dir, OptionValue["DocumentationSource"]], Throw[$Failed, "submit"]];
    offline = offlineDryRunQ[OptionValue["DryRun"]];
    If[ ! offline && ! publishingAccountQ[SubmitPacletResource, OptionValue["Account"]], Throw[$Failed, "submit"]];
    PacletDirectoryLoad[dir];
    paclet = PacletObject[File[dir]];
    {version, publisher} = {paclet["Version"], paclet["PublisherID"]};
    (* the repository takes only a version newer than the ones it has *)
    If[ ! offline,
        With[{latest = Last[SortBy[Map[#["Version"] &, PacletFindRemote[paclet["Name"], UpdatePacletSites -> True]], versionKey], None]},
            If[ StringQ[latest] && ! newerVersionQ[version, latest],
                Message[SubmitPacletResource::published, paclet["Name"], latest, version]; Throw[$Failed, "submit"]]
        ]
    ];
    loadResourceTools[];
    showAPIErrors[];
    (* ResourceSubmit checks the PublisherID against $PublisherID, which is None until set *)
    $PublisherID = publisher;
    (* the repository refuses a definition notebook with errors, so they stop the submission
       before the build; a definition notebook built from markdown has no file-manager cell
       listing PacletInfo.wl, an error the scrape makes up for by attaching the built paclet *)
    (* the check and the scrape reach the resource system, which needs a cloud session; an offline
       dry run builds and loads the archive only *)
    issues = If[ offline, {}, UsingFrontEnd @ Module[{notebook, check},
        notebook = NotebookOpen[definition, Visible -> False];
        check = DefinitionNotebookClient`CheckDefinitionNotebook[notebook];
        NotebookClose[notebook];
        Select[Normal[check], #Level === "Error" && #Tag =!= "NoPacletInfoFileCell" &]
    ]];
    If[ issues =!= {}, Message[SubmitPacletResource::check, Lookup[issues, "Tag"]]; Throw[$Failed, "submit"]];
    built = freshArchive[SubmitPacletResource, dir, paclet["Name"], OptionValue["DocumentationSource"], OptionValue["LoadCheck"],
        <|"Incremental" -> OptionValue["Incremental"], "Parallel" -> OptionValue["Parallel"], "TimeConstraint" -> OptionValue["TimeConstraint"], "Exclude" -> OptionValue["Exclude"], "ArchiveCheck" -> OptionValue["ArchiveCheck"], "Symbols" -> OptionValue["Symbols"]|>];
    If[ built === $Failed, Throw[$Failed, "submit"]];
    (* Scrape and submit with the notebook open: the scraped record reads its notebook data while
       it is open, and a closed notebook puts $Failed into the request. *)
    result = If[ offline, <|"Submitted" -> False, "Archive" -> built["Archive"]|>, UsingFrontEnd @ Module[{notebook, scraped, packed, submitted},
        notebook = NotebookOpen[definition, Visible -> False];
        (* the scrape reads the notebook's hints from the front end's input notebook, which a
           headless front end does not have; it is the definition notebook *)
        scraped = With[{inputNotebook = Symbol["DefinitionNotebookClient`PackageScope`inputNotebook"]},
            Block[{inputNotebook = Function[notebook]},
                DefinitionNotebookClient`ScrapeResource[DefinitionNotebookClient`NotebookResourceType[notebook], notebook, "IncludeNotebookData" -> True]
            ]
        ];
        packed = Replace[Lookup[First[scraped], "PacletFile", None], File[file_] :> file];
        submitted = Which[
            ! StringQ[packed] || ! StringContainsQ[FileNameTake[packed], version], Message[SubmitPacletResource::version, packed, version]; $Failed,
            realPath[packed] =!= realPath[built["Archive"]], Message[SubmitPacletResource::packed, packed, built["Archive"]]; $Failed,
            TrueQ[OptionValue["DryRun"]], <|"Submitted" -> False, "Archive" -> packed|>,
            True, ResourceSubmit[ResourceObject[scraped], "Version" -> version, PublisherID -> publisher]
        ];
        NotebookClose[notebook];
        submitted
    ]];
    Which[
        AssociationQ[result], Join[<|"Name" -> paclet["Name"], "Version" -> version, "Account" -> If[offline, None, connectedAccount[]]|>, result, <|"Pages" -> built["Pages"], "Load" -> built["Load"]|>],
        (* a submission answers Success["ResourceSubmission", ...], or the ResourceObject *)
        MatchQ[result, _Success | _ResourceObject],
            <|"Name" -> paclet["Name"], "Version" -> version, "Account" -> connectedAccount[], "Submitted" -> True, "Archive" -> built["Archive"], "Pages" -> built["Pages"], "Load" -> built["Load"], "Result" -> result|>,
        True, If[result =!= $Failed, Message[SubmitPacletResource::submit, result]]; $Failed
    ]
], "submit"];

(* ==================== the whole pipeline ==================== *)

Options[PublishPacletPage] = {
    "Account" -> Automatic,
    "Build" -> True,
    "Check" -> True,
    "Submit" -> False,
    "DryRun" -> False,
    "BuildOptions" -> {},
    "ResourceOptions" -> {},
    "SiteOptions" -> {}
};

(* versions compared by their numbers: 1.0.10 is newer than 1.0.9 *)
versionKey[version_String] := FromDigits /@ StringCases[version, DigitCharacter ..];
newerVersionQ[version_String, than_String] := With[{a = versionKey[version], b = versionKey[than]},
    With[{n = Max[Length[a], Length[b]]}, Order[PadRight[b, n], PadRight[a, n]] === 1]
];

(* the resource options a submission builds its archive with too *)
submissionOptions[options_List] := Sequence @@ FilterRules[options, Except["Account" | "DryRun", Options[SubmitPacletResource]]];

PublishPacletPage::cloud = DeployPacletResource::cloud;
PublishPacletPage::account = "Connected to the Wolfram Cloud as `1`, not as `2`; nothing was built or published. Connect as `2` to publish this paclet.";

PublishPacletPage[dir_String, name_String, opts : OptionsPattern[]] := Catch[Module[
    {build, resource, catalog, site, check},
    (* the account first: a paclet's documentation may publish as it is built, as PacletPage's own
       tutorial does *)
    If[ ! offlineDryRunQ[OptionValue["DryRun"]] && ! publishingAccountQ[PublishPacletPage, OptionValue["Account"]], Throw[$Failed, "publish"]];
    build = If[ TrueQ[OptionValue["Build"]], BuildPacletDocumentation[dir, Sequence @@ OptionValue["BuildOptions"]], None];
    If[ build === $Failed, Throw[$Failed, "publish"]];
    resource = DeployPacletResource[dir, "Account" -> OptionValue["Account"], "DryRun" -> OptionValue["DryRun"], Sequence @@ OptionValue["ResourceOptions"]];
    If[ resource === $Failed, Throw[$Failed, "publish"]];
    If[ TrueQ[OptionValue["DryRun"]],
        Throw[<|"Build" -> build, "Resource" -> resource, "Site" -> None, "Check" -> None,
            "Submission" -> If[TrueQ[OptionValue["Submit"]], SubmitPacletResource[dir, "Account" -> OptionValue["Account"], "DryRun" -> True, submissionOptions[OptionValue["ResourceOptions"]]], None]|>, "publish"]];
    catalog = PacletPageCatalog[resource["Resource"], "Documentation" -> dir];
    If[ catalog === $Failed, Throw[$Failed, "publish"]];
    site = DeployPacletPage[catalog, name, "Source" -> dir, "Account" -> OptionValue["Account"], Sequence @@ OptionValue["SiteOptions"]];
    If[ site === $Failed, Throw[$Failed, "publish"]];
    (* a site just published may have pages the cloud has not rendered yet: what fails is checked
       again for up to two minutes *)
    check = If[ TrueQ[OptionValue["Check"]], CheckPacletPage[catalog, name, "Pages" -> All, "Wait" -> 120], None];
    <|
        "Build" -> build, "Resource" -> resource, "Site" -> site, "Check" -> check,
        "Submission" -> If[TrueQ[OptionValue["Submit"]], SubmitPacletResource[dir, "Account" -> OptionValue["Account"], submissionOptions[OptionValue["ResourceOptions"]]], None]
    |>
], "publish"];
