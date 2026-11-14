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

$converterURL = "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Function/MarkdownToNotebook";

(* the AISkills checkout this paclet sits in, when it does: its converter and WolframParser *)
$checkoutDirectory = FileNameJoin[{ParentDirectory[$packageDirectory], "..", "..", ".."}];

(* The converter, as an association: "Function", the function to call; "Load", held code that
   gives it on a parallel kernel; "Distribute", held code naming what the parallel kernels get as
   distributed definitions, or None; "ID", what tells its versions apart in the page cache.
   Automatic is MarkdownToNotebook: the one loaded already, else the checkout's file (with the
   checkout's WolframParser registered first, for TeX), else the converter this release of
   PacletPage was tested with, which it carries, else the published function. The file
   defines its symbols in the current context, so it is read in a context of its own, on no
   context path: its helpers (tr, urlQ, ...) are then never the namesakes of the symbols of a
   package a page's examples load, whose shadowing messages would be captured into the page. The
   code that loads it names the function by its string, which keeps a parallel map from
   distributing it. *)
$converterContext = "WolframInstitute`PacletPage`Converter`";
converterLoad[file_String] := With[{path = file, context = $converterContext},
    Hold[Block[{$Context = context, $ContextPath = {"System`"}}, Get[path]]; Symbol[context <> "MarkdownToNotebook"]]
];

converterSpec[Automatic] := converterSpec[Automatic] = If[ DownValues[Global`MarkdownToNotebook] =!= {},
    <|
        "Function" -> Global`MarkdownToNotebook, "Load" -> Hold[Symbol["Global`MarkdownToNotebook"]],
        "Distribute" -> Hold[Global`MarkdownToNotebook],
        "ID" -> digest[{DownValues[Global`MarkdownToNotebook], Options[Global`MarkdownToNotebook]}]
    |>,
    converterSpec[automaticConverter[]]
];
automaticConverter[] := Which[
    FileExistsQ[FileNameJoin[{$checkoutDirectory, "MarkdownToNotebook.wl"}]],
        If[ DirectoryQ[FileNameJoin[{$checkoutDirectory, "examples", "Paclet", "WolframParser"}]],
            PacletDirectoryLoad[FileNameJoin[{$checkoutDirectory, "examples", "Paclet", "WolframParser"}]]
        ];
        FileNameJoin[{$checkoutDirectory, "MarkdownToNotebook.wl"}],
    FileExistsQ[$pinnedConverter],
        File[$pinnedConverter],
    True,
        $converterURL
];
converterSpec[file_String] /; FileExistsQ[file] := converterSpec[file] = <|
    "Function" -> ReleaseHold[converterLoad[file]], "Load" -> converterLoad[file], "Distribute" -> None,
    "ID" -> digest[{FileHash[file, "SHA256", "HexString"], $converterContext}]
|>;
(* A published function is fetched once, through its ResourceObject (a bare ResourceFunction[url]
   fails on parallel kernels), and its definitions saved where the kernels that convert pages read
   them, with no network: a page kernel that fetched it again would wait on the cloud, ten seconds
   or more, and could be handed a broken download. The resource system's lookup of a URL reports
   URLParse::invuri for an Automatic it parses on the way. *)
converterSpec[url_String] := converterSpec[url] = With[{snapshot = converterSnapshot[url]},
    With[{file = FileNameJoin[{$TemporaryDirectory, "PacletPage-converter-" <> snapshot["ID"] <> ".wxf"}]},
        Export[file, snapshot, "WXF"];
        savedConverterSpec[file, snapshot]
    ]
];

(* the published function's definitions, the full name of its symbol and its ID, the deployed
   definitions: a redeploy at the same URL is another converter *)
converterSnapshot[url_String] := With[{f = Quiet[ResourceFunction[ResourceObject[url]], URLParse::invuri]},
    With[{symbol = ResourceFunction[f, "Function"]},
        <|
            "URL" -> url, "Name" -> Context[symbol] <> SymbolName[Unevaluated[symbol]], "Definition" -> Language`ExtendedFullDefinition[symbol],
            "ID" -> digest[{url, ResourceFunction[f, "DefinitionData"]}], "Fetched" -> DateString["ISODateTime"]
        |>
    ]
];

(* A converter saved as a file of its definitions, {"URL", "Name", "Definition", "ID", ...}: its
   definitions made in a kernel, and its symbol given, by code that needs nothing of PacletPage and
   no network, so that a parallel kernel runs it as well. Each release of PacletPage carries the
   published converter it was tested with as such a file, Assets/Converter.wxf, pinned by
   pin-converter.wls: a build uses that converter, whatever has been deployed since. *)
$pinnedConverter := FileNameJoin[{ParentDirectory[$packageDirectory], "Assets", "Converter.wxf"}];

converterSpec[File[file_String]] := converterSpec[File[file]] = With[{snapshot = Import[file, "WXF"]},
    If[ MatchQ[snapshot, KeyValuePattern[{"Name" -> _String, "Definition" -> _, "ID" -> _String}]],
        savedConverterSpec[file, snapshot],
        Message[BuildPacletDocumentation::converter, file]; converterSpec[$converterURL]
    ]
];
BuildPacletDocumentation::converter = "The converter saved in `1` cannot be read; the published one is used.";

savedConverterSpec[file_String, snapshot_Association] := With[{load = savedConverterLoad[file]}, <|
    "Function" -> ReleaseHold[load], "Load" -> load, "Distribute" -> None, "ID" -> snapshot["ID"]
|>];
savedConverterLoad[file_String] := With[{f = file},
    Hold[With[{saved = Import[f, "WXF"]}, Language`ExtendedFullDefinition[] = saved["Definition"]; Symbol[saved["Name"]]]]
];

(* the published converter fetched and saved as the one this checkout of PacletPage carries *)
pinConverter[url_String : $converterURL] := With[{snapshot = converterSnapshot[url]},
    Export[$pinnedConverter, snapshot, "WXF", PerformanceGoal -> "Size"];
    KeyTake[snapshot, {"URL", "Name", "ID", "Fetched"}]
];
converterSpec[f_] := <|"Function" -> f, "Load" -> Hold[f], "Distribute" -> Hold[f], "ID" -> digest[f]|>;

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

(* A key that every kernel computes alike: the SHA-256 of an expression's WXF serialization, its
   packed arrays unpacked, as one kernel may pack a list another does not. Hash of an expression
   itself differs between kernels, and every key the builds and deploys keep is made by this. A
   list is tested as it stands, unevaluated, so code held in it, such as a page's NotebookLocate
   button, never runs. *)
digest[expr_] := Hash[BinarySerialize[expr /. array_List /; Developer`PackedArrayQ[Unevaluated[array]] :> Developer`FromPackedArray[array]], "SHA256", "HexString"];

(* a hash of the paclet's code (its PacletInfo, Kernel/ and Assets/): the documentation's examples
   are evaluated afresh when it changes, since the converter caches each example's result by its
   code alone; the PacletInfo carries the version an example may show. The files excluded, paths
   relative to the paclet's directory with * for any characters, are left out: a change to them
   leaves the examples' results as they are. With versioned False a PacletInfo is hashed without
   its "Version" rule, so that a new version alone leaves them too. *)
codeHash[dir_String, excluded_List : {}, versioned_ : True] := digest[
    Map[
        {StringDrop[#, StringLength[dir]], fileKey[#, versioned]} &,
        Select[
            Sort @ Join[
                Select[FileNameJoin[{dir, #}] & /@ {"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ],
                Select[FileNames["*", Select[FileNameJoin[{dir, #}] & /@ {"Kernel", "Assets"}, DirectoryQ], Infinity], FileType[#] === File &]
            ],
            ! StringMatchQ[StringTrim[StringDrop[#, StringLength[dir]], "/" | "\\"], excluded] &
        ]
    ]
];
codeHash[dirs_List, excluded_List : {}, versioned_ : True] := digest[codeHash[#, excluded, versioned] & /@ dirs];

(* a file's part of the code hash: its content's hash, a PacletInfo's without its version where versioned is False *)
fileKey[file_String, versioned_] := If[ TrueQ[versioned] || ! StringStartsQ[FileNameTake[file], "PacletInfo."],
    FileHash[file, "SHA256"],
    Hash[StringDelete[ByteArrayToString[ReadByteArray[file], "UTF-8"], RegularExpression["\"Version\"\\s*->\\s*\"[^\"]*\"\\s*,?"]], "SHA256"]
];

(* PacletResource's search index data skeletonizes each page, and DocumentationSearch asks
   WolframLanguageData about every page that is not one of System's symbols, a paclet's own among
   them, which have no entries. While that service fails, the unevaluated query is what it caches,
   and each page waits on it again. So the lookup is off around the deploy and the submission; the
   index is the same. DocumentationSearch is loaded first, as a later load would set the option
   back. *)
SetAttributes[withoutLanguageData, HoldFirst];
withoutLanguageData[expr_] := (
    Needs["DocumentationSearch`"];
    Internal`InheritedBlock[{DocumentationSearch`Skeletonizer`Private`skeletonizeFiles},
        SetOptions[DocumentationSearch`Skeletonizer`Private`skeletonizeFiles, "UseWolframLanguageData" -> False];
        expr
    ]
);

(* the paclet directories this kernel has loaded *)
pacletDirectories[] := Select[Replace[PacletDirectoryLoad[], Except[_List] -> {}], StringQ[#] && DirectoryQ[#] &];

(* the code a paclet's examples run: the paclet's own and its dependencies', given as directories
   or paclet names, by default the ones its PacletInfo declares, wherever they are found *)
dependencyName[name_String] := name;
dependencyName[name_String -> _] := name;
dependencyName[{name_String, ___}] := name;
dependencyName[_] := Nothing;

dependencyDirectory[location_String] /; DirectoryQ[location] := location;
dependencyDirectory[name_String] := Replace[PacletFind[name], {{paclet_PacletObject, ___} :> paclet["Location"], _ -> Nothing}];

exampleCode[dir_String, dependencies_, excluded_List : {}, versioned_ : True] := codeHash[exampleDirectories[dir, dependencies], excluded, versioned];
exampleDirectories[dir_String, dependencies_] := DeleteDuplicates @ Prepend[
    Map[dependencyDirectory, Replace[dependencies, {
        Automatic :> Map[dependencyName, Replace[PacletObject[File[dir]]["Dependencies"], Except[_List] -> {}]],
        Except[_List] -> {}
    }]],
    dir
];

(* the paclet's dependencies all the way down, as directories: its own, theirs, and so on *)
dependencyClosure[dir_String, dependencies_] := Module[{seen = {dir}, queue = Rest[exampleDirectories[dir, dependencies]]},
    While[ queue =!= {},
        With[{next = First[queue]},
            queue = Rest[queue];
            If[ ! MemberQ[seen, next],
                AppendTo[seen, next];
                queue = Join[queue, Rest[exampleDirectories[next, Automatic]]]
            ]
        ]
    ];
    Rest[seen]
];
(* the context that loads a paclet: its primary context, else its Kernel extension's *)
pacletContext[dir_String] := With[{paclet = PacletObject[File[dir]]},
    If[ ! PacletObjectQ[paclet], None,
        Replace[paclet["PrimaryContext"], Except[_String] :> FirstCase[
            Replace[paclet["Extensions"], Except[_List] -> {}],
            {"Kernel", rules___} :> First[Flatten[{Lookup[Cases[{rules}, _Rule], "Context", None]}], None], None]]
    ]
];

(* The code a page's examples run, by what the page uses ("CacheKey" -> "Definitions"): the cones
   of the paclet's symbols its markdown names, and what no definition shows. DefinitionScan.wls
   loads the paclet in a fresh kernel and reads each symbol of the contexts the loaded paclets
   declare: its whole definition, Language`ExtendedDefinition, and the symbols it uses, those of
   its code and the names in its code's strings with no space, and gives each symbol's cone, what
   it uses and what that uses, as one digest. A page's code is the digests of the cones its names
   reach and a part every page shares: the PacletInfo, the files under Kernel/ and Assets/ the
   loading did not read, such as data a function reads when it runs or code it loads on first use,
   the definitions of System` and Global` symbols the loading made, the dependencies' code and the
   converter. So a change to one function converts again only the pages whose markdown reaches it. *)
$definitionScan := FileNameJoin[{$packageDirectory, "DefinitionScan.wls"}];

BuildPacletDocumentation::outranked = "The dependency `1` loads from an installed version `2`, which outranks the directory given: its pages are evaluated with that version, and every page's key has the directory's code whole.";
BuildPacletDocumentation::scan = "The paclet's definitions were not scanned (`1`), so every page's key has all of its code.";

(* the scan, kept in the cache under the paclet's and its dependencies' whole code, the Wolfram
   version and the scan's own code, and done again when one of them changes *)
definitionScan[dirs_List, context_String, key_String, cache_String, work_String, monitored_ : False, dependencies_List : {}, more_Association : <||>] := Module[
    {saved = FileNameJoin[{cache, "definitions.wxf"}], in = FileNameJoin[{work, "definitions-in.wxf"}], out = FileNameJoin[{work, "definitions.wxf"}], old, data},
    old = If[FileExistsQ[saved], Import[saved, "WXF"], None];
    If[ AssociationQ[old] && Lookup[old, "Key", None] === key, Return[old["Scan"], Module]];
    Export[in, Join[<|"Directories" -> dirs, "Context" -> context, "Dependencies" -> dependencies|>, more], "WXF"];
    runScan[{in, out}, TrueQ[monitored], 1800];
    data = If[FileExistsQ[out], Import[out, "WXF"], <|"Failure" -> "the scan wrote nothing"|>];
    If[ AssociationQ[data] && KeyExistsQ[data, "Cones"],
        Export[saved, <|"Key" -> key, "Scan" -> data|>, "WXF"]; data,
        Failure["DefinitionScan", <|"MessageTemplate" -> "`1`", "MessageParameters" -> {Lookup[Replace[data, Except[_Association] -> <||>], "Failure", "no result"]}|>]
    ]
];

(* The scan, a process of its own, killed with what it started past its deadline. Its progress
   lines are printed as they come when the build is monitored, and only then: a page's example may
   build documentation, and a line would be one of its outputs. *)
runScan[arguments_List, monitored_, deadline_] := Module[{kernel = kernelExecutable[], process, end, pending = "", lines},
    If[ kernel === None, Return[$Failed, Module]];
    process = StartProcess[kernelScriptCommand[kernel, $definitionScan, arguments]];
    If[ ! MatchQ[process, _ProcessObject], Return[$Failed, Module]];
    (* nothing answers a prompt: with its input closed, code that asks for one fails at once *)
    Close[ProcessConnection[process, "StandardInput"]];
    end = AbsoluteTime[] + deadline;
    lines[text_] := If[ StringQ[text],
        pending = pending <> text;
        With[{parts = StringSplit[pending, "\n", All]},
            If[ monitored, Scan[Print["definitions: ", #] &, Select[Most[parts], # =!= "" &]]];
            pending = Last[parts]
        ]
    ];
    While[ ProcessStatus[process] === "Running" && AbsoluteTime[] < end,
        lines[ReadString[ProcessConnection[process, "StandardOutput"], EndOfBuffer]];
        ReadString[ProcessConnection[process, "StandardError"], EndOfBuffer];
        Pause[0.5]
    ];
    If[ ProcessStatus[process] === "Running",
        killProcessTree[process]; If[ monitored, Print["definitions: stopped past ", IntegerString[deadline], " s"]],
        With[{rest = ReadString[ProcessConnection[process, "StandardOutput"], EndOfFile]}, lines[If[StringQ[rest], rest, ""] <> "\n"]]
    ];
];

(* the part of the code every page's key shares *)
sharedCode[dir_String, scan_Association, dependencies_List, excluded_List, versioned_, converterID_] := With[
    {read = AssociationThread[ExpandFileName /@ Join[scan["Read"], Lookup[scan, "Attached", {}]] -> True], unloaded = Lookup[scan, "Unloaded", {}]},
    digest[{
        fileKey[#, versioned] & /@ Select[FileNameJoin[{dir, #}] & /@ {"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ],
        unreadFiles[dir, read, excluded],
        scan["Foreign"],
        (* a dependency's definitions, and what no definition shows of it, are in the cones that
           reach it; one that did not load is here whole *)
        codeHash[#, excluded, versioned] & /@ Select[dependencies, MemberQ[unloaded, pacletContext[#]] || ! StringQ[pacletContext[#]] &],
        converterID
    }]
];
(* A paclet's files that its code may use: those under Kernel/ and Assets/, and under what its
   PacletInfo declares besides, the roots of its Kernel, LibraryLink and AutoCompletionData
   extensions and the files and directories of its Asset extensions; not its documentation. *)
pacletFiles[dir_String] := Module[{paclet = PacletObject[File[dir]], extensions, places},
    extensions = If[PacletObjectQ[paclet], Replace[paclet["Extensions"], Except[_List] -> {}], {}];
    places = Join[
        {"Kernel", "Assets"},
        Catenate @ Cases[extensions, {kind : "Kernel" | "LibraryLink" | "AutoCompletionData" | "Asset", rules___} :> With[
            {options = Association[Cases[{rules}, _Rule]]},
            With[{root = Lookup[options, "Root", Switch[kind, "Kernel", "Kernel", "LibraryLink", "LibraryResources", "AutoCompletionData", "AutoCompletionData", _, "."]]},
                If[ kind === "Asset",
                    With[{assets = Cases[Flatten[{Lookup[options, "Assets", {}]}], (_ -> path_String) | {_, path_String} :> path]},
                        If[assets === {}, If[MemberQ[{".", ""}, root], {}, {root}], FileNameJoin[{root, #}] & /@ assets]],
                    {root}
                ]
            ]
        ]]
    ];
    Sort @ DeleteDuplicates @ Catenate @ Map[
        Function[place, With[{path = FileNameJoin[{dir, place}]},
            Which[
                StringQ[place] && DirectoryQ[path] && ExpandFileName[path] =!= ExpandFileName[dir], Select[FileNames["*", path, Infinity], FileType[#] === File &],
                FileExistsQ[path] && FileType[path] === File, {path},
                True, {}
            ]
        ]],
        Select[places, StringQ]
    ]
];
(* those of them that the loading did not read, with their hashes *)
unreadFiles[dir_String, read_Association, excluded_List] := Map[
    {StringDrop[#, StringLength[dir]], FileHash[#, "SHA256"]} &,
    Select[pacletFiles[dir], ! KeyExistsQ[read, ExpandFileName[#]] && ! StringMatchQ[StringTrim[StringDrop[#, StringLength[dir]], "/" | "\\"], excluded] &]
];

(* the names a text holds *)
$identifier = (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...) ~~ (("`" ~~ (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...)) ...);
textNames[text_String] := DeleteDuplicates[StringCases[text, $identifier]];

(* what the codes of pages and examples are made of: the scan's cone digests and the shared part *)
definitionKeys[scan_Association, shared_String] := <|
    "Cones" -> scan["Cones"], "Shared" -> shared,
    (* a name without its context is one the paclet may name so: a symbol of its own, or one of
       a paclet it depends on that is in no Private or PackageScope context; never a Module's
       local, such as the x$ that inline math $...x$ ends with *)
    "ByOwnName" -> GroupBy[Lookup[scan, "Seeds", Complement[Keys[scan["Cones"]], Lookup[scan, "Temporaries", {}]]], Last[StringSplit[#, "`"]] &],
    "System" -> AssociationThread[StringReplace[Names["System`*"], StartOfString ~~ "System`" -> ""] -> True]
|>;

(* the code names reach: the shared part and the digests of the cones of the paclet's symbols
   they name, a name in full or by its own name, unless that is a System` symbol's *)
namesCode[keys_Association, names_List, shared_String : "Shared"] := digest[{keys[shared], Union @ Lookup[keys["Cones"], namedSymbols[keys, names]]}];
(* the paclet's symbols that names name *)
namedSymbols[keys_Association, names_List] := DeleteDuplicates @ Catenate @ Map[
    Which[
        KeyExistsQ[keys["Cones"], #], {#},
        KeyExistsQ[keys["System"], Last[StringSplit[#, "`"]]], {},
        True, Lookup[keys["ByOwnName"], Last[StringSplit[#, "`"]], {}]
    ] &,
    names
];
(* the names of a page: those of its markdown, and its own *)
pageTextNames[file_String] := Prepend[textNames[ByteArrayToString[ReadByteArray[file], "UTF-8"]], FileBaseName[file]];

(* a page's code: what the names of its markdown reach, its code and its front matter among them,
   and its own name *)
pageCode[keys_Association, file_String] := namesCode[keys, pageTextNames[file]];

(* the scan of a paclet with its dependencies, from the cache or made anew, or a Failure *)
scanPaclet[dir_String, context_String, closure_List, opts_Association] := With[{dirs = DeleteDuplicates[Join[pacletDirectories[], {dir}, closure]]},
    definitionScan[
        dirs, context,
        digest[{codeHash[Prepend[closure, dir], {}, True], {StringDrop[#, StringLength[dir]], FileHash[#, "SHA256"]} & /@ pacletFiles[dir],
            Function[d, {FileNameTake[#], FileHash[#, "SHA256"]} & /@ pacletFiles[d]] /@ closure, opts["Excluded"], dirs, context, $VersionNumber, $ReleaseNumber, FileHash[$definitionScan, "SHA256"]}],
        opts["Cache"], opts["Work"], opts["Monitor"], DeleteDuplicates[Select[pacletContext /@ closure, StringQ]],
        <|"DependencyDirectories" -> Select[closure, StringQ[pacletContext[#]] &], "Excluded" -> opts["Excluded"], "Files" -> AssociationMap[pacletFiles, Prepend[closure, dir]]|>
    ]
];

(* ==================== the scan and a page's dependencies, for a reader ==================== *)

PacletDefinitionScan::dir = PacletPageDependencies::dir = "`1` is not a paclet directory with a primary context.";
PacletDefinitionScan::scan = PacletPageDependencies::scan = "The paclet's definitions were not scanned: `1`.";
PacletPageDependencies::file = "`1` is not a file.";

Options[PacletDefinitionScan] = {"Dependencies" -> Automatic, "CacheDirectory" -> Automatic, "CacheExcludedFiles" -> {}, "Monitor" -> False};
Options[PacletPageDependencies] = Join[Options[PacletDefinitionScan], {"CacheVersion" -> True, "Converter" -> Automatic}];

(* the scan, with what it was made of: {scan, closure, excluded}, or $Failed after a message *)
readerScan[head_, dir_String, options_List] := Module[{paclet, context, closure, cache, work, scan, excluded},
    paclet = If[FileExistsQ[FileNameJoin[{dir, "PacletInfo.wl"}]] || FileExistsQ[FileNameJoin[{dir, "PacletInfo.m"}]], PacletObject[File[dir]], None];
    context = If[PacletObjectQ[paclet], paclet["PrimaryContext"], None];
    If[ ! StringQ[context], With[{h = head}, Message[MessageName[h, "dir"], dir]]; Return[$Failed, Module]];
    PacletDirectoryLoad[dir];
    excluded = Flatten[{OptionValue[head, options, "CacheExcludedFiles"]}];
    closure = dependencyClosure[dir, OptionValue[head, options, "Dependencies"]];
    cache = Replace[OptionValue[head, options, "CacheDirectory"], Automatic -> FileNameJoin[{dir, "build", "cache"}]];
    If[ ! DirectoryQ[cache], CreateDirectory[cache, CreateIntermediateDirectories -> True]];
    work = CreateDirectory[];
    scan = scanPaclet[dir, context, closure, <|"Excluded" -> excluded, "Cache" -> cache, "Work" -> work, "Monitor" -> TrueQ[OptionValue[head, options, "Monitor"]]|>];
    DeleteDirectory[work, DeleteContents -> True];
    If[ FailureQ[scan], With[{h = head}, Message[MessageName[h, "scan"], scan["Message"]]]; Return[$Failed, Module]];
    {scan, closure, excluded}
];
scanGraph[scan_Association] := With[{names = Keys[scan["Symbols"]]},
    Graph[names, Catenate[MapThread[Function[{from, to}, DirectedEdge[from, #] & /@ DeleteCases[names[[to]], from]], {names, Lookup[scan, "Edges", ConstantArray[{}, Length[names]]]}]]]
];

PacletDefinitionScan[dir_String, opts : OptionsPattern[]] := Module[{result = readerScan[PacletDefinitionScan, dir, {opts}]},
    If[ result === $Failed, Return[$Failed, Module]];
    With[{scan = First[result]},
        <|
            "Symbols" -> scan["Symbols"], "Cones" -> scan["Cones"], "Graph" -> scanGraph[scan],
            "Contexts" -> scan["Contexts"], "Dependencies" -> result[[2]], "FilesRead" -> scan["Read"],
            "FilesNamed" -> Lookup[scan, "Attached", {}], "SystemChanged" -> scan["Changed"],
            "Unloaded" -> Lookup[scan, "Unloaded", {}], "Outranked" -> Lookup[scan, "Outranked", {}]
        |>
    ]
];

PacletPageDependencies[dir_String, file_String, opts : OptionsPattern[]] := Module[{result, scan, keys, named, graph},
    If[ ! FileExistsQ[file], Message[PacletPageDependencies::file, file]; Return[$Failed, Module]];
    result = readerScan[PacletPageDependencies, dir, {opts}];
    If[ result === $Failed, Return[$Failed, Module]];
    scan = First[result];
    keys = definitionKeys[scan, sharedCode[dir, scan, result[[2]], result[[3]], OptionValue["CacheVersion"], converterSpec[OptionValue["Converter"]]["ID"]]];
    named = Sort[namedSymbols[keys, pageTextNames[file]]];
    graph = scanGraph[scan];
    <|
        "Named" -> named,
        "Reached" -> If[named === {}, {}, Sort[VertexOutComponent[graph, named]]],
        "Graph" -> If[named === {}, Graph[{}], Subgraph[graph, VertexOutComponent[graph, named]]],
        "Code" -> namesCode[keys, pageTextNames[file]],
        "Shared" -> keys["Shared"]
    |>
];

(* each page's code, by "CacheKey": "Definitions", Automatic for a paclet with a primary context
   whose examples run here, or "Code", the paclet's whole code for every page, as when the scan
   fails; and the keys the examples' codes are made with, or None *)
pageCodes[mode_, dir_String, paclet_PacletObject, tasks_List, opts_Association] := Module[
    {whole, context = paclet["PrimaryContext"], dirs, scan, keys, closure},
    (* the whole code, hashed once, for every page *)
    whole[] := With[{code = digest[{exampleCode[dir, opts["Dependencies"], opts["Excluded"], opts["Versioned"]], opts["Converter"]}]},
        <|"Codes" -> AssociationMap[code &, Lookup[tasks, "Page", {}]], "Keys" -> None|>
    ];
    If[ ! StringQ[context] || mode === "Code" || (mode === Automatic && opts["EvaluationKernel"] =!= None), Return[whole[], Module]];
    closure = dependencyClosure[dir, opts["Dependencies"]];
    scan = scanPaclet[dir, context, closure, opts];
    If[ FailureQ[scan],
        Which[
            mode === "Definitions", Message[BuildPacletDocumentation::scan, scan["Message"]],
            TrueQ[opts["Monitor"]], Print["definitions not scanned: ", scan["Message"], "; every page's key has all of the code"]
        ];
        Return[whole[], Module]
    ];
    Scan[Message[BuildPacletDocumentation::outranked, First[#], Last[#]] &, Lookup[scan, "Outranked", {}]];
    keys = definitionKeys[scan, sharedCode[dir, scan, closure, opts["Excluded"], opts["Versioned"], opts["Converter"]]];
    (* An example's result is what its code evaluates to, which the paclet's code decides and the
       converter does not: its code has the shared part without the converter, so that another
       converter converts every page again and evaluates none of its examples again. *)
    keys["ExampleShared"] = sharedCode[dir, scan, closure, opts["Excluded"], opts["Versioned"], None];
    <|"Codes" -> Association[#Page -> pageCode[keys, #File] & /@ tasks], "Keys" -> keys|>
];

(* The converter caches each example's result by the example's code alone, the code of the cells
   before it that it runs after among it, as a PersistentSymbol. With the definitions scanned, the
   build keeps that cache true to the paclet's code, example by example: it records the code each
   example's result was evaluated against, what the names of the cells it runs after reach, and
   before a page is converted it deletes the results whose code is another, so that only a page
   with such an example evaluates its examples again. The cells, the chains of cells they run
   after and the cache's names are the converter's own, from its parsing and its cache's
   functions, which a converter that has none of them leaves to the page's code. *)
$converterInternals = {
    "resolveSource", "litParse", "resolveIncludes", "executableQ", "resetBoundaryQ", "$setupHeadingQ",
    "cumulativeHashes", "exampleCacheName", "evaluatorTag", "stableHash", "mdSep", "$narrativeTemplates", "$cacheLocation"
};
(* The converter loads Wolfram/Parser at its first call and, where no copy it can load is at hand,
   installs it from the Paclet Repository: a transfer in the middle of a page's conversion, which no
   build should make. Each kernel that converts pages prepares the parser before it: a copy vendored
   beside a converter read from its checkout, else one installed, then the converter's attempt
   marked made, so that it installs nothing. The code is System's alone, so a parallel kernel runs
   it as it is. *)
$parserPreparation = Function[context,
    If[ NameQ[context <> "$parserTried"],
        With[{vendored = If[NameQ[context <> "vendoredParserDir"], Symbol[context <> "vendoredParserDir"][], None]},
            If[StringQ[vendored] && DirectoryQ[vendored], PacletDirectoryLoad[vendored]]
        ];
        If[ PacletFind["Wolfram/Parser"] =!= {} && ! MemberQ[$Packages, "Wolfram`Parser`"], Needs["Wolfram`Parser`"]];
        ToExpression[context <> "$parserTried", InputForm, Function[s, s = True, HoldAllComplete]];
        If[ NameQ[context <> "parserLoadedQ"] && TrueQ[Symbol[context <> "parserLoadedQ"][]],
            ToExpression[context <> "$parserReady", InputForm, Function[s, s = True, HoldAllComplete]]]
    ]
];

converterInternals[spec_Association] := Module[{symbol, context},
    symbol = Replace[spec["Function"], rf_ResourceFunction :> ResourceFunction[rf, "Function"]];
    If[ Head[symbol] =!= Symbol, Return[None, Module]];
    context = With[{s = symbol}, Context[s]];
    If[ ! AllTrue[$converterInternals, NameQ[context <> #] && ToExpression[context <> #, InputForm, Function[s, OwnValues[s] =!= {} || DownValues[s] =!= {}, HoldAllComplete]] &],
        Return[None, Module]];
    $parserPreparation[context];
    (* a converter with rules for its outputs' forms has them in its cache's keys, as a salt *)
    Join[AssociationMap[Symbol[context <> #] &, $converterInternals],
        If[NameQ[context <> "outputSalt"], <|"outputSalt" -> Symbol[context <> "outputSalt"]|>, <||>]]
];

(* The converter keeps each example's result as a PersistentSymbol at its $cacheLocation, by default
   "Local", in the user's base directory, which a build server's fresh container loses. A build
   keeps them in its cache directory instead, under examples/, beside the pages: a page is converted
   with the converter's $cacheLocation bound to that store, in whichever kernel converts it. *)
converterCacheSymbol[spec_Association] := Module[{symbol},
    symbol = Replace[spec["Function"], rf_ResourceFunction :> ResourceFunction[rf, "Function"]];
    If[ Head[symbol] =!= Symbol, Return[None, Module]];
    With[{name = With[{s = symbol}, Context[s]] <> "$cacheLocation"},
        If[ NameQ[name] && ToExpression[name, InputForm, Function[s, OwnValues[s] =!= {}, HoldAllComplete]], name, None]
    ]
];

SetAttributes[withExampleStore, HoldRest];
withExampleStore[task_Association, expr_] := With[{name = Lookup[task, "CacheSymbol", None], store = Lookup[task, "ExampleStore", None]},
    If[ StringQ[name] && NameQ[name] && store =!= None,
        ToExpression[name, InputForm, Function[cacheSymbol, Block[{cacheSymbol = store}, expr], HoldAllComplete]],
        expr
    ]
];

(* the cells each executable cell runs after, itself last, as the converter's chain of codes: from
   the last reset, after the setup section's cells *)
exampleChains[internals_Association, mode_, items_List] := Module[
    {chain = {}, base = {}, setup = AnyTrue[items, internals["$setupHeadingQ"]], level = None, chains = {}},
    Scan[
        Function[item,
            If[ setup && IntegerQ[level] && item["Type"] === "Heading" && ! internals["$setupHeadingQ"][item] && item["Level"] <= level, setup = False];
            If[ internals["$setupHeadingQ"][item], level = item["Level"]];
            If[ internals["resetBoundaryQ"][mode, item], chain = base];
            If[ internals["executableQ"][item],
                chain = Append[chain, item["Code"]];
                If[ setup, base = Append[base, item["Code"]]];
                AppendTo[chains, chain]
            ]
        ],
        items
    ];
    chains
];

(* a page's examples: each one's name in the converter's cache and its code. An example whose
   chain does not give the converter's hash has every code cell of the page as its chain. *)
pageExamples[internals_Association, keys_Association, task_Association] := Module[
    {src, parsed, meta, template, blocks, mode, items, hashes, chains, doc, all, salt},
    If[ Lookup[task["Options"], "Evaluate", True] =!= True, Return[{}, Module]];
    src = internals["resolveSource"][task["File"]];
    parsed = internals["litParse"][src["Text"]];
    meta = parsed["Metadata"];
    template = Lookup[meta, "Template", "Default"];
    If[ template === "NotebookTemplate", Return[{}, Module]];
    blocks = internals["resolveIncludes"][parsed["Blocks"], src["Base"]];
    mode = Replace[Lookup[task["Options"], "EvaluateSeparator", Automatic], Automatic :> If[MemberQ[internals["$narrativeTemplates"], template], None, Automatic]];
    items = Cases[blocks, b_ /; (internals["executableQ"][b] || MatchQ[b["Type"], "Separator" | "Heading"]), Infinity];
    salt = If[KeyExistsQ[internals, "outputSalt"], Replace[internals["outputSalt"][meta], Except[_String] -> ""], ""];
    hashes = If[salt === "", internals["cumulativeHashes"][mode, items], internals["cumulativeHashes"][mode, items, salt]];
    chains = exampleChains[internals, mode, items];
    doc = Replace[Lookup[meta, "Name", ""], "" :> src["Name"]] <> internals["evaluatorTag"][Automatic];
    all = Select[items, internals["executableQ"]][[All, "Code"]];
    MapThread[
        Function[{hash, chain, index}, <|
            "Name" -> internals["exampleCacheName"][doc, index, hash],
            "Code" -> namesCode[keys, textNames[StringRiffle[If[internals["stableHash"][salt <> StringJoin[internals["mdSep"] <> # & /@ chain]] === hash, chain, all], "\n"]], "ExampleShared"]
        |>],
        {hashes, chains, Range[Length[hashes]]}
    ]
];

(* f mapped over the list, with a line every 15 s of how many are done when monitored *)
progressMap[f_, list_List, monitored_, what_String] := Module[{done = 0, next = AbsoluteTime[] + 15},
    Map[
        Function[x, With[{y = f[x]},
            done++;
            If[ monitored && AbsoluteTime[] > next, Print[what, ": ", done, " of ", Length[list]]; next = AbsoluteTime[] + 15];
            y
        ]],
        list
    ]
];

(* the names the store holds in the folders of the names given, and those of a page's examples *)
storedNames[store_, names_List] := Cases[
    Catenate[PersistentObjects[#, store] & /@ DeleteDuplicates[StringReplace[names, RegularExpression["/[^/]*$"] -> "/*"]]],
    PersistentObject[name_String, ___] :> name
];
cachedExamples[internals_Association, examples_List] := Intersection[Lookup[examples, "Name", {}], storedNames[internals["$cacheLocation"], Lookup[examples, "Name", {}]]];

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

(* ::Section:: *)
(*Examples on another kernel*)

(* A page's evaluation kernel, as "EvaluationKernel" gives one for its template: None; a command,
   {program, arguments...}, that LinkLaunch runs with -wstp among its arguments; <|"Command" ->
   command, "Directory" -> where it starts, "Initialization" -> Hold[code] it evaluates after the
   paclet loads, "Scope" -> "Page" or "Template", "Identity" -> what keys the page cache|>; or an
   association of templates to them, which evaluates a template it leaves out here. The spec carries
   the paclet the kernel loads, its identity, by default the command and the hash of its program,
   and the key of what its outputs depend on: the identity, the scope, the initialization and the
   directory. *)
evaluationSpec[spec_Association /; ! KeyExistsQ[spec, "Command"], template_String, dir_String, context_] :=
    evaluationSpec[Lookup[spec, template, None], template, dir, context];
evaluationSpec[command : {__String}, template_String, dir_String, context_] := evaluationSpec[<|"Command" -> command|>, template, dir, context];
evaluationSpec[spec_Association /; MatchQ[spec["Command"], {__String}], template_String, dir_String, context_] := With[{
        full = <|
            "Command" -> spec["Command"], "Directory" -> Lookup[spec, "Directory", None], "Initialization" -> Lookup[spec, "Initialization", None],
            "Scope" -> Lookup[spec, "Scope", "Page"], "Paclet" -> dir, "Context" -> context, "Template" -> template,
            "Identity" -> Lookup[spec, "Identity", StringRiffle[spec["Command"]] <>
                If[FileExistsQ[First[spec["Command"]]], " " <> programHash[First[spec["Command"]]], ""]]
        |>},
    Append[full, "Key" -> Lookup[full, {"Identity", "Scope", "Initialization", "Directory"}]]
];
evaluationSpec[_, _, _, _] := None;

(* a program's hash, once for every page of a build: $programHashes is an association inside
   BuildPacletDocumentation, so that a program rebuilt between builds is hashed again *)
$programHashes = None;
programHash[file_String] := If[ AssociationQ[$programHashes],
    If[ KeyExistsQ[$programHashes, file], $programHashes[file], $programHashes[file] = FileHash[file, "SHA256", "HexString"]],
    FileHash[file, "SHA256", "HexString"]
];

(* the command as the one line LinkLaunch runs, each part quoted *)
launchLine[command_List] := StringRiffle[Map["\"" <> StringReplace[#, "\"" -> "\\\""] <> "\"" &, command], " "];

(* how long a kernel may take to answer its first prompt *)
$evaluationKernelStart = 300;

(* a kernel that fails a page: its reason thrown to the page, which fails with it *)
evaluationKernelFailure[reason_String] := Throw[
    Failure["EvaluationKernel", <|"MessageTemplate" -> "the kernel that evaluates the examples `1`", "MessageParameters" -> {reason}|>],
    $evaluationKernelTag
];

(* The packets of the link up to the first the pattern end matches: a short wait at a time, so that
   the time limit of the page around it stops a kernel that no longer answers. A link that closes,
   as a kernel's does when the kernel dies, fails the page, with that reason and not the link's
   messages. *)
linkPackets[link_LinkObject, end_] := Module[{packets = {}, packet = None},
    While[ ! MatchQ[packet, Hold[end]],
        Switch[ Quiet[LinkReadyQ[link, 0.05], {LinkObject::linkd, LinkObject::linkn}],
            True,
                packet = Quiet[LinkRead[link, Hold], {LinkObject::linkd, LinkObject::linkn}];
                If[ MatchQ[packet, $Failed | Hold[$Failed]], evaluationKernelFailure["closed its link"]];
                AppendTo[packets, packet],
            False, Null,
            _, evaluationKernelFailure["closed its link"]
        ]
    ];
    packets
];

(* a packet written to the link, held as it is *)
linkWritten[link_LinkObject, packet_] := If[ Quiet[LinkWrite[link, Unevaluated[packet]], {LinkObject::linkd, LinkObject::linkn}] === $Failed,
    evaluationKernelFailure["closed its link"]];
SetAttributes[linkWritten, HoldRest];

(* an expression evaluated on the kernel, held until it gets there; its value *)
linkEvaluated[link_LinkObject, Hold[e___]] := (
    linkWritten[link, EvaluatePacket[CompoundExpression[e]]];
    Replace[Last[linkPackets[link, _ReturnPacket]], Hold[ReturnPacket[v_]] :> v]
);

(* A cell's code entered on the kernel as a front end enters it, a statement at a time, each a
   string of boxes, and what came back as the converter takes an evaluation: each output's boxes,
   or from a kernel that gives text the box of its text, which an Output cell shows as it is; each
   message's text, the text after its MessagePacket; and each print's cell. A kernel that answers a
   client that is not a front end with the value itself, ReturnExpressionPacket[value], as
   Wolfram 15 does, is asked for the boxes of that Out[n], so that they are its own formatting *)
linkEntered[link_LinkObject, code_String] := Merge[
    Prepend[Map[linkStatementEntered[link, #] &, cellStatements[code]], <|"outs" -> {}, "msgs" -> {}, "prints" -> {}, "cells" -> {}|>],
    Apply[Join]
];
linkStatementEntered[link_LinkObject, statement_String] := (
    linkWritten[link, EnterExpressionPacket[MakeExpression[BoxData[statement], StandardForm]]];
    linkCaptured[link, linkPackets[link, _InputNamePacket]]
);

(* The statements of a cell's code as a front end divides it, at its line breaks: each run of
   top-level expressions that no line break separates, from the first to the last, without the
   comments around it. A kernel given a cell's text whole as boxes reads its newlines as spaces, so
   that two lines multiply; and a line that CodeParser reads as more than one expression, such as
   ??f, an error and a symbol, is one statement still, whose answer or message the kernel gives. *)
cellStatements[code_String] := Module[{tree, runs},
    Needs["CodeParser`"];
    tree = CodeParser`CodeConcreteParse[code, "SourceConvention" -> "SourceCharacterIndex"];
    If[ Head[tree] =!= CodeParser`ContainerNode, Return[{code}, Module]];
    runs = Map[
        Function[run, Select[run, Function[node, ! triviaNodeQ[node] && MatchQ[Lookup[Last[node], CodeParser`Source, None], {_Integer, _Integer}]]]],
        Split[tree[[2]], ! newlineNodeQ[#1] && ! newlineNodeQ[#2] &]
    ];
    Map[
        Function[run, StringTake[code, {First[Lookup[Last[First[run]], CodeParser`Source]], Last[Lookup[Last[Last[run]], CodeParser`Source]]}]],
        DeleteCases[runs, {}]
    ]
];
triviaNodeQ[node_] := MatchQ[node, CodeParser`LeafNode[Token`Newline | Token`Comment | Token`LineContinuation | Whitespace, _, _]];
newlineNodeQ[node_] := MatchQ[node, CodeParser`LeafNode[Token`Newline, _, _]];

linkCaptured[link_, packets_List] := Module[{afterMessage = False, outs = {}, msgs = {}, prints = {}, line = None},
    Scan[
        Function[packet, Switch[packet,
            Hold[_MessagePacket], afterMessage = True,
            Hold[TextPacket[_String]], If[afterMessage, AppendTo[msgs, StringTrim[packet[[1, 1]]]], AppendTo[prints, Cell[StringTrim[packet[[1, 1]], "\n"], "Print"]]];
                afterMessage = False,
            Hold[ExpressionPacket[BoxData[_, ___]]], AppendTo[prints, Cell[BoxData[packet[[1, 1, 1]]], "Print"]]; afterMessage = False,
            Hold[OutputNamePacket[_String]], line = Replace[StringCases[packet[[1, 1]], "Out[" ~~ n : DigitCharacter .. ~~ "]" :> FromDigits[n]], {{n_} :> n, _ -> None}],
            Hold[ReturnExpressionPacket[BoxData[_, ___]]], AppendTo[outs, packet[[1, 1, 1]]]; afterMessage = False,
            Hold[ReturnExpressionPacket[_]], If[ IntegerQ[line], AppendTo[outs, linkEvaluated[link, With[{n = line}, Hold[ToBoxes[Out[n], StandardForm]]]]]];
                afterMessage = False,
            Hold[ReturnTextPacket[_String]], AppendTo[outs, ToBoxes[packet[[1, 1]]]]; afterMessage = False,
            _, afterMessage = False
        ]],
        packets
    ];
    <|"outs" -> outs, "msgs" -> msgs, "prints" -> prints, "cells" -> {}|>
];

(* What a kernel keeps to start each page and section afresh: the context path and the names of
   Global`, where the cells are entered, that it had once the paclet loaded; a page's start loads
   the contexts it names, on that path, and a reset clears the names made since the baseline. *)
$evaluationSetup = Hold[
    PacletPageEvaluation`$contextPath = $ContextPath;
    PacletPageEvaluation`globalNames[] := StringDelete[Names["Global`*"], StartOfString ~~ "Global`"];
    PacletPageEvaluation`$initial = PacletPageEvaluation`$baseline = PacletPageEvaluation`globalNames[];
    PacletPageEvaluation`start[PacletPageEvaluation`path_List] := With[{PacletPageEvaluation`contexts = DeleteCases[PacletPageEvaluation`path, "System`" | "Global`"]},
        Scan[If[! MemberQ[$Packages, #], Needs[#]] &, PacletPageEvaluation`contexts];
        $ContextPath = DeleteDuplicates[Join[PacletPageEvaluation`contexts, PacletPageEvaluation`$contextPath]]
    ];
    PacletPageEvaluation`clear[PacletPageEvaluation`since_List] := With[{PacletPageEvaluation`names = Map["Global`" <> # &, Complement[PacletPageEvaluation`globalNames[], PacletPageEvaluation`since]]},
        If[ PacletPageEvaluation`names =!= {}, Unprotect @@ PacletPageEvaluation`names; ClearAll @@ PacletPageEvaluation`names]
    ];
];

(* The converter's evaluator on the link's kernel: the cells entered there, the page's contexts
   loaded there, the baseline the names of Global` at the setup's end, and a reset the clearing of
   those made since *)
linkEvaluator[link_LinkObject, spec_Association] := <|
    "Evaluate" -> Function[{code, options}, linkEntered[link, code]],
    "Start" -> Function[path, linkEvaluated[link, Hold[PacletPageEvaluation`start[path]]]],
    "Baseline" -> Function[linkEvaluated[link, Hold[PacletPageEvaluation`$baseline = PacletPageEvaluation`globalNames[]]]],
    "Reset" -> Function[linkEvaluated[link, Hold[PacletPageEvaluation`clear[PacletPageEvaluation`$baseline]]]],
    "Identity" -> spec["Key"]
|>;

(* A kernel of the spec launched where the spec starts it, once it answers: <|"Link" -> its link,
   "Process" -> {its process ID, its machine}|> *)
launchedKernel[spec_Association] := Module[{link},
    link = If[ StringQ[spec["Directory"]],
        WithCleanup[SetDirectory[spec["Directory"]], LinkLaunch[launchLine[spec["Command"]]], ResetDirectory[]],
        LinkLaunch[launchLine[spec["Command"]]]
    ];
    If[ Head[link] =!= LinkObject, evaluationKernelFailure["could not be launched by " <> launchLine[spec["Command"]]]];
    TimeConstrained[linkPackets[link, _InputNamePacket], $evaluationKernelStart,
        LinkClose[link]; evaluationKernelFailure["launched by " <> launchLine[spec["Command"]] <> " did not answer in " <> ToString[$evaluationKernelStart] <> " seconds"]];
    <|"Link" -> link, "Process" -> linkEvaluated[link, Hold[{$ProcessID, $MachineName}]]|>
];

(* a kernel for pages: launched, the paclet loaded on it, the initialization evaluated and the
   setup made *)
evaluationKernel[spec_Association] := With[{kernel = launchedKernel[spec]},
    With[{dir = spec["Paclet"], context = spec["Context"]}, linkEvaluated[kernel["Link"], Hold[PacletDirectoryLoad[dir]; If[StringQ[context], Needs[context]]]]];
    If[ MatchQ[spec["Initialization"], _Hold], linkEvaluated[kernel["Link"], spec["Initialization"]]];
    linkEvaluated[kernel["Link"], $evaluationSetup];
    kernel
];

(* A kernel closed: its link, and then its process, on this machine, if the process is still there
   two seconds after, as a kernel in the middle of an evaluation may be *)
closeEvaluationKernel[kernel_Association] := (
    If[ MemberQ[Links[], kernel["Link"]], Quiet[LinkClose[kernel["Link"]], {LinkObject::linkd, LinkObject::linkn}]];
    Replace[kernel["Process"], {pid_Integer, machine_String} /; machine === $MachineName && $OperatingSystem =!= "Windows" :>
        If[ NoneTrue[Range[20], (Pause[0.1]; RunProcess[{"kill", "-0", ToString[pid]}]["ExitCode"] =!= 0) &],
            RunProcess[{"kill", "-9", ToString[pid]}]]];
);

(* The kernel a page's examples are evaluated on: one of its own, or the template's, which the
   pages of the template share, each starting from the names it had when it was made. A template's
   kernel is this process's: parallel kernels are given the definitions, and this table with them,
   and each launches its own. A template's kernel that died since its last page is replaced. *)
$evaluationKernels = <||>;
pageEvaluationKernel[spec_Association] := If[ spec["Scope"] =!= "Template",
    evaluationKernel[spec],
    With[{key = {$ProcessID, spec["Template"], spec["Key"]}},
        If[ KeyExistsQ[$evaluationKernels, key] && ! FailureQ[Catch[
                linkEvaluated[$evaluationKernels[key]["Link"],
                    Hold[PacletPageEvaluation`clear[PacletPageEvaluation`$initial]; PacletPageEvaluation`$baseline = PacletPageEvaluation`$initial]],
                $evaluationKernelTag]],
            $evaluationKernels[key],
            If[ KeyExistsQ[$evaluationKernels, key], closeEvaluationKernel[$evaluationKernels[key]]];
            $evaluationKernels[key] = evaluationKernel[spec]
        ]
    ]
];

(* after a page: its own kernel closed; a template's kept for the next page, unless the page failed
   on it, which may have left it in the middle of an evaluation *)
pageEvaluationDone[spec_Association, kernel_, ok_] := Which[
    ! AssociationQ[kernel], Null,
    spec["Scope"] =!= "Template", closeEvaluationKernel[kernel],
    ! TrueQ[ok], closeEvaluationKernel[kernel]; $evaluationKernels = KeyDrop[$evaluationKernels, Key[{$ProcessID, spec["Template"], spec["Key"]}]]
];

closeEvaluationKernels[] := With[{own = Select[Keys[$evaluationKernels], First[#] === $ProcessID &]},
    Scan[closeEvaluationKernel, Lookup[$evaluationKernels, own]];
    $evaluationKernels = KeyDrop[$evaluationKernels, own]
];

(* One page converted into its work file, under a time limit: "OK", "Result". A converter that
   wrote its notebook and then hung still counts, when the notebook it wrote is whole. *)
(* a converted page, with the sections the converter reports: a page that resets its context at
   each heading names, with MarkdownToNotebook::section, what a section uses from an earlier one,
   since those names come out unevaluated without a message of their own *)
(* The Global` symbols a page gives definitions to are cleared after it: a page whose examples put
   Global` on their context path reads and assigns names there, and every page converted after it
   in the same kernel would see their values. *)
convertPage[convert_, task_Association, limit_] := globalsClearedAfter[globalState[], Module[
    {work = task["Work"], run, result, evaluation = Lookup[task, "Evaluation", None], kernel = None},
    If[ FileExistsQ[work], DeleteFile[work]];
    run = EvaluationData[TimeConstrained[
        withExampleStore[task, If[ AssociationQ[evaluation],
            Catch[
                kernel = pageEvaluationKernel[evaluation];
                convert[task["File"], work, "UseCache" -> task["UseCache"], "Evaluator" -> linkEvaluator[kernel["Link"], evaluation], Sequence @@ task["Options"]],
                $evaluationKernelTag
            ],
            convert[task["File"], work, "UseCache" -> task["UseCache"], Sequence @@ task["Options"]]
        ]],
        limit, $TimedOut
    ]
];
    result = run["Result"];
    If[ AssociationQ[evaluation], pageEvaluationDone[evaluation, kernel, ! MatchQ[result, _?FailureQ | $TimedOut | $Aborted]]];
    <|
        "OK" -> Which[
            FailureQ[result], False,
            result === $TimedOut, FileExistsQ[work] && SyntaxQ[ReadString[work]],
            True, FileExistsQ[work]
        ],
        "Result" -> If[FailureQ[result] || result === $TimedOut, result, Null],
        "Unevaluated" -> sectionReports[run["MessagesExpressions"]]
    |>
]];

(* the names in Global`, and those of them with no definitions *)
globalState[] := With[{names = Names["Global`*"]}, {names, Select[names, ! globalDefinedQ[#] &]}];

(* the result, after the Global` symbols with definitions that had none in the state, or were not
   there, are cleared: a symbol defined before keeps its definitions *)
globalsClearedAfter[{names_List, bare_List}, result_] := (
    Replace[Select[Union[Complement[Names["Global`*"], names], bare], globalDefinedQ], assigned : {__} :> ClearAll @@ assigned];
    result
);

(* the parser prepared for a converter in this kernel, once *)
preparedHere[f_] := With[{symbol = Replace[f, rf_ResourceFunction :> ResourceFunction[rf, "Function"]]},
    If[ Head[symbol] === Symbol,
        With[{context = With[{s = symbol}, Context[s]]},
            If[ ! TrueQ[$parserPrepared[context]], $parserPreparation[context]; $parserPrepared[context] = True]
        ]
    ]
];

globalDefinedQ[name_String] := AnyTrue[{OwnValues, DownValues, UpValues, SubValues}, #[name] =!= {} &];

sectionReports[messages_List] := Cases[messages,
    Hold[Message[MessageName[_, "section"], _, heading_String, names_List]] :> <|"Heading" -> heading, "Symbols" -> names|>];
sectionReports[_] := {};

(* a page converted in this kernel, its examples evaluated here or on the kernel its evaluation spec
   gives: the parallel kernels its examples launched are closed after it,
   so that none of them is left to the examples of the pages after it *)
convertHere[spec_Association, setup_Association][task_Association] := With[{before = Kernels[]},
    preparedHere[spec["Function"]];
    With[{done = convertPage[spec["Function"], task, setup["TimeConstraint"]]},
        closeKernels[Complement[Kernels[], before]];
        If[ setup["Monitor"], Print[If[done["OK"], "converted ", "failed "], task["Page"]]];
        done
    ]
];

(* The General messages switched off in this kernel. A script kernel switches General::shdw off and
   a parallel kernel leaves it on, so a parallel kernel takes the switches of the kernel that builds:
   a page's messages are the same whichever kernel converts it. *)
offGeneralMessages[] := Sort @ Cases[Messages[General],
    RuleDelayed[Verbatim[HoldPattern][HoldPattern[MessageName][General, tag_String]], _$Off] :> tag];
matchGeneralMessages[off_List] := (
    Scan[With[{tag = #}, Off[MessageName[General, tag]]] &, off];
    Scan[With[{tag = #}, On[MessageName[General, tag]]] &, Complement[offGeneralMessages[], off]]
);
matchGeneralMessages[_] := Null;

(* the conversion a parallel kernel runs, preparing the kernel the first time: a kernel the pool
   restarts after a crash keeps the distributed definitions, and nothing else *)
workerFunction[setup_Association] := With[
    {dirs = setup["Directories"], context = setup["Context"], load = setup["Load"], limit = setup["TimeConstraint"], monitor = setup["Monitor"],
     off = Lookup[setup, "Off", None], prepare = Lookup[setup, "Prepare", Function[Null, Null]]},
    Function[task,
        If[ ! TrueQ[$workerReady],
            matchGeneralMessages[off];
            Scan[PacletDirectoryLoad, dirs];
            If[ StringQ[context], Needs[context]];
            $workerConverter = ReleaseHold[load];
            If[ Head[$workerConverter] === Symbol, prepare[With[{s = $workerConverter}, Context[s]]]];
            $workerReady = True
        ];
        If[ FileExistsQ[timeoutMarker[task]],
            pageTimedOut[],
            With[{watchdog = pageWatchdog[task, limit]},
                With[{done = convertPage[$workerConverter, task, limit]},
                    If[ Head[watchdog] === ProcessObject, KillProcess[watchdog]];
                    If[ monitor, Print[If[done["OK"], "converted ", "failed "], task["Page"]]];
                    done
                ]
            ]
        ]
    ]
];

(* A page's time limit on a parallel kernel is kept from outside it as well. TimeConstrained stops
   only code that yields, and a page whose code never does would hold its kernel, and the build,
   for good. So each page has a watchdog process: past the limit and a grace it leaves a marker
   beside the page's work file and kills the kernel, with the processes the kernel started. A
   kernel that meets the marker leaves the page; it is converted once more on a parallel kernel,
   and failed as timed out when it is stopped again, never converted in the building kernel, which
   nothing could stop. *)
timeoutMarker[task_Association] := task["Work"] <> ".timeout";
pageTimedOut[] := <|"OK" -> False, "Result" -> $TimedOut, "Unevaluated" -> {}|>;
pageWatchdog[task_Association, limit_ ? NumericQ] /; $OperatingSystem =!= "Windows" := StartProcess[{
    "sh", "-c",
    "sleep \"$1\" || exit 0; : > \"$0\"; kids=$(pgrep -P \"$2\"); kill -9 \"$2\"; for k in $kids; do [ \"$k\" != \"$$\" ] && kill -9 \"$k\"; done",
    timeoutMarker[task], ToString[Ceiling[limit + Min[30, limit]]], ToString[$ProcessID]
}];
pageWatchdog[_, _] := None;
clearTimeoutMarkers[tasks_List] := Scan[If[FileExistsQ[timeoutMarker[#]], DeleteFile[timeoutMarker[#]]] &, tasks];

(* The pages converted, in order: one by one in this kernel, or on the parallel kernels after the
   first, which this kernel converts so that whatever the converter installs is in place before
   they look for it; a page that fails on a parallel kernel is tried once more here. *)
convertTasks[tasks_List, spec_, setup_, kernels_] := With[{done = tasksConverted[tasks, spec, setup, kernels]},
    closeEvaluationKernels[];
    (* a kernel still busy is not waited for: it is killed when the build closes its kernels *)
    (* nor is one asked that was killed past a page's time: the kernels there are now *)
    With[{live = Intersection[kernels, Kernels[]]}, If[ live =!= {}, TimeConstrained[ParallelEvaluate[closeEvaluationKernels[], live], 60]]];
    done
];
tasksConverted[{}, _, _, _] := {};
tasksConverted[tasks_List, spec_, setup_, {}] := Map[convertHere[spec, setup], tasks];
tasksConverted[{task_}, spec_, setup_, _] := {convertHere[spec, setup][task]};
tasksConverted[tasks_List, spec_, setup_, kernels_List] := Module[{first, rest, worker, again},
    first = convertHere[spec, setup][First[tasks]];
    DistributeDefinitions[convertPage, withExampleStore, matchGeneralMessages, closeEvaluationKernels, timeoutMarker, pageTimedOut, pageWatchdog];
    clearTimeoutMarkers[Rest[tasks]];
    If[ MatchQ[spec["Distribute"], _Hold], DistributeDefinitions @@ spec["Distribute"]];
    worker = workerFunction[Join[setup, <|"Directories" -> DeleteDuplicates[Join[setup["Directories"], parserDirectories[]]], "Off" -> offGeneralMessages[]|>]];
    rest = ParallelMap[worker, Rest[tasks], Method -> "FinestGrained"];
    (* a page whose kernel was killed past its time is converted once more on the kernels there are
       now: a page that hangs now and then gets through, and one that always does fails *)
    again = Select[Range[Length[rest]], FileExistsQ[timeoutMarker[Rest[tasks][[#]]]] &];
    If[ again =!= {} && Kernels[] =!= {},
        If[ setup["Monitor"], Scan[Print["again ", #["Page"], ": stopped past its time"] &, Rest[tasks][[again]]]];
        clearTimeoutMarkers[Rest[tasks][[again]]];
        rest = ReplacePart[rest, Thread[again -> ParallelMap[worker, Rest[tasks][[again]], Method -> "FinestGrained"]]]
    ];
    rest = MapThread[
        Which[
            TrueQ[Lookup[Replace[#2, Except[_Association] -> <||>], "OK", False]], #2,
            FileExistsQ[timeoutMarker[#1]], If[setup["Monitor"], Print["failed ", #1["Page"], ": stopped past its time"]]; pageTimedOut[],
            True, convertHere[spec, setup][#1]
        ] &,
        {Rest[tasks], rest}
    ];
    clearTimeoutMarkers[Rest[tasks]];
    Prepend[rest, first]
];

(* the page cache's key: everything a page's notebook depends on. The guides' titles are in it
   because every page's Related Guides links show them. *)
$pageCacheVersion = "2";
pageKey[task_Association, converterID_, code_String, guides_List] := digest[
    Join[
        {$pageCacheVersion, task["Page"], FileHash[task["File"], "SHA256"], task["Options"], converterID, code, guides},
        Replace[Lookup[task, "Evaluation", None], {spec_Association :> {spec["Key"]}, _ -> {}}]
    ]
];

(* "EvaluateSeparator": the converter's, one value for every page or an association by template;
   by default None for tutorials and Overview pages, whose later sections use what earlier ones
   define, and for every other page the converter's own default: each section, and each example
   between ---, in a clean context;
   "ConverterOptions": options for every MarkdownToNotebook call, such as "MathFont", or an
   association of a page's file name, or a string pattern for it, to that page's options;
   "Files": All, or the markdown files (or a string pattern for their names) to build *)
(* a tutorial or an Overview page is one narrative: its later sections use what its earlier ones
   define, so it runs in one context *)
$separatorByTemplate = <|"TechNote" -> None, "Overview" -> None|>;

(* a page's converter options: one list for every page, or an association whose entries a page's
   file name matches, a name or a string pattern for it, each matching entry's options in turn *)
converterOptions[options_List, _String] := options;
converterOptions[byPage_Association, file_String] := Join @@ KeyValueMap[If[StringMatchQ[FileNameTake[file], #1], #2, {}] &, byPage];
converterOptions[_, _] := {};

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
    "CacheExcludedFiles" -> {},
    "CacheVersion" -> True,
    "CacheKey" -> Automatic,
    "EvaluationKernel" -> None,
    "Monitor" -> False
};

BuildPacletDocumentation::dir = "`1` is not a paclet directory: it has no PacletInfo.wl.";
BuildPacletDocumentation::source = "`1` has no markdown documentation.";
BuildPacletDocumentation::page = "`1` did not become a notebook: `2`.";
BuildPacletDocumentation::flagged = "`1` documentation pages carry a flag the documentation build leaves a page out for, from a Flag frontmatter key, a flag cell or the palette's flag buttons: `2`.";
BuildPacletDocumentation::nodefinition = "No markdown definition was found, as ResourceDefinition.md in `1` or a Template: Paclet page under `2`, so `3` was not rebuilt; a deploy would publish it as it is. Give \"Definition\", or \"Definition\" -> None to build without one.";

(* a build, as a deploy, keeps the kernel's asynchronous site update and information refresh from
   starting while it runs (see $sitesRefreshed) *)
BuildPacletDocumentation[dir_String, opts : OptionsPattern[]] := Block[{$programHashes = <||>,
    ResourceSystemClient`$AsyncronousResourceInformationUpdates = False, PacletManager`Services`Private`doWeeklyUpdate = Null &}, buildPacletDocumentation[dir, opts]];
buildPacletDocumentation[dir_String, opts : OptionsPattern[BuildPacletDocumentation]] := Module[
    {
        paclet, source, target, spec, cache, pagesDir, workDir, manifestFile, evaluated, codes, keyed, internals, examplesFile, examples, store, cacheSymbol, useCache, separator,
        all, files, metas, guides, definitionSource, tasks, misses, kernelsBefore, kernels, setup, results, outcomes,
        ok, failed, pruned, pageFiles, skipped, unevaluated, flagged, kernelPIDs
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
    (* the paclet itself, and the dependencies given as directories, for the examples *)
    PacletDirectoryLoad[dir];
    Scan[PacletDirectoryLoad, Select[Replace[OptionValue["Dependencies"], Except[_List] -> {}], StringQ[#] && DirectoryQ[#] &]];
    paclet = PacletObject[File[dir]];
    (* a paclet whose examples run on another kernel loads there, and need not load here *)
    If[ StringQ[paclet["PrimaryContext"]] && OptionValue["EvaluationKernel"] === None, Needs[paclet["PrimaryContext"]]];
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
                "Options" -> Join[{"EvaluateSeparator" -> separator[template]}, converterOptions[OptionValue["ConverterOptions"], file]],
                "Evaluation" -> evaluationSpec[OptionValue["EvaluationKernel"], template, dir, paclet["PrimaryContext"]]
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
            "Options" -> Join[{"EvaluateSeparator" -> separator["Paclet"]}, converterOptions[OptionValue["ConverterOptions"], definitionSource]],
            "Evaluation" -> evaluationSpec[OptionValue["EvaluationKernel"], "Paclet", dir, paclet["PrimaryContext"]]
        |>]
    ];
    (* the code each page's examples run against: what the page uses of the paclet's code, or all
       of it, its dependencies' and the converter's *)
    keyed = pageCodes[OptionValue["CacheKey"], dir, paclet, tasks, <|
        "Dependencies" -> OptionValue["Dependencies"], "Excluded" -> Flatten[{OptionValue["CacheExcludedFiles"]}],
        "Versioned" -> OptionValue["CacheVersion"], "Converter" -> spec["ID"], "EvaluationKernel" -> OptionValue["EvaluationKernel"],
        "Cache" -> cache, "Work" -> workDir, "Monitor" -> TrueQ[OptionValue["Monitor"]]
    |>];
    codes = keyed["Codes"];
    If[ TrueQ[OptionValue["Monitor"]] && keyed["Keys"] =!= None, Print["definitions: ", Length[codes], " pages keyed"]];
    (* with the definitions scanned, the code each example of the converter's cache was evaluated
       against *)
    internals = If[keyed["Keys"] === None, None, converterInternals[spec]];
    (* the converter's example results, in the cache directory *)
    store = PersistenceLocation["Local", FileNameJoin[{cache, "examples"}]];
    If[ ! DirectoryQ[FileNameJoin[{cache, "examples"}]], CreateDirectory[FileNameJoin[{cache, "examples"}], CreateIntermediateDirectories -> True]];
    cacheSymbol = converterCacheSymbol[spec];
    If[ internals =!= None, internals = Append[internals, "$cacheLocation" -> store]];
    examplesFile = FileNameJoin[{cache, "examples.wxf"}];
    (* the code each result was evaluated against, by its name in the store, and each page's examples *)
    examples = Replace[If[FileExistsQ[examplesFile], Import[examplesFile, "WXF"], <||>], Except[_Association] -> <||>];
    examples = If[ KeyExistsQ[examples, "Codes"], examples, <|"Codes" -> examples, "Pages" -> <||>|>];
    tasks = Map[
        Function[task, With[{code = codes[task["Page"]]}, With[{key = pageKey[task, spec["ID"], code, guides]},
            Join[task, <|
                "Code" -> code,
                "Entry" -> FileNameJoin[{pagesDir, key <> ".nb"}],
                "Work" -> FileNameJoin[{workDir, key <> ".nb"}],
                "Cached" -> useCache && FileExistsQ[FileNameJoin[{pagesDir, key <> ".nb"}]],
                "UseCache" -> useCache && Lookup[evaluated, task["Page"], None] === code
            |>]
        ]]],
        tasks
    ];
    tasks = Map[Join[#, <|"CacheSymbol" -> cacheSymbol, "ExampleStore" -> store|>] &, tasks];
    (* a page to convert whose examples have codes has the results of the ones whose code changed
       deleted from the converter's cache, and uses the rest *)
    If[ internals =!= None,
        tasks = progressMap[
            Function[task, If[ Lookup[task, "Evaluation", None] =!= None || task["Cached"], task,
                With[{cells = pageExamples[internals, keyed["Keys"], task]},
                    If[ ! useCache, Append[task, "Examples" -> cells],
                        With[{held = cachedExamples[internals, cells]},
                            With[{stale = Select[cells, MemberQ[held, #Name] && Lookup[examples["Codes"], #Name, None] =!= #Code &]},
                                Scan[DeleteObject[PersistentObject[#Name, internals["$cacheLocation"]]] &, stale];
                                Join[task, <|"Examples" -> cells, "UseCache" -> True, "Fresh" -> Length[held] - Length[stale] < Length[cells]|>]
                            ]
                        ]
                    ]
                ]
            ]],
            tasks, TrueQ[OptionValue["Monitor"]], "examples: pages read"
        ]
    ];
    misses = Select[tasks, ! #Cached &];
    If[ TrueQ[OptionValue["Monitor"]],
        Print[Length[tasks], " pages: ", Length[tasks] - Length[misses], " cached, ", Length[misses], " to convert"]];
    kernelsBefore = Kernels[];
    kernels = If[ Length[misses] > 1, buildKernels[OptionValue["Parallel"]], {}];
    kernelPIDs = If[kernels === {}, {}, kernelProcessIDs[Complement[Kernels[], kernelsBefore]]];
    setup = <|
        "Directories" -> DeleteDuplicates[Append[pacletDirectories[], dir]], "Load" -> spec["Load"], "Prepare" -> $parserPreparation,
        "Context" -> If[OptionValue["EvaluationKernel"] === None, paclet["PrimaryContext"], None],
        "TimeConstraint" -> OptionValue["TimeConstraint"], "Monitor" -> TrueQ[OptionValue["Monitor"]]
    |>;
    results = AssociationThread[Lookup[misses, "Entry", {}], convertTasks[misses, spec, setup, kernels]];
    If[ kernels =!= {}, closeKernels[Complement[Kernels[], kernelsBefore], kernelPIDs]];
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
    evaluated = Join[evaluated, Association[#Page -> #Code & /@ ok]];
    (* the converter's cache holds the results of a converted page's examples that evaluated
       cleanly, under the code they were evaluated against *)
    If[ internals =!= None,
        Scan[
            Function[task, With[{held = cachedExamples[internals, task["Examples"]]},
                examples["Codes"] = Join[examples["Codes"], Association[#Name -> #Code & /@ Select[task["Examples"], MemberQ[held, #Name] &]]];
                examples["Pages"] = Append[examples["Pages"], task["Page"] -> Lookup[task["Examples"], "Name", {}]]
            ]],
            Select[Pick[tasks, outcomes, "Converted"], KeyExistsQ[#, "Examples"] &]
        ]
    ];
    failed = Lookup[Pick[tasks, outcomes, "Failed"], "File", {}];
    DeleteDirectory[workDir, DeleteContents -> True];
    (* after a build of every page, the cache keeps what this build used *)
    If[ OptionValue["Files"] === All,
        Scan[DeleteFile, Complement[FileNames["*.nb", pagesDir], Lookup[tasks, "Entry"]]];
        evaluated = KeyTake[evaluated, Lookup[tasks, "Page"]];
        (* and the store, the results of the examples its pages have *)
        If[ internals =!= None,
            examples["Pages"] = KeyTake[examples["Pages"], Lookup[tasks, "Page"]];
            With[{current = Catenate[Values[examples["Pages"]]]},
                examples["Codes"] = KeyTake[examples["Codes"], current];
                Scan[DeleteObject[PersistentObject[#, store]] &, Complement[storedNames[store, current], current]]
            ]
        ]
    ];
    Put[evaluated, manifestFile];
    If[ internals =!= None, Export[examplesFile, examples, "WXF"]];
    (* the notebooks of pages whose markdown is gone: only after a whole build with no failure,
       since a page that did not convert keeps its last notebook *)
    pruned = If[ TrueQ[OptionValue["Prune"]] && OptionValue["Files"] === All && failed === {},
        With[{stale = Complement[FileNames["*.nb", target, Infinity], Lookup[Select[ok, #Page =!= "ResourceDefinition" &], "Out", {}]]},
            Scan[DeleteFile, stale]; stale],
        {}
    ];
    flagged = flaggedPages[target];
    If[ flagged =!= <||>, Message[BuildPacletDocumentation::flagged, Length[flagged], Take[Normal[flagged], UpTo[10]]]];
    <|
        "Notebooks" -> Lookup[Select[ok, #Page =!= "ResourceDefinition" &], "Out", {}],
        "Definition" -> Replace[Lookup[Select[ok, #Page === "ResourceDefinition" &], "Out", {}], {{out_} :> out, _ -> None}],
        "Failed" -> failed,
        "Flagged" -> flagged,
        "Converted" -> Lookup[Pick[tasks, outcomes, "Converted"], "File", {}],
        "Cached" -> Lookup[Pick[tasks, outcomes, "Cached"], "File", {}],
        "Evaluated" -> Lookup[Select[Pick[tasks, outcomes, "Converted"], Lookup[#, "Fresh", ! #UseCache] &], "File", {}],
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
     overwrites them anyway; the deletions go, so they stay in place while they are replaced;
   - the documentation directory is deleted and copied whole by a copy that cannot overwrite and
     ignores a file it fails to write; it is replaced in place instead, file by file
     (replaceDocumentation), and a failed write stops the deploy (stopOnCloudWriteFailure). *)
$deployFixesInstalled = False;
$appearanceElements = None;

(* The first failed cloud write stops the deploy, as a Failure naming the step it stopped at: the
   stock deploy carries on past one, and then deploys a resource page built from the failure. *)
$cloudWriteFailures = "srverr" | "notperm" | "notauth" | "unavailable" | "cloudprecondition";
$deployStep = None;
$deployMonitor = False;

(* a step of the deploy, or of another part of a publish, printed when it is monitored *)
monitor[step_] := monitor["deploy", step];
monitor[part_String, step_] := If[TrueQ[$deployMonitor], Print[part, ": ", step]];

SetAttributes[stopOnCloudWriteFailure, HoldFirst];
stopOnCloudWriteFailure[expr_] := Block[
    (* the stock deploy names each step as it starts it *)
    {$deployStep = None, PacletResource`Private`setDNCProgress = Function[step, $deployStep = step; monitor[step]]},
    Catch[Internal`HandlerBlock[{"Message", cloudWriteFailure}, expr], "cloudwrite"]
];

(* a failed write stops the deploy whether its message shows or is captured, as in a test or a
   documentation example *)
cloudWriteFailure[Hold[Message[MessageName[symbol_, tag_String], ___], _]] /; ! TrueQ[$cloudWriteRetry] && MatchQ[tag, $cloudWriteFailures] :=
    Throw[cloudWriteFailureObject[messageText[symbol, tag], SymbolName[Unevaluated[symbol]] <> "::" <> tag], "cloudwrite"];
cloudWriteFailure[_] := Null;

cloudWriteFailureObject[what_String, name_String] := Failure["CloudWrite", <|
    "MessageTemplate" -> "The deploy stopped at \"`1`\": `2`",
    "MessageParameters" -> {Replace[$deployStep, None -> "its start"], what},
    "Step" -> $deployStep,
    "CloudMessage" -> name
|>];

(* A file copied to the cloud, sent again when the cloud fails it, three times in all, after five
   seconds and then fifteen: the cloud now and then fails a write, with CopyFile::cloudunknown, that
   succeeds when sent again. An attempt that is tried again keeps its messages off the output, the
   one purpose of this Quiet, and does not stop the deploy; the last attempt's messages show, and
   stop it as any failed write does. Each attempt has a time limit, two minutes and ten seconds a
   megabyte of the file: an upload can stall without failing, as full-size packets die on some
   VPNs, and a stalled attempt is cut and sent again rather than waited on. *)
$cloudWriteRetry = False;
$cloudCopyPauses = {5, 15};
$cloudCopyTime = {120, 10};
cloudCopyLimit[from_] := First[$cloudCopyTime] + Last[$cloudCopyTime] * If[StringQ[from] && FileType[from] === File, FileByteCount[from] / 10.^6, 0];
(* the archive and the files the stock deploy copies, sent the same way *)
$cloudCopyRule = HoldPattern[CopyFile[from_, to_, OverwriteTarget -> True]] :> cloudCopy[from, to];
cloudCopy[from_, to_] := cloudCopy[from, to, $cloudCopyPauses];
cloudCopy[from_, to_, {}] := TimeConstrained[CopyFile[from, to, OverwriteTarget -> True], cloudCopyLimit[from],
    Message[DeployPacletResource::stalled, from, Round[cloudCopyLimit[from]]]; $Failed];
cloudCopy[from_, to_, {pause_, rest___}] := Replace[
    Block[{$cloudWriteRetry = True}, Quiet[TimeConstrained[CopyFile[from, to, OverwriteTarget -> True], cloudCopyLimit[from], $TimedOut]]],
    Except[_CloudObject] :> (Pause[pause]; cloudCopy[from, to, {rest}])
];

messageText[symbol_, tag_String] := With[{text = MessageName[symbol, tag]},
    If[StringQ[text], text, Replace[MessageName[General, tag], Except[_String] -> tag]]
];

(* The documentation, replaced in place, file by file, rather than deleted and copied whole: a
   deploy that stops leaves every page served. A file is written only when the cloud lacks it or
   its content changed since the resource's record of what it holds was written; with
   "Upload" -> All or "Documentation", every file is. A file not written stops the deploy, and the record keeps only
   what was written; the pages the new documentation no longer has are deleted once the deploy is
   complete (pruneDocumentation). *)
$deployedDocumentation = None;

(* the empty TaggingRules entries a page's trip through the documentation build's front end leaves
   on some builds and not on others, of the same sources *)
$frontEndNoise = ("WorkflowDockedCell" | "SearchTextTranslated") -> "";

(* what every build or deploy of a page makes afresh, which says nothing of its content: its cells'
   ids and change times, the time it was built, and the front end's empty entries *)
$buildNoise = ((ExpressionUUID | CellID | CellChangeTimes) -> _) | ("built" -> _) | $frontEndNoise;

(* an expression without its build noise, removed outright at every level, as a held notebook
   would keep the Sequence[] a replacement leaves, and with the Module locals its outputs hold
   numbered by where they first appear: a page evaluated again names them by another session's
   numbers, store$12143 for store$9876, and says the same *)
withoutNoise[expr_] := withoutLocalNumbers[DeleteCases[expr, $buildNoise, Infinity]];

(* each Module local, a symbol whose name ends in $ and digits, or a string that is such a name with
   three digits or more, as a box shows it, replaced by local[n], n its place among the distinct
   names in order of first appearance, held; a name in two contexts is one local *)
withoutLocalNumbers[expr_] := Module[{found, names, rules},
    found = Cases[expr, s : (_Symbol | _String) /; localQ[s] :> {HoldPattern[s], localKey[s]}, {0, Infinity}, Heads -> True];
    If[ found === {}, Return[expr, Module]];
    names = AssociationThread[DeleteDuplicates[found[[All, 2]]] -> Range[Length[DeleteDuplicates[found[[All, 2]]]]]];
    rules = Dispatch[DeleteDuplicates[Map[With[{n = names[#[[2]]]}, #[[1]] -> local[n]] &, found]]];
    Replace[expr, rules, {0, Infinity}, Heads -> True]
];

SetAttributes[{localQ, localKey}, HoldAllComplete];
localQ[s_Symbol] := StringMatchQ[SymbolName[Unevaluated[s]], $localNamePattern];
localQ[s_String] := StringContainsQ[s, "$"] && StringMatchQ[localName[s], $localNamePattern] && StringMatchQ[localName[s], __ ~~ "$" ~~ Repeated[DigitCharacter, {3, Infinity}]];
localQ[_] := False;
localKey[s_Symbol] := SymbolName[Unevaluated[s]];
localKey[s_String] := localName[s];
localName[s_String] := Last[StringSplit[s, "`"], s];
$localNamePattern = LetterCharacter ~~ (LetterCharacter | DigitCharacter) ... ~~ "$" ~~ DigitCharacter ..;

(* what a deploy writes and makes again whatever the record says: "Documentation", "Pages" *)
$rewrite = {};
rewriteQ[kind_String] := MemberQ[$rewrite, kind];
rewriteKinds[All] := {"Documentation", "Pages"};
rewriteKinds[kind_String] := {kind};
rewriteKinds[kinds : {___String}] := kinds;
rewriteKinds[_] := {};

replaceDocumentation[dir_String, root_CloudObject, permissions_] := Module[
    {target = FileNameJoin[{root, "Documentation"}], relatives, existing, record, keys, changed, written, failure},
    relatives = relativePath[dir, #] & /@ Select[FileNames["*", dir, Infinity], FileType[#] === File &];
    existing = cloudFiles[target];
    (* the files to write are never found from a listing that failed *)
    If[ existing === $Failed,
        Throw[cloudWriteFailureObject["the listing of the deployed documentation, " <> First[target] <> ", failed", "CloudObjects"], "cloudwrite"]];
    record = If[rewriteQ["Documentation"], <||>, readDeployRecord[root, "Documentation"]];
    keys = AssociationMap[contentKey[FileNameJoin[{dir, #}]] &, relatives];
    changed = changedFiles[keys, existing, record];
    monitor["documentation: " <> ToString[Length[changed]] <> " of " <> ToString[Length[relatives]] <> " files to write"];
    {written, failure} = writeDocumentation[dir, target, permissions, changed];
    writeDeployRecord[root, "Documentation", Join[KeyTake[record, Complement[relatives, changed]], KeyTake[keys, written]]];
    If[ failure =!= None, Throw[failure, "cloudwrite"]];
    setDocumentationPermissions[target, relatives, permissions];
    $deployedDocumentation = <|"Root" -> root, "Target" -> target, "Files" -> relatives, "Existing" -> Keys[existing], "Written" -> Length[written]|>;
    $pages = Join[$pages, <|"Root" -> root, "Existing" -> existing, "Keys" -> keys|>,
        With[{record = If[rewriteQ["Pages"], <||>, readDeployRecord[root, "Pages"]]}, <|"Signatures" -> record, "Recorded" -> record|>]];
    documentationObject[target, #, permissions] & /@ relatives
];

(* The documentation notebooks the stock build leaves out for their flag, by page, the flag: a flag
   cell, or the screen environment the palette's flag buttons set (Future sets "FutureObject"),
   read as the build reads them, with DocumentFlag, for a notebook whose text names either *)
$skippedFlags = <|"ExcisedFlag" -> "Excised", "FutureFlag" -> "Future", "InternalFlag" -> "Internal", "TemporaryFlag" -> "Temporary"|>;

flaggedPages[docsDir_String] := With[
    {candidates = If[DirectoryQ[docsDir], Select[FileNames["*.nb", docsDir, Infinity], flagCandidateQ], {}]},
    If[ candidates === {}, <||>,
        Needs["DocumentationBuild`"];
        Association @ Map[
            With[{flag = DocumentationBuild`Utils`DocumentFlag[#]},
                If[KeyExistsQ[$skippedFlags, flag], relativePath[docsDir, #] -> $skippedFlags[flag], Nothing]] &,
            candidates
        ]
    ]
];
flaggedPages[_] := <||>;

flagCandidateQ[file_String] := With[{text = ReadString[file]}, StringQ[text] && StringContainsQ[text, "Flag\"" | "ScreenStyleEnvironment"]];

(* the files to write: those the cloud lacks, and those whose content changed since the record *)
changedFiles[keys_Association, existing_Association, record_Association] :=
    Select[Keys[keys], ! KeyExistsQ[existing, #] || Lookup[record, #] =!= keys[#] &];

(* a notebook file's expressions, parsed but not evaluated, without the Null that Wolfram 15 gives
   for each comment or blank line before or between them and other kernels do not *)
heldNotebook[file_String] := Replace[ToExpression[ReadString[file], InputForm, HoldComplete], held_HoldComplete :> DeleteCases[held, Null]];

(* A file's content, for telling whether it changed. A notebook's is its expression, parsed but not
   evaluated, without what every build or deploy makes afresh: the random ids of its cells, and of
   the cells the build adds, the times its cells changed, and the time the page was built. *)
contentKey[file_String] := If[ ToLowerCase[FileExtension[file]] === "nb",
    With[{held = heldNotebook[file]},
        If[ MatchQ[held, _HoldComplete],
            digest[withoutNoise[held]],
            FileHash[file, "SHA256", "HexString"]
        ]
    ],
    FileHash[file, "SHA256", "HexString"]
];

(* The files, written in blocks: over the parallel kernels when they act as this kernel's account,
   and none after a block with a file not written. The files written, and the failure or None. *)
writeDocumentation[dir_String, target_CloudObject, permissions_, relatives_List] := Module[{written = {}, failure},
    failure = Catch[
        Scan[
            Function[block, With[
                {results = mapBlocks[
                    Function[task, MatchQ[cloudCopy[First[task], Last[task]], _CloudObject]],
                    {FileNameJoin[{dir, #}], documentationObject[target, #, permissions]} & /@ block
                ]},
                written = Join[written, Pick[block, results, True]];
                If[ Length[relatives] > Length[block], monitor["documentation: " <> ToString[Length[written]] <> " of " <> ToString[Length[relatives]] <> " written"]];
                If[ ! AllTrue[results, TrueQ],
                    Throw[cloudWriteFailureObject[First[documentationObject[target, First[Pick[block, results, False]], Automatic]] <> " was not written.", "CopyFile"], "cloudwrite"]]
            ]],
            Partition[relatives, UpTo[50]]
        ];
        None,
        "cloudwrite"
    ];
    {written, failure}
];

documentationObject[target_CloudObject, relative_String, permissions_] := PacletResource`Private`addEmbeddedOptions @ If[ permissions === Automatic,
    FileNameJoin[Prepend[FileNameSplit[relative], target]],
    CloudObject[First[FileNameJoin[Prepend[FileNameSplit[relative], target]]], Permissions -> permissions]
];

(* The documentation's directories get the resource's permissions, as the stock deploy gives
   them: the documentation directory and each directory a file is in *)
setDocumentationPermissions[_, _, Automatic] := Null;
setDocumentationPermissions[target_CloudObject, relatives_List, permissions_] := With[
    {directories = Prepend[FileNameJoin[Prepend[FileNameSplit[#], target]] & /@ documentationDirectories[relatives], target]},
    With[{results = mapBlocks[MatchQ[SetPermissions[#, permissions], _CloudObject | {___}] &, directories]},
        If[ ! AllTrue[results, TrueQ],
            Throw[cloudWriteFailureObject["The permissions of " <> First[First[Pick[directories, results, False]]] <> " were not set.", "SetPermissions"], "cloudwrite"]]
    ]
];

(* the directories of the files, by their paths, each directory once *)
documentationDirectories[relatives_List] := DeleteDuplicates @ Catenate @ Map[
    Function[parts, FileNameJoin[Take[parts, #]] & /@ Range[Length[parts] - 1]],
    FileNameSplit /@ relatives
];

(* the files beneath a cloud directory, by their paths within it, none when it is not there: two
   listings for each directory *)
cloudFiles[dir_CloudObject] := If[ ! TrueQ[DirectoryQ[dir]], <||>,
    With[{directories = cloudDirectories[dir]},
        If[ directories === $Failed, $Failed,
            With[{files = cloudListing[#] & /@ directories, subdirectories = cloudListing[#, "Directory"] & /@ directories},
                If[ MemberQ[Join[files, subdirectories], $Failed], $Failed,
                    AssociationMap[True &, cloudRelativePath[dir, #] & /@ Catenate[MapThread[Complement, {files, subdirectories}]]]
                ]
            ]
        ]
    ]
];

(* the path of a cloud object within a cloud directory *)
cloudRelativePath[target_CloudObject, object_CloudObject] :=
    FileNameJoin[URLDecode /@ StringSplit[StringDrop[First[object], StringLength[First[target]]], "/"]];

(* The resource's record of what it holds, private, beside it: each documentation file's content,
   and each page's signature *)
recordObject[root_CloudObject, kind_String] := FileNameJoin[{root, "DeployRecord", kind <> ".wl"}];

readDeployRecord[root_CloudObject, kind_String] := With[{object = recordObject[root, kind]},
    If[ TrueQ[FileExistsQ[object]], Replace[CloudGet[object], Except[_Association] -> <||>], <||>]
];

writeDeployRecord[root_CloudObject, kind_String, record_Association] := CloudPut[record, recordObject[root, kind], Permissions -> "Private"];

pruneDocumentation[] := If[ ! AssociationQ[$deployedDocumentation], 0,
    With[{target = $deployedDocumentation["Target"], stale = staleDocumentation[$deployedDocumentation["Existing"], $deployedDocumentation["Files"]]},
        If[stale =!= {}, monitor["documentation: removing " <> ToString[Length[stale]] <> " files the documentation no longer has"]];
        Scan[DeleteObject[FileNameJoin[Prepend[FileNameSplit[#], target]]] &, stale];
        If[stale =!= {}, monitor["documentation: " <> ToString[Length[stale]] <> " files removed"]];
        Length[stale]
    ]
];

(* The page each documentation notebook is shown in is made again only when what it is made from
   changed: its template, the resource's values the template shows, the sidebar, which lists every
   page, the notebook's URL and the notebook itself, since the page keeps the notebook's render
   from the first time it is read. The resource's record keeps each page's signature, saved every
   hundred pages and once the deploy is done.
   The stock sidebar asks for every page, one after another, each a cloud deploy of its own. The
   pages it asks for are queued and made once it is done: over the parallel kernels, in blocks,
   when every kernel acts as this kernel's account and makes a page's deploy exactly as this
   kernel does, else here. A page not made on a parallel kernel is made again here. *)
$pages = <||>;
$pageQueue = None;

newPages[] := <|"Root" -> None, "Existing" -> <||>, "Keys" -> <||>, "Signatures" -> <||>, "Recorded" -> <||>, "Made" -> 0, "Unchanged" -> 0, "Seen" -> {}, "Context" -> None|>;

pageRule[] := HoldPattern[PacletResource`Private`createDocumentationEmbedPage[as_Association, sidebar_String, url_String] /; ! TrueQ[$makingPage]] :>
    makePage[as, sidebar, url];

sidebarRule[] := HoldPattern[PacletResource`Private`createSidebarHTML[as_Association, nbs : {__CloudObject}] /; $pageQueue === None] :>
    Block[{$pageQueue = {}},
        With[{html = PacletResource`Private`createSidebarHTML[as, nbs]}, makeQueuedPages[]; html]
    ];

installPageRule[] := If[ DownValues[PacletResource`Private`createDocumentationEmbedPage] =!= {},
    Unprotect[PacletResource`Private`createDocumentationEmbedPage, PacletResource`Private`createSidebarHTML];
    DownValues[PacletResource`Private`createDocumentationEmbedPage] = Prepend[DownValues[PacletResource`Private`createDocumentationEmbedPage], pageRule[]];
    If[ DownValues[PacletResource`Private`createSidebarHTML] =!= {},
        DownValues[PacletResource`Private`createSidebarHTML] = Prepend[DownValues[PacletResource`Private`createSidebarHTML], sidebarRule[]]];
    True,
    False
];

makePage[as_Association, sidebar_String, url_String] := Module[{signature},
    If[ $pages["Context"] === None, $pages["Context"] = pageContext[as]];
    signature = digest[{$pages["Context"], sidebar, url, Lookup[$pages["Keys"], pageRelative[StringReplace[url, ".html" ~~ EndOfString -> ".nb"]], None]}];
    $pages["Seen"] = Append[$pages["Seen"], url];
    Which[
        Lookup[$pages["Signatures"], url] === signature && pageExistsQ[url],
            $pages["Unchanged"]++;
            CloudObject[url],
        ListQ[$pageQueue],
            AppendTo[$pageQueue, {as, sidebar, url, signature}];
            CloudObject[url],
        True,
            pageMade[url, signature, pageHere[as, sidebar, url]]
    ]
];

pageHere[as_, sidebar_, url_] := Block[{$makingPage = True}, PacletResource`Private`createDocumentationEmbedPage[as, sidebar, url]];

(* a page made: its signature kept, and the record saved every hundred pages *)
pageMade[url_String, signature_String, made_] := (
    If[ MatchQ[made, _CloudObject],
        $pages["Signatures", url] = signature;
        $pages["Made"]++;
        If[ Mod[$pages["Made"], 100] === 0 && MatchQ[$pages["Root"], _CloudObject], writeDeployRecord[$pages["Root"], "Pages", $pages["Signatures"]]]
    ];
    made
);

(* the queued pages, those of one sidebar together *)
makeQueuedPages[] := With[{queue = $pageQueue},
    $pageQueue = {};
    Scan[makePages[#[[1, 1]], #[[1, 2]], #[[All, 3 ;; 4]]] &, Split[queue, #1[[1]] === #2[[1]] && #1[[2]] === #2[[2]] &]]
];

makePages[as_, sidebar_, pages_List] := If[ Length[pages] > 1 && Length[Kernels[]] > 0 && parallelSameAccountQ[] && pageSetup[as, sidebar, First[First[pages]]],
    WithCleanup[
        Scan[
            Function[block,
                MapThread[
                    pageMade[First[#1], Last[#1], If[MatchQ[#2, _CloudObject], #2, pageHere[as, sidebar, First[#1]]]] &,
                    {block, madeThere[block[[All, 1]]]}
                ];
                monitor["pages: " <> ToString[$pages["Made"]] <> " made, " <> ToString[$pages["Unchanged"]] <> " unchanged"]
            ],
            Partition[pages, UpTo[8 Length[Kernels[]]]]
        ],
        ParallelEvaluate[Clear[$pageSetup], DistributedContexts -> None]
    ],
    Scan[pageMade[First[#], Last[#], pageHere[as, sidebar, First[#]]] &, pages]
];

(* the pages of some URLs made on the parallel kernels, or $Failed for each when the kernels gave
   no result for each *)
madeThere[urls_List] := With[{made = With[{there = $pageThere}, ParallelMap[there, urls, DistributedContexts -> None, Method -> "FinestGrained"]]},
    If[ListQ[made] && Length[made] === Length[urls], made, ConstantArray[$Failed, Length[urls]]]
];

(* A page made on a parallel kernel, from the resource, the sidebar and what the page's deploy
   reads of this kernel's state; none of this package's definitions, as the kernel has none. *)
$pageThere = Function[url,
    Needs["PacletResource`"];
    Block[{PacletResource`Private`$cloudObjectOpts = $pageSetup["Options"]},
        PacletResource`Private`createDocumentationEmbedPage[$pageSetup["As"], $pageSetup["Sidebar"], url]
    ]
];

(* what a page's deploy would deploy, the delayed page, made by the stock function without deploying *)
$pageDeployed = Function[{make, url},
    Block[{PacletResource`Private`deployDelayedEmbedTemplate = Function[{target, template, params, static, opts},
            PacletResource`Private`delayedEmbedTemplate[template, params, static, opts]]},
        make[url]
    ]
];

(* Sets the parallel kernels up to make a sidebar's pages, and says whether each makes the delayed
   page of one URL as this kernel does; when one does not, the pages are made here. *)
pageSetup[as_, sidebar_, url_String] := With[{
        setup = <|"As" -> as, "Sidebar" -> sidebar, "Options" -> Flatten[{PacletResource`Private`$cloudObjectOpts}]|>,
        deployed = $pageDeployed, there = $pageThere},
    ParallelEvaluate[$pageSetup = setup, DistributedContexts -> None];
    With[{
            here = Block[{$makingPage = True}, deployed[PacletResource`Private`createDocumentationEmbedPage[as, sidebar, #] &, url]],
            others = ParallelEvaluate[Needs["PacletResource`"]; deployed[there, url], DistributedContexts -> None]},
        MatchQ[here, _Delayed] && ListQ[others] && AllTrue[others, SameQ[#, here] &] || (
            monitor["cloud", "the parallel kernels make the pages otherwise than this kernel: the pages are made here"];
            ParallelEvaluate[Clear[$pageSetup], DistributedContexts -> None];
            False
        )
    ]
];

(* what the page template makes of the resource, besides the sidebar and the notebook: the template
   and the values it shows, and the options the pages are deployed with *)
pageContext[as_Association] := With[{template = PacletResource`Private`shingleTemplate["DocumentationPage"]},
    digest[{template, KeyTake[as, DeleteDuplicates[Cases[template, TemplateSlot[slot_String, ___] :> slot, Infinity]]], PacletResource`Private`$cloudObjectOpts}]
];

pageExistsQ[url_String] := KeyExistsQ[$pages["Existing"], pageRelative[url]];

(* a URL's path within the documentation *)
pageRelative[url_String] := If[ MatchQ[$pages["Root"], _CloudObject], cloudRelativePath[FileNameJoin[{$pages["Root"], "Documentation"}], CloudObject[url]], None];

(* The record of the pages once the deploy is done: a page no longer shown is dropped when every
   page was made or seen unchanged *)
savePages[complete_] := With[{record = If[TrueQ[complete], KeyTake[$pages["Signatures"], $pages["Seen"]], $pages["Signatures"]]},
    If[ MatchQ[$pages["Root"], _CloudObject] && record =!= $pages["Recorded"], writeDeployRecord[$pages["Root"], "Pages", record]]
];

(* ==================== the guides in the pages' sidebar ==================== *)

(* The sidebar of the resource's documentation pages lists the guides as a tree, as the site's left
   rail does: under the root guide, the guide named after the paclet, which the resource page
   shows, each guide its section headings link, then the guides it does not reach. A branch opens
   and closes, and stays as it was left, with the sidebar's scroll, while a reader moves between
   the pages. A paclet with one guide keeps the stock list. *)
guideChildren[guidesDir_String, name_String] := Association @ Map[
    FileBaseName[#] -> DeleteCases[StringDrop[#, 6] & /@ guideStructure[name, Import[#, "NB"]]["Children"], FileBaseName[#]] &,
    FileNames["*.nb", guidesDir]
];

guideTreeRules[children_Association, root_String] /; Length[children] > 1 := {
    HoldPattern[PacletResource`Private`createSidebarLinkSection["Guides", entries_List]] :> guideTreeSection[entries, children, root]
};
guideTreeRules[_, _] := {};

guideTreeSection[entries_List, children_Association, root_String] := Module[{url, byName, link, item, tree, reached},
    url[entry_] := If[AssociationQ[entry], Lookup[entry, "URL", ""], entry];
    byName = Association[(FileBaseName[url[#]] -> #) & /@ entries];
    link[name_] := If[ KeyExistsQ[byName, name],
        XMLElement["a", {"href" -> url[byName[name]]}, {Replace[byName[name], {entry_Association :> Lookup[entry, "Title", name], _ -> name}]}],
        name
    ];
    item[name_, path_] := If[ MemberQ[path, name], Nothing,
        With[{kids = Select[Lookup[children, name, {}], ! MemberQ[Append[path, name], #] &]},
            If[ kids === {},
                XMLElement["li", {}, {link[name]}],
                XMLElement["li", {}, {XMLElement["details", {}, {
                    XMLElement["summary", {}, {link[name]}],
                    XMLElement["ul", {}, item[#, Append[path, name]] & /@ kids]
                }]}]
            ]
        ]
    ];
    tree = item[#, {root}] & /@ Lookup[children, root, {}];
    reached = Append[Cases[tree, XMLElement["a", {"href" -> href_}, _] :> FileBaseName[href], Infinity], root];
    <|
        "Label" -> "Guides",
        "Links" -> {XMLElement["div", {}, {
            XMLElement["style", {}, {$guideTreeCSS}],
            XMLElement["ul", {"class" -> "guide-tree"}, Join[tree, item[#, reached] & /@ Sort[Complement[Keys[byName], reached]]]],
            XMLElement["script", {}, {$guideTreeScript}]
        }]}
    |>
];

$guideTreeCSS = StringJoin[
    ".guide-tree,.guide-tree ul{list-style:none;margin:0;padding:0}",
    ".guide-tree ul{padding-left:1em}",
    ".guide-tree details>summary{display:block;position:relative;cursor:pointer;list-style:none;padding-right:1.6em}",
    ".guide-tree details>summary::-webkit-details-marker{display:none}",
    ".guide-tree details>summary::after{content:\"\\25B8\";position:absolute;right:.8em;top:50%;transform:translateY(-50%);color:#999}",
    ".guide-tree details[open]>summary::after{content:\"\\25BE\"}",
    ".guide-tree summary>a{display:inline}"
];

(* the branches a reader opened, and the sidebar's scroll, kept for the browser tab's session *)
$guideTreeScript = StringJoin[
    "(function(){function R(){try{var SK='ppSidebarScroll',OK='ppSidebarOpen';",
    "var sb=document.getElementById('pac-nav-sidebar')||document.getElementById('pac-nav-sidebar-frame');if(!sb)return;",
    "var sc=sb;while(sc&&sc.scrollHeight<=sc.clientHeight&&sc.parentElement)sc=sc.parentElement;",
    "var o={};try{o=JSON.parse(sessionStorage.getItem(OK)||'{}')}catch(e){}",
    "var ds=document.querySelectorAll('.guide-tree details');",
    "for(var i=0;i<ds.length;i++){(function(d){var s=d.querySelector('summary');var k=s?s.textContent.trim():String(i);",
    "if(o[k])d.open=true;d.addEventListener('toggle',function(){o[k]=d.open;try{sessionStorage.setItem(OK,JSON.stringify(o))}catch(e){}})})(ds[i])}",
    "var v=sessionStorage.getItem(SK);if(v!==null)sc.scrollTop=parseFloat(v);",
    "sb.addEventListener('click',function(e){var t=e.target;if(t&&t.closest&&t.closest('a')){try{sessionStorage.setItem(SK,String(sc.scrollTop))}catch(x){}}})",
    "}catch(e){}}if(document.readyState!=='loading')R();else document.addEventListener('DOMContentLoaded',R)})();"
];

(* ==================== the page transform ==================== *)

(* The deploy makes each documentation notebook into its web page (ProcessNotebookForEmbedding),
   one after another. With parallel kernels it does so over them, when there are many pages; a page
   whose transform fails there, or takes over five minutes, is transformed again in this kernel.
   Each page's transform is kept in build/transform, keyed by everything it is made from: the
   notebook, its links into the paclet and whether a page is there for each, the transform's
   options and version, the symbols' form, and the link rule for a page with an OrangeLink. A page whose key is kept is taken from there, and
   the transforms a completed deploy did not use are deleted. *)
$transform = None;

newTransform[cache_String, context_, symbolRules_List, pinned_List, kernels_List] := <|
    "Cache" -> cache, "Context" -> context, "SymbolRules" -> symbolRules, "PinRules" -> pinned, "Kernels" -> kernels,
    "SymbolNames" -> transformSymbolNames[pinned],
    "Used" -> {}, "Cached" -> 0, "Transformed" -> 0, "Again" -> 0
|>;

transformRule[] := HoldPattern[PacletResource`Notebooks`ProcessNotebookForEmbedding[dir_?DirectoryQ, type_, opts : OptionsPattern[]] /;
    AssociationQ[$transform] && ! TrueQ[$transforming]] :> transformPages[dir, type, Flatten[{opts}]];

(* the stock transform of one notebook, in place *)
$pageTransform = Function[{file, type, options}, PacletResource`Notebooks`ProcessNotebookForEmbedding[file, type, Sequence @@ options]];

transformPages[dir_String, type_, opts_List] := Module[{files, options, valid, keys, cached, misses, results},
    files = FileNames["*.nb", dir, Infinity, IgnoreCase -> True];
    (* a transform into new files, rather than in place, is the stock one *)
    If[ ! TrueQ[Lookup[opts, OverwriteTarget, False]],
        Return[Block[{$transforming = True}, PacletResource`Notebooks`ProcessNotebookForEmbedding[dir, type, Sequence @@ opts]], Module]];
    If[ ! DirectoryQ[$transform["Cache"]], CreateDirectory[$transform["Cache"], CreateIntermediateDirectories -> True]];
    options = Append[DeleteCases[opts, "PackageURIs" -> _],
        "PackageURIs" -> Replace[Lookup[opts, "PackageURIs", Automatic], Automatic :> PacletResource`Notebooks`Private`$validRefs]];
    valid = AssociationThread[Replace[Lookup[options, "PackageURIs"], Except[_List] -> {}] -> True];
    keys = AssociationMap[transformKey[#, type, options, valid] &, files];
    cached = Select[files, FileExistsQ[transformFile[keys[#]]] &];
    Scan[CopyFile[transformFile[keys[#]], #, OverwriteTarget -> True] &, cached];
    misses = Select[files, ! MemberQ[cached, #] &];
    monitor["pages: " <> ToString[Length[cached]] <> " transformed before, " <> ToString[Length[misses]] <> " to transform"];
    results = transformFiles[misses, type, options];
    Scan[If[StringQ[results[#]], CopyFile[#, transformFile[keys[#]], OverwriteTarget -> True]] &, misses];
    $transform["Used"] = Join[$transform["Used"], Values[keys]];
    $transform["Cached"] += Length[cached];
    $transform["Transformed"] += Length[misses];
    DeleteMissing[Lookup[Join[AssociationThread[cached -> cached], results], files]]
];

transformFile[key_String] := FileNameJoin[{$transform["Cache"], key <> ".nb"}];

(* everything a page's transform is made from *)
transformKey[file_String, type_, options_List, valid_Association] := With[{held = heldNotebook[file]},
    digest[{
        If[ MatchQ[held, _HoldComplete], withoutNoise[held], FileHash[file, "SHA256", "HexString"]],
        pacletLinks[held, Lookup[options, "PacletName", None], valid],
        type,
        DeleteCases[options, ("PackageURIs" | "PacletInfo") -> _],
        transformPageContext[held],
        (* the link rule a page with a sub-guide heading is transformed by *)
        If[FreeQ[held, "OrangeLink"], None, orangeLinkRule[]]
    }]
];

(* what a page's transform is made with: the symbols' rules, which name the paclet's version, only for
   a page that holds one of its declared symbols' names, as the transform makes boxes of those alone;
   a page that holds none, and every page of a paclet that declares no symbols, keeps its transform
   when the paclet's version changes *)
transformPageContext[held_] := With[{context = $transform["Context"], names = Lookup[$transform, "SymbolNames", <||>]},
    If[ Length[names] === 0 || MatchQ[held, _HoldComplete] && ! showsSymbolQ[held, names], Take[context, UpTo[3]], context]
];
showsSymbolQ[held_, names_Association] := AnyTrue[Cases[held, s_String :> s, Infinity], KeyExistsQ[names, #] &];

(* the names, full and short, of the symbols the pin rules declare *)
transformSymbolNames[pinned_List] := With[{full = Union[Flatten[Cases[pinned, (Rule | RuleDelayed)[_, s : {___String}] :> s]]]},
    AssociationThread[Union[full, Map[Last[StringSplit[#, "`"], #] &, full]] -> True]
];

(* a page's links into the paclet, each with whether a page is there for it, which is all the
   transform reads of the paclet's pages *)
pacletLinks[held_HoldComplete, name_String, valid_Association] := Map[
    {#, KeyExistsQ[valid, StringDelete[#, "#" ~~ ___ ~~ EndOfString]]} &,
    Union[Cases[held, link_String /; StringStartsQ[link, ("paclet:" ~~ name) | name], Infinity]]
];
pacletLinks[_, _, _] := {};

(* The transforms: over the parallel kernels, in blocks, each kernel with the symbols' rules from
   the first page to the last, so that the boxes the stock transform keeps for each symbol, the
   same for every page, are made once a kernel rather than once a page; then the stock rules
   again. The pages that failed there, here. *)
transformFiles[{}, _, _] := <||>;
transformFiles[files_List, type_, options_List] := Module[{results = <||>, again},
    If[ Length[files] >= 24 && $transform["Kernels"] =!= {},
        With[{setup = <|"Type" -> type, "Options" -> options, "SymbolRules" -> $transform["SymbolRules"], "PinRules" -> $transform["PinRules"], "LinkRules" -> {orangeLinkRule[]}|>},
            ParallelEvaluate[
                $transformSetup = setup;
                Needs["PacletResource`"];
                $transformStock = {
                    DownValues[PacletResource`Private`fancyPacletSymbolBoxes],
                    DownValues[PacletResource`Private`pacletDeclaredSymbols],
                    PacletResource`Notebooks`Private`$embeddedLinkRules
                };
                Unprotect[PacletResource`Private`fancyPacletSymbolBoxes, PacletResource`Private`pacletDeclaredSymbols];
                DownValues[PacletResource`Private`fancyPacletSymbolBoxes] = Join[setup["SymbolRules"], DownValues[PacletResource`Private`fancyPacletSymbolBoxes]];
                DownValues[PacletResource`Private`pacletDeclaredSymbols] = Join[setup["PinRules"], DownValues[PacletResource`Private`pacletDeclaredSymbols]];
                With[{rules = PacletResource`Notebooks`Private`$embeddedLinkRules},
                    If[Head[rules] === Dispatch, PacletResource`Notebooks`Private`$embeddedLinkRules = Dispatch[Join[Normal[rules], setup["LinkRules"]]]]];
                Null,
                DistributedContexts -> None
            ]
        ];
        WithCleanup[
            Scan[
                Function[block,
                    results = Join[results, AssociationThread[block -> ParallelMap[
                        Function[file,
                            Replace[
                                TimeConstrained[PacletResource`Notebooks`ProcessNotebookForEmbedding[file, $transformSetup["Type"], Sequence @@ $transformSetup["Options"]], 300, $TimedOut],
                                Except[_String] -> $Failed
                            ]
                        ],
                        block,
                        DistributedContexts -> None
                    ]]];
                    monitor["pages: " <> ToString[Length[results]] <> " of " <> ToString[Length[files]] <> " transformed"]
                ],
                Partition[files, UpTo[8 Length[$transform["Kernels"]]]]
            ],
            ParallelEvaluate[
                If[ ListQ[$transformStock],
                    DownValues[PacletResource`Private`fancyPacletSymbolBoxes] = $transformStock[[1]];
                    DownValues[PacletResource`Private`pacletDeclaredSymbols] = $transformStock[[2]];
                    PacletResource`Notebooks`Private`$embeddedLinkRules = $transformStock[[3]]
                ];
                Clear[$transformSetup, $transformStock],
                DistributedContexts -> None
            ]
        ]
    ];
    again = Select[files, ! StringQ[Lookup[results, #]] &];
    If[ results =!= <||> && again =!= {}, monitor["pages: " <> ToString[Length[again]] <> " transformed again here"]];
    $transform["Again"] += If[results =!= <||>, Length[again], 0];
    Join[results, AssociationMap[$pageTransform[#, type, options] &, again]]
];

(* the transforms a completed deploy did not use *)
pruneTransforms[] := If[ AssociationQ[$transform] && DirectoryQ[$transform["Cache"]],
    With[{used = AssociationThread[$transform["Used"] -> True]},
        Scan[If[! KeyExistsQ[used, FileBaseName[#]], DeleteFile[#]] &, FileNames["*.nb", $transform["Cache"]]]
    ]
];

(* The stale pages among a documentation tree's files: a notebook the new documentation does not
   have, and the page <name>.html the resource builds around such a notebook. The resource builds
   the pages of the notebooks it keeps after the documentation is written, and any other file is
   none of the documentation's. *)
staleDocumentation[relatives_List, written_List] := Select[relatives,
    Switch[FileExtension[#],
        "nb", ! MemberQ[written, #],
        "html", ! MemberQ[written, StringReplace[#, ".html" ~~ EndOfString -> ".nb"]],
        _, False
    ] &
];

(* The copy-to-clipboard form of a documented symbol on the resource page. The stock boxes paste
   PacletSymbol[<the resource's URL>, symbol], which a kernel resolves by looking the URL up as a
   paclet name and never finding it, so every fresh session downloads the resource again. These
   paste a form that names the version the code was copied from: where that version alone is
   installed the paclet is found by name, and otherwise every installed version is uninstalled and
   that one installed, from its published archive of that version or else from its resource, which
   holds the newest. A copy loaded from a directory is not installed, and stays.
   The stock boxes are kept per id, so this is a rule placed before
   them, for the ids of this resource: its short URL or its deployed location. The rules here hold
   all they test, so a parallel kernel that transforms pages takes them as they are. *)

installForm[name_String, version_String, archive_String, _] := With[{n = name, v = version, a = archive},
    HoldComplete[If[Through[PacletFind[n]["Version"]] === {v}, n, PacletUninstall[n]; PacletInstall[a]]]];
installForm[name_String, version_String, None, resource_String] := With[{n = name, v = version, r = resource},
    HoldComplete[If[Through[PacletFind[n]["Version"]] === {v}, n, PacletUninstall[n]; PacletInstall[ResourceObject[r]]]]];
installForm[_, _, _, _] := None;

symbolBoxRules[name_String, form_HoldComplete] := {
    HoldPattern[PacletResource`Private`fancyPacletSymbolBoxes[
        HoldComplete[url_String /; StringStartsQ[url, "https://wolfr.am/"] || StringContainsQ[url, "DeployedResources/Paclet/" <> name]], paclet_, symbol_]] :>
        PacletResource`Private`fancyPacletSymbolBoxes[form, paclet, symbol]
};
symbolBoxRules[_, _] := {};

(* The symbols the resource page's examples link: the deploy looks the paclet up by name again, which
   can find another copy of it, one whose PacletInfo declares none or others *)
pinRules[name_String, symbols : {__String}] := {
    HoldPattern[PacletResource`Private`pacletDeclaredSymbols[name]] :> symbols,
    HoldPattern[PacletResource`Private`pacletDeclaredSymbols[KeyValuePattern["Name" -> name]]] :> symbols
};
pinRules[_, _] := {};

(* rules placed before a stock function's own, when it is loaded *)
SetAttributes[installRules, HoldFirst];
installRules[symbol_Symbol, rules_List] := If[ rules =!= {} && DownValues[symbol] =!= {},
    Unprotect[symbol];
    DownValues[symbol] = Join[rules, DownValues[symbol]];
    True,
    False
];

installSymbolBoxes[name_String, form_] := installRules[PacletResource`Private`fancyPacletSymbolBoxes, symbolBoxRules[name, form]];
pinDeclaredSymbols[name_String, symbols_] := installRules[PacletResource`Private`pacletDeclaredSymbols, pinRules[name, symbols]];

(* The scrape lints the paclet's code with CodeInspector, which says nothing about its documentation
   and, on some images, trips an assertion in the code parser that fails the scrape *)
(* The scrape packs the built paclet again, with CreatePacletArchive, over the archive the build
   made, checked and recorded in build/archive.wl, whose hash then no longer matches it: within a
   block over CreatePacletArchive, the scrape is handed that archive instead, so that the archive
   checked is the archive deployed, and a deploy with "Build" -> False after it, a dry run's or a
   real one's, finds the archive as recorded. *)
installArchiveRule[builtRoot_String, archive_String] := (
    Unprotect[CreatePacletArchive];
    CreatePacletArchive[root_String] /; sameDirectoryQ[root, builtRoot] := archive;
);

sameDirectoryQ[a_String, b_String] := realPath[FileNameJoin[FileNameSplit[a]]] === realPath[FileNameJoin[FileNameSplit[b]]];

codeLintOff[] := If[ DownValues[DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ] =!= {},
    Unprotect[DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ];
    DownValues[DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ] = {
        HoldPattern[DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ[_]] :> False
    };
    True,
    False
];

(* The errors the definition notebook's own check finds, which name what stopped a scrape; not the
   missing PacletInfo cell, which a definition notebook built from markdown never has *)
$builtNotebookErrors = {"NoPacletInfoFileCell"};

scrapeErrors[notebook_NotebookObject] := DeleteCases[
    Lookup[
        Select[Replace[Normal[DefinitionNotebookClient`CheckDefinitionNotebook[notebook]], Except[_List] -> {}], AssociationQ[#] && Lookup[#, "Level", ""] === "Error" &],
        "Tag", Nothing
    ],
    Alternatives @@ $builtNotebookErrors
];

(* the archive of one version, as publishArchive names it *)
archiveURL[dirName_String, name_String, version_String] := First[CloudObject[StringTrim[dirName, "/"] <> "/" <> StringReplace[name, "/" -> "__"] <> "-" <> version <> ".paclet"]];
archiveURL[_, _, _] := None;

(* the symbols a built paclet's PacletInfo declares *)
archiveSymbols[builtRoot_String] := Module[{file = FileNameJoin[{builtRoot, "PacletInfo.wl"}], info},
    info = If[FileExistsQ[file], FirstCase[Import[file, "HeldExpressions"], HoldComplete[PacletObject[assoc_Association]] :> assoc, None], None];
    If[ ! AssociationQ[info], Return[{}, Module]];
    Replace[
        FirstCase[Lookup[info, "Extensions", {}], {"Kernel", rules___} :> Lookup[Association[Cases[{rules}, _Rule]], "Symbols", {}], {}],
        Except[{___String}] -> {}
    ]
];

deployFunctions[] := {
    PacletResource`Private`cloudExportPacletResourceContent,
    PacletResource`Private`cloudDeployPacletWebFile,
    PacletResource`Private`cloudDeployNB
};

patchDeployFunctions[rule_, skip_ : Function[False]] := Scan[
    Function[f, Unprotect[f]; DownValues[f] = Map[If[TrueQ[skip[#]], #, # /. rule] &, DownValues[f]]],
    deployFunctions[]
];

(* A guide's sub-guide heading, which DocumentationBuild builds as an OrangeLink to a paclet: page,
   links to the deployed page, as the stock deploy's other paclet: links do: the cloud renders an
   OrangeLink as no link, and its viewer opens the paclet: link's cloud reference URL, where a
   deployed paclet has no page. The link keeps the OrangeLink's look in the cloud: the heading's
   style, not the code font of the link's inline cell, and the template's orange, lighter under the
   mouse, given in the hyperlink's own style slots, as the cloud draws a dynamic colour in the label
   as blue. The rule uses none of this package's definitions, as the parallel kernels apply it
   without the package. *)
orangeLinkRule[] := With[{
        orange = LightDarkSwitched[RGBColor[0.85098, 0.396078, 0.]],
        lighter = LightDarkSwitched[RGBColor[0.9686274509803922, 0.4666666666666667, 0.]]},
    TemplateBox[{label_, ref_String?PacletResource`Notebooks`Private`pacletRefQ}, "OrangeLink", ___, BaseStyle -> style_, ___] :>
        RuleCondition[PacletResource`Notebooks`Private`makeEmbeddedPackageLink[label, ref, style] /.
            TemplateBox[{text_, target_, note_, action_, _, BaseStyle -> _, rest___}, "HyperlinkTemplate"] :>
                TemplateBox[{text, target, note, action, {FontColor -> lighter}, BaseStyle -> Flatten[{"Hyperlink", style, FontColor -> orange}], rest}, "HyperlinkTemplate"]]
]

(* one definition, guarded inside: a literal installDeployFixes[] := Null beside a conditional
   rule would always be tried first, and the fixes would never run *)
installDeployFixes[] := If[ ! TrueQ[$deployFixesInstalled], Module[{probe = CloudObject["PacletPageAppearanceProbe.html"]},
    Needs["PacletResource`"];
    With[{rules = PacletResource`Notebooks`Private`$embeddedLinkRules},
        If[Head[rules] === Dispatch, PacletResource`Notebooks`Private`$embeddedLinkRules = Dispatch[Append[Normal[rules], orangeLinkRule[]]]]];
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
    patchDeployFunctions[$cloudCopyRule];
    Unprotect[PacletResource`Private`cloudDeployPacletWebFile];
    DownValues[PacletResource`Private`cloudDeployPacletWebFile] = DeleteCases[
        DownValues[PacletResource`Private`cloudDeployPacletWebFile],
        rule_ /; MemberQ[rule[[1]], "DocumentationNotebooks", Infinity]
    ];
    PacletResource`Private`cloudDeployPacletWebFile[_, root_CloudObject, "DocumentationNotebooks", dir_?DirectoryQ, opts : OptionsPattern[CloudObject]] :=
        replaceDocumentation[dir, root, OptionValue[CloudObject, {opts}, Permissions]];
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

(* The resource page's Examples section, which the stock template draws whatever the resource
   holds: drawn by a copy of the template only where there is an example notebook, and an example
   notebook that holds no example left out of the deploy. "Examples" -> True keeps the stock page,
   False leaves the example notebook out whatever it holds. *)
$examples = Automatic;
$examplesPatchInstalled = False;

(* one definition of the page's template, with the stock one inside: a rule beside the stock
   literal rule of the same argument would never be tried *)
installExamplesPatch[] := If[ ! TrueQ[$examplesPatchInstalled],
    Unprotect[PacletResource`Private`deployExtraPacletWebFiles];
    PrependTo[
        DownValues[PacletResource`Private`deployExtraPacletWebFiles],
        HoldPattern[PacletResource`Private`deployExtraPacletWebFiles[
            id_, as_?(AssociationQ[#] && KeyExistsQ[#, "PacletFile"] && KeyExistsQ[#, "ExampleNotebook"] && ! examplesShownQ[#["ExampleNotebook"]] &), root_, opts___]] :>
            PacletResource`Private`deployExtraPacletWebFiles[id, KeyDrop[as, "ExampleNotebook"], root, opts]
    ];
    Unprotect[DeployedResourceShingle`Private`templateFile];
    DeployedResourceShingle`Private`templateFile["Paclet"] := With[{file = examplesTemplate[]},
        If[$examples =!= True && StringQ[file], file, PacletResource`Private`shingleTemplateFile["PacletShingle"]]];
    $examplesPatchInstalled = True
];

(* whether the page shows an example notebook: as "Examples" says, and for Automatic, one that
   holds an example, an input cell *)
examplesShownQ[nb_] := Switch[$examples, True, True, False, False, _, exampleNotebookQ[nb]];
exampleNotebookQ[nbo_NotebookObject] := exampleNotebookQ[NotebookGet[nbo]];
exampleNotebookQ[nb_] := ! FreeQ[nb, Cell[_, "Input" | "Code", ___]];

(* the stock template with its Examples section, from the heading to the frame of the notebook it
   embeds, drawn only where there is an example notebook: written once, to a file of its own; None
   where the stock template has no section of that shape, which then stays in use *)
$examplesSection = Shortest["<div" ~~ Whitespace ~~ "class=\"nb-collapse label example-heading\"" ~~ ___ ~~
    "<!-- RS_SHINGLE_EXAMPLE_SECTION_END -->" ~~ Whitespace ~~ "</div>"];
examplesTemplate[] := examplesTemplate[] = Module[{stock = PacletResource`Private`shingleTemplateFile["PacletShingle"], text, file},
    text = If[StringQ[stock] && FileExistsQ[stock], ByteArrayToString[ReadByteArray[stock], "UTF-8"], None];
    If[ ! StringQ[text] || Length[StringCases[text, $examplesSection]] =!= 1, Return[None, Module]];
    file = FileNameJoin[{$TemporaryDirectory, "PacletPage-PacletShingle-" <> digest[text] <> ".xml"}];
    Export[file, StringReplace[text, section : $examplesSection :> StringJoin[
        "<wolfram:if test=\"StringQ@#ExampleNotebookURL || StringQ@#CloudOpenExampleNotebook || StringQ@#ExampleNotebookDownload\">\n",
        section, "\n</wolfram:if>"]], "Text", CharacterEncoding -> "UTF-8"];
    file
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
        With[{copied = cloudCopy[archive, CloudObject[#, Permissions -> permissions]]},
            If[MatchQ[copied, _CloudObject] && meta =!= <||>, SetOptions[copied, MetaInformation -> meta]];
            copied
        ] &,
        {versioned, latest}
    ] // Replace[{{v_CloudObject, l_CloudObject} :> <|"Latest" -> First[l], "Version" -> First[v]|>, other_ :> other}]
];

(* The paclet as it is authored, for editing elsewhere: its directory but build/, hidden files and
   what "Exclude" leaves out, documentation notebooks unbuilt, packed in a temporary directory *)
sourceArchive[dir_String, exclude_] := Module[{stageRoot = CreateDirectory[], stage, work, packed},
    stage = CreateDirectory[FileNameJoin[{stageRoot, FileNameTake[dir]}]];
    copyTree[dir, stage, "", Function[path,
        path === "build" || StringStartsQ[FileNameTake[path], "."] || AnyTrue[Flatten[{exclude}], StringMatchQ[path, #] &]]];
    work = CreateDirectory[];
    packed = CreatePacletArchive[stage, work];
    DeleteDirectory[stageRoot, DeleteContents -> True];
    If[StringQ[packed] && FileExistsQ[packed], packed, $Failed]
];

(* where the source archive goes: <dir>/<Publisher>__<Name>-source.paclet, the latest, and
   <dir>/<Publisher>__<Name>-source-<version>.paclet *)
sourceArchiveNames[dir_String, name_String, version_String] := With[{base = StringTrim[dir, "/"] <> "/" <> StringReplace[name, "/" -> "__"] <> "-source"},
    <|"Latest" -> base <> ".paclet", "Version" -> base <> "-" <> version <> ".paclet"|>
];

publishSourceArchive[dir_String, dirName_String, name_String, version_String, permissions_, commit_, exclude_] := Module[
    {packed = sourceArchive[dir, exclude], names = sourceArchiveNames[dirName, name, version], meta = If[StringQ[commit], <|"Commit" -> commit|>, <||>], copied},
    If[ ! StringQ[packed], Return[packed, Module]];
    copied = Map[
        With[{object = cloudCopy[packed, CloudObject[#, Permissions -> permissions]]},
            If[MatchQ[object, _CloudObject] && meta =!= <||>, SetOptions[object, MetaInformation -> meta]];
            object
        ] &,
        names
    ];
    DeleteDirectory[DirectoryName[packed], DeleteContents -> True];
    If[AllTrue[copied, MatchQ[_CloudObject]], First /@ copied, copied]
];

(* A dry run writes nothing to the cloud, so without a cloud session it runs offline: no account
   check, no version gate (which reads the published archive) and no deploy location. With a
   session it checks everything a deploy would. *)
offlineDryRunQ[dryRun_] := TrueQ[dryRun] && ! TrueQ[$CloudConnected];

(* whether a check that failed stops the run: a dry run publishes nothing, so there a check only
   reports, with its message, what a real run would refuse *)
stopsQ[ok_, dryRun_] := ! TrueQ[ok] && ! TrueQ[dryRun];

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

(* what licenses a kernel started here as this one is licensed: under on-demand licensing a kernel
   takes its license manager and its entitlement from its command line, -pwfile !cloudlm.wolfram.com
   -entitlement id, as wolframscript gives them from WOLFRAMSCRIPT_ENTITLEMENTID, and one started
   without them finds no license. This kernel's own, or else those wolframscript would give. *)
$onDemandLicenseManager = "!cloudlm.wolfram.com";
kernelLicenseArguments[] := kernelLicenseArguments[$CommandLine, Environment["WOLFRAMSCRIPT_ENTITLEMENTID"]];
kernelLicenseArguments[line_List, environment_] := With[
    {entitlement = FirstCase[Partition[line, 2, 1], {"-entitlement", id_String} :> id,
        If[StringQ[environment] && StringLength[StringTrim[environment]] > 0, StringTrim[environment], None]]},
    If[ entitlement === None, {},
        {"-pwfile", FirstCase[Partition[line, 2, 1], {"-pwfile", file_String} :> file, $onDemandLicenseManager], "-entitlement", entitlement}]
];
kernelLicenseArguments[_, _] := {};

(* What every kernel PacletPage starts for a script runs first: the paclet manager's weekly update of
   its sites and the resource system's refresh of its information are asynchronous transfers that a
   fresh kernel starts on its own, on a machine whose sites are not fresh, such as a new pod, and a
   15.0 kernel can crash in one (see $sitesRefreshed); the script's kernel has no use for either. *)
$quietKernelCode = "PacletManager`Services`Private`doWeeklyUpdate[] := Null;\nResourceSystemClient`$AsyncronousResourceInformationUpdates = False;\n";

(* the command of a script run in a fresh kernel, with its arguments *)
kernelScriptCommand[kernel_String, script_String, arguments_List : {}] := Join[{kernel, "-noprompt"}, kernelLicenseArguments[], {"-script", script}, arguments];

(* The archive installed alone, in a fresh kernel: extracted to a temporary directory, its primary
   context loaded without a message, from there, and at the archive's version. The kernel that
   publishes has the source paclet loaded, so it cannot tell. *)
archiveLoadScript[archive_String, context_String, dependencies_List : {}] := StringJoin[
    archiveLoadCode[archive, context, dependencies],
    "WriteString[\"stdout\", \"PACLETPAGE-LOAD \" <> ExportString[report, \"RawJSON\", \"Compact\" -> True] <> \"\\n\"];\n",
    "DeleteDirectory[dir, DeleteContents -> True];\n"
];

(* the code of the check, up to its report *)
archiveLoadCode[archive_String, context_String, dependencies_List : {}] := StringJoin[
    $quietKernelCode,
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
        "\"SymbolNames\" -> Sort[Map[", wlString[context], " <> Last[StringSplit[#, \"`\"]] &, Names[", wlString[context <> "*"], "]]]|>;\n"
];

archiveLoadCheck[archive_String, version_String, context_String, dependencies_List : {}] := Module[{kernel = kernelExecutable[], script, run, line, report},
    If[ kernel === None, Return[<|"OK" -> False, "Reason" -> "no kernel executable under " <> $InstallationDirectory|>, Module]];
    script = FileNameJoin[{$TemporaryDirectory, CreateUUID["pacletpage-load-"] <> ".wl"}];
    Export[script, archiveLoadScript[archive, context, dependencies], "Text"];
    run = runWatched[kernelScriptCommand[kernel, script], "archive: the archive loads alone in a fresh kernel"];
    DeleteFile[script];
    line = If[AssociationQ[run], SelectFirst[StringSplit[run["StandardOutput"], "\n"], StringStartsQ[#, "PACLETPAGE-LOAD "] &, None], None];
    report = If[StringQ[line], ImportString[StringDrop[line, 16], "RawJSON"], <|"Reason" -> "the fresh kernel gave no report: " <> If[AssociationQ[run], StringTake[run["StandardError"] <> run["StandardOutput"], UpTo[500]], ToString[run]]|>];
    If[ ! AssociationQ[report], report = <|"Reason" -> "the fresh kernel's report could not be read"|>];
    judgedLoadReport[report, version, context]
];

(* a load report judged: the paclet loaded from the archive, at its version, with no message. A
   kernel may report two of its own packages defining one symbol (SystemSymbolQ::shdw in 15.0); a
   shadowing message that names none of the paclet's contexts is not the paclet's. *)
judgedLoadReport[report0_Association, version_String, context_String] := With[{report = If[ ListQ[report0["Messages"]],
        Join[report0, <|
            "Messages" -> Select[report0["Messages"], ! foreignShadowQ[#, context] &],
            "IgnoredMessages" -> Select[report0["Messages"], foreignShadowQ[#, context] &]
        |>],
        report0
    ]},
    Append[report, "OK" -> And[TrueQ[report["Loaded"]], report["Messages"] === {}, TrueQ[report["FromArchive"]], report["Version"] === version]]
];

(* The archive loaded alone on a fresh kernel of an evaluation spec, for a paclet that loads there
   and not here: the stock check's code, evaluated on that kernel, in at most $loadCheckTime seconds *)
$loadCheckTime = 900;
archiveLoadCheckOnKernel[spec_Association, archive_String, version_String, context_String, dependencies_List : {}] := Module[{script, kernel = None, report},
    script = FileNameJoin[{$TemporaryDirectory, CreateUUID["pacletpage-load-"] <> ".wl"}];
    Export[script, archiveLoadCode[archive, context, dependencies] <> "DeleteDirectory[dir, DeleteContents -> True];\nreport\n", "Text"];
    monitor["archive: the archive loads alone on the evaluation kernel"];
    report = Catch[
        TimeConstrained[
            kernel = launchedKernel[spec];
            linkEvaluated[kernel["Link"], With[{file = script}, Hold[Get[file]]]],
            $loadCheckTime,
            <|"Reason" -> "the evaluation kernel gave no report in " <> ToString[$loadCheckTime] <> " seconds"|>
        ],
        $evaluationKernelTag
    ];
    If[ AssociationQ[kernel], closeEvaluationKernel[kernel]];
    DeleteFile[script];
    judgedLoadReport[
        Which[
            AssociationQ[report], report,
            FailureQ[report], <|"Reason" -> ToString[report["Message"]]|>,
            True, <|"Reason" -> "the evaluation kernel's report could not be read"|>
        ],
        version, context
    ]
];

(* the check of an archive's loading alone: none; the stock one; or, for a paclet whose examples
   run on another kernel, one on the kernel of the "Paclet" template's pages, else of the first
   template given one *)
loadCheckOf[check_, evaluation_, dir_String] := Which[
    ! TrueQ[check], False,
    AssociationQ[evaluationSpec[evaluation, "Paclet", dir, None]], evaluationSpec[evaluation, "Paclet", dir, None],
    AssociationQ[evaluation] && ! KeyExistsQ[evaluation, "Command"],
        Replace[SelectFirst[Map[evaluationSpec[#, "Paclet", dir, None] &, Values[evaluation]], AssociationQ, None], None -> True],
    True, True
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
sourceHash[dir_String] := digest[
    Map[
        {relativePath[dir, #], FileHash[#, "SHA256"]} &,
        Sort @ Select[FileNames["*", dir, Infinity],
            FileType[#] === File && ! StringMatchQ[relativePath[dir, #], ("build" | "Documentation" | ".git") ~~ $PathnameSeparator ~~ ___] &&
                ! StringEndsQ[#, ".paclet"] && FileNameTake[#] =!= "ResourceDefinition.nb" &]
    ]
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
   tidied. A page tidied before has neither the delimiter nor the front end's empty entries left,
   and is only read. *)
tidyBuiltPages[builtRoot_String] := Count[tidyBuiltPage /@ FileNames["*.nb", FileNameJoin[{builtRoot, "Documentation"}], Infinity], True];

tidyBuiltPage[file_String] := StringContainsQ[ReadString[file], "\"PageDelimiter\"" | "\"WorkflowDockedCell\"" | "\"SearchTextTranslated\""] && With[{notebook = Get[file]},
    With[{tidy = DeleteCases[singleRuleBeforeExamples[notebook], $frontEndNoise, Infinity]}, tidy =!= notebook && StringQ[Export[file, tidy, "NB"]]]
];

(* PacletBuild in a fresh kernel: the build's own documentation step loads packages and issues
   messages that belong to it, not to the caller, and a fresh kernel builds what is on disk, not
   what the caller has loaded. Its documentation step is the stock one, followed by the tidying of
   the pages it built, before PacletBuild hashes them into the manifest and packs them. *)
pacletBuildScript[source_String, buildDir_String, dependencies_List : {}] := StringJoin[
    $quietKernelCode,
    "$ProgressReporting = False;\n",
    dependencyLoadCode[dependencies],
    "PacletDirectoryLoad[", wlString[ParentDirectory[$packageDirectory]], "];\n",
    "Needs[\"WolframInstitute`PacletPage`\"];\n",
    "Needs[\"PacletTools`\"];\n",
    $pageKernelCode,
    "handlers = PacletTools`$PacletExtensionHandlers;\n",
    "With[{stock = handlers[\"Documentation\", \"Build\"]},\n",
    "    handlers[\"Documentation\", \"Build\"] = Function[{extension, root, built, files},\n",
    "        With[{done = stock[extension, root, built, files]}, WolframInstitute`PacletPage`Private`tidyBuiltPages[built]; done]]];\n",
    "result = PacletTools`PacletBuild[", wlString[source], ", ", wlString[buildDir], ", PacletTools`PacletExtensionHandlers -> handlers];\n",
    "report = If[MatchQ[result, _Success], <|\"Archive\" -> result[\"PacletArchive\"]|>, <|\"Failure\" -> ToString[result, InputForm]|>];\n",
    "WriteString[\"stdout\", \"PACLETPAGE-BUILD \" <> ExportString[report, \"RawJSON\", \"Compact\" -> True] <> \"\\n\"];\n",
    $endScriptCode
];

freshKernelBuild[source_String, buildDir_String, dependencies_List : {}] := Module[{kernel = kernelExecutable[], script, run, line},
    If[ kernel === None, Return[<|"Failure" -> "no kernel executable under " <> $InstallationDirectory|>, Module]];
    script = FileNameJoin[{$TemporaryDirectory, CreateUUID["pacletpage-build-"] <> ".wl"}];
    Export[script, pacletBuildScript[source, buildDir, dependencies], "Text"];
    run = runWatched[kernelScriptCommand[kernel, script], "archive: PacletBuild builds what is left, the search index and the archive"];
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
   changed pages, in one call, since each call reads the whole documentation tree first, without
   the search indexes, writing the record entries PacletBuild keeps for them to a file; then its
   pages tidied, as PacletBuild's documentation step tidies the pages it finds untidied, which a
   process does over its own share while the others build theirs. Its output goes to a log, so
   that nothing waits on a full pipe. *)
pageShareScript[source_String, docsDir_String, builtDocsDir_String, pages_List, recordOut_String, log_String, dependencies_List] := StringJoin[
    $quietKernelCode,
    "$ProgressReporting = False;\n",
    "$Output = $Messages = {OpenWrite[", wlString[log], "]};\n",
    dependencyLoadCode[dependencies],
    "PacletDirectoryLoad[", wlString[source], "];\n",
    "Needs[\"PacletTools`\"];\n",
    "Needs[\"DocumentationBuild`\"];\n",
    $pageKernelCode,
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
    "    Replace[processed, Except[_Association] -> <||>]]], ", wlString[recordOut], "];\n",
    "PacletDirectoryLoad[", wlString[ParentDirectory[$packageDirectory]], "];\n",
    "Needs[\"WolframInstitute`PacletPage`\"];\n",
    "Scan[WolframInstitute`PacletPage`Private`tidyBuiltPage, Select[FileNameJoin[{", wlString[builtDocsDir], ", #}] & /@ pages, FileExistsQ]];\n",
    $endScriptCode
];

(* Scripts run as separate kernel processes, at most n at a time, each one killed past its
   deadline, or when none of its "Record" and "Progress" files has changed for "Stall" seconds:
   "Finished", "TimedOut" or "Stalled" by script. A process killed takes the processes it started,
   its parallel kernels and front ends, with it. A process may start parallel kernels of its own,
   which a parallel kernel may not, and the stock documentation build does when it installs a
   format's paclet. When the deploy is monitored, a line says when each run with a "Label" ends,
   and every minute how many have. *)
runKernelScripts[runs_List, n_Integer, what_String : "processes"] := Module[
    {kernel = kernelExecutable[], queue = runs, running = {}, finished = <||>, next = AbsoluteTime[] + 60, stallCheck = AbsoluteTime[] + 10},
    If[ kernel === None, Return[AssociationMap["NoKernel" &, Lookup[runs, "Script", {}]], Module]];
    While[ queue =!= {} || running =!= {},
        While[ Length[running] < n && queue =!= {},
            AppendTo[running, Append[First[queue], {
                "Process" -> StartProcess[kernelScriptCommand[kernel, First[queue]["Script"], Lookup[First[queue], "Arguments", {}]]],
                "Start" -> AbsoluteTime[]
            }]];
            queue = Rest[queue]
        ];
        (* what a process or its front end writes is read and dropped, so that no pipe fills *)
        Scan[If[MatchQ[#["Process"], _ProcessObject], drainProcess[#["Process"]]] &, running];
        running = Select[running, Function[run, Which[
            ! MatchQ[run["Process"], _ProcessObject] || ProcessStatus[run["Process"]] =!= "Running",
                finished[run["Script"]] = "Finished"; runEnded[run, "finished"]; False,
            AbsoluteTime[] - run["Start"] > run["Deadline"],
                stoppedReport[run]; killProcessTree[run["Process"]]; finished[run["Script"]] = "TimedOut"; runEnded[run, "stopped past its time"]; False,
            AbsoluteTime[] > stallCheck && stalledQ[run],
                stoppedReport[run]; killProcessTree[run["Process"]]; finished[run["Script"]] = "Stalled"; runEnded[run, "stopped: nothing new for " <> ToString[run["Stall"]] <> " s"]; False,
            True, True
        ]]];
        If[ AbsoluteTime[] > stallCheck, stallCheck = AbsoluteTime[] + 10];
        If[ AbsoluteTime[] > next,
            monitor[what <> ": " <> ToString[Length[finished]] <> " of " <> ToString[Length[runs]] <> " done, " <> ToString[Length[running]] <> " running"];
            next += 60
        ];
        If[ running =!= {}, Pause[0.5]]
    ];
    finished
];

(* A process run to its end, as RunProcess runs it, with a line when it starts, every minute while
   it runs, and when it ends, when the deploy is monitored *)
runWatched[command_List, what_String] := Module[{process, out = {}, err = {}, start = AbsoluteTime[], next},
    monitor[what];
    process = StartProcess[command];
    If[ ! MatchQ[process, _ProcessObject], Return[$Failed, Module]];
    next = start + 60;
    While[ ProcessStatus[process] === "Running",
        AppendTo[out, ReadString[ProcessConnection[process, "StandardOutput"], EndOfBuffer]];
        AppendTo[err, ReadString[ProcessConnection[process, "StandardError"], EndOfBuffer]];
        If[ AbsoluteTime[] > next, monitor[what <> ": " <> ToString[Round[(AbsoluteTime[] - start) / 60]] <> " min"]; next += 60];
        Pause[0.5]
    ];
    AppendTo[out, ReadString[ProcessConnection[process, "StandardOutput"], EndOfFile]];
    AppendTo[err, ReadString[ProcessConnection[process, "StandardError"], EndOfFile]];
    monitor[what <> ": done in " <> ToString[Round[AbsoluteTime[] - start]] <> " s"];
    <|"ExitCode" -> ProcessInformation[process, "ExitCode"],
      "StandardOutput" -> StringJoin[Select[out, StringQ]], "StandardError" -> StringJoin[Select[err, StringQ]]|>
];

(* what a run about to be stopped is doing, when the deploy is monitored: its process and those it
   started, each with its share of a processor and its state, so that a log tells a process that
   spins from one that waits *)
stoppedReport[run_Association] := If[ StringQ[Lookup[run, "Label", None]] && MatchQ[run["Process"], _ProcessObject],
    Replace[processReport[run["Process"]["PID"]], report_String :> monitor[run["Label"] <> " when stopped: " <> report]]
];

(* a process and those it started: each one's name, share of a processor and state; None where
   there is no ps *)
processReport[pid_Integer] /; $OperatingSystem =!= "Windows" := With[{ps = RunProcess[{"ps", "-e", "-o", "pid=,ppid=,pcpu=,stat=,comm="}]},
    If[ ! AssociationQ[ps] || ps["ExitCode"] =!= 0, None,
        With[{rows = Select[StringSplit[StringTrim[#], Whitespace, 5] & /@ StringSplit[ps["StandardOutput"], "\n"],
                Length[#] === 5 && StringMatchQ[#[[1]] <> #[[2]], DigitCharacter ..] &]},
            With[{tree = Prepend[descendantProcesses[Association[(FromDigits[#[[1]]] -> {FromDigits[#[[2]]], #[[5]], #[[4]]}) & /@ rows], {pid}], pid]},
                StringRiffle[Map[FileNameTake[#[[5]]] <> " " <> #[[3]] <> "% " <> #[[4]] &, Select[rows, MemberQ[tree, FromDigits[#[[1]]]] &]], ", "]
            ]
        ]
    ]
];
processReport[_] := None;

runEnded[run_Association, how_String] := If[StringQ[Lookup[run, "Label", None]], monitor[run["Label"] <> " " <> how]];

(* a run that has written nothing new for its "Stall" seconds, since it started or since its record,
   or a file it makes, last changed *)
stalledQ[run_Association] := NumericQ[Lookup[run, "Stall", None]] && AbsoluteTime[] - Max[run["Start"],
    Map[If[FileExistsQ[#], AbsoluteTime[FileDate[#]], 0] &, Select[Flatten[{Lookup[run, "Record", {}], Lookup[run, "Progress", {}]}], StringQ]]] > run["Stall"];

(* a process killed with the processes it started: the process first, so that it starts no more,
   then what it started, the youngest first; a process object gives its "PID" itself, where
   ProcessInformation does not *)
killProcessTree[process_ProcessObject] := With[{pid = process["PID"]},
    With[{table = If[IntegerQ[pid], processTable[], <||>]},
        KillProcess[process];
        If[IntegerQ[pid], endProcesses[table, First[processLayers[table, {pid}], {}]]]
    ]
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
    {work, runs, again, retries, alone, last, record},
    If[ changed === {} || n < 2, Return[0, Module]];
    work = CreateDirectory[];
    If[ ! DirectoryQ[builtDocsDir], CreateDirectory[builtDocsDir, CreateIntermediateDirectories -> True]];
    monitor["archive: " <> ToString[Length[changed]] <> " changed pages built in " <> ToString[n] <> " processes"];
    runs = pageShareRuns[source, docsDir, builtDocsDir, changed, n, limit, dependencies, work, "share"];
    runKernelScripts[runs, n, "archive: page shares"];
    unrecordedLogs[runs];
    (* a share stopped before its end has recorded none of its pages, which get one more process *)
    again = Catenate[Complement[#["Pages"], Keys[readRecord[#["Record"]]]] & /@ runs];
    retries = If[ again === {}, {},
        monitor["archive: " <> ToString[Length[again]] <> " pages again, in new processes"];
        pageShareRuns[source, docsDir, builtDocsDir, again, n, limit, dependencies, work, "again"]
    ];
    If[ retries =!= {}, runKernelScripts[retries, n, "archive: pages again"]; unrecordedLogs[retries]];
    (* the pages of a process that stopped a second time are built one at a time, a process each
       and no other beside it: processes that run together can hold each other up, and a page
       that builds alone is sound *)
    alone = Catenate[Complement[#["Pages"], Keys[readRecord[#["Record"]]]] & /@ retries];
    last = If[ alone === {}, {},
        monitor["archive: " <> ToString[Length[alone]] <> " pages one at a time"];
        pageShareRuns[source, docsDir, builtDocsDir, alone, n, limit, dependencies, work, "alone", 1]
    ];
    If[ last =!= {}, runKernelScripts[last, 1, "archive: pages one at a time"]; unrecordedLogs[last]];
    (* a page that stopped alone as well is one the stock build never finishes, and the last build,
       which has no time limit of its own, would wait on it without end *)
    $stuckPages = Catenate[Complement[#["Pages"], Keys[readRecord[#["Record"]]]] & /@ last];
    If[ $stuckPages =!= {}, monitor["archive: " <> ToString[Length[$stuckPages]] <> " pages did not build in their time, alone: " <> StringRiffle[$stuckPages, ", "]]];
    record = Join[readRecord[recordFile], Association[Select[Map[readRecord[#["Record"]] &, Join[runs, retries, last]], AssociationQ]]];
    If[ ! DirectoryQ[DirectoryName[recordFile]], CreateDirectory[DirectoryName[recordFile], CreateIntermediateDirectories -> True]];
    Put[record, recordFile];
    DeleteDirectory[work, DeleteContents -> True];
    Count[Lookup[record, changed, None], KeyValuePattern["Result" -> Except[_?FailureQ]]]
];

(* what the processes that recorded none or not all of their pages last wrote, when the deploy is
   monitored: a process that ended by itself without its record died, and its log says of what *)
unrecordedLogs[runs_List] := Scan[
    Function[run, With[{left = Complement[run["Pages"], Keys[readRecord[run["Record"]]]]},
        If[ left =!= {},
            monitor[run["Label"] <> " recorded " <> ToString[Length[run["Pages"]] - Length[left]] <> " of " <> ToString[Length[run["Pages"]]] <> " pages; its log ends: " <>
                logTail[run["Log"], 12]]]
    ]],
    runs
];
logTail[file_String, n_Integer] := If[ ! FileExistsQ[file], "(no log)",
    With[{lines = Select[StringSplit[ByteArrayToString[Replace[ReadByteArray[file], Except[_ByteArray] -> ByteArray[{}]], "ISO8859-1"], "\n"], StringLength[StringTrim[#]] > 0 &]},
        If[ lines === {}, "(empty)", StringRiffle[StringTake[#, UpTo[300]] & /@ Take[lines, -Min[n, Length[lines]]], " | "]]]
];
logTail[_, _] := "(no log)";

(* the shares of pages and their processes' scripts, two a process: each share reads the whole
   documentation tree once, and two keep every process busy to the end; a share that has built
   none of its pages for a page's time and ten minutes, the time the stock build takes to read the
   tree first, has stalled *)
pageShareRuns[source_String, docsDir_String, builtDocsDir_String, pages_List, n_Integer, limit_, dependencies_List, work_String, kind_String, size_ : Automatic] := With[
    {shares = Partition[pages, UpTo[If[IntegerQ[size], size, Max[1, Ceiling[Length[pages] / (2 n)]]]]]},
    MapIndexed[
        Function[{share, index}, With[{base = FileNameJoin[{work, kind <> "-" <> IntegerString[First[index]]}]},
            Export[base <> ".wl", pageShareScript[source, docsDir, builtDocsDir, share, base <> "-record.wl", base <> ".log", dependencies], "Text"];
            <|"Script" -> base <> ".wl", "Record" -> base <> "-record.wl", "Log" -> base <> ".log", "Pages" -> share, "Progress" -> (FileNameJoin[{builtDocsDir, #}] & /@ share),
              "Deadline" -> limit * Length[share] + 600, "Stall" -> limit + 600,
              "Label" -> "archive: " <> kind <> " " <> IntegerString[First[index]] <> " of " <> IntegerString[Length[shares]] <> " (" <> If[Length[share] === 1, FileBaseName[First[share]], IntegerString[Length[share]] <> " pages"] <> ")"|>
        ]],
        shares
    ]
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
     excludedQ, links, missing, declared, sources, flagged},
    (* the files the archive leaves out; a symbolic link that leads outside the paclet must be one *)
    excludedQ = Function[path, AnyTrue[Flatten[{Lookup[pageOptions, "Exclude", {}]}], StringMatchQ[path, #] &]];
    links = outsideLinks[dir];
    If[ ! AllTrue[links, excludedQ], Message[head::links, Select[links, ! excludedQ[#] &]]; Return[$Failed, Module]];
    buildDir = FileNameJoin[{dir, "build"}];
    prefix = StringReplace[name, "/" -> "__"];
    recordFile = builtPagesRecord[buildDir, prefix];
    sources = sourceHash[dir, docSource, pageOptions];
    builtDocsDir = FileNameJoin[{buildDir, prefix, "Documentation"}];
    authoring = FileNames["*.nb", Replace[docSource, Automatic -> FileNameJoin[{dir, "Documentation"}]], Infinity];
    (* the pages the stock build leaves out for their flag, which the archive goes without *)
    flagged = flaggedPages[Replace[docSource, Automatic -> FileNameJoin[{dir, "Documentation"}]]];
    If[ flagged =!= <||>, Message[head::flagged, Length[flagged], Take[Normal[flagged], UpTo[10]]]];
    (* the archives of earlier builds, which the scrape would pack, and their record; without
       "Incremental", the pages built last time too *)
    Scan[DeleteFile, FileNames[prefix ~~ ("" | ("-" ~~ ___)) ~~ ".paclet", buildDir]];
    If[ FileExistsQ[FileNameJoin[{buildDir, "archive.wl"}]], DeleteFile[FileNameJoin[{buildDir, "archive.wl"}]]];
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
    monitor["archive: " <> ToString[Length[authoring]] <> " documentation pages, " <> ToString[Length[changed]] <> " to build"];
    processes = If[ Length[changed] > 1, pageProcesses[Lookup[pageOptions, "Parallel", False]], 0];
    $stuckPages = {};
    parallel = buildPagesInParallel[source, FileNameJoin[{source, "Documentation"}], builtDocsDir, recordFile, changed, processes, Lookup[pageOptions, "TimeConstraint", 900], dependencies];
    If[ $stuckPages =!= {},
        If[ source =!= dir, DeleteDirectory[stageRoot, DeleteContents -> True]];
        Message[head::stuck, Length[$stuckPages], $stuckPages]; Return[$Failed, Module]];
    built = freshKernelBuild[source, buildDir, dependencies];
    If[ source =!= dir, DeleteDirectory[stageRoot, DeleteContents -> True]];
    If[ ! StringQ[built["Archive"]] || ! FileExistsQ[built["Archive"]], Message[head::build, built["Failure"]]; Return[$Failed, Module]];
    pages = archivePages[built["Archive"]];
    missing = Complement[
        Map[relativePath[Replace[docSource, Automatic -> FileNameJoin[{dir, "Documentation"}]], #] &, authoring],
        Map[Last[StringSplit[#, "/Documentation/", 2]] &, pages],
        Keys[flagged]
    ];
    If[ Length[missing] > failureBudget[Lookup[pageOptions, "MaxFailures", 0], Length[authoring] - Length[flagged]],
        Message[head::pages, Length[authoring] - Length[missing], Length[authoring], Take[missing, UpTo[10]]]; Return[$Failed, Module]];
    If[ missing =!= {}, Message[head::missing, Length[missing], Take[missing, UpTo[10]]]];
    load = Which[
        TrueQ[loadCheck],
            With[{paclet = PacletObject[File[dir]]}, archiveLoadCheck[built["Archive"], paclet["Version"], paclet["PrimaryContext"], dependencies]],
        AssociationQ[loadCheck],
            With[{paclet = PacletObject[File[dir]]}, archiveLoadCheckOnKernel[loadCheck, built["Archive"], paclet["Version"], paclet["PrimaryContext"], dependencies]],
        True,
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
    With[{result = <|"Archive" -> built["Archive"], "Pages" -> Length[pages], "Load" -> load, "DeclaredSymbols" -> declared, "Symbols" -> archiveSymbols[FileNameJoin[{buildDir, prefix}]], "ArchiveSize" -> Quantity[FileByteCount[built["Archive"]], "Bytes"],
          "Documentation" -> <|"Changed" -> Length[changed], "Pruned" -> Length[pruned], "Processes" -> processes, "BuiltInParallel" -> parallel, "Missing" -> missing, "Flagged" -> flagged|>|>},
        Put[
            <|"Archive" -> FileNameTake[built["Archive"]], "ArchiveHash" -> FileHash[built["Archive"], "SHA256", "HexString"], "Source" -> sources, "Result" -> result|>,
            FileNameJoin[{buildDir, "archive.wl"}]
        ];
        result
    ]
];

(* The archive an earlier build made, from the same sources, when "Build" -> False: the build and
   the deploy can run in separate processes. The build records, in build/archive.wl, what it
   made, a hash of the archive, and a hash of the sources: every file of the paclet but build/,
   hidden files and the files left out, the authoring notebooks, and the options that change the
   archive. *)
builtArchive[head_Symbol, dir_String, name_String, docSource_, pageOptions_Association] := Module[
    {buildDir = FileNameJoin[{dir, "build"}], stamp, archive},
    stamp = If[FileExistsQ[FileNameJoin[{buildDir, "archive.wl"}]], Get[FileNameJoin[{buildDir, "archive.wl"}]], None];
    archive = If[AssociationQ[stamp] && StringQ[stamp["Archive"]], FileNameJoin[{buildDir, stamp["Archive"]}], None];
    If[ ! StringQ[archive] || ! FileExistsQ[archive] || FileHash[archive, "SHA256", "HexString"] =!= stamp["ArchiveHash"] ||
            stamp["Source"] =!= sourceHash[dir, docSource, pageOptions],
        Message[head::nobuild, dir]; Return[$Failed, Module]];
    Append[stamp["Result"], "Archive" -> archive]
];

sourceHash[dir_String, docSource_, pageOptions_Association] := With[
    {excludedQ = Function[path, AnyTrue[Flatten[{Lookup[pageOptions, "Exclude", {}]}], StringMatchQ[path, #] &]]},
    digest[{
        Map[{#, FileHash[FileNameJoin[{dir, #}], "SHA256", "HexString"]} &,
            Sort @ Select[relativePath[dir, #] & /@ Select[FileNames["*", dir, Infinity], FileType[#] === File &],
                FileNameSplit[#][[1]] =!= "build" && ! AnyTrue[FileNameSplit[#], StringStartsQ[#, "."] &] && ! excludedQ[#] &]],
        If[ docSource === Automatic, None,
            Map[{relativePath[docSource, #], FileHash[#, "SHA256", "HexString"]} &, Sort @ Select[FileNames["*", docSource, Infinity], FileType[#] === File &]]],
        KeyTake[pageOptions, {"Exclude", "Symbols"}]
    }]
];

(* the documentation pages an archive may lack: Automatic, three or a hundredth of them, whichever
   is more *)
failureBudget[Automatic, pages_Integer] := Max[3, Ceiling[pages / 100]];
failureBudget[n_Integer?NonNegative, _] := n;
failureBudget[Infinity, _] := Infinity;
failureBudget[_, _] := 0;

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
    (* loaded whole before a deploy patches any of its functions for the deploy's duration: what
       loaded during it would be dropped with the patches *)
    Needs["PacletResource`"];
    Needs["PacletResource`DefinitionNotebook`"];
    Needs["ResourceSystemClient`"];
    (* the scrape computes the documentation's paths with PacletTools`, which a headless kernel
       does not load by itself; without it a $Failed lands in the scraped file listing *)
    Needs["PacletTools`"]
);

(* The start of a script that builds documentation pages: one parallel kernel, which the stock
   build's notebook export finds; with none it launches as many as the processors, in every process.
   And the end of every script run as a separate kernel process: Processes.wl read into it, then its
   endScript[], which ends what the script started and closes its kernels. *)
$pageKernelCode = "LaunchKernels[1];\n";

$endScriptCode = StringJoin["Get[", wlString[FileNameJoin[{$packageDirectory, "Processes.wl"}]], "];\nendScript[];\n"];

(* Parallel kernels closed: each ended first with KILL, with the processes it started, its front
   ends before it, since a front end that quits, or loses its kernel, can crash on its way out, and
   a kernel this kernel kills it reaps, where one left to exit by itself after CloseKernels it may
   not *)
closeKernels[{}, ___] := {};
(* known gives the process ids taken while the kernels were idle: a kernel that is busy when it is
   closed, a page's code still running in it, does not answer for its id, and would be left
   running with CloseKernels waiting on it *)
closeKernels[kernels_List, known_List : {}] := With[{pids = Union[known, kernelProcessIDs[kernels]]},
    With[{table = processTable[]}, endProcesses[table, frontEndProcesses[table, pids]]];
    endProcesses[processTable[], pids];
    CloseKernels[kernels]
];

kernelProcessIDs[kernels_List] := Select[Flatten[{TimeConstrained[ParallelEvaluate[$ProcessID, kernels, DistributedContexts -> None], 30, {}]}], IntegerQ];

(* the front end processes the kernels started, among the processes descended from them *)
kernelFrontEnds[kernels_List] := frontEndProcesses[processTable[], kernelProcessIDs[kernels]];

(* The parallel kernels of a deploy, launched for it and closed after it: those "Parallel" asks for,
   as buildKernels counts them; f gets the kernels *)
withDeployKernels[parallel_, f_] := Module[{before = Kernels[], kernels},
    kernels = buildKernels[parallel];
    WithCleanup[f[kernels], closeKernels[Complement[Kernels[], before]]]
];

(* what every page's transform is made with: the version of the stock transform, the definitions of
   PacletPage's own part in it, and the rules the symbols' boxes are made with. A PacletPage release
   that leaves the transform's code as it was keeps every page transformed before. *)
transformContext[symbolRules_List, pinned_List] := {
    $VersionNumber,
    Replace[PacletFind["PacletResource"], {{paclet_, ___} :> paclet["Version"], _ -> None}],
    transformCode[],
    symbolRules, pinned
};

(* a digest of the definitions the transform of a page runs, and of those its key is made by *)
transformCode[] := digest[List @@ Map[Function[s, {OwnValues[s], DownValues[s], SubValues[s]}, HoldAllComplete], Hold[
    transformRule, transformPages, transformFiles, $pageTransform, transformKey, transformFile, pacletLinks, heldNotebook, transformPageContext, showsSymbolQ,
    orangeLinkRule, installDeployFixes, withoutNoise, withoutLocalNumbers, localQ, localKey, localName, $localNamePattern,
    $buildNoise, $frontEndNoise
]]];

Options[DeployPacletResource] = {
    "Account" -> Automatic,
    "DryRun" -> False,
    "DocumentationSource" -> Automatic,
    "HeroImage" -> Automatic,
    "Examples" -> Automatic,
    "Archive" -> None,
    "Commit" -> Automatic,
    "Republish" -> False,
    "LoadCheck" -> True,
    "EvaluationKernel" -> None,
    "Incremental" -> False,
    "Parallel" -> False,
    "TimeConstraint" -> 900,
    "Exclude" -> {},
    "ArchiveCheck" -> None,
    "Symbols" -> Automatic,
    "Monitor" -> False,
    "Upload" -> Automatic,
    "Build" -> True,
    "MaxFailures" -> 0,
    "SourceArchive" -> None,
    Permissions -> "Public"
};

DeployPacletResource::blockers = "The definition notebook's own check finds these errors: `1`.";
DeployPacletResource::nobuild = "`1`/build holds no archive built from the paclet's present sources; build it first, with \"DryRun\" -> True, or deploy with \"Build\" -> True.";
DeployPacletResource::write = "`1` Nothing was deleted, so the resource keeps its pages; deploy again.";
DeployPacletResource::nodef = "`1` has no ResourceDefinition.nb or no documentation: build it first with BuildPacletDocumentation.";
DeployPacletResource::build = "PacletBuild failed: `1`.";
DeployPacletResource::stuck = SubmitPacletResource::stuck = "`1` documentation pages did not build within their time, the last time each alone in a process with no other beside it, so the stock documentation build does not finish them and nothing was built or deployed: `2`.";
DeployPacletResource::pages = "The built archive holds `1` of the `2` documentation pages; nothing was deployed. Missing: `3`.";
DeployPacletResource::missing = "`1` documentation pages did not build, within \"MaxFailures\"; the archive goes without them: `2`.";
DeployPacletResource::flagged = "`1` documentation pages carry a flag the documentation build leaves a page out for, and the archive goes without them: `2`.";
DeployPacletResource::library = "PacletInfo.wl declares LibraryLink libraries, and the archive holds none under `1`; nothing was deployed.";
DeployPacletResource::archivecheck = "The archive check `1` did not give True; nothing was deployed.";
DeployPacletResource::links = "These symbolic links lead outside the paclet, and the archive would hold what they lead to: `1`; nothing was deployed. Remove them, or leave them out with \"Exclude\".";
DeployPacletResource::scrape = "Scraping the resource from `1` failed: `2`.";
DeployPacletResource::packed = "The scrape packed `1`, not the fresh build `2`; nothing was deployed.";
DeployPacletResource::deploy = "Deploying the resource failed: `1`.";
DeployPacletResource::cloud = "Not connected to the Wolfram Cloud.";
DeployPacletResource::stalled = "The upload of `1` did not end within `2` seconds, three times; on a network whose VPN drops full-size packets, send the deploy through a proxy that sets a smaller segment size.";
DeployPacletResource::account = "Connected to the Wolfram Cloud as `1`, not as `2`; connect as `2` to publish this resource.";
DeployPacletResource::archive = "The archive could not be published to `1`: `2`.";
DeployPacletResource::load = "The built archive does not load cleanly in a fresh kernel: `1`; nothing was deployed.";
DeployPacletResource::commit = "`1` is already published from commit `2`, not `3`; nothing was deployed. Bump the version, or use \"Republish\" -> True to replace it.";
DeployPacletResource::unknowncommit = "`1` is already published from commit `2`, and this paclet's commit is unknown (not a git checkout); nothing was deployed. Pass \"Commit\", or use \"Republish\" -> True.";
DeployPacletResource::nocommit = "`1` is already published and records no commit; nothing was deployed. Use \"Republish\" -> True to replace it.";

(* A deploy runs with the resource system's daily refresh of its public-resource information done
   synchronously, not by an asynchronous request: on Wolfram 15.0 the asynchronous request's body,
   delivered as a byte array on CURLLink's own thread, can crash the kernel (a freed chunk read in
   libcurllink), as it did in the middle of a deploy's definition notebook three times. The
   information is then read from the local cache, as it is between refreshes. *)
(* The paclet manager's weekly update of its sites likewise starts asynchronous transfers, from a task
   it schedules when it starts, which can fire in the middle of a deploy: on 15.0 one in flight was
   followed by those crashes each time, and a kernel that ends with one in flight crashes too. A
   deploy keeps the weekly update from starting, and refreshes the sites itself, synchronously, once
   a kernel, before it deploys. *)
$sitesRefreshed = False;
refreshPacletSites[] := If[ ! TrueQ[$sitesRefreshed] && TrueQ[$AllowInternet],
    $sitesRefreshed = True;
    monitor["the paclet sites refreshed"];
    TimeConstrained[Scan[PacletSiteUpdate[#, Asynchronous -> False] &, PacletSites[]], 60]
];

(* The sites whose last update succeeded over three days ago, or never, by the paclet manager's own
   record, which its weekly update reads; none where that record is not at hand *)
staleSites[] := If[ DownValues[PacletManager`Services`Private`getPacletSiteData] === {} || DownValues[PacletManager`Package`differenceInDays] === {}, {},
    Cases[PacletManager`Services`Private`getPacletSiteData[],
        {url_String, _, _, _, last_, ___} /; ! MatchQ[last, {__Integer} | {__?NumericQ}] || PacletManager`Package`differenceInDays[last, Date[]] > 3 :> url]
];

(* When PacletPage loads in a kernel whose sites are stale, such as each container of a build on a
   new pod, it refreshes them synchronously at once, so that the weekly update the paclet manager
   scheduled when the kernel started finds them fresh when it fires, later, in the middle of
   whatever the kernel does then, and starts nothing. Not on parallel kernels, which start no weekly
   update. It is a convenience, so loading never waits on it long nor reports it: each site has twelve
   seconds, and a site that cannot be reached, its connection refused or timed out, ends it, the
   network being down; its messages, the transfer's and the update's, are the expected answer of an
   unreachable site, kept off the output, the one purpose of this Quiet, and the paclet manager's
   own weekly update then runs as it would have. *)
refreshStaleSites[] := If[ $KernelID === 0 && TrueQ[$AllowInternet],
    With[{stale = staleSites[]},
        Catch[Scan[
            If[ ! MatchQ[TimeConstrained[Quiet[PacletSiteUpdate[#, Asynchronous -> False], {URLFetch::invhttp, PacletSiteUpdate::err, General::stop}], 12, $TimedOut], _PacletSiteObject],
                Throw[Null, "sites"]] &,
            stale
        ], "sites"]
    ]
];

DeployPacletResource[dir_String, opts : OptionsPattern[]] := Block[{$deployMonitor = TrueQ[$deployMonitor] || TrueQ[OptionValue["Monitor"]],
    ResourceSystemClient`$AsyncronousResourceInformationUpdates = False, PacletManager`Services`Private`doWeeklyUpdate = Null &}, Catch[Module[
    {definition, paclet, name, built, resource, location, hero, deployed, archive, commit, versioned, offline, target, accountOK, removed, symbolRules, pinned,
     sourceVersioned, source},
    definition = FileNameJoin[{dir, "ResourceDefinition.nb"}];
    If[ ! readyToPublishQ[DeployPacletResource, dir, OptionValue["DocumentationSource"]], Throw[$Failed, "deploy"]];
    offline = offlineDryRunQ[OptionValue["DryRun"]];
    If[ ! offline && ! TrueQ[OptionValue["DryRun"]], refreshPacletSites[]];
    $deployedDocumentation = None;
    $pages = newPages[];
    $transform = None;
    accountOK = offline || publishingAccountQ[DeployPacletResource, OptionValue["Account"]];
    If[ stopsQ[accountOK, OptionValue["DryRun"]], Throw[$Failed, "deploy"]];
    PacletDirectoryLoad[dir];
    paclet = PacletObject[File[dir]];
    name = paclet["Name"];
    loadResourceTools[];
    (* the fixes probe the cloud; a dry run writes nothing there *)
    If[ ! TrueQ[OptionValue["DryRun"]], installDeployFixes[]; installHeroPatch[]; installExamplesPatch[]];
    (* the version gate, before anything is built: on the deployed resource, which records the
       version and commit of its release, and on the versioned archive *)
    commit = Replace[OptionValue["Commit"], Automatic :> gitCommit[dir]];
    versioned = Replace[OptionValue["Archive"], {
        dirName_String :> StringTrim[dirName, "/"] <> "/" <> StringReplace[name, "/" -> "__"] <> "-" <> paclet["Version"] <> ".paclet",
        _ -> None
    }];
    sourceVersioned = Replace[OptionValue["SourceArchive"], {
        dirName_String :> sourceArchiveNames[dirName, name, paclet["Version"]]["Version"],
        _ -> None
    }];
    target = If[ offline, None,
        ResourceSystemClient`Private`loadDeployResourceShingle[];
        Replace[DeployedResourceShingle`Private`defaultCloudObjectLocation["Paclet", name], Except[_CloudObject] :> None]
    ];
    (* the gate is about the deployment of the account checked: in a dry run as another account
       there is nothing to gate *)
    If[ ! offline && accountOK && ! TrueQ[OptionValue["Republish"]],
        If[ MatchQ[target, _CloudObject] && stopsQ[releaseAllowedQ[DeployPacletResource, paclet["Version"] <> " at " <> First[target], deployedCommit[target, paclet["Version"]], commit], OptionValue["DryRun"]],
            Throw[$Failed, "deploy"]];
        If[ StringQ[sourceVersioned] && stopsQ[releaseAllowedQ[DeployPacletResource, sourceVersioned, publishedCommit[sourceVersioned], commit], OptionValue["DryRun"]],
            Throw[$Failed, "deploy"]];
        If[ StringQ[versioned] && stopsQ[releaseAllowedQ[DeployPacletResource, versioned, publishedCommit[versioned], commit], OptionValue["DryRun"]],
            Throw[$Failed, "deploy"]]
    ];
    built = With[
        {pageOptions = <|"Incremental" -> OptionValue["Incremental"], "Parallel" -> OptionValue["Parallel"], "TimeConstraint" -> OptionValue["TimeConstraint"], "Exclude" -> OptionValue["Exclude"], "ArchiveCheck" -> OptionValue["ArchiveCheck"], "Symbols" -> OptionValue["Symbols"], "MaxFailures" -> OptionValue["MaxFailures"]|>},
        If[ TrueQ[OptionValue["Build"]],
            freshArchive[DeployPacletResource, dir, name, OptionValue["DocumentationSource"], loadCheckOf[OptionValue["LoadCheck"], OptionValue["EvaluationKernel"], dir], pageOptions],
            builtArchive[DeployPacletResource, dir, name, OptionValue["DocumentationSource"], pageOptions]
        ]
    ];
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
    (* the symbols' copied form and the declared symbols, in this kernel and the parallel ones *)
    symbolRules = If[ offline, {}, symbolBoxRules[name, installForm[name, paclet["Version"], archiveURL[OptionValue["Archive"], name, paclet["Version"]], If[MatchQ[target, _CloudObject], First[target], None]]]];
    pinned = pinRules[name, built["Symbols"]];
    {resource, location, deployed} = Replace[
        If[ offline, {None, None, None}, UsingFrontEnd @ Internal`InheritedBlock[
            {PacletResource`Private`fancyPacletSymbolBoxes, PacletResource`Private`pacletDeclaredSymbols, DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ,
             PacletResource`Private`createDocumentationEmbedPage, PacletResource`Private`createSidebarHTML, PacletResource`Notebooks`ProcessNotebookForEmbedding, PacletResource`Private`createSidebarLinkSection,
             CreatePacletArchive, $rewrite},
            $rewrite = rewriteKinds[OptionValue["Upload"]];
            installPageRule[];
            installRules[PacletResource`Private`fancyPacletSymbolBoxes, symbolRules];
            installRules[PacletResource`Private`pacletDeclaredSymbols, pinned];
            installRules[PacletResource`Notebooks`ProcessNotebookForEmbedding, {transformRule[]}];
            installRules[PacletResource`Private`createSidebarLinkSection,
                guideTreeRules[guideChildren[FileNameJoin[{dir, "build", StringReplace[name, "/" -> "__"], "Documentation", "English", "Guides"}], name], Last[StringSplit[name, "/"]]]];
            codeLintOff[];
            installArchiveRule[FileNameJoin[{dir, "build", StringReplace[name, "/" -> "__"]}], built["Archive"]];
            Module[{notebook, scraped, versioned, where, packed, done},
            monitor["scraping " <> definition];
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
                    Message[DeployPacletResource::scrape, definition, scraped];
                    With[{errors = scrapeErrors[notebook]}, If[errors =!= {}, Message[DeployPacletResource::blockers, errors]]];
                    $Failed,
                ! StringQ[packed] || realPath[packed] =!= realPath[built["Archive"]],
                    Message[DeployPacletResource::packed, packed, built["Archive"]]; $Failed,
                TrueQ[OptionValue["DryRun"]],
                    {versioned, where, None},
                True,
                    With[{deploy = Block[{$heroImage = hero, $examples = OptionValue["Examples"]}, withDeployKernels[
                            If[built["Pages"] >= 24, OptionValue["Parallel"], False],
                            Function[kernels,
                                $transform = newTransform[FileNameJoin[{dir, "build", "transform"}], transformContext[symbolRules, pinned], symbolRules, pinned, kernels];
                                withoutLanguageData[stopOnCloudWriteFailure[CloudDeploy[versioned, where, Permissions -> OptionValue[Permissions]]]]
                            ]
                        ]]},
                        If[ MatchQ[deploy, _CloudObject], pruneTransforms[]];
                        savePages[MatchQ[deploy, _CloudObject]];
                        monitor["pages: " <> ToString[$pages["Made"]] <> " made, " <> ToString[$pages["Unchanged"]] <> " unchanged"];
                        If[ FailureQ[deploy], Message[DeployPacletResource::write, deploy["Message"]]; deploy, {versioned, where, deploy}]
                    ]
            ];
            (* the deploy reads the notebook's data, so the notebook closes after it *)
            NotebookClose[notebook];
            done
        ]]],
        {$Failed :> Throw[$Failed, "deploy"], failure_Failure :> Throw[failure, "deploy"]}
    ];
    If[ deployed =!= None && ! MatchQ[deployed, _CloudObject], Message[DeployPacletResource::deploy, deployed]; Throw[$Failed, "deploy"]];
    If[ deployed =!= None, recordRelease[location, paclet["Version"], commit]; URLRead[First[location] <> "/", TimeConstraint -> 60]];
    removed = If[deployed =!= None, pruneDocumentation[], 0];
    archive = Replace[OptionValue["Archive"], {
        None -> None,
        dirName_String /; TrueQ[OptionValue["DryRun"]] :> None,
        dirName_String :> With[{published = publishArchive[built["Archive"], dirName, name, OptionValue[Permissions], commit]},
            If[AssociationQ[published], published, Message[DeployPacletResource::archive, dirName, published]; Throw[$Failed, "deploy"]]
        ]
    }];
    source = Replace[OptionValue["SourceArchive"], {
        None -> None,
        dirName_String /; TrueQ[OptionValue["DryRun"]] :> None,
        dirName_String :> With[{published = publishSourceArchive[dir, dirName, name, paclet["Version"], OptionValue[Permissions], commit, OptionValue["Exclude"]]},
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
        "SourceArchive" -> source,
        "ArchiveSize" -> built["ArchiveSize"],
        "Version" -> paclet["Version"],
        "Commit" -> commit,
        "Load" -> built["Load"],
        "AppearanceElements" -> $appearanceElements,
        "Removed" -> removed,
        "Written" -> <|
            "Files" -> If[AssociationQ[$deployedDocumentation], $deployedDocumentation["Written"], 0],
            "Pages" -> $pages["Made"],
            "UnchangedPages" -> $pages["Unchanged"]
        |>,
        "Transformed" -> If[ AssociationQ[$transform],
            <|"Transformed" -> $transform["Transformed"], "Cached" -> $transform["Cached"], "TransformedAgain" -> $transform["Again"]|>,
            <|"Transformed" -> 0, "Cached" -> 0, "TransformedAgain" -> 0|>
        ]
    |>
], "deploy"]];

(* ==================== submitting to the Paclet Repository ==================== *)

Options[SubmitPacletResource] = {
    "Account" -> Automatic, "DryRun" -> False, "DocumentationSource" -> Automatic, "LoadCheck" -> True, "EvaluationKernel" -> None,
    "Incremental" -> False, "Parallel" -> False, "TimeConstraint" -> 900, "Exclude" -> {}, "ArchiveCheck" -> None, "Symbols" -> Automatic
};

SubmitPacletResource::nodef = DeployPacletResource::nodef;
SubmitPacletResource::build = DeployPacletResource::build;
SubmitPacletResource::pages = DeployPacletResource::pages;
SubmitPacletResource::missing = DeployPacletResource::missing;
SubmitPacletResource::flagged = DeployPacletResource::flagged;
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
SubmitPacletResource[dir_String, opts : OptionsPattern[]] := Block[
    {ResourceSystemClient`$AsyncronousResourceInformationUpdates = False, PacletManager`Services`Private`doWeeklyUpdate = Null &},
    submitPacletResource[dir, opts]
];
submitPacletResource[dir_String, opts : OptionsPattern[SubmitPacletResource]] := Catch[Module[
    {definition, paclet, version, publisher, issues, built, result, offline},
    definition = FileNameJoin[{dir, "ResourceDefinition.nb"}];
    If[ ! readyToPublishQ[SubmitPacletResource, dir, OptionValue["DocumentationSource"]], Throw[$Failed, "submit"]];
    offline = offlineDryRunQ[OptionValue["DryRun"]];
    If[ stopsQ[offline || publishingAccountQ[SubmitPacletResource, OptionValue["Account"]], OptionValue["DryRun"]], Throw[$Failed, "submit"]];
    PacletDirectoryLoad[dir];
    paclet = PacletObject[File[dir]];
    {version, publisher} = {paclet["Version"], paclet["PublisherID"]};
    (* the repository takes only a version newer than the ones it has *)
    If[ ! offline,
        With[{latest = Last[SortBy[Map[#["Version"] &, PacletFindRemote[paclet["Name"], UpdatePacletSites -> True]], versionKey], None]},
            If[ StringQ[latest] && ! newerVersionQ[version, latest],
                Message[SubmitPacletResource::published, paclet["Name"], latest, version];
                If[ ! TrueQ[OptionValue["DryRun"]], Throw[$Failed, "submit"]]]
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
    ]
];
    If[ issues =!= {}, Message[SubmitPacletResource::check, Lookup[issues, "Tag"]]; Throw[$Failed, "submit"]];
    built = freshArchive[SubmitPacletResource, dir, paclet["Name"], OptionValue["DocumentationSource"], loadCheckOf[OptionValue["LoadCheck"], OptionValue["EvaluationKernel"], dir],
        <|"Incremental" -> OptionValue["Incremental"], "Parallel" -> OptionValue["Parallel"], "TimeConstraint" -> OptionValue["TimeConstraint"], "Exclude" -> OptionValue["Exclude"], "ArchiveCheck" -> OptionValue["ArchiveCheck"], "Symbols" -> OptionValue["Symbols"]|>];
    If[ built === $Failed, Throw[$Failed, "submit"]];
    (* Scrape and submit with the notebook open: the scraped record reads its notebook data while
       it is open, and a closed notebook puts $Failed into the request. *)
    result = If[ offline, <|"Submitted" -> False, "Archive" -> built["Archive"]|>, UsingFrontEnd @ Internal`InheritedBlock[{CreatePacletArchive}, Module[{notebook, scraped, packed, submitted},
        installArchiveRule[FileNameJoin[{dir, "build", StringReplace[paclet["Name"], "/" -> "__"]}], built["Archive"]];
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
            True, withoutLanguageData[ResourceSubmit[ResourceObject[scraped], "Version" -> version, PublisherID -> publisher]]
        ];
        NotebookClose[notebook];
        submitted
    ]]];
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
    "SiteOptions" -> {},
    "CheckOptions" -> {},
    "Monitor" -> False
};

(* versions compared by their numbers: 1.0.10 is newer than 1.0.9 *)
versionKey[version_String] := FromDigits /@ StringCases[version, DigitCharacter ..];
newerVersionQ[version_String, than_String] := With[{a = versionKey[version], b = versionKey[than]},
    With[{n = Max[Length[a], Length[b]]}, Order[PadRight[b, n], PadRight[a, n]] === 1]
];

(* the resource options a submission builds its archive with too *)
submissionOptions[options_List] := Sequence @@ FilterRules[options, Except["Account" | "DryRun", Options[SubmitPacletResource]]];

(* the resource's options and, after them, the build's evaluation kernel: a paclet whose examples
   run on another kernel is checked to load alone there *)
resourceOptionsWith[resourceOptions_, buildOptions_] := Join[
    Cases[Flatten[{resourceOptions}], _Rule | _RuleDelayed],
    Replace[Lookup[Association[Cases[Flatten[{buildOptions}], _Rule]], "EvaluationKernel", None], {None -> {}, evaluation_ :> {"EvaluationKernel" -> evaluation}}]
];

PublishPacletPage::cloud = DeployPacletResource::cloud;
PublishPacletPage::account = "Connected to the Wolfram Cloud as `1`, not as `2`; nothing was built or published. Connect as `2` to publish this paclet.";

PublishPacletPage[dir_String, name_String, opts : OptionsPattern[]] := Block[{$deployMonitor = TrueQ[$deployMonitor] || TrueQ[OptionValue["Monitor"]],
    ResourceSystemClient`$AsyncronousResourceInformationUpdates = False, PacletManager`Services`Private`doWeeklyUpdate = Null &}, Catch[Module[
    {build, resource, catalog, site, check, monitored = TrueQ[OptionValue["Monitor"]]},
    (* the account first: a paclet's documentation may publish as it is built, as PacletPage's own
       tutorial does, so a dry run that builds takes the account too; one that does not only reports *)
    If[ stopsQ[offlineDryRunQ[OptionValue["DryRun"]] || publishingAccountQ[PublishPacletPage, OptionValue["Account"]], TrueQ[OptionValue["DryRun"]] && ! TrueQ[OptionValue["Build"]]],
        Throw[$Failed, "publish"]];
    build = If[ TrueQ[OptionValue["Build"]], BuildPacletDocumentation[dir, Sequence @@ OptionValue["BuildOptions"], "Monitor" -> monitored], None];
    If[ build === $Failed, Throw[$Failed, "publish"]];
    (* the parallel kernels the deploy asks for serve the site's uploads too, so they stay until the
       site is published and checked *)
    withDeployKernels[Lookup[Association[Cases[Flatten[{OptionValue["ResourceOptions"]}], _Rule]], "Parallel", False], Function[kernels,
        resource = DeployPacletResource[dir, "Account" -> OptionValue["Account"], "DryRun" -> OptionValue["DryRun"],
            Sequence @@ resourceOptionsWith[OptionValue["ResourceOptions"], OptionValue["BuildOptions"]]];
        If[ resource === $Failed || FailureQ[resource], Throw[resource, "publish"]];
        If[ TrueQ[OptionValue["DryRun"]],
            Throw[<|"Build" -> build, "Resource" -> resource, "Site" -> None, "Check" -> None,
                "Submission" -> If[TrueQ[OptionValue["Submit"]], SubmitPacletResource[dir, "Account" -> OptionValue["Account"], "DryRun" -> True, submissionOptions[resourceOptionsWith[OptionValue["ResourceOptions"], OptionValue["BuildOptions"]]]], None]|>, "publish"]];
        (* the version just deployed, whatever the catalog read from the cloud *)
        catalog = Replace[PacletPageCatalog[resource["Resource"], "Documentation" -> dir, "Monitor" -> monitored],
            c_Association :> Append[c, "Version" -> resource["Version"]]];
        If[ catalog === $Failed, Throw[$Failed, "publish"]];
        (* the site's options before the paclet's sources, so a "Source" among them wins *)
        site = DeployPacletPage[catalog, name, "Account" -> OptionValue["Account"], Sequence @@ OptionValue["SiteOptions"], "Source" -> dir];
        If[ site === $Failed, Throw[$Failed, "publish"]];
        (* a site just published may have pages the cloud has not rendered yet: what fails is
           checked again for up to two minutes *)
        check = If[ TrueQ[OptionValue["Check"]], CheckPacletPage[catalog, name, Sequence @@ OptionValue["CheckOptions"], "Pages" -> All, "Wait" -> 120], None]
    ]
];
    <|
        "Build" -> build, "Resource" -> resource, "Site" -> site, "Check" -> check,
        "Submission" -> If[TrueQ[OptionValue["Submit"]], SubmitPacletResource[dir, "Account" -> OptionValue["Account"], submissionOptions[resourceOptionsWith[OptionValue["ResourceOptions"], OptionValue["BuildOptions"]]]], None]
    |>
], "publish"]];
