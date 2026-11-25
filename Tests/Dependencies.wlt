(* Offline regression tests for consumer dependency management. The installation test creates
   a unique paclet, and removes only the copies it created. *)
PacletDirectoryLoad[DirectoryName[$TestFileName, 2]]
Needs["WolframInstitute`PacletPage`"]

Begin["WolframInstitute`PacletPage`Private`"]

VerificationTest[
    Get[PacletObject[File[DirectoryName[$TestFileName, 2]]]["AssetLocation", "DependencyLoader"]],
    Null, TestID -> "Dependencies: the standalone loader recognizes the active manager"
]

VerificationTest[
    With[{loader = PacletObject[File[DirectoryName[$TestFileName, 2]]]["AssetLocation", "DependencyLoader"],
            older = PacletObject[<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.83", "Extensions" -> {}|>]},
        Block[{$Packages = DeleteCases[$Packages, "WolframInstitute`PacletPage`"], PacletFind = ({older} &),
                PacletInstall = (Throw["Unexpected installation"] &)},
            With[{result = Get[loader]}, {First[result], StringStartsQ[result["InstallCommand"], "PacletInstall[ResourceObject["] &&
                StringFreeQ[result["Message"], result["InstallCommand"]]}]]],
    {"DependencyManagerRequired", True}, TestID -> "Dependencies: a legacy release gives an explicit first-install command"
]

VerificationTest[
    Module[{root = CreateDirectory[], checkout = DirectoryName[$TestFileName, 2], source, resources, loader,
            archive, consumer, packages = "WolframInstitute`PacletPage`", files = {}, reused, result, incompatible},
        WithCleanup[
            source = CreateDirectory[FileNameJoin[{root, "manager", "Kernel"}]];
            CopyFile[FileNameJoin[{checkout, "PacletInfo.wl"}], FileNameJoin[{DirectoryName[source], "PacletInfo.wl"}]];
            CopyFile[FileNameJoin[{checkout, "Kernel", "PacletPage.wl"}], FileNameJoin[{source, "PacletPage.wl"}]];
            resources = CreateDirectory[FileNameJoin[{root, "consumer", "Resources"}]];
            Put[PacletObject[<|"Name" -> "Demo/Consumer", "Version" -> "1.0"|>], FileNameJoin[{DirectoryName[resources], "PacletInfo.wl"}]];
            loader = CopyFile[FileNameJoin[{checkout, "Assets", "LoadDependencyManager.wl"}], FileNameJoin[{resources, "LoadDependencyManager.wl"}]];
            archive = CreatePacletArchive[DirectoryName[source], root];
            CopyFile[archive, FileNameJoin[{resources, "PacletPage.paclet"}]];
            consumer = PacletObject[File[CreatePacletArchive[DirectoryName[resources], root]]];
            Internal`InheritedBlock[{PacletFind, Needs}, Unprotect[PacletFind, Needs];
                PacletFind["WolframInstitute/PacletPage"] = {};
                Needs["WolframInstitute`PacletPage`", file_String] := (AppendTo[files, file]; Null);
                Block[{$Packages = DeleteCases[$Packages, packages], $CacheBaseDirectory = root},
                    result = Get[loader];
                    reused = Block[{ExtractPacletArchive = (Throw["Unexpected second extraction"] &)}, Get[loader]];
                    If[files =!= {}, PacletDirectoryUnload[DirectoryName[First[files], 2]]];
                    Put[PacletObject[Join[PacletObject[File[DirectoryName[source]]]["PacletInfo"],
                        <|"WolframVersion" -> "99+"|>]], FileNameJoin[{DirectoryName[source], "PacletInfo.wl"}]];
                    CopyFile[CreatePacletArchive[DirectoryName[source], CreateDirectory[FileNameJoin[{root, "future"}]]],
                        FileNameJoin[{resources, "PacletPage.paclet"}], OverwriteTarget -> True];
                    incompatible = Get[loader]]];
            {consumer["Name"], result, reused, MatchQ[files, {_String, _String}] && SameQ @@ files && FileExistsQ[First[files]],
                MatchQ[incompatible, Failure["DependencyManagerIncompatible", _Association]]},
            DeleteDirectory[root, DeleteContents -> True]]],
    {"Demo/Consumer", Null, Null, True, True}, TestID -> "Dependencies: a packaged consumer reuses its compatible offline manager and rejects an incompatible bundle"
]

(* a consumer that declares the required dependencies, then the optional ones *)
testDependencyOwner[required_, optional_] := PacletObject[<|
    "Name" -> "Demo/Consumer", "Version" -> "1.0", "PrimaryContext" -> "Demo`Consumer`",
    "Dependencies" -> Join[required, Append[dependencyEntry[#], "Optional" -> True] & /@ optional]|>]

VerificationTest[
    Normal[PacletDependencyInformation[testDependencyOwner[{}, {}]]],
    <||>, TestID -> "Dependencies: empty manifest"
]

VerificationTest[
    With[{owner = Function[declared, PacletObject[<|"Name" -> "Demo/Consumer", "Version" -> "1.0", "Dependencies" -> declared|>]]},
        {#["Required"] & /@ dependencyManifest[owner[{"Demo/A", <|"Name" -> "Demo/B", "Optional" -> True|>, <|"Name" -> "Demo/C", "Optional" -> False|>}]],
         FailureQ[dependencyManifest[owner[{<|"Name" -> "Demo/B", "Optional" -> "yes"|>}]]]}],
    {<|"Demo/A" -> True, "Demo/B" -> False, "Demo/C" -> True|>, True},
    TestID -> "Dependencies: one Dependencies list, where a dependency is optional when its declaration says \"Optional\" -> True"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{"PacletTools", "Demo/A" -> "2.0.*"}, {<|"Name" -> "Demo/B", "Version" -> "1.2+"|>}]},
        {Keys[PacletDependencyInformation[owner]], Values[Normal[PacletDependencyInformation[owner]][[All, "Required"]]],
            Keys[PacletDependencyInformation[owner, {"Demo/B", "PacletTools", "Demo/B"}]],
            Normal[PacletDependencyInformation[owner, {}]]}],
    {{"PacletTools", "Demo/A", "Demo/B"}, {True, True, False}, {"Demo/B", "PacletTools"}, <||>},
    TestID -> "Dependencies: records and standard declarations preserve order and selection"
]

VerificationTest[
    Module[{names = Join[{"PacletTools"}, Table["Demo/Optional" <> IntegerString[index], {index, 30}]], owner},
        owner = testDependencyOwner[{}, names];
        Normal[PacletDependencyInformation[owner]] === Join @@ (PacletDependencyInformation[owner, #, "Output" -> "Association"] & /@ names)],
    True, TestID -> "Dependencies: a bulk snapshot preserves the result and order of individual lookups"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {"Demo/Test"}]},
        With[{report = PacletDependencyInformation[owner], raw = PacletDependencyInformation[owner, "Output" -> "Association"]},
            {Head[report], Normal[report] === raw, report["Demo/Test"] === raw["Demo/Test"], Values[report] === Values[raw],
                Length[report], MissingQ[report["Demo/Unknown"]], First[PacletDependencyInformation[owner, "Output" -> "Invalid"]]}]],
    {PacletDependencyReport, True, True, True, 1, True, "DependencyOutput"}, TestID -> "Dependencies: the report preserves access to raw records"
]

VerificationTest[
    With[{report = PacletDependencyInformation[testDependencyOwner[{}, {"Demo/Library"}]]},
        With[{boxes = ToBoxes[report, StandardForm]},
            {! FreeQ[boxes, _InterpretationBox], ToExpression[boxes, StandardForm] === report}]],
    {True, True}, TestID -> "Dependencies: a notebook summary round-trips to the complete report"
]

VerificationTest[
    With[{local = PacletObject[<|"Name" -> "Demo/Library", "Version" -> "1.0", "Description" -> "Installed description."|>]},
        Block[{dependencyInstallations = Function[names, AssociationMap[Function[name, {local}], names]],
                dependencyRelease = Function[Null, Throw["unexpected release lookup"]]},
            {PacletDependencyInformation[testDependencyOwner[{}, {"Demo/Library"}]]["Demo/Library"]["Description"],
                PacletDependencyInformation[testDependencyOwner[{}, {<|"Name" -> "Demo/Library", "Description" -> "Consumer description."|>}]]["Demo/Library"]["Description"]}]],
    {"Installed description.", "Consumer description."}, TestID -> "Dependencies: installed metadata supplies a description when the consumer omits one"
]

VerificationTest[
    With[{site = PacletObject[<|"Name" -> "Demo/Library", "Version" -> "1.1", "Description" -> "A release description."|>]},
        Block[{dependencyRelease = Function[Null, <|"Version" -> "1.1", "SitePaclet" -> site|>]},
            PacletDependencyInformation[testDependencyOwner[{}, {"Demo/Library"}], "CheckUpdates" -> True]["Demo/Library"]["Description"]]],
    "A release description.", TestID -> "Dependencies: an explicit update lookup can supply a library description before installation"
]

VerificationTest[
    Module[{ran = False, owner = testDependencyOwner[{}, {<|"Name" -> "PacletTools", "Check" -> "Demo`Consumer`TestReadyQ"|>}]},
        Block[{Demo`Consumer`TestReadyQ = Function[Null, ran = True; True], dependencyRelease = Function[Null, Throw["network"]]},
            {PacletDependencyInformation[owner]["PacletTools"]["Status"], ran,
                PacletDependencyInformation[owner, "CheckReadiness" -> True]["PacletTools"]["Status"], ran}]],
    {"Installed", False, "Ready", True}, TestID -> "Dependencies: listing has no release lookup or runtime check"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {<|"Name" -> "PacletTools", "Check" -> None|>}]},
        Block[{dependencyReadiness = (Throw["Unexpected readiness test"] &)},
            With[{rows = PacletDependencyInformation[owner, "CheckReadiness" -> True, "Output" -> "Association"]},
                {rows["PacletTools"]["Status"], dependencyCheckAvailability[rows]["Readiness"],
                    MatchQ[InstallPacletDependencies[owner, "PacletTools"]["PacletTools"], _PacletObject]}]]],
    {"Installed", False, True}, TestID -> "Dependencies: an explicit None omits readiness checks in scripts and the dialog"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {<|"Name" -> "PacletTools", "Check" -> "Demo`Consumer`TestReadyQ"|>}]},
        Block[{Demo`Consumer`TestReadyQ = Function[Null, False], dependencyRelease = Function[Null, Throw["network"]]},
            {PacletDependencyInformation[owner, "CheckReadiness" -> True]["PacletTools"]["Status"],
                First[InstallPacletDependencies[owner, "PacletTools"]["PacletTools"]]}]],
    {"Needs setup", "DependencySetupRequired"}, TestID -> "Dependencies: installed runtime can require setup without reinstalling"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {<|"Name" -> "PacletTools", "Check" -> "Demo`Consumer`TestReadyQ"|>}],
            reason = Failure["Toolchain", <|"MessageTemplate" -> "The local toolchain could not start."|>]},
        Block[{Demo`Consumer`TestReadyQ = Function[Null, reason]},
            {PacletDependencyInformation[owner, "CheckReadiness" -> True]["PacletTools"]["ReadinessFailure"] === reason,
                InstallPacletDependencies[owner, All]["PacletTools"]["Cause"] === reason}]],
    {True, True}, TestID -> "Dependencies: retain the consumer's readiness failure"
]

VerificationTest[
    Module[{dir = CreateDirectory[], context = "Check" <> StringDelete[CreateUUID[], "-"] <> "`", owner, failure, messages},
        WithCleanup[
            Export[FileNameJoin[{dir, StringDrop[context, -1] <> ".m"}], StringJoin[
                "BeginPackage[", ToString[context, InputForm], "]\n",
                "Get[", ToString[FileNameJoin[{dir, "missing-driver"}], InputForm], "]\n",
                "readyQ[] := (Get[", ToString[FileNameJoin[{dir, "missing-config"}], InputForm], "]; ",
                "Failure[\"Toolchain\", <|\"MessageTemplate\" -> \"Setup failed.\", \"Code\" -> 7, \"Messages\" -> {\"Consumer detail.\"}|>])\n",
                "EndPackage[]\n"], "Text"];
            owner = <|"PrimaryContext" -> context|>;
            failure = Block[{$Path = Prepend[$Path, dir], $Packages = $Packages},
                dependencyReadiness[owner, <|"Check" -> context <> "readyQ"|>]];
            messages = failure["Messages"];
            {First[failure], failure["Code"], First[messages], Length[messages],
                StringContainsQ[messages[[2]], "missing-driver"], StringContainsQ[messages[[3]], "missing-config"]},
            Remove[Evaluate[context <> "*"]]; DeleteDirectory[dir, DeleteContents -> True]]],
    {"Toolchain", 7, "Consumer detail.", 3, True, True}, {Get::noopen, Get::noopen},
    TestID -> "Dependencies: preserve loading and hook messages alongside consumer failure data"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {}], entry = <|"Check" -> "Demo`Consumer`TestReadyQ"|>},
        {Block[{Demo`Consumer`TestReadyQ = (Throw[13] &)}, Catch[dependencyReadiness[owner, entry]]],
         Block[{Demo`Consumer`TestReadyQ = (Throw[17, "outside"] &)}, Catch[dependencyReadiness[owner, entry], "outside"]],
         Block[{Demo`Consumer`TestReadyQ = (Abort[] &)}, CheckAbort[dependencyReadiness[owner, entry], "aborted"]]}],
    {13, 17, "aborted"}, TestID -> "Dependencies: readiness callbacks can throw or abort through the public check"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {}], entry = <|"Check" -> "Demo`Consumer`TestReadyQ"|>},
        Function[{outcome, noisy}, Block[{Demo`Consumer`TestReadyQ = Function[Null, If[noisy, Message[Power::infy, "1/0"]]; outcome]},
            With[{failure = dependencyReadiness[owner, entry]},
                {First[failure], Length[failure["Messages"]], StringQ[dependencyReadinessDetail[failure]]}]]] @@@
            {{$Failed, False}, {$Failed, True}, {$Aborted, False}, {False, False}, {Failure["Bare"], True}}],
    {{"DependencyCheckFailed", 0, True}, {"DependencyCheckFailed", 1, True}, {"DependencyCheckFailed", 0, True},
        {"DependencyCheckFailed", 0, True}, {"DependencyCheckFailed", 1, True}},
    {Power::infy, Power::infy},
    TestID -> "Dependencies: a check that gives $Failed, or a Failure with no association, is a failed check that keeps its messages"
]

VerificationTest[
    Module[{reason = Failure["Setup", <|"MessageTemplate" -> "Setup failed.", "Messages" -> {"Missing runtime."}|>],
            row, report, boxes, text = "Setup failed.\nMissing runtime."},
        row = <|"Name" -> "Demo/Library", "Status" -> "Needs setup", "InstalledVersion" -> "1.0", "ReadinessFailure" -> reason|>;
        Table[report = PacletDependencyReport["Demo/Consumer", records]; boxes = ToBoxes[report, StandardForm];
            {dependencyRowNotice[row]["NoticeDetail"] === text, ! FreeQ[First[boxes], ToBoxes[text, StandardForm]],
                ToExpression[boxes, StandardForm] === report},
            {records, {<|"Demo/Library" -> row|>, <|"Demo/Library" -> row, "Demo/Other" -> <|"Name" -> "Demo/Other", "Status" -> "Installed"|>|>}}]],
    {{True, True, True}, {True, True, True}},
    TestID -> "Dependencies: single and multiple library reports and row tooltips display the captured readiness diagnostic"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {<|"Name" -> "PacletTools", "Check" -> "Demo`Consumer`TestReadyQ"|>}]},
        Block[{$dependencyRestarts = <|"PacletTools" -> True|>, $Context = Context[dependencyLocalStatus],
                $ContextPath = {"WolframInstitute`PacletPage`", "System`"}},
            Get[FileNameJoin[{$packageDirectory, "Dependencies.wl"}]];
            Block[{dependencyReadiness = (Throw["Unexpected readiness check"] &)}, Catch[
                PacletDependencyInformation[owner, "CheckReadiness" -> True]["PacletTools"]["Status"]]]]],
    "Restart required", TestID -> "Dependencies: reloading in the same evaluator preserves pending restart status"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {"Demo/Real"}], ran = 0},
        Block[{dependencyInstall = Function[Null, ran++]},
            {First[InstallPacletDependencies[owner, {"Demo/Real", "Demo/Typo"}]], ran}]],
    {"UnknownDependency", 0}, TestID -> "Dependencies: validate the entire selection before mutation"
]

VerificationTest[
    First[PacletDependencyInformation[testDependencyOwner[{"Demo/A"}, {"Demo/A"}]]],
    "DependencyManifest", TestID -> "Dependencies: duplicate names fail"
]

VerificationTest[
    AllTrue[{"*", "Demo/?", "Demo/Consumer", "", "../Lib"}, FailureQ[PacletDependencyInformation[testDependencyOwner[{}, {#}]]] &],
    True, TestID -> "Dependencies: reject patterns, paths and self-dependencies"
]

VerificationTest[
    AllTrue[{
        <|"Name" -> "Demo/Lib", "Version" -> "x"|>, <|"Name" -> "Demo/Lib", "Check" -> "System`Quit"|>,
        <|"Name" -> "Demo/Lib", "Resource" -> "ftp://example.org/lib"|>, <|"Name" -> "Demo/Lib", "Resorce" -> "typo"|>,
        <|"Name" -> "Demo/Lib", "SetupLinks" -> {"invalid"}|>,
        <|"Name" -> "Demo/Lib", "Resource" -> "https://example.org/lib", "Archive" -> "lib.paclet"|>},
        FailureQ[PacletDependencyInformation[testDependencyOwner[{}, {#}]]] &],
    True, TestID -> "Dependencies: validate versions, source, links and owner-scoped readiness"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {<|"Name" -> "Demo/Lib", "Platforms" -> {"OtherSystem"}|>}]},
        Block[{dependencyReadiness = Function[Null, Throw["probe"]], dependencyRelease = Function[Null, Throw["network"]]},
            {PacletDependencyInformation[owner, "CheckReadiness" -> True, "CheckUpdates" -> True]["Demo/Lib"]["Status"],
                First[InstallPacletDependencies[owner, All]["Demo/Lib"]], First[UpdatePacletDependencies[owner, All]["Demo/Lib"]]}]],
    {"Unsupported", "UnsupportedSystem", "UnsupportedSystem"}, TestID -> "Dependencies: unsupported platforms do no work"
]

VerificationTest[
    Module[{name = "PacletPageTest/Remote", resource = "https://www.wolframcloud.com/obj/demo/library", owner, lookups = 0, offline, online},
        owner = testDependencyOwner[{}, {<|"Name" -> name, "Resource" -> resource|>}];
        Block[{dependencyResourceMetadata = Function[url, lookups++;
                <|"Name" -> name, "ResourceType" -> "Paclet", "PacletVersion" -> "2.0", "ResourceLocation" -> CloudObject[resource]|>]},
            offline = Block[{$AllowInternet = False},
                {First[PacletDependencyInformation[owner, name, "CheckUpdates" -> True][name]["LatestVersion"]],
                 First[InstallPacletDependencies[owner, name][name]], lookups}];
            online = Block[{$AllowInternet = True, $AllowDataUpdates = False},
                PacletDependencyInformation[owner, name, "CheckUpdates" -> True][name]["LatestVersion"]]];
        {offline, online, lookups}],
    {{"DependencyInternetDisabled", "DependencyInternetDisabled", 0}, "2.0", 1},
    TestID -> "Dependencies: an offline cloud check or install makes no request and an explicit retry works with automatic updates disabled"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {"Demo/Indexed", "Demo/Unknown"}], calls = {}, rows},
        Block[{$AllowInternet = False, PacletFindRemote = Function[{name, refresh},
                AppendTo[calls, Last[refresh]];
                If[name === "Demo/Indexed", {PacletObject[<|"Name" -> name, "Version" -> "1.0"|>]}, {}]],
                dependencyInstallFile = Function[Null, Throw["download"]]},
            rows = PacletDependencyInformation[owner, "CheckUpdates" -> True];
            {rows["Demo/Indexed"]["LatestVersion"], First[rows["Demo/Unknown"]["LatestVersion"]],
                First[InstallPacletDependencies[owner, "Demo/Indexed"]["Demo/Indexed"]], calls}]],
    {"1.0", "DependencyInternetDisabled", "DependencyInternetDisabled", {False, False, False}},
    TestID -> "Dependencies: an offline site lookup reads cached releases without refreshing or downloading"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {"Demo/First", "Demo/Second"}]},
        Block[{dependencyRelease = Function[{p, entry}, If[entry["Name"] === "Demo/First", dependencyFailure["Network", "Offline."], <|"Version" -> "1.0"|>]]},
            With[{rows = PacletDependencyInformation[owner, "CheckUpdates" -> True]},
                {First[rows["Demo/First"]["LatestVersion"]], rows["Demo/Second"]["LatestVersion"]}]]],
    {"Network", "1.0"}, TestID -> "Dependencies: update lookup returns errors per library"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {"Demo/Optional"}],
            releases = PacletObject[<|"Name" -> "Demo/Optional", "Version" -> #|>] & /@ {"2.0.0-beta", "1.9.0", "1.8.0"}},
        Block[{$AllowInternet = True, PacletFindRemote = (releases &)},
            PacletDependencyInformation[owner, "CheckUpdates" -> True]["Demo/Optional"]["LatestVersion"]]],
    "1.9.0", TestID -> "Dependencies: a newer prerelease on a site does not hide its latest compatible numeric release"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {"Demo/Slow", "Demo/Fast"}], calls = {}, latest},
        Block[{$AllowInternet = True, $dependencyVersionTimeout = 0.15, PacletFindRemote = Function[{name, refresh},
                AppendTo[calls, {name, Last[refresh]}];
                If[name === "Demo/Slow", Pause[5]];
                {PacletObject[<|"Name" -> name, "Version" -> "2.0"|>]}]},
            latest = Lookup[Values[PacletDependencyInformation[owner, "CheckUpdates" -> True, "Output" -> "Association"]], "LatestVersion"]];
        {First[First[latest]], Last[latest], calls}],
    {"DependencyTimeout", "2.0", {{"Demo/Slow", True}, {"Demo/Fast", False}}},
    TestID -> "Dependencies: a slow version lookup times out and the next library reuses the site index"
]

VerificationTest[
    {dependencyCompatibleQ["1.10.0", "1.9+"], dependencyCompatibleQ["1.9", "1.10+"],
        dependencyCompatibleQ["1.0.0", "1.0"], dependencyCompatibleQ["0.10.2", "0.1.*"],
        dependencyCompatibleQ["0.1.25", "0.1.*"], dependencyCompatibleQ["0.1", "0.1.*"]},
    {True, False, True, False, True, True}, TestID -> "Dependencies: numeric versions, padding and component wildcards"
]

VerificationTest[
    Block[{dependencyCancelledQ = Function[Null, True], dependencyInstall = Function[Null, Throw["install"]]},
        First /@ Values[InstallPacletDependencies[testDependencyOwner[{}, {"Demo/A", "Demo/B"}], All]]],
    {"DependencyCancelled", "DependencyCancelled"}, TestID -> "Dependencies: batch cancellation skips remaining actions"
]

VerificationTest[
    Table[Module[{owner = testDependencyOwner[{}, {"Demo/Optional"}], cancelled = False, calls = 0, result},
        Block[{dependencyCancelledQ = (cancelled &), $dependencyProgress = (If[before && #1 === "Checking release", cancelled = True] &),
                dependencyRelease = Function[Null, calls++; cancelled = True; dependencyFailure["DependencyRelease", "Lookup interrupted."]]},
            result = InstallPacletDependencies[owner, All]["Demo/Optional"]];
        {First[result], calls}], {before, {True, False}}],
    {{"DependencyCancelled", 0}, {"DependencyCancelled", 1}},
    TestID -> "Dependencies: cancellation during release checks skips pending work and takes precedence over a failed lookup"
]

VerificationTest[
    Table[Module[{calls = 0, cancelled = False, result},
        Block[{dependencyCancelledQ = (cancelled &), $dependencyProgress = (If[#1 === stage, cancelled = True] &),
                URLDownloadSubmit = Function[Null, calls++; $Failed]},
            result = dependencyDownloadInstall["https://example.org/library.paclet", "Demo/Optional", "1.0"]];
        {First[result], calls}], {stage, {"Downloading", "Retrying"}}],
    {{"DependencyCancelled", 0}, {"DependencyCancelled", 1}},
    TestID -> "Dependencies: a cancelled transfer starts no new download or retry"
]

VerificationTest[
    Module[{names = {"Demo/First", "Demo/Second"}, owner, libraries, exercise},
        owner = testDependencyOwner[{}, <|"Name" -> #, "Check" -> "Demo`Consumer`readyQ"|> & /@ names];
        libraries = PacletObject[<|"Name" -> #, "Version" -> "1.0"|>] & /@ names;
        exercise[probe_] := Module[{stopped = False, calls = 0, rows},
            Block[{dependencyInstallations = (AssociationThread[names, List /@ libraries] &), dependencyCancelledQ = (stopped &),
                    dependencyReadiness = Function[Null, calls++; stopped = True; True],
                    dependencyRelease = Function[Null, calls++; stopped = True; <|"Version" -> "1.1"|>]},
                rows = PacletDependencyInformation[owner, "CheckReadiness" -> probe, "CheckUpdates" -> ! probe, "Output" -> "Association"]];
            {calls, rows[First[names]][If[probe, "Status", "LatestVersion"]], First[rows[Last[names]]]}];
        exercise /@ {True, False}],
    {{1, "Ready", "DependencyCancelled"}, {1, "1.1", "DependencyCancelled"}},
    TestID -> "Dependencies: cancelling either check completes the active library and skips the next probe or lookup"
]

VerificationTest[
    Module[{name = "Demo/Library", owner, local, installed = {}, rows, started = {}, published = {},
            probes = {}, releases = {}, check, empty, added, removed},
        owner = testDependencyOwner[{}, {<|"Name" -> name, "Check" -> "Demo`Consumer`readyQ"|>}];
        local = PacletObject[<|"Name" -> name, "Version" -> "1.0"|>];
        Block[{dependencyInstallations = Function[names, AssociationMap[installed &, names]],
                dependencyReadiness = Function[{p, entry}, AppendTo[probes, entry["Name"]]; True],
                dependencyRelease = Function[{p, entry}, AppendTo[releases, entry["Name"]]; <|"Version" -> "1.0"|>]},
            rows = dependencyDialogRows[PacletDependencyInformation[owner, "Output" -> "Association"]];
            check[probe_] := dependencyCheckLibraries[owner, rows, probe, (AppendTo[started, #] &),
                Function[{key, row}, AppendTo[published, key]; rows[key] = row]];
            empty = {Values[dependencyCheckAvailability[rows]], dependencyReadinessSummary[check[True]],
                dependencyUpdateSummary[check[False]], started, published, probes, releases};
            installed = {local};
            added = {dependencyUpdateSummary[check[False]], dependencyReadinessSummary[check[True]],
                Values[dependencyCheckAvailability[rows]], rows[name]["Status"]};
            rows[name] = Append[rows[name], "LatestVersion" -> dependencyFailure["DependencyInternetDisabled", "Offline."]];
            installed = {};
            removed = {dependencyReadinessSummary[check[True]], rows[name]["Status"], Values[dependencyCheckAvailability[rows]],
                KeyExistsQ[rows[name], "LatestVersion"]}];
        {empty, added, removed, started, published, probes, releases}],
    {{{False, False}, "No libraries to check", "No libraries to check", {}, {}, {}, {}},
     {"No upgrades", "1 passed", {True, True}, "Ready"}, {"No libraries to check", "Not installed", {False, False}, False},
     {"Library", "Library"}, {"Demo/Library", "Demo/Library", "Demo/Library"}, {"Demo/Library"}, {"Demo/Library"}},
    TestID -> "Dependencies: dialog checks refresh externally installed and removed libraries without probing an absent library"
]

VerificationTest[
    Module[{versions = <|"Demo/Added" -> None, "Demo/Removed" -> "1.0", "Demo/Changed" -> "1.0", "Demo/Stable" -> "1.0"|>,
            owner, rows, unchanged, changed, offline, snapshots = 0},
        owner = testDependencyOwner[{}, <|"Name" -> #, "Check" -> "Demo`Consumer`readyQ"|> & /@ Keys[versions]];
        Block[{dependencyInstallations = Function[names, snapshots++; AssociationMap[Function[name,
                    If[StringQ[versions[name]], {PacletObject[<|"Name" -> name, "Version" -> versions[name]|>]}, {}]], names]],
                dependencyReadiness = (Throw["unexpected readiness check"] &), dependencyRelease = (Throw["unexpected release lookup"] &)},
            rows = dependencyDialogRows[PacletDependencyInformation[owner, "Output" -> "Association"]];
            rows["Demo/Removed"] = Append[rows["Demo/Removed"], "LatestVersion" -> dependencyFailure["DependencyInternetDisabled", "Offline."]];
            rows["Demo/Changed"] = Join[rows["Demo/Changed"], <|"Status" -> "Needs setup", "LatestVersion" -> "2.0",
                "ReadinessFailure" -> dependencyFailure["Setup", "Setup required."]|>];
            rows["Demo/Stable"] = Join[rows["Demo/Stable"], <|"Status" -> "Ready", "LatestVersion" -> "1.1"|>];
            snapshots = 0;
            unchanged = dependencyLocalChanges[owner, rows];
            offline = dependencyOfflineQ[rows];
            versions["Demo/Added"] = "1.0"; versions["Demo/Removed"] = None; versions["Demo/Changed"] = "2.0";
            changed = dependencyLocalChanges[owner, rows]];
        {unchanged, snapshots, Keys[changed], Lookup[Values[changed], "Status"],
            dependencyRowActions[changed["Demo/Removed"]], KeyExistsQ[changed["Demo/Changed"], "ReadinessFailure"],
            changed["Demo/Changed"]["LatestVersion"], {offline, dependencyOfflineQ[Join[rows, changed]]}}],
    {<||>, 2, {"Demo/Added", "Demo/Removed", "Demo/Changed"}, {"Installed", "Not installed", "Installed"},
        {"Install"}, False, "2.0", {True, False}},
    TestID -> "Dependencies: focus reconciliation reads one local snapshot and publishes only changed installations"
]

VerificationTest[
    Module[{owner, rows, probes = {}, releases = {}, availability, readiness, updates, afterReadiness},
        owner = testDependencyOwner[{}, {
            <|"Name" -> "Demo/Ready", "Check" -> "Demo`Consumer`readyQ"|>, "Demo/NoProbe",
            <|"Name" -> "Demo/Missing", "Check" -> "Demo`Consumer`readyQ"|>,
            <|"Name" -> "Demo/Unsupported", "Check" -> "Demo`Consumer`readyQ", "Platforms" -> {"OtherSystem"}|>,
            <|"Name" -> "Demo/Mismatch", "Check" -> "Demo`Consumer`readyQ", "Version" -> "2.0+"|>}];
        Block[{dependencyInstallations = Function[names, AssociationMap[Function[name, If[name === "Demo/Missing", {},
                    {PacletObject[<|"Name" -> name, "Version" -> "1.0"|>]}]], names]],
                dependencyReadiness = Function[{p, entry}, AppendTo[probes, entry["Name"]]; True],
                dependencyRelease = Function[{p, entry}, AppendTo[releases, entry["Name"]];
                    Switch[entry["Name"], "Demo/NoProbe", dependencyFailure["Network", "Offline."],
                        "Demo/Mismatch", <|"Version" -> "2.0"|>, _, <|"Version" -> "1.0"|>]]},
            rows = dependencyDialogRows[PacletDependencyInformation[owner, "Output" -> "Association"]];
            availability = Values[dependencyCheckAvailability[KeyTake[rows, {#}]]] & /@ Keys[rows];
            readiness = dependencyCheckLibraries[owner, rows, True, (Null &), Function[{name, row}, rows[name] = row]];
            afterReadiness = {probes, releases};
            updates = dependencyCheckLibraries[owner, rows, False, (Null &), Function[{name, row}, rows[name] = row]]];
        {availability, Keys[readiness], dependencyReadinessSummary[readiness], afterReadiness,
            Keys[updates], dependencyUpdateSummary[updates], rows["Demo/Ready"]["Status"], probes, releases}],
    {{{True, True}, {False, True}, {False, False}, {False, False}, {False, True}},
     {"Demo/Ready"}, "1 passed", {{"Demo/Ready"}, {}},
     {"Demo/Ready", "Demo/NoProbe", "Demo/Mismatch"}, "1 upgrade, 1 unavailable", "Ready",
     {"Demo/Ready"}, {"Demo/Ready", "Demo/NoProbe", "Demo/Mismatch"}},
    TestID -> "Dependencies: dialog checks limit probes and release lookups to eligible libraries and summarize lookup failures"
]

VerificationTest[
    Module[{names = {"Demo/First", "Demo/Second"}, owner, exercise},
        owner = testDependencyOwner[{}, <|"Name" -> #, "Check" -> "Demo`Consumer`readyQ"|> & /@ names];
        exercise[probe_] := Module[{rows, checked, stopped = False, probes = {}, releases = {}, published = {}},
            Block[{dependencyInstallations = Function[keys, AssociationMap[{PacletObject[<|"Name" -> #, "Version" -> "1.0"|>]} &, keys]],
                    dependencyCancelledQ = (stopped &),
                    dependencyReadiness = Function[{p, entry}, AppendTo[probes, entry["Name"]]; True],
                    dependencyRelease = Function[{p, entry}, AppendTo[releases, entry["Name"]]; <|"Version" -> "1.1"|>]},
                rows = dependencyDialogRows[PacletDependencyInformation[owner, "Output" -> "Association"]];
                checked = dependencyCheckLibraries[owner, rows, probe, (Null &),
                    Function[{name, row}, AppendTo[published, name]; stopped = True]]];
            {Keys[checked], published, probes, releases}];
        exercise /@ {True, False}],
    {{{"Demo/First"}, {"Demo/First"}, {"Demo/First"}, {}}, {{"Demo/First"}, {"Demo/First"}, {}, {"Demo/First"}}},
    TestID -> "Dependencies: cancelling a dialog check keeps the completed row and skips every remaining probe or publisher"
]

VerificationTest[
    With[{boxes = ToBoxes[dependencyCheckControl["Check readiness", "Test readiness.", testStart[], testCancelled]]},
        Cases[boxes, ButtonBox[_, opts___] /; ! FreeQ[HoldComplete[opts], testStart | testCancelled] :>
            FirstCase[HoldComplete[opts], HoldPattern[Method -> m_] :> m, None, {1}], Infinity]],
    {"Queued", "Preemptive"}, TestID -> "Dependencies: a check uses the main evaluation queue and cancellation uses the preemptive link"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {}], original = NotebookObject["Original"], copy = NotebookObject["Copy"], start, stop, held},
        held[options_, key_] := FirstCase[options, HoldPattern[RuleDelayed[key, body_]] :> HoldComplete[body], None, {1}];
        Replace[dependencyLibraryList[owner, owner, <||>, start, stop], HoldPattern[DynamicModule[locals_, _, opts___]] :>
            Block[locals, With[{init = held[HoldComplete[opts], Initialization], dispose = held[HoldComplete[opts], Deinitialization]},
                Block[{EvaluationNotebook = (original &)}, ReleaseHold[init]];
                Block[{EvaluationNotebook = (copy &)}, ReleaseHold[init]; ReleaseHold[dispose]];
                Block[{dependencyRunCheck = Function[Null, "checked", HoldAll]},
                    {start[True, original], stop[original], FreeQ[DownValues[start], copy], FreeQ[DownValues[stop], copy]}]]]]],
    {"checked", True, True, True}, TestID -> "Dependencies: a rendered notebook copy has its own check callback lifetime"
]

VerificationTest[
    UsingFrontEnd[Module[{measure, empty, one, many},
        measure[n_] := Module[{names, owner, rows, nb},
            names = Table["Demo/Library" <> ToString[i], {i, n}];
            owner = testDependencyOwner[{}, names];
            rows = PacletDependencyInformation[owner, "Output" -> "Association"];
            nb = CreateDialog[dependencyDialogContent[owner, owner["Name"], rows], WindowSize -> {740, All},
                Visible -> False, TaggingRules -> dependencyDialogTags[rows]];
            WithCleanup[Last[AbsoluteCurrentValue[Cells[nb][[2]], CellSize]], NotebookClose[nb]]];
        {empty, one, many} = measure /@ {0, 1, 16};
        {0 < empty < one < many, 395 <= many <= 401}]],
    {True, True}, TestID -> "Dependencies: library rows occupy their first frame before the window is revealed"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {"Demo/Library"}], cells, settings, edges, color},
        {cells, settings} = Block[{dependencyDesktopQ = (True &), Notebooks = ({} &),
                EvaluationNotebook = (None &), CurrentValue = (Inherited &), $dependencyDialogs = <||>,
                CreateDialog = Function[Null, {First[{##}], Association[Rest[{##}]]}]},
            PacletDependencyDialog[owner]];
        edges = Cases[#, HoldPattern[CellFrame -> _], {1}] & /@ cells[[{1, 3}]];
        color = Cases[#, HoldPattern[CellFrameColor -> _], {1}] & /@ cells[[{1, 3}]];
        {edges, MatchQ[color, {{_Rule}, {_Rule}}] && First[color] === Last[color],
            ContainsAll[settings[WindowFrameElements], {"CloseBox", "MinimizeBox", "ZoomBox", "ResizeArea"}],
            ! KeyExistsQ[settings, NotebookDynamicExpression] &&
                ! FreeQ[cells[[2]], HoldPattern[DynamicWrapper[_, _, SynchronousUpdating -> False, TrackedSymbols :> {}]]]}],
    {{{CellFrame -> {{0, 0}, {1, 0}}}, {CellFrame -> {{0, 0}, {0, 1}}}}, True, True, True},
    TestID -> "Dependencies: the window has matching dividers, native controls and an asynchronous observer scoped to the displayed list"
]

VerificationTest[
    UsingFrontEnd[Module[{items, bounds, alignedQ},
        items = Graphics[{#, Rectangle[]}, ImageSize -> {48, 12}, ImagePadding -> 0, PlotRangePadding -> None] & /@ {Red, Blue};
        bounds[alignment_, width_] := Module[{image, pixels},
            image = Rasterize[Pane[dependencyFlow[items, 6, alignment], {width, Automatic}, Alignment -> alignment],
                "Image", ImageResolution -> 72];
            pixels = ImageData[ColorConvert[image, "RGB"], "Byte"];
            (* Image positions are {row, column}. Keep the two ranges for each colored rectangle. *)
            (MinMax /@ Transpose[Position[pixels, #, {2}]]) & /@ {{255, 0, 0}, {0, 0, 255}}];
        alignedQ[alignment_] := Module[{wide, narrow, edge, margin},
            {wide, narrow} = bounds[alignment, #] & /@ {160, 80};
            edge = If[alignment === Left, 1, 2];
            margin[box_, width_] := If[alignment === Left, box[[2, 1]], width - box[[2, 2]]];
            wide[[1, 2, 2]] < wide[[2, 2, 1]] && Abs[wide[[1, 1, 1]] - wide[[2, 1, 1]]] <= 1 &&
                narrow[[1, 1, 2]] < narrow[[2, 1, 1]] &&
                AllTrue[narrow, Abs[margin[#, 80] - margin[wide[[edge]], 160]] <= 1 &]];
        alignedQ /@ {Left, Right}]],
    {True, True}, TestID -> "Dependencies: both wrapped lines keep the same outer edge as the unwrapped controls"
]

VerificationTest[
    UsingFrontEnd[Module[{names, name, owner, rows, nb, cell, height, before, upgraded},
        names = Table["Demo/Library" <> IntegerString[i, 10, 3], {i, 64}];
        name = Last[names];
        owner = testDependencyOwner[{}, names];
        rows = AssociationMap[<|"Name" -> #, "InstalledVersion" -> "1.0", "Status" -> "Installed", "Required" -> False|> &, names];
        rows[name] = Join[rows[name], <|"LatestVersion" -> "1.1", "Description" -> "Previous release."|>];
        (* Use the list cell alone so its full height, including the last row, can be measured. *)
        nb = CreateDialog[{dependencyDialogContent[owner, owner["Name"], rows][[2]]}, WindowSize -> {740, 420},
            Visible -> False, TaggingRules -> Append[dependencyDialogTags[rows], "DependencyListSize" -> {Full, Automatic}]];
        WithCleanup[
            cell = First[Cells[nb]];
            height[] := Last[AbsoluteCurrentValue[cell, CellSize]];
            CurrentValue[nb, {TaggingRules, "DependencyCopied"}] = name;
            SetOptions[nb, WindowSize -> {460, 420}];
            before = height[];
            CurrentValue[nb, {TaggingRules, "DependencyCopied"}] = None;
            upgraded = Join[rows[name], <|"InstalledVersion" -> "1.1",
                "Description" -> StringRiffle[Table["Release note " <> ToString[i], {i, 12}], "\n"]|>];
            CurrentValue[nb, {TaggingRules, "DependencyRows", name}] = dependencyRowState[upgraded, dependencyNotice["Updated"]];
            (* the front end lays the list out again in its own time *)
            TimeConstrained[While[height[] <= before, Pause[0.1]], 5];
            {before > Length[names] CurrentValue[nb, "FontLineHeight"], height[] > before},
            NotebookClose[nb]]]],
    {True, True}, TestID -> "Dependencies: the 64-row list still formats the updated last row after copy and resize"
]

VerificationTest[
    UsingFrontEnd[Module[{dialog, size, plain, wrapped, tall, zoom, tiny, nb, long},
        dialog[footer_] := CreateDialog[{
            ExpressionCell["Heading", "DialogStyle", CellTags -> "DependencyDialogControls", CellMargins -> 0],
            ExpressionCell["List", "DialogStyle", CellMargins -> 0],
            ExpressionCell[footer, "DialogStyle", CellTags -> "DependencyDialogControls", CellMargins -> 0]},
            WindowSize -> {560, 310}, Visible -> False];
        nb = dialog["Footer"]; long = dialog[Column[{"First line", "Second line", "Third line"}]];
        WithCleanup[
            size[window_] := Last[Last[dependencyListSize[window]]];
            plain = size[nb]; wrapped = size[long];
            SetOptions[long, WindowSize -> {560, 820}]; tall = size[long];
            SetOptions[long, Magnification -> 2]; zoom = size[long];
            SetOptions[long, WindowSize -> {320, 40}];
            tiny = {dependencyListSize[long], CurrentValue[long, WindowElements]};
            SetOptions[long, WindowSize -> {740, All}];
            {0 < wrapped < plain < 310, tall > 400, 0 < zoom < tall, tiny,
                dependencyListSize[long], CurrentValue[long, WindowElements]},
            NotebookClose[long]; NotebookClose[nb]]]],
    {True, True, True, {{Full, Automatic}, {"VerticalScrollBar"}}, {Full, UpTo[400]}, {}},
    TestID -> "Dependencies: measured controls leave room for wrapped text, magnification and native scrolling"
]

VerificationTest[
    UsingFrontEnd[Module[{nb = CreateDocument[{}, Visible -> False], tag = {TaggingRules, "DependencyActivity", "Target"}, seen, cleaned, run},
        WithCleanup[
            SetAttributes[run, HoldRest];
            run[label_, expr_] := (seen = None; cleaned = False;
                CheckAbort[Catch[dependencyRunCheck[nb, label, seen = CurrentValue[nb, tag]; expr, cleaned = True]], Null];
                {seen, cleaned, CurrentValue[nb, tag], CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}]});
            {run["Check readiness", "1 passed"], run["Check readiness", Throw["interrupted"]], run["Check for updates", Abort[]]},
            NotebookClose[nb]]]],
    {{"Check readiness", True, None, "1 passed"}, {"Check readiness", True, None, ""}, {"Check for updates", True, None, ""}},
    TestID -> "Dependencies: the notebook clears its active check on completion, throw and abort"
]

VerificationTest[
    Module[{name = "PacletPageTest/Absent", stopped = False, result, row},
        Block[{dependencyInstall = Function[Null, stopped = True; If[dependencyCancelledQ[], dependencyCancelled[], $Failed]]},
            {result, row} = dependencyDialogAction[testDependencyOwner[{}, {name}], name, "Install", Function[Null, stopped], (Null &)]];
        {First[result], AssociationQ[row], row["Name"], row["Status"], dependencyRowActions[row]}],
    {"DependencyCancelled", True, "PacletPageTest/Absent", "Not installed", {"Install"}},
    TestID -> "Dependencies: cancelling a row action still reads its local record and restores Install"
]

VerificationTest[
    Table[Values[dependencyProgressState["Downloading", fraction]], {fraction, {None, -1/10, 3/8, 1, 2}}],
    {{"Downloading…", None, True}, {"Downloading 0%", 0, True}, {"Downloading 37%", 37, True},
        {"Downloading 100%", 100, True}, {"Downloading 100%", 100, True}},
    TestID -> "Dependencies: transfer progress shows a percentage only for a measured download"
]

VerificationTest[
    With[{stages = {"Preparing", "Checking release", "Retrying", "Installing", "Checking readiness", "Uninstalling", "Cancelling"}},
        Lookup[dependencyProgressState[#, None] & /@ stages, "Cancellable"]],
    {True, True, True, False, False, False, False},
    TestID -> "Dependencies: Cancel is available until the change to the installed library begins"
]

VerificationTest[
    Module[{name = "Demo/Upgrade", installed, owner, result, row},
        installed = PacletObject[<|"Name" -> name, "Version" -> "1.0"|>];
        owner = testDependencyOwner[{}, {name}];
        Block[{dependencyInstallations = Function[names, AssociationMap[Function[key, {installed}], names]]},
            {result, row} = dependencyDialogAction[owner, name, "Update", (True &), (Null &)]];
        {First[result], row["InstalledVersion"], dependencyRowActions[Append[row, "LatestVersion" -> "1.1"]]}],
    {"DependencyCancelled", "1.0", {"Uninstall", "Update"}},
    TestID -> "Dependencies: a cancelled upgrade preserves the installed version and a retry is available"
]

VerificationTest[
    Module[{installed = PacletObject[<|"Name" -> "Demo/Library", "Version" -> "1.1"|>],
            row = <|"InstalledVersion" -> "1.1", "Status" -> "Installed"|>, unconfirmed},
        unconfirmed = dependencyActionNotice["Install", $Failed, row];
        {Values[unconfirmed], dependencyActionNotice["Uninstall", $Failed, row] === unconfirmed,
            dependencyActionNotice["Install", installed, Missing["Unknown"]] === unconfirmed,
            dependencyActionNotice["Install", installed, <|"InstalledVersion" -> Missing["NotInstalled"]|>] === unconfirmed,
            Values /@ {dependencyActionNotice["Update", installed, row], dependencyActionNotice["Uninstall", Null, row],
                dependencyActionNotice["Install", installed, row], dependencyActionNotice["Install", dependencyCancelled[], row]}}],
    {{"Unconfirmed", "Reopen the dialog to check the installed version."}, True, True, True,
        {{"Updated", ""}, {"Removed", ""}, {"Installed", ""}, {"Cancelled", ""}}},
    TestID -> "Dependencies: success feedback requires a confirmed action and local record"
]

VerificationTest[
    Module[{installed = PacletObject[<|"Name" -> "Demo/Library", "Version" -> "1.1"|>],
            row = <|"InstalledVersion" -> "1.1", "Status" -> "Installed"|>,
            reason = dependencyFailure["Read", "Version 1.1. Retry."], setup, restart, fields},
        setup = dependencyFailure["DependencySetupRequired", "Configure the library."];
        restart = Append[row, "Status" -> "Restart required"];
        fields[state_] := Lookup[state, {"Notice", "NoticeDetail"}];
        {fields[dependencyRowState[row, dependencyActionNotice["Install", reason, row]]],
            fields[dependencyRowState[row, dependencyActionNotice["Install", installed, reason]]],
            fields[dependencyRowState[row, dependencyActionNotice["Install", setup, row]]],
            fields[dependencyRowState[Append[row, "ReadinessFailure" -> reason]]],
            fields[dependencyRowState[Append[row, "LatestVersion" -> reason]]],
            fields[dependencyRowState[restart]], Values[dependencyActionNotice["Update", installed, restart]],
            dependencyDialogMessage["Waiting..."]}],
    {{"Failed", "Version 1.1. Retry."}, {"Unconfirmed", "Version 1.1. Retry."}, {"Needs setup", "Configure the library."},
        {"Needs setup", "Version 1.1. Retry."}, {"Check failed", "Version 1.1. Retry."},
        {"Restart required", "Restart the kernel to use the changed library."},
        {"Restart required", "Restart the kernel to use the changed library."}, "Waiting..."},
    TestID -> "Dependencies: short row notices retain diagnostic and recovery details"
]

VerificationTest[
    {First[UpdatePacletDependencies[testDependencyOwner[{}, {"PacletPageTest/Absent"}], All]["PacletPageTest/Absent"]],
        UninstallPacletDependencies[testDependencyOwner[{}, {"PacletPageTest/Absent"}], All]["PacletPageTest/Absent"]},
    {"DependencyNotInstalled", Null}, TestID -> "Dependencies: updating or removing an absent dependency"
]

VerificationTest[
    First[PacletDependencyInformation["PacletPageTest/AbsentOwner"]],
    "DependencyOwner", TestID -> "Dependencies: missing owner is a local lookup failure"
]

VerificationTest[
    With[{owner = testDependencyOwner[{"PacletTools"}, {"PacletPageTest/Absent"}]},
        {UninstallPacletDependencies[owner, All], First[UninstallPacletDependencies[owner, "PacletTools"]["PacletTools"]],
            Keys[UninstallPacletDependencies[testDependencyOwner[{"PacletTools"}, {}], All]]}],
    {<|"PacletPageTest/Absent" -> Null|>, "RequiredDependency", {}},
    TestID -> "Dependencies: bulk removal selects optional libraries and explicit required removals are protected"
]

VerificationTest[
    With[{manager = "WolframInstitute/PacletPage"},
        With[{owner = testDependencyOwner[{}, {manager}]},
            {UninstallPacletDependencies[owner, All], First[UninstallPacletDependencies[owner, manager][manager]],
                dependencyRowActions[PacletDependencyInformation[owner][manager]]}]],
    {<||>, "DependencyManagerInUse", {}}, TestID -> "Dependencies: even an optional manager cannot uninstall itself"
]

VerificationTest[
    dependencyRowActions /@ {<|"Required" -> True, "InstalledVersion" -> "1.0"|>,
        <|"Required" -> False, "InstalledVersion" -> "1.0"|>, <|"Required" -> True, "InstalledVersion" -> Missing[]|>},
    {{}, {"Uninstall"}, {"Install"}}, TestID -> "Dependencies: dialog actions protect required installations"
]

VerificationTest[
    With[{row = <|"Name" -> "Demo/Library", "InstalledVersion" -> "1.9", "Status" -> "Installed", "Required" -> False|>},
        dependencyRowActions /@ {row, Append[row, "LatestVersion" -> "1.9"], Append[row, "LatestVersion" -> "1.8"],
            Append[row, "LatestVersion" -> "1.10"], Join[row, <|"LatestVersion" -> "1.10", "Required" -> True|>],
            Join[row, <|"LatestVersion" -> "1.10", "Status" -> "Unsupported"|>]}],
    {{"Uninstall"}, {"Uninstall"}, {"Uninstall"}, {"Uninstall", "Update"}, {"Update"}, {"Uninstall"}},
    TestID -> "Dependencies: upgrade appears only for a confirmed newer supported release"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {"First/Shared", "Demo/Other", "Second/Shared"}]},
        With[{rows = dependencyDialogRows[PacletDependencyInformation[owner, "Output" -> "Association"]]},
            {Keys[rows], Lookup[Values[rows], "Label"], dependencyDialogHeading[rows]}]],
    {{"First/Shared", "Demo/Other", "Second/Shared"}, {"First/Shared", "Other", "Second/Shared"}, "Optional libraries"},
    TestID -> "Dependencies: dialog keeps declaration order and distinguishes matching short names"
]

VerificationTest[
    With[{rows = AssociationMap[<|"Status" -> #|> &, {"Ready", "Needs setup", "Not installed", "Version mismatch", "Installed"}]},
        dependencyReadinessSummary /@ {<||>, KeyTake[rows, {"Ready"}], KeyDrop[rows, {"Ready", "Needs setup"}], rows}],
    {"No libraries to check", "1 passed", "3 skipped", "1 passed, 1 failed, 3 skipped"},
    TestID -> "Dependencies: the readiness summary counts actual probes and distinguishes skipped libraries"
]

VerificationTest[
    Module[{root = CreateDirectory[], source, name = "Test" <> StringDelete[CreateUUID[], "-"]},
        WithCleanup[
            source = CreateDirectory[FileNameJoin[{root, "RepositoryBackup", name}]];
            Put[PacletObject[<|"Name" -> name, "Version" -> "1.0"|>], FileNameJoin[{source, "PacletInfo.wl"}]];
            PacletDirectoryLoad[source];
            Block[{$UserBasePacletsDirectory = root, PacletUninstall = (Throw["Unexpected uninstall"] &)},
                Catch[{First[UninstallPacletDependencies[testDependencyOwner[{}, {name}], All][name]], DirectoryQ[source]}]],
            PacletDirectoryUnload[source]; DeleteDirectory[root, DeleteContents -> True]]],
    {"ExternalInstallation", True}, TestID -> "Dependencies: protect development checkouts beside the user repository"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"], source, copies},
        source = CreateDirectory[FileNameJoin[{root, "library"}]];
        copies[] := PacletFind[name, <|"EvaluationEnvironment" -> All|>];
        WithCleanup[
            Put[PacletObject[<|"Name" -> name, "Version" -> "1.0",
                "EvaluationEnvironment" -> If[$EvaluationEnvironment === "Script", "Cloud", "Script"]|>], FileNameJoin[{source, "PacletInfo.wl"}]];
            Quiet[PacletInstall[CreatePacletArchive[source, root]]];
            {PacletFind[name], Length[copies[]], UninstallPacletDependencies[testDependencyOwner[{}, {name}], name][name], copies[]},
            Scan[PacletUninstall, copies[]]; DeleteDirectory[root, DeleteContents -> True]]],
    {{}, 1, Null, {}}, TestID -> "Dependencies: explicit removal also removes user copies installed for another evaluation environment"
]

(* A literal counter in unused release metadata must stay unevaluated. *)
VerificationTest[
    Module[{source},
        source = "ResourceObject[<|\"Name\" -> \"Demo/A\", \"Version\" -> \"2.0\", \"ResourceType\" -> \"Paclet\", " <>
            "\"SystemID\" -> {\"MacOSX-ARM64\", \"Linux-x86-64\"}, \"Extra\" -> (Global`releaseSideEffect = True), " <>
            "\"PacletFile\" -> CloudObject[\"https://example.org/a.paclet\"]|>]";
        Block[{Global`releaseSideEffect = False}, {dependencyArchiveURL[source, "Demo/A", "2.0", "Linux-x86-64"],
            FailureQ[dependencyArchiveURL[source, "Demo/A", "1.0", "Linux-x86-64"]],
            FailureQ[dependencyArchiveURL[source, "Demo/A", "2.0", "Windows-x86-64"]], Global`releaseSideEffect}]],
    {"https://example.org/a.paclet", True, True, False}, TestID -> "Dependencies: inspect literal release fields without evaluating publisher code"
]

VerificationTest[
    Module[{record, table, source},
        record = "<|\"Name\" -> \"Demo/A\", \"Version\" -> \"2.0\", \"ResourceType\" -> \"Paclet\", " <>
            "\"SystemID\" -> \"Linux-x86-64\", \"PacletFile\" -> \"https://example.org/linux.paclet\"|>";
        table = "<|\"Linux-x86-64\" -> " <> record <> "|>";
        source = "If[KeyExistsQ[" <> table <> ", $SystemID], ResourceObject[" <> table <> "[$SystemID]], $Failed]";
        {dependencyArchiveURL[source, "Demo/A", "2.0", "Linux-x86-64"],
            FailureQ[dependencyArchiveURL[source, "Demo/A", "2.0", "MacOSX-ARM64"]]}],
    {"https://example.org/linux.paclet", True}, TestID -> "Dependencies: select the published archive for this platform"
]

VerificationTest[
    With[{resource = "https://example.org/obj/team/DeployedResources/Paclet/Demo/A"},
        dependencyPublisherURLQ[resource, #] & /@ {"https://example.org/obj/team/a", "https://example.org/obj/team2/a",
            "http://example.org/obj/team/a", "https://elsewhere.org/obj/team/a", "https://example.org/obj/team/../other/a"}],
    {True, False, False, False, False}, TestID -> "Dependencies: release files belong to the declared publisher"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {<|"Name" -> "Demo/A", "Archive" -> "../sibling.paclet"|>}]},
        First[InstallPacletDependencies[owner, All]["Demo/A"]]],
    "DependencyArchive", TestID -> "Dependencies: a relative archive belongs to the consumer"
]

VerificationTest[
    Block[{$FrontEnd = Null}, {First[PacletDependencyDialog["Demo/Consumer"]], PacletDependencyDialog["Demo/Consumer", "Notify"]}],
    {"NotebookRequired", Null}, TestID -> "Dependencies: scripts can load consumers without opening setup"
]

VerificationTest[
    dependencyCommand["Demo/Consumer", "Update", "Demo/A"],
    "UpdatePacletDependencies[\"Demo/Consumer\", \"Demo/A\"]", TestID -> "Dependencies: copy the same public command used by scripts"
]

VerificationTest[
    Block[{$ContextPath = {"System`", "Global`"}},
        ToExpression[dependencyCommand["Demo/Consumer", "Install", "Demo/A"], InputForm, HoldComplete]],
    HoldComplete[InstallPacletDependencies["Demo/Consumer", "Demo/A"]],
    TestID -> "Dependencies: a copied command also works when only the consumer is on the context path"
]

VerificationTest[
    UsingFrontEnd[With[{owner = testDependencyOwner[{"Demo/Required"}, {"Demo/Optional"}]},
        Block[{dependencyDesktopQ = (True &), Notebooks = ({} &), $dependencyDialogs = <||>,
                dependencyRelease = (Throw["Unexpected release lookup"] &), dependencyReadiness = (Throw["Unexpected readiness check"] &),
                PacletSiteUpdate = (Throw["Unexpected site update"] &), PacletFindRemote = (Throw["Unexpected remote lookup"] &),
                URLRead = (Throw["Unexpected request"] &), URLSubmit = (Throw["Unexpected request"] &),
                dependencyDialogContent = Function[{paclet, command, rows}, Keys[rows]], CreateDialog = Function[Null, First[{##}]]},
            Catch[PacletDependencyDialog[owner]]]]],
    {"Demo/Required", "Demo/Optional"},
    TestID -> "Dependencies: opening the dialog lists every dependency, reads local metadata and leaves readiness and updates to its buttons"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"], source, owner,
            installed, archives, offered, declare, selectVersion},
        source = CreateDirectory[FileNameJoin[{root, "library"}]];
        owner = CreateDirectory[FileNameJoin[{root, "consumer"}]];
        offered = FileNameJoin[{owner, "library.paclet"}];
        declare[record_] := Put[PacletObject[<|"Name" -> "TestConsumer", "Version" -> "1.0",
            "Dependencies" -> {Append[record, "Optional" -> True]}|>], FileNameJoin[{owner, "PacletInfo.wl"}]];
        selectVersion[version_] := CopyFile[archives[version], offered, OverwriteTarget -> True];
        WithCleanup[
            archives = AssociationMap[Function[version,
                Put[PacletObject[<|"Name" -> name, "Version" -> version|>], FileNameJoin[{source, "PacletInfo.wl"}]];
                CreatePacletArchive[source, root]], {"1.9.0", "1.10.0"}];
            declare[<|"Name" -> name, "Archive" -> "library.paclet", "Version" -> "1.8+"|>];
            selectVersion["1.9.0"];
            installed = InstallPacletDependencies[File[owner], All][name];
            DeleteFile[offered];
            With[{repeat = InstallPacletDependencies[owner, name][name]},
                selectVersion["1.10.0"];
                With[{offeredVersion = Block[{$AllowInternet = False}, PacletDependencyInformation[owner, name, "CheckUpdates" -> True][name]["LatestVersion"]],
                      updated = UpdatePacletDependencies[owner, name][name]},
                    selectVersion["1.9.0"];
                    With[{retained = UpdatePacletDependencies[owner, name][name]},
                        Export[offered, "Invalid archive", "Text"];
                        {installed["Version"], repeat === installed, offeredVersion, updated["Version"], retained["Version"],
                         FailureQ[UpdatePacletDependencies[owner, name][name]], First[PacletFind[name]]["Version"],
                         UninstallPacletDependencies[owner, name][name], PacletFind[name]}]]],
            Scan[PacletUninstall, PacletFind[name]]; DeleteDirectory[root, DeleteContents -> True]]],
    {"1.9.0", True, "1.10.0", "1.10.0", "1.10.0", True, "1.10.0", Null, {}},
    (* the failed update reads an archive that is not a paclet, which the paclet manager reports *)
    {PacletManager`CreatePaclet::badppi},
    TestID -> "Dependencies: real archive install, idempotence, numeric update, no downgrade, failed update and removal"
]

VerificationTest[
    Module[{root = CreateDirectory[], source, name = "Test" <> StringDelete[CreateUUID[], "-"], owner, archive, record, result},
        source = CreateDirectory[FileNameJoin[{root, "library"}]];
        WithCleanup[
            Table[
                Put[PacletObject[Join[<|"Name" -> name, "Version" -> "1.0"|>, constraint]], FileNameJoin[{source, "PacletInfo.wl"}]];
                archive = CreatePacletArchive[source, root];
                record = <|"Name" -> name, "Archive" -> FileNameTake[archive]|>;
                owner = PacletObject[<|"Name" -> "TestConsumer", "Version" -> "1.0", "Location" -> root, "Dependencies" -> {Append[record, "Optional" -> True]}|>];
                result = Block[{PacletInstall = (Throw["Unexpected installation"] &)}, Catch[{
                    PacletDependencyInformation[owner, "CheckUpdates" -> True][name]["LatestVersion"],
                    InstallPacletDependencies[owner, name][name], dependencyInstallFile[File[archive], name, "1.0"]}]];
                DeleteFile[archive];
                MatchQ[result, {Repeated[Failure["DependencyArchive", _], {3}]}],
                {constraint, {<|"SystemID" -> If[$SystemID === "Linux-x86-64", "MacOSX-ARM64", "Linux-x86-64"]|>,
                    <|"WolframVersion" -> "99.0+"|>}}],
            DeleteDirectory[root, DeleteContents -> True]]],
    {True, True}, TestID -> "Dependencies: incompatible archives are excluded from release checks and rejected before installation"
]

VerificationTest[
    With[{owner = testDependencyOwner[{"Demo/Required"}, {"Demo/Optional"}], optional = testDependencyOwner[{}, {"Demo/Optional"}]},
        Block[{dependencyDesktopQ = (True &), Notebooks = ({} &), $dependencyDialogs = <||>,
                EvaluationNotebook = (None &), CurrentValue = (Inherited &), dependencyDialogContent = (Null &),
                CreateDialog = Function[Null, With[{options = Association[Rest[{##}]]},
                    {options[WindowTitle], options[TaggingRules]["DependencyText"], Keys[options[TaggingRules]["DependencyRows"]]}]]},
            {PacletDependencyDialog[owner, "Title" -> "Proof tools", "Description" -> "Check a proof."],
                PacletDependencyDialog[optional, "Description" -> ""], PacletDependencyDialog[owner],
                First[PacletDependencyDialog[owner, "Title" -> " "]], First[PacletDependencyDialog[owner, "Description" -> 1]]}]],
    {{"Consumer: Proof tools", <|"Title" -> "Proof tools", "Description" -> "Check a proof."|>, {"Demo/Required", "Demo/Optional"}},
        {"Consumer: Optional libraries", <|"Title" -> "Optional libraries", "Description" -> ""|>, {"Demo/Optional"}},
        {"Consumer: Library dependencies", <|"Title" -> "Library dependencies", "Description" -> "Install, upgrade, or remove libraries as needed."|>,
            {"Demo/Required", "Demo/Optional"}}, "DependencyDialogTitle", "DependencyDialogDescription"},
    TestID -> "Dependencies: title and description options initialize the dialog and its native window title, Optional libraries when nothing is required"
]

VerificationTest[
    UsingFrontEnd[Module[{owner = testDependencyOwner[{}, {}], nb, changed, restored},
        nb = CreateDocument[{}, Visible -> False, TaggingRules -> Join[dependencyDialogTags[<||>],
            <|"DependencyInternetDisabled" -> True, "DependencyCheckFeedback" -> "2 passed"|>]];
        WithCleanup[
            Block[{dependencyDesktopQ = (True &), $dependencyDialogs = <|dependencyCommandOwner[owner, owner] -> nb|>,
                    SetSelectedNotebook = (Null &), PacletDependencyInformation = (Throw["Unexpected second snapshot"] &)},
                Catch[
                    changed = PacletDependencyDialog[owner, "Title" -> "Proof tools", "Description" -> ""];
                    With[{title = CurrentValue[nb, WindowTitle], text = CurrentValue[nb, {TaggingRules, "DependencyText", "Description"}]},
                        restored = PacletDependencyDialog[owner];
                        {changed === nb === restored, title, text, CurrentValue[nb, WindowTitle],
                            CurrentValue[nb, {TaggingRules, "DependencyText", "Description"}],
                            CurrentValue[nb, {TaggingRules, "DependencyInternetDisabled"}],
                            CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}]}]]],
            NotebookClose[nb]]]],
    {True, "Consumer: Proof tools", "", "Consumer: Optional libraries", "Install, upgrade, or remove libraries as needed.", True, "2 passed"},
    TestID -> "Dependencies: reopening changes or restores the text of the same window and preserves its check state"
]

VerificationTest[
    With[{owner = testDependencyOwner[{}, {}], source = NotebookObject["Source"]},
        Block[{dependencyDesktopQ = (True &), EvaluationNotebook = (source &), Notebooks = ({source} &), $dependencyDialogs = <||>,
                CurrentValue = Function[{nb, key}, Switch[key, LightDark, "Dark", System`NotebookTheme, "Solarized", _, Automatic]],
                dependencyDialogContent = (Null &), CreateDialog = Function[Null, Association[Rest[{##}]]]},
            With[{options = PacletDependencyDialog[owner]},
                {options[Visible], options[LightDark], Lookup[options, System`NotebookTheme, None],
                    options[TaggingRules]["DependencyAppearanceSource"] === source, options[Background]} ===
                {False, "Dark", If[$dependencyNamedThemes, "Solarized", None], True,
                    If[$dependencyNamedThemes, ThemeColor["Background"], LightDarkSwitched[GrayLevel[.925], GrayLevel[.145]]]}]]],
    True, TestID -> "Dependencies: appearance is set on the dialog's first hidden frame"
]

VerificationTest[
    UsingFrontEnd[Block[{$dependencyDialogs = <||>}, Module[
        {owners = {testDependencyOwner[{}, {"Demo/Optional"}], testDependencyOwner[{}, {"Demo/Other"}]}, source, open, first, other, reloaded, recovered, feedback, kept, events},
        source = CreateDocument[{}, Visible -> False];
        open[owner_] := Block[{dependencyDesktopQ = (True &), EvaluationNotebook = (source &), SetSelectedNotebook = (Null &)},
            PacletDependencyDialog[owner]];
        feedback = {TaggingRules, "DependencyCheckFeedback"};
        WithCleanup[
            first = open[First[owners]]; other = open[Last[owners]];
            SetOptions[other, WindowSize -> {440, 300}];
            CurrentValue[other, feedback] = "1 passed";
            Block[{$Context = Context[dependencyDialogTags], $ContextPath = {"WolframInstitute`PacletPage`", "System`"}},
                Get[FileNameJoin[{$packageDirectory, "DependencyDialog.wl"}]]];
            reloaded = open[Last[owners]]; kept = CurrentValue[reloaded, feedback];
            events = Keys[Association[NotebookEventActions /. Options[reloaded, NotebookEventActions]]];
            $dependencyDialogs = <||>;
            recovered = open /@ owners;
            {first =!= other, reloaded === other, kept, recovered === {first, other},
                CurrentValue[Last[recovered], feedback], Round[CurrentValue[Last[recovered], WindowSize]],
                ContainsAll[events, {"ReturnKeyDown", {"MenuCommand", "HandleShiftReturn"}}]},
            Scan[NotebookClose, Intersection[Cases[Flatten[{source, first, other, reloaded, recovered}], _NotebookObject], Notebooks[]]]]
    ]]],
    {True, True, "1 passed", True, "", {440, 300}, True},
    TestID -> "Dependencies: a package reload preserves open dialogs and a fresh evaluator rebuilds each consumer's dialog in place"
]

VerificationTest[
    With[{source = NotebookObject["Source"]},
        Block[{$dependencyNamedThemes = False, Notebooks = ({source} &),
                CurrentValue = Function[{nb, key}, If[key === LightDark, Automatic, Throw["Unknown notebook option"]]]},
            Catch[{dependencyAppearanceOptions[source], dependencyDialogBackground[{}],
                Block[{Notebooks = ({} &)}, dependencyAppearanceOptions[source]]}]]],
    {{LightDark -> Automatic}, LightDarkSwitched[GrayLevel[.925], GrayLevel[.145]], {}},
    TestID -> "Dependencies: light and dark settings also work without named notebook themes"
]

VerificationTest[
    UsingFrontEnd[Module[{owner = testDependencyOwner[{}, {}], first, second, dialog, openFrom, state, result},
        first = CreateDocument[{}, Visible -> False, LightDark -> "Dark",
            Sequence @@ If[$dependencyNamedThemes, {System`NotebookTheme -> "Solarized"}, {}]];
        second = CreateDocument[{}, Visible -> False, LightDark -> "Light",
            Sequence @@ If[$dependencyNamedThemes, {System`NotebookTheme -> "Monokai"}, {}]];
        dialog = CreateDocument[{}, Visible -> False, TaggingRules -> dependencyDialogTags[<||>]];
        WithCleanup[
            state[] := {CurrentValue[dialog, LightDark], If[$dependencyNamedThemes, CurrentValue[dialog, System`NotebookTheme], None]};
            openFrom[source_] := Block[{EvaluationNotebook = (source &), SetSelectedNotebook = (Null &)}, PacletDependencyDialog[owner]];
            result = Block[{dependencyDesktopQ = (True &), $dependencyDialogs = <|dependencyCommandOwner[owner, owner] -> dialog|>},
                openFrom[first];
                With[{dark = state[]}, openFrom[second];
                    With[{light = state[]}, SetOptions[second, LightDark -> Automatic]; dependencySyncAppearance[dialog];
                        NotebookClose[second]; dependencySyncAppearance[dialog];
                        {dark, light, state[], CurrentValue[first, LightDark]}]]];
            result === {{"Dark", If[$dependencyNamedThemes, "Solarized", None]}, {"Light", If[$dependencyNamedThemes, "Monokai", None]},
                {Automatic, If[$dependencyNamedThemes, "Monokai", None]}, "Dark"},
            Scan[If[MemberQ[Notebooks[], #], NotebookClose[#]] &, {dialog, second, first}]]]],
    True, TestID -> "Dependencies: appearance follows the most recent caller and survives closing its notebook"
]

VerificationTest[
    {
        dependencyGitRepository /@ {"https://github.com/sw1sh/WolframParser", "https://github.com/sw1sh/WolframParser.git", "https://github.com/a/b/",
            "https://gitlab.com/a/b", "http://github.com/a/b", "https://github.com/a"},
        dependencyCommitQ /@ {"5e05149", "5e05149ead1c292ebd39d02eee85f1c0b8059f55", "main", "5E05149", "5e051"},
        dependencyGitPathQ /@ {"", "Parser", "src/Parser", "../Parser", "/Parser", "C:Parser"}
    },
    {
        {{"sw1sh", "WolframParser"}, {"sw1sh", "WolframParser"}, {"a", "b"}, None, None, None},
        {True, True, False, True, False},
        {True, True, True, False, False, False}
    },
    TestID -> "Dependencies: a git source is a GitHub repository, a commit of 7 to 40 hexadecimal digits and a relative path"
]

VerificationTest[
    With[{git = <|"Name" -> "Demo/Git", "Git" -> "https://github.com/demo/git", "Commit" -> "abcdef0"|>},
        {
            AssociationQ[dependencyManifest[testDependencyOwner[{}, {git, Append[git, {"Name" -> "Demo/Sub", "Path" -> "Sub"}]}]]],
            FailureQ[dependencyManifest[testDependencyOwner[{}, {Append[git, "Resource" -> "https://example.org/r"]}]]],
            FailureQ[dependencyManifest[testDependencyOwner[{}, {Append[git, "Archive" -> "Resources/A.paclet"]}]]],
            FailureQ[dependencyManifest[testDependencyOwner[{}, {<|"Name" -> "Demo/Git", "Commit" -> "abcdef0"|>}]]],
            FailureQ[dependencyManifest[testDependencyOwner[{}, {KeyDrop[git, "Commit"]}]]],
            FailureQ[dependencyManifest[testDependencyOwner[{}, {Append[git, "Path" -> "../x"]}]]]
        }
    ],
    {True, True, True, True, True, True},
    TestID -> "Dependencies: a git declaration has a commit, no other source, and a commit or a path only with a repository"
]

VerificationTest[
    dependencyPacletInfoFields /@ {
        "PacletObject[<|\"Name\" -> \"A/B\", \"Version\" -> \"2.1\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> Print[\"evaluated\"]}}|>]",
        "(* ::Package:: *)\n\nPacletObject[<|\n    \"Name\" -> \"A/B\",\n    \"Version\" -> \"2.1\"\n|>]\n",
        "(* old manifest *)\nPaclet[Name -> \"A/B\", Version -> \"2.1\", MathematicaVersion -> \"12+\", SystemID -> {\"Linux-x86-64\"}]",
        "PacletObject[<|\"Name\" -> \"A/B\", \"SystemID\" -> StringJoin[\"Win\", \"dows\"], \"Extensions\" -> {{\"Kernel\", \"SystemID\" -> All}}|>]"
    },
    {<|"Name" -> "A/B", "Version" -> "2.1"|>, <|"Name" -> "A/B", "Version" -> "2.1"|>,
        <|"Name" -> "A/B", "Version" -> "2.1", "MathematicaVersion" -> "12+", "SystemID" -> {"Linux-x86-64"}|>,
        <|"Name" -> "A/B", "SystemID" -> Missing["Nonliteral"]|>},
    TestID -> "Dependencies: current and legacy Git manifests supply literal top-level compatibility fields"
]

VerificationTest[
    Module[{entry = <|"Name" -> "Demo/FromGit", "Git" -> "https://github.com/example/tools", "Commit" -> "ABCDEF0", "Path" -> "Math Tools"|>,
            urls = {}, release, failed},
        Block[{dependencyGitText = Function[url, AppendTo[urls, url];
                If[StringEndsQ[url, ".wl"], Missing["NotFound"], "Paclet[Name -> \"Demo/FromGit\", Version -> \"1.0\"]"]]},
            release = dependencyGitRelease[entry]];
        Block[{dependencyGitText = Function[url, AppendTo[urls, url]; $Failed]}, failed = dependencyGitRelease[entry]];
        {release["Version"], release["GitManifest"], First[failed], urls}
    ],
    {"1.0", "PacletInfo.m", "DependencyRelease", {
        "https://raw.githubusercontent.com/example/tools/abcdef0/Math%20Tools/PacletInfo.wl",
        "https://raw.githubusercontent.com/example/tools/abcdef0/Math%20Tools/PacletInfo.m",
        "https://raw.githubusercontent.com/example/tools/abcdef0/Math%20Tools/PacletInfo.wl"}},
    TestID -> "Dependencies: Git uses PacletInfo.m after a missing PacletInfo.wl and preserves transport errors"
]

VerificationTest[
    Block[{$ProgressReporting = True, URLRead = Function[Null, <|"StatusCode" -> 200, "Body" -> ToString[$ProgressReporting]|>]},
        dependencyGitText["https://raw.githubusercontent.com/example/tools/abcdef0/PacletInfo.wl"]],
    "False", TestID -> "Dependencies: a Git manifest request reports no progress, which a script's kernel would print from then on"
]

VerificationTest[
    Module[{root = CreateDirectory[], commit = StringRepeat["c", 40], name = "Demo/Extracted", info, top, zip, owner, seen},
        info = "PacletObject[<|\"Name\" -> \"" <> name <> "\", \"Version\" -> \"1.0\"|>]";
        top = FileNameJoin[{root, "repo-" <> commit}];
        Scan[Function[file, With[{path = FileNameJoin[Prepend[file, top]]},
                CreateDirectory[DirectoryName[path], CreateIntermediateDirectories -> True];
                Export[path, If[Last[file] === "PacletInfo.wl", info, "1"], "Text"]]],
            {{"PacletInfo.wl"}, {"Kernel", "Root.wl"}, {"Sub", "PacletInfo.wl"}, {"Sub", "Kernel", "Library.wl"}, {"Docs", "Page.md"}}];
        zip = CreateArchive[top, FileNameJoin[{root, "tree.zip"}]];
        owner[path_] := testDependencyOwner[{}, {Join[<|"Name" -> name, "Git" -> "https://github.com/demo/repo", "Commit" -> commit|>, path]}];
        WithCleanup[
            Block[{$AllowInternet = True, dependencyGitText = (info &),
                    dependencyDownloaded = Function[{url, file}, CopyFile[zip, file]; File[file]],
                    dependencyInstallFile = Function[{archive, library, version, git},
                        seen = Sort[FileNameTake /@ Select[FileNames["*", FileNameJoin[{DirectoryName[First[archive]], "tree"}], Infinity], ! DirectoryQ[#] &]];
                        {git["Path"], seen}]},
                {InstallPacletDependencies[owner[<|"Path" -> "Sub"|>], name][name], InstallPacletDependencies[owner[<||>], name][name],
                    First[InstallPacletDependencies[owner[<|"Path" -> "Missing"|>], name][name]]}],
            DeleteDirectory[root, DeleteContents -> True]]],
    {{"Sub", Sort[{$dependencyGitReceipt, "Library.wl", "PacletInfo.wl"}]},
        {"", Sort[{$dependencyGitReceipt, "Library.wl", "PacletInfo.wl", "PacletInfo.wl", "Page.md", "Root.wl"}]}, "DependencyArchive"},
    TestID -> "Dependencies: only the paclet's directory is extracted from a commit's archive, and all of it for a paclet at the root"
]

VerificationTest[
    Module[{name = "Demo/GitRuntime", owner, manifest, downloads = 0, outcomes},
        owner = testDependencyOwner[{}, {<|"Name" -> name, "Git" -> "https://github.com/example/tools", "Commit" -> "abcdef0"|>}];
        Block[{$AllowInternet = True, dependencyGitText = Function[url, manifest], dependencyDownloaded = Function[Null, downloads++; $Failed]},
            outcomes = Table[
                manifest = ToString[PacletObject[Join[<|"Name" -> name, "Version" -> "2.0"|>, restriction]], InputForm];
                {First[PacletDependencyInformation[owner, "CheckUpdates" -> True][name]["LatestVersion"]],
                    First[InstallPacletDependencies[owner, name][name]]},
                {restriction, {<|"SystemID" -> If[$SystemID === "Windows-x86-64", "MacOSX-ARM64", "Windows-x86-64"]|>,
                    <|"WolframVersion" -> IntegerString[Ceiling[$VersionNumber] + 1] <> "+"|>}}]];
        {outcomes, downloads}
    ],
    {{{"DependencyArchive", "DependencyArchive"}, {"DependencyArchive", "DependencyArchive"}}, 0},
    TestID -> "Dependencies: Git release discovery and installation reject an incompatible target before fetching the archive"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Demo/GitLibrary" <> StringDelete[CreateUUID[], "-"], commit = "0123456789abcdef0123456789abcdef01234567",
            info, tree, zip, owner, urls = {}, release, installed, found},
        info = "PacletObject[<|\"Name\" -> \"" <> name <> "\", \"Version\" -> \"1.4.0\"|>]";
        tree = CreateDirectory[FileNameJoin[{root, "repo-" <> commit, "Sub", "Kernel"}]];
        Export[FileNameJoin[{DirectoryName[tree], "PacletInfo.wl"}], info, "Text"];
        Export[FileNameJoin[{tree, "Library.wl"}], "1", "Text"];
        zip = CreateArchive[FileNameJoin[{root, "repo-" <> commit}], FileNameJoin[{root, "tree.zip"}]];
        owner = testDependencyOwner[{}, {<|"Name" -> name, "Git" -> "https://github.com/demo/repo.git", "Commit" -> commit, "Path" -> "Sub"|>}];
        WithCleanup[
            Block[{$AllowInternet = True, $AllowDataUpdates = False, dependencyGitText = Function[url, AppendTo[urls, url]; info],
                    dependencyDownloaded = Function[{url, file}, AppendTo[urls, url]; CopyFile[zip, file]; File[file]]},
                release = dependencyRelease[owner, dependencyManifest[owner][name]];
                installed = InstallPacletDependencies[owner, name]];
            found = PacletFind[name];
            {release["Version"], urls, MatchQ[installed[name], _PacletObject], If[found === {}, None, First[found]["Version"]]},
            Scan[PacletUninstall, PacletFind[name]];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {"1.4.0", {
        "https://raw.githubusercontent.com/demo/repo/0123456789abcdef0123456789abcdef01234567/Sub/PacletInfo.wl",
        "https://raw.githubusercontent.com/demo/repo/0123456789abcdef0123456789abcdef01234567/Sub/PacletInfo.wl",
        "https://codeload.github.com/demo/repo/zip/0123456789abcdef0123456789abcdef01234567"}, True, "1.4.0"},
    TestID -> "Dependencies: a git dependency installs the paclet at the path in the commit's tree, at the version its PacletInfo.wl gives"
]

End[]
