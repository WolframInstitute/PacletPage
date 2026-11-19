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
            archive, consumer, packages = "WolframInstitute`PacletPage`", files = {}, reused, result},
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
            Block[{$Packages = DeleteCases[$Packages, packages], $CacheBaseDirectory = root, PacletFind = ({} &),
                    PacletDirectoryLoad = (Null &), Needs = Function[{context, file}, AppendTo[files, file]; Null]},
                result = Get[loader];
                reused = Block[{ExtractPacletArchive = (Throw["Unexpected second extraction"] &)}, Get[loader]]];
            {consumer["Name"], result, reused, MatchQ[files, {_String, _String}] && SameQ @@ files && FileExistsQ[First[files]]},
            DeleteDirectory[root, DeleteContents -> True]]],
    {"Demo/Consumer", Null, Null, True}, TestID -> "Dependencies: a packaged offline consumer keeps its own identity and reuses its cached manager"
]

testDependencyOwner[required_, optional_] := PacletObject[<|
    "Name" -> "Demo/Consumer", "Version" -> "1.0", "PrimaryContext" -> "Demo`Consumer`",
    "Dependencies" -> required, "OptionalDependencies" -> optional|>]

VerificationTest[
    Normal[PacletDependencyInformation[testDependencyOwner[{}, {}]]],
    <||>, TestID -> "Dependencies: empty manifest"
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
    Module[{owner = testDependencyOwner[{}, {"Demo/First", "Demo/Second"}]},
        Block[{dependencyRelease = Function[{p, entry}, If[entry["Name"] === "Demo/First", dependencyFailure["Network", "Offline."], <|"Version" -> "1.0"|>]]},
            With[{rows = PacletDependencyInformation[owner, "CheckUpdates" -> True]},
                {First[rows["Demo/First"]["LatestVersion"]], rows["Demo/Second"]["LatestVersion"]}]]],
    {"Network", "1.0"}, TestID -> "Dependencies: update lookup returns errors per library"
]

VerificationTest[
    Module[{owner = testDependencyOwner[{}, {"Demo/Slow", "Demo/Fast"}], calls = {}, latest},
        Block[{$dependencyVersionTimeout = 0.15, PacletFindRemote = Function[{name, refresh},
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
    With[{boxes = ToBoxes[dependencyCheckControl["Check readiness", "Test readiness.", testStart[], testCancelled]]},
        Cases[boxes, ButtonBox[_, opts___] /; ! FreeQ[HoldComplete[opts], testStart | testCancelled] :>
            FirstCase[HoldComplete[opts], HoldPattern[Method -> m_] :> m, None, {1}], Infinity]],
    {"Queued", "Preemptive"}, TestID -> "Dependencies: a check uses the main evaluation queue and cancellation uses the preemptive link"
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
        {StringStartsQ[unconfirmed, "Could not confirm"], dependencyActionNotice["Uninstall", $Failed, row] === unconfirmed,
            dependencyActionNotice["Install", installed, Missing["Unknown"]] === unconfirmed,
            dependencyActionNotice["Install", installed, <|"InstalledVersion" -> Missing["NotInstalled"]|>] === unconfirmed,
            dependencyActionNotice["Update", installed, row], dependencyActionNotice["Uninstall", Null, row]}],
    {True, True, True, True, "Version 1.1 is installed.", "Removed from the user repository."},
    TestID -> "Dependencies: success feedback requires a confirmed action and local record"
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
    {"No libraries to check.", "1 passed", "3 skipped", "1 passed, 1 failed, 3 skipped"},
    TestID -> "Dependencies: the readiness summary counts actual probes and distinguishes skipped libraries"
]

VerificationTest[
    Module[{source = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"]},
        WithCleanup[
            Put[PacletObject[<|"Name" -> name, "Version" -> "1.0"|>], FileNameJoin[{source, "PacletInfo.wl"}]];
            PacletDirectoryLoad[source];
            {First[UninstallPacletDependencies[testDependencyOwner[{}, {name}], All][name]], DirectoryQ[source]},
            PacletDirectoryUnload[source]; DeleteDirectory[source, DeleteContents -> True]]],
    {"ExternalInstallation", True}, TestID -> "Dependencies: protect development checkouts"
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
    With[{owner = testDependencyOwner[{"Demo/Required"}, {"Demo/Optional"}]},
        Block[{dependencyDesktopQ = (True &), Notebooks = ({} &), $dependencyDialogs = <||>,
                dependencyRelease = (Throw["Unexpected release lookup"] &), dependencyReadiness = (Throw["Unexpected readiness check"] &),
                PacletSiteUpdate = (Throw["Unexpected site update"] &), PacletFindRemote = (Throw["Unexpected remote lookup"] &),
                URLRead = (Throw["Unexpected request"] &), URLSubmit = (Throw["Unexpected request"] &),
                dependencyDialogContent = Function[{paclet, command, rows}, Keys[rows]], CreateDialog = Function[Null, First[{##}]]},
            Catch[{PacletDependencyDialog[owner, "Optional"], PacletDependencyDialog[owner]}]]],
    {{"Demo/Optional"}, {"Demo/Required", "Demo/Optional"}},
    TestID -> "Dependencies: opening either dialog reads local metadata and leaves readiness and updates to its buttons"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"], source, owner,
            installed, archives, offered, declare, selectVersion},
        source = CreateDirectory[FileNameJoin[{root, "library"}]];
        owner = CreateDirectory[FileNameJoin[{root, "consumer"}]];
        offered = FileNameJoin[{owner, "library.paclet"}];
        declare[record_] := Put[PacletObject[<|"Name" -> "TestConsumer", "Version" -> "1.0",
            "OptionalDependencies" -> {record}|>], FileNameJoin[{owner, "PacletInfo.wl"}]];
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
    TestID -> "Dependencies: real archive install, idempotence, numeric update, no downgrade, failed update and removal"
]

End[]
