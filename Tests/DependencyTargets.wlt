(* Installation into a directory of the caller's own, a published release beside a Git pin, the
   required-dependency loader, and what the Wolfram System's PacletInstall does with a consumer's
   Dependencies. Every paclet these tests create has a unique name, and each test removes only the
   copies it created. *)
PacletDirectoryLoad[DirectoryName[$TestFileName, 2]]
Needs["WolframInstitute`PacletPage`"]

Begin["WolframInstitute`PacletPage`Private`"]

testUniqueName[prefix_String] := prefix <> StringDelete[CreateUUID[], "-"]
testPaclet[root_String, sub_String, info_Association] := With[{dir = CreateDirectory[FileNameJoin[{root, sub}], CreateIntermediateDirectories -> True]},
    Put[PacletObject[info], FileNameJoin[{dir, "PacletInfo.wl"}]];
    dir
]

VerificationTest[
    Module[{root = CreateDirectory[], reqName = testUniqueName["TestTargetReq"], optName = testUniqueName["TestTargetOpt"],
            reqArc, optArc, owner, target, first, again, copies},
        target = FileNameJoin[{root, "deps"}];
        WithCleanup[
            reqArc = CreatePacletArchive[testPaclet[root, "req", <|"Name" -> reqName, "Version" -> "1.0.0"|>], root];
            optArc = CreatePacletArchive[testPaclet[root, "opt", <|"Name" -> optName, "Version" -> "1.0.0"|>], root];
            owner = PacletObject[<|"Name" -> "Example/TargetConsumer", "Version" -> "1.0.0", "Location" -> root,
                "Dependencies" -> {<|"Name" -> reqName, "Archive" -> FileNameTake[reqArc]|>,
                    <|"Name" -> optName, "Optional" -> True, "Archive" -> FileNameTake[optArc]|>}|>];
            first = InstallPacletDependencies[owner, "Required", "CheckReadiness" -> False, "TargetDirectory" -> target];
            again = Block[{ExtractPacletArchive = (Throw["Unexpected second unpacking"] &)},
                InstallPacletDependencies[owner, "Required", "CheckReadiness" -> False, "TargetDirectory" -> target]];
            copies = PacletFind[reqName];
            {
                Keys[first] === {reqName} && MatchQ[first[reqName], _PacletObject],
                StringStartsQ[ExpandFileName[first[reqName]["Location"]], ExpandFileName[target]],
                MatchQ[again[reqName], _PacletObject] && again[reqName]["Location"] === first[reqName]["Location"],
                Length[copies] === 1,
                PacletFind[optName] === {},
                Select[FileNames[All, FileNameJoin[{$UserBasePacletsDirectory, "Repository"}]], StringContainsQ[#, reqName] &] === {},
                FileNames[All, FileNameJoin[{target, ".staging"}]] === {}
            },
            PacletDirectoryUnload[target];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, True, True},
    TestID -> "TargetDirectory: required dependencies unpack into the directory, stay loaded and are reused, and nothing reaches the user repository"
]

VerificationTest[
    Module[{root = CreateDirectory[], gitName = testUniqueName["TestTargetGit"], c1 = StringRepeat["a", 40], c2 = StringRepeat["b", 40],
            url = "https://github.com/example/target-git", gitDir, assets, owner, target, first, second, copies, locks},
        target = FileNameJoin[{root, "deps"}];
        owner = Function[commit, PacletObject[<|"Name" -> "Example/TargetGitConsumer", "Version" -> "1.0.0", "Location" -> root,
            "Dependencies" -> {<|"Name" -> gitName, "Git" -> url, "Commit" -> commit|>}|>]];
        WithCleanup[
            gitDir = testPaclet[root, "git", <|"Name" -> gitName, "Version" -> "2.0.0"|>];
            assets = CreateDirectory[FileNameJoin[{root, "Assets", "Dependencies"}], CreateIntermediateDirectories -> True];
            Scan[Function[commit,
                Export[FileNameJoin[{gitDir, $dependencyGitReceipt}], <|"Git" -> "example/target-git", "Path" -> "", "Commit" -> commit|>, "RawJSON"];
                RenameFile[CreatePacletArchive[gitDir, root], FileNameJoin[{assets, StringTake[commit, 7] <> ".paclet"}]]],
                {c1, c2}];
            (* the receipt as each installation left it: the second replaces the first's directory *)
            first = Replace[InstallPacletDependencies[owner[c1], gitName, "CheckReadiness" -> False, "TargetDirectory" -> target],
                a_Association :> Map[If[MatchQ[#, _PacletObject], {#, dependencyInstalledGit[#]["Commit"]}, #] &, a]];
            second = InstallPacletDependencies[owner[c2], gitName, "CheckReadiness" -> False, "TargetDirectory" -> target];
            copies = Select[FileNames[All, target], DirectoryQ[#] && ! StringStartsQ[FileNameTake[#], "."] &];
            locks = Block[{$dependencyTarget = ExpandFileName[target]}, withDependencyGitLock[gitName, FileNames[".git-pin-*", target]]];
            {
                MatchQ[first[gitName], {_PacletObject, c1}],
                MatchQ[second[gitName], _PacletObject] && dependencyInstalledGit[second[gitName]]["Commit"] === c2,
                Length[copies] === 1 && dependencyInstalledGit[First[PacletFind[gitName]]]["Commit"] === c2,
                Length[locks] >= 1
            },
            PacletDirectoryUnload[target];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True},
    TestID -> "TargetDirectory: a Git pin unpacks with its receipt, a new pin replaces the earlier copy, and the pin's lock is kept in the directory"
]

VerificationTest[
    Module[{owner = PacletObject[<|"Name" -> "Example/MonorepoConsumer", "Version" -> "1.0.0"|>], both, receipt, other, gitRelease,
            installed, published, fallback, failedBoth, failure},
        both = <|"Name" -> "Demo/Algebra", "Version" -> "1.0+",
            "Resource" -> "https://www.wolframcloud.com/obj/demo/DeployedResources/Paclet/Demo/Algebra",
            "Git" -> "https://github.com/demo/monorepo", "Commit" -> StringRepeat["c", 40], "Path" -> "Algebra"|>;
        receipt = dependencyGitSource[both, StringRepeat["c", 40]];
        other = dependencyGitSource[both, StringRepeat["d", 40]];
        gitRelease = <|"Version" -> "1.1.0", "GitURL" -> both["Git"], "GitManifest" -> "PacletInfo.wl"|>;
        installed = {PacletObject[<|"Name" -> "Demo/Algebra", "Version" -> "1.2.0"|>]};
        failure = Failure["DependencyRelease", <|"MessageTemplate" -> "No archive"|>];
        published = Block[{dependencyResourceMetadata = (<|"Name" -> "Demo/Algebra", "ResourceType" -> "Paclet", "PacletVersion" -> "1.2.0",
                    "ResourceLocation" -> CloudObject["https://www.wolframcloud.com/obj/demo/description"]|> &),
                dependencyGitRelease = (Throw["Unexpected Git lookup"] &)},
            dependencyRelease[owner, both, True]];
        fallback = Block[{dependencyResourceMetadata = ($Failed &), dependencyGitRelease = (gitRelease &)},
            dependencyRelease[owner, both, True]];
        {
            dependencyEntryQ[both, owner],
            ! dependencyEntryQ[Append[KeyDrop[both, "Resource"], "Archive" -> "Algebra.paclet"], owner],
            dependencyCopySatisfiesQ[both, Missing["Unknown"]] && dependencyCopySatisfiesQ[both, receipt] && ! dependencyCopySatisfiesQ[both, other],
            ! dependencyCopySatisfiesQ[KeyDrop[both, "Resource"], Missing["Unknown"]],
            {dependencyLocalStatus[both, installed, Missing["Unknown"]], dependencyLocalStatus[both, installed, other]},
            published["Version"] === "1.2.0" && KeyExistsQ[published, "DescriptionURL"],
            fallback === gitRelease,
            Block[{dependencyResourceInstall = (failure &), dependencyGitRelease = (gitRelease &), dependencyGitInstall = ({"checkout", #1["Version"]} &)},
                dependencyPublishedInstall[both, <|"Version" -> "1.2.0"|>]],
            MatchQ[Block[{dependencyResourceInstall = (failure &), dependencyGitRelease = (Failure["DependencyRelease", <|"MessageTemplate" -> "No manifest"|>] &)},
                dependencyPublishedInstall[both, <|"Version" -> "1.2.0"|>]], Failure["DependencyRelease", KeyValuePattern["PublishedFailure" -> failure]]],
            (* the dialog offers no upgrade to a copy from the release at the latest version *)
            With[{row = dependencyInfo[owner, both, installed, False, False]},
                {row["Status"], KeyExistsQ[row, "GitRevisionMatch"], MemberQ[dependencyRowActions[Append[row, "LatestVersion" -> "1.2.0"]], "Update"]}]
        }
    ],
    {True, True, True, True, {"Installed", "Revision mismatch"}, True, True, {"checkout", "1.1.0"}, True, {"Installed", False, False}},
    TestID -> "Resource beside Git: the published release first, the pinned checkout as its fallback, and a copy from either meets the declaration"
]

VerificationTest[
    {
        dependencySiteArchiveURL[PacletObject[<|"Name" -> "Wolfram/Foo", "Version" -> "1.2.3", "Location" -> "https://pacletserver.wolfram.com"|>]],
        dependencySiteArchiveURL[PacletObject[<|"Name" -> "Bar", "Version" -> "2.0", "Location" -> "https://example.org/site/"|>]],
        FailureQ[dependencySiteArchiveURL[PacletObject[<|"Name" -> "Bar", "Version" -> "2.0", "Location" -> FileNameJoin[{$TemporaryDirectory, "site"}]|>]]]
    },
    {"https://pacletserver.wolfram.com/Paclets/Wolfram__Foo-1.2.3.paclet", "https://example.org/site/Paclets/Bar-2.0.paclet", True},
    TestID -> "TargetDirectory: a paclet site's release downloads from the site's Paclets directory"
]

VerificationTest[
    With[{dir = FileNameJoin[{$TemporaryDirectory, "consumer"}]},
        {buildDependencyDirectory[dir, Automatic], buildDependencyDirectory[dir, None], buildDependencyDirectory[dir, File[FileNameJoin[{dir, "shared"}]]],
            buildDependencyDirectory[dir, 3], FailureQ[withDependencyTarget[3, Throw["Unexpected installation"]]],
            Quiet[installBuildDependencies[BuildPacletDocumentation, dir, All, 3], {BuildPacletDocumentation::installdeps}]}],
    With[{dir = FileNameJoin[{$TemporaryDirectory, "consumer"}]},
        {FileNameJoin[{dir, "build", "dependencies"}], Automatic, FileNameJoin[{dir, "shared"}], $Failed, True, $Failed}],
    TestID -> "BuildPacletDocumentation: DependencyDirectory is build/dependencies by default, a directory, or None for the user repository"
]

VerificationTest[
    Module[{root = CreateDirectory[], checkout = DirectoryName[$TestFileName, 2], reqName = testUniqueName["TestLoaderReq"],
            optName = testUniqueName["TestLoaderOpt"], consumer, quiet, installing, calls = {}},
        consumer = Function[{sub, declared, manager}, With[{dir = testPaclet[root, sub, <|"Name" -> "Demo/LoaderConsumer", "Version" -> "1.0",
                "Dependencies" -> declared|>]},
            CreateDirectory[FileNameJoin[{dir, "Resources"}]];
            Export[FileNameJoin[{dir, "Resources", "LoadDependencyManager.wl"}], manager, "Text"];
            CopyFile[FileNameJoin[{checkout, "Assets", "LoadRequiredDependencies.wl"}], FileNameJoin[{dir, "Resources", "LoadRequiredDependencies.wl"}]]]];
        WithCleanup[
            (* every required dependency present or unsupported here: the manager must not load *)
            quiet = Get[consumer["present", {"PacletTools", <|"Name" -> reqName, "Platforms" -> {"NonExistent-OS-99"}|>,
                <|"Name" -> optName, "Optional" -> True|>}, "Throw[\"Unexpected manager load\"]"]];
            (* one required dependency missing: the manager loads and installs that one alone *)
            installing = Block[{InstallPacletDependencies = Function[Null, AppendTo[calls, {##}];
                    <|reqName -> PacletObject[<|"Name" -> reqName, "Version" -> "1.0"|>]|>]},
                Get[consumer["missing", {"PacletTools", reqName -> "1.0+", <|"Name" -> optName, "Optional" -> True|>}, "Null"]]];
            {quiet, installing, Length[calls], calls[[1, 1]]["Name"], calls[[1, 2 ;;]]},
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {Null, Null, 1, "Demo/LoaderConsumer", {{reqName}, "CheckReadiness" -> False}},
    SameTest -> (MatchQ[#1, {Null, Null, 1, "Demo/LoaderConsumer", {{_String}, "CheckReadiness" -> False}}] &),
    TestID -> "Required dependency loader: nothing loads while every required dependency is present, and a missing one installs alone, without the optional ones"
]

VerificationTest[
    Module[{root = CreateDirectory[], checkout = DirectoryName[$TestFileName, 2], reqName = testUniqueName["TestRuntimeReq"],
            optName = testUniqueName["TestRuntimeOpt"], consumer, resources, reqArc, optArc, loader, first, second},
        WithCleanup[
            consumer = CreateDirectory[FileNameJoin[{root, "consumer"}]];
            resources = CreateDirectory[FileNameJoin[{consumer, "Resources"}]];
            reqArc = CreatePacletArchive[testPaclet[root, "req", <|"Name" -> reqName, "Version" -> "1.0.0"|>], consumer];
            optArc = CreatePacletArchive[testPaclet[root, "opt", <|"Name" -> optName, "Version" -> "1.0.0"|>], consumer];
            Put[PacletObject[<|"Name" -> "Demo/RuntimeConsumer", "Version" -> "1.0",
                "Dependencies" -> {<|"Name" -> reqName, "Archive" -> FileNameTake[reqArc]|>,
                    <|"Name" -> optName, "Optional" -> True, "Archive" -> FileNameTake[optArc]|>}|>], FileNameJoin[{consumer, "PacletInfo.wl"}]];
            CopyFile[FileNameJoin[{checkout, "Assets", "LoadDependencyManager.wl"}], FileNameJoin[{resources, "LoadDependencyManager.wl"}]];
            loader = CopyFile[FileNameJoin[{checkout, "Assets", "LoadRequiredDependencies.wl"}], FileNameJoin[{resources, "LoadRequiredDependencies.wl"}]];
            first = Get[loader];
            second = Block[{InstallPacletDependencies = (Throw["Unexpected second installation"] &)}, Get[loader]];
            {first, PacletFind[reqName] =!= {}, PacletFind[optName] === {}, second},
            Scan[PacletUninstall, Join[PacletFind[reqName], PacletFind[optName]]];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {Null, True, True, Null},
    TestID -> "Required dependency loader: a packaged consumer installs its missing required dependency, leaves the optional one alone, and is quiet once it is installed"
]

(* The Wolfram System reads no Dependencies list: nothing is installed for any kind of entry, and
   with no network a fetch it tried would leave a message. *)
VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestIgnored"], reqName = testUniqueName["TestIgnoredReq"],
            optName = testUniqueName["TestIgnoredOpt"], gitName = testUniqueName["TestIgnoredGit"], context, dir, archive, run, load},
        context = name <> "`";
        WithCleanup[
            dir = testPaclet[root, "consumer", <|"Name" -> name, "Version" -> "1.0.0", "PrimaryContext" -> context,
                "Extensions" -> {{"Kernel", "Root" -> "Kernel", "Context" -> {{context, "Main.wl"}}}},
                "Dependencies" -> {reqName,
                    <|"Name" -> optName, "Optional" -> True, "Resource" -> "https://www.wolframcloud.com/obj/demo/DeployedResources/Paclet/" <> optName|>,
                    <|"Name" -> gitName, "Git" -> "https://github.com/demo/monorepo", "Commit" -> StringRepeat["e", 40], "Path" -> "Pinned"|>}|>];
            CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
            Export[FileNameJoin[{dir, "Kernel", "Main.wl"}],
                "BeginPackage[\"" <> context <> "\"]\nloadedQ::usage = \"loadedQ[] is True.\";\nBegin[\"`Private`\"]\nloadedQ[] := True\nEnd[]\nEndPackage[]\n", "Text"];
            archive = CreatePacletArchive[dir, root];
            run = EvaluationData[PacletInstall[archive]];
            load = EvaluationData[Block[{$ContextPath = $ContextPath}, Needs[context]; Symbol[context <> "loadedQ"][]]];
            {MatchQ[run["Result"], _PacletObject] && run["Result"]["Name"] === name, run["MessagesText"],
                PacletFind[reqName] === {} && PacletFind[optName] === {} && PacletFind[gitName] === {},
                load["Result"], load["MessagesText"]},
            Scan[PacletUninstall, PacletFind[name]];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, {}, True, True, {}},
    TestID -> "PacletInstall ignores a consumer's Dependencies: required, optional and Git entries install nothing and leave no message"
]

End[]
