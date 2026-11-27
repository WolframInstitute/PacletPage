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
                Select[FileNames[All, target], StringStartsQ[FileNameTake[#], "."] &] === {}
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

VerificationTest[
    Module[{owner = PacletObject[<|"Name" -> "Example/PinConsumer", "Version" -> "1.0"|>], url = "https://www.wolframcloud.com/obj/demo/DeployedResources/Paclet/Example/Lib",
            sha = StringRepeat["f", 40], entry, gitPinned, paclet200},
        entry = <|"Name" -> "Example/Lib", "Version" -> "1.0.0+", "Resource" -> url, "Pin" -> <|"Version" -> "1.5.0"|>|>;
        paclet200 = PacletObject[<|"Name" -> "Example/Lib", "Version" -> "2.0.0"|>];
        gitPinned = dependencyPinned[<|"Name" -> "Example/Lib", "Resource" -> url, "Pin" -> <|"Git" -> "https://github.com/example/lib", "Commit" -> sha|>|>];
        Block[{dependencyResourceRelease = (<|"Version" -> "2.0.0", "DescriptionURL" -> "https://www.wolframcloud.com/obj/demo/description"|> &)},
            {
                dependencyEntryQ[entry, owner],
                (* a pinned install checks the pin whole: an exact version, something to pin, keys it knows *)
                Block[{$dependencyPinned = True}, ! dependencyEntryQ[Append[entry, "Pin" -> <|"Version" -> "1.5+"|>], owner]],
                Block[{$dependencyPinned = True}, ! dependencyEntryQ[Append[entry, "Pin" -> <||>], owner]],
                Block[{$dependencyPinned = True}, ! dependencyEntryQ[Append[entry, "Pin" -> <|"Resource" -> url|>], owner]],
                dependencyRelease[owner, entry, True]["Version"],
                MatchQ[dependencyRelease[owner, dependencyPinned[entry], True], Failure["DependencyVersion", _]],
                KeyExistsQ[gitPinned, "Resource"], gitPinned["Commit"] === sha && ! KeyExistsQ[gitPinned, "Pin"],
                dependencyLocalStatus[entry, {paclet200}],
                dependencyPinned[KeyDrop[entry, "Pin"]] === KeyDrop[entry, "Pin"]
            }]],
    {True, True, True, True, "2.0.0", True, False, True, "Installed", True},
    TestID -> "Pin: ignored outside pinned mode; pinned, its exact version refuses another release and a pinned source replaces the entry's"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestPinLib"], arc, owner, target, unpinned, pinned, conflict},
        target = FileNameJoin[{root, "deps"}];
        WithCleanup[
            arc = CreatePacletArchive[testPaclet[root, "lib", <|"Name" -> name, "Version" -> "1.0.0"|>], root];
            testPaclet[root, "elsewhere/" <> name, <|"Name" -> name, "Version" -> "1.0.0"|>];
            PacletDirectoryLoad[FileNameJoin[{root, "elsewhere"}]];
            owner = PacletObject[<|"Name" -> "Example/PinTarget", "Version" -> "1.0", "Location" -> root,
                "Dependencies" -> {<|"Name" -> name, "Version" -> "1.0+", "Archive" -> FileNameTake[arc], "Pin" -> <|"Version" -> "1.0.0"|>|>}|>];
            unpinned = InstallPacletDependencies[owner, name, "CheckReadiness" -> False, "TargetDirectory" -> target][name];
            pinned = InstallPacletDependencies[owner, name, "CheckReadiness" -> False, "TargetDirectory" -> target, "Pinned" -> True][name];
            testPaclet[root, "newer/" <> name, <|"Name" -> name, "Version" -> "1.1.0"|>];
            PacletDirectoryLoad[FileNameJoin[{root, "newer"}]];
            conflict = InstallPacletDependencies[owner, name, "CheckReadiness" -> False, "TargetDirectory" -> target, "Pinned" -> True][name];
            {
                MatchQ[unpinned, _PacletObject] && StringStartsQ[ExpandFileName[unpinned["Location"]], ExpandFileName[FileNameJoin[{root, "elsewhere"}]]],
                MatchQ[pinned, _PacletObject] && StringStartsQ[ExpandFileName[pinned["Location"]], ExpandFileName[target]],
                MatchQ[conflict, Failure["DependencyVersionConflict", _]]
            },
            Scan[PacletDirectoryUnload, FileNameJoin[{root, #}] & /@ {"elsewhere", "newer", "deps"}];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True},
    TestID -> "Pin: a pinned installation into a directory counts only the copies there, and a newer copy elsewhere is a conflict"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestPinBackLib"], url, arcs, owner, install, newer, back},
        WithCleanup[
            url[v_] := "https://example.org/Example__Lib-" <> v <> ".paclet";
            arcs = AssociationMap[CreatePacletArchive[testPaclet[root, "v" <> #, <|"Name" -> name, "Version" -> #|>], root] &, {"1.5.0", "2.0.0"}];
            owner[v_] := PacletObject[<|"Name" -> "Example/PinBackConsumer", "Version" -> "1.0",
                "Dependencies" -> {<|"Name" -> name, "Version" -> "1.0+", "Pin" -> <|"Version" -> v, "Archive" -> url[v]|>|>}|>];
            install[v_] := Block[{dependencyDownloaded = Function[{u, file}, CopyFile[arcs[v], file]; File[file]],
                    dependencyGitText = (Missing["NotFound"] &), dependencyCurlText = (Missing["NotFound"] &)},
                InstallPacletDependencies[owner[v], name, "CheckReadiness" -> False, "TargetDirectory" -> FileNameJoin[{root, "deps"}], "Pinned" -> True][name]];
            newer = install["2.0.0"];
            (* the pin moves back, as a revert does *)
            back = install["1.5.0"];
            {
                MatchQ[newer, _PacletObject] && newer["Version"] === "2.0.0",
                MatchQ[back, _PacletObject] && back["Version"] === "1.5.0",
                Sort[Through[Select[PacletFind[name], StringStartsQ[ExpandFileName[#["Location"]], ExpandFileName[FileNameJoin[{root, "deps"}]]] &]["Version"]]]
            },
            PacletDirectoryUnload[FileNameJoin[{root, "deps"}]];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, {"1.5.0"}},
    TestID -> "Pin: a pin that moves back replaces the newer copy in the directory with the pinned release"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestURLLib"], url = "https://example.org/Example__Lib-1.5.0.paclet",
            demo = "https://www.wolframcloud.com/obj/demo/DeployedResources/Paclet/Example/Lib", arc150, arc151, owner, install, good, bad},
        WithCleanup[
            arc150 = CreatePacletArchive[testPaclet[root, "v150", <|"Name" -> name, "Version" -> "1.5.0"|>], root];
            arc151 = CreatePacletArchive[testPaclet[root, "v151", <|"Name" -> name, "Version" -> "1.5.1"|>], root];
            owner = PacletObject[<|"Name" -> "Example/URLConsumer", "Version" -> "1.0",
                "Dependencies" -> {<|"Name" -> name, "Pin" -> <|"Version" -> "1.5.0", "Archive" -> url|>|>}|>];
            (* no SHA-256 published beside the archive, and no network *)
            install = Function[{archive, target}, Block[{dependencyDownloaded = Function[{u, file}, CopyFile[archive, file]; File[file]],
                    dependencyGitText = (Missing["NotFound"] &), dependencyCurlText = (Missing["NotFound"] &)},
                InstallPacletDependencies[owner, name, "CheckReadiness" -> False, "TargetDirectory" -> target, "Pinned" -> True][name]]];
            good = install[arc150, FileNameJoin[{root, "good"}]];
            bad = install[arc151, FileNameJoin[{root, "bad"}]];
            {
                MatchQ[good, _PacletObject] && good["Version"] === "1.5.0" && StringStartsQ[ExpandFileName[good["Location"]], ExpandFileName[FileNameJoin[{root, "good"}]]],
                MatchQ[bad, Failure["DependencyArchive", _]],
                dependencyEntryQ[<|"Name" -> name, "Version" -> "1.5.0", "Archive" -> url|>, owner],
                ! dependencyEntryQ[<|"Name" -> name, "Version" -> "1.5+", "Archive" -> url|>, owner],
                ! dependencyEntryQ[<|"Name" -> name, "Version" -> "1.5.0", "Archive" -> "https://example.org/Lib.zip"|>, owner],
                ! dependencyEntryQ[<|"Name" -> name, "Version" -> "1.5.0", "Resource" -> demo, "Archive" -> url|>, owner],
                dependencyEntryQ[<|"Name" -> name, "Version" -> "1.5.0", "Resource" -> demo,
                    "Archive" -> "https://www.wolframcloud.com/obj/demo/Paclets/Example__Lib-1.5.0.paclet"|>, owner]
            },
            Scan[PacletDirectoryUnload, FileNameJoin[{root, #}] & /@ {"good", "bad"}];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, True, True},
    TestID -> "Archive at an https address: an exact pinned release downloads into the directory, and an archive of another version is refused"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestPinHashLib"], url = "https://example.org/Example__Lib-1.0.0.paclet", arc, hash, owner, install, results},
        WithCleanup[
            arc = CreatePacletArchive[testPaclet[root, "lib", <|"Name" -> name, "Version" -> "1.0.0"|>], root];
            hash = FileHash[arc, "SHA256", "HexString"];
            owner[pin_] := PacletObject[<|"Name" -> "Example/PinHashConsumer", "Version" -> "1.0", "Dependencies" -> {<|"Name" -> name, "Pin" -> pin|>}|>];
            (* the archive as downloaded, and a SHA-256 published beside it, or none *)
            install = Function[{pin, sidecar, target}, Block[{
                    dependencyDownloaded = Function[{u, file}, CopyFile[arc, file]; File[file]],
                    dependencyGitText = (sidecar &), dependencyCurlText = (Missing["NotFound"] &)},
                InstallPacletDependencies[owner[pin], name, "CheckReadiness" -> False, "TargetDirectory" -> FileNameJoin[{root, target}], "Pinned" -> True][name]]];
            results = {
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> ToUpperCase[hash]|>, Missing["NotFound"], "nosidecar"],
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> StringRepeat["0", 64]|>, Missing["NotFound"], "wrongpin"],
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> hash|>, StringRepeat["f", 64] <> "  Example__Lib-1.0.0.paclet\n", "disagree"],
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> hash|>, hash <> "  Example__Lib-1.0.0.paclet\n", "agree"],
                (* by system, as an "Archive" by system: this system's entry, another's wrong one, this system's wrong one, and none for this system *)
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> <|$SystemID -> hash, "Other-System" -> StringRepeat["0", 64]|>|>, Missing["NotFound"], "bysystem"],
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> <|$SystemID -> StringRepeat["0", 64], "All" -> hash|>|>, Missing["NotFound"], "bysystemwrong"],
                install[<|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> <|"Other-System" -> StringRepeat["0", 64]|>|>, Missing["NotFound"], "bysystemnone"]
            };
            {
                MatchQ[results[[1]], _PacletObject] && results[[1]]["Version"] === "1.0.0",
                MatchQ[results[[2]], Failure["DependencyArchive", _]],
                MatchQ[results[[3]], Failure["DependencyArchive", _]] && StringContainsQ[results[[3]]["MessageTemplate"], "disagrees"],
                MatchQ[results[[4]], _PacletObject],
                MatchQ[results[[5]], _PacletObject], MatchQ[results[[6]], Failure["DependencyArchive", _]], MatchQ[results[[7]], _PacletObject],
                Block[{$dependencyPinned = True}, dependencyPinQ[<|"Name" -> name, "Pin" -> <|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> <|$SystemID -> "abc"|>|>|>, owner[<||>]]],
                (* a hash names one archive: it needs an exact version, a commit pins a Git source instead, and it is 64 hex digits *)
                Block[{$dependencyPinned = True}, dependencyPinQ[<|"Name" -> name, "Version" -> "1.0+", "Pin" -> <|"Archive" -> url, "SHA256" -> hash|>|>, owner[<||>]]],
                Block[{$dependencyPinned = True}, dependencyPinQ[<|"Name" -> name, "Pin" -> <|"Version" -> "1.0.0", "Git" -> "https://github.com/demo/lib", "Commit" -> StringRepeat["a", 40], "SHA256" -> hash|>|>, owner[<||>]]],
                Block[{$dependencyPinned = True}, dependencyPinQ[<|"Name" -> name, "Pin" -> <|"Version" -> "1.0.0", "Archive" -> url, "SHA256" -> "abc"|>|>, owner[<||>]]]
            },
            Scan[PacletDirectoryUnload, FileNameJoin[{root, #}] & /@ {"nosidecar", "wrongpin", "disagree", "agree", "bysystem", "bysystemwrong", "bysystemnone"}];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, True, True, False, False, False, False},
    TestID -> "Archive by its pin's SHA-256: a pinned archive must match the hash its pin gives, for its system or for all, published one or not, and a published one that disagrees is refused"
]

VerificationTest[
    Module[{name = testUniqueName["TestFuturePinLib"], owner, entry},
        entry = <|"Name" -> name, "Version" -> "1.0+", "Pin" -> <|"Version" -> "1.0.0", "FutureField" -> "x"|>|>;
        owner = PacletObject[<|"Name" -> "Example/FuturePinConsumer", "Version" -> "1.0", "Dependencies" -> {entry}|>];
        {
            (* read for the dialog, the report and the loaders: the pin is an association, whatever it holds *)
            Block[{$dependencyPinned = False}, dependencyEntryQ[entry, owner]],
            AssociationQ[Block[{$dependencyPinned = False}, dependencyManifest[owner]]],
            (* a pinned install fails on a key it does not know *)
            Block[{$dependencyPinned = True}, dependencyEntryQ[entry, owner]],
            MatchQ[Block[{$dependencyPinned = True}, dependencyManifest[owner]], Failure["DependencyManifest", _]],
            Block[{$dependencyPinned = False}, dependencyEntryQ[<|"Name" -> name, "Pin" -> "not an association"|>, owner]]
        }
    ],
    {True, True, False, True, False},
    TestID -> "Pin: outside a pinned install a pin may hold keys this PacletPage does not know; a pinned install refuses them"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestSystemLib"], gh, repo, demo, arc, hash, owner, install, good, wrong, none, valid},
        gh = "https://github.com/demo/lib/releases/download/v1.0.0/Lib-" <> # <> "-1.0.0.paclet" &;
        repo = "https://resources.wolframcloud.com/PacletRepository/pacletsite/Paclets/Demo__Lib-1.0.0.paclet";
        demo = "https://www.wolframcloud.com/obj/demo/DeployedResources/Paclet/Demo/Lib";
        WithCleanup[
            arc = CreatePacletArchive[testPaclet[root, "lib", <|"Name" -> name, "Version" -> "1.0.0"|>], root];
            hash = FileHash[arc, "SHA256", "HexString"];
            owner = Function[archives, PacletObject[<|"Name" -> "Example/SystemConsumer", "Version" -> "1.0",
                "Dependencies" -> {<|"Name" -> name, "Pin" -> <|"Version" -> "1.0.0", "Archive" -> archives|>|>}|>]];
            (* downloads come from the local archive; the published checksum is the text given *)
            install = Function[{archives, sidecar, target}, Block[{
                    dependencyDownloaded = Function[{u, file}, CopyFile[arc, file]; File[file]],
                    dependencyGitText = Function[u, If[StringEndsQ[u, ".sha256"], sidecar, Missing["NotFound"]]]},
                InstallPacletDependencies[owner[archives], name, "CheckReadiness" -> False, "TargetDirectory" -> target, "Pinned" -> True][name]]];
            good = install[<|$SystemID -> gh[$SystemID], "Windows-x86-64" -> gh["Windows-x86-64"]|>, hash <> "  Lib.paclet\n", FileNameJoin[{root, "good"}]];
            wrong = install[<|$SystemID -> gh[$SystemID]|>, StringRepeat["0", 64] <> "  Lib.paclet\n", FileNameJoin[{root, "wrong"}]];
            none = install[<|"All" -> repo|>, Missing["NotFound"], FileNameJoin[{root, "none"}]];
            valid = Function[e, dependencyEntryQ[e, owner[<||>]]];
            {
                MatchQ[good, _PacletObject] && good["Version"] === "1.0.0",
                MatchQ[wrong, Failure["DependencyArchive", _]],
                MatchQ[none, _PacletObject],
                MatchQ[install[<|"NonExistent-OS-99" -> gh["NonExistent-OS-99"]|>, Missing["NotFound"], FileNameJoin[{root, "other"}]], Failure["UnsupportedSystem", _]],
                dependencySupportedQ[<|"Name" -> name, "Archive" -> <|"NonExistent-OS-99" -> gh["x"]|>|>],
                (* beside a resource: the publisher's own files, a GitHub release's and the Paclet Repository's *)
                valid[<|"Name" -> name, "Version" -> "1.0.0", "Resource" -> demo, "Archive" -> <|$SystemID -> gh[$SystemID], "All" -> repo|>|>],
                valid[<|"Name" -> name, "Version" -> "1.0.0", "Resource" -> demo, "Archive" -> <|$SystemID -> "https://example.org/Lib.paclet"|>|>],
                valid[<|"Name" -> name, "Version" -> "1.0+", "Archive" -> <|$SystemID -> gh[$SystemID]|>|>],
                valid[<|"Name" -> name, "Version" -> "1.0.0", "Archive" -> <||>|>]
            },
            Scan[PacletDirectoryUnload, FileNameJoin[{root, #}] & /@ {"good", "wrong", "none", "other"}];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, False, True, False, False, False},
    TestID -> "Archive by system: a pin names an archive for each system, checked against its published SHA-256, and a system without one is unsupported"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = testUniqueName["TestCurlLib"], url = "https://github.com/demo/lib/releases/download/v1.0.0/Lib-1.0.0.paclet",
            arc, hash, owner, kernelFails, install, viaCurl, wrong, bothFail, curlCalls = 0},
        WithCleanup[
            arc = CreatePacletArchive[testPaclet[root, "lib", <|"Name" -> name, "Version" -> "1.0.0"|>], root];
            hash = FileHash[arc, "SHA256", "HexString"];
            owner = PacletObject[<|"Name" -> "Example/CurlConsumer", "Version" -> "1.0",
                "Dependencies" -> {<|"Name" -> name, "Pin" -> <|"Version" -> "1.0.0", "Archive" -> url|>|>}|>];
            kernelFails = Failure["DependencyDownload", <|"MessageTemplate" -> "The archive download did not finish.", "Response" -> <||>|>];
            (* the kernel reaches neither the archive nor its checksum; curl, as given, reaches both *)
            install = Function[{curlFile, curlSidecar, target}, Block[{
                    dependencyDownloaded = (kernelFails &),
                    dependencyGitText = ($Failed &),
                    dependencyCurlDownload = Function[{u, file}, curlCalls++; If[curlFile, CopyFile[arc, file]; File[file], $Failed]],
                    dependencyCurlText = (curlSidecar &)},
                InstallPacletDependencies[owner, name, "CheckReadiness" -> False, "TargetDirectory" -> target, "Pinned" -> True][name]]];
            viaCurl = install[True, hash <> "  Lib-1.0.0.paclet\n", FileNameJoin[{root, "curl"}]];
            wrong = install[True, StringRepeat["f", 64] <> "  Lib-1.0.0.paclet\n", FileNameJoin[{root, "wrong"}]];
            bothFail = install[False, Missing["NotFound"], FileNameJoin[{root, "fail"}]];
            {
                MatchQ[viaCurl, _PacletObject] && viaCurl["Version"] === "1.0.0",
                MatchQ[wrong, Failure["DependencyArchive", _]],
                bothFail === kernelFails,
                curlCalls,
                (* a resource's archive, read from its description, gets no second try *)
                Block[{dependencyDownloaded = (kernelFails &), dependencyCurlDownload = (Throw["Unexpected curl"] &)},
                    dependencyDownloadInstall[url, name, "1.0.0"]] === kernelFails
            },
            Scan[PacletDirectoryUnload, FileNameJoin[{root, #}] & /@ {"curl", "wrong", "fail"}];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, 3, True},
    TestID -> "Archive by curl: a declared archive the kernel cannot fetch, and its SHA-256, come through the system's curl and are checked"
]

VerificationTest[
    Module[{root = CreateDirectory[], clients = {}, listener, drip, sent = 0, url, stalled, slow, time},
        WithCleanup[
            (* a local server that promises 1000 bytes and sends the first 100 at once; to the second
               client, the drip sends the rest 100 at a time *)
            listener = SocketListen[0, Function[event, If[StringContainsQ[event["Data"], "\r\n\r\n"],
                AppendTo[clients, event["SourceSocket"]];
                WriteString[event["SourceSocket"], "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\n\r\n" <> StringRepeat["x", 100]]]]];
            url = "http://127.0.0.1:" <> ToString[listener["Socket"]["DestinationPort"]] <> "/Lib.paclet";
            Block[{$dependencyStallSeconds = 2},
                {time, stalled} = AbsoluteTiming[dependencyDownloadAttempt[url, FileNameJoin[{root, "stalled"}]]];
                drip = SessionSubmit[ScheduledTask[If[Length[clients] === 2 && sent < 9, sent++; WriteString[Last[clients], StringRepeat["x", 100]]], 0.5]];
                slow = dependencyDownloadAttempt[url, FileNameJoin[{root, "slow"}]]];
            {
                MatchQ[stalled, Failure["DependencyDownload", KeyValuePattern["MessageParameters" -> {2}]]],
                time < 15,
                slow === File[FileNameJoin[{root, "slow"}]] && FileByteCount[First[slow]] === 1000
            },
            If[MatchQ[drip, _TaskObject], TaskRemove[drip]];
            Scan[Close, clients];
            DeleteObject[listener];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True},
    TestID -> "Archive download stall: a download that receives nothing for the stall time is given up, and a slow one that keeps receiving runs to its end"
]

VerificationTest[
    Module[{root = CreateDirectory[], consumer, target, calls = {}, result},
        WithCleanup[
            consumer = testPaclet[root, "consumer", <|"Name" -> "Demo/BuildToolConsumer", "Version" -> "1.0",
                "Dependencies" -> {<|"Name" -> "WolframInstitute/PacletPage"|>, <|"Name" -> "Demo/Other", "Optional" -> True|>}|>];
            target = FileNameJoin[{root, "deps"}];
            result = Block[{InstallPacletDependencies = Function[Null, AppendTo[calls, {##}];
                    AssociationMap[PacletObject[<|"Name" -> #, "Version" -> "1.0"|>] &, Flatten[{#2}]]]},
                installBuildDependencies[BuildPacletDocumentation, consumer, All, target]];
            {result, Length[calls], calls[[1, 1]]["Name"], calls[[1, 2]], calls[[2, 2]],
                AllTrue[calls, MemberQ[#, "Pinned" -> True] && MemberQ[#, "TargetDirectory" -> target] &]},
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, 2, "WolframInstitute/PacletPage", {"WolframInstitute/GitLink", "Wolfram/Parser"}, {"Demo/Other"}, True},
    TestID -> "BuildPacletDocumentation: the build tool's own dependencies install first, pinned, into the same directory, and the consumer's entry for the build tool is skipped"
]

VerificationTest[
    Module[{root = CreateDirectory[], a, b, key},
        WithCleanup[
            a = testPaclet[root, "a", <|"Name" -> "Wolfram/Parser", "Version" -> "1.0"|>];
            b = testPaclet[root, "b", <|"Name" -> "Wolfram/Parser", "Version" -> "1.1"|>];
            key = Function[dirs, Block[{parserDirectories = (dirs &)}, converterKey[<|"ID" -> "converter"|>]]];
            {key[{a}] === key[{a}], Length[DeleteDuplicates[{key[{}], key[{a}], key[{b}]}]]},
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, 3},
    TestID -> "BuildPacletDocumentation: a page's converter key has the code of the parser the converter loads"
]

(* a consumer with the two loaders in its Resources directory, the manager's loader given as text or
   copied from this checkout *)
testLoaderConsumer[root_String, sub_String, declared_List, manager_ : Automatic] := With[{dir = testPaclet[root, sub,
        <|"Name" -> "Demo/" <> StringDelete[sub, Except[LetterCharacter]] <> "Consumer", "Version" -> "1.0", "Dependencies" -> declared|>],
        checkout = DirectoryName[$TestFileName, 2]},
    CreateDirectory[FileNameJoin[{dir, "Resources"}]];
    If[StringQ[manager], Export[FileNameJoin[{dir, "Resources", "LoadDependencyManager.wl"}], manager, "Text"],
        CopyFile[FileNameJoin[{checkout, "Assets", "LoadDependencyManager.wl"}], FileNameJoin[{dir, "Resources", "LoadDependencyManager.wl"}]]];
    CopyFile[FileNameJoin[{checkout, "Assets", "LoadRequiredDependencies.wl"}], FileNameJoin[{dir, "Resources", "LoadRequiredDependencies.wl"}]];
    FileNameJoin[{dir, "Resources"}]
]
$testManagerResource = "https://www.wolframcloud.com/obj/demo/DeployedResources/Paclet/WolframInstitute/PacletPage";

VerificationTest[
    Module[{root = CreateDirectory[], functions, older, newer, behind, required},
        functions = Append["WolframInstitute`PacletPage`" <> # & /@ {"PacletDependencyDialog", "PacletDependencyInformation",
            "InstallPacletDependencies", "UpdatePacletDependencies", "UninstallPacletDependencies"}, "WolframInstitute`PacletPage`PacletDependencyReport"];
        older = PacletObject[<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.92", "Extensions" -> {{"Kernel", "Symbols" -> functions}}|>];
        WithCleanup[
            newer = Get[FileNameJoin[{testLoaderConsumer[root, "atleast", {<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.0+"|>}], "LoadDependencyManager.wl"}]];
            behind = Get[FileNameJoin[{testLoaderConsumer[root, "ahead", {<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "99.0+"|>}], "LoadDependencyManager.wl"}]];
            required = With[{loader = FileNameJoin[{testLoaderConsumer[root, "older",
                    {<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.94+", "Resource" -> $testManagerResource|>}], "LoadDependencyManager.wl"}]},
                Internal`InheritedBlock[{PacletFind}, Unprotect[PacletFind];
                    PacletFind["WolframInstitute/PacletPage"] = {older};
                    Block[{$Packages = DeleteCases[$Packages, "WolframInstitute`PacletPage`"], Needs = (Throw["Unexpected manager load"] &)}, Get[loader]]]];
            {newer, MatchQ[behind, Failure["DependencyManagerRestart", KeyValuePattern["MessageParameters" -> {_String, "99.0+"}]]],
                MatchQ[required, Failure["DependencyManagerRequired", _]] && StringContainsQ[required["InstallCommand"], $testManagerResource]},
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {Null, True, True},
    TestID -> "Manager loader: only a release that meets the consumer's PacletPage minimum is loaded, and its install command names the consumer's address"
]

VerificationTest[
    Module[{root = CreateDirectory[], other = testUniqueName["TestBootstrapReq"], declared, stub, run, online, offline, onlineCalls, offlineCalls},
        declared = {<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.94+", "Resource" -> $testManagerResource|>, other};
        (* a manager's loader that finds a manager once one is installed *)
        stub = "If[TrueQ[WolframInstitute`PacletPage`Private`$testManagerInstalled], Null, Failure[\"DependencyManagerRequired\", <|\"MessageTemplate\" -> \"needed\"|>]]";
        run = Function[{sub, offlineQ}, Module[{calls = {}, loader = FileNameJoin[{testLoaderConsumer[root, sub, declared, stub], "LoadRequiredDependencies.wl"}], result},
            WolframInstitute`PacletPage`Private`$testManagerInstalled = False;
            result = Internal`InheritedBlock[{PacletFind}, Unprotect[PacletFind];
                PacletFind["WolframInstitute/PacletPage"] := If[TrueQ[WolframInstitute`PacletPage`Private`$testManagerInstalled],
                    {PacletObject[<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.94"|>]}, {}];
                Block[{PacletInstall = Function[Null, AppendTo[calls, {"PacletInstall", ##}]; WolframInstitute`PacletPage`Private`$testManagerInstalled = True;
                            PacletObject[<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.94"|>]],
                        ResourceObject = (testResource[#] &),
                        InstallPacletDependencies = Function[Null, AppendTo[calls, {"Install", #2}]; AssociationMap[PacletObject[<|"Name" -> #, "Version" -> "1.0"|>] &, #2]]},
                    If[offlineQ, Block[{$AllowInternet = False, PacletManager`Package`writeManagerData = (Null &)}, Get[loader]], Get[loader]]]];
            {result, calls}]];
        WithCleanup[
            {online, onlineCalls} = run["online", False];
            {offline, offlineCalls} = run["offline", True];
            {online, onlineCalls, MatchQ[offline, Failure["DependencyManagerRequired", _]], offlineCalls},
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {Null, {{"PacletInstall", testResource[$testManagerResource], ForceVersionInstall -> True}, {"Install", {"TestBootstrapReq"}}}, True, {}},
    SameTest -> (MatchQ[#1, {Null, {{"PacletInstall", testResource[$testManagerResource], ForceVersionInstall -> True}, {"Install", {_String}}}, True, {}}] &),
    TestID -> "Required dependency loader: a missing PacletPage installs from the consumer's address, the other missing ones through it, and nothing installs offline"
]

VerificationTest[
    Module[{root = CreateDirectory[], other = testUniqueName["TestAnswerReq"], result},
        WithCleanup[
            result = Block[{InstallPacletDependencies = ($Failed &)},
                Get[FileNameJoin[{testLoaderConsumer[root, "answer", {<|"Name" -> "WolframInstitute/PacletPage", "Version" -> "0.3.0+"|>, other}, "Null"],
                    "LoadRequiredDependencies.wl"}]]];
            MatchQ[result, Failure["RequiredDependencies", KeyValuePattern["Result" -> HoldForm[$Failed]]]],
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    True,
    TestID -> "Required dependency loader: a manager that does not answer with an association gives a Failure"
]

VerificationTest[
    Module[{root = CreateDirectory[], copy},
        WithCleanup[
            copy = testPaclet[root, "copy", <|"Name" -> "WolframInstitute/PacletPage", "Version" -> "9.9.9"|>];
            CreateDirectory[FileNameJoin[{copy, "Kernel"}]];
            {runningPacletPageDirectories[],
                Block[{$packageDirectory = FileNameJoin[{copy, "Kernel"}]}, {runningPacletPageDirectories[] === {copy}, MemberQ[pacletDirectories[], copy]}]},
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {{}, {True, True}},
    TestID -> "Build kernels: the running PacletPage's directory is passed on when the paclet catalog does not find it"
]

End[]
