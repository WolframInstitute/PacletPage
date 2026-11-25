(* Bundled dependency assets, Required/Optional selectors, CheckReadiness,
   BuildPacletDocumentation InstallDependencies, and GitLink sparse checkout. *)
PacletDirectoryLoad[DirectoryName[$TestFileName, 2]]
Needs["WolframInstitute`PacletPage`"]

Begin["WolframInstitute`PacletPage`Private`"]

VerificationTest[
    Module[{root = CreateDirectory[], reqName, optName, reqDir, optDir, reqArc, optArc, owner,
            infoReq, infoOpt, instReq, instOpt, probes = 0, uninstOpt},
        reqName = "TestReq" <> StringDelete[CreateUUID[], "-"];
        optName = "TestOpt" <> StringDelete[CreateUUID[], "-"];
        WithCleanup[
            reqDir = CreateDirectory[FileNameJoin[{root, "req"}]];
            optDir = CreateDirectory[FileNameJoin[{root, "opt"}]];
            Put[PacletObject[<|"Name" -> reqName, "Version" -> "1.0.0"|>], FileNameJoin[{reqDir, "PacletInfo.wl"}]];
            Put[PacletObject[<|"Name" -> optName, "Version" -> "1.2.0"|>], FileNameJoin[{optDir, "PacletInfo.wl"}]];
            reqArc = CreatePacletArchive[reqDir, root];
            optArc = CreatePacletArchive[optDir, root];
            owner = PacletObject[<|
                "Name" -> "Example/SelectorConsumer", "Version" -> "1.0.0",
                "PrimaryContext" -> "Example`SelectorConsumer`",
                "Location" -> root,
                "Dependencies" -> {
                    <|"Name" -> reqName, "Archive" -> FileNameTake[reqArc], "Check" -> "Example`SelectorConsumer`ReqReadyQ"|>,
                    <|"Name" -> optName, "Optional" -> True, "Archive" -> FileNameTake[optArc], "Check" -> "Example`SelectorConsumer`OptReadyQ"|>
                }
            |>];
            infoReq = Keys[Normal[PacletDependencyInformation[owner, "Required"]]];
            infoOpt = Keys[Normal[PacletDependencyInformation[owner, "Optional"]]];
            Block[{dependencyReadiness = Function[Null, probes++; Failure["NotReady", <|"MessageTemplate" -> "Not ready"|>]]},
                instReq = InstallPacletDependencies[owner, "Required", "CheckReadiness" -> False];
                instOpt = InstallPacletDependencies[owner, "Optional", "CheckReadiness" -> True];
            ];
            uninstOpt = UninstallPacletDependencies[owner, "Optional"];
            {
                infoReq === {reqName},
                infoOpt === {optName},
                MatchQ[instReq[reqName], _PacletObject],
                MatchQ[instOpt[optName], Failure["DependencySetupRequired", _]],
                probes === 1,
                uninstOpt === <|optName -> Null|>,
                PacletFind[optName] === {} && PacletFind[reqName] =!= {}
            },
            Scan[PacletUninstall, Join[PacletFind[reqName], PacletFind[optName]]];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, True, True},
    TestID -> "Dependencies: Required/Optional selectors and CheckReadiness -> False"
]

VerificationTest[
    Module[{root = CreateDirectory[], depName, gitName, depDir, gitDir, commit, repoURL,
            consumerDir, stageDir, bundled, stagedOwner, assetDir, offlineRes, installedGit,
            existingAssetConsumer, existingStage},
        depName = "TestBundleDep" <> StringDelete[CreateUUID[], "-"];
        gitName = "TestBundleGit" <> StringDelete[CreateUUID[], "-"];
        commit = StringRepeat["c", 40];
        repoURL = "https://github.com/example/bundled-git";
        WithCleanup[
            (* Install a normal dependency and a Git-pinned dependency with receipt *)
            depDir = CreateDirectory[FileNameJoin[{root, "dep"}]];
            Put[PacletObject[<|"Name" -> depName, "Version" -> "1.1.0"|>], FileNameJoin[{depDir, "PacletInfo.wl"}]];
            PacletInstall[CreatePacletArchive[depDir, root]];

            gitDir = CreateDirectory[FileNameJoin[{root, "gitdep"}]];
            Put[PacletObject[<|"Name" -> gitName, "Version" -> "2.0.0"|>], FileNameJoin[{gitDir, "PacletInfo.wl"}]];
            Export[FileNameJoin[{gitDir, $dependencyGitReceipt}],
                <|"Git" -> "example/bundled-git", "Path" -> "Sub", "Commit" -> commit|>, "RawJSON"];
            PacletInstall[CreatePacletArchive[gitDir, root]];

            (* Consumer declaring both as required, one resource-sourced and one Git-sourced *)
            consumerDir = CreateDirectory[FileNameJoin[{root, "consumer"}]];
            Put[PacletObject[<|
                "Name" -> "Example/BundleConsumer", "Version" -> "1.0.0",
                "Dependencies" -> {
                    <|"Name" -> depName, "Version" -> "1.0+", "Resource" -> "https://www.wolframcloud.com/obj/example/DeployedResources/Paclet/" <> depName|>,
                    <|"Name" -> gitName, "Git" -> repoURL, "Commit" -> commit, "Path" -> "Sub"|>
                }
            |>], FileNameJoin[{consumerDir, "PacletInfo.wl"}]];

            stageDir = CopyDirectory[consumerDir, FileNameJoin[{root, "stage"}]];
            bundled = stageBundledDependencies[DeployPacletResource, consumerDir, stageDir, "Required"];
            stagedOwner = PacletObject[File[stageDir]];
            assetDir = stagedOwner["AssetLocation", "Dependencies"];

            (* Uninstall both dependencies and verify 100% offline install from bundled Assets/Dependencies *)
            Scan[PacletUninstall, Join[PacletFind[depName], PacletFind[gitName]]];
            offlineRes = Block[{$AllowInternet = False, PacletManager`Package`writeManagerData = (Null &)},
                InstallPacletDependencies[stagedOwner, "Required", "CheckReadiness" -> False]
            ];
            installedGit = If[MatchQ[offlineRes[gitName], _PacletObject], dependencyInstalledGit[offlineRes[gitName]], Missing["None"]];

            (* Verify merging into an existing rootless Asset extension (PacletPage style) *)
            existingAssetConsumer = CreateDirectory[FileNameJoin[{root, "existing"}]];
            Put[PacletObject[<|
                "Name" -> "Example/ExistingAsset", "Version" -> "1.0.0",
                "Dependencies" -> {<|"Name" -> depName, "Version" -> "1.0+"|>},
                "Extensions" -> {{"Asset", "Assets" -> {{"Other", "Assets/Other"}}}}
            |>], FileNameJoin[{existingAssetConsumer, "PacletInfo.wl"}]];
            existingStage = CopyDirectory[existingAssetConsumer, FileNameJoin[{root, "existing-stage"}]];
            stageBundledDependencies[DeployPacletResource, existingAssetConsumer, existingStage, "Required"];

            {
                Length[bundled] === 2,
                StringQ[assetDir] && DirectoryQ[assetDir],
                MatchQ[offlineRes[depName], _PacletObject] && offlineRes[depName]["Version"] === "1.1.0",
                MatchQ[offlineRes[gitName], _PacletObject] && offlineRes[gitName]["Version"] === "2.0.0",
                AssociationQ[installedGit] && installedGit["Commit"] === commit && installedGit["Path"] === "Sub",
                StringQ[PacletObject[File[existingStage]]["AssetLocation", "Dependencies"]]
            },
            Scan[PacletUninstall, Join[PacletFind[depName], PacletFind[gitName]]];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, True},
    TestID -> "BundleDependencies: stages required and Git-pinned .paclet assets and installs them offline"
]

VerificationTest[
    Module[{root = CreateDirectory[], reqName, optName, reqDir, reqArc, consumerDir, okAll, failReq, target, copies},
        reqName = "TestBuildReq" <> StringDelete[CreateUUID[], "-"];
        optName = "TestBuildOpt" <> StringDelete[CreateUUID[], "-"];
        WithCleanup[
            reqDir = CreateDirectory[FileNameJoin[{root, "req"}]];
            Put[PacletObject[<|"Name" -> reqName, "Version" -> "1.0.0"|>], FileNameJoin[{reqDir, "PacletInfo.wl"}]];
            reqArc = CreatePacletArchive[reqDir, root];
            consumerDir = CreateDirectory[FileNameJoin[{root, "consumer"}]];
            CopyFile[reqArc, FileNameJoin[{consumerDir, FileNameTake[reqArc]}]];
            Put[PacletObject[<|
                "Name" -> "Example/BuildDepsConsumer", "Version" -> "1.0.0",
                "Dependencies" -> {
                    <|"Name" -> reqName, "Archive" -> FileNameTake[reqArc]|>,
                    <|"Name" -> optName, "Optional" -> True, "Platforms" -> {"NonExistent-OS-99"}|>
                }
            |>], FileNameJoin[{consumerDir, "PacletInfo.wl"}]];
            target = FileNameJoin[{consumerDir, "build", "dependencies"}];
            (* the consumer's own list alone: the build tool's dependencies have a test of their own *)
            okAll = Block[{pacletPageOwner = (None &)}, installBuildDependencies[BuildPacletDocumentation, consumerDir, All]];
            copies = PacletFind[reqName];

            (* Required unsupported dependency must abort with message *)
            Put[PacletObject[<|
                "Name" -> "Example/BuildDepsFail", "Version" -> "1.0.0",
                "Dependencies" -> {<|"Name" -> optName, "Platforms" -> {"NonExistent-OS-99"}|>}
            |>], FileNameJoin[{consumerDir, "PacletInfo.wl"}]];
            failReq = Block[{pacletPageOwner = (None &)},
                Quiet[installBuildDependencies[BuildPacletDocumentation, consumerDir, "Required"], {BuildPacletDocumentation::installdeps}]];
            {okAll === True, Length[copies] === 1 && StringStartsQ[ExpandFileName[First[copies]["Location"]], ExpandFileName[target]],
                MemberQ[ExpandFileName /@ PacletDirectoryLoad[], ExpandFileName[target]], failReq === $Failed},
            PacletDirectoryUnload[target];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True},
    TestID -> "BuildPacletDocumentation: InstallDependencies installs required deps into build/dependencies, tolerates unsupported optional deps, and fails on required errors"
]

VerificationTest[
    Module[{root = CreateDirectory[], srcDir, targetDir, rootTarget, sig, r, c, checkedSub, checkedRoot,
            canonGH, canonOther},
        canonGH = dependencyGitCanonical["https://github.com/WolframInstitute/PacletPage.git/"];
        canonOther = dependencyGitCanonical["https://gitlab.example.org/Group/Repo.git/"];
        WithCleanup[
            If[dependencyGitLinkAvailableQ[],
                sig = <|"Name" -> "Test User", "Email" -> "test@example.org",
                    "TimeStamp" -> DateObject[{2026, 1, 1, 0, 0, 0}, TimeZone -> 0]|>;
                srcDir = FileNameJoin[{root, "src"}];
                r = WolframInstitute`GitLink`GitInit[srcDir];
                CreateDirectory[FileNameJoin[{srcDir, "PkgA"}]];
                CreateDirectory[FileNameJoin[{srcDir, "Other"}]];
                Export[FileNameJoin[{srcDir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Demo/Root\", \"Version\" -> \"1.0\"|>]", "Text"];
                Export[FileNameJoin[{srcDir, "PkgA", "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Demo/PkgA\", \"Version\" -> \"1.0\"|>]", "Text"];
                Export[FileNameJoin[{srcDir, "Other", "skip.txt"}], "skip", "Text"];
                WolframInstitute`GitLink`GitAdd[r, All];
                c = WolframInstitute`GitLink`GitCommit[r, "init", "AuthorSignature" -> sig, "CommitterSignature" -> sig];
                targetDir = FileNameJoin[{root, "sparse-sub"}];
                checkedSub = dependencyGitLinkCheckout["file://" <> srcDir, WolframInstitute`GitLink`GitSHA[c], {"PkgA"}, targetDir];
                rootTarget = FileNameJoin[{root, "sparse-root"}];
                checkedRoot = dependencyGitLinkCheckout["file://" <> srcDir, WolframInstitute`GitLink`GitSHA[c], {}, rootTarget];
                {
                    canonGH === "wolframinstitute/pacletpage",
                    canonOther === "https://gitlab.example.org/group/repo",
                    dependencyGitURLQ["https://gitlab.example.org/Group/Repo.git"],
                    AssociationQ[checkedSub] && checkedSub["Commit"] === WolframInstitute`GitLink`GitSHA[c],
                    FileExistsQ[FileNameJoin[{targetDir, "PkgA", "PacletInfo.wl"}]] && ! FileExistsQ[FileNameJoin[{targetDir, "Other", "skip.txt"}]],
                    AssociationQ[checkedRoot] && ! DirectoryQ[FileNameJoin[{rootTarget, ".git"}]]
                },
                {
                    canonGH === "wolframinstitute/pacletpage",
                    canonOther === "https://gitlab.example.org/group/repo",
                    dependencyGitURLQ["https://gitlab.example.org/Group/Repo.git"],
                    True, True, True
                }
            ],
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, True},
    TestID -> "GitLink sparse checkout: canonical URLs, subdirectory sparse checkout, and .git stripping for root paclets"
]

VerificationTest[
    With[{info = PacletDependencyInformation[File[DirectoryName[$TestFileName, 2]], "Output" -> "Association"]},
        {Keys[info], info["WolframInstitute/GitLink"]["Required"], info["WolframInstitute/GitLink"]["Version"],
            info["Wolfram/Parser"]["Required"], info["Wolfram/Parser"]["Version"], KeyTake[info["Wolfram/Parser"]["Pin"], {"Git", "Path"}]}
    ],
    {{"WolframInstitute/GitLink", "Wolfram/Parser"}, False, "2.0+", True, "1.9.13+", <|"Git" -> "https://github.com/sw1sh/WolframParser", "Path" -> "Parser"|>},
    TestID -> "PacletPage declares GitLink as optional and the converter's parser as required, pinned for builds"
]

End[]
