(* A commit can change a library without changing its numeric version. *)
PacletDirectoryLoad[DirectoryName[$TestFileName, 2]]
Needs["WolframInstitute`PacletPage`"]

Begin["WolframInstitute`PacletPage`Private`"]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"], a, b, repo, owner, revision, trees,
            tree, seed, check, before, failed, aborted, restored, updated, repeated, status, mismatch, removed, cached, loaded, text},
        {a, b} = StringRepeat[#, 40] & /@ {"a", "b"};
        repo = "https://github.com/example/library";
        owner[commit_, url_: repo, path_: "Library"] := PacletObject[<|"Name" -> "Example/Consumer", "Version" -> "1.0",
            "Dependencies" -> {<|"Name" -> name, "Optional" -> True, "Git" -> url, "Commit" -> commit, "Path" -> path|>}|>];
        text = ToString[PacletObject[<|"Name" -> name, "Version" -> "1.0", "Extensions" -> {{"Kernel", "Context" -> "TestGitPin`"}}|>], InputForm];
        tree[commit_] := Module[{source = CreateDirectory[FileNameJoin[{root, "repo-" <> commit, "Library"}]]},
            Export[FileNameJoin[{source, "PacletInfo.wl"}], text, "Text"];
            Export[FileNameJoin[{source, "marker.txt"}], commit, "Text"];
            If[commit === a, Export[FileNameJoin[{source, "old-only.txt"}], "retired", "Text"]];
            CreateArchive[DirectoryName[source], FileNameJoin[{root, commit <> ".zip"}]]];
        revision[] := Module[{path = First[PacletFind[name]]["Location"]}, {
            Import[FileNameJoin[{path, "marker.txt"}], "Text"], FileExistsQ[FileNameJoin[{path, "old-only.txt"}]]}];
        WithCleanup[
            trees = AssociationMap[tree, {a, b}];
            seed = CreateDirectory[FileNameJoin[{root, "seed"}]];
            Export[FileNameJoin[{seed, "PacletInfo.wl"}], text, "Text"];
            PacletInstall[CreatePacletArchive[seed, root]];
            Block[{$AllowInternet = True, $dependencyRestarts = <||>, dependencyGitText = (text &),
                    dependencyDownloaded = Function[{url, file}, CopyFile[trees[Last[StringSplit[url, "/"]]], file]; File[file]]},
                before = PacletDependencyInformation[owner[a], "CheckUpdates" -> True][name];
                check = InstallPacletDependencies[owner[a], name][name];
                cached = Block[{$AllowInternet = False, dependencyGitText = (Throw["Unexpected lookup"] &)}, {
                    InstallPacletDependencies[owner[StringTake[a, 7], "https://github.com/Example/Library"], name][name],
                    UpdatePacletDependencies[owner[a, repo <> ".git/"], name][name],
                    PacletDependencyInformation[owner[a], "CheckUpdates" -> True][name]["LatestVersion"]}];
                mismatch = Table[PacletDependencyInformation[other][name]["GitRevisionMatch"],
                    {other, {owner[a, repo <> "2"], owner[a, repo, "Other"], owner[b]}}];
                failed = Block[{PacletInstall = ($Failed &)}, UpdatePacletDependencies[owner[b], name][name]];
                aborted = CheckAbort[Block[{PacletInstall = (Abort[] &)}, UpdatePacletDependencies[owner[b], name]]; False, True];
                restored = revision[];
                Block[{$Packages = Append[$Packages, "TestGitPin`"]}, updated = UpdatePacletDependencies[owner[b], name][name]];
                status = PacletDependencyInformation[owner[b]][name]; loaded = revision[];
                repeated = Block[{$AllowInternet = False}, UpdatePacletDependencies[owner[b], name][name]];
                removed = UninstallPacletDependencies[owner[b], name][name]];
            {{before["Status"], before["GitRevisionMatch"]}, MemberQ[dependencyRowActions[before], "Update"],
                MatchQ[check, _PacletObject], MatchQ[cached, {_PacletObject, _PacletObject, "1.0"}], mismatch,
                FailureQ[failed], aborted, restored === {a, True}, MatchQ[updated, _PacletObject], loaded === {b, False},
                {status["Status"], status["GitRevisionMatch"], status["InstalledGitSource"]["Commit"] === b},
                MatchQ[repeated, _PacletObject], removed, PacletFind[name]},
            Scan[PacletUninstall, PacletFind[name]]; DeleteDirectory[root, DeleteContents -> True]]],
    {{"Revision mismatch", False}, True, True, True, {False, False, False}, True, True, True, True, True, {"Restart required", True, True}, True, Null, {}},
    TestID -> "Git pins: reconcile unknown and same-version sources, retain the prior tree on failure, reuse the verified pin offline"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"], external, commit = StringRepeat["a", 40],
            results, owner, row, probes = 0},
        WithCleanup[
            external = CreateDirectory[FileNameJoin[{root, "checkout"}]];
            Put[PacletObject[<|"Name" -> name, "Version" -> "2.0"|>], FileNameJoin[{external, "PacletInfo.wl"}]];
            PacletDirectoryLoad[external];
            owner = PacletObject[<|"Name" -> "Example/Consumer", "Version" -> "1.0", "PrimaryContext" -> "Example`Consumer`",
                "Dependencies" -> {<|"Name" -> name, "Optional" -> True, "Git" -> "https://github.com/example/library",
                    "Commit" -> commit, "Check" -> "Example`Consumer`ReadyQ"|>}|>];
            Block[{dependencyReadiness = Function[Null, probes++; True], dependencyRelease = (<|"Version" -> "2.0"|> &)},
                row = PacletDependencyInformation[owner, "CheckReadiness" -> True, "CheckUpdates" -> True][name]];
            results = Table[Block[{dependencyRelease = (<|"Version" -> version|> &),
                    dependencyInstallRelease = (Throw["Unexpected download"] &)},
                Catch[First[UpdatePacletDependencies[owner, name][name]]]], {version, {"2.0", "1.0"}}];
            {results, DirectoryQ[external], row["Status"], probes, dependencyRowActions[row], dependencyCheckAvailability[<|name -> row|>]["Readiness"]},
            PacletDirectoryUnload[external]; DeleteDirectory[root, DeleteContents -> True]]],
    {{"ExternalInstallation", "DependencyVersionConflict"}, True, "Revision mismatch", 0, {}, False},
    TestID -> "Git pins: an external installation and a newer active version are protected"
]

VerificationTest[
    Module[{root = CreateDirectory[], name = "Test" <> StringDelete[CreateUUID[], "-"], file, original, location, backup, result},
        WithCleanup[
            Put[PacletObject[<|"Name" -> name, "Version" -> "1.0"|>], FileNameJoin[{root, "PacletInfo.wl"}]];
            Export[FileNameJoin[{root, "original.txt"}], "original", "Text"];
            file = File[CreatePacletArchive[root]];
            original = PacletInstall[file]; location = original["Location"];
            result = Block[{PacletInstall = Function[Null,
                    CreateDirectory[location]; Export[FileNameJoin[{location, "other.txt"}], "other writer", "Text"]; $Failed]},
                dependencyCommitGitFile[file, name, "1.0", <|"Git" -> "example/library", "Path" -> "", "Commit" -> StringRepeat["a", 40]|>]];
            backup = Last[result["MessageParameters"]];
            {First[result], Import[FileNameJoin[{location, "other.txt"}], "Text"], Import[FileNameJoin[{backup, "original.txt"}], "Text"]},
            If[StringQ[backup] && DirectoryQ[backup],
                If[DirectoryQ[location], DeleteDirectory[location, DeleteContents -> True]]; RenameDirectory[backup, location]];
            Scan[PacletUninstall, PacletFind[name]]; If[FileExistsQ[file], DeleteFile[file]];
            DeleteDirectory[root, DeleteContents -> True]]],
    {"DependencyInstallFailed", "other writer", "original"},
    TestID -> "Git pins: rollback preserves a destination claimed by another writer and reports the recoverable original"
]

(* The lock of a kernel that has gone names a process that no longer runs: 2147483646 is above every system's process IDs. *)
VerificationTest[
    Module[{name = "Demo/Locked" <> StringDelete[CreateUUID[], "-"], file, text, abandon, busy, gone, elsewhere, unreadable},
        file = FileNameJoin[{$UserBasePacletsDirectory, ".git-pin-" <> IntegerString[Hash[name, "SHA256"], 16] <> ".lock"}];
        abandon[machine_] := Export[file, StringReplace[text, {
            ("\"ProcessID\" -> " ~~ DigitCharacter ..) -> "\"ProcessID\" -> 2147483646",
            ("\"MachineName\" -> \"" ~~ Shortest[__] ~~ "\"") -> "\"MachineName\" -> " <> ToString[machine, InputForm]}], "Text"];
        WithCleanup[
            Block[{$dependencyLockWait = 0.3},
                With[{lock = File[file]}, busy = WithLock[lock, text = ReadString[file]; withDependencyGitLock[name, "ran"]]];
                abandon[$MachineName]; gone = {withDependencyGitLock[name, "ran"], FileExistsQ[file]};
                abandon["another-machine"]; elsewhere = {First[withDependencyGitLock[name, "ran"]], FileExistsQ[file]};
                Export[file, "not a lock", "Text"]; unreadable = dependencyLockAbandonedQ[file]];
            {First[busy], gone, elsewhere, unreadable},
            If[FileExistsQ[file], DeleteFile[file]]]],
    {"DependencyBusy", {"ran", False}, {"DependencyBusy", True}, False},
    TestID -> "Git pins: an operation waits for a live holder, takes over the lock of a kernel that has gone, and leaves another machine's alone"
]

End[]
