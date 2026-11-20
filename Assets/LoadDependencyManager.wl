(* Copy this loader into the consumer's Resources directory. For offline setup put a
   compatible PacletPage.paclet archive beside it. Only an explicit setup call loads it. *)
Module[{name = "WolframInstitute/PacletPage", context = "WolframInstitute`PacletPage`",
        functions, report, providesManager, loadedManager, selected, bundled, cache, staging, unpacked, file, command},
    functions = context <> # & /@ {"PacletDependencyDialog", "PacletDependencyInformation", "InstallPacletDependencies",
        "UpdatePacletDependencies", "UninstallPacletDependencies"};
    report = context <> "PacletDependencyReport";
    providesManager = Function[paclet, MatchQ[paclet, _PacletObject] && paclet["Name"] === name &&
        ContainsAll[Flatten[Cases[paclet["PacletInfo"]["Extensions"], {"Kernel", opts___} :>
            Lookup[Association[opts], "Symbols", {}]]], Append[functions, report]]];
    loadedManager = Function[Null, NameQ[report] && UpValues[Evaluate[Symbol[report]]] =!= {} &&
        AllTrue[functions, NameQ[#] && DownValues[Evaluate[Symbol[#]]] =!= {} &]];
    If[MemberQ[$Packages, context] && loadedManager[], Return[Null, Module]];

    selected = SelectFirst[PacletFind[name], providesManager, None];
    bundled = FileNameJoin[{DirectoryName[$InputFileName], "PacletPage.paclet"}];
    If[selected === None && ! (FileExistsQ[bundled] && providesManager[PacletObject[File[bundled]]]),
        command = "PacletInstall[ResourceObject[\"https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage\"], ForceVersionInstall -> True]";
        Return[Failure["DependencyManagerRequired", <|
            "MessageTemplate" -> "Setup needs a PacletPage release with dependency management. Run the install command below, then retry setup in a fresh kernel.",
            "InstallCommand" -> command|>], Module]
    ];
    If[MemberQ[$Packages, context], Return[Failure["DependencyManagerRestart", <|
        "MessageTemplate" -> "This kernel has an earlier PacletPage loaded. Restart the kernel and retry setup."|>], Module]];

    If[selected === None,
        (* Keep the consumer's archive intact. Several consumers or kernels can share the same extracted release. *)
        cache = FileNameJoin[{$CacheBaseDirectory, "WolframInstitute", "PacletPage", "DependencyManager", FileHash[bundled, "SHA256", "HexString"]}];
        If[! DirectoryQ[cache],
            staging = CreateDirectory[FileNameJoin[{DirectoryName[cache], CreateUUID["unpack-"]}], CreateIntermediateDirectories -> True];
            If[StringQ[staging], WithCleanup[
                unpacked = ExtractPacletArchive[bundled, staging];
                (* another kernel may have unpacked the same release meanwhile *)
                If[StringQ[unpacked] && ! DirectoryQ[cache], RenameDirectory[unpacked, cache]],
                DeleteDirectory[staging, DeleteContents -> True]]]
        ];
        If[! FileExistsQ[FileNameJoin[{cache, "Kernel", "PacletPage.wl"}]], Return[Failure["DependencyManagerBundle", <|
            "MessageTemplate" -> "Could not unpack the bundled dependency manager into `1`.", "MessageParameters" -> {cache}|>], Module]];
        PacletDirectoryLoad[cache];
        selected = First[PacletFind[name, <|"Location" -> cache|>], None];
        If[selected === None,
            PacletDirectoryUnload[cache];
            Return[Failure["DependencyManagerIncompatible", <|
                "MessageTemplate" -> "The bundled dependency manager is incompatible with this Wolfram system.",
                "Archive" -> bundled|>], Module]]
    ];
    file = FileNameJoin[{selected["Location"], "Kernel", "PacletPage.wl"}];
    Block[{$ContextPath = $ContextPath, $AllowInternet = False}, Needs[context, file]];
    If[loadedManager[], Null, Failure["DependencyManagerLoad", <|
        "MessageTemplate" -> "The dependency manager in `1` could not load.", "MessageParameters" -> {file}|>]]
]
