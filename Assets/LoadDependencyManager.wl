(* Copy this loader into the consumer's Resources directory. For offline setup put a
   compatible PacletPage.paclet archive beside it. Only an explicit setup call loads it. The
   consumer's own declaration of WolframInstitute/PacletPage, when it has one, gives the oldest
   release it accepts and the address the install command names. *)
Module[{name = "WolframInstitute/PacletPage", context = "WolframInstitute`PacletPage`", infoQ, root, declared, spec, resource,
        compatibleQ, functions, report, providesManager, loadedManager, loadedVersion, selected, bundled, cache, staging, unpacked, file, command},
    functions = context <> # & /@ {"PacletDependencyDialog", "PacletDependencyInformation", "InstallPacletDependencies",
        "UpdatePacletDependencies", "UninstallPacletDependencies"};
    report = context <> "PacletDependencyReport";

    (* the consumer: the nearest directory above this file with a PacletInfo, and its entry for the manager *)
    infoQ = Function[dir, StringQ[dir] && AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ[FileNameJoin[{dir, #}]] &]];
    root = NestWhile[ParentDirectory, DirectoryName[$InputFileName], ! infoQ[#] && StringQ[#] && ParentDirectory[#] =!= # &, 1, 3];
    declared = If[infoQ[root],
        FirstCase[
            Replace[Replace[Lookup[PacletObject[File[root]]["PacletInfo"], "Dependencies", {}], Except[_List] -> {}], {
                n_String :> <|"Name" -> n|>, (n_String -> v_String) | {n_String, v_String} :> <|"Name" -> n, "Version" -> v|>,
                a_Association :> a, _ -> <||>}, {1}],
            a_Association /; Lookup[a, "Name"] === name, <||>],
        <||>];
    spec = Lookup[declared, "Version", All];
    resource = Lookup[declared, "Resource", "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage"];
    compatibleQ = Function[version, Which[
        ! StringQ[spec] || spec === "*", True,
        ! StringQ[version], False,
        StringEndsQ[spec, ".*"], StringStartsQ[version <> ".", StringDrop[spec, -1]],
        StringEndsQ[spec, "+"], ! PacletNewerQ[StringDrop[spec, -1], version],
        True, ! PacletNewerQ[spec, version] && ! PacletNewerQ[version, spec]]];

    providesManager = Function[paclet, MatchQ[paclet, _PacletObject] && paclet["Name"] === name && compatibleQ[paclet["Version"]] &&
        ContainsAll[Flatten[Cases[paclet["PacletInfo"]["Extensions"], {"Kernel", opts___} :>
            Lookup[Association[opts], "Symbols", {}]]], Append[functions, report]]];
    loadedManager = Function[Null, NameQ[report] && UpValues[Evaluate[Symbol[report]]] =!= {} &&
        AllTrue[functions, NameQ[#] && DownValues[Evaluate[Symbol[#]]] =!= {} &]];
    (* the release this kernel loaded, from the directory its package was read from *)
    loadedVersion = If[NameQ[context <> "Private`$packageDirectory"],
        With[{dir = Symbol[context <> "Private`$packageDirectory"]},
            If[StringQ[dir] && infoQ[ParentDirectory[dir]], PacletObject[File[ParentDirectory[dir]]]["Version"], None]],
        None];
    If[MemberQ[$Packages, context] && loadedManager[],
        Return[If[loadedVersion === None || compatibleQ[loadedVersion], Null, Failure["DependencyManagerRestart", <|
            "MessageTemplate" -> "This kernel has PacletPage `1` loaded and the consumer needs `2`. Restart the kernel and retry setup.",
            "MessageParameters" -> {loadedVersion, spec}|>]], Module]];

    selected = SelectFirst[PacletFind[name], providesManager, None];
    bundled = FileNameJoin[{DirectoryName[$InputFileName], "PacletPage.paclet"}];
    If[selected === None && ! (FileExistsQ[bundled] && providesManager[PacletObject[File[bundled]]]),
        command = "PacletInstall[ResourceObject[\"" <> resource <> "\"], ForceVersionInstall -> True]";
        Return[Failure["DependencyManagerRequired", <|
            "MessageTemplate" -> "Setup needs a PacletPage release with dependency management. Run the install command below, then retry setup in a fresh kernel.",
            "InstallCommand" -> command, "Resource" -> resource, "Version" -> spec|>], Module]
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
