(* Copy this file into a consumer's Resources directory and Get it from its setup command.
   An offline distribution can put an unpacked PacletPage release beside it in PacletPage/. *)
Module[{name = "WolframInstitute/PacletPage", context = "WolframInstitute`PacletPage`",
        functions, providesManager, loadedManager, selected, bundled, file, command},
    functions = context <> # & /@ {"PacletDependencyDialog", "PacletDependencyInformation", "InstallPacletDependencies",
        "UpdatePacletDependencies", "UninstallPacletDependencies"};
    providesManager = Function[paclet, MatchQ[paclet, _PacletObject] && paclet["Name"] === name &&
        ContainsAll[Flatten[Cases[paclet["PacletInfo"]["Extensions"], {"Kernel", opts___} :>
            Lookup[Association[opts], "Symbols", {}]]], functions]];
    loadedManager = Function[Null, AllTrue[functions, NameQ[#] && ToExpression[#, InputForm, DownValues] =!= {} &]];
    If[MemberQ[$Packages, context] && loadedManager[], Return[Null, Module]];

    selected = SelectFirst[PacletFind[name], providesManager, None];
    If[selected === None,
        bundled = FileNameJoin[{DirectoryName[$InputFileName], "PacletPage"}];
        If[FileExistsQ[FileNameJoin[{bundled, "PacletInfo.wl"}]],
            With[{paclet = PacletObject[File[bundled]]},
                If[providesManager[paclet], selected = paclet; PacletDirectoryLoad[bundled]]]]
    ];
    If[selected === None,
        command = "PacletInstall[ResourceObject[\"https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/PacletPage\"], ForceVersionInstall -> True]";
        Return[Failure["DependencyManagerRequired", <|
            "MessageTemplate" -> "Setup needs a PacletPage release with dependency management. Run `1`, then retry setup in a fresh kernel.",
            "MessageParameters" -> {command}, "InstallCommand" -> command|>], Module]
    ];
    If[MemberQ[$Packages, context], Return[Failure["DependencyManagerRestart", <|
        "MessageTemplate" -> "This kernel has an earlier PacletPage loaded. Restart the kernel and retry setup."|>], Module]];

    (* A registered development copy can have the same version as an older installation. *)
    file = FileNameJoin[{selected["Location"], "Kernel", "PacletPage.wl"}];
    Block[{$ContextPath = $ContextPath, $AllowInternet = False}, Needs[context, file]];
    If[loadedManager[], Null, Failure["DependencyManagerLoad", <|
        "MessageTemplate" -> "The dependency manager in `1` could not load.", "MessageParameters" -> {file}|>]]
]
