(* Copy this file beside LoadDependencyManager.wl in the consumer's Resources directory and Get it
   while the consumer loads. It installs the consumer's missing required dependencies and nothing
   else: an optional dependency installs only from PacletDependencyDialog or an explicit call. With
   every required dependency present it returns Null without loading PacletPage, so a consumer
   that loads often pays only for reading its own PacletInfo. *)
Module[{resources = DirectoryName[$InputFileName], root, owner, declared, entry, compatibleQ, presentQ, required, missing, manager, installed, failed},
    (* the consumer directory: the nearest one above this file with a PacletInfo *)
    root = NestWhile[ParentDirectory, resources,
        StringQ[#] && ! AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, Function[file, FileExistsQ[FileNameJoin[{#, file}]]]] &&
            ParentDirectory[#] =!= # &, 1, 3];
    owner = If[StringQ[root] && AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ[FileNameJoin[{root, #}]] &], PacletObject[File[root]], None];
    If[! MatchQ[owner, _PacletObject], Return[Failure["DependencyOwner", <|
        "MessageTemplate" -> "No consumer paclet was found above `1`.", "MessageParameters" -> {resources}|>], Module]];

    declared = Replace[Lookup[owner["PacletInfo"], "Dependencies", {}], Except[_List] -> {}];
    entry = Replace[#, {name_String :> <|"Name" -> name|>, (name_String -> version_String) | {name_String, version_String} :> <|"Name" -> name, "Version" -> version|>,
        a_Association :> a, _ -> <||>}] &;
    compatibleQ = Function[{version, spec}, Which[
        ! StringQ[spec] || spec === "*", True,
        StringEndsQ[spec, ".*"], StringStartsQ[version <> ".", StringDrop[spec, -1]],
        StringEndsQ[spec, "+"], ! PacletNewerQ[StringDrop[spec, -1], version],
        True, ! PacletNewerQ[spec, version] && ! PacletNewerQ[version, spec]]];
    (* present: a compatible version is installed, or the dependency is unavailable on this system *)
    presentQ = Function[e, With[{systems = Lookup[e, "Platforms", All]},
        (ListQ[systems] && ! MemberQ[systems, $SystemID]) ||
            AnyTrue[PacletFind[e["Name"]], compatibleQ[#["Version"], Lookup[e, "Version", All]] &]]];
    required = Select[entry /@ declared, StringQ[Lookup[#, "Name"]] && ! TrueQ[Lookup[#, "Optional", False]] &];
    missing = Lookup[Select[required, ! presentQ[#] &], "Name", {}];
    If[missing === {}, Return[Null, Module]];

    manager = Get[FileNameJoin[{resources, "LoadDependencyManager.wl"}]];
    If[manager =!= Null, Return[manager, Module]];
    installed = Symbol["WolframInstitute`PacletPage`InstallPacletDependencies"][owner, missing, "CheckReadiness" -> False];
    If[! AssociationQ[installed], Return[installed, Module]];
    failed = Select[installed, ! MatchQ[#, _PacletObject] &];
    If[failed === <||>, Null, Failure["RequiredDependencies", <|
        "MessageTemplate" -> "`1` could not install its required dependencies `2`.",
        "MessageParameters" -> {owner["Name"], Keys[failed]}, "Failures" -> failed|>]]
]
