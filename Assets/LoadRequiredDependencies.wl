(* Copy this file beside LoadDependencyManager.wl in the consumer's Resources directory and Get it
   while the consumer loads. It installs the consumer's missing required dependencies and nothing
   else: an optional dependency installs only from PacletDependencyDialog or an explicit call. With
   every required dependency present it returns Null without loading PacletPage, so a consumer
   that loads often pays only for reading its own PacletInfo. A missing PacletPage, which the
   Wolfram System's PacletInstall never installs for the consumer, is installed from the
   consumer's own declaration of WolframInstitute/PacletPage. *)
Module[{manager = "WolframInstitute/PacletPage", resources = DirectoryName[$InputFileName], infoQ, root, owner, entries, compatibleQ, presentQ,
        required, missing, own, loaded, bootstrap, installed, failed},
    (* the consumer directory: the nearest one above this file with a PacletInfo *)
    infoQ = Function[dir, StringQ[dir] && AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ[FileNameJoin[{dir, #}]] &]];
    root = NestWhile[ParentDirectory, resources, ! infoQ[#] && StringQ[#] && ParentDirectory[#] =!= # &, 1, 3];
    owner = If[infoQ[root], PacletObject[File[root]], None];
    If[! MatchQ[owner, _PacletObject], Return[Failure["DependencyOwner", <|
        "MessageTemplate" -> "No consumer paclet was found above `1`.", "MessageParameters" -> {resources}|>], Module]];

    entries = Replace[Replace[Lookup[owner["PacletInfo"], "Dependencies", {}], Except[_List] -> {}], {
        name_String :> <|"Name" -> name|>, (name_String -> version_String) | {name_String, version_String} :> <|"Name" -> name, "Version" -> version|>,
        a_Association :> a, _ -> <||>}, {1}];
    compatibleQ = Function[{version, spec}, Which[
        ! StringQ[spec] || spec === "*", True,
        StringEndsQ[spec, ".*"], StringStartsQ[version <> ".", StringDrop[spec, -1]],
        StringEndsQ[spec, "+"], ! PacletNewerQ[StringDrop[spec, -1], version],
        True, ! PacletNewerQ[spec, version] && ! PacletNewerQ[version, spec]]];
    (* present: a compatible version is installed, or the dependency is unavailable on this system *)
    presentQ = Function[e, With[{systems = Lookup[e, "Platforms", All]},
        (ListQ[systems] && ! MemberQ[systems, $SystemID]) ||
            AnyTrue[PacletFind[e["Name"]], compatibleQ[#["Version"], Lookup[e, "Version", All]] &]]];
    required = Select[entries, StringQ[Lookup[#, "Name"]] && ! TrueQ[Lookup[#, "Optional", False]] &];
    missing = Lookup[Select[required, ! presentQ[#] &], "Name", {}];
    If[missing === {}, Return[Null, Module]];

    (* PacletPage itself, from the address the consumer declares, once a kernel at a time on this user
       base: kernels that install the same paclet at once can crash, and the one that waited finds it *)
    own = SelectFirst[required, Lookup[#, "Name"] === manager &, <||>];
    bootstrap = Function[failure, If[$AllowInternet === False || ! StringQ[Lookup[own, "Resource", None]], failure,
        If[! DirectoryQ[$UserBasePacletsDirectory], CreateDirectory[$UserBasePacletsDirectory, CreateIntermediateDirectories -> True]];
        Replace[WithLock[File[FileNameJoin[{$UserBasePacletsDirectory, ".pacletpage-manager.lock"}]],
            If[presentQ[own], Null, Block[{CloudConnect = ($Failed &)},
                TimeConstrained[PacletInstall[ResourceObject[own["Resource"]], ForceVersionInstall -> True], 300, $TimedOut]]],
            TimeConstraint -> 330, PersistenceTime -> 600], {
            Null :> Null,
            p_PacletObject /; p["Name"] === manager && compatibleQ[p["Version"], Lookup[own, "Version", All]] :> Null,
            other_ :> Failure["DependencyManagerInstall", <|
                "MessageTemplate" -> "Could not install PacletPage from `1`.", "MessageParameters" -> {own["Resource"]},
                "Result" -> HoldForm[other], "Cause" -> failure|>]}]]];

    loaded = Get[FileNameJoin[{resources, "LoadDependencyManager.wl"}]];
    If[MatchQ[loaded, Failure["DependencyManagerRequired", _]] && MemberQ[missing, manager],
        With[{installedManager = bootstrap[loaded]},
            If[installedManager =!= Null, Return[installedManager, Module]]];
        missing = DeleteCases[missing, manager];
        loaded = Get[FileNameJoin[{resources, "LoadDependencyManager.wl"}]]];
    If[loaded =!= Null, Return[loaded, Module]];
    If[missing === {}, Return[Null, Module]];

    installed = Symbol["WolframInstitute`PacletPage`InstallPacletDependencies"][owner, missing, "CheckReadiness" -> False];
    If[! AssociationQ[installed], Return[With[{result = installed}, Failure["RequiredDependencies", <|
        "MessageTemplate" -> "`1` could not install its required dependencies `2`: the dependency manager did not answer with their results.",
        "MessageParameters" -> {owner["Name"], missing}, "Result" -> HoldForm[result]|>]], Module]];
    failed = Select[installed, ! MatchQ[#, _PacletObject] &];
    If[failed === <||>, Null, Failure["RequiredDependencies", <|
        "MessageTemplate" -> "`1` could not install its required dependencies `2`.",
        "MessageParameters" -> {owner["Name"], Keys[failed]}, "Failures" -> failed|>]]
]
