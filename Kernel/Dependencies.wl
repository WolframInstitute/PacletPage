(* ::Package:: *)

(* Reading a manifest neither loads the consumer's code nor contacts its release server. *)
dependencyFailure[tag_, message_, args___] := Failure[tag, <|"MessageTemplate" -> message, "MessageParameters" -> {args}|>]
dependencyNameQ[name_] := StringQ[name] && (pacletNameQ[name] || StringMatchQ[name, RegularExpression["[A-Za-z0-9_-]+"]])
dependencyVersionQ[v_] := StringQ[v] && StringMatchQ[v, DigitCharacter .. ~~ ("." ~~ DigitCharacter ..) ...]
dependencyVersionSpecQ[version_] := version === All || (StringQ[version] &&
    (version === "*" || dependencyVersionQ[StringReplace[version, ("+" | ".*") ~~ EndOfString -> ""]]))
dependencyCompatibleQ[_, All | "*"] := True
dependencyCompatibleQ[version_String, spec_String] := Which[
    StringEndsQ[spec, ".*"], StringStartsQ[version <> ".", StringDrop[spec, -1]],
    StringEndsQ[spec, "+"], ! PacletNewerQ[StringDrop[spec, -1], version],
    True, ! PacletNewerQ[spec, version] && ! PacletNewerQ[version, spec]
]

dependencyOwner[p_PacletObject] := p
dependencyOwner[File[path_String]] := If[DirectoryQ[path] && AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ[FileNameJoin[{path, #}]] &],
    PacletObject[File[path]],
    dependencyFailure["DependencyOwner", "No paclet directory was found at `1`.", path]]
dependencyOwner[name_String] := Which[
    DirectoryQ[name], dependencyOwner[File[name]],
    dependencyNameQ[name], First[PacletFind[name], dependencyFailure["DependencyOwner", "The paclet `1` could not be found locally.", name]],
    True, dependencyFailure["DependencyOwner", "Give an installed paclet name or a paclet directory."]]

dependencyEntry[name_String] := <|"Name" -> name|>
dependencyEntry[(name_String -> version_String) | {name_String, version_String}] := <|"Name" -> name, "Version" -> version|>
dependencyEntry[entry_Association] := entry
dependencyEntry[_] := <||>

dependencyHTTPQ[url_] := StringQ[url] && StringMatchQ[url, "https://" ~~ Except[WhitespaceCharacter] ..]
dependencyTextMapQ[value_] := AssociationQ[value] && AllTrue[Keys[value], StringQ] && AllTrue[Values[value], StringQ]

dependencyEntryQ[entry_, owner_] := With[{
    name = Lookup[entry, "Name", None], version = Lookup[entry, "Version", All],
    platforms = Lookup[entry, "Platforms", All], check = Lookup[entry, "Check", None],
    links = Lookup[entry, "SetupLinks", <||>], size = Lookup[entry, "DownloadSize", ""]
},
    ContainsAll[{"Name", "Required", "Optional", "Version", "Platforms", "Description", "Check", "Resource", "Archive", "Git", "Commit", "Path",
        "DownloadSize", "License", "URL", "SetupLinks"}, Keys[entry]] &&
    BooleanQ[Lookup[entry, "Optional", False]] &&
    dependencyNameQ[name] && name =!= owner["Name"] && dependencyVersionSpecQ[version] &&
    (platforms === All || MatchQ[platforms, {___String}]) &&
    AllTrue[Values[KeyTake[entry, {"Description", "License"}]], StringQ] &&
    AllTrue[Values[KeyTake[entry, {"URL", "Resource"}]], dependencyHTTPQ] &&
    dependencyTextMapQ[links] && AllTrue[Values[links], dependencyHTTPQ] &&
    (StringQ[size] || dependencyTextMapQ[size]) &&
    (! KeyExistsQ[entry, "Archive"] || (StringQ[entry["Archive"]] && entry["Archive"] =!= "" && ! KeyExistsQ[entry, "Resource"])) &&
    (* a repository at a commit is a source of its own, and a commit or a path names nothing without one *)
    If[KeyExistsQ[entry, "Git"],
        dependencyGitURLQ[entry["Git"]] && dependencyCommitQ[Lookup[entry, "Commit", None]] &&
            dependencyGitPathQ[Lookup[entry, "Path", ""]] && ! KeyExistsQ[entry, "Resource"] && ! KeyExistsQ[entry, "Archive"],
        ! KeyExistsQ[entry, "Commit"] && ! KeyExistsQ[entry, "Path"]] &&
    (check === None || (StringQ[check] && StringMatchQ[check, RegularExpression["([a-zA-Z$][a-zA-Z0-9$]*`)+[a-zA-Z$][a-zA-Z0-9$]*"]] &&
        AnyTrue[Cases[Flatten[{owner["PrimaryContext"], owner["Context"]}], _String], StringStartsQ[check, #] &]))
]

(* The paclet's own "Dependencies" list: a dependency is required unless its declaration says
   "Optional" -> True *)
dependencyManifest[owner_PacletObject] := Module[{declared = Lookup[owner["PacletInfo"], "Dependencies", {}], entries, names},
    If[! ListQ[declared], Return[dependencyFailure["DependencyManifest", "Dependencies must be a list."]]];
    entries = With[{entry = dependencyEntry[#]}, Append[entry, "Required" -> ! TrueQ[Lookup[entry, "Optional", False]]]] & /@ declared;
    With[{invalid = SelectFirst[entries, ! dependencyEntryQ[#, owner] &, None]},
        If[invalid =!= None, Return[dependencyFailure["DependencyManifest", "Invalid dependency declaration in `1`: `2`.", owner["Name"], invalid]]]];
    names = #["Name"] & /@ entries;
    If[! DuplicateFreeQ[names], Return[dependencyFailure["DependencyManifest", "Each dependency in `1` must have a unique name.", owner["Name"]]]];
    AssociationThread[names, entries]
]

(* Resolve All, "Required", "Optional", a single name, or a list of names against the manifest. *)
dependencySelectionNames[entries_Association, selection_] := Which[
    selection === All, Keys[entries],
    selection === "Required" && ! KeyExistsQ[entries, "Required"],
        Keys[Select[entries, TrueQ[#["Required"]] &]],
    selection === "Optional" && ! KeyExistsQ[entries, "Optional"],
        Keys[Select[entries, ! TrueQ[#["Required"]] &]],
    True, DeleteDuplicates[Flatten[{selection}]]
]

(* Validate the whole selection before its first action. Each library then keeps its own result. *)
withDependencySelection[ownerSpec_, selection_, action_] := Module[{owner = dependencyOwner[ownerSpec], entries, names, unknown},
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    entries = dependencyManifest[owner];
    If[FailureQ[entries], Return[entries]];
    names = dependencySelectionNames[entries, selection];
    unknown = Complement[names, Keys[entries]];
    If[unknown =!= {}, Return[dependencyFailure["UnknownDependency", "`1` does not declare `2`.", owner["Name"], unknown]]];
    Block[{$refreshDependencySites = True}, action[owner, AssociationThread[names, Lookup[entries, names]]]]
]

dependencyMap[action_, ownerSpec_, selection_, filter_: Identity] := withDependencySelection[ownerSpec, selection,
    Function[{owner, entries}, If[dependencyCancelledQ[], dependencyCancelled[],
        If[KeyExistsQ[#, "Git"], withDependencyGitLock[#["Name"], action[owner, #]], action[owner, #]]] & /@ filter[entries]]]

(* Exact-name lookups are cheaper for a small selection. For a long one the paclet manager can
   enumerate the same local index once. Keep its own version and directory precedence in each group. *)
dependencyInstallations[names_List] := If[Length[names] < 16, AssociationMap[PacletFind, names],
    With[{byName = GroupBy[PacletFind[], #["Name"] &]}, AssociationThread[names, Lookup[byName, names, {}]]]
]

dependencySupportedQ[entry_] := With[{systems = Lookup[entry, "Platforms", All]}, systems === All || MemberQ[systems, $SystemID]]

(* A loaded context can outlive both an upgrade and an uninstall followed by a reinstall. *)
If[! AssociationQ[$dependencyRestarts], $dependencyRestarts = <||>]
dependencyLoadedQ[p_PacletObject] := ! DisjointQ[$Packages, Flatten[{p["Context"]}]]

(* Capture both stages together. EvaluationData otherwise converts an escaping Throw into
   an uncaught-throw message, even when the caller has a matching Catch. *)
dependencyReadiness[owner_, entry_] := Module[{check = entry["Check"], context, run, completed, thrown, result, failure, messages},
    run = EvaluationData[Catch[Catch[completed[Block[{$ContextPath = $ContextPath},
        If[! NameQ[check],
            context = SelectFirst[Flatten[{owner["PrimaryContext"], owner["Context"]}],
                StringQ[#] && StringStartsQ[check, #] &, None];
            If[StringQ[context], Needs[context]]];
        If[NameQ[check], Symbol[check][],
            Failure["DependencyCheckUnavailable", <|"MessageTemplate" -> "The readiness function `1` could not be loaded.",
                "MessageParameters" -> {check}, "Messages" -> {}|>]]]], _, thrown]]];
    result = Replace[run["Result"], {completed[value_] :> value,
        thrown[value_, tag_] :> Throw[value, tag], value_ :> Throw[value]}];
    If[result === True, Return[True]];
    (* Only a Failure with its association is the consumer's own diagnostic: $Failed is a failure too, with nothing to keep. *)
    failure = Replace[result, Except[Failure[_, _Association]] :> Failure["DependencyCheckFailed", <|
        "MessageTemplate" -> "The readiness check `1` did not succeed.", "MessageParameters" -> {check}, "Messages" -> {}|>]];
    messages = run["MessagesText"];
    If[messages === {}, failure, MapAt[Function[data, Append[data, With[{own = Lookup[data, "Messages", {}]},
        If[ListQ[own], "Messages" -> Join[own, messages], "ReadinessMessages" -> messages]]]], failure, 2]]
]

dependencyLocalStatus[entry_, installed_, git_: Automatic] := Which[
    ! dependencySupportedQ[entry], "Unsupported",
    installed === {}, "Not installed",
    KeyExistsQ[$dependencyRestarts, entry["Name"]], "Restart required",
    ! dependencyCompatibleQ[First[installed]["Version"], Lookup[entry, "Version", All]], "Version mismatch",
    KeyExistsQ[entry, "Git"] && ! dependencyGitMatchesQ[Replace[git, Automatic :> dependencyInstalledGit[First[installed]]], entry], "Revision mismatch",
    True, "Installed"
]

$dependencyVersionTimeout = 10
Options[PacletDependencyInformation] = {"CheckReadiness" -> False, "CheckUpdates" -> False, "Output" -> "Report"}

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject)] :=
    PacletDependencyInformation[owner, All]

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject),
        first : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ..}), rest : OptionsPattern[]] :=
    PacletDependencyInformation[owner, All, first, rest]

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String}), OptionsPattern[]] :=
    With[{probe = TrueQ[OptionValue["CheckReadiness"]], updates = TrueQ[OptionValue["CheckUpdates"]], output = OptionValue["Output"]},
        If[! MemberQ[{"Report", "Association"}, output], Return[dependencyFailure["DependencyOutput", "Choose Report or Association for the output."]]];
        withDependencySelection[owner, names, Function[{paclet, entries}, Module[{installed, rows},
            installed = dependencyInstallations[Keys[entries]];
            rows = If[dependencyCancelledQ[], dependencyCancelled[], dependencyInfo[paclet, #, installed[#["Name"]], probe, updates]] & /@ entries;
            If[output === "Association", rows, PacletDependencyReport[paclet["Name"], rows]]
        ]]]
    ]

dependencyInfo[owner_, entry_, installed_List, probe_, updates_] := Module[{result, readiness, latest, sources, description, git},
    git = If[installed =!= {} && KeyExistsQ[entry, "Git"], dependencyInstalledGit[First[installed]], None];
    result = Join[entry, <|"Status" -> dependencyLocalStatus[entry, installed, git],
        "InstalledVersion" -> If[installed === {}, Missing["NotInstalled"], First[installed]["Version"]]|>];
    If[git =!= None,
        AssociateTo[result, {"InstalledGitSource" -> git, "GitRevisionMatch" -> dependencyGitMatchesQ[git, entry],
            "GitUserInstallation" -> dependencyGitUserCopyQ[First[installed]]}]];
    If[probe && result["Status"] === "Installed" && StringQ[entry["Check"]],
        readiness = dependencyReadiness[owner, entry];
        If[readiness === True, result["Status"] = "Ready",
            AssociateTo[result, {"Status" -> "Needs setup", "ReadinessFailure" -> readiness}]]];
    If[updates && dependencySupportedQ[entry],
        latest = If[TrueQ[result["GitRevisionMatch"]] && dependencyCompatibleQ[result["InstalledVersion"], Lookup[entry, "Version", All]],
            <|"Version" -> result["InstalledVersion"]|>,
            TimeConstrained[dependencyRelease[owner, entry, True], $dependencyVersionTimeout,
                dependencyFailure["DependencyTimeout", "The version check for `1` timed out. Try checking for updates again.", entry["Name"]]]];
        result["LatestVersion"] = If[FailureQ[latest], latest, latest["Version"]]];
    If[StringTrim[Lookup[entry, "Description", ""]] === "",
        sources = {First[installed, None], If[AssociationQ[latest], Lookup[latest, "SitePaclet", None], None]};
        description = SelectFirst[Cases[sources, p_PacletObject :> p["Description"]], StringQ[#] && StringTrim[#] =!= "" &, None];
        If[StringQ[description], result["Description"] = description]];
    result
]

Options[InstallPacletDependencies] = {"CheckReadiness" -> True}
InstallPacletDependencies[owner : (_String | File[_String] | _PacletObject)] :=
    InstallPacletDependencies[owner, All]
InstallPacletDependencies[owner : (_String | File[_String] | _PacletObject),
        first : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ..}), rest : OptionsPattern[]] :=
    InstallPacletDependencies[owner, All, first, rest]
InstallPacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String}), OptionsPattern[]] :=
    With[{check = TrueQ[OptionValue["CheckReadiness"]]},
        dependencyMap[dependencyInstall[#1, #2, False, check] &, owner, names]
    ]

Options[UpdatePacletDependencies] = {"CheckReadiness" -> True}
UpdatePacletDependencies[owner : (_String | File[_String] | _PacletObject)] :=
    UpdatePacletDependencies[owner, All]
UpdatePacletDependencies[owner : (_String | File[_String] | _PacletObject),
        first : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ..}), rest : OptionsPattern[]] :=
    UpdatePacletDependencies[owner, All, first, rest]
UpdatePacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String}), OptionsPattern[]] :=
    With[{check = TrueQ[OptionValue["CheckReadiness"]]},
        dependencyMap[dependencyInstall[#1, #2, True, check] &, owner, names]
    ]

dependencyRemovableQ[entry_] := ! TrueQ[entry["Required"]] && entry["Name"] =!= "WolframInstitute/PacletPage"
UninstallPacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyUninstall, owner, names, If[names === All || names === "Optional", Select[dependencyRemovableQ], Identity]]

dependencyChecked[owner_, entry_, paclet_, checkReadiness_ : True] := Module[{readiness},
    If[! TrueQ[checkReadiness] || KeyExistsQ[$dependencyRestarts, entry["Name"]] || ! StringQ[entry["Check"]], Return[paclet]];
    readiness = dependencyReadiness[owner, entry];
    If[readiness === True, paclet, Failure["DependencySetupRequired", <|
        "MessageTemplate" -> "`1` `2` needs setup. `3`", "MessageParameters" -> {entry["Name"], paclet["Version"], readiness["Message"]},
        "InstalledVersion" -> paclet["Version"], "Cause" -> readiness|>]]
]

dependencyInstall[owner_, entry_, update_, checkReadiness_ : True] := Module[{name = entry["Name"], installed, current, release, result, loaded, git = KeyExistsQ[entry, "Git"]},
    If[! dependencySupportedQ[entry], Return[dependencyFailure["UnsupportedSystem", "`1` is unavailable on `2`.", name, $SystemID]]];
    installed = PacletFind[name];
    current = First[installed, None];
    If[update && current === None, Return[dependencyFailure["DependencyNotInstalled", "Install `1` before updating it.", name]]];
    If[current =!= None && dependencyCompatibleQ[current["Version"], Lookup[entry, "Version", All]] &&
            If[git, dependencyGitMatchesQ[dependencyInstalledGit[current], entry], ! update],
        Return[dependencyChecked[owner, entry, current, checkReadiness]]];
    $dependencyProgress["Checking release", None];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    release = dependencyRelease[owner, entry, update];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    If[FailureQ[release], Return[release]];
    If[git && current =!= None,
        With[{conflict = dependencyGitConflict[current, release["Version"]]}, If[FailureQ[conflict], Return[conflict]]]];
    If[! git && current =!= None && ! PacletNewerQ[release["Version"], current["Version"]],
        Return[If[dependencyCompatibleQ[current["Version"], Lookup[entry, "Version", All]], dependencyChecked[owner, entry, current, checkReadiness],
            dependencyFailure["DependencyVersionConflict", "Installed `1` `2` does not satisfy `3`.", name, current["Version"], entry["Version"]]]]];
    loaded = AnyTrue[installed, dependencyLoadedQ];
    result = dependencyInstallRelease[entry, release];
    If[! MatchQ[result, _PacletObject], Return[result]];
    If[loaded, AssociateTo[$dependencyRestarts, name -> True]];
    $dependencyProgress["Checking readiness", None];
    dependencyChecked[owner, entry, result, checkReadiness]
]

dependencyUninstall[owner_, entry_] := Module[{name = entry["Name"], userCopies, loaded, result},
    If[! dependencyRemovableQ[entry], Return[If[entry["Required"],
        dependencyFailure["RequiredDependency", "`1` requires `2`.", owner["Name"], name],
        dependencyFailure["DependencyManagerInUse", "`1` is running this dependency manager.", name]]]];
    userCopies = PacletFind[name, "Location" -> (FileNameJoin[{$UserBasePacletsDirectory, "Repository"}] <> $PathnameSeparator),
        "WolframVersion" -> All, "SystemID" -> All, "ProductID" -> All, "EvaluationEnvironment" -> All, "Enabled" -> All];
    If[userCopies === {}, Return[If[PacletFind[name] === {}, Null,
        dependencyFailure["ExternalInstallation", "`1` is supplied outside the user paclet repository.", name]]]];
    Do[
        loaded = dependencyLoadedQ[p];
        result = PacletUninstall[p];
        If[result =!= Null, Return[If[MatchQ[result, _Failure], result, dependencyFailure["DependencyUninstallFailed", "Could not remove `1`.", p["Location"]]], Module]];
        If[loaded, AssociateTo[$dependencyRestarts, name -> True]],
        {p, userCopies}];
    Null
]
