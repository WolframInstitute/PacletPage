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
dependencyOwner[File[path_String]] := If[DirectoryQ[path], Quiet[PacletObject[File[path]]],
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
    ContainsAll[{"Name", "Required", "Version", "Platforms", "Description", "Check", "Resource", "Archive",
        "DownloadSize", "License", "URL", "SetupLinks"}, Keys[entry]] &&
    dependencyNameQ[name] && name =!= owner["Name"] && dependencyVersionSpecQ[version] &&
    (platforms === All || MatchQ[platforms, {___String}]) &&
    AllTrue[Values[KeyTake[entry, {"Description", "License"}]], StringQ] &&
    AllTrue[Values[KeyTake[entry, {"URL", "Resource"}]], dependencyHTTPQ] &&
    dependencyTextMapQ[links] && AllTrue[Values[links], dependencyHTTPQ] &&
    (StringQ[size] || dependencyTextMapQ[size]) &&
    (! KeyExistsQ[entry, "Archive"] || (StringQ[entry["Archive"]] && entry["Archive"] =!= "" && ! KeyExistsQ[entry, "Resource"])) &&
    (check === None || (StringQ[check] && StringMatchQ[check, RegularExpression["([a-zA-Z$][a-zA-Z0-9$]*`)+[a-zA-Z$][a-zA-Z0-9$]*"]] &&
        AnyTrue[Cases[Flatten[{owner["PrimaryContext"], owner["Context"]}], _String], StringStartsQ[check, #] &]))
]

dependencyManifest[owner_PacletObject] := Module[{info = owner["PacletInfo"], required, optional, entries, names},
    required = Lookup[info, "Dependencies", {}];
    optional = Lookup[info, "OptionalDependencies", {}];
    If[! ListQ[required] || ! ListQ[optional], Return[dependencyFailure["DependencyManifest", "Dependencies and OptionalDependencies must be lists."]]];
    entries = Join[Append[dependencyEntry[#], "Required" -> True] & /@ required, Append[dependencyEntry[#], "Required" -> False] & /@ optional];
    With[{invalid = SelectFirst[entries, ! dependencyEntryQ[#, owner] &, None]},
        If[invalid =!= None, Return[dependencyFailure["DependencyManifest", "Invalid dependency declaration in `1`: `2`.", owner["Name"], invalid]]]];
    names = #["Name"] & /@ entries;
    If[! DuplicateFreeQ[names], Return[dependencyFailure["DependencyManifest", "Each dependency in `1` must have a unique name.", owner["Name"]]]];
    AssociationThread[names, entries]
]

(* Validate the whole selection before its first action. Each library then keeps its own result. *)
withDependencySelection[ownerSpec_, selection_, action_] := Module[{owner = dependencyOwner[ownerSpec], entries, names, unknown},
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    entries = dependencyManifest[owner];
    If[FailureQ[entries], Return[entries]];
    names = If[selection === All, Keys[entries], DeleteDuplicates[Flatten[{selection}]]];
    unknown = Complement[names, Keys[entries]];
    If[unknown =!= {}, Return[dependencyFailure["UnknownDependency", "`1` does not declare `2`.", owner["Name"], unknown]]];
    Block[{$refreshDependencySites = True}, action[owner, AssociationThread[names, Lookup[entries, names]]]]
]

dependencyMap[action_, ownerSpec_, selection_, filter_: Identity] := withDependencySelection[ownerSpec, selection,
    Function[{owner, entries}, If[dependencyCancelledQ[], dependencyCancelled[], action[owner, #]] & /@ filter[entries]]]

(* Exact-name lookups are cheaper for a small selection. For a long one the paclet manager can
   enumerate the same local index once. Keep its own version and directory precedence in each group. *)
dependencyInstallations[names_List] := If[Length[names] < 16, AssociationMap[PacletFind, names],
    With[{byName = GroupBy[PacletFind[], #["Name"] &]}, AssociationThread[names, Lookup[byName, names, {}]]]
]

dependencySupportedQ[entry_] := With[{systems = Lookup[entry, "Platforms", All]}, systems === All || MemberQ[systems, $SystemID]]

(* A loaded context can outlive both an upgrade and an uninstall followed by a reinstall. *)
$dependencyRestarts = <||>
dependencyLoadedQ[p_PacletObject] := ! DisjointQ[$Packages, Flatten[{p["Context"]}]]

dependencyReadiness[owner_, entry_] := Module[{check = entry["Check"], context, result},
    Block[{$ContextPath = $ContextPath},
        If[! NameQ[check],
            context = SelectFirst[Flatten[{owner["PrimaryContext"], owner["Context"]}],
                StringQ[#] && StringStartsQ[check, #] &, None];
            If[StringQ[context], Quiet[Needs[context]]]];
        If[! NameQ[check], Return[dependencyFailure["DependencyCheckUnavailable", "The readiness function `1` could not be loaded.", check]]];
        result = Quiet[Symbol[check][]]];
    Which[result === True, True, MatchQ[result, _Failure], result,
        True, dependencyFailure["DependencyCheckFailed", "The readiness check `1` did not succeed.", check]]
]

dependencyLocalStatus[entry_, installed_] := Which[
    ! dependencySupportedQ[entry], "Unsupported",
    installed === {}, "Not installed",
    KeyExistsQ[$dependencyRestarts, entry["Name"]], "Restart required",
    ! dependencyCompatibleQ[First[installed]["Version"], Lookup[entry, "Version", All]], "Version mismatch",
    True, "Installed"
]

Options[PacletDependencyInformation] = {"CheckReadiness" -> False, "CheckUpdates" -> False}

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject)] :=
    PacletDependencyInformation[owner, All]

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject),
        first : (_Rule | _RuleDelayed | {(_Rule | _RuleDelayed) ..}), rest : OptionsPattern[]] :=
    PacletDependencyInformation[owner, All, first, rest]

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String}), OptionsPattern[]] :=
    With[{probe = TrueQ[OptionValue["CheckReadiness"]], updates = TrueQ[OptionValue["CheckUpdates"]]},
        withDependencySelection[owner, names, Function[{paclet, entries}, With[{installed = dependencyInstallations[Keys[entries]]},
            Map[If[dependencyCancelledQ[], dependencyCancelled[], dependencyInfo[paclet, #, installed[#["Name"]], probe, updates]] &, entries]]]]
    ]

dependencyInfo[owner_, entry_, installed_List, probe_, updates_] := Module[{result, readiness, latest},
    result = Join[entry, <|"Status" -> dependencyLocalStatus[entry, installed],
        "InstalledVersion" -> If[installed === {}, Missing["NotInstalled"], First[installed]["Version"]]|>];
    If[probe && result["Status"] === "Installed" && KeyExistsQ[entry, "Check"],
        readiness = dependencyReadiness[owner, entry];
        If[readiness === True, result["Status"] = "Ready",
            AssociateTo[result, {"Status" -> "Needs setup", "ReadinessFailure" -> readiness}]]];
    If[updates && dependencySupportedQ[entry],
        latest = dependencyRelease[owner, entry];
        result["LatestVersion"] = If[FailureQ[latest], latest, latest["Version"]]];
    result
]

InstallPacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyInstall[#1, #2, False] &, owner, names]
UpdatePacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyInstall[#1, #2, True] &, owner, names]
UninstallPacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyUninstall, owner, names, If[names === All, Select[! #["Required"] &], Identity]]

dependencyChecked[owner_, entry_, paclet_] := Module[{readiness},
    If[KeyExistsQ[$dependencyRestarts, entry["Name"]] || ! KeyExistsQ[entry, "Check"], Return[paclet]];
    readiness = dependencyReadiness[owner, entry];
    If[readiness === True, paclet, Failure["DependencySetupRequired", <|
        "MessageTemplate" -> "`1` `2` needs setup. `3`", "MessageParameters" -> {entry["Name"], paclet["Version"], readiness["Message"]},
        "InstalledVersion" -> paclet["Version"], "Cause" -> readiness|>]]
]

dependencyInstall[owner_, entry_, update_] := Module[{name = entry["Name"], installed, current, release, result, loaded},
    If[! dependencySupportedQ[entry], Return[dependencyFailure["UnsupportedSystem", "`1` is unavailable on `2`.", name, $SystemID]]];
    installed = PacletFind[name];
    current = First[installed, None];
    If[update && current === None, Return[dependencyFailure["DependencyNotInstalled", "Install `1` before updating it.", name]]];
    If[! update && current =!= None && dependencyCompatibleQ[current["Version"], Lookup[entry, "Version", All]],
        Return[dependencyChecked[owner, entry, current]]];
    $dependencyProgress["Checking release", None];
    release = dependencyRelease[owner, entry];
    If[FailureQ[release], Return[release]];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    If[current =!= None && ! PacletNewerQ[release["Version"], current["Version"]],
        Return[If[dependencyCompatibleQ[current["Version"], Lookup[entry, "Version", All]], dependencyChecked[owner, entry, current],
            dependencyFailure["DependencyVersionConflict", "Installed `1` `2` does not satisfy `3`.", name, current["Version"], entry["Version"]]]]];
    loaded = AnyTrue[installed, dependencyLoadedQ];
    result = dependencyInstallRelease[entry, release];
    If[! MatchQ[result, _PacletObject], Return[result]];
    If[loaded, AssociateTo[$dependencyRestarts, name -> True]];
    $dependencyProgress["Checking readiness", None];
    dependencyChecked[owner, entry, result]
]

dependencyUninstall[owner_, entry_] := Module[{name = entry["Name"], userCopies, loaded, result},
    If[entry["Required"], Return[dependencyFailure["RequiredDependency", "`1` requires `2`.", owner["Name"], name]]];
    userCopies = PacletFind[name, "Location" -> FileNameJoin[{$UserBasePacletsDirectory, "Repository"}],
        "WolframVersion" -> All, "SystemID" -> All, "Enabled" -> All];
    If[userCopies === {}, Return[If[PacletFind[name] === {}, Null,
        dependencyFailure["ExternalInstallation", "`1` is supplied outside the user paclet repository.", name]]]];
    Do[
        loaded = dependencyLoadedQ[p];
        result = Quiet[PacletUninstall[p]];
        If[result =!= Null, Return[If[MatchQ[result, _Failure], result, dependencyFailure["DependencyUninstallFailed", "Could not remove `1`.", p["Location"]]], Module]];
        If[loaded, AssociateTo[$dependencyRestarts, name -> True]],
        {p, userCopies}];
    Null
]
