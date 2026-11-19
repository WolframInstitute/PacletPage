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
dependencyMap[f_, ownerSpec_, selection_] := Module[{owner = dependencyOwner[ownerSpec], entries, names, unknown},
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    entries = dependencyManifest[owner];
    If[FailureQ[entries], Return[entries]];
    names = If[selection === All, Keys[entries], DeleteDuplicates[Flatten[{selection}]]];
    unknown = Complement[names, Keys[entries]];
    If[unknown =!= {}, Return[dependencyFailure["UnknownDependency", "`1` does not declare `2`.", owner["Name"], unknown]]];
    Block[{$refreshDependencySites = True}, AssociationMap[If[dependencyCancelledQ[], dependencyCancelled[], f[owner, entries[#]]] &, names]]
]

dependencySupportedQ[entry_] := With[{systems = Lookup[entry, "Platforms", All]}, systems === All || MemberQ[systems, $SystemID]]

(* A loaded context can outlive both an upgrade and an uninstall followed by a reinstall. *)
$dependencyRestarts = <||>
dependencyLoadedQ[p_PacletObject] := ! DisjointQ[$Packages, Flatten[{p["Context"]}]]

dependencyReadyQ[owner_, entry_] := With[{check = Lookup[entry, "Check", None]},
    check === None || Block[{$ContextPath = $ContextPath},
        If[! NameQ[check], With[{context = SelectFirst[Flatten[{owner["PrimaryContext"], owner["Context"]}],
                StringQ[#] && StringStartsQ[check, #] &, None]}, If[StringQ[context], Quiet[Needs[context]]]]];
        NameQ[check] && TrueQ[Quiet[Check[Symbol[check][], False]]]
    ]
]

dependencyStatus[owner_, entry_, installed_, probe_] := Which[
    ! dependencySupportedQ[entry], "Unsupported",
    installed === {}, "Not installed",
    KeyExistsQ[$dependencyRestarts, entry["Name"]], "Restart required",
    ! dependencyCompatibleQ[First[installed]["Version"], Lookup[entry, "Version", All]], "Version mismatch",
    ! probe || ! KeyExistsQ[entry, "Check"], "Installed",
    dependencyReadyQ[owner, entry], "Ready",
    True, "Needs setup"
]

Options[PacletDependencyInformation] = {"CheckReadiness" -> False, "CheckUpdates" -> False}

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject), opts : OptionsPattern[]] :=
    PacletDependencyInformation[owner, All, opts]

PacletDependencyInformation[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String}), OptionsPattern[]] :=
    With[{probe = TrueQ[OptionValue["CheckReadiness"]], release = TrueQ[OptionValue["CheckUpdates"]]},
        dependencyMap[Function[{p, entry}, Module[{installed = PacletFind[entry["Name"]], result, latest},
            result = Join[entry, <|"Status" -> dependencyStatus[p, entry, installed, probe],
                "InstalledVersion" -> If[installed === {}, Missing["NotInstalled"], First[installed]["Version"]]|>];
            If[release && dependencySupportedQ[entry],
                latest = dependencyRelease[p, entry];
                AssociateTo[result, "LatestVersion" -> If[FailureQ[latest], latest, latest["Version"]]]];
            result
        ]], owner, names]
    ]

InstallPacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyInstall[#1, #2, False] &, owner, names]
UpdatePacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyInstall[#1, #2, True] &, owner, names]
UninstallPacletDependencies[owner : (_String | File[_String] | _PacletObject), names : (All | _String | {___String})] :=
    dependencyMap[dependencyUninstall, owner, names]

dependencyChecked[owner_, entry_, p_] := If[KeyExistsQ[$dependencyRestarts, entry["Name"]] || dependencyReadyQ[owner, entry], p,
    dependencyFailure["DependencySetupRequired", "`1` is installed and needs additional setup.", entry["Name"]]]

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

dependencyUninstall[_, entry_] := Module[{name = entry["Name"], userCopies, loaded, result},
    userCopies = PacletFind[name, "Location" -> FileNameJoin[{$UserBasePacletsDirectory, "Repository"}],
        "WolframVersion" -> All, "SystemID" -> All, "Enabled" -> All];
    If[userCopies === {}, Return[If[PacletFind[name] === {}, Null,
        dependencyFailure["ExternalInstallation", "`1` is supplied outside the user paclet repository.", name]]]];
    Do[
        loaded = dependencyLoadedQ[p];
        result = Quiet[PacletUninstall[p]];
        If[result =!= Null, Return[If[FailureQ[result], result, dependencyFailure["DependencyUninstallFailed", "Could not remove `1`.", p["Location"]]], Module]];
        If[loaded, AssociateTo[$dependencyRestarts, name -> True]],
        {p, userCopies}];
    Null
]
