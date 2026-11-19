(* ::Package:: *)

$dependencyProgress = (Null &)
dependencyCancelledQ[] := False
cancelDependencyDownload[] := Null
dependencyCancelled[] := dependencyFailure["DependencyCancelled", "The operation was cancelled."]

dependencyArchivePath[owner_, path_String] := If[StringStartsQ[path, "/" | "\\"] || StringContainsQ[path, ":"] ||
        MemberQ[FileNameSplit[path], ".."] || ! StringQ[owner["Location"]],
    dependencyFailure["DependencyArchive", "Give a .paclet archive relative to the consumer directory."],
    File[FileNameJoin[{owner["Location"], path}]]
]

(* The lightweight cloud metadata contains the version. Fetch the larger description and the
   archive only after the version comparison. Keep public requests independent of cloud sign-in. *)
dependencyResourceMetadata[url_] := Block[{$ProgressReporting = False, CloudConnect = ($Failed &)},
    Quiet[TimeConstrained[Association[MetaInformation /. Options[CloudObject[url], MetaInformation]], 20, $Failed]]
]

dependencyRelease[owner_, entry_] := Module[{name = entry["Name"], spec = Lookup[entry, "Version", All], release, archive, p, meta, candidates},
    release = Which[
        KeyExistsQ[entry, "Archive"],
            archive = dependencyArchivePath[owner, entry["Archive"]];
            If[FailureQ[archive], Return[archive]];
            If[FileType[First[archive]] =!= File || ToLowerCase[FileExtension[First[archive]]] =!= "paclet",
                Return[dependencyFailure["DependencyArchive", "The paclet archive `1` could not be found.", archive]]];
            p = Quiet[PacletObject[archive]];
            If[! MatchQ[p, _PacletObject] || p["Name"] =!= name,
                Return[dependencyFailure["DependencyArchive", "The archive `1` does not contain `2`.", archive, name]]];
            <|"Version" -> p["Version"], "Archive" -> archive|>,
        KeyExistsQ[entry, "Resource"],
            meta = dependencyResourceMetadata[entry["Resource"]];
            If[! AssociationQ[meta] || Lookup[meta, "Name"] =!= name || Lookup[meta, "ResourceType"] =!= "Paclet" ||
                    ! MatchQ[Lookup[meta, "ResourceLocation"], CloudObject[_String, ___]],
                Return[dependencyFailure["DependencyRelease", "Could not read the published release of `1`.", name]]];
            <|"Version" -> Lookup[meta, "PacletVersion"], "DescriptionURL" -> First[meta["ResourceLocation"]]|>,
        True,
            candidates = Quiet[PacletFindRemote[name, UpdatePacletSites -> $refreshDependencySites]];
            $refreshDependencySites = False;
            p = If[ListQ[candidates], SelectFirst[candidates, dependencyCompatibleQ[#["Version"], spec] &, None], None];
            If[! MatchQ[p, _PacletObject], Return[dependencyFailure["DependencyRelease", "No compatible release of `1` was found on the configured paclet sites.", name]]];
            <|"Version" -> p["Version"], "SitePaclet" -> p|>
    ];
    If[dependencyVersionQ[release["Version"]] && dependencyCompatibleQ[release["Version"], spec], release,
        dependencyFailure["DependencyVersion", "Release `1` of `2` does not satisfy the requested version `3`.", release["Version"], name, spec]]
]

(* Resolve only literal fields, keeping all other publisher expressions held. Platform resources
   published for bundled libraries select a record by $SystemID before constructing ResourceObject. *)
dependencyResourceRecord[HoldComplete[ResourceObject[a_Association]], _] := HoldComplete[a]
dependencyResourceRecord[HoldPattern[HoldComplete[If[KeyExistsQ[_Association, $SystemID],
        ResourceObject[(entries_Association)[$SystemID]], _]]], system_] :=
    FirstCase[HoldComplete[entries], HoldPattern[Rule[system, a_Association]] :> HoldComplete[a], None, {2}]
dependencyResourceRecord[_, _] := None

dependencyArchiveURL[text_String, name_, version_, system_] := Module[{held, record, strings, platforms, archives},
    held = Block[{$Context = "WolframInstitute`PacletPage`ResourceDescription`", $ContextPath = {"System`"}}, Quiet[ToExpression[text, InputForm, HoldComplete]]];
    record = dependencyResourceRecord[held, system];
    If[record === None, Return[dependencyFailure["DependencyRelease", "No release record for `1` on `2`.", name, system]]];
    strings = Cases[record, HoldPattern[Rule[key : ("Name" | "Version" | "ResourceType"), value_String]] :> (key -> value), {2}];
    platforms = Cases[record, HoldPattern[Rule["SystemID", value_]] :> HoldComplete[value], {2}];
    archives = Cases[record, HoldPattern[Rule["PacletFile", CloudObject[url_String, ___] | url_String]] :> url, {2}];
    If[Sort[strings] === Sort[{"Name" -> name, "Version" -> version, "ResourceType" -> "Paclet"}] &&
            (platforms === {} || MatchQ[platforms, {HoldComplete[All | system]}] ||
                MatchQ[platforms, {HoldComplete[{___String}]}] && MemberQ[First[platforms], system, {2}]) &&
            MatchQ[archives, {_String?dependencyHTTPQ}], First[archives],
        dependencyFailure["DependencyRelease", "The published archive does not match `1` `2` on `3`.", name, version, system]]
]

(* A deployed resource may put files elsewhere under the same publisher's cloud directory. *)
dependencyPublisherURLQ[resource_, url_] := Module[{base = URLParse[resource], target = URLParse[url], path},
    path = TakeWhile[Rest[base["Path"]], # =!= "DeployedResources" &];
    If[MatchQ[path, {"obj" | "env", _String, ___}], path = Take[path, 2], path = {}];
    dependencyHTTPQ[url] && KeyTake[base, {"Scheme", "Domain", "Port", "User"}] === KeyTake[target, {"Scheme", "Domain", "Port", "User"}] &&
        ! MemberQ[target["Path"], ".." | "."] && Take[Rest[target["Path"]], UpTo[Length[path]]] === path
]

dependencyInstallRelease[entry_, release_] := Module[{name = entry["Name"], text, url, resource, desc},
    Which[
        KeyExistsQ[release, "SitePaclet"], dependencyInstallFile[release["SitePaclet"], name, release["Version"]],
        KeyExistsQ[release, "Archive"], dependencyInstallFile[release["Archive"], name, release["Version"]],
        True,
            resource = entry["Resource"];
            desc = release["DescriptionURL"];
            If[! dependencyPublisherURLQ[resource, desc], Return[dependencyFailure["DependencyPublisher", "Unexpected release description address for `1`.", name]]];
            text = Block[{CloudConnect = ($Failed &)}, Quiet[TimeConstrained[Import[CloudObject[desc], "Text"], 60, $Failed]]];
            If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
            If[! StringQ[text], Return[dependencyFailure["DependencyRelease", "Could not read the release description for `1`.", name]]];
            url = dependencyArchiveURL[text, name, release["Version"], $SystemID];
            If[FailureQ[url], Return[url]];
            If[! dependencyPublisherURLQ[resource, url], Return[dependencyFailure["DependencyPublisher", "Unexpected archive address for `1`.", name]]];
            dependencyDownloadInstall[url, name, release["Version"]]
    ]
]

dependencyInstallFile[source_, name_, version_] := Module[{p, result, installed},
    p = If[MatchQ[source, _PacletObject], source, Quiet[PacletObject[source]]];
    If[! MatchQ[p, _PacletObject] || p["Name"] =!= name || p["Version"] =!= version,
        Return[dependencyFailure["DependencyArchive", "Expected an archive containing `1` `2`.", name, version]]];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    $dependencyProgress["Installing", None];
    result = Quiet[PacletInstall[source, KeepExistingVersion -> True]];
    If[MatchQ[result, _PacletObject] && result["Name"] === name && result["Version"] === version, Return[result]];
    (* Another installer may have committed the same release since the first local lookup. *)
    installed = PacletFind[name -> version];
    Which[installed =!= {}, First[installed], MatchQ[result, _Failure], result,
        True, dependencyFailure["DependencyInstallFailed", "Could not install `1` `2`.", name, version]]
]

(* TaskWait wakes on cancellation even if a stalled server never sends a progress event. *)
dependencyDownloadAttempt[url_, file_] := Module[{response = <||>, task},
    WithCleanup[
        task = URLDownloadSubmit[url, file, TimeConstraint -> <|"Connecting" -> 30, "Reading" -> Infinity|>, ProgressReporting -> False,
            HandlerFunctions -> <|"TaskProgress" -> Function[event, $dependencyProgress["Downloading", event["FractionComplete"]]],
                "TaskFinished" -> Function[event, response = event]|>,
            HandlerFunctionsKeys -> {"StatusCode", "Failure", "FractionComplete", "ByteCountDownloaded", "ByteCountTotal"}];
        If[MatchQ[task, _TaskObject], Block[{cancelDependencyDownload = Function[Null, If[task["TaskStatus"] =!= "Removed", TaskRemove[task]]]},
            If[! dependencyCancelledQ[], TaskWait[task]]]];
        Which[
            dependencyCancelledQ[], dependencyCancelled[],
            FailureQ[Lookup[response, "Failure", None]] || ! TrueQ[200 <= Lookup[response, "StatusCode", 0] < 400] ||
                (TrueQ[Lookup[response, "ByteCountTotal", 0] > 0] && response["ByteCountDownloaded"] =!= response["ByteCountTotal"]),
                Failure["DependencyDownload", <|"MessageTemplate" -> "The archive download did not finish.", "Response" -> response|>],
            True, File[file]],
        If[MatchQ[task, _TaskObject] && task["TaskStatus"] =!= "Removed", TaskRemove[task]]
    ]
]

dependencyDownloadInstall[url_, name_, version_] := Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["paclet-"] <> ".paclet"}], download},
    WithCleanup[
        $dependencyProgress["Downloading", 0];
        download = NestWhile[Function[previous, $dependencyProgress["Retrying", None]; dependencyDownloadAttempt[url, file]],
            dependencyDownloadAttempt[url, file],
            Function[result, MatchQ[result, Failure["DependencyDownload", _]] && ! TrueQ[400 <= Lookup[result["Response"], "StatusCode", 0] < 500]], 1, 2];
        If[FailureQ[download], download, dependencyInstallFile[download, name, version]],
        If[FileExistsQ[file], DeleteFile[file]]
    ]
]
