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

$dependencyInternetMessage = "Wolfram internet access is disabled. Enable it in Internet & Mail Settings, then try again."

(* The lightweight cloud metadata contains the version. Fetch the larger description and the
   archive only after the version comparison. Keep public requests independent of cloud sign-in. *)
dependencyResourceMetadata[url_] := Block[{$ProgressReporting = False, CloudConnect = ($Failed &)},
    TimeConstrained[Association[MetaInformation /. Options[CloudObject[url], MetaInformation]], 20, $Failed]
]

(* Apply the paclet manager's runtime filters to this candidate before PacletInstall,
   which also accepts archives for other systems and Wolfram versions. *)
dependencyArchiveSupportedQ[p_PacletObject] := PacletFind[p["Name"], <|"Paclets" -> {p}, "Enabled" -> All, "Internal" -> All|>] === {p}
dependencyArchiveUnsupported[p_] := dependencyFailure["DependencyArchive", "The archive for `1` `2` does not support this Wolfram system.", p["Name"], p["Version"]]

dependencyRelease[owner_, entry_] := Module[{name = entry["Name"], spec = Lookup[entry, "Version", All], release, archive, p, meta, candidates},
    release = Which[
        KeyExistsQ[entry, "Archive"],
            archive = dependencyArchivePath[owner, entry["Archive"]];
            If[FailureQ[archive], Return[archive]];
            If[FileType[First[archive]] =!= File || ToLowerCase[FileExtension[First[archive]]] =!= "paclet",
                Return[dependencyFailure["DependencyArchive", "The paclet archive `1` could not be found.", archive]]];
            p = PacletObject[archive];
            If[! MatchQ[p, _PacletObject] || p["Name"] =!= name,
                Return[dependencyFailure["DependencyArchive", "The archive `1` does not contain `2`.", archive, name]]];
            If[! dependencyArchiveSupportedQ[p], Return[dependencyArchiveUnsupported[p]]];
            <|"Version" -> p["Version"], "Archive" -> archive|>,
        KeyExistsQ[entry, "Git"],
            If[$AllowInternet === False, Return[dependencyFailure["DependencyInternetDisabled", $dependencyInternetMessage]]];
            With[{git = dependencyGitRelease[entry]}, If[FailureQ[git], Return[git], git]],
        KeyExistsQ[entry, "Resource"],
            If[$AllowInternet === False, Return[dependencyFailure["DependencyInternetDisabled", $dependencyInternetMessage]]];
            meta = dependencyResourceMetadata[entry["Resource"]];
            If[! AssociationQ[meta] || Lookup[meta, "Name"] =!= name || Lookup[meta, "ResourceType"] =!= "Paclet" ||
                    ! MatchQ[Lookup[meta, "ResourceLocation"], CloudObject[_String, ___]],
                Return[dependencyFailure["DependencyRelease", "Could not read the published release of `1`.", name]]];
            <|"Version" -> Lookup[meta, "PacletVersion"], "DescriptionURL" -> First[meta["ResourceLocation"]]|>,
        True,
            If[$AllowInternet === False, Return[dependencyFailure["DependencyInternetDisabled", $dependencyInternetMessage]]];
            With[{refresh = $refreshDependencySites},
                $refreshDependencySites = False;
                candidates = PacletFindRemote[name, UpdatePacletSites -> refresh]];
            p = If[ListQ[candidates], SelectFirst[candidates, dependencyVersionQ[#["Version"]] && dependencyCompatibleQ[#["Version"], spec] &, None], None];
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
    held = If[SyntaxQ[text], Block[{$Context = "WolframInstitute`PacletPage`ResourceDescription`", $ContextPath = {"System`"}}, ToExpression[text, InputForm, HoldComplete]], None];
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
        KeyExistsQ[release, "GitArchive"], dependencyGitInstall[release, name],
        KeyExistsQ[release, "Archive"], dependencyInstallFile[release["Archive"], name, release["Version"]],
        True,
            resource = entry["Resource"];
            desc = release["DescriptionURL"];
            If[! dependencyPublisherURLQ[resource, desc], Return[dependencyFailure["DependencyPublisher", "Unexpected release description address for `1`.", name]]];
            text = Block[{CloudConnect = ($Failed &)}, TimeConstrained[Import[CloudObject[desc], "Text"], 60, $Failed]];
            If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
            If[! StringQ[text], Return[dependencyFailure["DependencyRelease", "Could not read the release description for `1`.", name]]];
            url = dependencyArchiveURL[text, name, release["Version"], $SystemID];
            If[FailureQ[url], Return[url]];
            If[! dependencyPublisherURLQ[resource, url], Return[dependencyFailure["DependencyPublisher", "Unexpected archive address for `1`.", name]]];
            dependencyDownloadInstall[url, name, release["Version"]]
    ]
]

dependencyInstallFile[source_, name_, version_] := Module[{p, result, installed},
    p = If[MatchQ[source, _PacletObject], source, PacletObject[source]];
    If[! MatchQ[p, _PacletObject] || p["Name"] =!= name || p["Version"] =!= version,
        Return[dependencyFailure["DependencyArchive", "Expected an archive containing `1` `2`.", name, version]]];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    If[! dependencyArchiveSupportedQ[p], Return[dependencyArchiveUnsupported[p]]];
    $dependencyProgress["Installing", None];
    result = PacletInstall[source, KeepExistingVersion -> True];
    If[MatchQ[result, _PacletObject] && result["Name"] === name && result["Version"] === version, Return[result]];
    (* Another installer may have committed the same release since the first local lookup. *)
    installed = PacletFind[name -> version];
    Which[installed =!= {}, First[installed], MatchQ[result, _Failure], result,
        True, dependencyFailure["DependencyInstallFailed", "Could not install `1` `2`.", name, version]]
]

(* TaskWait wakes on cancellation even if a stalled server never sends a progress event. *)
dependencyDownloadAttempt[url_, file_] := Module[{response = <||>, task},
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    WithCleanup[
        task = URLDownloadSubmit[url, file, TimeConstraint -> <|"Connecting" -> 30, "Reading" -> Infinity|>, ProgressReporting -> False,
            HandlerFunctions -> <|"TaskProgress" -> Function[event, With[{bytes = event["ByteCountDownloaded"], total = event["ByteCountTotal"]},
                    $dependencyProgress["Downloading", If[NumberQ[bytes] && TrueQ[total > 0], bytes / total, None]]]],
                "TaskFinished" -> Function[event, response = event]|>,
            HandlerFunctionsKeys -> {"StatusCode", "Failure", "ByteCountDownloaded", "ByteCountTotal"}];
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

(* a download tried again once where the server did not refuse it *)
dependencyDownloaded[url_, file_] := (
    $dependencyProgress["Downloading", None];
    NestWhile[Function[previous, $dependencyProgress["Retrying", None]; dependencyDownloadAttempt[url, file]],
        dependencyDownloadAttempt[url, file],
        Function[result, MatchQ[result, Failure["DependencyDownload", _]] && ! TrueQ[400 <= Lookup[result["Response"], "StatusCode", 0] < 500]], 1, 2]
)

dependencyDownloadInstall[url_, name_, version_] := Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["paclet-"] <> ".paclet"}], download},
    WithCleanup[
        download = dependencyDownloaded[url, file];
        If[FailureQ[download], download, dependencyInstallFile[download, name, version]],
        If[FileExistsQ[file], DeleteFile[file]]
    ]
]

(* ::Section:: *)
(*A GitHub repository at a commit*)

(* A dependency may be a paclet in a GitHub repository at a commit: "Git" is the repository's
   address, "Commit" the commit, and "Path" the directory of the paclet in it, the repository's root
   by default. Its version is the one the commit's PacletInfo.wl gives, read without evaluating it,
   and installing it unpacks the commit's tree and packs the paclet's directory as an archive. *)
dependencyGitRepository[url_] := If[StringQ[url],
    Replace[StringCases[url, RegularExpression["^https://github\\.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+?)(?:\\.git)?/?$"] :> {"$1", "$2"}],
        {{{owner_String, repository_String}} :> {owner, repository}, _ -> None}],
    None
]
dependencyCommitQ[commit_] := StringQ[commit] && StringMatchQ[commit, RegularExpression["[0-9a-f]{7,40}"]]
dependencyGitPathQ[path_] := StringQ[path] && (path === "" ||
    ! StringStartsQ[path, "/" | "\\"] && ! StringContainsQ[path, ":"] && ! MemberQ[FileNameSplit[path], ".." | "."])

dependencyGitText[url_String] := With[{response = TimeConstrained[URLRead[HTTPRequest[url], {"StatusCode", "Body"}], 30, $Failed]},
    If[AssociationQ[response] && response["StatusCode"] === 200 && StringQ[response["Body"]], response["Body"], $Failed]
]

(* the "Name" and "Version" of a PacletInfo.wl's text, which is parsed and never evaluated: the
   PacletObject among its statements, after the comments and blank lines a file opens with *)
dependencyPacletInfoFields[text_String] := If[ ! SyntaxQ[text], <||>,
    Replace[
        FirstCase[
            Block[{$Context = "WolframInstitute`PacletPage`PacletInfoText`", $ContextPath = {"System`"}}, ToExpression[text, InputForm, HoldComplete]],
            HoldPattern[PacletObject[info_]] :> HoldComplete[info], None, {1}],
        {HoldComplete[Association[rules___]] :>
            Association[Cases[HoldComplete[rules], HoldPattern[Rule[key : ("Name" | "Version"), value_String]] :> (key -> value)]],
         HoldComplete[info_?AssociationQ] :> Select[KeyTake[info, {"Name", "Version"}], StringQ],
         _ -> <||>}
    ]
]

dependencyGitRelease[entry_] := Module[{name = entry["Name"], commit = entry["Commit"], path = Lookup[entry, "Path", ""], repository, text, fields},
    repository = dependencyGitRepository[entry["Git"]];
    text = dependencyGitText[StringRiffle[Join[{"https://raw.githubusercontent.com", repository[[1]], repository[[2]], commit},
        If[path === "", {}, FileNameSplit[path]], {"PacletInfo.wl"}], "/"]];
    If[! StringQ[text], Return[dependencyFailure["DependencyRelease", "Could not read the PacletInfo.wl of `1` at commit `2`.", name, commit]]];
    fields = dependencyPacletInfoFields[text];
    If[Lookup[fields, "Name", None] =!= name, Return[dependencyFailure["DependencyRelease", "The PacletInfo.wl at commit `1` does not declare `2`.", commit, name]]];
    <|"Version" -> fields["Version"], "GitArchive" -> "https://codeload.github.com/" <> repository[[1]] <> "/" <> repository[[2]] <> "/zip/" <> commit,
      "GitPath" -> path|>
]

(* the commit's tree downloaded, the paclet's directory in it packed as an archive and installed *)
dependencyGitInstall[release_, name_] := Module[{dir = CreateDirectory[], zip, download, top, source, archive},
    zip = FileNameJoin[{dir, "tree.zip"}];
    WithCleanup[
        download = dependencyDownloaded[release["GitArchive"], zip];
        If[FailureQ[download], Return[download, Module]];
        If[dependencyCancelledQ[], Return[dependencyCancelled[], Module]];
        ExtractArchive[zip, FileNameJoin[{dir, "tree"}]];
        top = FileNames["*", FileNameJoin[{dir, "tree"}]];
        source = If[MatchQ[top, {_String?DirectoryQ}], If[release["GitPath"] === "", First[top], FileNameJoin[Join[top, FileNameSplit[release["GitPath"]]]]], None];
        If[! StringQ[source] || ! FileExistsQ[FileNameJoin[{source, "PacletInfo.wl"}]],
            Return[dependencyFailure["DependencyArchive", "The commit's tree has no paclet at `1`.", release["GitPath"]], Module]];
        archive = CreatePacletArchive[source, dir];
        If[! StringQ[archive], Return[dependencyFailure["DependencyArchive", "Could not pack `1` from the commit's tree.", name], Module]];
        dependencyInstallFile[File[archive], name, release["Version"]],
        DeleteDirectory[dir, DeleteContents -> True]
    ]
]
