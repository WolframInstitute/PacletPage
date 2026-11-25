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

(* With "TargetDirectory" each release is unpacked into a directory of the caller's own, which stays
   loaded with PacletDirectoryLoad, and nothing is written under the user paclet base: a
   documentation build installs this way, so the machine's other kernels never see its copies. A
   directory loaded this way wins a version tie with the user repository. *)
$dependencyTarget = None
dependencyTargetQ[] := StringQ[$dependencyTarget]
dependencyInTargetQ[p_] := dependencyTargetQ[] && With[{location = p["Location"]}, StringQ[location] &&
    Most[FileNameSplit[ExpandFileName[location]]] === FileNameSplit[$dependencyTarget]]

(* the copies of a paclet in the target directory, by its directories just beneath it *)
dependencyTargetCopies[name_] := Select[FileNames[All, $dependencyTarget], DirectoryQ[#] && ! StringStartsQ[FileNameTake[#], "."] &&
    AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, Function[file, FileExistsQ[FileNameJoin[{#, file}]]]] &&
    PacletObject[File[#]]["Name"] === name &]

(* Unpack beside the target, two levels down where PacletDirectoryLoad does not look, then replace
   the earlier copies of the paclet and load the directory again. *)
dependencyUnpackFile[source_, name_, version_, git_] := Module[{target = $dependencyTarget, file, staging, unpacked, destination, installed},
    file = Replace[source, File[path_String] :> path];
    staging = FileNameJoin[{target, ".staging", CreateUUID["unpack-"]}];
    If[! StringQ[CreateDirectory[staging, CreateIntermediateDirectories -> True]],
        Return[dependencyFailure["DependencyInstallFailed", "Could not write to `1`.", target]]];
    WithCleanup[
        unpacked = ExtractPacletArchive[file, staging];
        If[! StringQ[unpacked] || ! DirectoryQ[unpacked],
            Return[dependencyFailure["DependencyInstallFailed", "Could not unpack `1` `2` into `3`.", name, version, target], Module]];
        destination = FileNameJoin[{target, FileNameTake[unpacked]}];
        Scan[DeleteDirectory[#, DeleteContents -> True] &, dependencyTargetCopies[name]];
        If[DirectoryQ[destination], DeleteDirectory[destination, DeleteContents -> True]];
        If[! StringQ[RenameDirectory[unpacked, destination]],
            Return[dependencyFailure["DependencyInstallFailed", "Could not move `1` `2` into `3`.", name, version, target], Module]];
        PacletDirectoryUnload[target];
        PacletDirectoryLoad[target];
        installed = PacletObject[File[destination]];
        If[MatchQ[installed, _PacletObject] && installed["Name"] === name && installed["Version"] === version &&
                (! AssociationQ[git] || dependencyInstalledGit[installed] === git),
            installed,
            dependencyFailure["DependencyInstallFailed", "Could not install `1` `2` into `3`.", name, version, target]],
        DeleteDirectory[staging, DeleteContents -> True]
    ]
]

(* A paclet site serves each release at Paclets/<name>-<version>.paclet, with __ for a slash in the name. *)
dependencySiteArchiveURL[p_PacletObject] := With[{site = p["Location"], qualifier = p["Qualifier"]},
    If[StringQ[site] && StringStartsQ[site, "https://" | "http://", IgnoreCase -> True],
        StringTrim[site, "/"] <> "/Paclets/" <> StringReplace[p["Name"], "/" -> "__"] <> "-" <>
            If[StringQ[qualifier] && qualifier =!= "", qualifier <> "-", ""] <> p["Version"] <> ".paclet",
        dependencyFailure["DependencyRelease", "The paclet site of `1` serves no archive this installation can download.", p["Name"]]]
]

$dependencyInternetMessage = "Wolfram internet access is disabled. Enable it in Internet & Mail Settings, then try again."
dependencyOffline[] := dependencyFailure["DependencyInternetDisabled", $dependencyInternetMessage]

(* The lightweight cloud metadata contains the version. Fetch the larger description and the
   archive only after the version comparison. Keep public requests independent of cloud sign-in. *)
dependencyResourceMetadata[url_] := Block[{$ProgressReporting = False, CloudConnect = ($Failed &)},
    TimeConstrained[Association[MetaInformation /. Options[CloudObject[url], MetaInformation]], 20, $Failed]
]

(* Apply the paclet manager's runtime filters to this candidate before PacletInstall,
   which also accepts archives for other systems and Wolfram versions. *)
dependencyArchiveSupportedQ[p_PacletObject] := PacletFind[p["Name"], <|"Paclets" -> {p}, "Enabled" -> All, "Internal" -> All|>] === {p}
dependencyArchiveUnsupported[p_] := dependencyFailure["DependencyArchive", "The archive for `1` `2` does not support this Wolfram system.", p["Name"], p["Version"]]

(* Bundled dependency archives live in the owner's "Dependencies" Asset directory (Assets/Dependencies). *)
dependencyBundledDirectory[owner_PacletObject] := Module[{loc = owner["Location"], asset, fallback},
    If[! StringQ[loc] || ! DirectoryQ[loc], Return[None]];
    asset = Quiet[owner["AssetLocation", "Dependencies"], {PacletObject::noasset}];
    If[StringQ[asset] && DirectoryQ[asset], Return[asset]];
    fallback = FileNameJoin[{loc, "Assets", "Dependencies"}];
    If[DirectoryQ[fallback], fallback, None]
]
dependencyBundledDirectory[_] := None

dependencyArchiveGitReceipt[archive_String] := Module[{entries, receiptEntry, text, source},
    entries = Quiet[Import[archive, {"ZIP", "FileNames"}], {Import::nffil, Import::fmterr, General::noopen}];
    If[! ListQ[entries], Return[Missing["Unknown"]]];
    receiptEntry = SelectFirst[entries, # === $dependencyGitReceipt || StringMatchQ[#, Except["/"] .. ~~ "/" ~~ $dependencyGitReceipt] &, None];
    If[! StringQ[receiptEntry], Return[Missing["Unknown"]]];
    text = Quiet[Import[archive, {"ZIP", receiptEntry, "String"}], {Import::nffil, Import::fmterr, General::noopen}];
    If[! StringQ[text], Return[Missing["Unknown"]]];
    source = Quiet[Developer`ReadRawJSONString[text], {
        Developer`ReadRawJSONString::jsonnullinput, Developer`ReadRawJSONString::jsonkeynstr,
        Developer`ReadRawJSONString::jsonhintposandchar, Developer`ReadRawJSONString::jsonunexpected,
        Developer`ReadRawJSONString::jsonsyntax, Developer`ReadRawJSONString::jsonarrayelem,
        Developer`ReadRawJSONString::jsoncolon, Developer`ReadRawJSONString::jsonobjsep,
        Developer`ReadRawJSONString::jsoninvalidval, Developer`ReadRawJSONString::jsonnum,
        Developer`ReadRawJSONString::jsonstr, Developer`ReadRawJSONString::jsonutf8
    }];
    If[AssociationQ[source] && dependencyFullCommitQ[source["Commit"]], source, Missing["Unknown"]]
]

dependencyBundledRelease[owner_, entry_] := Module[{dir = dependencyBundledDirectory[owner], name = entry["Name"],
        spec = Lookup[entry, "Version", All], gitQ = KeyExistsQ[entry, "Git"], candidates, best},
    If[! StringQ[dir], Return[None]];
    candidates = Reap[Scan[Function[file, Module[{p, receipt},
        p = Quiet[PacletObject[File[file]], {PacletObject::invfile}];
        If[MatchQ[p, _PacletObject] && p["Name"] === name && dependencyVersionQ[p["Version"]] &&
                dependencyCompatibleQ[p["Version"], spec] && dependencyArchiveSupportedQ[p],
            receipt = If[gitQ, dependencyArchiveGitReceipt[file], None];
            Which[
                gitQ && dependencyGitMatchesQ[receipt, entry],
                    Sow[<|"Version" -> p["Version"], "Archive" -> File[file], "Bundled" -> True, "BundledGit" -> receipt|>],
                dependencyCopySatisfiesQ[entry, receipt],
                    Sow[<|"Version" -> p["Version"], "Archive" -> File[file], "Bundled" -> True|>]
            ]
        ]
    ]], FileNames["*.paclet", dir]]][[2]];
    candidates = If[candidates === {}, {}, First[candidates]];
    If[candidates === {}, Return[None]];
    best = First[Sort[candidates, PacletNewerQ[#1["Version"], #2["Version"]] &]];
    best
]

(* the version of a public cloud resource, read from its metadata *)
dependencyResourceRelease[entry_] := With[{name = entry["Name"], meta = dependencyResourceMetadata[entry["Resource"]]},
    If[! AssociationQ[meta] || Lookup[meta, "Name"] =!= name || Lookup[meta, "ResourceType"] =!= "Paclet" ||
            ! MatchQ[Lookup[meta, "ResourceLocation"], CloudObject[_String, ___]],
        dependencyFailure["DependencyRelease", "Could not read the published release of `1`.", name],
        <|"Version" -> Lookup[meta, "PacletVersion"], "DescriptionURL" -> First[meta["ResourceLocation"]]|>]
]

dependencyRelease[owner_, entry_, checkUpstream_ : False] := Module[{name = entry["Name"], spec = Lookup[entry, "Version", All], release, archive, p, published, candidates, bundled},
    If[! TrueQ[checkUpstream] && ! KeyExistsQ[entry, "Archive"],
        bundled = dependencyBundledRelease[owner, entry];
        If[AssociationQ[bundled], Return[bundled]]
    ];
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
        (* a published release first, and the repository at its commit where none is compatible *)
        KeyExistsQ[entry, "Git"] && KeyExistsQ[entry, "Resource"],
            If[$AllowInternet === False, Return[dependencyOffline[]]];
            published = dependencyResourceRelease[entry];
            If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
            If[AssociationQ[published] && dependencyVersionQ[published["Version"]] && dependencyCompatibleQ[published["Version"], spec],
                published,
                With[{git = dependencyGitRelease[entry]}, If[FailureQ[git], Return[git], git]]],
        KeyExistsQ[entry, "Git"],
            If[$AllowInternet === False, Return[dependencyOffline[]]];
            With[{git = dependencyGitRelease[entry]}, If[FailureQ[git], Return[git], git]],
        KeyExistsQ[entry, "Resource"],
            If[$AllowInternet === False, Return[dependencyOffline[]]];
            With[{resource = dependencyResourceRelease[entry]}, If[FailureQ[resource], Return[resource], resource]],
        True,
            With[{refresh = $AllowInternet =!= False && TrueQ[$refreshDependencySites]},
                $refreshDependencySites = False;
                candidates = PacletFindRemote[name, UpdatePacletSites -> refresh]];
            p = If[ListQ[candidates], SelectFirst[candidates, dependencyVersionQ[#["Version"]] && dependencyCompatibleQ[#["Version"], spec] &, None], None];
            If[! MatchQ[p, _PacletObject], Return[If[$AllowInternet === False, dependencyOffline[],
                dependencyFailure["DependencyRelease", "No compatible release of `1` was found on the configured paclet sites.", name]]]];
            <|"Version" -> p["Version"], "SitePaclet" -> p|>
    ];
    If[dependencyVersionQ[release["Version"]] && dependencyCompatibleQ[release["Version"], spec], release,
        dependencyFailure["DependencyVersion", "Release `1` of `2` does not satisfy the requested version `3`.", release["Version"], name, spec]]
]

(* Text from a publisher, a repository or another kernel is parsed and never evaluated. The symbols
   it names stay in a context of their own. *)
dependencyHeldText[text_] := If[StringQ[text] && SyntaxQ[text],
    Block[{$Context = "WolframInstitute`PacletPage`DependencyText`", $ContextPath = {"System`"}}, ToExpression[text, InputForm, HoldComplete]],
    None]

(* Resolve only literal fields, keeping all other publisher expressions held. Platform resources
   published for bundled libraries select a record by $SystemID before constructing ResourceObject. *)
dependencyResourceRecord[HoldComplete[ResourceObject[a_Association]], _] := HoldComplete[a]
dependencyResourceRecord[HoldPattern[HoldComplete[If[KeyExistsQ[_Association, $SystemID],
        ResourceObject[(entries_Association)[$SystemID]], _]]], system_] :=
    FirstCase[HoldComplete[entries], HoldPattern[Rule[system, a_Association]] :> HoldComplete[a], None, {2}]
dependencyResourceRecord[_, _] := None

dependencyArchiveURL[text_String, name_, version_, system_] := Module[{record, strings, platforms, archives},
    record = dependencyResourceRecord[dependencyHeldText[text], system];
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

dependencyInstallRelease[entry_, release_] := With[{name = entry["Name"]},
    Which[
        KeyExistsQ[release, "SitePaclet"],
            Which[
                $AllowInternet === False, dependencyOffline[],
                dependencyTargetQ[], With[{url = dependencySiteArchiveURL[release["SitePaclet"]]},
                    If[FailureQ[url], url, dependencyDownloadInstall[url, name, release["Version"]]]],
                True, dependencyInstallFile[release["SitePaclet"], name, release["Version"]]],
        KeyExistsQ[release, "GitURL"] || KeyExistsQ[release, "GitArchive"],
            dependencyGitInstall[release, entry],
        KeyExistsQ[release, "Archive"],
            dependencyInstallFile[release["Archive"], name, release["Version"], Lookup[release, "BundledGit", None]],
        True,
            dependencyPublishedInstall[entry, release]
    ]
]

(* A published release that fails to install, as one with no archive for this system or one the
   cloud cannot serve, leaves an entry with a Git source to its repository at the pinned commit. *)
dependencyPublishedInstall[entry_, release_] := With[{installed = dependencyResourceInstall[entry, release]},
    If[! FailureQ[installed] || ! KeyExistsQ[entry, "Git"] || dependencyCancelledQ[], installed,
        With[{git = dependencyGitRelease[entry]},
            With[{result = If[FailureQ[git], git, dependencyGitInstall[git, entry]]},
                If[FailureQ[result] && MatchQ[result, Failure[_, _Association]],
                    Failure[First[result], Append[Last[result], "PublishedFailure" -> installed]], result]]]]
]

dependencyResourceInstall[entry_, release_] := Module[{name = entry["Name"], resource = entry["Resource"], desc = release["DescriptionURL"], text, url},
    If[! dependencyPublisherURLQ[resource, desc], Return[dependencyFailure["DependencyPublisher", "Unexpected release description address for `1`.", name]]];
    text = Block[{CloudConnect = ($Failed &)}, TimeConstrained[Import[CloudObject[desc], "Text"], 60, $Failed]];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    If[! StringQ[text], Return[dependencyFailure["DependencyRelease", "Could not read the release description for `1`.", name]]];
    url = dependencyArchiveURL[text, name, release["Version"], $SystemID];
    If[FailureQ[url], Return[url]];
    If[! dependencyPublisherURLQ[resource, url], Return[dependencyFailure["DependencyPublisher", "Unexpected archive address for `1`.", name]]];
    dependencyDownloadInstall[url, name, release["Version"]]
]

dependencyInstallFile[source_, name_, version_, git_: None] := Module[{p, result, installed},
    p = If[MatchQ[source, _PacletObject], source, PacletObject[source]];
    If[! MatchQ[p, _PacletObject] || p["Name"] =!= name || p["Version"] =!= version,
        Return[dependencyFailure["DependencyArchive", "Expected an archive containing `1` `2`.", name, version]]];
    If[dependencyCancelledQ[], Return[dependencyCancelled[]]];
    If[! dependencyArchiveSupportedQ[p], Return[dependencyArchiveUnsupported[p]]];
    $dependencyProgress["Installing", None];
    If[dependencyTargetQ[], Return[dependencyUnpackFile[source, name, version, git]]];
    If[AssociationQ[git], Return[dependencyCommitGitFile[source, name, version, git]]];
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
(*A Git repository at a commit*)

(* Git sources identify a repository, a commit, and an optional paclet subdirectory.
   Discovery reads the commit's manifest without evaluating it. Installation checks out only the
   paclet's directory using WolframInstitute/GitLink (shallow fetch + sparse path checkout),
   falling back to GitHub's archive when GitLink is unavailable, and records the full commit. *)
dependencyGitRepository[url_] := If[StringQ[url],
    Replace[StringCases[url, RegularExpression["^https://github\\.com/([A-Za-z0-9_.-]+)/([A-Za-z0-9_.-]+?)(?:\\.git)?/?$"] :> {"$1", "$2"}],
        {{{owner_String, repository_String}} :> {owner, repository}, _ -> None}],
    None
]
dependencyGitURLQ[url_] := dependencyGitRepository[url] =!= None || dependencyHTTPQ[url]
dependencyGitCanonical[url_String] := With[{gh = dependencyGitRepository[url]},
    If[gh =!= None,
        ToLowerCase[StringRiffle[gh, "/"]],
        StringReplace[ToLowerCase[StringTrim[url, "/"]], ".git" ~~ EndOfString -> ""]
    ]
]
dependencyCommitQ[commit_] := StringQ[commit] && StringMatchQ[commit, RegularExpression["[0-9a-fA-F]{7,40}"]]
dependencyFullCommitQ[commit_] := StringQ[commit] && StringMatchQ[commit, RegularExpression["[0-9a-f]{40}"]]
dependencyGitPathQ[path_] := StringQ[path] && (path === "" ||
    ! StringStartsQ[path, "/" | "\\"] && ! StringContainsQ[path, ":"] && ! MemberQ[FileNameSplit[path], ".." | "."])

(* the declared commit as addresses and receipts write it, and the directories of the declared path *)
dependencyGitCommit[entry_] := ToLowerCase[entry["Commit"]]
dependencyGitPath[entry_] := FileNameSplit[Lookup[entry, "Path", ""]]

dependencyGitLinkAvailableQ[] := Module[{loaded},
    If[NameQ["WolframInstitute`GitLink`GitSparseCheckout"] && DownValues[WolframInstitute`GitLink`GitSparseCheckout] =!= {},
        Return[True]];
    If[PacletFind["WolframInstitute/GitLink"] === {}, Return[False]];
    loaded = Quiet[
        Block[{$ContextPath = $ContextPath}, Needs["WolframInstitute`GitLink`"]],
        {Needs::nocont, Get::noopen, WolframInstitute`GitLink`InitializeGitLibrary::libnotfound}
    ];
    loaded =!= $Failed && NameQ["WolframInstitute`GitLink`GitSparseCheckout"] &&
        DownValues[WolframInstitute`GitLink`GitSparseCheckout] =!= {}
]

(* Fetch a commit using WolframInstitute/GitLink (shallow over network transports) and check out
   only the requested path patterns into targetDir. Returns <|"Source" -> ..., "Commit" -> ...|>
   or $Failed. *)
dependencyGitLinkCheckout[url_String, requested_String, path_List, targetDir_String, explicitPaths_ : Automatic] := Module[
    {gitInit, gitAddRemote, gitFetch, gitSparse, gitSHA, gitToObj, gitClose,
     repo = None, networkQ, fetchRes, paths, checked, commit, source, gitDir},
    If[! dependencyGitLinkAvailableQ[], Return[$Failed]];
    gitInit = Symbol["WolframInstitute`GitLink`GitInit"];
    gitAddRemote = Symbol["WolframInstitute`GitLink`GitAddRemote"];
    gitFetch = Symbol["WolframInstitute`GitLink`GitFetch"];
    gitSparse = Symbol["WolframInstitute`GitLink`GitSparseCheckout"];
    gitSHA = Symbol["WolframInstitute`GitLink`GitSHA"];
    gitToObj = Symbol["WolframInstitute`GitLink`ToGitObject"];
    gitClose = Symbol["WolframInstitute`GitLink`GitClose"];
    networkQ = ! StringStartsQ[url, "file://"];
    paths = Replace[explicitPaths, Automatic :> If[path === {}, All, {StringRiffle[path, "/"] <> "/*"}]];
    Quiet[
        WithCleanup[
            repo = gitInit[targetDir];
            If[! MatchQ[repo, _WolframInstitute`GitLink`GitRepo], Return[$Failed, Module]];
            If[gitAddRemote[repo, "origin", url] === $Failed, Return[$Failed, Module]];
            If[dependencyCancelledQ[], Return[dependencyCancelled[], Module]];
            $dependencyProgress["Fetching Git commit", None];
            fetchRes = Which[
                networkQ && dependencyFullCommitQ[requested],
                    With[{r = gitFetch[repo, "origin", "Depth" -> 1, "Refspecs" -> {requested}]},
                        If[! TrueQ[r] || gitToObj[repo, requested] === $Failed,
                            With[{r2 = gitFetch[repo, "origin", "Depth" -> 1]},
                                If[! TrueQ[r2] || gitToObj[repo, requested] === $Failed,
                                    If[TrueQ[r2], gitFetch[repo, "origin"], False], r2]],
                            r]],
                networkQ,
                    With[{r = gitFetch[repo, "origin", "Depth" -> 1]},
                        If[! TrueQ[r] || gitToObj[repo, requested] === $Failed,
                            If[TrueQ[r], gitFetch[repo, "origin"], False], r]],
                True,
                    gitFetch[repo, "origin"]
            ];
            If[! TrueQ[fetchRes] || gitToObj[repo, requested] === $Failed, Return[$Failed, Module]];
            If[dependencyCancelledQ[], Return[dependencyCancelled[], Module]];
            $dependencyProgress["Checking out files", None];
            checked = gitSparse[repo, requested, paths];
            If[checked === $Failed, Return[$Failed, Module]];
            commit = gitSHA[repo, "HEAD"];
            If[! dependencyFullCommitQ[commit] || ! StringStartsQ[commit, requested], Return[$Failed, Module]];
            gitClose[repo];
            repo = None;
            source = If[path === {}, targetDir, FileNameJoin[Prepend[path, targetDir]]];
            gitDir = FileNameJoin[{targetDir, ".git"}];
            If[path === {} && DirectoryQ[gitDir], DeleteDirectory[gitDir, DeleteContents -> True]];
            If[DirectoryQ[source], <|"Source" -> source, "Commit" -> commit|>, $Failed],
            If[MatchQ[repo, _WolframInstitute`GitLink`GitRepo], gitClose[repo]]
        ],
        {
            General::fetchfailed, General::noconnect, General::downloadfailed, General::updatetipsfailed,
            General::badcommitish, General::checkoutfailed, General::gitoperationfailed, General::nosrcdir,
            General::badrepo, General::badremote, General::notree
        }
    ]
]

(* A request that reports its progress leaves a script's kernel printing "Connecting" for the rest of its session. *)
dependencyGitText[url_String] := With[{response = Block[{$ProgressReporting = False},
        TimeConstrained[URLRead[HTTPRequest[url], {"StatusCode", "Body"}], 30, $Failed]]},
    Switch[If[AssociationQ[response], response["StatusCode"], None],
        200, response["Body"],
        404, Missing["NotFound"],
        _, $Failed]
]

(* Extract only literal top-level metadata. Extension rules can have their own SystemID. *)
dependencyPacletInfoFields[text_String] := Module[{rules},
    rules = Replace[dependencyHeldText[text], {
        HoldPattern[HoldComplete[___, PacletObject[Association[entries___]], ___]] :> HoldComplete[entries],
        HoldPattern[HoldComplete[___, head_Symbol[entries___], ___]] /; SymbolName[Unevaluated[head]] === "Paclet" :> HoldComplete[entries],
        _ -> HoldComplete[]}];
    Association @ Cases[rules, HoldPattern[(Rule | RuleDelayed)[key : (_String | _Symbol), value_]] :>
        With[{name = If[StringQ[Unevaluated[key]], key, SymbolName[Unevaluated[key]]]},
            If[MemberQ[{"Name", "Version", "WolframVersion", "MathematicaVersion", "SystemID", "ProductID", "EvaluationEnvironment"}, name],
                name -> Replace[HoldComplete[value], {HoldComplete[literal : (_String | All | {___String})] :> literal, _ -> Missing["Nonliteral"]}],
                Nothing]], {1}]
]

dependencyGitRelease[entry_] := Module[{name = entry["Name"], commit = dependencyGitCommit[entry],
        repository, path = dependencyGitPath[entry], directory, manifest = "PacletInfo.wl", text, fields, paclet, probeDir, checked, file},
    repository = dependencyGitRepository[entry["Git"]];
    If[repository =!= None,
        directory = Join[{"https://raw.githubusercontent.com"}, repository, {commit}, path];
        text = dependencyGitText[URLBuild[Append[directory, manifest]]];
        If[text === Missing["NotFound"],
            manifest = "PacletInfo.m";
            text = dependencyGitText[URLBuild[Append[directory, manifest]]]],
        (* Non-GitHub Git URL: fetch only the paclet manifest via GitLink sparse checkout. *)
        If[! dependencyGitLinkAvailableQ[],
            Return[dependencyFailure["DependencyRelease", "WolframInstitute/GitLink is required to read `1` from `2`.", name, entry["Git"]]]];
        probeDir = CreateDirectory[];
        WithCleanup[
            checked = dependencyGitLinkCheckout[entry["Git"], commit, path, probeDir,
                With[{prefix = If[path === {}, "", StringRiffle[path, "/"] <> "/"]}, {prefix <> "PacletInfo.wl", prefix <> "PacletInfo.m"}]];
            If[AssociationQ[checked],
                file = FileNameJoin[{checked["Source"], "PacletInfo.wl"}];
                If[! FileExistsQ[file], manifest = "PacletInfo.m"; file = FileNameJoin[{checked["Source"], "PacletInfo.m"}]];
                text = If[FileExistsQ[file], ReadString[file], $Failed],
                text = $Failed
            ],
            DeleteDirectory[probeDir, DeleteContents -> True]
        ]
    ];
    If[! StringQ[text], Return[dependencyFailure["DependencyRelease", "Could not read the paclet manifest of `1` at commit `2`.", name, commit]]];
    fields = dependencyPacletInfoFields[text];
    If[! FreeQ[fields, _Missing], Return[dependencyFailure["DependencyRelease",
        "The source manifest for `1` at commit `2` must give literal values for its name, version, and compatibility requirements.", name, commit]]];
    If[Lookup[fields, "Name", None] =!= name, Return[dependencyFailure["DependencyRelease", "The manifest at commit `1` does not declare `2`.", commit, name]]];
    If[! dependencyVersionQ[Lookup[fields, "Version", None]], Return[dependencyFailure["DependencyVersion", "The manifest for `1` at commit `2` must declare a numeric version.", name, commit]]];
    paclet = PacletObject[fields];
    If[! dependencyArchiveSupportedQ[paclet], Return[dependencyArchiveUnsupported[paclet]]];
    Join[
        <|"Version" -> fields["Version"], "GitURL" -> entry["Git"], "GitManifest" -> manifest|>,
        If[repository =!= None, <|"GitArchive" -> URLBuild[Join[{"https://codeload.github.com"}, repository, {"zip", commit}]]|>, <||>]
    ]
]

(* One operation at a time changes a library installed from Git: replacing it moves its directory
   aside. WithLock keeps its file for the hour when the kernel that holds it has gone, as after a
   quit during a download, and every later operation would wait and then report one in progress.
   The file names its process, so a lock whose process no longer runs on this machine is removed. *)
$dependencyLockWait = 10
dependencyLockAbandonedQ[file_String] := FileExistsQ[file] && With[{held = dependencyHeldText[Quiet[ReadString[file], {OpenRead::noopen, General::noopen}]]},
    FirstCase[held, HoldPattern["MachineName" -> machine_String] :> machine, None, {2}] === $MachineName &&
        With[{process = FirstCase[held, HoldPattern["ProcessID" -> id_Integer] :> id, None, {2}]},
            IntegerQ[process] && SystemProcesses["PID" -> process] === {}]]

SetAttributes[withDependencyGitLock, HoldRest]
withDependencyGitLock[name_, action_] := With[{wait = $dependencyLockWait, lock = File[FileNameJoin[{
        If[dependencyTargetQ[], $dependencyTarget, $UserBasePacletsDirectory],
        ".git-pin-" <> IntegerString[Hash[name, "SHA256"], 16] <> ".lock"}]]},
    If[dependencyLockAbandonedQ[First[lock]],
        (* A second kernel may have reclaimed the old lock and acquired a new one since the first read. *)
        WithLock[File[First[lock] <> ".reclaim"],
            If[dependencyLockAbandonedQ[First[lock]], DeleteFile[First[lock]]],
            TimeConstraint -> wait, PersistenceTime -> wait]];
    Replace[WithLock[lock, If[dependencyCancelledQ[], dependencyCancelled[], action], TimeConstraint -> wait, PersistenceTime -> 3600],
        $TimedOut :> dependencyFailure["DependencyBusy", "Another operation on `1` is in progress. Try again.", name]]]

(* The receipt travels with this installation. A copy from another source has no known revision. *)
$dependencyGitReceipt = ".pacletpage-git.json"
dependencyGitSource[entry_, commit_] := <|"Git" -> dependencyGitCanonical[entry["Git"]],
    "Path" -> StringRiffle[dependencyGitPath[entry], "/"], "Commit" -> commit|>

dependencyInstalledGit[p_] := Module[{location = p["Location"], file, source},
    If[! StringQ[location], Return[Missing["Unknown"]]];
    file = FileNameJoin[{location, $dependencyGitReceipt}];
    If[! FileExistsQ[file], Return[Missing["Unknown"]]];
    source = Quiet[Developer`ReadRawJSONFile[file], {
        Developer`ReadRawJSONFile::noopen, Developer`ReadRawJSONFile::jsonnullinput,
        Developer`ReadRawJSONFile::jsonkeynstr, Developer`ReadRawJSONFile::jsonhintposandchar,
        Developer`ReadRawJSONFile::jsonunexpected, Developer`ReadRawJSONFile::jsonsyntax,
        Developer`ReadRawJSONFile::jsonarrayelem, Developer`ReadRawJSONFile::jsoncolon,
        Developer`ReadRawJSONFile::jsonobjsep, Developer`ReadRawJSONFile::jsoninvalidval,
        Developer`ReadRawJSONFile::jsonnum, Developer`ReadRawJSONFile::jsonstr,
        Developer`ReadRawJSONFile::jsonutf8, General::noopen
    }];
    If[AssociationQ[source] && dependencyFullCommitQ[source["Commit"]], source, Missing["Unknown"]]
]
dependencyGitMatchesQ[source_, entry_] := AssociationQ[source] &&
    source === dependencyGitSource[entry, source["Commit"]] && StringStartsQ[source["Commit"], dependencyGitCommit[entry]]
(* A copy meets a Git pin by its receipt of the pinned commit. Beside a published "Resource", a copy
   without a receipt came from that release and meets the declaration as well. *)
dependencyCopySatisfiesQ[entry_, receipt_] := ! KeyExistsQ[entry, "Git"] || dependencyGitMatchesQ[receipt, entry] ||
    (KeyExistsQ[entry, "Resource"] && MissingQ[receipt])
dependencyGitReleaseQ[release_] := KeyExistsQ[release, "GitURL"] || KeyExistsQ[release, "GitArchive"] || AssociationQ[Lookup[release, "BundledGit", None]]
dependencyGitUserCopyQ[p_] := With[{location = p["Location"]}, StringQ[location] &&
    Most[FileNameSplit[ExpandFileName[location]]] === FileNameSplit[ExpandFileName[FileNameJoin[{$UserBasePacletsDirectory, "Repository"}]]]]
(* A target directory holds its own copies, which it replaces, and wins a tie with any other copy:
   only a newer copy elsewhere would still load instead. *)
dependencyGitConflict[p_, version_] := Which[
    dependencyInTargetQ[p], None,
    PacletNewerQ[p["Version"], version], dependencyFailure["DependencyVersionConflict",
        "Installed `1` `2` is newer than the pinned version `3`.", p["Name"], p["Version"], version],
    ! dependencyTargetQ[] && ! PacletNewerQ[version, p["Version"]] && ! dependencyGitUserCopyQ[p], dependencyFailure["ExternalInstallation",
        "The active copy of `1` is outside the user paclet repository.", p["Name"]],
    True, None]

(* GitHub writes the full SHA as the 40-byte comment on the ZIP end-of-central-directory record,
   including when its top directory is named after an abbreviated SHA. *)
dependencyGitArchiveCommit[zip_, requested_, top_] := Module[{stream, tail, commit},
    If[FileByteCount[zip] >= 62,
        tail = WithCleanup[stream = OpenRead[zip, BinaryFormat -> True],
            SetStreamPosition[stream, -62]; BinaryReadList[stream, "Byte", 62], Close[stream]];
        If[Take[tail, 4] === {80, 75, 5, 6} && tail[[21 ;; 22]] === {40, 0},
            commit = FromCharacterCode[Take[tail, -40]];
            If[dependencyFullCommitQ[commit] && StringStartsQ[commit, requested], Return[commit]]]];
    If[! StringQ[commit] && StringLength[requested] === 40 && StringEndsQ[FileNameTake[top], "-" <> requested], Return[requested]];
    dependencyFailure["DependencyArchive", "The downloaded archive does not identify the full commit for `1`.", requested]
]

(* Save the entire old directory on the same volume for a same-version replacement. Overlaying
   it would leave behind files removed in the new revision. Restore it if installation fails. *)
dependencyCommitGitFile[file_, name_, version_, git_] := Module[{current, conflict, location, backup, result, installed, confirmed = False},
    current = First[PacletFind[name], None];
    If[current =!= None,
        conflict = dependencyGitConflict[current, version];
        If[FailureQ[conflict], Return[conflict]];
        If[! PacletNewerQ[version, current["Version"]],
            If[dependencyInstalledGit[current] === git, Return[current]];
            location = current["Location"];
            backup = FileNameJoin[{$UserBasePacletsDirectory, CreateUUID["previous-paclet-"]}]]];
    WithCleanup[
        If[StringQ[backup] && ! StringQ[RenameDirectory[location, backup]],
            Return[dependencyFailure["DependencyInstallFailed", "Could not preserve the current installation of `1`.", name], Module]];
        result = PacletInstall[file, ForceVersionInstall -> StringQ[backup], KeepExistingVersion -> True];
        installed = First[PacletFind[name], None];
        confirmed = MatchQ[installed, _PacletObject] && installed["Version"] === version && dependencyInstalledGit[installed] === git;
        Which[confirmed, installed, StringQ[backup] && DirectoryQ[location],
            dependencyFailure["DependencyInstallFailed", "The destination changed before `1` was confirmed. Its previous installation is at `2`.", name, backup],
            MatchQ[result, _Failure], result,
            True, dependencyFailure["DependencyInstallFailed", "Could not activate the requested commit of `1`.", name]],
        If[StringQ[backup] && DirectoryQ[backup],
            If[confirmed, DeleteDirectory[backup, DeleteContents -> True],
                If[! FileExistsQ[location], RenameDirectory[backup, location]; PacletDataRebuild[]]]]]
]

(* Check out the paclet's directory at the pinned commit using WolframInstitute/GitLink
   (shallow fetch + sparse checkout), falling back to GitHub's archive when GitLink is unavailable
   or cannot reach the repository. That directory is packed with its receipt and installed. *)
dependencyGitInstall[release_, entry_] := Module[{name = entry["Name"], path = dependencyGitPath[entry], dir = CreateDirectory[],
        gitTree, zip, tree, checked = $Failed, download, top, source = None, archive, commit = $Failed, git, receipt},
    gitTree = FileNameJoin[{dir, "git"}];
    zip = FileNameJoin[{dir, "tree.zip"}];
    tree = FileNameJoin[{dir, "tree"}];
    WithCleanup[
        If[KeyExistsQ[release, "GitURL"] && dependencyGitLinkAvailableQ[],
            checked = dependencyGitLinkCheckout[release["GitURL"], dependencyGitCommit[entry], path, gitTree];
            If[MatchQ[checked, _Failure], Return[checked, Module]];
            If[AssociationQ[checked] && FileExistsQ[FileNameJoin[{checked["Source"], release["GitManifest"]}]],
                source = checked["Source"];
                commit = checked["Commit"]
            ]
        ];
        If[! StringQ[source],
            If[! KeyExistsQ[release, "GitArchive"],
                Return[dependencyFailure["DependencyArchive", "Could not check out `1` at commit `2` from `3`.",
                    name, dependencyGitCommit[entry], Lookup[release, "GitURL", entry["Git"]]], Module]];
            download = dependencyDownloaded[release["GitArchive"], zip];
            If[FailureQ[download], Return[download, Module]];
            If[dependencyCancelledQ[], Return[dependencyCancelled[], Module]];
            ExtractArchive[zip, tree, Sequence @@ If[path === {}, {}, {"*/" <> StringRiffle[path, "/"] <> "/*"}]];
            top = FileNames["*", tree];
            source = If[MatchQ[top, {_String?DirectoryQ}], FileNameJoin[Join[top, path]], None];
            If[! StringQ[source] || ! FileExistsQ[FileNameJoin[{source, release["GitManifest"]}]],
                Return[dependencyFailure["DependencyArchive", "The commit's tree has no paclet at `1`.", StringRiffle[path, "/"]], Module]];
            commit = dependencyGitArchiveCommit[zip, dependencyGitCommit[entry], First[top]];
            If[FailureQ[commit], Return[commit, Module]]
        ];
        git = dependencyGitSource[entry, commit];
        receipt = FileNameJoin[{source, $dependencyGitReceipt}];
        If[FileExistsQ[receipt] || ! StringQ[Export[receipt, git, "RawJSON"]],
            Return[dependencyFailure["DependencyArchive", "Could not record the Git source for `1`.", name], Module]];
        archive = CreatePacletArchive[source, dir];
        If[! StringQ[archive], Return[dependencyFailure["DependencyArchive", "Could not pack `1` from the commit's tree.", name], Module]];
        dependencyInstallFile[File[archive], name, release["Version"], git],
        DeleteDirectory[dir, DeleteContents -> True]
    ]
]
