(* ::Package:: *)

$dependencyDialogs = <||>
dependencyDesktopQ[] := Block[{$AllowInternet = False, $AllowDataUpdates = False},
    MatchQ[$FrontEnd, _FrontEndObject] && ! TrueQ[$CloudEvaluation] && $KernelID === 0]
dependencyPreference[name_] := "WolframInstitute/PacletPage/DependencyDialog/" <> name
dependencyRemember[name_] := (PersistentSymbol[dependencyPreference[name], "Local"] = True)

PacletDependencyDialog::available = "`1` has optional dependencies. Run PacletDependencyDialog[`2`] to review them."
PacletDependencyDialog[spec : (_String | File[_String] | _PacletObject), "Notify"] := Module[{owner, rows},
    If[! dependencyDesktopQ[] || ! TrueQ[$Notebooks] || TrueQ[$BatchInput] || MemberQ[{"true", "1"}, Environment["CI"]], Return[Null]];
    owner = dependencyOwner[spec];
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    If[! TrueQ[PersistentSymbol[dependencyPreference[owner["Name"]], "Local"]],
        rows = PacletDependencyInformation[owner, "Output" -> "Association"];
        If[FailureQ[rows], Return[rows]];
        If[AnyTrue[Values[rows], ! #["Required"] && #["Status"] === "Not installed" &],
            Message[PacletDependencyDialog::available, owner["Name"], ToString[dependencyCommandOwner[spec, owner], InputForm]]]];
    Null
]

dependencyCommandOwner[spec_String, _] := If[DirectoryQ[spec], ExpandFileName[spec], spec]
dependencyCommandOwner[_, owner_] := Replace[owner["Location"], Except[_String] :> owner]
dependencyAction["Install"] = InstallPacletDependencies
dependencyAction["Update"] = UpdatePacletDependencies
dependencyAction["Uninstall"] = UninstallPacletDependencies
dependencyCommand[owner_, action_, name_] := ToString[dependencyAction[action], InputForm] <> "[" <> ToString[owner, InputForm] <> ", " <> ToString[name, InputForm] <> "]"

dependencyDialogHeading[rows_] := If[AnyTrue[Values[rows], TrueQ[#["Required"]] &], "Library dependencies", "Optional libraries"]
dependencyShortName[name_] := Last[StringSplit[name, "/"]]
dependencyMetaStyle[expr_] := Style[expr, Small, LightDarkSwitched[GrayLevel[.35], GrayLevel[.72]]]

dependencyCompactFooterQ[width_, zoom_] := width < 520 zoom

(* Keep the heading and footer visible when the user gives the window a fixed height.
   An automatically sized window fits a short list and caps a long one at 400 points. *)
dependencyListSize[{width_, height_}, zoom_] := {Full, UpTo[If[NumberQ[height],
    Max[24, height/zoom - If[dependencyCompactFooterQ[width, zoom], 174, 136]], 400]]}

dependencyDetails[row_, label_] := Module[{size, license, requirement, links, versions, metadata},
    size = Replace[Lookup[row, "DownloadSize", None], a_Association :> Lookup[a, $SystemID, None]];
    license = Lookup[row, "License", None];
    requirement = Lookup[row, "Version", All];
    versions = Row[{"Installed: ", Replace[row["InstalledVersion"], _Missing -> "Not installed"], Spacer[16],
        "Latest: ", With[{name = row["Name"]}, Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyVersions", name}]]]}];
    metadata = {If[StringQ[size], "Download: " <> size, Nothing], If[StringQ[license], "License: " <> license, Nothing],
        If[TrueQ[row["Required"]], "Required", Nothing], If[StringQ[requirement] && requirement =!= "*", "Requires " <> requirement, Nothing]};
    links = Join[If[StringQ[row["URL"]], {Hyperlink["Setup instructions", row["URL"]]}, {}],
        KeyValueMap[Hyperlink, Lookup[row, "SetupLinks", <||>]]];
    Column[{Tooltip[Style[label, 14, Bold], row["Name"]], dependencyMetaStyle[versions], Lookup[row, "Description", Nothing],
        If[metadata === {}, Nothing, dependencyMetaStyle[Row[Riffle[metadata, Spacer[12]]]]],
        If[links === {}, Nothing, Row[Riffle[links, Spacer[12]]]]}, Alignment -> Left, Spacings -> .5]
]

dependencyRowActions[row_] := If[! StringQ[row["InstalledVersion"]], {"Install"}, {
    If[dependencyRemovableQ[row], "Uninstall", Nothing],
    If[row["Status"] =!= "Unsupported" && StringQ[row["LatestVersion"]] &&
        PacletNewerQ[row["LatestVersion"], row["InstalledVersion"]], "Update", Nothing]}]

(* Cancellation belongs to the action. Always read the local installation state afterwards. *)
dependencyDialogAction[owner_, name_, action_, cancelledQ_, progress_] := Module[{result, fresh},
    result = Block[{dependencyCancelledQ = cancelledQ, $dependencyProgress = progress},
        dependencyAction[action][owner, name]];
    fresh = PacletDependencyInformation[owner, name, "Output" -> "Association"];
    {If[AssociationQ[result], result[name], result], If[AssociationQ[fresh], fresh[name], fresh]}
]

dependencyActionNotice[action_, result_, fresh_] := Which[
    MatchQ[result, Failure["DependencyCancelled", _]], "Cancelled.",
    MatchQ[result, _Failure], result["Message"],
    MatchQ[fresh, _Failure], fresh["Message"],
    ! AssociationQ[fresh] || ! If[action === "Uninstall", result === Null,
        MatchQ[result, _PacletObject] && StringQ[fresh["InstalledVersion"]]],
        "Could not confirm the change. Reopen the dialog to check the installed version.",
    fresh["Status"] === "Restart required", "Restart the kernel to use the changed library.",
    action === "Uninstall", "Removed from the user repository.",
    action === "Update", "Version " <> fresh["InstalledVersion"] <> " is installed.",
    True, "Installation complete."
]

(* Prepare the display in the owning kernel, keeping the full identifier for each action. *)
dependencyDialogRow[row_, label_] := Join[row, <|"Label" -> label, "Details" -> dependencyDetails[row, label],
    "Actions" -> dependencyRowActions[row]|>]

dependencyDialogRows[rows_] := With[{counts = Counts[dependencyShortName /@ Keys[rows]]},
    Association@KeyValueMap[Function[{name, row}, With[{short = dependencyShortName[name]},
        name -> dependencyDialogRow[row, If[counts[short] === 1, short, name]]]], rows]
]

dependencyReadinessSummary[rows_] := Module[{states, passed, failed, skipped},
    states = #["Status"] & /@ Values[Select[rows, AssociationQ]];
    passed = Count[states, "Ready"];
    failed = Count[states, "Needs setup"];
    skipped = Length[states] - passed - failed;
    If[states === {}, "No libraries to check.", StringRiffle[{
        If[passed > 0, ToString[passed] <> " passed", Nothing],
        If[failed > 0, ToString[failed] <> " failed", Nothing],
        If[skipped > 0, ToString[skipped] <> " skipped", Nothing]}, ", "]]
]

dependencySetActivity[nb_, target_] := (CurrentValue[nb, {TaggingRules, "DependencyActivity"}] =
    <|"Target" -> target, "Idle" -> (target === None)|>)

dependencyProgressState[stage_, fraction_] := With[{
        percent = If[stage === "Downloading" && NumberQ[fraction], Floor[100 Clip[fraction, {0, 1}]], None]},
    <|"Label" -> If[IntegerQ[percent], "Downloading " <> ToString[percent] <> "%", stage <> "…"],
      "Percent" -> percent, "Cancellable" -> MemberQ[{"Preparing", "Checking release", "Downloading", "Retrying"}, stage]|>
]

SetAttributes[dependencyProgressView, HoldAll]
dependencyProgressView[cancel_] := With[{
        label = Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyProgress", "Label"}]],
        percent = Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyProgress", "Percent"}]]},
    Column[{
        PaneSelector[{None -> Row[{ProgressIndicator[Appearance -> "Necklace", ImageSize -> {14, 14}], Spacer[6], label}]},
            percent, Column[{label, ProgressIndicator[percent, {0, 100}, ImageSize -> {140, 8}]}, Alignment -> Right, Spacings -> .2],
            Alignment -> Right],
        PaneSelector[{True -> Button["Cancel", cancel, Method -> "Preemptive", ImageSize -> {100, Automatic}]},
            Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyProgress", "Cancellable"}]], Spacer[{0, 0}]]
    }, Alignment -> Right, Spacings -> .45]
]

(* The front end owns the selector so it can paint and animate while the kernel is checking a library. *)
SetAttributes[dependencyCheckControl, HoldRest]
dependencyCheckControl[label_String, hint_String, start_, cancelled_] := With[{
        spinner = ProgressIndicator[Appearance -> "Necklace", ImageSize -> {14, 14}], size = {140, 26}},
    PaneSelector[{
        None -> Tooltip[Button[label, start, Method -> "Queued", ImageSize -> size], hint],
        label -> Tooltip[Button[Row[{spinner, Spacer[6], "Cancel check"}],
            cancelled = True; dependencySetActivity[ButtonNotebook[], "Stopping " <> label],
            Method -> "Preemptive", ImageSize -> size], "Stop after the current library."],
        ("Stopping " <> label) -> Button[Row[{spinner, Spacer[6], "Stopping…"}], Null, Enabled -> False, ImageSize -> size]
    }, Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyActivity", "Target"}]],
        Button[label, Null, Enabled -> False, ImageSize -> size], ImageSize -> All]
]

(* The start button runs on the main evaluation link. The front end can still paint the
   selector and send Cancel on the preemptive link while the main kernel waits for I/O. *)
SetAttributes[dependencyRunCheck, HoldRest]
dependencyRunCheck[nb_NotebookObject, label_String, work_, finish_] := Module[{summary = ""},
    WithCleanup[
        If[MemberQ[Notebooks[], nb],
            dependencySetActivity[nb, label];
            summary = work],
        finish;
        If[MemberQ[Notebooks[], nb],
            CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}] = If[StringQ[summary], summary, ""];
            dependencySetActivity[nb, None]]]
]

(* The front end can display completed versions and the active library while the next lookup is running. *)
dependencyDialogContent[owner_, commandOwner_, initial_] := With[{
        initialRows = dependencyDialogRows[initial], heading = dependencyDialogHeading[initial], packageFile = FileNameJoin[{$packageDirectory, "PacletPage.wl"}]}, DynamicModule[
    {rows = initialRows, notices = <||>, copied = None, operation = None, cancelled = False,
        startCheck, refresh, perform, cancelTransfer, rowPanel},
    Framed[Column[{
        Column[{Style[heading, 18, Bold], PaneSelector[{True -> $dependencyInternetMessage},
            Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyInternetDisabled"}]],
            "Install, upgrade, or remove libraries as needed."]}, Alignment -> Left, Spacings -> .6],
        If[initialRows === <||>, "This paclet declares no dependencies.",
            Pane[Dynamic[Column[KeyValueMap[rowPanel, rows], ItemSize -> {{Scaled[1]}}, Spacings -> 1.2],
                    TrackedSymbols :> {rows, notices}],
                ImageSize -> Dynamic[dependencyListSize[CurrentValue[EvaluationNotebook[], WindowSize],
                    AbsoluteCurrentValue[EvaluationNotebook[], Magnification]]],
                Scrollbars -> {False, Automatic}, AppearanceElements -> None, Alignment -> {Left, Top}]],
        With[{
            readiness = dependencyCheckControl["Check readiness", "Test whether installed libraries work in this kernel.",
                startCheck[True, ButtonNotebook[]], cancelled],
            updates = dependencyCheckControl["Check for updates", "Look up the latest compatible versions.",
                startCheck[False, ButtonNotebook[]], cancelled],
            feedback = dependencyMetaStyle[Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyCheckFeedback"}]]],
            close = DefaultButton["Close", cancelled = True; If[operation =!= None, cancelDependencyDownload[]];
                dependencyRemember[owner["Name"]]; DialogReturn[], ImageSize -> {90, Automatic}, Method -> "Preemptive"]},
            PaneSelector[{True -> Grid[{{readiness, feedback}, {updates, close}}, ItemSize -> {{Scaled[.58], Scaled[.42]}},
                    Alignment -> {{Left, Right}, Center}, Spacings -> {0, .4}],
                False -> Grid[{{Grid[{{readiness, updates, feedback}}, Alignment -> {Left, Center}, Spacings -> {.6, 0}], close}},
                    ItemSize -> {{Scaled[.82], Scaled[.18]}}, Alignment -> {{Left, Right}, Center}]},
                Dynamic[dependencyCompactFooterQ[First[CurrentValue[EvaluationNotebook[], WindowSize]],
                    AbsoluteCurrentValue[EvaluationNotebook[], Magnification]]], ImageSize -> {Full, Automatic}]]
    }, ItemSize -> {{Scaled[1]}}, Spacings -> 1.3, Alignment -> Left, Dividers -> {False, {False, False, GrayLevel[.85], False}}], FrameMargins -> 16, FrameStyle -> None],
    Initialization :> (
        If[DownValues[dependencyRowActions] === {}, Block[{$ContextPath = $ContextPath, $AllowInternet = False}, Get[packageFile]]];
        startCheck[probe_, nb_NotebookObject] := If[operation === None, With[{label = If[probe, "Check readiness", "Check for updates"]},
            dependencyRunCheck[nb, label,
                operation = label; cancelled = False;
                Block[{dependencyCancelledQ = Function[Null, cancelled || ! MemberQ[Notebooks[], nb]]}, refresh[probe, nb]],
                operation = None]
        ]];
        refresh[probe_, nb_] := Module[{outcome, refreshed = <||>},
            outcome = withDependencySelection[owner, Keys[rows], Function[{paclet, entries}, Module[{installed, old, new},
                installed = dependencyInstallations[Keys[entries]];
                Do[
                    If[dependencyCancelledQ[], Break[]];
                    old = rows[name];
                    CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}] = old["Label"] <> "…";
                    FinishDynamic[];
                    new = dependencyInfo[paclet, entries[name], installed[name], probe, ! probe];
                    refreshed[name] = dependencyDialogRow[Join[KeyTake[old, {"LatestVersion", "Description"}], new,
                        If[! probe && new["Status"] === "Installed" && old["InstalledVersion"] === new["InstalledVersion"],
                            KeyTake[old, {"Status", "ReadinessFailure"}], <||>]], old["Label"]];
                    If[KeyExistsQ[new, "LatestVersion"],
                        CurrentValue[nb, {TaggingRules, "DependencyVersions", name}] = Replace[new["LatestVersion"],
                            {Failure["DependencyInternetDisabled", _] -> "Offline", _Failure -> "Unavailable"}]],
                    {name, Keys[entries]}]
            ]]];
            If[refreshed =!= <||>, rows = Join[rows, refreshed]; notices = KeyDrop[notices, Keys[refreshed]]];
            If[! probe && MemberQ[Notebooks[], nb],
                CurrentValue[nb, {TaggingRules, "DependencyInternetDisabled"}] =
                    MemberQ[Lookup[Values[rows], "LatestVersion", None], Failure["DependencyInternetDisabled", _]]];
            Which[FailureQ[outcome], outcome["Message"], cancelled, "Check stopped.",
                probe, dependencyReadinessSummary[rows], True, ""]
        ];
        cancelTransfer[nb_NotebookObject] := If[TrueQ[CurrentValue[nb, {TaggingRules, "DependencyProgress", "Cancellable"}]],
            cancelled = True;
            CurrentValue[nb, {TaggingRules, "DependencyProgress"}] = dependencyProgressState["Cancelling", None];
            cancelDependencyDownload[]];
        perform[name_, action_, nb_NotebookObject] := If[operation === None && MemberQ[Notebooks[], nb],
            Module[{result, fresh, previous = None, progress},
                (* Only a new stage or whole percentage needs a front-end round trip. *)
                progress[step_, fraction_] := With[{state = dependencyProgressState[step, fraction]},
                    If[! cancelled && state =!= previous,
                        previous = state;
                        CurrentValue[nb, {TaggingRules, "DependencyProgress"}] = state]];
                WithCleanup[
                    operation = action; cancelled = False; copied = None;
                    CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}] = "";
                    progress[If[action === "Uninstall", "Uninstalling", "Preparing"], None];
                    dependencySetActivity[nb, name];
                    {result, fresh} = dependencyDialogAction[owner, name, action, Function[Null, cancelled], progress];
                    If[AssociationQ[fresh], rows[name] = dependencyDialogRow[Join[KeyTake[rows[name], {"LatestVersion", "Description"}], fresh], rows[name]["Label"]]];
                    If[rows[name]["Status"] === "Installed" && KeyExistsQ[rows[name], "Check"],
                        Which[MatchQ[result, Failure["DependencySetupRequired", _]], rows[name] = Append[rows[name], "Status" -> "Needs setup"],
                            MatchQ[result, _PacletObject], rows[name] = Append[rows[name], "Status" -> "Ready"]]];
                    notices[name] = dependencyActionNotice[action, result, fresh],
                    operation = None; If[MemberQ[Notebooks[], nb], dependencySetActivity[nb, None]]]
            ]];
        rowPanel[name_, row_] := With[{actions = row["Actions"], supported = row["Status"] =!= "Unsupported",
                idle = Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyActivity", "Idle"}]]},
            Grid[{{Pane[row["Details"], ImageSize -> Full, Alignment -> Left],
                PaneSelector[{name -> dependencyProgressView[cancelTransfer[ButtonNotebook[]]]},
                    Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyActivity", "Target"}]],
                    Column[DeleteCases[{
                        Row[Riffle[Map[Function[action, Button[If[action === "Update", "Upgrade", action],
                            perform[name, action, ButtonNotebook[]], ImageSize -> {100, Automatic}, Method -> "Queued",
                            Enabled -> If[supported || action === "Uninstall", idle, False]]], actions], Spacer[6]]],
                        If[actions === {}, Nothing, With[{action = First[actions]},
                            Button[Dynamic[If[copied === name, "Copied", "Copy " <> ToLowerCase[action] <> " command"], TrackedSymbols :> {copied}],
                                CopyToClipboard[dependencyCommand[commandOwner, action, name]]; copied = name,
                                Appearance -> "Frameless", BaseStyle -> "Hyperlink", Enabled -> If[supported || action === "Uninstall", idle, False]]]],
                        Lookup[notices, name, Which[
                            FailureQ[row["ReadinessFailure"]], row["ReadinessFailure"]["Message"],
                            MatchQ[row["LatestVersion"], Failure[Except["DependencyInternetDisabled"], _]], row["LatestVersion"]["Message"],
                            ! supported, "No release for " <> $SystemID <> ".",
                            row["Status"] === "Restart required", "Restart the kernel to use the changed library.", True, Nothing]]
                    }, Nothing], Alignment -> Right, Spacings -> .5], Alignment -> Right]}},
                ItemSize -> {{Scaled[.65], Scaled[.35]}}, Alignment -> {{Left, Right}, Top}]]
    ),
    Deinitialization :> (cancelled = True; If[operation =!= None, cancelDependencyDownload[]]),
    SynchronousInitialization -> True
]]

PacletDependencyDialog[spec : (_String | File[_String] | _PacletObject), scope : (All | "Optional") : All] := Module[{owner, key, dialog, rows},
    If[! dependencyDesktopQ[], Return[dependencyFailure["NotebookRequired", "Open a desktop notebook to use the dependency dialog. Scripts can use PacletDependencyInformation and the install, update and uninstall functions."]]];
    owner = dependencyOwner[spec];
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    key = {dependencyCommandOwner[owner, owner], scope};
    dialog = $dependencyDialogs[key];
    If[MemberQ[Notebooks[], dialog], SetSelectedNotebook[dialog]; Return[dialog]];
    rows = PacletDependencyInformation[owner, "Output" -> "Association"];
    If[FailureQ[rows], Return[rows]];
    If[scope === "Optional", rows = Select[rows, ! TrueQ[#["Required"]] &]];
    dialog = With[{name = owner["Name"], location = key}, CreateDialog[dependencyDialogContent[owner, dependencyCommandOwner[spec, owner], rows],
        WindowTitle -> dependencyShortName[name] <> ": " <> dependencyDialogHeading[rows], WindowSize -> {740, All}, WindowFrame -> "Normal",
        WindowFrameElements -> {"CloseBox", "ResizeArea"}, TaggingRules -> <|"DependencyActivity" -> <|"Target" -> None, "Idle" -> True|>,
            "DependencyCheckFeedback" -> "", "DependencyInternetDisabled" -> False,
            "DependencyVersions" -> AssociationMap["Not checked" &, Keys[rows]]|>,
        NotebookEventActions -> {"WindowClose" :> (dependencyRemember[name]; KeyDropFrom[$dependencyDialogs, {location}]), PassEventsDown -> True}]];
    If[MatchQ[dialog, _NotebookObject], AssociateTo[$dependencyDialogs, key -> dialog]];
    dialog
]
