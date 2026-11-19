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

dependencyDetails[row_, label_] := Module[{size, license, requirement, links, versions, metadata},
    size = Replace[Lookup[row, "DownloadSize", None], a_Association :> Lookup[a, $SystemID, None]];
    license = Lookup[row, "License", None];
    requirement = Lookup[row, "Version", All];
    versions = Row[{"Installed: ", Replace[row["InstalledVersion"], _Missing -> "Not installed"], Spacer[16],
        "Latest: ", Switch[Lookup[row, "LatestVersion", None], _String, row["LatestVersion"], _Failure, "Unavailable", _, "Not checked"]}];
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

(* The front end owns the selector so it can paint and animate while the kernel is checking a library. *)
SetAttributes[dependencyCheckControl, HoldRest]
dependencyCheckControl[label_String, hint_String, start_, operation_, cancelled_] := With[{
        spinner = ProgressIndicator[Appearance -> "Necklace", ImageSize -> {14, 14}], size = {140, 26}},
    PaneSelector[{
        None -> Tooltip[Button[label, start, Enabled -> Dynamic[operation === None], Method -> "Preemptive", ImageSize -> size], hint],
        label -> Tooltip[Button[Row[{spinner, Spacer[6], "Cancel check"}],
            cancelled = True; CurrentValue[ButtonNotebook[], {TaggingRules, "DependencyCheck"}] = "Stopping " <> label,
            Method -> "Preemptive", ImageSize -> size], "Stop after the current library."],
        ("Stopping " <> label) -> Button[Row[{spinner, Spacer[6], "Stopping…"}], Null, Enabled -> False, ImageSize -> size]
    }, Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyCheck"}]],
        Button[label, Null, Enabled -> False, ImageSize -> size], ImageSize -> All]
]

(* Return to the front end before a remote lookup or native runtime check occupies the kernel. *)
dependencyScheduleCheck[nb_NotebookObject, label_String, check_, finish_] := Module[{frontEnd = $FrontEnd, cleanup, task},
    cleanup[] := (finish[]; If[MemberQ[Notebooks[], nb], CurrentValue[nb, {TaggingRules, "DependencyCheck"}] = None]);
    CurrentValue[nb, {TaggingRules, "DependencyCheck"}] = label;
    task = SessionSubmit[ScheduledTask[Block[{$FrontEnd = frontEnd},
        WithCleanup[If[MemberQ[Notebooks[], nb], check[]], cleanup[]]], {0.05, 1}]];
    If[! MatchQ[task, _TaskObject], cleanup[]];
    task
]

(* Redraw the rows only when their data change. Progress updates touch just the footer. *)
dependencyDialogContent[owner_, commandOwner_, initial_] := With[{
        initialRows = dependencyDialogRows[initial], heading = dependencyDialogHeading[initial], packageFile = FileNameJoin[{$packageDirectory, "PacletPage.wl"}]}, DynamicModule[
    {rows = initialRows, notices = <||>, copied = None, feedback = "", operation = None, cancelled = False, stage = "", fraction = None,
        run, startCheck, refresh, perform, rowPanel},
    Framed[Column[{
        Column[{Style[heading, 18, Bold], "Install, upgrade, or remove libraries as needed."}, Alignment -> Left, Spacings -> .6],
        Dynamic[If[rows === <||>, "This paclet declares no dependencies.",
            Pane[Column[KeyValueMap[rowPanel, rows],
                    ItemSize -> {{Scaled[1]}}, Spacings -> 1.2],
                ImageSize -> {Full, UpTo[400]}, Scrollbars -> {False, Automatic}]], TrackedSymbols :> {rows, notices, copied}],
        Dynamic[If[MemberQ[{"Install", "Update", "Uninstall"}, operation], Row[{stage, Spacer[12], If[NumberQ[fraction], ProgressIndicator[fraction, {0, 1}],
                ProgressIndicator[Appearance -> "Indeterminate"]], Spacer[12],
            Button["Cancel", cancelled = True; cancelDependencyDownload[], Method -> "Preemptive",
                Enabled -> Dynamic[! cancelled && MemberQ[{"Checking release", "Downloading", "Retrying"}, stage]]]}], Spacer[{0, 0}]],
            TrackedSymbols :> {operation, stage, fraction}],
        Grid[{{Grid[{{
            dependencyCheckControl["Check readiness", "Test whether installed libraries work in this kernel.",
                startCheck[True, ButtonNotebook[]], operation, cancelled],
            dependencyCheckControl["Check for updates", "Look up the latest compatible versions.",
                startCheck[False, ButtonNotebook[]], operation, cancelled],
            dependencyMetaStyle[Dynamic[feedback, TrackedSymbols :> {feedback}]]}}, Alignment -> {Left, Center}, Spacings -> {.6, 0}],
            DefaultButton["Close", cancelled = True; If[operation =!= None, cancelDependencyDownload[]]; dependencyRemember[owner["Name"]]; DialogReturn[], ImageSize -> {90, Automatic}, Method -> "Preemptive"]}},
            ItemSize -> {{Scaled[.72], Scaled[.28]}}, Alignment -> {{Left, Right}, Center}]
    }, ItemSize -> {{Scaled[1]}}, Spacings -> 1.3, Alignment -> Left, Dividers -> {False, {False, False, False, GrayLevel[.85], False}}], FrameMargins -> 16, FrameStyle -> None],
    Initialization :> (
        If[DownValues[dependencyRowActions] === {}, Block[{$ContextPath = $ContextPath, $AllowInternet = False}, Get[packageFile]]];
        SetAttributes[run, HoldRest];
        run[caption_, expr_] := If[operation === None, WithCleanup[
            operation = caption; cancelled = False; feedback = ""; stage = caption; fraction = None; FinishDynamic[];
            Block[{dependencyCancelledQ = Function[Null, cancelled],
                    $dependencyProgress = Function[{step, value}, stage = step; fraction = value; FinishDynamic[]]}, expr],
            operation = None; stage = ""; fraction = None; FinishDynamic[]]];
        startCheck[probe_, nb_NotebookObject] := If[operation === None, With[{label = If[probe, "Check readiness", "Check for updates"]},
            operation = label; cancelled = False; feedback = "";
            With[{task = dependencyScheduleCheck[nb, label,
                    Function[Null, If[cancelled, feedback = "Check stopped.", Block[{dependencyCancelledQ = Function[Null, cancelled]}, refresh[probe]]]],
                    Function[Null, operation = None]]},
                If[! MatchQ[task, _TaskObject], feedback = "Could not start this check. Try again."];
                task]
        ]];
        refresh[probe_] := With[{fresh = PacletDependencyInformation[owner, Keys[rows], "CheckReadiness" -> probe, "CheckUpdates" -> ! probe, "Output" -> "Association"]},
                If[AssociationQ[fresh],
                    rows = AssociationMap[Function[name, With[{old = rows[name], new = fresh[name]},
                        If[! AssociationQ[new], old, dependencyDialogRow[Join[KeyTake[old, {"LatestVersion", "Description"}], new,
                            If[! probe && new["Status"] === "Installed" && old["InstalledVersion"] === new["InstalledVersion"],
                                KeyTake[old, {"Status", "ReadinessFailure"}], <||>]], old["Label"]]]]], Keys[fresh]];
                    notices = (#["Message"] & /@ Select[fresh, FailureQ[#] && First[#] =!= "DependencyCancelled" &]);
                    feedback = Which[cancelled, "Check stopped.", probe, dependencyReadinessSummary[fresh], True, ""],
                    If[FailureQ[fresh], feedback = fresh["Message"]]]];
        perform[name_, action_] := run[action, Module[{result, fresh},
            result = dependencyAction[action][owner, name];
            result = If[AssociationQ[result], result[name], result];
            fresh = PacletDependencyInformation[owner, name, "Output" -> "Association"];
            If[AssociationQ[fresh], rows[name] = dependencyDialogRow[Join[KeyTake[rows[name], {"LatestVersion", "Description"}], fresh[name]], rows[name]["Label"]]];
            If[rows[name]["Status"] === "Installed" && KeyExistsQ[rows[name], "Check"],
                Which[MatchQ[result, Failure["DependencySetupRequired", _]], rows[name] = Append[rows[name], "Status" -> "Needs setup"],
                    MatchQ[result, _PacletObject], rows[name] = Append[rows[name], "Status" -> "Ready"]]];
            notices[name] = Which[FailureQ[result], result["Message"],
                rows[name]["Status"] === "Restart required", "Restart the kernel to use the changed library.",
                action === "Uninstall", "Removed from the user repository.",
                StringQ[rows[name]["InstalledVersion"]] && action === "Update", "Update check complete.",
                True, "Installation complete."];
            copied = None]];
        rowPanel[name_, row_] := With[{actions = row["Actions"], supported = row["Status"] =!= "Unsupported"},
            Grid[{{Pane[row["Details"], ImageSize -> Full, Alignment -> Left],
                Column[DeleteCases[{
                    Row[Riffle[Map[Function[action, Button[If[action === "Update", "Upgrade", action], perform[name, action], ImageSize -> {100, Automatic}, Method -> "Queued",
                        Enabled -> Dynamic[operation === None && (supported || action === "Uninstall")]]],
                        actions], Spacer[6]]],
                    If[actions === {}, Nothing, With[{action = First[actions]},
                        Button[If[copied === name, "Copied", "Copy " <> ToLowerCase[action] <> " command"],
                            CopyToClipboard[dependencyCommand[commandOwner, action, name]]; copied = name,
                            Appearance -> "Frameless", BaseStyle -> "Hyperlink", Enabled -> Dynamic[operation === None && (supported || action === "Uninstall")]]]],
                    Lookup[notices, name, Which[
                        FailureQ[row["ReadinessFailure"]], row["ReadinessFailure"]["Message"],
                        FailureQ[row["LatestVersion"]], row["LatestVersion"]["Message"],
                        ! supported, "No release for " <> $SystemID <> ".",
                        row["Status"] === "Restart required", "Restart the kernel to use the changed library.", True, Nothing]]
                }, Nothing], Alignment -> Right, Spacings -> .5]}}, ItemSize -> {{Scaled[.65], Scaled[.35]}}, Alignment -> {{Left, Right}, Top}]]
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
        WindowFrameElements -> {"CloseBox", "ResizeArea"}, TaggingRules -> <|"DependencyCheck" -> None|>,
        NotebookEventActions -> {"WindowClose" :> (dependencyRemember[name]; KeyDropFrom[$dependencyDialogs, {location}]), PassEventsDown -> True}]];
    If[MatchQ[dialog, _NotebookObject], AssociateTo[$dependencyDialogs, key -> dialog]];
    dialog
]
