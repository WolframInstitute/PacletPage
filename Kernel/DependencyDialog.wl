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

(* Both buttons occupy the same space while idle, checking, and stopping. *)
SetAttributes[dependencyCheckControl, HoldRest]
dependencyCheckControl[label_String, hint_String, start_, operation_, cancelled_] := PaneSelector[{
    False -> Tooltip[Button[label, start, Enabled -> Dynamic[operation === None], Method -> "Queued",
        ImageSize -> {128, 26}], hint],
    True -> Tooltip[Button[Row[{ProgressIndicator[Appearance -> "Necklace", ImageSize -> {14, 14}], Spacer[6],
            Dynamic[If[cancelled, "Stopping…", "Cancel check"]]}],
        cancelled = True, Enabled -> Dynamic[! cancelled], Method -> "Preemptive", ImageSize -> {128, 26}],
        "Stop after the current library."]
}, Dynamic[operation === label], ImageSize -> All]

(* Redraw the rows only when their data change. Progress updates touch just the footer. *)
dependencyDialogContent[owner_, commandOwner_, initial_] := With[{
        initialRows = dependencyDialogRows[initial], heading = dependencyDialogHeading[initial], packageFile = FileNameJoin[{$packageDirectory, "PacletPage.wl"}]}, DynamicModule[
    {rows = initialRows, notices = <||>, copied = None, operation = None, cancelled = False, stage = "", fraction = None,
        run, refresh, perform, rowPanel},
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
        Grid[{{Row[{
            dependencyCheckControl["Check readiness", "Test whether installed libraries work in this kernel.",
                refresh[True], operation, cancelled], Spacer[8],
            dependencyCheckControl["Check for updates", "Look up the latest compatible versions.",
                refresh[False], operation, cancelled]}],
            DefaultButton["Close", cancelled = True; If[operation =!= None, cancelDependencyDownload[]]; dependencyRemember[owner["Name"]]; DialogReturn[], ImageSize -> {90, Automatic}, Method -> "Preemptive"]}},
            ItemSize -> {{Scaled[.72], Scaled[.28]}}, Alignment -> {{Left, Right}, Center}]
    }, ItemSize -> {{Scaled[1]}}, Spacings -> 1.3, Alignment -> Left, Dividers -> {False, {False, False, False, GrayLevel[.85], False}}], FrameMargins -> 16, FrameStyle -> None],
    Initialization :> (
        If[DownValues[dependencyRowActions] === {}, Block[{$ContextPath = $ContextPath, $AllowInternet = False}, Get[packageFile]]];
        SetAttributes[run, HoldRest];
        run[caption_, expr_] := If[operation === None, WithCleanup[
            operation = caption; cancelled = False; stage = caption; fraction = None; FinishDynamic[];
            Block[{dependencyCancelledQ = Function[Null, cancelled],
                    $dependencyProgress = Function[{step, value}, stage = step; fraction = value; FinishDynamic[]]}, expr],
            operation = None; stage = ""; fraction = None; FinishDynamic[]]];
        refresh[probe_] := run[If[probe, "Check readiness", "Check for updates"],
            With[{fresh = PacletDependencyInformation[owner, Keys[rows], "CheckReadiness" -> probe, "CheckUpdates" -> ! probe, "Output" -> "Association"]},
                If[AssociationQ[fresh],
                    rows = AssociationMap[Function[name, With[{old = rows[name], new = fresh[name]},
                        If[! AssociationQ[new], old, dependencyDialogRow[Join[KeyTake[old, {"LatestVersion", "Description"}], new,
                            If[! probe && new["Status"] === "Installed" && old["InstalledVersion"] === new["InstalledVersion"],
                                KeyTake[old, {"Status", "ReadinessFailure"}], <||>]], old["Label"]]]]], Keys[fresh]];
                    notices = (#["Message"] & /@ Select[fresh, FailureQ[#] && First[#] =!= "DependencyCancelled" &]) ]]];
        perform[name_, action_] := run[action, Module[{result, fresh},
            result = dependencyAction[action][owner, name];
            result = If[AssociationQ[result], result[name], result];
            fresh = PacletDependencyInformation[owner, name, "Output" -> "Association"];
            If[AssociationQ[fresh], rows[name] = dependencyDialogRow[Join[KeyTake[rows[name], {"LatestVersion", "Description"}], fresh[name]], rows[name]["Label"]]];
            If[rows[name]["Status"] === "Installed" && KeyExistsQ[rows[name], "Check"],
                Which[MatchQ[result, Failure["DependencySetupRequired", _]], rows[name] = Append[rows[name], "Status" -> "Needs setup"],
                    MatchQ[result, _PacletObject], rows[name] = Append[rows[name], "Status" -> "Ready"]]];
            notices[name] = Which[FailureQ[result], result["Message"], action === "Uninstall", "Removed from the user repository.",
                StringQ[rows[name]["InstalledVersion"]] && action === "Update", "Update check complete.",
                True, "Installation complete."];
            copied = None]];
        rowPanel[name_, row_] := With[{actions = row["Actions"], supported = row["Status"] =!= "Unsupported"},
            Grid[{{Pane[row["Details"], ImageSize -> Full, Alignment -> Left],
                Column[DeleteCases[{
                    row["Status"],
                    Row[Riffle[Map[Function[action, Button[If[action === "Update", "Upgrade", action], perform[name, action], ImageSize -> {100, Automatic}, Method -> "Queued",
                        Enabled -> Dynamic[operation === None && (supported || action === "Uninstall")]]],
                        actions], Spacer[6]]],
                    If[actions === {}, Nothing, With[{action = First[actions]},
                        Button[If[copied === name, "Copied", "Copy " <> ToLowerCase[action] <> " command"],
                            CopyToClipboard[dependencyCommand[commandOwner, action, name]]; copied = name,
                            Appearance -> "Frameless", BaseStyle -> "Hyperlink", Enabled -> Dynamic[operation === None && (supported || action === "Uninstall")]]]],
                    Lookup[notices, name, Which[
                        FailureQ[row["ReadinessFailure"]], row["ReadinessFailure"]["Message"],
                        FailureQ[row["LatestVersion"]], row["LatestVersion"]["Message"], True, Nothing]]
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
        WindowFrameElements -> {"CloseBox", "ResizeArea"},
        NotebookEventActions -> {"WindowClose" :> (dependencyRemember[name]; KeyDropFrom[$dependencyDialogs, {location}]), PassEventsDown -> True}]];
    If[MatchQ[dialog, _NotebookObject], AssociateTo[$dependencyDialogs, key -> dialog]];
    dialog
]
