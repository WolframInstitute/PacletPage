(* ::Package:: *)

$dependencyDialogs = <||>
dependencyDesktopQ[] := MatchQ[$FrontEnd, _FrontEndObject] && ! TrueQ[$CloudEvaluation] && $KernelID === 0
dependencyPreference[name_] := "WolframInstitute/PacletPage/DependencyDialog/" <> name
dependencyRemember[name_] := (PersistentSymbol[dependencyPreference[name], "Local"] = True)

PacletDependencyDialog::available = "`1` has optional dependencies. Run PacletDependencyDialog[`2`] to review them."
PacletDependencyDialog[spec : (_String | File[_String] | _PacletObject), "Notify"] := Module[{owner, rows},
    If[! dependencyDesktopQ[] || ! TrueQ[$Notebooks] || TrueQ[$BatchInput] || MemberQ[{"true", "1"}, Environment["CI"]], Return[Null]];
    owner = dependencyOwner[spec];
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    If[! TrueQ[PersistentSymbol[dependencyPreference[owner["Name"]], "Local"]],
        rows = PacletDependencyInformation[owner];
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
dependencyCommand[owner_, action_, name_] := SymbolName[dependencyAction[action]] <> "[" <> ToString[owner, InputForm] <> ", " <> ToString[name, InputForm] <> "]"

dependencyCaption[row_] := Row[Riffle[DeleteCases[{
    If[TrueQ[row["Required"]], "Required", "Optional"],
    Replace[Lookup[row, "Version", All], {All | "*" -> Nothing, v_String :> "Requires " <> v}],
    Replace[Lookup[row, "DownloadSize", None], {a_Association :> Lookup[a, $SystemID, Nothing], None -> Nothing}],
    Lookup[row, "License", Nothing]}, Nothing], Spacer[12]]]

dependencyDetails[row_] := Column[DeleteCases[{
    Style[row["Name"], 14, Bold], Lookup[row, "Description", Nothing],
    Style[dependencyCaption[row], Small],
    Row[Riffle[Join[If[KeyExistsQ[row, "URL"], {Hyperlink["Setup instructions", row["URL"]]}, {}],
        KeyValueMap[Hyperlink, Lookup[row, "SetupLinks", <||>]]], Spacer[10]]]
}, Row[{}]], Alignment -> Left, Spacings -> .35]

dependencyVersions[row_] := StringRiffle[{
    If[StringQ[row["InstalledVersion"]], "Installed " <> row["InstalledVersion"], Nothing],
    Switch[Lookup[row, "LatestVersion", None], _String, "Latest " <> row["LatestVersion"], _Failure, "Latest unavailable", _, Nothing]
}, "    "]

(* Redraw the rows only when their data change. Progress updates touch just the footer. *)
dependencyDialogContent[owner_, commandOwner_, initial_] := DynamicModule[
    {rows = initial, notices = <||>, copied = None, busy = False, cancelled = False, stage = "", fraction = None,
        run, refresh, perform, rowPanel},
    Column[{
        Style[owner["Name"], 18, Bold],
        Dynamic[If[rows === <||>, "This paclet declares no dependencies.",
            Pane[Column[KeyValueMap[rowPanel, KeySortBy[rows, StringQ[rows[#]["InstalledVersion"]] &]], Dividers -> Center, Spacings -> 1],
                ImageSize -> {Full, UpTo[400]}, Scrollbars -> {False, Automatic}]], TrackedSymbols :> {rows, notices, copied}],
        Dynamic[If[busy, Row[{stage, Spacer[12], If[NumberQ[fraction], ProgressIndicator[fraction, {0, 1}],
                ProgressIndicator[Appearance -> "Indeterminate"]], Spacer[12],
            Button["Cancel", cancelled = True; cancelDependencyDownload[], Method -> "Preemptive",
                Enabled -> Dynamic[! cancelled && MemberQ[{"Checking release", "Checking for updates", "Downloading", "Retrying"}, stage]]]}], Spacer[{0, 0}]],
            TrackedSymbols :> {busy, stage, fraction}],
        Row[{Button["Check readiness", refresh[True, False], Enabled -> Dynamic[! busy], Method -> "Queued"], Spacer[8],
            Button["Check for updates", refresh[False, True], Enabled -> Dynamic[! busy], Method -> "Queued"], Spacer[16],
            DefaultButton["Close", cancelled = True; If[busy, cancelDependencyDownload[]]; dependencyRemember[owner["Name"]]; DialogReturn[], Method -> "Preemptive"]}]
    }, Spacings -> 1, Alignment -> Left],
    Initialization :> (
        Needs["WolframInstitute`PacletPage`"];
        SetAttributes[run, HoldRest];
        run[caption_, expr_] := If[! busy, WithCleanup[
            busy = True; cancelled = False; stage = caption; fraction = None; FinishDynamic[];
            Block[{dependencyCancelledQ = Function[Null, cancelled],
                    $dependencyProgress = Function[{step, value}, stage = step; fraction = value; FinishDynamic[]]}, expr],
            busy = False; stage = ""; fraction = None]];
        refresh[probe_, updates_] := run[If[updates, "Checking for updates", "Checking readiness"],
            With[{fresh = PacletDependencyInformation[owner, "CheckReadiness" -> probe, "CheckUpdates" -> updates]},
                If[AssociationQ[fresh],
                    rows = AssociationMap[Function[name, With[{old = rows[name], new = fresh[name]},
                        If[! AssociationQ[new], old, Join[KeyTake[old, {"LatestVersion"}], new,
                            If[! probe && new["Status"] === "Installed" && old["InstalledVersion"] === new["InstalledVersion"],
                                KeyTake[old, {"Status"}], <||>]]]]], Keys[fresh]];
                    notices = (#["Message"] & /@ Select[fresh, FailureQ]) ]]];
        perform[name_, action_] := run[action, Module[{result, fresh},
            result = dependencyAction[action][owner, name];
            result = If[AssociationQ[result], result[name], result];
            fresh = PacletDependencyInformation[owner, name];
            If[AssociationQ[fresh], rows[name] = Join[KeyTake[rows[name], {"LatestVersion"}], fresh[name]]];
            If[rows[name]["Status"] === "Installed" && KeyExistsQ[rows[name], "Check"],
                Which[MatchQ[result, Failure["DependencySetupRequired", _]], rows[name] = Append[rows[name], "Status" -> "Needs setup"],
                    MatchQ[result, _PacletObject], rows[name] = Append[rows[name], "Status" -> "Ready"]]];
            notices[name] = Which[FailureQ[result], result["Message"], action === "Uninstall", "Removed from the user repository.",
                StringQ[rows[name]["InstalledVersion"]] && action === "Update", "Update check complete.",
                True, "Installation complete."];
            copied = None]];
        rowPanel[name_, row_] := With[{installed = StringQ[row["InstalledVersion"]], supported = row["Status"] =!= "Unsupported"},
            Grid[{{Pane[dependencyDetails[row], ImageSize -> {360, Automatic}],
                Column[DeleteCases[{
                    Style[row["Status"], Bold], Replace[dependencyVersions[row], "" -> Nothing],
                    Row[Riffle[Map[Function[action, Button[action, perform[name, action], Method -> "Queued",
                        Enabled -> Dynamic[! busy && (supported || action === "Uninstall")]]],
                        If[installed, {"Update", "Uninstall"}, {"Install"}]], Spacer[6]]],
                    With[{action = If[installed, "Uninstall", "Install"]},
                        Button[If[copied === name, "Copied", "Copy " <> ToLowerCase[action] <> " command"],
                            CopyToClipboard[dependencyCommand[commandOwner, action, name]]; copied = name,
                            Appearance -> "Frameless", BaseStyle -> "Hyperlink", Enabled -> Dynamic[! busy]]],
                    Lookup[notices, name, If[FailureQ[row["ReadinessFailure"]], row["ReadinessFailure"]["Message"], Nothing]]
                }, Nothing], Alignment -> Right, Spacings -> .35]}}, Alignment -> {{Left, Right}, Top}]]
    ),
    Deinitialization :> (cancelled = True; If[busy, cancelDependencyDownload[]]),
    SynchronousInitialization -> True
]

PacletDependencyDialog[spec : (_String | File[_String] | _PacletObject)] := Module[{owner, key, dialog, rows},
    If[! dependencyDesktopQ[], Return[dependencyFailure["NotebookRequired", "Open a desktop notebook to use the dependency dialog. Scripts can use PacletDependencyInformation and the install, update and uninstall functions."]]];
    owner = dependencyOwner[spec];
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    key = dependencyCommandOwner[owner, owner];
    dialog = Lookup[$dependencyDialogs, key, None];
    If[MemberQ[Notebooks[], dialog], SetSelectedNotebook[dialog]; Return[dialog]];
    rows = PacletDependencyInformation[owner];
    If[FailureQ[rows], Return[rows]];
    dialog = With[{name = owner["Name"], location = key}, CreateDialog[dependencyDialogContent[owner, dependencyCommandOwner[spec, owner], rows],
        WindowTitle -> name <> " dependencies", WindowSize -> {680, All}, WindowFrame -> "Normal",
        WindowFrameElements -> {"CloseBox", "ResizeArea"},
        NotebookEventActions -> {"WindowClose" :> (dependencyRemember[name]; KeyDropFrom[$dependencyDialogs, location]), PassEventsDown -> True}]];
    If[MatchQ[dialog, _NotebookObject], AssociateTo[$dependencyDialogs, key -> dialog]];
    dialog
]
