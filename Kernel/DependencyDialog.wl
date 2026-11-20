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
dependencyDialogText[defaultTitle_, title_, description_] := Which[
    title =!= Automatic && (! StringQ[title] || StringTrim[title] === ""),
        dependencyFailure["DependencyDialogTitle", "Use a nonempty string or Automatic for the dialog title."],
    description =!= Automatic && ! StringQ[description],
        dependencyFailure["DependencyDialogDescription", "Use a string or Automatic for the dialog description."],
    True, <|"Title" -> Replace[title, Automatic -> defaultTitle],
        "Description" -> Replace[description, Automatic -> "Install, upgrade, or remove libraries as needed."]|>
]
dependencyShortName[name_] := Last[StringSplit[name, "/"]]
dependencyMetaStyle[expr_] := Style[expr, Small, LightDarkSwitched[GrayLevel[.35], GrayLevel[.72]]]

(* CellSize includes magnification. Let the front end measure wrapped headings and controls.
   A window with too little room for one line scrolls the entire notebook. *)
dependencyListSize[nb_NotebookObject] := Module[{window = CurrentValue[nb, WindowSize], zoom, controls, heights, available, elements = {}, height = UpTo[400]},
    If[MatchQ[window, {_?NumberQ, _?NumberQ}],
        zoom = AbsoluteCurrentValue[nb, Magnification];
        controls = Cells[nb, CellTags -> "DependencyDialogControls"];
        heights = Cases[AbsoluteCurrentValue[#, CellSize] & /@ controls, {_, h_?NumberQ} :> Ceiling[h]];
        If[heights =!= {},
            available = Floor[(Last[window] - Total[heights])/zoom];
            If[available < CurrentValue[nb, "FontLineHeight"],
                elements = {"VerticalScrollBar"}; height = Automatic,
                height = UpTo[available]]]];
    If[CurrentValue[nb, WindowElements] =!= elements, SetOptions[nb, WindowElements -> elements]];
    {Full, height}
]

dependencyFitList[nb_NotebookObject] := With[{size = dependencyListSize[nb]},
    If[CurrentValue[nb, {TaggingRules, "DependencyListSize"}] =!= size,
        CurrentValue[nb, {TaggingRules, "DependencyListSize"}] = size]]

(* Put spacing on the inside of each line so wrapping preserves its aligned edge. *)
dependencyFlow[items_List, gap_, alignment_ : Left] := With[{margins = If[alignment === Right, {gap, 0}, {0, gap}]},
    Style[Row[Pane[#, ImageMargins -> {margins, {0, 0}}] & /@ items, Alignment -> {alignment, Center}],
        TextAlignment -> alignment, LineIndent -> 0, LineIndentMaxFraction -> 0]]

dependencyField[name_, key_] := Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyRows", name, key}]]

dependencyDetails[row_, label_] := Module[{name = row["Name"], size, license, requirement, links, installed, versions, metadata, description},
    size = Replace[Lookup[row, "DownloadSize", None], a_Association :> Lookup[a, $SystemID, None]];
    license = Lookup[row, "License", None];
    requirement = Lookup[row, "Version", All];
    installed = Row[{"Installed: ", dependencyField[name, "Installed"]}];
    versions = PaneSelector[{True -> dependencyFlow[{installed, Row[{"Latest: ", dependencyField[name, "Latest"]}]}, 16]},
        dependencyField[name, "ShowLatest"], installed, Alignment -> Left, ImageSize -> Automatic];
    metadata = {If[StringQ[size], "Download: " <> size, Nothing], If[StringQ[license], "License: " <> license, Nothing],
        If[TrueQ[row["Required"]], "Required", Nothing], If[StringQ[requirement] && requirement =!= "*", "Requires " <> requirement, Nothing]};
    links = Join[If[StringQ[row["URL"]], {Hyperlink["Setup instructions", row["URL"]]}, {}],
        KeyValueMap[Hyperlink, Lookup[row, "SetupLinks", <||>]]];
    description = Lookup[row, "Description", ""];
    Column[{Tooltip[Style[label, 14, Bold], name], dependencyMetaStyle[versions],
        If[description === "", dependencyField[name, "Description"], description],
        If[metadata === {}, Nothing, dependencyMetaStyle[dependencyFlow[metadata, 12]]],
        If[links === {}, Nothing, dependencyFlow[links, 12]]}, Alignment -> Left, Spacings -> .5]
]

dependencyRowActions[row_] := If[! StringQ[row["InstalledVersion"]], {"Install"}, {
    If[dependencyRemovableQ[row], "Uninstall", Nothing],
    If[row["Status"] =!= "Unsupported" && StringQ[row["LatestVersion"]] &&
        PacletNewerQ[row["LatestVersion"], row["InstalledVersion"]], "Update", Nothing]}]

dependencyRowState[row_, notice_ : Automatic] := With[{actions = dependencyRowActions[row]}, With[{first = First[actions, None]},
    Join[<|"Installed" -> Replace[row["InstalledVersion"], _Missing -> "Not installed"],
      "Latest" -> Replace[Lookup[row, "LatestVersion", "Not checked"],
          {Failure["DependencyInternetDisabled", _] -> "Offline", _Failure -> "Unavailable"}],
      "ShowLatest" -> (StringQ[row["InstalledVersion"]] || StringQ[row["LatestVersion"]]),
      "Description" -> Lookup[row, "Description", ""], "ActionCount" -> Length[actions], "Action" -> first,
      "ActionLabel" -> Replace[first, {"Update" -> "Upgrade", None -> ""}],
      "CopyLabel" -> If[StringQ[first], "Copy " <> ToLowerCase[first] <> " command", ""]|>,
      Replace[notice, Automatic :> dependencyRowNotice[row]]]]]

(* Cancellation belongs to the action. Always read the local installation state afterwards. *)
dependencyDialogAction[owner_, name_, action_, cancelledQ_, progress_] := Module[{result, fresh},
    result = Block[{dependencyCancelledQ = cancelledQ, $dependencyProgress = progress},
        dependencyAction[action][owner, name]];
    fresh = PacletDependencyInformation[owner, name, "Output" -> "Association"];
    {If[AssociationQ[result], result[name], result], If[AssociationQ[fresh], fresh[name], fresh]}
]

dependencyDialogMessage[text_String] := StringReplace[text, (c : Except["."]) ~~ "." ~~ EndOfString :> c]

dependencyNotice[label_, detail_ : ""] := <|"Notice" -> label, "NoticeDetail" -> detail|>

dependencyRowNotice[row_] := Which[
    FailureQ[row["ReadinessFailure"]], dependencyNotice["Needs setup", row["ReadinessFailure"]["Message"]],
    row["Status"] === "Needs setup", dependencyNotice["Needs setup", "Check readiness for details."],
    row["Status"] === "Restart required", dependencyNotice["Restart required", "Restart the kernel to use the changed library."],
    StringQ[row["InstalledVersion"]] && MatchQ[row["LatestVersion"], Failure[Except["DependencyInternetDisabled"], _]],
        dependencyNotice["Check failed", row["LatestVersion"]["Message"]],
    row["Status"] === "Unsupported", dependencyNotice["Unsupported", "No release for " <> $SystemID],
    True, dependencyNotice[""]
]

dependencyActionNotice[action_, result_, fresh_] := Which[
    MatchQ[result, Failure["DependencyCancelled", _]], dependencyNotice["Cancelled"],
    MatchQ[result, Failure["DependencySetupRequired", _]], dependencyNotice["Needs setup", result["Message"]],
    MatchQ[result, _Failure], dependencyNotice["Failed", result["Message"]],
    MatchQ[fresh, _Failure], dependencyNotice["Unconfirmed", fresh["Message"]],
    ! AssociationQ[fresh] || ! If[action === "Uninstall", result === Null,
        MatchQ[result, _PacletObject] && StringQ[fresh["InstalledVersion"]]],
        dependencyNotice["Unconfirmed", "Reopen the dialog to check the installed version."],
    MemberQ[{"Restart required", "Needs setup"}, fresh["Status"]], dependencyRowNotice[fresh],
    action === "Uninstall", dependencyNotice["Removed"],
    action === "Update", dependencyNotice["Updated"],
    True, dependencyNotice["Installed"]
]

(* Prepare the display in the owning kernel, keeping the full identifier for each action. *)
dependencyDialogRow[row_, label_] := Append[row, "Label" -> label]

dependencyDialogRows[rows_] := With[{counts = Counts[dependencyShortName /@ Keys[rows]]},
    Association@KeyValueMap[Function[{name, row}, With[{short = dependencyShortName[name]},
        name -> dependencyDialogRow[row, If[counts[short] === 1, short, name]]]], rows]
]

dependencyReadinessSummary[rows_] := Module[{states, passed, failed, skipped},
    states = #["Status"] & /@ Values[Select[rows, AssociationQ]];
    passed = Count[states, "Ready"];
    failed = Count[states, "Needs setup"];
    skipped = Length[states] - passed - failed;
    If[states === {}, "No libraries to check", StringRiffle[{
        If[passed > 0, ToString[passed] <> " passed", Nothing],
        If[failed > 0, ToString[failed] <> " failed", Nothing],
        If[skipped > 0, ToString[skipped] <> " skipped", Nothing]}, ", "]]
]

dependencyCheckableQ[entry_, status_, probe_] := If[probe,
    StringQ[Lookup[entry, "Check", None]] && MemberQ[{"Installed", "Ready", "Needs setup"}, status],
    ! MemberQ[{"Not installed", "Unsupported"}, status]]

dependencyCheckAvailability[rows_] := With[{installed = Select[rows, StringQ[#["InstalledVersion"]] &]},
    <|"Readiness" -> AnyTrue[installed, dependencyCheckableQ[#, #["Status"], True] &],
      "Updates" -> AnyTrue[installed, dependencyCheckableQ[#, #["Status"], False] &]|>]

(* Local inventory does not reproduce readiness results. Keep the existing row and its
   notice unless the installed version or its underlying local status changed. *)
dependencyLocalChanges[owner_, rows_] := Module[{fresh, old},
    fresh = PacletDependencyInformation[owner, Keys[rows], "Output" -> "Association"];
    If[! AssociationQ[fresh], Return[<||>]];
    Association@KeyValueMap[Function[{name, row},
        old = rows[name];
        If[row["InstalledVersion"] === old["InstalledVersion"] &&
            row["Status"] === Replace[old["Status"], ("Ready" | "Needs setup") -> "Installed"], Nothing,
            name -> dependencyDialogRow[Join[KeyTake[old, {"Description"}],
                If[StringQ[old["LatestVersion"]], KeyTake[old, {"LatestVersion"}], <||>], row], old["Label"]]]], fresh]
]

dependencyOfflineQ[rows_] := AnyTrue[rows, StringQ[#["InstalledVersion"]] &&
    MatchQ[#["LatestVersion"], Failure["DependencyInternetDisabled", _]] &]

dependencyUpdateSummary[rows_] := Module[{upgrades, failed},
    If[rows === <||>, Return["No libraries to check"]];
    upgrades = Count[Values[rows], row_ /; MemberQ[dependencyRowActions[row], "Update"]];
    failed = Count[Lookup[Values[rows], "LatestVersion"], _Failure];
    If[upgrades + failed === 0, "No upgrades", StringRiffle[{
        If[upgrades > 0, ToString[upgrades] <> If[upgrades === 1, " upgrade", " upgrades"], Nothing],
        If[failed > 0, ToString[failed] <> " unavailable", Nothing]}, ", "]]
]

(* Re-read local installations on each explicit check. Publish each completed row,
   including a library removed elsewhere while this window was open. *)
dependencyCheckLibraries[owner_, rows_, probe_, started_, publish_] := withDependencySelection[owner, Keys[rows],
    Function[{paclet, entries}, Module[{installed, names, old, eligible, fresh, row, checked = <||>},
        installed = dependencyInstallations[Keys[entries]];
        names = Select[Keys[entries], installed[#] =!= {} || StringQ[rows[#]["InstalledVersion"]] &];
        Do[
            If[dependencyCancelledQ[], Break[]];
            old = rows[name];
            eligible = installed[name] =!= {} && dependencyCheckableQ[entries[name], dependencyLocalStatus[entries[name], installed[name]], probe];
            If[eligible, started[old["Label"]]];
            fresh = dependencyInfo[paclet, entries[name], installed[name], eligible && probe, eligible && ! probe];
            row = dependencyDialogRow[Join[KeyTake[old, {"LatestVersion", "Description"}], fresh,
                If[! (eligible && probe) && fresh["Status"] === "Installed" && old["InstalledVersion"] === fresh["InstalledVersion"],
                    KeyTake[old, {"Status", "ReadinessFailure"}], <||>]], old["Label"]];
            If[eligible || row =!= old, publish[name, row]];
            If[eligible, checked[name] = row],
            {name, names}];
        checked
    ]]]

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
            Alignment -> Right, ImageSize -> Automatic],
        PaneSelector[{True -> Button["Cancel", cancel, Method -> "Preemptive", ImageSize -> {100, Automatic}]},
            Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyProgress", "Cancellable"}]], Spacer[{0, 0}], ImageSize -> Automatic]
    }, Alignment -> Right, Spacings -> .45]
]

(* The front end owns the selector so it can paint and animate while the kernel is checking a library. *)
SetAttributes[dependencyCheckControl, HoldRest]
dependencyCheckControl[label_String, hint_String, start_, stop_, enabled_ : True] := With[{
        spinner = ProgressIndicator[Appearance -> "Necklace", ImageSize -> {14, 14}], size = {140, 26}},
    PaneSelector[{
        None -> Tooltip[Button[label, start, Method -> "Queued", ImageSize -> size, Enabled -> enabled], hint],
        label -> Tooltip[Button[Row[{spinner, Spacer[6], "Cancel check"}],
            stop; dependencySetActivity[ButtonNotebook[], "Stopping " <> label],
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

dependencyRowPanel[name_, row_, perform_, copy_, cancelTransfer_] := Module[{enabled, primary, upgrade, commands, notice, detail},
    enabled = If[dependencySupportedQ[row], Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyActivity", "Idle"}]],
        Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyActivity", "Idle"}] &&
            CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyRows", name, "Action"}] === "Uninstall"]];
    primary = Button[dependencyField[name, "ActionLabel"],
        perform[name, CurrentValue[ButtonNotebook[], {TaggingRules, "DependencyRows", name, "Action"}], ButtonNotebook[]],
        ImageSize -> {100, Automatic}, Method -> "Queued", Enabled -> enabled];
    upgrade = Button["Upgrade", perform[name, "Update", ButtonNotebook[]],
        ImageSize -> {100, Automatic}, Method -> "Queued", Enabled -> enabled];
    commands = Column[{
        PaneSelector[{2 -> dependencyFlow[{primary, upgrade}, 6, Right]}, dependencyField[name, "ActionCount"], primary,
            Alignment -> Right, ImageSize -> Automatic],
        Button[PaneSelector[{name -> "Copied"}, Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyCopied"}]],
                dependencyField[name, "CopyLabel"], Alignment -> Right, ImageSize -> Automatic],
            copy[name, CurrentValue[ButtonNotebook[], {TaggingRules, "DependencyRows", name, "Action"}], ButtonNotebook[]],
            Appearance -> "Frameless", BaseStyle -> {"Hyperlink", TextAlignment -> Right, LineIndent -> 0}, Alignment -> Right, Enabled -> enabled]
    }, Alignment -> Right, Spacings -> .5];
    notice = dependencyField[name, "Notice"];
    detail = dependencyField[name, "NoticeDetail"];
    Grid[{{Pane[dependencyDetails[row, row["Label"]], ImageSize -> Full, Alignment -> Left],
        PaneSelector[{True -> dependencyProgressView[cancelTransfer[ButtonNotebook[]]]},
            Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyTransfer", name}]],
            Column[{PaneSelector[{0 -> Spacer[0]}, dependencyField[name, "ActionCount"], commands, Alignment -> Right, ImageSize -> Automatic],
                PaneSelector[{"" -> Spacer[{0, 0}]}, notice,
                    Style[PaneSelector[{"" -> notice}, detail, Tooltip[notice, detail], ImageSize -> Automatic], TextAlignment -> Right, LineIndent -> 0],
                    Alignment -> Right, ImageSize -> Automatic, ContentPadding -> False]},
                Alignment -> Right, Spacings -> .3], Alignment -> Right, ImageSize -> Automatic]}},
        ItemSize -> {{Scaled[.65], Scaled[.35]}}, Alignment -> {{Left, Right}, Top}]
]

(* The three cells let the front end measure the controls independently of the library list.
   The callbacks belong to this dialog and are initialized in its evaluator. *)
dependencyDialogContent[owner_, commandOwner_, initial_] := Module[{startCheck, stopCheck, heading, description, withIntro},
    heading = Style[Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyText", "Title"}]], 18, Bold];
    description = Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyText", "Description"}]];
    withIntro[text_] := Column[{heading, text}, Alignment -> Left, Spacings -> .6];
    {
        ExpressionCell[Framed[DynamicWrapper[
            PaneSelector[{True -> withIntro[dependencyDialogMessage[$dependencyInternetMessage]]},
                Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyInternetDisabled"}]],
                PaneSelector[{"" -> heading}, description, withIntro[description], Alignment -> Left, ImageSize -> Automatic],
                Alignment -> Left, ImageSize -> Automatic],
            dependencyFitList[EvaluationNotebook[]], SynchronousUpdating -> False],
            FrameStyle -> None, FrameMargins -> {{0, 0}, {10, 20}}], "DialogStyle",
            CellTags -> "DependencyDialogControls", CellMargins -> {{25, 28}, {0, 0}}],
        ExpressionCell[dependencyLibraryList[owner, commandOwner, initial, startCheck, stopCheck], "DialogStyle",
            CellMargins -> {{25, 28}, {0, 0}}],
        ExpressionCell[Framed[Grid[{{Pane[dependencyFlow[{
            dependencyCheckControl["Check readiness", "Run the readiness tests supplied for installed libraries that can run in this kernel.",
                startCheck[True, ButtonNotebook[]], stopCheck[], Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyChecks", "Readiness"}]]],
            dependencyCheckControl["Check for updates", "Look up compatible releases for installed libraries.",
                startCheck[False, ButtonNotebook[]], stopCheck[], Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyChecks", "Updates"}]]],
            dependencyMetaStyle[Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyCheckFeedback"}]]]},
            6], ImageSize -> Full, Alignment -> Left],
            DefaultButton["Close", stopCheck[True]; NotebookClose[ButtonNotebook[]], ImageSize -> {90, Automatic}, Method -> "Preemptive"]}},
            ItemSize -> {{Automatic, Full}}, Alignment -> {{Left, Right}, Bottom}, Spacings -> {.6, 0}],
            FrameMargins -> {{0, 0}, {17, 7}}, FrameStyle -> None], "DialogStyle",
            CellTags -> "DependencyDialogControls", CellMargins -> {{25, 28}, {0, 0}},
            CellFrame -> {{0, 0}, {0, 1}}, CellFrameColor -> GrayLevel[.85], CellFrameMargins -> 0]
    }
]

dependencyLibraryList[owner_, commandOwner_, initial_, startCheck_Symbol, stopCheck_Symbol] := With[{
        initialRows = dependencyDialogRows[initial], packageFile = FileNameJoin[{$packageDirectory, "PacletPage.wl"}]}, DynamicModule[
    {rows = initialRows, operation = None, cancelled = False, wasSelected = True, focus, refresh, perform, copy, cancelTransfer},
    Pane[If[initialRows === <||>, "This paclet declares no dependencies",
            Column[KeyValueMap[dependencyRowPanel[#1, #2, perform, copy, cancelTransfer] &, initialRows],
                ItemSize -> {{Scaled[1]}}, Spacings -> 1.2]],
        ImageSize -> Dynamic[CurrentValue[EvaluationNotebook[], {TaggingRules, "DependencyListSize"}]],
        Scrollbars -> {False, Automatic}, AppearanceElements -> None, Alignment -> {Left, Top}],
    Initialization :> (
        If[DownValues[dependencyRowActions] === {}, Block[{$ContextPath = $ContextPath, $AllowInternet = False}, Get[packageFile]]];
        focus[nb_, selected_, active_] := Which[
            selected === False && wasSelected, wasSelected = False,
            selected === True && ! wasSelected && active === None,
            wasSelected = True;
            With[{changed = dependencyLocalChanges[owner, rows]}, If[changed =!= <||> && MemberQ[Notebooks[], nb],
                rows = Join[rows, changed];
                KeyValueMap[(CurrentValue[nb, {TaggingRules, "DependencyRows", #1}] = dependencyRowState[#2]) &, changed];
                If[KeyExistsQ[changed, CurrentValue[nb, {TaggingRules, "DependencyCopied"}]],
                    CurrentValue[nb, {TaggingRules, "DependencyCopied"}] = None];
                CurrentValue[nb, {TaggingRules, "DependencyChecks"}] = dependencyCheckAvailability[rows];
                CurrentValue[nb, {TaggingRules, "DependencyInternetDisabled"}] = dependencyOfflineQ[rows];
                CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}] = ""]]];
        With[{nb = EvaluationNotebook[]}, SetOptions[nb, NotebookDynamicExpression :> focus[nb, CurrentValue[nb, "NotebookSelected"], operation]]];
        copy[name_, action_, nb_] := (CopyToClipboard[dependencyCommand[commandOwner, action, name]];
            CurrentValue[nb, {TaggingRules, "DependencyCopied"}] = name);
        stopCheck[closing_ : False] := (cancelled = True; If[closing && operation =!= None, cancelDependencyDownload[]]);
        startCheck[probe_, nb_NotebookObject] := If[operation === None, With[{label = If[probe, "Check readiness", "Check for updates"]},
            dependencyRunCheck[nb, label,
                operation = label; cancelled = False;
                Block[{dependencyCancelledQ = Function[Null, cancelled || ! MemberQ[Notebooks[], nb]]}, refresh[probe, nb]],
                operation = None]
        ]];
        refresh[probe_, nb_] := Module[{checked},
            checked = dependencyCheckLibraries[owner, rows, probe,
                Function[label, CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}] = label <> "…"],
                Function[{name, row}, If[MemberQ[Notebooks[], nb], rows[name] = row;
                    CurrentValue[nb, {TaggingRules, "DependencyRows", name}] = dependencyRowState[row]]]];
            If[MemberQ[Notebooks[], nb],
                CurrentValue[nb, {TaggingRules, "DependencyChecks"}] = dependencyCheckAvailability[rows];
                If[! probe, CurrentValue[nb, {TaggingRules, "DependencyInternetDisabled"}] = dependencyOfflineQ[rows]]];
            Which[FailureQ[checked], dependencyDialogMessage[checked["Message"]], cancelled, "Check stopped",
                probe, dependencyReadinessSummary[checked], True, dependencyUpdateSummary[checked]]
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
                    operation = action; cancelled = False; CurrentValue[nb, {TaggingRules, "DependencyCopied"}] = None;
                    CurrentValue[nb, {TaggingRules, "DependencyCheckFeedback"}] = "";
                    progress[If[action === "Uninstall", "Uninstalling", "Preparing"], None];
                    dependencySetActivity[nb, name];
                    CurrentValue[nb, {TaggingRules, "DependencyTransfer", name}] = True;
                    {result, fresh} = dependencyDialogAction[owner, name, action, Function[Null, cancelled], progress];
                    If[AssociationQ[fresh], rows[name] = dependencyDialogRow[Join[KeyTake[rows[name], {"LatestVersion", "Description"}], fresh], rows[name]["Label"]]];
                    If[rows[name]["Status"] === "Installed" && StringQ[rows[name]["Check"]],
                        Which[MatchQ[result, Failure["DependencySetupRequired", _]],
                            rows[name] = Join[rows[name], <|"Status" -> "Needs setup", "ReadinessFailure" -> result["Cause"]|>],
                            MatchQ[result, _PacletObject], rows[name] = Append[rows[name], "Status" -> "Ready"]]];
                    If[MemberQ[Notebooks[], nb],
                        CurrentValue[nb, {TaggingRules, "DependencyRows", name}] = dependencyRowState[rows[name], dependencyActionNotice[action, result, fresh]];
                        CurrentValue[nb, {TaggingRules, "DependencyChecks"}] = dependencyCheckAvailability[rows];
                        CurrentValue[nb, {TaggingRules, "DependencyInternetDisabled"}] = dependencyOfflineQ[rows]],
                    operation = None; If[MemberQ[Notebooks[], nb],
                        CurrentValue[nb, {TaggingRules, "DependencyTransfer", name}] = False;
                        dependencySetActivity[nb, None]]]
            ]]
    ),
    Deinitialization :> (cancelled = True; If[operation =!= None, cancelDependencyDownload[]]; Clear[startCheck, stopCheck]),
    SynchronousInitialization -> True
]]

dependencyDialogTags[rows_, text_ : Automatic] := With[{defaultTitle = dependencyDialogHeading[rows]},
    <|"DependencyDefaultTitle" -> defaultTitle, "DependencyText" -> Replace[text, Automatic :> dependencyDialogText[defaultTitle, Automatic, Automatic]],
    "DependencyActivity" -> <|"Target" -> None, "Idle" -> True|>,
    "DependencyCheckFeedback" -> "", "DependencyInternetDisabled" -> False,
    "DependencyListSize" -> {Full, UpTo[400]}, "DependencyCopied" -> None, "DependencyTransfer" -> <||>,
    "DependencyRows" -> Map[dependencyRowState, rows], "DependencyChecks" -> dependencyCheckAvailability[rows]|>]

Options[PacletDependencyDialog] = {"Title" -> Automatic, "Description" -> Automatic}
PacletDependencyDialog[spec : (_String | File[_String] | _PacletObject), scope : (All | "Optional") : All, OptionsPattern[]] :=
 Module[{owner, key, dialog, rows, existing, defaultTitle, text, windowTitle},
    If[! dependencyDesktopQ[], Return[dependencyFailure["NotebookRequired", "Open a desktop notebook to use the dependency dialog. Scripts can use PacletDependencyInformation and the install, update and uninstall functions."]]];
    owner = dependencyOwner[spec];
    If[! MatchQ[owner, _PacletObject], Return[owner]];
    key = {dependencyCommandOwner[owner, owner], scope};
    dialog = $dependencyDialogs[key];
    existing = MemberQ[Notebooks[], dialog];
    If[existing,
        defaultTitle = CurrentValue[dialog, {TaggingRules, "DependencyDefaultTitle"}],
        rows = PacletDependencyInformation[owner, "Output" -> "Association"];
        If[FailureQ[rows], Return[rows]];
        If[scope === "Optional", rows = Select[rows, ! TrueQ[#["Required"]] &]];
        defaultTitle = dependencyDialogHeading[rows]];
    text = dependencyDialogText[defaultTitle, OptionValue["Title"], OptionValue["Description"]];
    If[FailureQ[text], Return[text]];
    windowTitle = dependencyShortName[owner["Name"]] <> ": " <> text["Title"];
    If[existing,
        If[CurrentValue[dialog, {TaggingRules, "DependencyText"}] =!= text,
            CurrentValue[dialog, {TaggingRules, "DependencyText"}] = text];
        If[CurrentValue[dialog, WindowTitle] =!= windowTitle, SetOptions[dialog, WindowTitle -> windowTitle]];
        SetSelectedNotebook[dialog]; Return[dialog]];
    dialog = With[{name = owner["Name"], location = key}, CreateDialog[dependencyDialogContent[owner, dependencyCommandOwner[spec, owner], rows],
        WindowTitle -> windowTitle, WindowSize -> {740, All}, WindowFrame -> "Normal",
        WindowFrameElements -> {"CloseBox", "ResizeArea"}, WindowElements -> {},
        ScrollingOptions -> {"VerticalScrollRange" -> Fit}, Evaluator -> CurrentValue[EvaluationNotebook[], Evaluator],
        Visible -> False, TaggingRules -> dependencyDialogTags[rows, text],
        NotebookEventActions -> {"WindowClose" :> (dependencyRemember[name]; KeyDropFrom[$dependencyDialogs, {location}]), PassEventsDown -> True}]];
    If[MatchQ[dialog, _NotebookObject],
        dependencyFitList[dialog];
        AssociateTo[$dependencyDialogs, key -> dialog];
        SetOptions[dialog, Visible -> True]; SetSelectedNotebook[dialog]];
    dialog
]
