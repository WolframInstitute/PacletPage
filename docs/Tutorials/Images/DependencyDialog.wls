#!/usr/bin/env wolframscript
(* The screenshots of the Using the Dependency Dialog tutorial: the dialog of this checkout, opened in a
   front end for the tutorial's demo consumer, one image per state, written next to this script.

       wolframscript -f docs/Tutorials/Images/DependencyDialog.wls

   The rows are those PacletDependencyInformation reads for the consumer, with the installed and latest
   versions of each state set here, so that the images do not depend on what this machine has installed.
   A state the dialog reaches by an action (a download, a check) is set in the dialog's tagging rules,
   as the action sets it. The front end renders at the screen's resolution; each image is scaled to
   one pixel per point, the size a documentation notebook shows it at. *)

$images = DirectoryName[$InputFileName];
$repo = DirectoryName[$images, 3];
Block[{$ContextPath = {"System`"}}, Get[FileNameJoin[{$repo, "Kernel", "PacletPage.wl"}]]];
PrependTo[$ContextPath, "WolframInstitute`PacletPage`"];
Begin["WolframInstitute`PacletPage`Private`"];

$typeTheory = "WolframInstitute/TypeTheory";
$parser = "Wolfram/Parser";
$owner = PacletObject[<|"Name" -> "Demo/Consumer", "Version" -> "1.0.0", "PrimaryContext" -> "Demo`Consumer`",
    "Dependencies" -> {
        "WolframInstitute/PacletPage",
        <|"Name" -> $typeTheory,
          "Optional" -> True,
          "Description" -> "Type checking for the Calculus of Inductive Constructions.",
          "Resource" -> "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/TypeTheory",
          "License" -> "MIT",
          "URL" -> "https://github.com/WolframInstitute/TypeTheory#loading",
          "Check" -> "Demo`Consumer`TypeTheoryReadyQ"|>,
        <|"Name" -> $parser,
          "Optional" -> True,
          "Description" -> "Parsers for the Wolfram Language and other languages, built from their grammars.",
          "Git" -> "https://github.com/sw1sh/WolframParser",
          "Commit" -> "5e05149ead1c292ebd39d02eee85f1c0b8059f55",
          "Path" -> "Parser",
          "DownloadSize" -> "4 MB"|>}|>];

$rows = Block[{$AllowInternet = False}, PacletDependencyInformation[$owner, "Output" -> "Association"]];
If[! AssociationQ[$rows], Print["no rows: ", $rows]; Exit[1]];

installed[row_, version_] := Join[row, <|"InstalledVersion" -> version, "Status" -> "Installed"|>];
missing[row_] := Join[KeyDrop[row, "LatestVersion"], <|"InstalledVersion" -> Missing["NotInstalled"], "Status" -> "Not installed"|>];
latest[row_, version_] := Append[row, "LatestVersion" -> version];

$pacletPage = PacletObject[File[$repo]]["Version"];
$opened = <|
    "WolframInstitute/PacletPage" -> installed[$rows["WolframInstitute/PacletPage"], $pacletPage],
    $typeTheory -> missing[$rows[$typeTheory]],
    $parser -> installed[$rows[$parser], "1.9.12"]|>;
$installed = MapAt[installed[#, "0.18.0"] &, $opened, Key[$typeTheory]];
$checked = <|
    "WolframInstitute/PacletPage" -> latest[$installed["WolframInstitute/PacletPage"], $pacletPage],
    $typeTheory -> latest[$installed[$typeTheory], "0.18.0"],
    $parser -> latest[$installed[$parser], "1.9.13"]|>;
(* the consumer's check, as a consumer whose proof engine cannot run yet would give it *)
Demo`Consumer`TypeTheoryReadyQ[] := False;
$notReady = Join[$checked[$typeTheory], <|"Status" -> "Needs setup", "ReadinessFailure" -> dependencyReadiness[$owner, $installed[$typeTheory]]|>];

(* open the dialog from a notebook of the given appearance, with the given rows, set the tagging
   rules an action would set, and render it *)
shot[file_String, appearance_String, rows_Association, opts_List, tags_List] := Module[{source, nb, image, background},
    source = CreateDocument[{}, Visible -> False, LightDark -> appearance];
    WithCleanup[
        Block[{dependencyDesktopQ = (True &), EvaluationNotebook = (source &), SetSelectedNotebook = (Null &),
                PacletDependencyInformation = (rows &), $dependencyDialogs = <||>},
            nb = PacletDependencyDialog[$owner, Sequence @@ opts];
            Scan[(CurrentValue[nb, {TaggingRules, Sequence @@ First[#]}] = Last[#]) &, tags];
            image = Rasterize[nb]],
        If[MatchQ[nb, _NotebookObject], NotebookClose[nb]];
        NotebookClose[source]];
    If[! ImageQ[image], Print[file, ": no image: ", Short[image]]; Return[$Failed]];
    background = PixelValue[image, {1, 1}];
    image = ImageResize[ImagePad[ImagePad[image, {{50, 56}, {8, 8}}, background], 2,
        If[appearance === "Dark", GrayLevel[.32], GrayLevel[.78]]], Scaled[1/2], Resampling -> "Lanczos"];
    Export[FileNameJoin[{$images, file}], image];
    Print[file, ": ", ImageDimensions[image]]
];

UsingFrontEnd[
    shot["DependencyDialog-Opened.png", "Light", $opened, {}, {}];
    shot["DependencyDialog-Downloading.png", "Light", $opened, {}, {
        {"DependencyTransfer", $typeTheory} -> True,
        {"DependencyProgress"} -> dependencyProgressState["Downloading", .42],
        {"DependencyActivity"} -> <|"Target" -> $typeTheory, "Idle" -> False|>}];
    shot["DependencyDialog-Updates.png", "Light", $checked, {}, {
        {"DependencyCheckFeedback"} -> dependencyUpdateSummary[$checked]}];
    shot["DependencyDialog-NeedsSetup.png", "Light", ReplacePart[$checked, Key[$typeTheory] -> $notReady], {}, {
        {"DependencyCheckFeedback"} -> dependencyReadinessSummary[<|$typeTheory -> $notReady|>]}];
    shot["DependencyDialog-Titled.png", "Light", $opened,
        {"Title" -> "Proof libraries", "Description" -> "Install TypeTheory to check proofs in this notebook."}, {}];
    shot["DependencyDialog-Dark.png", "Dark", $opened, {}, {}]
];
