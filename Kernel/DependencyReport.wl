(* ::Package:: *)

PacletDependencyReport[_, rows_Association][name_String] := rows[name]
PacletDependencyReport /: Normal[PacletDependencyReport[_, rows_Association]] := rows
PacletDependencyReport /: Length[PacletDependencyReport[_, rows_Association]] := Length[rows]
PacletDependencyReport /: Keys[PacletDependencyReport[_, rows_Association]] := Keys[rows]
PacletDependencyReport /: Values[PacletDependencyReport[_, rows_Association]] := Values[rows]

(* Formatting uses only the snapshot, including failures and versions captured by an explicit check. *)
dependencyDisplayValue[_Missing] := "\[LongDash]"
dependencyDisplayValue[failure_Failure] := failure["Message"]
dependencyDisplayValue[value_] := value

dependencyDisplayRecord[name_, failure_Failure] := <|"Name" -> name, "Status" -> "Failed", "Diagnostic" -> failure["Message"]|>
dependencyDisplayRecord[_, row_Association] := row

dependencyReportDetails[row_] := {
    If[StringQ[row["Description"]], {"Description: ", Pane[row["Description"], ImageSize -> {UpTo[370], Automatic}]}, Nothing],
    If[KeyExistsQ[row, "Required"], {"Dependency: ", If[row["Required"], "Required", "Optional"]}, Nothing],
    If[KeyExistsQ[row, "Version"], {"Version requirement: ", row["Version"]}, Nothing],
    If[ListQ[row["Platforms"]], {"Platforms: ", StringRiffle[row["Platforms"], ", "]}, Nothing],
    If[StringQ[row["License"]], {"License: ", row["License"]}, Nothing],
    If[StringQ[row["URL"]], {"Setup: ", Hyperlink["Instructions", row["URL"]]}, Nothing],
    If[FailureQ[row["ReadinessFailure"]], {"Readiness: ", row["ReadinessFailure"]["Message"]}, Nothing],
    If[KeyExistsQ[row, "Diagnostic"], {"Diagnostic: ", row["Diagnostic"]}, Nothing]
}

dependencyReportIcon = Graphics[{
    GrayLevel[.48], AbsoluteThickness[1.6], Line[{{0, 0}, {1, 1}, {2, 0}}],
    EdgeForm[Directive[GrayLevel[.38], AbsoluteThickness[1.3]]], FaceForm[GrayLevel[.91]],
    Disk[{0, 0}, .24], Disk[{1, 1}, .24], Disk[{2, 0}, .24]
}, ImageSize -> 32, PlotRange -> {{-.35, 2.35}, {-.45, 1.45}}]

PacletDependencyReport /: MakeBoxes[report : PacletDependencyReport[owner_String, rows_Association],
        form : (StandardForm | TraditionalForm)] := Module[{records, row, summary, details, columns},
    records = KeyValueMap[dependencyDisplayRecord, rows];
    Switch[Length[records],
        0, summary = {{"Paclet: ", owner}, {"Dependencies: ", 0}}; details = {},
        1, row = First[records];
           summary = {{"Library: ", row["Name"]}, {"Status: ", row["Status"]},
               {"Installed version: ", dependencyDisplayValue[row["InstalledVersion"]]},
               If[KeyExistsQ[row, "LatestVersion"], {"Latest version: ", dependencyDisplayValue[row["LatestVersion"]]}, Nothing]};
           details = Prepend[dependencyReportDetails[row], {"Declared by: ", owner}],
        _, summary = {{"Paclet: ", owner}, {"Dependencies: ", Length[records]}};
           columns = {"Name", "Status", "InstalledVersion", If[AnyTrue[records, KeyExistsQ[#, "LatestVersion"] &], "LatestVersion", Nothing]};
           details = {{"Libraries: ", Pane[Grid[Prepend[
               (dependencyDisplayValue /@ Lookup[#, columns, Missing[]] & /@ records),
               Map[Style[#, Bold] &, Take[{"Library", "Status", "Installed", "Latest"}, Length[columns]]]],
               Alignment -> Left], ImageSize -> {Automatic, UpTo[300]}, Scrollbars -> Automatic]}}
    ];
    BoxForm`ArrangeSummaryBox[PacletDependencyReport, report, dependencyReportIcon,
        BoxForm`SummaryItem /@ summary, BoxForm`SummaryItem /@ details, form, "Interpretable" -> Automatic]
]
