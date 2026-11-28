(* shell-fixture.wls: the shell PacletPage writes around a page in "Expression" mode, as HTML, for
   Tests/shell.test.cjs: a small paclet's notebooks, its catalog, and the shell of one symbol's page,
   the notebook replaced by a placeholder. Run from the paclet's directory. *)
$ProgressReporting = False;
Get[FileNameJoin[{Directory[], "Kernel", "PacletPage.wl"}]];
Begin["WolframInstitute`PacletPage`Private`"];
fixtureDir = CreateDirectory[];
put[path_List, nb_] := With[{file = FileNameJoin[Join[{fixtureDir, "Documentation", "English"}, path]]},
    If[! DirectoryQ[DirectoryName[file]], CreateDirectory[DirectoryName[file], CreateIntermediateDirectories -> True]];
    Export[file, nb, "NB"]];
Export[FileNameJoin[{fixtureDir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Test/Shell\", \"Version\" -> \"1.0.0\"|>]", "Text"];
link[label_, uri_] := ButtonBox[label, BaseStyle -> "Link", ButtonData -> "paclet:Test/Shell/" <> uri];
put[{"Guides", "Shell.nb"}, Notebook[{Cell["Shell Guide", "GuideTitle"],
    Cell[TextData[{link["Alpha", "ref/Alpha"], " ", link["Beta", "ref/Beta"]}], "InlineGuideFunctionListing"],
    Cell[TextData[link["Sub", "guide/Sub"]], "GuideFunctionsSection"]}]];
put[{"Guides", "Sub.nb"}, Notebook[{Cell["Sub Guide", "GuideTitle"], Cell[TextData[link["Gamma", "ref/Gamma"]], "InlineGuideFunctionListing"]}]];
Scan[put[{"ReferencePages", "Symbols", # <> ".nb"}, Notebook[{Cell[#, "ObjectName"]}, TaggingRules -> {"Metadata" -> {"title" -> #, "keywords" -> {"kw" <> #}}}]] &,
    {"Alpha", "AlphaBeta", "Beta", "Gamma"}];
put[{"Tutorials", "Steps.nb"}, Notebook[{Cell["Shell Steps", "Title"], Cell["Some text.", "Text"]}]];
catalog = PacletPageCatalog[File[fixtureDir]];
shell = PacletPage[catalog, "ref/Alpha", "LayoutControls" -> All, "FeedbackURL" -> "https://feedback.test/Form"];
html = xmlToHTML[shell /. _Notebook -> XMLElement["div", {"class" -> "pp-notebook"}, {"The notebook"}]];
DeleteDirectory[fixtureDir, DeleteContents -> True];
End[];
WriteString["stdout", WolframInstitute`PacletPage`Private`html];
