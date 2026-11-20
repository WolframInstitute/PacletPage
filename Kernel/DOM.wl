(* ::Package:: *)

(* ==================== a paclet's documentation as a page of Wolfram Studio ====================

   PacletPageDOM gives a page of a paclet's documentation as WebColumn, WebRow and WebItem, the parts
   Wolfram Studio draws a page from: beside the page's own notebook, the guides, tutorials and
   symbols, in the order and with the titles the site's rails give them. The notebooks are those
   BuildPacletDocumentation writes, as they are, links and all; nothing is read from the cloud,
   built or rendered, and no front end is asked, so the kernel a Studio session runs computes it. *)

Options[PacletPageDOM] = {"Filter" -> "", "StripTitleNumbers" -> True};

PacletPageDOM[dir_String, opts : OptionsPattern[]] := PacletPageDOM[dir, Automatic, opts];

PacletPageDOM[dir_String, page : (_String | Automatic), opts : OptionsPattern[]] := Module[{catalog, id},
    catalog = PacletPageCatalog[File[dir]];
    If[ FailureQ[catalog], Return[catalog, Module]];
    id = domPageID[catalog, page];
    If[ ! KeyExistsQ[catalog["Pages"], id],
        Return[Failure["PacletPageNotFound", <|"MessageTemplate" -> "`1` has no documentation page `2`.",
            "MessageParameters" -> {catalog["Name"], page}, "Page" -> page|>], Module]];
    WebRow[{
        domNav[catalog, id, OptionValue["Filter"], OptionValue["StripTitleNumbers"]],
        WebItem[localNotebook[catalog["Pages"][id]["File"]], "flex" -> "1", "min-width" -> "0"]
    }, "align-items" -> "flex-start", "gap" -> "28px"]
];

(* the page a link or a name means: its main guide; a paclet: URI of the paclet; an id such as
   "ref/Name"; or the name of a symbol *)
domPageID[catalog_Association, Automatic] := catalog["RootGuide"];
domPageID[catalog_Association, uri_String /; StringStartsQ[uri, "paclet:"]] := Replace[{uriPageID[catalog["Name"], uri]}, {{id_String} :> id, _ -> None}];
domPageID[_, id_String /; StringMatchQ[id, ("guide" | "ref" | "tutorial") ~~ "/" ~~ Except["/"] ..]] := id;
domPageID[_, name_String] := "ref/" <> name;

(* The rails as one column: the guide tree from the main guide, the guides it does not reach, the
   tutorials, the applications and the symbols, each entry a link to its page and the page shown
   marked. A filter keeps the entries whose title or keywords hold its text, the guides then in a
   list rather than a tree. *)
domNav[catalog_Association, current_String, filter_String, strip_] := Module[
    {pages = catalog["Pages"], shown, root = catalog["RootGuide"], tree, reached, others, guides, tutorials, symbols, keep, entry, label, branch},
    shown = If[TrueQ[strip], Map[shownPage, pages], pages];
    keep[id_] := StringTrim[filter] === "" || StringContainsQ[shown[id]["Title"], StringTrim[filter], IgnoreCase -> True] ||
        AnyTrue[Lookup[shown[id], "Keywords", {}], StringContainsQ[#, StringTrim[filter], IgnoreCase -> True] &];
    entry[id_, depth_] := WebItem[Hyperlink[shown[id]["Title"], "paclet:" <> catalog["Name"] <> "/" <> id],
        "padding" -> "3px 8px 3px " <> ToString[8 + 14 depth] <> "px", "border-radius" -> "6px",
        Sequence @@ If[id === current, {"font-weight" -> "600", "background" -> "rgba(221, 17, 0, 0.1)"}, {}]];
    label[text_, ids_List, items_List] := If[ ids === {}, {},
        Prepend[items, WebItem[text, "font-size" -> "11px", "text-transform" -> "uppercase", "letter-spacing" -> "0.06em",
            "opacity" -> "0.6", "margin" -> "14px 0 4px 8px"]]];
    branch[node_, depth_] := Prepend[Catenate[branch[#, depth + 1] & /@ node["Children"]], entry[node["ID"], depth]];
    tree = If[StringQ[root] && KeyExistsQ[pages, root], guideTree[pages, root, {}], None];
    reached = If[AssociationQ[tree], treeIDs[tree], {}];
    others = SortBy[Select[Keys[pages], StringStartsQ[#, "guide/"] && ! MemberQ[reached, #] &], guideKey[pages[#]] &];
    guides = If[ StringTrim[filter] === "" && AssociationQ[tree],
        Join[branch[tree, 0], entry[#, 0] & /@ others],
        entry[#, 0] & /@ Select[Join[reached, others], keep]];
    tutorials = Select[SortBy[Select[Keys[pages], StringStartsQ[#, "tutorial/"] &], tutorialKey[pages[#]] &], keep];
    symbols = Select[SortBy[Select[Keys[pages], StringStartsQ[#, "ref/"] &], symbolKey[pages[#]] &], keep];
    WebColumn[Join[
        label["Guides", guides, guides],
        With[{ids = Select[tutorials, ! applicationQ[pages[#]] &]}, label["Tutorials", ids, entry[#, 0] & /@ ids]],
        With[{ids = Select[tutorials, applicationQ[pages[#]] &]}, label["Applications", ids, entry[#, 0] & /@ ids]],
        label["Symbols", symbols, entry[#, 0] & /@ symbols]
    ], "flex" -> "none", "width" -> "260px", "position" -> "sticky", "top" -> "0", "max-height" -> "100vh",
        "overflow-y" -> "auto", "padding" -> "12px 0", "font-size" -> "14px"]
];
