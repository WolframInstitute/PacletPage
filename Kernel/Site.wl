(* ::Package:: *)

(* ==================== text for HTML, JSON and templates ==================== *)

(* the files of Assets/Web, read as UTF-8 *)
readAsset[name_String] := ByteArrayToString[ReadByteArray[FileNameJoin[{$webDirectory, name}]], "UTF-8"];

(* text for HTML content and attribute values: markup characters as entities and every character
   past ASCII as a numeric reference, so the page reads right whatever charset a server declares *)
esc[s_String] := StringReplace[s, {
    "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;", "\"" -> "&quot;", "'" -> "&#39;",
    c : RegularExpression["[^\\x00-\\x7F]"] :> "&#" <> IntegerString[First[ToCharacterCode[c]]] <> ";"
}];
esc[other_] := esc[TextString[other]];

(* JSON, all ASCII, safe inside a <script> element: no "<", ">" or "&" appears literally, so
   neither "</script>" nor "<!--" can end the element early *)
jsonCharacter[c_String] := With[{code = First[ToCharacterCode[c]]},
    If[ code > 65535,
        StringJoin[jsonUnit /@ {55296 + BitShiftRight[code - 65536, 10], 56320 + BitAnd[code - 65536, 1023]}],
        jsonUnit[code]
    ]
];
jsonUnit[code_Integer] := "\\u" <> IntegerString[code, 16, 4];

jsonString[s_String] := "\"" <> StringReplace[s, {
    "\\" -> "\\\\", "\"" -> "\\\"", "\n" -> "\\n", "\r" -> "\\r", "\t" -> "\\t",
    c : RegularExpression["[\\x00-\\x1F<>&\\x7F-\\x{10FFFF}]"] :> jsonCharacter[c]
}] <> "\"";

toJSON[s_String] := jsonString[s];
toJSON[True] := "true";
toJSON[False] := "false";
toJSON[Null | None] := "null";
toJSON[n_Integer] := IntegerString[n] /; n >= 0;
toJSON[n_Integer] := "-" <> IntegerString[-n];
toJSON[list_List] := "[" <> StringRiffle[toJSON /@ list, ","] <> "]";
toJSON[a_Association] := "{" <> StringRiffle[KeyValueMap[jsonString[ToString[#1]] <> ":" <> toJSON[#2] &, a], ","] <> "}";

(* a template's `slot`s filled in, each value inserted as it is *)
fillTemplate[text_String, slots_Association] := StringReplace[text,
    "`" ~~ key : (WordCharacter ..) ~~ "`" /; KeyExistsQ[slots, key] :> slots[key]];

(* a CSS color: a hex string as given, or a color *)
cssColor[s_String] /; StringMatchQ[s, RegularExpression["#([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})"]] := s;
cssColor[c_?ColorQ] := "#" <> StringJoin[IntegerString[Round[255 #], 16, 2] & /@ Take[List @@ ColorConvert[c, "RGB"], 3]];
cssColor[_] := $Failed;

(* "WolframInstitute" -> "Wolfram Institute" *)
publisherLabel[name_String] := StringTrim @ StringReplace[First[StringSplit[name, "/"]],
    a : CharacterRange["a", "z"] ~~ b : CharacterRange["A", "Z"] :> a <> " " <> b];

(* ==================== the navigation ==================== *)

(* a link of the rails: the page's id, its title, the words the filter also matches, and the
   notebook behind it when that is not named by the id *)
navLink[id_String, page_Association, class_String, notebooks_Association] := StringJoin[
    "<a class=\"", class, "\" data-id=\"", esc[id], "\"",
    If[page["Keywords"] === {}, "", " data-kw=\"" <> esc[ToLowerCase[StringRiffle[page["Keywords"], " "]]] <> "\""],
    If[KeyExistsQ[notebooks, id], " data-nb=\"" <> esc[notebooks[id]] <> "\"", ""],
    " href=\"#", esc[id], "\">", camelBreaks[esc[page["Title"]]], "</a>"
];

(* a line may break at each CamelCase step of a long name, rather than at any letter *)
camelBreaks[s_String] := StringReplace[s, RegularExpression["([a-z0-9])(?=[A-Z])"] -> "$1<wbr>"];

(* The guide tree: a guide's children are the guides it links from its section headings, in its
   order. A guide sits under every guide that links it; only a guide already on the path from the
   root is left out, which breaks cycles. *)
guideTree[pages_Association, id_String, path_List] := If[
    MemberQ[path, id] || ! KeyExistsQ[pages, id],
    Nothing,
    <|"ID" -> id, "Children" -> Map[guideTree[pages, #, Append[path, id]] &, pages[id]["Guides"]]|>
];

treeItem[pages_Association, notebooks_Association, node_Association] := With[
    {a = navLink[node["ID"], pages[node["ID"]], "gd", notebooks]},
    If[ node["Children"] === {},
        "<li class=\"leaf\">" <> a <> "</li>",
        StringJoin["<li class=\"branch\"><details><summary>", a, "</summary><ul>",
            treeItem[pages, notebooks, #] & /@ node["Children"], "</ul></details></li>"]
    ]
];

treeIDs[node_Association] := Prepend[Catenate[treeIDs /@ node["Children"]], node["ID"]];

applicationQ[page_Association] := AnyTrue[page["Keywords"], StringContainsQ[#, "application", IgnoreCase -> True] &];

(* symbol order: alphabetical, ignoring case, with $-named symbols last *)
symbolKey[page_Association] := {StringStartsQ[page["Name"], "$"], ToLowerCase[page["Name"]], page["Name"]};

(* the left rail: the resource's definition notebook, the root guide, its tree, the guides it
   does not reach, then the tutorials and the applications (tutorials with the keyword
   "application") *)
sidebarHTML[catalog_Association, notebooks_Association] := Module[
    {pages = catalog["Pages"], root = catalog["RootGuide"], tree, reached, others, tutorials, list, definitions},
    definitions = Select[Keys[pages], StringStartsQ[#, "resource/"] &];
    tree = If[KeyExistsQ[pages, root], guideTree[pages, root, {}], Nothing];
    reached = If[AssociationQ[tree], treeIDs[tree], {}];
    others = Select[Keys[pages], StringStartsQ[#, "guide/"] && ! MemberQ[reached, #] &];
    tutorials = SortBy[Select[Keys[pages], StringStartsQ[#, "tutorial/"] &], {ToLowerCase[pages[#]["Title"]], pages[#]["Title"]} &];
    list[ids_List, label_String, class_String] := If[ ids === {},
        "",
        StringJoin["<div class=\"nav-label\">", label, "</div><ul class=\"nav-tutorials\">",
            Map["<li>" <> navLink[#, pages[#], class <> If[TrueQ[Lookup[pages[#], "Stub", False]], " stub", ""], notebooks] <> "</li>" &, ids], "</ul>"]
    ];
    StringJoin[
        "<ul class=\"nav-root\">",
        Map["<li class=\"leaf\">" <> navLink[#, pages[#], "res", notebooks] <> "</li>" &, definitions],
        If[ AssociationQ[tree],
            {"<li class=\"leaf\">", navLink[root, pages[root], "gd", notebooks], "</li>",
                treeItem[pages, notebooks, #] & /@ tree["Children"]},
            ""
        ],
        treeItem[pages, notebooks, guideTree[pages, #, reached]] & /@ others,
        "</ul>",
        If[tutorials === {}, "", "<div class=\"nav-sep\"></div>"],
        list[Select[tutorials, ! applicationQ[pages[#]] &], "Tutorials", "tut"],
        list[Select[tutorials, applicationQ[pages[#]] &], "Applications", "app"]
    ]
];

(* the right rail: the symbols *)
symbolsHTML[catalog_Association, notebooks_Association] := With[
    {ids = SortBy[Select[Keys[catalog["Pages"]], StringStartsQ[#, "ref/"] &], symbolKey[catalog["Pages"][#]] &]},
    StringJoin["<ul class=\"nav-symbols\">",
        Map["<li>" <> navLink[#, catalog["Pages"][#], "sym", notebooks] <> "</li>" &, ids],
        "</ul>"]
];

(* ==================== notebooks ==================== *)

(* the directory the notebooks of the catalog's documentation pages share *)
documentationPages[catalog_Association] := KeySelect[catalog["Pages"], ! StringStartsQ[#, "resource/"] &];

notebookPrefix[catalog_Association] := Module[{urls = Values[documentationPages[catalog]][[All, "Notebook"]], common},
    common = If[ urls === {}, "", Fold[commonPrefix, urls]];
    StringReplace[common, RegularExpression["[^/]*$"] -> ""]
];

commonPrefix[a_String, b_String] := Module[{n = Min[StringLength[a], StringLength[b]], k = 0},
    While[k < n && StringTake[a, {k + 1}] === StringTake[b, {k + 1}], k++];
    StringTake[a, k]
];

(* the published path of a page under its kind, as the site writes it: the name, URI-encoded but
   for "$" *)
pagePath[id_String] := With[{parts = StringSplit[id, "/", 2]},
    parts[[1]] <> "/" <> StringReplace[URLEncode[parts[[2]]], "%24" -> "$"]
];

(* the notebooks whose path under the prefix is not the page's id: the Paclet Repository's
   storage names; the definition notebook is named in the frame page's configuration instead *)
notebookNames[catalog_Association, prefix_String] := Association @ KeyValueMap[
    Function[{id, page},
        With[{relative = StringDrop[page["Notebook"], StringLength[prefix]]},
            If[relative === pagePath[id] <> ".nb", Nothing, id -> relative]
        ]
    ],
    documentationPages[catalog]
];

(* the page of the resource's definition notebook, or None *)
definitionID[catalog_Association] := SelectFirst[Keys[catalog["Pages"]], StringStartsQ[#, "resource/"] &, None];

(* ==================== the site's files ==================== *)

(* The stock chrome of a page the site frames as it is published (a resource page): the header,
   rail and footer, hidden by a stylesheet injected into the frame, on the same origin. *)
$stockChromeCSS = StringJoin[
    "header.stickyheader,#pg-header,#pac-nav-sidebar,#pac-nav-sidebar-frame,",
    ".page-sidebar,.page-sidebar-frame,.page-sidebar-toggle,#pageSidebar,#pageSidebarFrame,",
    ".menu-container,#_footer,#_footer-offset{display:none!important}",
    (* the page stylesheet reserves a band for the wolfram.com header as a border on the root *)
    "html{border-top:0!important}",
    ".shingle-content,main.shingle-content{margin-left:0!important;max-width:none!important;width:auto!important}",
    (* the embed page caps its column at 830px; in a frame that cap only wastes width *)
    ".wrap{max-width:none!important;padding:0 24px!important;margin:0!important}",
    (* a caption carrying each input's code for a screen reader, a class no shipped stylesheet defines *)
    ".screen-reader-only{position:absolute!important;width:1px;height:1px;margin:-1px;padding:0;",
    "overflow:hidden;clip:rect(0 0 0 0);white-space:nowrap;border:0}",
    (* icons the shipped asset bundle lacks, drawn as broken images beside their text labels *)
    "img[src*=\"/img/DetailsOptions-\"],img[src*=\"/img/Options-hamburger-\"]{display:none!important}",
    (* the cloud draws a summary box's cells top-aligned, so its expand button and icon sit at the
       top of the box; the desktop draws them centered beside the box's rows *)
    ".FrameBox-Inner .PaneSelectorBox-Inner td{vertical-align:middle!important}",
    (* the resource page alone: a Paclet Source link for the author, which the site's own toggle
       replaces; an Examples section with no examples is hidden by the page, which can tell *)
    "#Paclet-Source,#Paclet-Source+ul{display:none!important}"
];

$defaultFeedbackEmail = "To: `address`
Subject: `site` documentation feedback: {title}

Page: {title} ({page})
Link: {link}
Showing: {view}
Notebook: {notebook}
Date: {date}

What is wrong, unclear or missing on this page?


Where on the page (section, example or output)?


What did you expect to see instead?


Anything else? (optional)

";

(* the email template: None, an address, a template with To: and Subject: lines, or a file of one *)
feedbackTemplate[None | "", _] := "";
feedbackTemplate[address_String, title_String] /; StringMatchQ[address, RegularExpression["[^\\s@]+@[^\\s@]+"]] :=
    fillTemplate[$defaultFeedbackEmail, <|"address" -> address, "site" -> title|>];
feedbackTemplate[File[file_String], _] := ByteArrayToString[ReadByteArray[file], "UTF-8"];
feedbackTemplate[template_String, _] := template;
feedbackTemplate[_, _] := $Failed;

(* the address an email setting names: an address, or the To: line of a template or of a file of one *)
emailAddressQ[s_String] := StringMatchQ[s, RegularExpression["[^\\s@]+@[^\\s@]+"]];
feedbackAddress[address_String] /; emailAddressQ[address] := address;
feedbackAddress[template_String] := Replace[
    StringCases[template, StartOfLine ~~ "To:" ~~ (" " | "\t") ... ~~ address : Except[WhitespaceCharacter] .. :> address, 1],
    {{address_String} /; emailAddressQ[address] :> address, _ -> None}
];
feedbackAddress[File[file_String]] /; FileExistsQ[file] := feedbackAddress[ByteArrayToString[ReadByteArray[file], "UTF-8"]];
feedbackAddress[_] := None;

Options[PacletPageFiles] = {
    "SiteURL" -> "",
    "PageURL" -> "page.html",
    "FeedbackURL" -> "",
    "SourceURL" -> "",
    "Title" -> Automatic,
    "Subtitle" -> Automatic,
    "Accent" -> "#dd1100",
    "Home" -> Automatic,
    "FeedbackEmail" -> None,
    "FeedbackThanks" -> Automatic,
    "DefinitionPage" -> True
};

PacletPageFiles::accent = "`1` is not a color; give a color or a hex string such as \"#dd1100\".";
PacletPageFiles::home = "`1` is not a home page; use \"Definition\", \"RootGuide\", \"Resource\" or Automatic.";
PacletPageFiles::email = "The feedback email `1` is not an address, a template with a To: line, or a File of one.";

PacletPageFiles[catalog_, opts : OptionsPattern[]] /; ! catalogQ[catalog] := (Message[PacletPageFiles::catalog, catalog]; $Failed);

PacletPageFiles[site_?catalogQ, opts : OptionsPattern[]] := Module[
    {catalog, title, subtitle, accent, home, mail, prefix, notebooks, routes, kind, page, shellConfig, pageConfig, css},
    (* the definition notebook is a page of the site unless "DefinitionPage" -> False *)
    catalog = If[ TrueQ[OptionValue["DefinitionPage"]], site, MapAt[KeySelect[! StringStartsQ[#, "resource/"] &], site, Key["Pages"]]];
    title = Replace[OptionValue["Title"], Automatic -> catalog["Title"]];
    subtitle = Replace[OptionValue["Subtitle"], Automatic :> publisherLabel[catalog["Name"]]];
    accent = cssColor[OptionValue["Accent"]];
    If[accent === $Failed, Message[PacletPageFiles::accent, OptionValue["Accent"]]; Return[$Failed, Module]];
    (* the page the site opens on: the definition notebook, else the root guide, else the
       resource page itself *)
    home = Replace[OptionValue["Home"], {
        Automatic :> Which[definitionID[catalog] =!= None, "Definition", KeyExistsQ[catalog["Pages"], catalog["RootGuide"]], "RootGuide", True, "Resource"],
        "Definition" /; definitionID[catalog] === None -> "RootGuide",
        "RootGuide" /; ! KeyExistsQ[catalog["Pages"], catalog["RootGuide"]] -> "Resource"
    }];
    If[! MatchQ[home, "Definition" | "RootGuide" | "Resource"], Message[PacletPageFiles::home, home]; Return[$Failed, Module]];
    mail = feedbackTemplate[OptionValue["FeedbackEmail"], title];
    If[! StringQ[mail], Message[PacletPageFiles::email, OptionValue["FeedbackEmail"]]; Return[$Failed, Module]];
    kind = If[catalog["Kind"] === "Cloud", "cloud", "repository"];
    prefix = If[kind === "cloud", catalog["DocBase"], notebookPrefix[catalog]];
    notebooks = notebookNames[catalog, prefix];
    routes = {catalog["DocBase"], "https://reference.wolfram.com/language/" <> catalog["Name"] <> "/", "paclet:" <> catalog["Name"] <> "/"};
    page = OptionValue["PageURL"];
    shellConfig = <|
        "name" -> catalog["Name"],
        "title" -> title,
        "kind" -> kind,
        "docBase" -> catalog["DocBase"],
        "resource" -> catalog["Resource"],
        "page" -> page,
        "home" -> <|
            "type" -> If[home === "Resource", "resource", "page"],
            "id" -> If[home === "Definition", definitionID[catalog], catalog["RootGuide"]],
            "url" -> catalog["Resource"]
        |>,
        "routes" -> routes,
        "hide" -> $stockChromeCSS,
        "prefetch" -> If[KeyExistsQ[catalog["Pages"], catalog["RootGuide"]], catalog["Pages"][catalog["RootGuide"]]["Notebook"], ""]
    |>;
    pageConfig = <|
        "title" -> title,
        "kind" -> kind,
        "docBase" -> catalog["DocBase"],
        "notebooks" -> prefix,
        "definition" -> If[definitionID[catalog] === None, "", catalog["Pages"][definitionID[catalog]]["Notebook"]],
        "resource" -> catalog["Resource"],
        "source" -> OptionValue["SourceURL"],
        "form" -> OptionValue["FeedbackURL"],
        "site" -> OptionValue["SiteURL"],
        "routes" -> routes,
        "hide" -> $stockChromeCSS,
        "mail" -> mail,
        "thanks" -> Replace[OptionValue["FeedbackThanks"], Except[_String] -> ""]
    |>;
    (* the stylesheet's first rule holds its colors *)
    css = StringReplace[readAsset["site.css"], RegularExpression["^:root \\{[^}]*\\}"] ->
        ":root { --pp-accent: " <> accent <> "; --pp-active-bg: " <> cssColor[Lighter[RGBColor[accent], 0.92]] <>
        "; --pp-active-fg: " <> accent <> "; }", 1];
    <|
        "index.html" -> fillTemplate[readAsset["site.html"], <|
            "title" -> esc[title],
            "brand" -> esc[title],
            "subtitle" -> If[subtitle === "" || subtitle === None, "", "<span class=\"sub\">" <> esc[subtitle] <> "</span>"],
            "resource" -> esc[catalog["Resource"]],
            "resourceLabel" -> If[kind === "cloud", "Resource page", "Paclet Repository"],
            "css" -> css,
            "sidebar" -> sidebarHTML[catalog, notebooks],
            "symbols" -> symbolsHTML[catalog, notebooks],
            "config" -> toJSON[shellConfig],
            "js" -> readAsset["site.js"]
        |>],
        "page.html" -> fillTemplate[readAsset["page.html"], <|
            "title" -> esc[title],
            "accent" -> accent,
            "config" -> toJSON[pageConfig]
        |>]
    |>
];
