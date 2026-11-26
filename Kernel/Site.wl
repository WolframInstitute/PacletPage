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

(* The guide tree: a guide's children are the guides it links from its section headings, in its
   order. A guide sits under every guide that links it; only a guide already on the path from the
   root is left out, which breaks cycles. *)
guideTree[pages_Association, id_String, path_List] := If[
    MemberQ[path, id] || ! KeyExistsQ[pages, id],
    Nothing,
    <|"ID" -> id, "Children" -> Map[guideTree[pages, #, Append[path, id]] &, pages[id]["Guides"]]|>
];

treeIDs[node_Association] := Prepend[Catenate[treeIDs /@ node["Children"]], node["ID"]];

applicationQ[page_Association] := AnyTrue[page["Keywords"], StringContainsQ[#, "application", IgnoreCase -> True] &];

(* symbol order: alphabetical, ignoring case, with $-named symbols last *)
symbolKey[page_Association] := {StringStartsQ[page["Name"], "$"], ToLowerCase[page["Name"]], page["Name"]};

(* an Overview page: its notebook's metadata says so, or, with no notebook at hand, its name, which
   the Overview template gives it *)
overviewQ[page_Association] := TrueQ[Lookup[page, "Overview", False]] || (page["Kind"] === "tutorial" && page["Name"] === "Overview");

(* the tutorials in the order of their titles, an Overview page first, a title's digits read as its
   numbers: a tutorial numbered 10 comes after one numbered 9 *)
tutorialKey[page_Association] := {If[overviewQ[page], 0, 1], naturalKey[page["Title"]], page["Title"]};

(* the guides no guide reaches, in the order of their titles *)
guideKey[page_Association] := {naturalKey[page["Title"]], page["Title"]};

(* a title in an order that reads each run of digits as its number: the runs padded to one width,
   so that their characters compare as their values do *)
naturalKey[s_String] := StringReplace[ToLowerCase[s], d : DigitCharacter .. :> IntegerString[FromDigits[d], 10, Max[12, StringLength[d]]]];

(* A title's numbering: a number, or numbers joined by dots, before the title proper, parted from it
   by a dot, a parenthesis, a colon or a dash and a space, as in "1. ", "2.3 ", "4) ", "5: " or "06 - ".
   A number that is part of the title, as in "3D Graphics" or "2048 Games", is not one. *)
$titleNumbering = RegularExpression["^\\s*(?:\\d+(?:\\.\\d+)*[.):]|\\d+(?:\\.\\d+)+|\\d+\\s*[-\\x{2013}\\x{2014}])\\s+(?=\\S)"];

strippedTitle[title_String] := StringReplace[title, $titleNumbering -> "", 1];

(* a tutorial or a guide as the rail shows it: its title without its numbering *)
shownPage[page_Association] /; MemberQ[{"tutorial", "guide"}, page["Kind"]] && StringQ[page["Title"]] := Append[page, "Title" -> strippedTitle[page["Title"]]];
shownPage[page_] := page;

(* a page as the rail lists it: an Overview is labelled Overview, since the Overview template titles
   it after the paclet, as its root guide is, unless its title says Overview already *)
railPage[page_Association] := If[ overviewQ[page] && ! StringContainsQ[page["Title"], "Overview", IgnoreCase -> True],
    Append[page, "Title" -> "Overview"],
    page
];

(* HTML serialization wrappers over Shell.wl's unified XMLElement rail builders *)
sidebarHTML[catalog_Association, notebooks_Association, rootGuide_ : True, strip_ : True] :=
    xmlToHTML[leftRailXML[catalog, <|
        "Mode" -> "HTML", "Name" -> catalog["Name"], "Prefix" -> "", "Notebooks" -> notebooks,
        "HomeID" -> catalog["RootGuide"], "ShowRootGuide" -> TrueQ[rootGuide],
        "StripTitleNumbers" -> TrueQ[strip], "Filter" -> "", "Current" -> None,
        "IncludeSymbols" -> False, "GuidesHeading" -> False
    |>]];

symbolRailSlots[catalog_Association, notebooks_Association, enabled_ : True, index_ : Automatic] :=
    htmlSymbolsRailParts[catalog, <|
        "Mode" -> "HTML", "Name" -> Lookup[catalog, "Name", ""], "Prefix" -> "", "Notebooks" -> notebooks,
        "SymbolsRail" -> TrueQ[enabled], "SymbolIndex" -> index, "Resource" -> Lookup[catalog, "Resource", ""],
        "Filter" -> "", "Current" -> None
    |>]["Slots"];

symbolsHTML[catalog_Association, notebooks_Association] :=
    xmlToHTML[symbolsRailXML[catalog, <|
        "Mode" -> "HTML", "Name" -> catalog["Name"], "Prefix" -> "", "Notebooks" -> notebooks,
        "Filter" -> "", "Current" -> None
    |>]];

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
    (* the stock template's paclet guide below the details: the site's rail holds the guides *)
    "div.documentation-notebook.paclet-guide{display:none!important}",
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

(* A page rendered on this computer comes with the documentation center's header and search, its
   footer, and a side menu and title menus whose stylesheets the renderer does not ship; the site's
   rails do their work *)
$localChromeCSS = StringJoin[
    "header,.stickyheader,#_footer,#_footer-offset,footer,.menu-container,.select-container2{display:none!important}",
    "html{border-top:0!important}body{padding-top:0!important;margin-top:0!important}",
    ".page-container .content-container{margin-left:0!important;max-width:none!important}"
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
    "DefinitionPage" -> True,
    "ViewSource" -> True,
    "Filter" -> True,
    "SymbolsRail" -> True,
    "ResourceLink" -> True,
    "AllFeedbackLink" -> True,
    "VersionLabel" -> False,
    "Footer" -> False,
    "RootGuide" -> True,
    "StripTitleNumbers" -> True,
    "LayoutControls" -> True,
    "DarkMode" -> Automatic,
    "SymbolIndex" -> Automatic
};

(* "DarkMode": Automatic follows the reader's system setting, True starts dark, False keeps the
   site light; with either of the first two the header has a button that switches, and the reader's
   choice is kept for every PacletPage site on the cloud, which share one origin *)
siteTheme[Automatic] := "auto";
siteTheme[True] := "dark";
siteTheme[False | None] := "off";
siteTheme[_] := $Failed;

(* "SymbolIndex": the A-Z index of a symbols rail of more than 24 symbols, off until the reader
   turns it on (Automatic), on until the reader turns it off (True), or none (False) *)
siteSymbolIndex[spec : (Automatic | True | False)] := spec;
siteSymbolIndex[None] := False;
siteSymbolIndex[_] := $Failed;

(* The theme before the first paint: the reader's choice, else the site's default, the system's for
   "auto". The shell and the page each run it in their head, and follow a choice made in another
   document of the origin, and a change of the system's setting. *)
themeHeadScript["off"] := "";
themeHeadScript[default_String] := StringJoin[
    "<script>(function () {",
    "var root = document.documentElement, fallback = '", default, "';",
    "function stored() { try { return localStorage.getItem('pp-theme'); } catch (e) { return null; } }",
    "function system() { return !!(window.matchMedia && matchMedia('(prefers-color-scheme: dark)').matches); }",
    "function apply() { var s = stored(); root.setAttribute('data-theme', s === 'dark' || s === 'light' ? s : ",
    "(fallback === 'dark' || (fallback === 'auto' && system())) ? 'dark' : 'light'); }",
    "root.setAttribute('data-theme-default', fallback); apply(); window.ppApplyTheme = apply;",
    "addEventListener('storage', function (e) { if (e.key === 'pp-theme' || e.key === null) apply(); });",
    "if (window.matchMedia) { var q = matchMedia('(prefers-color-scheme: dark)'); if (q.addEventListener) q.addEventListener('change', apply); }",
    "})();</script>"
];

(* The label under the site's name: the version given, or with True the catalog's "Version", the
   paclet's published version; none otherwise. *)
versionLabel[label_String, _] := label;
versionLabel[True, catalog_Association] := Replace[Lookup[catalog, "Version", None], Except[_String] -> None];
versionLabel[_, _] := None;

brandHTML[title_String, label_] := xmlToHTML[brandNameXML[title, label, "", False]];

footerHTML[version_] := xmlToHTML[footerXML[<|"Mode" -> "HTML", "PacletPageVersion" -> version|>]];

PacletPageFiles::accent = "`1` is not a color; give a color or a hex string such as \"#dd1100\".";
PacletPageFiles::home = "`1` is not a home page; use \"Definition\", \"RootGuide\", \"Resource\" or Automatic.";
PacletPageFiles::email = "The feedback email `1` is not an address, a template with a To: line, or a File of one.";
PacletPageFiles::layoutcontrols = "`1` is not a LayoutControls setting; use False, True, All, or a list of \"LeftSidebar\", \"RightSidebar\", and \"Fullscreen\".";
PacletPageFiles::darkmode = "`1` is not a DarkMode setting; use Automatic, True or False.";
PacletPageFiles::symbolindex = "`1` is not a SymbolIndex setting; use Automatic, True or False.";

PacletPageFiles[catalog_, opts : OptionsPattern[]] /; ! catalogQ[catalog] := (Message[PacletPageFiles::catalog, catalog]; $Failed);

PacletPageFiles[site_?catalogQ, opts : OptionsPattern[]] := Module[
    {catalog, title, subtitle, accent, home, mail, controls, theme, index, prefix, notebooks, routes, kind, page, shellConfig, pageConfig, css},
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
    controls = normalizeLayoutControls[OptionValue["LayoutControls"]];
    If[controls === $Failed, Message[PacletPageFiles::layoutcontrols, OptionValue["LayoutControls"]]; Return[$Failed, Module]];
    theme = siteTheme[OptionValue["DarkMode"]];
    If[theme === $Failed, Message[PacletPageFiles::darkmode, OptionValue["DarkMode"]]; Return[$Failed, Module]];
    index = siteSymbolIndex[OptionValue["SymbolIndex"]];
    If[index === $Failed, Message[PacletPageFiles::symbolindex, OptionValue["SymbolIndex"]]; Return[$Failed, Module]];
    kind = Switch[catalog["Kind"], "Cloud", "cloud", "Local", "local", _, "repository"];
    prefix = Switch[kind, "cloud", catalog["DocBase"], "local", "", _, notebookPrefix[catalog]];
    notebooks = If[kind === "local", <||>, notebookNames[catalog, prefix]];
    (* a local site's pages link each other relative to the site, which the shell resolves; a
       published site routes its own documentation, the paclet's pages on reference.wolfram.com,
       the reference.wolframcloud.com URLs the cloud shows a paclet: link as, and paclet: URIs *)
    routes = If[ kind === "local",
        {catalog["DocBase"], "./", "paclet:" <> catalog["Name"] <> "/"},
        {catalog["DocBase"], "https://reference.wolfram.com/language/" <> catalog["Name"] <> "/",
            "https://reference.wolframcloud.com/cloudplatform/" <> catalog["Name"] <> "/", "paclet:" <> catalog["Name"] <> "/"}
    ];
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
        "hide" -> If[kind === "local", $localChromeCSS, $stockChromeCSS],
        "prefetch" -> If[kind =!= "local" && KeyExistsQ[catalog["Pages"], catalog["RootGuide"]], catalog["Pages"][catalog["RootGuide"]]["Notebook"], ""],
        "filter" -> TrueQ[OptionValue["Filter"]],
        "theme" -> theme
    |>;
    pageConfig = <|
        "title" -> title,
        "kind" -> kind,
        "docBase" -> catalog["DocBase"],
        "notebooks" -> prefix,
        "definition" -> If[definitionID[catalog] === None, "", catalog["Pages"][definitionID[catalog]]["Notebook"]],
        "resource" -> catalog["Resource"],
        "source" -> If[TrueQ[OptionValue["ViewSource"]], OptionValue["SourceURL"], ""],
        "viewSource" -> TrueQ[OptionValue["ViewSource"]],
        "allFeedback" -> TrueQ[OptionValue["AllFeedbackLink"]],
        "form" -> OptionValue["FeedbackURL"],
        "site" -> OptionValue["SiteURL"],
        "routes" -> routes,
        "hide" -> $stockChromeCSS,
        "mail" -> mail,
        "thanks" -> Replace[OptionValue["FeedbackThanks"], Except[_String] -> ""],
        "theme" -> theme
    |>;
    (* the stylesheet's first rule holds the colors made from the accent, for the light theme and
       the dark one *)
    css = StringReplace[readAsset["site.css"], RegularExpression["^:root \\{[^}]*\\}"] ->
        ":root { --pp-accent: " <> accent <> "; --pp-active-bg: " <> cssTintHex[accent, 0.92] <>
        "; --pp-active-fg: " <> accent <> "; --pp-dark-active-bg: " <> cssMixHex[accent, "#1e2023", 0.78] <>
        "; --pp-dark-active-fg: " <> cssMixHex[accent, "#ffffff", 0.35] <> "; }", 1];
    <|
        "index.html" -> fillTemplate[readAsset["site.html"], <|
            "title" -> esc[title],
            "head" -> themeHeadScript[theme],
            (* the parts of the shell a switch leaves out, hidden by the stylesheet *)
            "bodyClass" -> StringRiffle[Join[
                Pick[{"no-filter", "no-symbols", "no-resource-link"}, ! TrueQ[OptionValue[#]] & /@ {"Filter", "SymbolsRail", "ResourceLink"}],
                If[TrueQ[OptionValue["Footer"]], {"with-footer"}, {}]
            ], " "],
            "css" -> css,
            "body" -> xmlToHTML[shellXML[catalog, <|
                "Mode" -> "HTML",
                "Name" -> catalog["Name"],
                "Prefix" -> "",
                "Title" -> title,
                "Subtitle" -> If[subtitle === "" || subtitle === None, "", subtitle],
                "VersionTag" -> versionLabel[OptionValue["VersionLabel"], catalog],
                "Resource" -> catalog["Resource"],
                "ResourceLabel" -> Switch[kind, "cloud", "Resource page", "local", "Open page", _, "Paclet Repository"],
                "Notebooks" -> notebooks,
                "HomeID" -> catalog["RootGuide"],
                "ShowRootGuide" -> TrueQ[OptionValue["RootGuide"]],
                "StripTitleNumbers" -> TrueQ[OptionValue["StripTitleNumbers"]],
                "SymbolsRail" -> TrueQ[OptionValue["SymbolsRail"]],
                "LayoutControls" -> controls,
                "Theme" -> theme,
                "SymbolIndex" -> index,
                "Footer" -> TrueQ[OptionValue["Footer"]],
                "PacletPageVersion" -> $pacletPageVersion,
                "Filter" -> "",
                "Current" -> None,
                "GuidesHeading" -> False
            |>]],
            "config" -> toJSON[shellConfig],
            "js" -> readAsset["site.js"]
        |>],
        "page.html" -> fillTemplate[readAsset["page.html"], <|
            "title" -> esc[title],
            "head" -> themeHeadScript[theme],
            "accent" -> accent,
            "config" -> toJSON[pageConfig]
        |>]
    |>
];
