(* ::Package:: *)

(* ==================== a paclet's documentation as a WL XML expression ====================

   PacletPage gives a documentation page of a local or installed paclet as an XMLElement shell
   holding the three-column PacletPage layout (header, collapsible guide tree, tutorials and
   applications, live Notebook[...] in the center frame, symbols rail, footer). Nothing is read
   from the cloud, built or rendered, and no front end is asked, so any Wolfram kernel computes it. *)

Options[PacletPage] = {
    "Filter" -> "",
    "StripTitleNumbers" -> True,
    "SymbolsRail" -> True,
    "Header" -> True,
    "Footer" -> True,
    "Title" -> Automatic,
    "Subtitle" -> Automatic,
    "RootGuide" -> Automatic,
    "LayoutControls" -> False
};

$pageCatalogCache = <||>;

PacletPage[spec_, opts : OptionsPattern[]] := Replace[spec, {
    uri_String /; StringStartsQ[uri, "paclet:"] :> pageFromURI[uri, opts],
    other_ :> PacletPage[other, Automatic, opts]
}];

PacletPage[spec_, page : (_String | Automatic), opts : OptionsPattern[]] := Module[
    {controls, dir, catalog, pages, rootOpt, homeID, id, file, nb, filter, strip, showSymbols,
        showHeader, showFooter, title, subtitle, version, desc, activeTitle,
        leftItems, rightItems, hasSymbols, hasLeft, layoutEl, shellClass, parts},
    controls = normalizeLayoutControls[OptionValue["LayoutControls"]];
    If[ controls === $Failed,
        Return[Failure["InvalidOption", <|
            "MessageTemplate" -> "`1` is not a LayoutControls setting; use False, True, All, or a list of \"LeftSidebar\", \"RightSidebar\", and \"Fullscreen\".",
            "MessageParameters" -> {OptionValue["LayoutControls"]},
            "Option" -> "LayoutControls",
            "Value" -> OptionValue["LayoutControls"]
        |>], Module]
    ];
    catalog = If[ catalogQ[spec],
        spec,
        dir = pagePacletDirectory[spec];
        If[ ! StringQ[dir] || ! DirectoryQ[dir],
            Return[Failure["PacletNotFound", <|
                "MessageTemplate" -> "No paclet was found for `1`.",
                "MessageParameters" -> {spec},
                "Spec" -> spec
            |>], Module]
        ];
        pageLocalCatalog[dir]
    ];
    If[ FailureQ[catalog], Return[catalog, Module]];
    pages = catalog["Pages"];
    rootOpt = OptionValue["RootGuide"];
    homeID = Replace[rootOpt, {
        r_String /; KeyExistsQ[pages, r] :> r,
        r_String /; KeyExistsQ[pages, "guide/" <> r] :> "guide/" <> r,
        _ :> pageDefaultHome[catalog]
    }];
    id = pageResolveID[catalog, homeID, page];
    If[ ! StringQ[id] || ! KeyExistsQ[pages, id],
        Return[Failure["PacletPageNotFound", <|
            "MessageTemplate" -> "`1` has no documentation page `2`.",
            "MessageParameters" -> {catalog["Name"], page},
            "Paclet" -> catalog["Name"],
            "Page" -> page
        |>], Module]
    ];
    file = Lookup[pages[id], "File", Missing["NoFile"]];
    nb = If[StringQ[file], localNotebook[file], Missing["NoFile", id]];
    If[ ! MatchQ[nb, _Notebook],
        Return[Failure["PacletPageUnreadable", <|
            "MessageTemplate" -> "Could not read documentation notebook `1`.",
            "MessageParameters" -> {file},
            "File" -> file
        |>], Module]
    ];
    filter = Replace[OptionValue["Filter"], Except[_String] -> ""];
    strip = TrueQ[OptionValue["StripTitleNumbers"]];
    showSymbols = TrueQ[OptionValue["SymbolsRail"]];
    showHeader = TrueQ[OptionValue["Header"]];
    showFooter = TrueQ[OptionValue["Footer"]];
    title = Replace[OptionValue["Title"], Automatic -> catalog["Title"]];
    subtitle = Replace[OptionValue["Subtitle"], {
        Automatic :> pageSubtitle[catalog["Name"]],
        None | "" -> ""
    }];
    version = Replace[Lookup[catalog, "Version", ""], Except[_String] -> ""];
    desc = Replace[Lookup[catalog, "Description", ""], Except[_String] -> ""];
    activeTitle = If[strip, shownPage[pages[id]]["Title"], pages[id]["Title"]];
    leftItems = pageLeftRail[catalog, homeID, id, filter, strip, ! showSymbols];
    rightItems = If[showSymbols, pageSymbolsRail[catalog, id, filter], {}];
    hasSymbols = rightItems =!= {};
    hasLeft = leftItems =!= {};
    layoutEl = pageLayoutControlsXML[controls, hasLeft, hasSymbols, ! showHeader];
    shellClass = StringRiffle[DeleteCases[{
        "pp-shell",
        If[! hasSymbols, "pp-no-symbols", Nothing],
        If[! hasLeft, "pp-no-left", Nothing]
    }, ""], " "];
    parts = DeleteCases[{
        If[showHeader, pageHeader[catalog, If[StringQ[homeID], homeID, id], title, subtitle, version, filter, layoutEl], layoutEl],
        XMLElement["div", {"class" -> "pp-main"}, DeleteCases[{
            If[hasLeft, XMLElement["nav", {"class" -> "pp-side", "aria-label" -> "Guides and tutorials"}, leftItems], Nothing],
            XMLElement["main", {"class" -> "pp-content"}, {nb}],
            If[hasSymbols, XMLElement["nav", {"class" -> "pp-side-right", "aria-label" -> "Symbols"}, rightItems], Nothing]
        }, Nothing]],
        If[showFooter, pageFooter[catalog["Name"], version, desc], Nothing]
    }, Nothing];
    XMLElement["div", {
        "class" -> shellClass,
        "data-paclet-page" -> catalog["Name"],
        "data-paclet-id" -> id,
        "data-paclet-title" -> activeTitle
    }, parts]
];

(* ==================== paclet & page resolution ==================== *)

pageFromURI[uri_String, opts : OptionsPattern[PacletPage]] := With[
    {clean = StringReplace[uri, "#" ~~ ___ ~~ EndOfString -> ""]},
    Replace[
        StringCases[clean, {
            StartOfString ~~ "paclet:" ~~ p : (Except["/"] .. ~~ "/" ~~ Except["/"] ..) ~~ rest : ("/" ~~ __) ~~ EndOfString :> {p, rest},
            StartOfString ~~ "paclet:" ~~ p : (Except["/"] .. ~~ "/" ~~ Except["/"] ..) ~~ EndOfString :> {p, ""},
            StartOfString ~~ "paclet:" ~~ p : (Except["/"] ..) ~~ rest : ("/" ~~ ("guide" | "ref" | "tutorial") ~~ "/" ~~ __) ~~ EndOfString /;
                ! MemberQ[{"guide", "ref", "tutorial"}, p] :> {p, rest}
        }, 1],
        {
            {{name_String, ""}} :> PacletPage[name, Automatic, opts],
            {{name_String, _String}} :> PacletPage[name, clean, opts],
            _ :> Failure["PacletNotFound", <|
                "MessageTemplate" -> "`1` is not a paclet documentation URI.",
                "MessageParameters" -> {uri},
                "URI" -> uri
            |>]
        }
    ]
];

pagePacletDirectory[paclet_PacletObject] := Replace[paclet["Location"], Except[_String] -> None];
pagePacletDirectory[File[dir_String]] := If[DirectoryQ[dir], dir, None];
pagePacletDirectory[spec_String] := If[ DirectoryQ[spec],
    spec,
    Replace[PacletFind[spec], {
        {p_PacletObject, ___} :> Replace[p["Location"], Except[_String] -> None],
        _ -> None
    }]
];
pagePacletDirectory[_] := None;

fileUnixTime[file_String] := If[ FileExistsQ[file],
    Replace[UnixTime[FileDate[file]], Except[_?NumericQ] -> 0],
    0
];

(* Cache the local catalog per directory keyed by notebook count and modification times, so
   navigating between pages of a paclet does not reread every documentation notebook. *)
pageLocalCatalog[dir_String] := Module[{files, stamp, cached, catalog},
    files = If[AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ[FileNameJoin[{dir, #}]] &], localDocumentation[dir], <||>];
    If[ files === <||>,
        Return[Failure["PacletPageCatalog", <|
            "MessageTemplate" -> "`1` is not a paclet directory with documentation notebooks.",
            "MessageParameters" -> {dir},
            "Directory" -> dir
        |>], Module]
    ];
    stamp = {
        Length[files],
        Max[0, fileUnixTime /@ Values[files]],
        fileUnixTime[FileNameJoin[{dir, "PacletInfo.wl"}]],
        fileUnixTime[FileNameJoin[{dir, "PacletInfo.m"}]]
    };
    cached = Lookup[$pageCatalogCache, dir, None];
    If[ AssociationQ[cached] && Lookup[cached, "Stamp", None] === stamp,
        Return[cached["Catalog"], Module]
    ];
    catalog = PacletPageCatalog[File[dir]];
    If[ ! FailureQ[catalog], $pageCatalogCache[dir] = <|"Stamp" -> stamp, "Catalog" -> catalog|>];
    catalog
];

pageDefaultHome[catalog_Association] := Module[{pages = catalog["Pages"], root = catalog["RootGuide"]},
    If[ StringQ[root] && KeyExistsQ[pages, root], Return[root, Module]];
    SelectFirst[
        Join[
            SortBy[Select[Keys[pages], StringStartsQ[#, "guide/"] &], guideKey[pages[#]] &],
            SortBy[Select[Keys[pages], StringStartsQ[#, "tutorial/"] &], tutorialKey[pages[#]] &],
            SortBy[Select[Keys[pages], StringStartsQ[#, "ref/"] &], symbolKey[pages[#]] &]
        ],
        StringQ[#] && KeyExistsQ[pages, #] &,
        None
    ]
];

(* the page a link or a name means: the home page; a paclet: URI of the paclet; an id such as
   "ref/Name"; or a bare page or symbol name *)
pageResolveID[_, homeID_, Automatic] := homeID;
pageResolveID[catalog_Association, _, uri_String /; StringStartsQ[uri, "paclet:"]] :=
    Replace[{uriPageID[catalog["Name"], uri]}, {{id_String} :> id, _ -> None}];
pageResolveID[_, _, id_String /; StringMatchQ[id, ("guide" | "ref" | "tutorial") ~~ "/" ~~ Except["/"] ..]] := id;
pageResolveID[catalog_Association, _, name_String] := SelectFirst[
    {"ref/" <> name, "guide/" <> name, "tutorial/" <> name},
    KeyExistsQ[catalog["Pages"], #] &,
    "ref/" <> name
];

pageSubtitle[name_String] := If[StringContainsQ[name, "/"], publisherLabel[name], ""];

(* ==================== XMLElement builders ==================== *)

(* Break long CamelCase names cleanly at word boundaries with <wbr> *)
pageCamelBreaks[s_String] := Module[{parts},
    parts = StringSplit[s, RegularExpression["(?<=[a-z0-9])(?=[A-Z])"]];
    If[Length[parts] <= 1, {s}, Riffle[parts, XMLElement["wbr", {}, {}]]]
];

pageMatchesFilterQ[page_Association, filter_String] := With[{q = StringTrim[filter]},
    q === "" ||
    StringContainsQ[page["Title"], q, IgnoreCase -> True] ||
    StringContainsQ[page["Name"], q, IgnoreCase -> True] ||
    AnyTrue[Lookup[page, "Keywords", {}], StringContainsQ[#, q, IgnoreCase -> True] &]
];

pageAnchor[pacletName_String, id_String, page_Association, class_String, activeQ_] := With[
    {kw = If[Lookup[page, "Keywords", {}] === {}, {}, {"data-kw" -> ToLowerCase[StringRiffle[page["Keywords"], " "]]}]},
    XMLElement["a", Join[
        {
            "class" -> "link " <> class <> If[activeQ, " active", ""],
            "href" -> "paclet:" <> pacletName <> "/" <> id,
            "data-id" -> id
        },
        kw
    ], pageCamelBreaks[page["Title"]]]
];

pageGuideTreeLI[pacletName_String, pages_Association, node_Association, current_String] := With[
    {id = node["ID"], kids = node["Children"]},
    With[{a = pageAnchor[pacletName, id, pages[id], "gd", id === current], activeCls = If[id === current, " active", ""]},
        If[ kids === {},
            XMLElement["li", {"class" -> "leaf" <> activeCls}, {a}],
            With[{openAttr = If[id === current || MemberQ[treeIDs[node], current], {"open" -> "open"}, {}]},
                XMLElement["li", {"class" -> "branch" <> activeCls}, {
                    XMLElement["details", openAttr, {
                        XMLElement["summary", {}, {a}],
                        XMLElement["ul", {}, pageGuideTreeLI[pacletName, pages, #, current] & /@ kids]
                    }]
                }]
            ]
        ]
    ]
];

(* Layout-control placeholder slot (safe div element; hydrated by the host into toggle buttons).
   Emitted only when "LayoutControls" enables at least one applicable control. *)
pageLayoutControlsXML[controls_List, hasLeft_, hasSymbols_, standalone_] := Module[{kinds},
    kinds = DeleteCases[{
        If[MemberQ[controls, "LeftSidebar"] && TrueQ[hasLeft], "left", Nothing],
        If[MemberQ[controls, "RightSidebar"] && TrueQ[hasSymbols], "right", Nothing],
        If[MemberQ[controls, "Fullscreen"], "fullscreen", Nothing]
    }, Nothing];
    If[ kinds === {},
        Nothing,
        XMLElement["div", {
            "class" -> If[TrueQ[standalone], "pp-layout-controls pp-layout-controls-bar", "pp-layout-controls"],
            "role" -> "group",
            "aria-label" -> "Layout",
            "data-pp-layout-controls" -> StringRiffle[kinds, ","]
        }, {}]
    ]
];

pageHeader[catalog_Association, homeID_String, title_String, subtitle_String, version_String, filter_String, layoutEl_] := Module[
    {pages = catalog["Pages"], nGuides, nTuts, nSyms, summary, nameChildren, brandChildren},
    nGuides = Count[Keys[pages], _?(StringStartsQ[#, "guide/"] &)];
    nTuts = Count[Keys[pages], _?(StringStartsQ[#, "tutorial/"] &)];
    nSyms = Count[Keys[pages], _?(StringStartsQ[#, "ref/"] &)];
    summary = StringRiffle[DeleteCases[{
        If[nGuides > 0, ToString[nGuides] <> If[nGuides === 1, " guide", " guides"], ""],
        If[nTuts > 0, ToString[nTuts] <> If[nTuts === 1, " tech note", " tech notes"], ""],
        If[nSyms > 0, ToString[nSyms] <> If[nSyms === 1, " symbol", " symbols"], ""]
    }, ""], " \:00b7 "];
    nameChildren = DeleteCases[{
        title,
        If[version =!= "", XMLElement["span", {"class" -> "pp-ver"}, {If[StringMatchQ[version, DigitCharacter ~~ ___], "v" <> version, version]}], Nothing]
    }, Nothing];
    brandChildren = DeleteCases[{
        XMLElement["span", {"class" -> "pp-name"}, nameChildren],
        If[subtitle =!= "", XMLElement["span", {"class" -> "pp-sub"}, {subtitle}], Nothing]
    }, Nothing];
    XMLElement["header", {"class" -> "pp-hdr"}, DeleteCases[{
        XMLElement["a", {"class" -> "pp-brand link", "href" -> "paclet:" <> catalog["Name"] <> "/" <> homeID}, brandChildren],
        XMLElement["div", {"class" -> "pp-filter-slot", "data-pp-filter" -> filter}, {}],
        layoutEl,
        XMLElement["span", {"class" -> "pp-meta"}, {summary}]
    }, Nothing]]
];

pageLeftRail[catalog_Association, homeID_, current_String, filter_String, strip_, includeSymbols_] := Module[
    {name = catalog["Name"], pages = catalog["Pages"], shown, tree, reached, others, tutorials, tuts, apps, syms, out = {}, listSection},
    shown = If[strip, Map[shownPage, pages], pages];
    tree = If[StringQ[homeID] && StringStartsQ[homeID, "guide/"] && KeyExistsQ[shown, homeID], guideTree[shown, homeID, {}], None];
    reached = If[AssociationQ[tree], treeIDs[tree], {}];
    others = SortBy[Select[Keys[shown], StringStartsQ[#, "guide/"] && ! MemberQ[reached, #] &], guideKey[shown[#]] &];
    tutorials = SortBy[Select[Keys[shown], StringStartsQ[#, "tutorial/"] &], tutorialKey[shown[#]] &];
    If[ StringTrim[filter] === "",
        If[ AssociationQ[tree],
            AppendTo[out, XMLElement["div", {"class" -> "pp-nav-label"}, {"Guides"}]];
            AppendTo[out, XMLElement["ul", {"class" -> "pp-nav-root"}, Join[
                {XMLElement["li", {"class" -> "leaf" <> If[homeID === current, " active", ""]}, {pageAnchor[name, homeID, shown[homeID], "gd", homeID === current]}]},
                pageGuideTreeLI[name, shown, #, current] & /@ tree["Children"]
            ]]]
        ];
        If[ others =!= {},
            AppendTo[out, XMLElement["div", {"class" -> "pp-nav-label"}, {If[AssociationQ[tree], "Other guides", "Guides"]}]];
            AppendTo[out, XMLElement["ul", {"class" -> "pp-nav-root"}, pageGuideTreeLI[name, shown, guideTree[shown, #, reached], current] & /@ others]]
        ],
        With[{matchedGuides = Select[Join[reached, others], pageMatchesFilterQ[shown[#], filter] &]},
            If[ matchedGuides =!= {},
                AppendTo[out, XMLElement["div", {"class" -> "pp-nav-label"}, {"Guides"}]];
                AppendTo[out, XMLElement["ul", {"class" -> "pp-nav-root"},
                    XMLElement["li", {"class" -> "leaf" <> If[# === current, " active", ""]}, {pageAnchor[name, #, shown[#], "gd", # === current]}] & /@ matchedGuides]]
            ]
        ]
    ];
    listSection[ids_List, heading_String, cls_String] := Module[{kept = Select[ids, pageMatchesFilterQ[shown[#], filter] &]},
        If[kept === {}, Return[{}, Module]];
        {
            XMLElement["div", {"class" -> "pp-nav-label"}, {heading}],
            XMLElement["ul", {"class" -> "pp-nav-tutorials"},
                Map[Function[id,
                    With[{p = railPage[shown[id]]},
                        XMLElement["li", {"class" -> If[id === current, "active", ""]}, {
                            pageAnchor[name, id, p, cls <> If[TrueQ[Lookup[p, "Stub", False]], " stub", ""], id === current]
                        }]
                    ]
                ], kept]
            ]
        }
    ];
    tuts = Select[tutorials, ! applicationQ[shown[#]] && pageMatchesFilterQ[shown[#], filter] &];
    apps = Select[tutorials, applicationQ[shown[#]] && pageMatchesFilterQ[shown[#], filter] &];
    If[(tuts =!= {} || apps =!= {}) && out =!= {}, AppendTo[out, XMLElement["div", {"class" -> "pp-nav-sep"}, {}]]];
    out = Join[out, listSection[tuts, "Tutorials", "tut"], listSection[apps, "Applications", "app"]];
    If[ includeSymbols,
        syms = Select[SortBy[Select[Keys[shown], StringStartsQ[#, "ref/"] &], symbolKey[shown[#]] &], pageMatchesFilterQ[shown[#], filter] &];
        If[ syms =!= {},
            If[out =!= {}, AppendTo[out, XMLElement["div", {"class" -> "pp-nav-sep"}, {}]]];
            out = Join[out, {
                XMLElement["div", {"class" -> "pp-nav-label"}, {"Symbols"}],
                XMLElement["ul", {"class" -> "pp-nav-symbols"},
                    Map[Function[id, XMLElement["li", {"class" -> If[id === current, "active", ""]}, {pageAnchor[name, id, shown[id], "sym", id === current]}]], syms]
                ]
            }]
        ]
    ];
    DeleteCases[out, Nothing]
];

pageSymbolsRail[catalog_Association, current_String, filter_String] := Module[
    {name = catalog["Name"], pages = catalog["Pages"], syms},
    syms = Select[
        SortBy[Select[Keys[pages], StringStartsQ[#, "ref/"] &], symbolKey[pages[#]] &],
        pageMatchesFilterQ[pages[#], filter] &
    ];
    If[syms === {}, Return[{}, Module]];
    {
        XMLElement["div", {"class" -> "pp-nav-label"}, {"Symbols"}],
        XMLElement["ul", {"class" -> "pp-nav-symbols"},
            Map[Function[id,
                XMLElement["li", {"class" -> If[id === current, "active", ""]}, {
                    pageAnchor[name, id, pages[id], "sym", id === current]
                }]
            ], syms]
        ]
    }
];

pageFooter[name_String, version_String, desc_String] := XMLElement["footer", {"class" -> "pp-ftr"}, DeleteCases[{
    XMLElement["span", {"class" -> "pp-ftr-pkg"}, {name <> If[version =!= "", " " <> version, ""]}],
    If[desc =!= "", XMLElement["span", {"class" -> "pp-ftr-desc"}, {desc}], Nothing]
}, Nothing]];
