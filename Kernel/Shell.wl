(* ::Package:: *)

(* ==================== unified PacletPage XMLElement shell & HTML serializer ====================

   One implementation builds the three-column PacletPage shell as XMLElement expressions:
   - PacletPage[spec, page, opts] returns the shell XMLElement directly (with live Notebook[...]),
     using only safe HTML layout tags so hosts such as Wolfram Studio render it natively.
   - PacletPageFiles[catalog, opts] calls the same shellXML builder in "HTML" mode and serializes
     it to ASCII HTML via xmlToHTML for index.html. *)

(* ==================== ASCII HTML serializer ==================== *)

$htmlVoidTags = {"wbr", "input", "br", "hr", "img", "meta", "link"};
$svgSelfClosingTags = {"rect", "path", "circle", "line", "polyline", "polygon"};

xmlAttrHTML[k_String -> v_String] := " " <> k <> "=\"" <> esc[v] <> "\"";
xmlAttrHTML[_] := "";

xmlToHTML[s_String] := esc[s];
xmlToHTML[Nothing] := "";
xmlToHTML[list_List] := StringJoin[xmlToHTML /@ list];
xmlToHTML[XMLElement[tag_String, attrs_List, children_List]] := With[
    {a = StringJoin[xmlAttrHTML /@ attrs]},
    Which[
        MemberQ[$htmlVoidTags, tag], "<" <> tag <> a <> ">",
        MemberQ[$svgSelfClosingTags, tag] && children === {}, "<" <> tag <> a <> "/>",
        True, "<" <> tag <> a <> ">" <> xmlToHTML[children] <> "</" <> tag <> ">"
    ]
];

(* ==================== shared accent tint & layout control specs ==================== *)

(* Parse #rgb or #rrggbb and compute the 92% lighter active-background tint in pure arithmetic,
   avoiding ColorConvert/Lighter so kernels without those functions compute it directly. *)
cssTintHex[hex_String, frac_ : 0.92] := Module[{digits, pairs, rgb},
    digits = StringDrop[hex, 1];
    pairs = If[StringLength[digits] === 3,
        StringJoin[#, #] & /@ Characters[digits],
        StringPartition[digits, 2]
    ];
    rgb = FromDigits[#, 16] & /@ pairs;
    "#" <> StringJoin[IntegerString[Round[# * (1 - frac) + 255 * frac], 16, 2] & /@ rgb]
];

$layoutControlNames = {"LeftSidebar", "RightSidebar", "Fullscreen"};

(* Normalize "LayoutControls" into an ordered list of canonical control names, or $Failed. *)
normalizeLayoutControls[False | None | {}] := {};
normalizeLayoutControls[True | All] := $layoutControlNames;
normalizeLayoutControls[name_String] /; MemberQ[$layoutControlNames, name] := {name};
normalizeLayoutControls[names_List] /; AllTrue[names, StringQ[#] && MemberQ[$layoutControlNames, #] &] :=
    Select[$layoutControlNames, MemberQ[names, #] &];
normalizeLayoutControls[_] := $Failed;

$layoutControlSpecs = <|
    "LeftSidebar" -> <|
        "ID" -> "toggleSideL",
        "Kind" -> "left",
        "Label" -> "Toggle left sidebar",
        "Pressed" -> True,
        "Shapes" -> {
            XMLElement["rect", {"x" -> "3", "y" -> "4", "width" -> "14", "height" -> "12", "rx" -> "2"}, {}],
            XMLElement["path", {"d" -> "M8 4v12"}, {}]
        }
    |>,
    "RightSidebar" -> <|
        "ID" -> "toggleSideR",
        "Kind" -> "right",
        "Label" -> "Toggle right sidebar",
        "Pressed" -> True,
        "Shapes" -> {
            XMLElement["rect", {"x" -> "3", "y" -> "4", "width" -> "14", "height" -> "12", "rx" -> "2"}, {}],
            XMLElement["path", {"d" -> "M12 4v12"}, {}]
        }
    |>,
    "Fullscreen" -> <|
        "ID" -> "toggleFull",
        "Kind" -> "fullscreen",
        "Label" -> "Toggle fullscreen",
        "Pressed" -> False,
        "Shapes" -> {
            XMLElement["path", {"d" -> "M3 8V3h5M12 3h5v5M17 12v5h-5M8 17H3v-5"}, {}]
        }
    |>
|>;

layoutSVGXML[shapes_List] := XMLElement["svg", {
    "aria-hidden" -> "true",
    "viewBox" -> "0 0 20 20",
    "fill" -> "none",
    "stroke" -> "currentColor",
    "stroke-width" -> "1.5",
    "stroke-linecap" -> "round",
    "stroke-linejoin" -> "round"
}, shapes];

layoutButtonXML[spec_Association] := With[{pressed = TrueQ[spec["Pressed"]]},
    XMLElement["button", {
        "id" -> spec["ID"],
        "class" -> "pp-layout-btn" <> If[pressed, " on", ""],
        "type" -> "button",
        "data-pp-layout" -> spec["Kind"],
        "aria-label" -> spec["Label"],
        "title" -> spec["Label"],
        "aria-pressed" -> If[pressed, "true", "false"]
    }, {layoutSVGXML[spec["Shapes"]]}]
];

layoutControlsXML[controls_List, hasLeft_, hasSymbols_, mode_String, standalone_ : False] := Module[{active},
    active = Select[controls, Switch[#,
        "LeftSidebar", TrueQ[hasLeft],
        "RightSidebar", TrueQ[hasSymbols],
        _, True
    ] &];
    If[ active === {}, Return[Nothing, Module]];
    If[ mode === "HTML",
        XMLElement["div", {"class" -> "pp-layout-controls", "role" -> "group", "aria-label" -> "Layout"},
            layoutButtonXML[$layoutControlSpecs[#]] & /@ active],
        XMLElement["div", {
            "class" -> If[TrueQ[standalone], "pp-layout-controls pp-layout-controls-bar", "pp-layout-controls"],
            "role" -> "group",
            "aria-label" -> "Layout",
            "data-pp-layout-controls" -> StringRiffle[$layoutControlSpecs[#]["Kind"] & /@ active, ","]
        }, {}]
    ]
];

fullExitXML[controls_List, mode_String] := If[ mode === "HTML" && MemberQ[controls, "Fullscreen"],
    XMLElement["button", {
        "id" -> "ppFullExit",
        "class" -> "pp-full-exit",
        "type" -> "button",
        "data-pp-layout" -> "fullscreen-exit",
        "aria-label" -> "Exit fullscreen",
        "title" -> "Exit fullscreen (Esc)"
    }, {layoutSVGXML[{XMLElement["path", {"d" -> "M8 3v5H3M17 8h-5V3M12 17v-5h5M3 12h5v5"}, {}]}]}],
    Nothing
];

(* ==================== unified navigation builders ==================== *)

(* Break long CamelCase names cleanly at word boundaries with <wbr>. *)
camelBreakXML[s_String] := Module[{parts},
    parts = StringSplit[s, RegularExpression["(?<=[a-z0-9])(?=[A-Z])"]];
    If[Length[parts] <= 1, {s}, Riffle[parts, XMLElement["wbr", {}, {}]]]
];

pageMatchesFilterQ[page_Association, filter_String] := With[{q = StringTrim[filter]},
    q === "" ||
    StringContainsQ[page["Title"], q, IgnoreCase -> True] ||
    StringContainsQ[page["Name"], q, IgnoreCase -> True] ||
    AnyTrue[Lookup[page, "Keywords", {}], StringContainsQ[#, q, IgnoreCase -> True] &]
];

(* A navigation link for either the static HTML site or the WL XMLElement expression. *)
navLinkXML[id_String, page_Association, class_String, cfg_Association] := Module[
    {mode = cfg["Mode"], current = Lookup[cfg, "Current", None], notebooks = Lookup[cfg, "Notebooks", <||>],
        activeQ, cls, href, kw, nbAttr},
    activeQ = (mode === "Expression" && StringQ[current] && id === current);
    cls = If[mode === "Expression", "link " <> class <> If[activeQ, " active", ""], class];
    href = If[mode === "Expression", "paclet:" <> cfg["Name"] <> "/" <> id, "#" <> id];
    kw = If[Lookup[page, "Keywords", {}] === {}, {}, {"data-kw" -> ToLowerCase[StringRiffle[page["Keywords"], " "]]}];
    nbAttr = If[KeyExistsQ[notebooks, id], {"data-nb" -> notebooks[id]}, {}];
    XMLElement["a", Join[{"class" -> cls, "data-id" -> id}, kw, nbAttr, {"href" -> href}], camelBreakXML[page["Title"]]]
];

guideTreeLI[pages_Association, node_Association, cfg_Association] := With[
    {id = node["ID"], kids = node["Children"], mode = cfg["Mode"], current = Lookup[cfg, "Current", None]},
    With[{
        a = navLinkXML[id, pages[id], "gd", cfg],
        activeCls = If[mode === "Expression" && id === current, " active", ""]
    },
        If[ kids === {},
            XMLElement["li", {"class" -> "leaf" <> activeCls}, {a}],
            With[{openAttr = If[mode === "Expression" && (id === current || MemberQ[treeIDs[node], current]), {"open" -> "open"}, {}]},
                XMLElement["li", {"class" -> "branch" <> activeCls}, {
                    XMLElement["details", openAttr, {
                        XMLElement["summary", {}, {a}],
                        XMLElement["ul", {}, guideTreeLI[pages, #, cfg] & /@ kids]
                    }]
                }]
            ]
        ]
    ]
];

(* Unified left rail: definition pages, root guide and its tree, unreached guides, separator,
   tutorials, applications, and (when IncludeSymbols is True) symbols. *)
leftRailXML[catalog_Association, cfg_Association] := Module[
    {mode = cfg["Mode"], pfx = Lookup[cfg, "Prefix", ""], pages = catalog["Pages"],
        homeID = Lookup[cfg, "HomeID", catalog["RootGuide"]],
        showRootGuide = TrueQ[Lookup[cfg, "ShowRootGuide", True]],
        strip = TrueQ[Lookup[cfg, "StripTitleNumbers", True]],
        filter = Replace[Lookup[cfg, "Filter", ""], Except[_String] -> ""],
        current = Lookup[cfg, "Current", None],
        includeSymbols = TrueQ[Lookup[cfg, "IncludeSymbols", False]],
        guidesHeading = TrueQ[Lookup[cfg, "GuidesHeading", False]],
        definitions, shown, tree, reached, others, tutorials, tuts, apps, syms, out = {}, listSection, rootItems},
    definitions = Select[Keys[pages], StringStartsQ[#, "resource/"] &];
    tree = If[StringQ[homeID] && StringStartsQ[homeID, "guide/"] && KeyExistsQ[pages, homeID], guideTree[pages, homeID, {}], None];
    reached = If[AssociationQ[tree], treeIDs[tree], {}];
    others = SortBy[Select[Keys[pages], StringStartsQ[#, "guide/"] && ! MemberQ[reached, #] &], guideKey[pages[#]] &];
    tutorials = SortBy[Select[Keys[pages], StringStartsQ[#, "tutorial/"] &], tutorialKey[pages[#]] &];
    (* Ordered by numbered titles above; strip numbering for display unless StripTitleNumbers -> False. *)
    shown = If[strip, Map[shownPage, pages], pages];
    If[ StringTrim[filter] === "",
        rootItems = DeleteCases[Join[
            Map[XMLElement["li", {"class" -> "leaf" <> If[mode === "Expression" && # === current, " active", ""]}, {navLinkXML[#, shown[#], "res", cfg]}] &, definitions],
            If[ AssociationQ[tree],
                Join[
                    If[showRootGuide,
                        {XMLElement["li", {"class" -> "leaf" <> If[mode === "Expression" && homeID === current, " active", ""]}, {navLinkXML[homeID, shown[homeID], "gd", cfg]}]},
                        {}
                    ],
                    guideTreeLI[shown, #, cfg] & /@ tree["Children"]
                ],
                {}
            ]
        ], Nothing];
        If[ mode === "HTML",
            AppendTo[out, XMLElement["ul", {"class" -> pfx <> "nav-root"}, rootItems]],
            If[ rootItems =!= {},
                If[guidesHeading, AppendTo[out, XMLElement["div", {"class" -> pfx <> "nav-label"}, {"Guides"}]]];
                AppendTo[out, XMLElement["ul", {"class" -> pfx <> "nav-root"}, rootItems]]
            ]
        ];
        If[ others =!= {},
            AppendTo[out, XMLElement["div", {"class" -> pfx <> "nav-label"}, {If[AssociationQ[tree] || mode === "HTML", "Other guides", "Guides"]}]];
            AppendTo[out, XMLElement["ul", {"class" -> pfx <> "nav-root"}, guideTreeLI[shown, guideTree[shown, #, reached], cfg] & /@ others]]
        ],
        With[{matchedGuides = Select[Join[definitions, reached, others], pageMatchesFilterQ[shown[#], filter] &]},
            If[ matchedGuides =!= {},
                If[guidesHeading, AppendTo[out, XMLElement["div", {"class" -> pfx <> "nav-label"}, {"Guides"}]]];
                AppendTo[out, XMLElement["ul", {"class" -> pfx <> "nav-root"},
                    Map[XMLElement["li", {"class" -> "leaf" <> If[mode === "Expression" && # === current, " active", ""]},
                        {navLinkXML[#, shown[#], If[StringStartsQ[#, "resource/"], "res", "gd"], cfg]}] &, matchedGuides]]]
            ]
        ]
    ];
    listSection[ids_List, heading_String, cls_String] := Module[{kept = Select[ids, pageMatchesFilterQ[shown[#], filter] &]},
        If[kept === {}, Return[{}, Module]];
        {
            XMLElement["div", {"class" -> pfx <> "nav-label"}, {heading}],
            XMLElement["ul", {"class" -> pfx <> "nav-tutorials"},
                Map[Function[id,
                    With[{p = railPage[shown[id]], liAttrs = If[mode === "Expression" && id === current, {"class" -> "active"}, {}]},
                        XMLElement["li", liAttrs, {
                            navLinkXML[id, p, cls <> If[TrueQ[Lookup[p, "Stub", False]], " stub", ""], cfg]
                        }]
                    ]
                ], kept]
            ]
        }
    ];
    tuts = Select[tutorials, ! applicationQ[shown[#]] && pageMatchesFilterQ[shown[#], filter] &];
    apps = Select[tutorials, applicationQ[shown[#]] && pageMatchesFilterQ[shown[#], filter] &];
    If[ (tuts =!= {} || apps =!= {}) && (mode === "HTML" || out =!= {}),
        AppendTo[out, XMLElement["div", {"class" -> pfx <> "nav-sep"}, {}]]
    ];
    out = Join[out, listSection[tuts, "Tutorials", "tut"], listSection[apps, "Applications", "app"]];
    If[ includeSymbols,
        syms = Select[SortBy[Select[Keys[shown], StringStartsQ[#, "ref/"] &], symbolKey[shown[#]] &], pageMatchesFilterQ[shown[#], filter] &];
        If[ syms =!= {},
            If[out =!= {}, AppendTo[out, XMLElement["div", {"class" -> pfx <> "nav-sep"}, {}]]];
            out = Join[out, {
                XMLElement["div", {"class" -> pfx <> "nav-label"}, {"Symbols"}],
                XMLElement["ul", {"class" -> pfx <> "nav-symbols"},
                    Map[Function[id,
                        With[{liAttrs = If[mode === "Expression" && id === current, {"class" -> "active"}, {}]},
                            XMLElement["li", liAttrs, {navLinkXML[id, shown[id], "sym", cfg]}]
                        ]
                    ], syms]
                ]
            }]
        ]
    ];
    DeleteCases[out, Nothing]
];

(* Unified right rail: the symbols, sorted alphabetically with $-named symbols last. *)
symbolsRailXML[catalog_Association, cfg_Association] := Module[
    {mode = cfg["Mode"], pfx = Lookup[cfg, "Prefix", ""], pages = catalog["Pages"],
        filter = Replace[Lookup[cfg, "Filter", ""], Except[_String] -> ""],
        current = Lookup[cfg, "Current", None], syms},
    syms = Select[
        SortBy[Select[Keys[pages], StringStartsQ[#, "ref/"] &], symbolKey[pages[#]] &],
        pageMatchesFilterQ[pages[#], filter] &
    ];
    If[ mode === "Expression" && syms === {}, Return[{}, Module]];
    DeleteCases[{
        If[mode === "Expression", XMLElement["div", {"class" -> pfx <> "nav-label"}, {"Symbols"}], Nothing],
        XMLElement["ul", {"class" -> pfx <> "nav-symbols"},
            Map[Function[id,
                With[{liAttrs = If[mode === "Expression" && id === current, {"class" -> "active"}, {}]},
                    XMLElement["li", liAttrs, {navLinkXML[id, pages[id], "sym", cfg]}]
                ]
            ], syms]
        ]
    }, Nothing]
];

(* ==================== unified header, footer & shell ==================== *)

formatVersionTag[label_String] := If[StringMatchQ[label, DigitCharacter ~~ ___], "v" <> label, label];

brandNameXML[title_String, ver_, pfx_String, wrapAlways_] := Module[{hasVer = StringQ[ver] && ver =!= ""},
    If[ hasVer || TrueQ[wrapAlways],
        XMLElement["span", {"class" -> pfx <> "name"}, DeleteCases[{
            title,
            If[hasVer, XMLElement["span", {"class" -> pfx <> "ver"}, {formatVersionTag[ver]}], Nothing]
        }, Nothing]],
        title
    ]
];

headerXML[catalog_Association, cfg_Association, layoutEl_] := Module[
    {mode = cfg["Mode"], pfx = Lookup[cfg, "Prefix", ""], title = cfg["Title"],
        subtitle = Replace[Lookup[cfg, "Subtitle", ""], Except[_String] -> ""],
        ver = Lookup[cfg, "VersionTag", None], pages = catalog["Pages"],
        brandKids, nGuides, nTuts, nSyms, summary, homeHref},
    brandKids = DeleteCases[{
        brandNameXML[title, ver, pfx, mode === "Expression"],
        If[subtitle =!= "", XMLElement["span", {"class" -> pfx <> "sub"}, {subtitle}], Nothing]
    }, Nothing];
    If[ mode === "HTML",
        XMLElement["header", {"id" -> "hdr"}, DeleteCases[{
            XMLElement["button", {"id" -> "navToggle", "type" -> "button", "aria-label" -> "Guides and tutorials"}, {"\:2630"}],
            XMLElement["a", {"class" -> "brand", "id" -> "homelink", "href" -> "#"}, brandKids],
            XMLElement["input", {
                "id" -> "q", "type" -> "search", "autocomplete" -> "off", "spellcheck" -> "false",
                "enterkeyhint" -> "go",
                "placeholder" -> "Filter guides, tutorials and symbols  (/)",
                "title" -> "Type to filter; Enter opens the best match; Esc clears",
                "aria-label" -> "Filter the navigation; Enter opens the best match"
            }, {}],
            XMLElement["button", {"id" -> "symToggle", "type" -> "button", "aria-label" -> "Symbols"}, {"\:22ee"}],
            layoutEl,
            XMLElement["a", {
                "class" -> "home", "id" -> "resourcelink",
                "href" -> Lookup[cfg, "Resource", ""],
                "target" -> "_blank", "rel" -> "noopener"
            }, {
                Lookup[cfg, "ResourceLabel", "Resource page"],
                " ",
                XMLElement["svg", {"viewBox" -> "0 0 24 24"}, {XMLElement["path", {"d" -> "M7 17 17 7M9 7h8v8"}, {}]}]
            }]
        }, Nothing]],
        (* Expression mode *)
        nGuides = Count[Keys[pages], _?(StringStartsQ[#, "guide/"] &)];
        nTuts = Count[Keys[pages], _?(StringStartsQ[#, "tutorial/"] &)];
        nSyms = Count[Keys[pages], _?(StringStartsQ[#, "ref/"] &)];
        summary = StringRiffle[DeleteCases[{
            If[nGuides > 0, ToString[nGuides] <> If[nGuides === 1, " guide", " guides"], ""],
            If[nTuts > 0, ToString[nTuts] <> If[nTuts === 1, " tech note", " tech notes"], ""],
            If[nSyms > 0, ToString[nSyms] <> If[nSyms === 1, " symbol", " symbols"], ""]
        }, ""], " \:00b7 "];
        homeHref = "paclet:" <> catalog["Name"] <> "/" <> Lookup[cfg, "BrandHomeID", Lookup[cfg, "Current", ""]];
        XMLElement["header", {"class" -> "pp-hdr"}, DeleteCases[{
            XMLElement["a", {"class" -> "pp-brand link", "href" -> homeHref}, brandKids],
            If[TrueQ[Lookup[cfg, "ShowFilter", True]],
                XMLElement["div", {"class" -> "pp-filter-slot", "data-pp-filter" -> Replace[Lookup[cfg, "Filter", ""], Except[_String] -> ""]}, {}],
                Nothing
            ],
            layoutEl,
            XMLElement["span", {"class" -> "pp-meta"}, {summary}]
        }, Nothing]]
    ]
];

footerXML[cfg_Association] := If[ cfg["Mode"] === "HTML",
    With[{ver = Lookup[cfg, "PacletPageVersion", None]},
        XMLElement["footer", {"id" -> "ftr"}, {
            "Published with ",
            XMLElement["a", {"href" -> $pacletPageSite, "target" -> "_blank", "rel" -> "noopener"},
                {"PacletPage" <> If[StringQ[ver], " " <> ver, ""]}]
        }]
    ],
    With[{
        name = cfg["Name"],
        ver = Replace[Lookup[cfg, "Version", ""], Except[_String] -> ""],
        desc = Replace[Lookup[cfg, "Description", ""], Except[_String] -> ""]
    },
        XMLElement["footer", {"class" -> "pp-ftr"}, DeleteCases[{
            XMLElement["span", {"class" -> "pp-ftr-pkg"}, {name <> If[ver =!= "", " " <> ver, ""]}],
            If[desc =!= "", XMLElement["span", {"class" -> "pp-ftr-desc"}, {desc}], Nothing]
        }, Nothing]]
    ]
];

(* The unified three-column PacletPage shell.
   - In "HTML" mode: returns the list of top-level body XMLElements for index.html.
   - In "Expression" mode: returns the outer XMLElement["div", {"class" -> "pp-shell ...", ...}, ...]. *)
shellXML[catalog_Association, cfg_Association] := Module[
    {mode = cfg["Mode"], controls = Lookup[cfg, "LayoutControls", {}],
        showSymbols = TrueQ[Lookup[cfg, "SymbolsRail", True]],
        showHeader = TrueQ[Lookup[cfg, "Header", True]],
        showFooter = TrueQ[Lookup[cfg, "Footer", False]],
        leftItems, rightItems, hasLeft, hasSymbols, layoutEl, exitEl, parts, shellClass, accent, styleAttr},
    leftItems = leftRailXML[catalog, Append[cfg, "IncludeSymbols" -> (mode === "Expression" && ! showSymbols)]];
    rightItems = If[mode === "HTML" || showSymbols, symbolsRailXML[catalog, cfg], {}];
    hasLeft = If[mode === "HTML", True, leftItems =!= {}];
    hasSymbols = If[mode === "HTML", showSymbols, rightItems =!= {}];
    layoutEl = layoutControlsXML[controls, hasLeft, hasSymbols, mode, mode === "Expression" && ! showHeader];
    exitEl = fullExitXML[controls, mode];
    If[ mode === "HTML",
        DeleteCases[{
            headerXML[catalog, cfg, layoutEl],
            XMLElement["div", {"id" -> "scrim"}, {}],
            XMLElement["div", {"id" -> "main"}, {
                XMLElement["nav", {"id" -> "side", "aria-label" -> "Guides and tutorials"}, leftItems],
                XMLElement["div", {"class" -> "splitter", "id" -> "splitL", "role" -> "separator", "aria-orientation" -> "vertical",
                    "tabindex" -> "0", "aria-label" -> "Resize the guides rail"}, {}],
                XMLElement["div", {"id" -> "content"}, {
                    XMLElement["div", {"id" -> "loading"}, {"Loading..."}],
                    XMLElement["iframe", {"id" -> "frame", "title" -> "documentation"}, {}]
                }],
                XMLElement["div", {"class" -> "splitter", "id" -> "splitR", "role" -> "separator", "aria-orientation" -> "vertical",
                    "tabindex" -> "0", "aria-label" -> "Resize the symbols rail"}, {}],
                XMLElement["nav", {"id" -> "side-right", "aria-label" -> "Symbols"}, rightItems]
            }],
            exitEl,
            If[showFooter, footerXML[cfg], Nothing]
        }, Nothing],
        (* Expression mode *)
        shellClass = StringRiffle[DeleteCases[{
            "pp-shell",
            If[! hasSymbols, "pp-no-symbols", Nothing],
            If[! hasLeft, "pp-no-left", Nothing]
        }, ""], " "];
        accent = Lookup[cfg, "Accent", Automatic];
        styleAttr = If[StringQ[accent],
            {"style" -> "--pp-accent: " <> accent <> "; --pp-active-bg: " <> cssTintHex[accent, 0.92] <> "; --pp-active-fg: " <> accent <> ";"},
            {}
        ];
        parts = DeleteCases[{
            If[showHeader, headerXML[catalog, cfg, layoutEl], layoutEl],
            XMLElement["div", {"class" -> "pp-main"}, DeleteCases[{
                If[hasLeft, XMLElement["nav", {"class" -> "pp-side", "aria-label" -> "Guides and tutorials"}, leftItems], Nothing],
                XMLElement["main", {"class" -> "pp-content"}, {cfg["Notebook"]}],
                If[hasSymbols, XMLElement["nav", {"class" -> "pp-side-right", "aria-label" -> "Symbols"}, rightItems], Nothing]
            }, Nothing]],
            If[showFooter, footerXML[cfg], Nothing]
        }, Nothing];
        XMLElement["div", Join[
            {"class" -> shellClass},
            styleAttr,
            {
                "data-paclet-page" -> catalog["Name"],
                "data-paclet-id" -> cfg["Current"],
                "data-paclet-title" -> cfg["ActiveTitle"]
            }
        ], parts]
    ]
];
