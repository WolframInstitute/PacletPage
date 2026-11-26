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

xmlAttrHTML[k_String -> True] := " " <> k;
xmlAttrHTML[k_String -> v_String] := " " <> k <> "=\"" <> esc[v] <> "\"";
xmlAttrHTML[_] := "";

xmlToHTML[s_String] := esc[s];
xmlToHTML[Nothing] := "";
xmlToHTML[list_List] := StringJoin[xmlToHTML /@ list];
xmlToHTML[XMLElement["script", attrs_List, {code_String}]] :=
    "<script" <> StringJoin[xmlAttrHTML /@ attrs] <> ">" <> code <> "</script>";
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
cssTintHex[hex_String, frac_ : 0.92] := cssMixHex[hex, "#ffffff", frac];

(* the color frac of the way from hex to toward, both #rgb or #rrggbb: the dark theme's active
   colors mix the accent with its rail and with white *)
cssHexRGB[hex_String] := With[{digits = StringDrop[hex, 1]},
    FromDigits[#, 16] & /@ If[StringLength[digits] === 3, StringJoin[#, #] & /@ Characters[digits], StringPartition[digits, 2]]
];
cssMixHex[hex_String, toward_String, frac_] :=
    "#" <> StringJoin[IntegerString[Round[#1 * (1 - frac) + #2 * frac], 16, 2] & @@@ Transpose[{cssHexRGB[hex], cssHexRGB[toward]}]];

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

layoutButtonXML[spec_Association, mode_String] := With[{pressed = TrueQ[spec["Pressed"]]},
    XMLElement["button", Join[
        If[mode === "HTML", {"id" -> spec["ID"]}, {}],
        {
            "class" -> "pp-layout-btn" <> If[pressed, " on", ""],
            "type" -> "button",
            "data-pp-layout" -> spec["Kind"],
            "aria-label" -> spec["Label"],
            "title" -> spec["Label"],
            "aria-pressed" -> If[pressed, "true", "false"]
        }
    ], {layoutSVGXML[spec["Shapes"]]}]
];

layoutControlsXML[controls_List, hasLeft_, hasSymbols_, mode_String, standalone_ : False] := Module[{active, cls},
    active = Select[controls, Switch[#,
        "LeftSidebar", TrueQ[hasLeft],
        "RightSidebar", TrueQ[hasSymbols],
        _, True
    ] &];
    If[ active === {}, Return[Nothing, Module]];
    cls = If[mode === "Expression" && TrueQ[standalone], "pp-layout-controls pp-layout-controls-bar", "pp-layout-controls"];
    XMLElement["div", Join[
        {"class" -> cls, "role" -> "group", "aria-label" -> "Layout"},
        If[mode === "Expression", {"data-pp-layout-controls" -> StringRiffle[$layoutControlSpecs[#]["Kind"] & /@ active, ","]}, {}]
    ], layoutButtonXML[$layoutControlSpecs[#], mode] & /@ active]
];

(* The header's switch between the light theme and the dark one: a moon while the site is light, a
   sun while it is dark; the stylesheet shows the one for the theme the page has, and the shell's
   script keeps aria-pressed. None for a site with "DarkMode" -> False. *)
themeButtonXML["off"] := Nothing;
themeButtonXML[_] := XMLElement["button", {
    "id" -> "themeToggle", "class" -> "pp-theme-btn", "type" -> "button",
    "aria-label" -> "Dark mode", "title" -> "Dark mode", "aria-pressed" -> "false"
}, {
    XMLElement["svg", {"class" -> "pp-moon", "aria-hidden" -> "true", "viewBox" -> "0 0 20 20", "fill" -> "none", "stroke" -> "currentColor",
        "stroke-width" -> "1.5", "stroke-linecap" -> "round", "stroke-linejoin" -> "round"},
        {XMLElement["path", {"d" -> "M16 12.5A6.5 6.5 0 0 1 7.5 4a6.5 6.5 0 1 0 8.5 8.5z"}, {}]}],
    XMLElement["svg", {"class" -> "pp-sun", "aria-hidden" -> "true", "viewBox" -> "0 0 20 20", "fill" -> "none", "stroke" -> "currentColor",
        "stroke-width" -> "1.5", "stroke-linecap" -> "round", "stroke-linejoin" -> "round"}, {
        XMLElement["circle", {"cx" -> "10", "cy" -> "10", "r" -> "3.5"}, {}],
        XMLElement["path", {"d" -> "M10 2v2M10 16v2M2 10h2M16 10h2M4.3 4.3l1.4 1.4M14.3 14.3l1.4 1.4M4.3 15.7l1.4-1.4M14.3 5.7l1.4-1.4"}, {}]
    }]
}];

fullExitXML[controls_List, mode_String] := If[ MemberQ[controls, "Fullscreen"],
    XMLElement["button", Join[
        If[mode === "HTML", {"id" -> "ppFullExit"}, {}],
        {
            "class" -> "pp-full-exit",
            "type" -> "button",
            "data-pp-layout" -> "fullscreen-exit",
            "aria-label" -> "Exit fullscreen",
            "title" -> "Exit fullscreen (Esc)"
        }
    ], {layoutSVGXML[{XMLElement["path", {"d" -> "M8 3v5H3M17 8h-5V3M12 17v-5h5M3 12h5v5"}, {}]}]}],
    Nothing
];

filterInputXML[filter_String, mode_String] := If[ mode === "HTML",
    XMLElement["input", {
        "id" -> "q", "type" -> "search", "autocomplete" -> "off", "spellcheck" -> "false",
        "enterkeyhint" -> "go",
        "placeholder" -> "Filter guides, tutorials and symbols  (/)",
        "title" -> "Type to filter; Enter opens the best match; Esc clears",
        "aria-label" -> "Filter the navigation; Enter opens the best match"
    }, {}],
    XMLElement["div", {"class" -> "pp-filter-slot", "data-pp-filter" -> filter}, {
        XMLElement["input", Join[
            {
                "type" -> "search", "class" -> "pp-filter", "autocomplete" -> "off", "spellcheck" -> "false",
                "placeholder" -> "Filter guides, tutorials and symbols",
                "aria-label" -> "Filter guides, tutorials and symbols"
            },
            If[filter =!= "", {"value" -> filter}, {}]
        ], {}]
    }]
];

splitterXML[side_String, mode_String] := With[
    {isLeft = (side === "Left")},
    XMLElement["div", Join[
        If[mode === "HTML",
            {"class" -> "splitter", "id" -> If[isLeft, "splitL", "splitR"]},
            {"class" -> If[isLeft, "pp-split pp-split-left", "pp-split pp-split-right"]}
        ],
        {
            "role" -> "separator",
            "aria-orientation" -> "vertical",
            "tabindex" -> "0",
            "aria-label" -> If[isLeft, "Resize the guides rail", "Resize the symbols rail"],
            "title" -> "Drag to resize; double-click to reset"
        }
    ], {}]
];

resourceLinkXML[url_String, label_String, mode_String] := If[ mode === "Expression" && url === "",
    Nothing,
    XMLElement["a", Join[
        If[mode === "HTML", {"class" -> "home", "id" -> "resourcelink"}, {"class" -> "pp-home"}],
        {"href" -> url, "target" -> "_blank", "rel" -> "noopener"}
    ], {
        label,
        " ",
        XMLElement["svg", {"viewBox" -> "0 0 24 24"}, {XMLElement["path", {"d" -> "M7 17 17 7M9 7h8v8"}, {}]}]
    }]
];

(* Parse a FeedbackEmail template ("To:" and "Subject:" header lines, blank line, body) and fill
   {title}, {page}, {link}, {view}, {notebook}, {date}, matching Assets/Web/page.html. *)
parseFeedbackEmail["", _Association] := None;
parseFeedbackEmail[tpl_String, info_Association] := Module[{lines, i = 1, n, headers = <||>, body, fill, to, subject, filledBody, mailto},
    lines = StringSplit[StringReplace[tpl, {"\r\n" -> "\n", "\r" -> "\n"}], "\n", All];
    n = Length[lines];
    While[i <= n && StringTrim[lines[[i]]] === "", i++];
    While[i <= n && StringTrim[lines[[i]]] =!= "",
        Replace[
            StringCases[lines[[i]], StartOfString ~~ Whitespace ... ~~ k : (LetterCharacter | "-") .. ~~ ":" ~~ Whitespace ... ~~ v : ___ ~~ EndOfString :> {ToLowerCase[k], StringTrim[v]}, 1],
            {{{k_, v_}} :> (headers[k] = v)}
        ];
        i++
    ];
    to = StringDelete[Lookup[headers, "to", ""], Whitespace];
    If[ ! emailAddressQ[to], Return[None, Module]];
    fill[s_String] := StringReplace[s, "{" ~~ k : WordCharacter .. ~~ "}" /; KeyExistsQ[info, k] :> info[k]];
    subject = fill[Lookup[headers, "subject", ""]];
    body = If[i < n, StringRiffle[Drop[lines, i], "\n"], ""];
    filledBody = fill[body];
    mailto = "mailto:" <> to <> "?subject=" <> URLEncode[subject] <> "&body=" <> URLEncode[StringReplace[filledBody, "\n" -> "\r\n"]];
    <|"To" -> to, "Subject" -> subject, "Body" -> filledBody, "Mailto" -> mailto|>
];
parseFeedbackEmail[_, _Association] := None;

(* Page tools bar (#pagetools / .pp-pagetools): Feedback and View source / View built page,
   matching Assets/Web/page.html. *)
pageToolsXML[cfg_Association] := Module[
    {showFeedback = TrueQ[Lookup[cfg, "Feedback", True]],
        showSource = TrueQ[Lookup[cfg, "ViewSource", True]],
        fbURL = Replace[Lookup[cfg, "FeedbackURL", ""], Except[_String] -> ""],
        fbMailTpl = Replace[Lookup[cfg, "FeedbackEmailTemplate", ""], Except[_String] -> ""],
        fbThanks = Replace[Lookup[cfg, "FeedbackThanks", ""], Except[_String] -> ""],
        allFeedback = TrueQ[Lookup[cfg, "AllFeedbackLink", True]],
        view = Lookup[cfg, "View", "built"],
        viewLabel = Lookup[cfg, "ViewLabel", "built page"],
        sourceType = Replace[Lookup[cfg, "SourceType", ""], Except[_String] -> ""],
        sourceURL = Replace[Lookup[cfg, "SourceURL", ""], Except[_String] -> ""],
        info, mailInfo, fbHref, fbTitle, fbEl, srcEl, tools},
    info = <|
        "title" -> Lookup[cfg, "ActiveTitle", ""],
        "page" -> Lookup[cfg, "Current", ""],
        "link" -> "paclet:" <> cfg["Name"] <> "/" <> Lookup[cfg, "Current", ""],
        "view" -> viewLabel,
        "notebook" -> Replace[Lookup[cfg, "PageFile", ""], Except[_String] -> ""],
        "date" -> DateString[TimeZoneConvert[Now, 0], "ISODate"]
    |>;
    mailInfo = parseFeedbackEmail[fbMailTpl, info];
    fbEl = If[ showFeedback,
        fbHref = Which[
            fbURL =!= "", fbURL,
            AssociationQ[mailInfo], mailInfo["Mailto"],
            True, "#feedback"
        ];
        fbTitle = Which[
            fbURL =!= "", "Report a problem with this page or suggest an improvement",
            AssociationQ[mailInfo], "Report a problem with this page (opens an email to " <> mailInfo["To"] <> ")",
            True, "Report a problem with this page or suggest an improvement"
        ];
        XMLElement["a", DeleteCases[Join[
            {
                "class" -> "pp-pagetool",
                "data-pp-tool" -> "feedback",
                "href" -> fbHref,
                "title" -> fbTitle,
                "data-pp-feedback-paclet" -> cfg["Name"],
                "data-pp-feedback-page" -> Lookup[cfg, "Current", ""],
                "data-pp-feedback-title" -> Lookup[cfg, "ActiveTitle", ""],
                "data-pp-feedback-view" -> viewLabel
            },
            If[fbURL =!= "", {
                "data-pp-feedback-url" -> fbURL,
                If[allFeedback && StringEndsQ[fbURL, "/Form"], "data-pp-all-feedback" -> StringReplace[fbURL, RegularExpression["/Form$"] -> "/All"], Nothing]
            }, {}],
            If[AssociationQ[mailInfo], {
                "data-pp-feedback-to" -> mailInfo["To"],
                "data-pp-feedback-subject" -> mailInfo["Subject"],
                "data-pp-feedback-body" -> mailInfo["Body"]
            }, {}],
            If[fbThanks =!= "", {"data-pp-feedback-thanks" -> fbThanks}, {}],
            Which[
                StringStartsQ[fbHref, "mailto:"], {"target" -> "_top"},
                StringStartsQ[fbHref, "http://" | "https://"], {"target" -> "_blank", "rel" -> "noopener"},
                True, {}
            ]
        ], Nothing], {"Feedback"}],
        Nothing
    ];
    srcEl = If[ showSource,
        XMLElement["button", DeleteCases[Join[
            {
                "class" -> "pp-pagetool",
                "type" -> "button",
                "data-pp-tool" -> "viewtoggle",
                "data-view" -> view,
                "title" -> If[view === "source",
                    "Showing the authoring source this page is built from. Click for the built page.",
                    "Show the authoring source this page is built from."]
            },
            If[sourceType =!= "", {"data-pp-source-type" -> sourceType}, {}],
            If[sourceURL =!= "", {"data-pp-source-url" -> sourceURL}, {}]
        ], Nothing], {If[view === "source", "View built page", "View source"]}],
        Nothing
    ];
    tools = DeleteCases[{fbEl, srcEl}, Nothing];
    If[tools === {}, Nothing, XMLElement["div", {"class" -> "pp-pagetools"}, tools]]
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

(* HTML-only right rail with an A-Z symbol index for paclets with > 24 symbols, unless
   "SymbolIndex" -> False. The index starts on with "SymbolIndex" -> True and off with Automatic,
   and the button in the rail's heading switches it, remembered for the reader. Started on, the rail
   streams the first letter and up to 45 links before the guide tree and places the remaining links
   in an inert <template id="symbol-rest">; started off, it lists every link. Expression mode
   (PacletPage[...]) uses symbolsRailXML directly so no <script>/<select>/<option>/<template>/
   <noscript> tags enter Studio's $trustedPacletTags. The inline script applies the reader's choice
   before the rail paints, and keeps a letter chosen before the shell's script runs. *)
$symbolRailPreScript = StringJoin[
    "(function (rail) {",
    "if (rail.classList.contains('indexable')) {",
    "var choice = null; try { choice = localStorage.getItem('pp-symbol-index:' + location.pathname); } catch (e) {}",
    "if (choice === 'on' || choice === 'off') rail.classList.toggle('indexed', choice === 'on');",
    "}",
    "if (!rail.classList.contains('indexed')) return;",
    "function choose(value) { rail.dataset.pendingLetter = value; delete rail.dataset.extraBatches; }",
    "rail.onclick = function (event) {",
    "if (event.target.matches('.symbol-letters button')) choose(event.target.value);",
    "if (event.target.matches('.symbol-more')) rail.dataset.extraBatches = Number(rail.dataset.extraBatches || 0) + 1;",
    "};",
    "rail.onchange = function (event) { if (event.target.id === 'symbol-first-letter') choose(event.target.value); };",
    "})(document.currentScript.parentNode);"
];

htmlSymbolsRailParts[catalog_Association, cfg_Association] := Module[
    {pages = catalog["Pages"], enabled = TrueQ[Lookup[cfg, "SymbolsRail", True]],
        setting = Lookup[cfg, "SymbolIndex", Automatic],
        resource = Replace[Lookup[cfg, "Resource", ""], Except[_String] -> ""],
        ids, indexable, indexed, letters, buckets, initial, group, batch, batchSize = 45,
        positions, label, remaining, count, initialOf, rowXML, rowsXML, buttonXML, optionXML,
        symbolCountText, moreText, ulEl, remainingEl, letterButtons, letterOptions, railEl, railClass},
    ids = SortBy[Select[Keys[pages], StringStartsQ[#, "ref/"] &], symbolKey[pages[#]] &];
    indexable = enabled && setting =!= False && Length[ids] > 24;
    indexed = indexable && TrueQ[setting];
    initialOf[id_String] := Replace[ToUpperCase[StringTake[StringDrop[id, 4], UpTo[1]]], "" -> "#"];
    buckets = If[indexable, GroupBy[ids, initialOf], <||>];
    letters = Join[
        CharacterRange["A", "Z"],
        Select[Keys[buckets], ! StringMatchQ[#, RegularExpression["[A-Z]"] | "$"] &],
        If[KeyExistsQ[buckets, "$"], {"$"}, {}]
    ];
    initial = SelectFirst[letters, KeyExistsQ[buckets, #] &, ""];
    group = Lookup[buckets, initial, {}];
    batch = Which[indexed, Take[group, UpTo[batchSize]], enabled, ids, True, {}];
    positions = If[indexed, KeyTake[AssociationThread[ids -> Range[0, Length[ids] - 1]], batch], <||>];
    remaining = Which[indexed, Select[ids, ! KeyExistsQ[positions, #] &], enabled, {}, True, ids];
    label = If[indexable, "All " <> initial <> " names", ""];
    count[number_Integer] := StringReplace[IntegerString[number], RegularExpression["(?<=\\d)(?=(?:\\d{3})+$)"] -> ","];
    rowXML[id_String] := XMLElement["li", DeleteCases[{
        If[KeyExistsQ[positions, id], "data-position" -> IntegerString[positions[id]], Nothing],
        If[indexable && ! StringStartsQ[id, "ref/" ~~ RegularExpression["[a-zA-Z$]"]], "data-letter" -> initialOf[id], Nothing]
    }, Nothing], {navLinkXML[id, pages[id], "sym", cfg]}];
    rowsXML[names_List] := rowXML /@ names;
    buttonXML[letter_String] := XMLElement["button", DeleteCases[{
        "type" -> "button",
        "value" -> letter,
        "aria-label" -> "Jump to " <> letter,
        "aria-pressed" -> If[letter === initial, "true", "false"],
        If[KeyExistsQ[buckets, letter], Nothing, "disabled" -> True]
    }, Nothing], {letter}];
    optionXML[letter_String] := XMLElement["option", {"value" -> letter}, {letter}];
    symbolCountText = If[indexable, initial <> " \:00b7 " <> count[Length[group]] <> If[Length[group] === 1, " symbol", " symbols"], ""];
    moreText = If[indexable && Length[group] > batchSize, "Show " <> IntegerString[Min[batchSize, Length[group] - batchSize]] <> " more", ""];
    ulEl = XMLElement["ul", {"class" -> "nav-symbols"}, rowsXML[batch]];
    remainingEl = If[remaining === {}, Nothing, XMLElement["template", {"id" -> "symbol-rest"}, rowsXML[remaining]]];
    letterButtons = If[indexable, buttonXML /@ letters, {}];
    letterOptions = optionXML /@ Select[letters, KeyExistsQ[buckets, #] &];
    railClass = StringRiffle[Join[If[indexable, {"indexable"}, {}], If[indexed, {"indexed"}, {}]], " "];
    railEl = XMLElement["nav", DeleteCases[{
        "id" -> "side-right",
        If[railClass =!= "", "class" -> railClass, Nothing],
        "data-batch-size" -> IntegerString[batchSize],
        "aria-label" -> "Symbols"
    }, Nothing], {
        XMLElement["script", {}, {$symbolRailPreScript}],
        XMLElement["div", DeleteCases[{"class" -> "symbol-index", If[indexable, Nothing, "hidden" -> True]}, Nothing], {
            XMLElement["div", {"class" -> "symbol-heading"}, {
                XMLElement["h2", {}, {"Symbols"}],
                XMLElement["span", {"class" -> "symbol-total"}, {count[Length[ids]]}],
                XMLElement["button", {
                    "type" -> "button", "class" -> "symbol-index-toggle",
                    "aria-pressed" -> If[indexed, "true", "false"],
                    "aria-label" -> "Index by first letter", "title" -> "Index by first letter"
                }, {"A\:2013Z"}]
            }],
            XMLElement["div", {"class" -> "symbol-browse"}, {
                XMLElement["div", {"class" -> "symbol-letters", "role" -> "group", "aria-label" -> "First letter"}, letterButtons],
                XMLElement["div", {"class" -> "symbol-letter-picker"}, {
                    XMLElement["label", {"for" -> "symbol-first-letter"}, {"First letter"}],
                    XMLElement["div", {"class" -> "symbol-select"}, {
                        XMLElement["select", {"id" -> "symbol-first-letter"}, letterOptions]
                    }]
                }],
                XMLElement["div", DeleteCases[{"class" -> "symbol-prefix", If[indexed && Length[group] > batchSize, Nothing, "hidden" -> True]}, Nothing], {
                    XMLElement["label", {"for" -> "symbol-prefix"}, {"Symbol group"}],
                    XMLElement["div", {"class" -> "symbol-select"}, {
                        XMLElement["select", {"id" -> "symbol-prefix"}, {
                            XMLElement["option", {"value" -> ""}, {label}]
                        }],
                        XMLElement["span", {"class" -> "symbol-selection", "aria-hidden" -> "true"}, {label}]
                    }]
                }]
            }],
            XMLElement["div", {"class" -> "symbol-count", "role" -> "status"}, If[symbolCountText === "", {}, {symbolCountText}]],
            XMLElement["noscript", {}, {XMLElement["a", {"href" -> resource}, {"Open the documentation"}]}]
        }],
        XMLElement["div", {"class" -> "symbol-scroll"}, {
            ulEl,
            XMLElement["button", DeleteCases[{
                "class" -> "symbol-more",
                "type" -> "button",
                If[indexed && Length[group] > batchSize, Nothing, "hidden" -> True]
            }, Nothing], If[moreText === "", {}, {moreText}]]
        }]
    }];
    <|
        "Rail" -> railEl,
        "Remaining" -> remainingEl,
        "Slots" -> <|
            "railClass" -> railClass,
            "batchSize" -> IntegerString[batchSize],
            "indexHidden" -> If[indexable, "", "hidden"],
            "symbolTotal" -> count[Length[ids]],
            "letterButtons" -> xmlToHTML[letterButtons],
            "letterOptions" -> xmlToHTML[letterOptions],
            "prefixHidden" -> If[indexed && Length[group] > batchSize, "", "hidden"],
            "initialOption" -> "<option value=\"\">" <> esc[label] <> "</option>",
            "initialGroup" -> esc[label],
            "symbolCount" -> esc[symbolCountText],
            "symbols" -> xmlToHTML[ulEl],
            "moreHidden" -> If[indexed && Length[group] > batchSize, "", "hidden"],
            "moreText" -> moreText,
            "remainingSymbols" -> xmlToHTML[remainingEl]
        |>
    |>
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
            XMLElement["button", {"id" -> "navToggle", "type" -> "button", "aria-label" -> "Guides and tutorials", "aria-controls" -> "side", "aria-expanded" -> "false"}, {"\:2630"}],
            XMLElement["a", {"class" -> "brand", "id" -> "homelink", "href" -> "#"}, brandKids],
            filterInputXML["", "HTML"],
            XMLElement["button", {"id" -> "symToggle", "type" -> "button", "aria-label" -> "Symbols", "aria-controls" -> "side-right", "aria-expanded" -> "false"}, {"\:22ee"}],
            layoutEl,
            themeButtonXML[Lookup[cfg, "Theme", "off"]],
            resourceLinkXML[Lookup[cfg, "Resource", ""], Lookup[cfg, "ResourceLabel", "Resource page"], "HTML"]
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
                filterInputXML[Replace[Lookup[cfg, "Filter", ""], Except[_String] -> ""], "Expression"],
                Nothing
            ],
            layoutEl,
            XMLElement["span", {"class" -> "pp-meta"}, {summary}],
            resourceLinkXML[Replace[Lookup[cfg, "Resource", ""], Except[_String] -> ""], Lookup[cfg, "ResourceLabel", "Resource page"], "Expression"]
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
    rightItems = If[mode === "Expression" && showSymbols, symbolsRailXML[catalog, cfg], {}];
    hasLeft = If[mode === "HTML", True, leftItems =!= {}];
    hasSymbols = If[mode === "HTML", showSymbols, rightItems =!= {}];
    layoutEl = layoutControlsXML[controls, hasLeft, hasSymbols, mode, mode === "Expression" && ! showHeader];
    exitEl = fullExitXML[controls, mode];
    If[ mode === "HTML",
        With[{symParts = htmlSymbolsRailParts[catalog, cfg]},
            DeleteCases[{
                headerXML[catalog, cfg, layoutEl],
                XMLElement["div", {"id" -> "main"}, {
                    XMLElement["div", {"id" -> "scrim"}, {}],
                    symParts["Rail"],
                    XMLElement["nav", {"id" -> "side", "aria-label" -> "Guides and tutorials"}, leftItems],
                    splitterXML["Left", "HTML"],
                    XMLElement["div", {"id" -> "content"}, {
                        XMLElement["div", {"id" -> "loading"}, {"Loading..."}],
                        XMLElement["iframe", {"id" -> "frame", "title" -> "documentation"}, {}]
                    }],
                    splitterXML["Right", "HTML"]
                }],
                exitEl,
                If[showFooter, footerXML[cfg], Nothing],
                symParts["Remaining"]
            }, Nothing]
        ],
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
                If[hasLeft, splitterXML["Left", "Expression"], Nothing],
                XMLElement["main", {"class" -> "pp-content"}, DeleteCases[{pageToolsXML[cfg], cfg["Notebook"]}, Nothing]],
                If[hasSymbols, splitterXML["Right", "Expression"], Nothing],
                If[hasSymbols, XMLElement["nav", {"class" -> "pp-side-right", "aria-label" -> "Symbols"}, rightItems], Nothing]
            }, Nothing]],
            exitEl,
            If[showFooter, footerXML[cfg], Nothing]
        }, Nothing];
        XMLElement["div", Join[
            {"class" -> shellClass},
            styleAttr,
            {
                "data-paclet-page" -> catalog["Name"],
                "data-paclet-id" -> cfg["Current"],
                "data-paclet-title" -> cfg["ActiveTitle"],
                "data-paclet-view" -> Lookup[cfg, "View", "built"]
            }
        ], parts]
    ]
];
