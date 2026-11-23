(* ::Package:: *)

(* ==================== a paclet's documentation as a WL XML expression ====================

   PacletPage gives a documentation page of a local or installed paclet as the unified
   three-column PacletPage XMLElement shell (built by Shell.wl's shellXML) holding the page's live
   Notebook[...] in the center frame. Nothing is read from the cloud, built or rendered, and no
   front end is asked, so any Wolfram kernel computes it. *)

Options[PacletPage] = {
    "Filter" -> "",
    "StripTitleNumbers" -> True,
    "SymbolsRail" -> True,
    "Header" -> True,
    "Footer" -> True,
    "Title" -> Automatic,
    "Subtitle" -> Automatic,
    "Accent" -> Automatic,
    "VersionLabel" -> Automatic,
    "RootGuide" -> Automatic,
    "LayoutControls" -> False
};

$pageCatalogCache = <||>;

PacletPage[spec_, opts : OptionsPattern[]] := Replace[spec, {
    uri_String /; StringStartsQ[uri, "paclet:"] :> pageFromURI[uri, opts],
    other_ :> PacletPage[other, Automatic, opts]
}];

PacletPage[spec_, page : (_String | Automatic), opts : OptionsPattern[]] := Module[
    {controls, accentOpt, accent, dir, catalog, pages, rootOpt, showRootGuide, homeID, id, file, nb,
        filterOpt, showFilter, filter, strip, title, subtitle, version, versionTag, desc, activeTitle},
    controls = normalizeLayoutControls[OptionValue["LayoutControls"]];
    If[ controls === $Failed,
        Return[Failure["InvalidOption", <|
            "MessageTemplate" -> "`1` is not a LayoutControls setting; use False, True, All, or a list of \"LeftSidebar\", \"RightSidebar\", and \"Fullscreen\".",
            "MessageParameters" -> {OptionValue["LayoutControls"]},
            "Option" -> "LayoutControls",
            "Value" -> OptionValue["LayoutControls"]
        |>], Module]
    ];
    accentOpt = OptionValue["Accent"];
    accent = If[accentOpt === Automatic, Automatic, cssColor[accentOpt]];
    If[ accent === $Failed,
        Return[Failure["InvalidOption", <|
            "MessageTemplate" -> "`1` is not a color; give Automatic, a color, or a hex string such as \"#dd1100\".",
            "MessageParameters" -> {accentOpt},
            "Option" -> "Accent",
            "Value" -> accentOpt
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
    showRootGuide = (rootOpt =!= False);
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
    filterOpt = OptionValue["Filter"];
    showFilter = ! MatchQ[filterOpt, False | None];
    filter = If[StringQ[filterOpt], filterOpt, ""];
    strip = TrueQ[OptionValue["StripTitleNumbers"]];
    title = Replace[OptionValue["Title"], Automatic -> catalog["Title"]];
    subtitle = Replace[OptionValue["Subtitle"], {
        Automatic :> pageSubtitle[catalog["Name"]],
        None | "" -> ""
    }];
    version = Replace[Lookup[catalog, "Version", ""], Except[_String] -> ""];
    versionTag = Replace[OptionValue["VersionLabel"], {
        Automatic | True :> If[version =!= "", version, None],
        label_String /; label =!= "" :> label,
        _ -> None
    }];
    desc = Replace[Lookup[catalog, "Description", ""], Except[_String] -> ""];
    activeTitle = If[strip, shownPage[pages[id]]["Title"], pages[id]["Title"]];
    shellXML[catalog, <|
        "Mode" -> "Expression",
        "Name" -> catalog["Name"],
        "Prefix" -> "pp-",
        "Title" -> title,
        "Subtitle" -> subtitle,
        "Accent" -> accent,
        "Version" -> version,
        "VersionTag" -> versionTag,
        "Description" -> desc,
        "HomeID" -> homeID,
        "BrandHomeID" -> If[StringQ[homeID], homeID, id],
        "ShowRootGuide" -> showRootGuide,
        "StripTitleNumbers" -> strip,
        "SymbolsRail" -> TrueQ[OptionValue["SymbolsRail"]],
        "Header" -> TrueQ[OptionValue["Header"]],
        "Footer" -> TrueQ[OptionValue["Footer"]],
        "ShowFilter" -> showFilter,
        "Filter" -> filter,
        "Current" -> id,
        "ActiveTitle" -> activeTitle,
        "Notebook" -> nb,
        "LayoutControls" -> controls,
        "GuidesHeading" -> True
    |>]
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
