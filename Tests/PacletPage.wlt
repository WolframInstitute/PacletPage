(* :Title: PacletPage.wlt - PacletPage test suite *)
(* :Context: WolframInstitute`PacletPage` *)
(* :Summary:
    Offline tests: nothing here reads the network or the cloud. The catalog's parsing runs on
    fixture pages and notebooks, the site's pages on fixture catalogs, and each feedback
    endpoint through the function that decides its answer. Run via run-tests.wls.
    Each test has a TestID with the shape "Area: behavior".
*)

Begin["WolframInstitute`PacletPage`Private`"]


(* === fixtures === *)

fixturePage[kind_, name_, title_, notebook_, guides_ : {}, keywords_ : {}] := <|
    "Kind" -> kind, "Name" -> name, "Title" -> title, "Keywords" -> keywords, "Guides" -> guides,
    "HTML" -> "https://r.example/D/" <> kind <> "/" <> name <> ".html", "Notebook" -> notebook
|>;

repositoryCatalog = <|
    "Name" -> "Pub/Name", "Title" -> "Fixture Caf\[EAcute]", "Kind" -> "Repository",
    "Resource" -> "https://r.example/D/", "DocBase" -> "https://r.example/D/", "RootGuide" -> "guide/Main",
    "Pages" -> <|
        "guide/Main" -> fixturePage["guide", "Main", "Main Guide", "https://s.example/obj/store/N/aaa.nb", {"guide/A", "guide/B"}],
        "guide/A" -> fixturePage["guide", "A", "Area A", "https://s.example/obj/store/N/bbb.nb", {"guide/B"}],
        "guide/B" -> fixturePage["guide", "B", "Area B", "https://s.example/obj/store/N/ccc.nb", {"guide/A"}],
        "guide/Lone" -> fixturePage["guide", "Lone", "Lone Guide", "https://s.example/obj/store/N/ddd.nb"],
        "ref/Foo" -> fixturePage["ref", "Foo", "Foo", "https://s.example/obj/store/N/eee.nb", {}, {"alpha", "Beta Words"}],
        "ref/$Bar" -> fixturePage["ref", "$Bar", "$Bar", "https://s.example/obj/store/N/fff.nb"],
        "ref/apple" -> fixturePage["ref", "apple", "apple", "https://s.example/obj/store/N/ggg.nb"],
        "ref/LongCamelCaseName" -> fixturePage["ref", "LongCamelCaseName", "LongCamelCaseName", "https://s.example/obj/store/N/jjj.nb"],
        "tutorial/Intro" -> fixturePage["tutorial", "Intro", "Intro </script> <b>", "https://s.example/obj/store/N/hhh.nb"],
        "tutorial/Use" -> fixturePage["tutorial", "Use", "Worked Use", "https://s.example/obj/store/N/iii.nb", {}, {"Application"}]
    |>
|>;

cloudCatalog = <|
    "Name" -> "Pub/Name", "Title" -> "Cloud Fixture", "Kind" -> "Cloud",
    "Resource" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/",
    "DocBase" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/", "RootGuide" -> "guide/Name",
    "Pages" -> <|
        "resource/Name" -> <|"Kind" -> "resource", "Name" -> "Name", "Title" -> "Pub/Name", "Keywords" -> {}, "Guides" -> {},
            "HTML" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/",
            "Notebook" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Name-ResourceDefinition.nb"|>,
        "guide/Name" -> fixturePage["guide", "Name", "Name Guide", "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/guide/Name.nb"],
        "ref/$Var" -> fixturePage["ref", "$Var", "$Var", "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/ref/$Var.nb"]
    |>
|>;

configOf[html_String] := ImportString[
    First[StringCases[html, "<script type=\"application/json\" id=\"pp-config\">" ~~ json : Shortest[___] ~~ "</script>" :> json]],
    "RawJSON"
];

asciiQ[s_String] := StringMatchQ[s, RegularExpression["[\\x00-\\x7F]*"]];

settings = <|
    "Title" -> "T", "Site" -> "https://c.example/obj/u/Site", "Databin" -> "bin123",
    "SetStatus" -> "https://c.example/obj/u/SiteFeedback/SetStatus", "Status" -> "https://c.example/obj/u/SiteFeedback/status",
    "Attachments" -> "https://c.example/obj/u/SiteFeedback/attachments",
    "Owner" -> "owner@example.org", "Readers" -> {"reader@other.org"}, "Domains" -> {"team.example"},
    "Pages" -> {"https://c.example/obj/u/Site", "https://c.example/obj/u/SitePage"}
|>;


(* === the published resource === *)

VerificationTest[
    resourceSpec["WolframInstitute/TuringMachine"],
    <|"Kind" -> "Repository", "Name" -> "WolframInstitute/TuringMachine",
      "Resource" -> "https://resources.wolframcloud.com/PacletRepository/resources/WolframInstitute/TuringMachine/",
      "DocBase" -> "https://resources.wolframcloud.com/PacletRepository/resources/WolframInstitute/TuringMachine/"|>,
    TestID -> "Catalog: Publisher/Name names a Paclet Repository listing"
]

VerificationTest[
    resourceSpec["https://resources.wolframcloud.com/PacletRepository/resources/Pub/Name/ref/Foo.html"]["Name"],
    "Pub/Name",
    TestID -> "Catalog: a Repository page URL names its paclet"
]

VerificationTest[
    KeyTake[resourceSpec[CloudObject["https://www.wolframcloud.com/obj/acct/DeployedResources/Paclet/Pub/Name/"]], {"Kind", "Name", "DocBase"}],
    <|"Kind" -> "Cloud", "Name" -> "Pub/Name", "DocBase" -> "https://www.wolframcloud.com/obj/acct/DeployedResources/Paclet/Pub/Name/Documentation/"|>,
    TestID -> "Catalog: a deployed resource's CloudObject names its documentation"
]

VerificationTest[
    resourceSpec /@ {"NoSlash", "a/b/c", "https://example.org/x", 42},
    {$Failed, $Failed, $Failed, $Failed},
    TestID -> "Catalog: anything else names no resource"
]

VerificationTest[
    docLinks[
        StringJoin[
            "<a href=\"https://r.example/D/guide/Main.html\">Main <b>Guide</b></a>",
            "<a class=\"x\" href=\"https://r.example/D/ref/Foo.html\">Foo</a>",
            "<a href=\"https://r.example/D/ref/%24Bar.html\">$Bar</a>",
            "<a href=\"https://r.example/D/ref/Foo.html\">Foo again</a>",
            "<a href=\"https://r.example/D/tutorial/Intro.html\">Intro &amp; More</a>",
            "<a href=\"https://other.example/ref/Plus.html\">Plus</a>",
            "<a href=\"https://r.example/D/ref/Foo.nb\">notebook</a>",
            "<a href=\"https://r.example/D/ref/format/TTE.html\">TTE</a>"
        ],
        "https://r.example/D/"
    ],
    {{"guide", "Main", "Main Guide"}, {"ref", "Foo", "Foo"}, {"ref", "$Bar", "$Bar"}, {"tutorial", "Intro", "Intro & More"}, {"ref", "format/TTE", "TTE"}},
    TestID -> "Catalog: the documentation links of a page, once each, $ names decoded, a format page's of more parts"
]

VerificationTest[
    repositoryNotebook["<div id=\"notebook-embed\" data-nb-uri=\"https://s.example/obj/a/b.nb?x=1&amp;y=2\"></div>"],
    "https://s.example/obj/a/b.nb?x=1&y=2",
    TestID -> "Catalog: a Repository page names its notebook"
]

VerificationTest[
    repositoryNotebook["<div id=\"notebook-embed\"></div>"],
    Missing["NoNotebook"],
    TestID -> "Catalog: a page without a notebook names none"
]

VerificationTest[
    {uriPageID["P/Q", "paclet:P/Q/ref/Foo#anchor"], uriPageID["P/Q", "paclet:P/Q/tutorial/$Var"], uriPageID["P/Q", "paclet:ref/Plus"], uriPageID["P/Q", "paclet:P/QR/ref/Foo"],
        uriPageID["P/Q", "paclet:P/Q/ref/format/TTE"], uriPageID["P/Q", "paclet:P/Q/ref/message/Foo/bad#x"]},
    {"ref/Foo", "tutorial/$Var", "ref/format/TTE", "ref/message/Foo/bad"},
    TestID -> "Catalog: paclet: URIs of the paclet's own pages"
]

VerificationTest[
    guideStructure["P/Q", Notebook[{
        Cell["Main Guide", "GuideTitle"],
        Cell[TextData[{ButtonBox["Sub One", BaseStyle -> "Link", ButtonData -> "paclet:P/Q/guide/SubOne"]}], "GuideFunctionsSection"],
        Cell[TextData[{Cell[BoxData[TemplateBox[{Cell[TextData["Sub Two"]], "paclet:P/Q/guide/SubTwo"}, "RefLinkPlain"]]]}], "GuideFunctionsSubsection"],
        Cell[TextData[{ButtonBox["Foo", BaseStyle -> "Link", ButtonData -> "paclet:P/Q/ref/Foo"]}], "InlineGuideFunction"],
        Cell[TextData[{ButtonBox["Plus", BaseStyle -> "Link", ButtonData -> "paclet:ref/Plus"]}], "GuideText"],
        Cell[TextData[{ButtonBox["Tut", BaseStyle -> "Link", ButtonData -> "paclet:P/Q/tutorial/Tut#anchor"]}], "GuideTutorial"]
    }]],
    <|"Children" -> {"guide/SubOne", "guide/SubTwo"}, "Links" -> {"guide/SubOne", "guide/SubTwo", "ref/Foo", "tutorial/Tut"}, "Title" -> "Main Guide"|>,
    TestID -> "Catalog: a guide's children are the guides its headings link, its links every page"
]

VerificationTest[
    notebookMetadata[Notebook[{}, TaggingRules -> {"Metadata" -> {"title" -> "A Title", "keywords" -> {"k1", "k2"}, "synonyms" -> {"s1"}}}]],
    <|"Stub" -> False, "Overview" -> False, "Title" -> "A Title", "Keywords" -> {"k1", "k2", "s1"}|>,
    TestID -> "Catalog: a built notebook's title and keywords"
]

VerificationTest[
    {
        withMetadata[<|"Name" -> "UsingIt", "Title" -> "Using It Well", "Keywords" -> {}|>, <|"Title" -> "UsingIt", "Keywords" -> {"k"}|>],
        withMetadata[<|"Name" -> "Foo", "Title" -> "Foo", "Keywords" -> {}|>, <|"Title" -> "The Foo Function", "Keywords" -> {}|>]
    },
    {
        <|"Name" -> "UsingIt", "Title" -> "Using It Well", "Keywords" -> {"k"}|>,
        <|"Name" -> "Foo", "Title" -> "The Foo Function", "Keywords" -> {}|>
    },
    TestID -> "Catalog: a published title wins over metadata that only names the page"
]

VerificationTest[
    notebookMetadata[Notebook[{}]],
    <|"Stub" -> False, "Overview" -> False, "Title" -> Missing["NoTitle"], "Keywords" -> {}|>,
    TestID -> "Catalog: a notebook without metadata gives none"
]

VerificationTest[
    Module[{dir = CreateDirectory[], ids},
        Scan[
            (CreateDirectory[DirectoryName[#], CreateIntermediateDirectories -> True]; Put[Notebook[{}], #]) &,
            FileNameJoin[Prepend[#, dir]] & /@ {
                {"Documentation", "English", "Guides", "G.nb"},
                {"Documentation", "English", "ReferencePages", "Symbols", "$S.nb"},
                {"Documentation", "English", "Tutorials", "T.nb"},
                {"Documentation", "English", "ReferencePages", "Formats", "TTE.nb"},
                {"Documentation", "English", "ReferencePages", "Services", "Svc.nb"}
            }
        ];
        (* a message's page is named by its tag alone; its URI names its symbol too *)
        CreateDirectory[FileNameJoin[{dir, "Documentation", "English", "ReferencePages", "Messages"}]];
        Put[Notebook[{}, TaggingRules -> {"Metadata" -> {"uri" -> "Pub/Name/ref/message/Foo/bad"}}],
            FileNameJoin[{dir, "Documentation", "English", "ReferencePages", "Messages", "bad.nb"}]];
        ids = Sort[Keys[localDocumentation[dir]]];
        DeleteDirectory[dir, DeleteContents -> True];
        ids
    ],
    {"guide/G", "ref/format/TTE", "ref/message/Foo/bad", "ref/service/Svc", "ref/$S", "tutorial/T"},
    TestID -> "Catalog: the pages of a built paclet's documentation, the formats, services and messages among them"
]


(* === text for pages === *)

VerificationTest[
    esc["<a href=\"x\">caf\[EAcute] & 'q'</a>"],
    "&lt;a href=&quot;x&quot;&gt;caf&#233; &amp; &#39;q&#39;&lt;/a&gt;",
    TestID -> "Site: HTML text is escaped and ASCII"
]

VerificationTest[
    jsonString["</script><!-- & caf\[EAcute] \:2028"],
    "\"\\u003c/script\\u003e\\u003c!-- \\u0026 caf\\u00e9 \\u2028\"",
    TestID -> "Site: a JSON string is ASCII and cannot end its script element"
]

VerificationTest[
    ImportString[toJSON[<|"a" -> {1, True, Null, "x\"y\\z\n"}, "b" -> <|"c" -> "\:0394 \|01f600"|>|>], "RawJSON"],
    <|"a" -> {1, True, Null, "x\"y\\z\n"}, "b" -> <|"c" -> "\:0394 \|01f600"|>|>,
    TestID -> "Site: JSON round trip, astral characters as surrogate pairs"
]

VerificationTest[
    fillTemplate["<p>`a` `b` `missing`</p>", <|"a" -> "`b`", "b" -> "B"|>],
    "<p>`b` B `missing`</p>",
    TestID -> "Site: a template's slots are filled once, unknown slots left"
]

VerificationTest[
    {cssColor["#dd1100"], cssColor[RGBColor[1, 0, 0]], cssColor["red; background: url(x)"]},
    {"#dd1100", "#ff0000", $Failed},
    TestID -> "Site: colors as CSS, anything else refused"
]

VerificationTest[
    publisherLabel /@ {"WolframInstitute/X", "Wolfram/X", "ABCLabs/X"},
    {"Wolfram Institute", "Wolfram", "ABCLabs"},
    TestID -> "Site: the publisher's name as a subtitle"
]

VerificationTest[
    pagePath /@ {"ref/$Var", "ref/a b", "guide/Main"},
    {"ref/$Var", "ref/a%20b", "guide/Main"},
    TestID -> "Site: a page's published path keeps $"
]


(* === the navigation === *)

VerificationTest[
    guideTree[repositoryCatalog["Pages"], "guide/Main", {}],
    <|"ID" -> "guide/Main", "Children" -> {
        <|"ID" -> "guide/A", "Children" -> {<|"ID" -> "guide/B", "Children" -> {}|>}|>,
        <|"ID" -> "guide/B", "Children" -> {<|"ID" -> "guide/A", "Children" -> {}|>}|>
    }|>,
    TestID -> "Site: a guide sits under every guide that links it; cycles end on the path"
]

VerificationTest[
    StringCases[sidebarHTML[repositoryCatalog, <||>], "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id],
    {"guide/Main", "guide/A", "guide/B", "guide/B", "guide/A", "guide/Lone", "tutorial/Intro", "tutorial/Use"},
    TestID -> "Site: the left rail: the root, its tree, unreached guides, tutorials, applications"
]

VerificationTest[
    StringContainsQ[sidebarHTML[repositoryCatalog, <||>], "<div class=\"nav-label\">Applications</div><ul class=\"nav-tutorials\"><li><a class=\"app\" data-id=\"tutorial/Use\""],
    True,
    TestID -> "Site: a tutorial with the keyword application is an application"
]

VerificationTest[
    StringCases[symbolRailSlots[repositoryCatalog, <||>, True]["symbols"], "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id],
    {"ref/apple", "ref/Foo", "ref/LongCamelCaseName", "ref/$Bar"},
    TestID -> "Site: symbols alphabetically, ignoring case, $ names last"
]

VerificationTest[
    StringContainsQ[symbolRailSlots[repositoryCatalog, <||>, True]["symbols"], ">Long<wbr>Camel<wbr>Case<wbr>Name</a>"],
    True,
    TestID -> "Site: a long name may break at its CamelCase steps"
]

VerificationTest[
    Module[{names, catalog, slots, ids},
        names = Join[{"0Before", "$Last", "\[Beta]Symbol"}, "Apple" <> IntegerString[#, 10, 2] & /@ Range[47]];
        catalog = <|"Pages" -> AssociationMap[Function[id, With[{name = StringDrop[id, 4]},
            fixturePage["ref", name, name, id <> ".nb"]]], "ref/" <> # & /@ names]|>;
        slots = symbolRailSlots[catalog, <||>, True, True];
        ids = StringCases[slots["symbols"] <> slots["remainingSymbols"], "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id];
        {
            slots["symbolCount"], slots["moreText"], slots["prefixHidden"], slots["initialGroup"],
            StringCount[slots["symbols"], "data-id="], DuplicateFreeQ[ids], Sort[ids] === Sort[esc /@ Keys[catalog["Pages"]]],
            StringContainsQ[slots["letterOptions"], "<option value=\"$\">$</option>"]
        }
    ],
    {"A &#183; 47 symbols", "Show 2 more", "", "All A names", 45, True, True, True},
    TestID -> "Site: the first batch, count and letter picker precede the remaining symbol links; every id appears once"
]

VerificationTest[
    Module[{names, pages, catalog, html, expr, forbidden},
        names = Join[{"$Last", "Zebra"}, "Apple" <> IntegerString[#, 10, 2] & /@ Range[30]];
        pages = AssociationMap[Function[id, With[{name = StringDrop[id, 4]},
            fixturePage["ref", name, name, id <> ".nb"]]], "ref/" <> # & /@ names];
        catalog = <|"Name" -> "Pub/Big", "Title" -> "Big", "Kind" -> "Local",
            "Resource" -> "", "DocBase" -> "Documentation/", "RootGuide" -> None, "Pages" -> pages|>;
        html = PacletPageFiles[catalog, "LayoutControls" -> True, "SymbolIndex" -> True]["index.html"];
        expr = shellXML[catalog, <|
            "Mode" -> "Expression", "Name" -> "Pub/Big", "Prefix" -> "pp-", "Title" -> "Big",
            "Subtitle" -> "", "Resource" -> "", "HomeID" -> None, "ShowRootGuide" -> True,
            "StripTitleNumbers" -> True, "SymbolsRail" -> True, "Header" -> True, "Footer" -> True,
            "ShowFilter" -> True, "Filter" -> "", "LayoutControls" -> {"LeftSidebar", "RightSidebar", "Fullscreen"},
            "Current" -> "ref/Apple01", "ActiveTitle" -> "Apple01", "Notebook" -> Notebook[{}]
        |>];
        forbidden = Cases[expr, XMLElement["script" | "select" | "option" | "template" | "noscript", _, _], Infinity];
        {
            StringContainsQ[html, "id=\"side-right\" class=\"indexable indexed\""],
            StringContainsQ[html, "<template id=\"symbol-rest\">"],
            StringContainsQ[html, "id=\"toggleSideR\""],
            StringContainsQ[html, "id=\"splitR\""],
            StringContainsQ[html, "id=\"ppFullExit\""],
            forbidden === {}
        }
    ],
    {True, True, True, True, True, True},
    TestID -> "Shell: A-Z symbol index is HTML-only alongside LayoutControls and keeps Expression mode free of script/select/template tags"
]

VerificationTest[
    StringContainsQ[sidebarHTML[repositoryCatalog, <||>], ">Intro &lt;/script&gt; &lt;b&gt;</a>"],
    True,
    TestID -> "Site: titles are escaped in the rails"
]

VerificationTest[
    {notebookPrefix[repositoryCatalog], notebookNames[repositoryCatalog, notebookPrefix[repositoryCatalog]]["ref/$Bar"]},
    {"https://s.example/obj/store/N/", "fff.nb"},
    TestID -> "Site: Repository notebooks by their storage names under one prefix"
]

VerificationTest[
    {notebookPrefix[cloudCatalog], notebookNames[cloudCatalog, cloudCatalog["DocBase"]]},
    {"https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/", <||>},
    TestID -> "Site: a deployed resource's notebooks are named by their ids"
]


(* === the site's pages === *)

VerificationTest[
    With[{files = PacletPageFiles[repositoryCatalog, "SiteURL" -> "https://c.example/obj/u/Site", "FeedbackEmail" -> "team@example.org"]},
        {
            Keys[files],
            AllTrue[Values[files], asciiQ],
            StringCases[StringJoin[Values[files]], "`" ~~ WordCharacter .. ~~ "`"],
            configOf[files["index.html"]]["home"],
            configOf[files["page.html"]]["notebooks"],
            StringTake[configOf[files["page.html"]]["mail"], 22],
            StringCount[files["index.html"], "data-nb="]
        }
    ],
    {
        {"index.html", "page.html"},
        True,
        {},
        <|"type" -> "page", "id" -> "guide/Main", "url" -> "https://r.example/D/"|>,
        "https://s.example/obj/store/N/",
        "To: team@example.org\nS",
        (* ten pages, with guides A and B twice each in the tree *)
        12
    },
    TestID -> "Site: a Repository site: ASCII pages, filled slots, root guide as home, storage names"
]

VerificationTest[
    With[{files = PacletPageFiles[cloudCatalog]},
        {
            configOf[files["index.html"]]["home"],
            KeyTake[configOf[files["page.html"]], {"kind", "definition", "resource"}],
            StringCount[files["index.html"], "data-nb="],
            StringCases[files["index.html"], "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id]
        }
    ],
    {
        <|"type" -> "page", "id" -> "resource/Name", "url" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/"|>,
        <|"kind" -> "cloud", "definition" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Name-ResourceDefinition.nb",
          "resource" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/"|>,
        0,
        {"ref/$Var", "resource/Name", "guide/Name"}
    },
    TestID -> "Site: a deployed resource's site opens on its definition notebook, first in the rail; ids name the notebooks"
]

VerificationTest[
    configOf[PacletPageFiles[cloudCatalog, "Home" -> "Resource"]["index.html"]]["home"]["type"],
    "resource",
    TestID -> "Site: the resource page itself as home, on request"
]

(* the cloud shows a paclet: link (a guide's sub-guide heading keeps one) as a reference.wolframcloud.com
   URL, which routes in the site as the paclet's other links do *)
VerificationTest[
    configOf[PacletPageFiles[cloudCatalog]["index.html"]]["routes"],
    {"https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/", "https://reference.wolfram.com/language/Pub/Name/",
     "https://reference.wolframcloud.com/cloudplatform/Pub/Name/", "paclet:Pub/Name/"},
    TestID -> "Site: links route in the site from its documentation, reference.wolfram.com's, the cloud's and paclet: URIs"
]

VerificationTest[
    With[{files = PacletPageFiles[cloudCatalog, "DefinitionPage" -> False]},
        {
            configOf[files["index.html"]]["home"]["id"],
            configOf[files["page.html"]]["definition"],
            StringCases[files["index.html"], "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id]
        }
    ],
    {"guide/Name", "", {"ref/$Var", "guide/Name"}},
    TestID -> "Site: \"DefinitionPage\" -> False leaves the definition notebook out of the site"
]

VerificationTest[
    {
        KeyTake[configOf[PacletPageFiles[cloudCatalog, "SourceURL" -> "https://c.example/obj/u/SiteSource", "ViewSource" -> False]["page.html"]], {"source", "viewSource"}],
        KeyTake[configOf[PacletPageFiles[cloudCatalog, "SourceURL" -> "https://c.example/obj/u/SiteSource"]["page.html"]], {"source", "viewSource"}]
    },
    {<|"source" -> "", "viewSource" -> False|>, <|"source" -> "https://c.example/obj/u/SiteSource", "viewSource" -> True|>},
    TestID -> "Site: \"ViewSource\" -> False gives no page a toggle to its source"
]

VerificationTest[
    With[{bodyClass = Function[html, StringCases[html, "<body class=\"" ~~ c : Except["\""] ... ~~ "\"" :> c]]},
        {
            With[{files = PacletPageFiles[cloudCatalog, "Filter" -> False, "SymbolsRail" -> False, "ResourceLink" -> False, "AllFeedbackLink" -> False]},
                {bodyClass[files["index.html"]], configOf[files["index.html"]]["filter"], configOf[files["page.html"]]["allFeedback"],
                    StringContainsQ[files["index.html"], "data-id=\"ref/$Var\""]}],
            With[{files = PacletPageFiles[cloudCatalog]},
                {bodyClass[files["index.html"]], configOf[files["index.html"]]["filter"], configOf[files["page.html"]]["allFeedback"]}]
        }
    ],
    {{{"no-filter no-symbols no-resource-link"}, False, False, True}, {{""}, True, True}},
    TestID -> "Site: switches leave out the filter, the symbols rail, the resource link and the All feedback link; the symbols stay pages"
]

VerificationTest[
    With[{brand = Function[html, StringCases[html, "<span class=\"ver\">" ~~ v : Except["<"] ... ~~ "</span>" :> v]],
        footer = Function[html, StringCases[html, "<footer id=\"ftr\">" ~~ f : Shortest[___] ~~ "</footer>" :> f]]},
        {
            brand[PacletPageFiles[Append[cloudCatalog, "Version" -> "1.2.3"], "VersionLabel" -> True]["index.html"]],
            brand[PacletPageFiles[cloudCatalog, "VersionLabel" -> "preview"]["index.html"]],
            brand[PacletPageFiles[cloudCatalog, "VersionLabel" -> True]["index.html"]],
            brand[PacletPageFiles[Append[cloudCatalog, "Version" -> "1.2.3"]]["index.html"]],
            With[{html = PacletPageFiles[cloudCatalog, "Footer" -> True]["index.html"]},
                {StringContainsQ[html, "<body class=\"with-footer\""], footer[html]}],
            footer[PacletPageFiles[cloudCatalog]["index.html"]]
        }
    ],
    {{"v1.2.3"}, {"preview"}, {}, {},
        {True, {"Published with <a href=\"" <> $pacletPageSite <> "\" target=\"_blank\" rel=\"noopener\">PacletPage " <> $pacletPageVersion <> "</a>"}}, {}},
    TestID -> "Site: \"VersionLabel\" puts the paclet's version under the name, \"Footer\" names the PacletPage it was published with; both off by default"
]

VerificationTest[
    With[{
        def = PacletPageFiles[cloudCatalog, "LayoutControls" -> False]["index.html"],
        all = PacletPageFiles[cloudCatalog]["index.html"],
        noSym = PacletPageFiles[cloudCatalog, "LayoutControls" -> True, "SymbolsRail" -> False]["index.html"],
        subset = PacletPageFiles[cloudCatalog, "LayoutControls" -> {"LeftSidebar", "Fullscreen"}]["index.html"],
        leftOnly = PacletPageFiles[cloudCatalog, "LayoutControls" -> {"LeftSidebar"}]["index.html"]
    },
        {
            StringFreeQ[def, {"<div class=\"pp-layout-controls\"", "id=\"toggleSideL\"", "id=\"toggleSideR\"", "id=\"toggleFull\"", "id=\"ppFullExit\""}],
            AllTrue[{"<div class=\"pp-layout-controls\"", "id=\"toggleSideL\"", "id=\"toggleSideR\"", "id=\"toggleFull\"", "id=\"ppFullExit\"",
                "M8 4v12", "M12 4v12", "M3 8V3h5M12 3h5v5M17 12v5h-5M8 17H3v-5"}, StringContainsQ[all, #] &],
            {StringContainsQ[noSym, "id=\"toggleSideL\""], StringFreeQ[noSym, "id=\"toggleSideR\""], StringContainsQ[noSym, "id=\"toggleFull\""]},
            {StringContainsQ[subset, "id=\"toggleSideL\""], StringFreeQ[subset, "id=\"toggleSideR\""], StringContainsQ[subset, "id=\"toggleFull\""], StringContainsQ[subset, "id=\"ppFullExit\""]},
            {StringContainsQ[leftOnly, "id=\"toggleSideL\""], StringFreeQ[leftOnly, "id=\"toggleSideR\""], StringFreeQ[leftOnly, "id=\"toggleFull\""], StringFreeQ[leftOnly, "id=\"ppFullExit\""]}
        }
    ],
    {True, True, {True, True, True}, {True, True, True, True}, {True, True, True, True}},
    TestID -> "Site: \"LayoutControls\" adds left/right sidebar and fullscreen buttons, on by default, and False leaves them out"
]

VerificationTest[
    With[{
        auto = PacletPageFiles[cloudCatalog],
        dark = PacletPageFiles[cloudCatalog, "DarkMode" -> True],
        off = PacletPageFiles[cloudCatalog, "DarkMode" -> False]
    },
        {
            StringContainsQ[auto["index.html"], "id=\"themeToggle\""],
            StringContainsQ[auto["index.html"], "fallback = 'auto'"] && StringContainsQ[auto["page.html"], "fallback = 'auto'"],
            StringContainsQ[dark["index.html"], "fallback = 'dark'"] && StringContainsQ[dark["page.html"], "fallback = 'dark'"],
            StringFreeQ[off["index.html"], {"id=\"themeToggle\"", "fallback = '"}] && StringFreeQ[off["page.html"], "fallback = '"],
            (* the head script runs before the stylesheet, so the first paint has the theme *)
            Less @@ (First[First[StringPosition[auto["index.html"], #]]] & /@ {"fallback = '", "<style>"}),
            StringContainsQ[auto["index.html"], "--pp-dark-active-bg: #481d1b; --pp-dark-active-fg: #e96459;"],
            StringContainsQ[auto["page.html"], "html[data-theme=\"dark\"] #notebook-embed"],
            StringFreeQ[auto["index.html"] <> auto["page.html"], "`head`"]
        }
    ],
    {True, True, True, True, True, True, True, True},
    TestID -> "Site: \"DarkMode\" follows the system by default with a switch in the header, True starts dark, False keeps the site light"
]

VerificationTest[
    PacletPageFiles[cloudCatalog, "DarkMode" -> "Dim"],
    $Failed,
    {PacletPageFiles::darkmode},
    TestID -> "Site: an invalid \"DarkMode\" value reports a message and fails"
]

VerificationTest[
    {cssMixHex["#dd1100", "#1e2023", 0.78], cssMixHex["#dd1100", "#ffffff", 0.35], cssTintHex["#dd1100", 0.92], cssMixHex["#abc", "#000", 0]},
    {"#481d1b", "#e96459", "#fceceb", "#aabbcc"},
    TestID -> "Site: the dark theme's active colors mix the accent with its rail and with white"
]

VerificationTest[
    Module[{pages, catalog, small, html, rail},
        pages = AssociationMap[Function[id, With[{name = StringDrop[id, 4]}, fixturePage["ref", name, name, id <> ".nb"]]],
            "ref/" <> # & /@ Join[{"Zebra"}, "Apple" <> IntegerString[#, 10, 2] & /@ Range[30]]];
        catalog = <|"Name" -> "Pub/Big", "Title" -> "Big", "Kind" -> "Local", "Resource" -> "", "DocBase" -> "Documentation/",
            "RootGuide" -> None, "Pages" -> pages|>;
        small = MapAt[Take[#, 24] &, catalog, Key["Pages"]];
        html = Function[{c, opts}, PacletPageFiles[c, Sequence @@ opts]["index.html"]];
        rail = Function[h, First[StringCases[h, "<nav id=\"side-right\"" ~~ Shortest[attrs___] ~~ ">" :> attrs], ""]];
        {
            (* Automatic: the index there and off, every symbol listed, a button to turn it on *)
            {rail[html[catalog, {}]], StringFreeQ[html[catalog, {}], "<template id=\"symbol-rest\">"],
                StringCount[html[catalog, {}], "class=\"sym\""], StringContainsQ[html[catalog, {}], "class=\"symbol-index-toggle\" aria-pressed=\"false\""]},
            (* True: on from the start, streamed in batches, the button to turn it off *)
            {rail[html[catalog, {"SymbolIndex" -> True}]], StringContainsQ[html[catalog, {"SymbolIndex" -> True}], "<template id=\"symbol-rest\">"],
                StringContainsQ[html[catalog, {"SymbolIndex" -> True}], "class=\"symbol-index-toggle\" aria-pressed=\"true\""]},
            (* False, and a rail of 24 symbols: no index and no button *)
            {StringFreeQ[rail[html[catalog, {"SymbolIndex" -> False}]], "indexable"], StringFreeQ[html[catalog, {"SymbolIndex" -> False}], "class=\"symbol-index-toggle\""],
                StringCount[html[catalog, {"SymbolIndex" -> False}], "class=\"sym\""]},
            {StringFreeQ[rail[html[small, {}]], "indexable"], StringFreeQ[html[small, {}], "class=\"symbol-index-toggle\""]},
            (* the reader's choice, kept per site, applies before the rail paints *)
            StringContainsQ[html[catalog, {}], "localStorage.getItem('pp-symbol-index:' + location.pathname)"]
        }
    ],
    {
        {" class=\"indexable\" data-batch-size=\"45\" aria-label=\"Symbols\"", True, 31, True},
        {" class=\"indexable indexed\" data-batch-size=\"45\" aria-label=\"Symbols\"", True, True},
        {True, True, 31},
        {True, True},
        True
    },
    TestID -> "Site: \"SymbolIndex\" makes the A-Z index optional, off until the reader turns it on by default, on with True, none with False"
]

VerificationTest[
    Module[{catalog, html, expr, rail},
        catalog = MapAt[Join[#, <|
            "ref/format/TTE" -> fixturePage["ref", "format/TTE", "TTE", "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/ref/format/TTE.nb"],
            "ref/service/Zed" -> fixturePage["ref", "service/Zed", "Zed", "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/ref/service/Zed.nb"]|>] &,
            cloudCatalog, Key["Pages"]];
        html = PacletPageFiles[catalog]["index.html"];
        rail = First[StringCases[html, "<nav id=\"side-right\"" ~~ Shortest[r___] ~~ "</nav>" :> r]];
        expr = shellXML[catalog, <|"Mode" -> "Expression", "Name" -> "Pub/Name", "Prefix" -> "pp-", "Title" -> "Cloud Fixture",
            "Subtitle" -> "", "Resource" -> "", "HomeID" -> "guide/Name", "ShowRootGuide" -> True, "StripTitleNumbers" -> True,
            "SymbolsRail" -> True, "Header" -> True, "Footer" -> False, "ShowFilter" -> True, "Filter" -> "",
            "Current" -> "ref/format/TTE", "ActiveTitle" -> "TTE", "Notebook" -> Notebook[{}]|>];
        {
            StringFreeQ[rail, {"data-id=\"ref/format/TTE\"", "data-id=\"ref/service/Zed\""}] && StringContainsQ[rail, "data-id=\"ref/$Var\""],
            StringContainsQ[html, ">Formats</div>"] && StringContainsQ[html, ">Services</div>"],
            StringContainsQ[html, "class=\"ref\" data-id=\"ref/format/TTE\""] || StringContainsQ[html, "data-id=\"ref/format/TTE\""],
            Cases[expr, XMLElement["div", {___, "class" -> "pp-nav-label", ___}, {label_String}] :> label, Infinity],
            FirstCase[expr, XMLElement["span", {___, "class" -> "pp-meta", ___}, {m_String}] :> m, None, Infinity],
            {symbolPageQ["ref/F"], symbolPageQ["ref/format/TTE"], otherReferencePageQ["ref/format/TTE"], referenceKind["ref/message/Foo/bad"], referenceKindLabel["format"], referenceKindLabel["widget"]},
            pageResolveID[catalog, "guide/Name", "ref/format/TTE"]
        }
    ],
    {True, True, True, {"Formats", "Services", "Symbols"}, "1 guide \[CenterDot] 1 symbol", {True, False, True, "message", "Formats", "Widgets"}, "ref/format/TTE"},
    TestID -> "Shell: a format's or a service's page is listed under its kind in the left rail, not among the symbols"
]

VerificationTest[
    PacletPageFiles[cloudCatalog, "SymbolIndex" -> "Sometimes"],
    $Failed,
    {PacletPageFiles::symbolindex},
    TestID -> "Site: an invalid \"SymbolIndex\" value reports a message and fails"
]

VerificationTest[
    PacletPageFiles[cloudCatalog, "LayoutControls" -> "NotAControl"],
    $Failed,
    {PacletPageFiles::layoutcontrols},
    TestID -> "Site: an invalid \"LayoutControls\" value reports a message and fails"
]

VerificationTest[
    {
        configOf[PacletPageFiles[cloudCatalog, "FeedbackThanks" -> "The team will read it."]["page.html"]]["thanks"],
        configOf[PacletPageFiles[cloudCatalog]["page.html"]]["thanks"]
    },
    {"The team will read it.", ""},
    TestID -> "Site: the thanks a sent feedback shows, the page's own by default"
]

VerificationTest[
    With[{catalog = MapAt[
            Join[#, <|"tutorial/Zed" -> Join[fixturePage["tutorial", "Zed", "Aardvark Notes", "https://s.example/obj/store/N/jjj.nb"], <|"Stub" -> True|>]|>] &,
            repositoryCatalog, Key["Pages"]]},
        {
            StringCases[sidebarHTML[catalog, <||>], "data-id=\"" ~~ id : ("tutorial/" ~~ Except["\""] ..) ~~ "\"" :> id],
            StringContainsQ[sidebarHTML[catalog, <||>], "<a class=\"tut stub\" data-id=\"tutorial/Zed\""]
        }
    ],
    {{"tutorial/Zed", "tutorial/Intro", "tutorial/Use"}, True},
    TestID -> "Site: tutorials in the order of their titles; a placeholder is marked"
]

VerificationTest[
    With[{catalog = MapAt[
            Join[#, <|
                "tutorial/Overview" -> Join[fixturePage["tutorial", "Overview", "Fixture Caf\[EAcute]", "https://s.example/obj/store/N/ov.nb"], <|"Overview" -> True|>],
                "tutorial/Aaa" -> fixturePage["tutorial", "Aaa", "Aaa Notes", "https://s.example/obj/store/N/aaa.nb"]|>] &,
            repositoryCatalog, Key["Pages"]]},
        {
            StringCases[sidebarHTML[catalog, <||>], "data-id=\"" ~~ id : ("tutorial/" ~~ Except["\""] ..) ~~ "\"" :> id],
            StringContainsQ[sidebarHTML[catalog, <||>], "data-id=\"tutorial/Overview\" href=\"#tutorial/Overview\">Overview</a>"]
        }
    ],
    {{"tutorial/Overview", "tutorial/Aaa", "tutorial/Intro", "tutorial/Use"}, True},
    TestID -> "Site: an Overview page comes first among the tutorials, labelled Overview"
]

VerificationTest[
    {
        strippedTitle /@ {"1. Basics", "2.3 Data", "4) Plots", "5: Sets", "06 - Notes", "3D Graphics", "2048 Games", "Basics", "7."},
        SortBy[{"10. Ten", "2. Two", "1. One", "9. Nine"}, naturalKey]
    },
    {{"Basics", "Data", "Plots", "Sets", "Notes", "3D Graphics", "2048 Games", "Basics", "7."}, {"1. One", "2. Two", "9. Nine", "10. Ten"}},
    TestID -> "Site: a title's numbering is a number and a dot, a parenthesis, a colon or a dash before it, and a title's digits order as its numbers"
]

VerificationTest[
    With[{catalog = MapAt[
            Join[#, <|
                "tutorial/Ten" -> fixturePage["tutorial", "Ten", "10. Tenth Steps", "https://s.example/obj/store/N/ten.nb"],
                "tutorial/Two" -> fixturePage["tutorial", "Two", "2. Second Steps", "https://s.example/obj/store/N/two.nb"],
                "tutorial/One" -> fixturePage["tutorial", "One", "1. First Steps", "https://s.example/obj/store/N/one.nb"]|>] &,
            repositoryCatalog, Key["Pages"]]},
        {
            StringCases[sidebarHTML[catalog, <||>], "data-id=\"" ~~ id : ("tutorial/" ~~ Except["\""] ..) ~~ "\"" :> id],
            StringContainsQ[sidebarHTML[catalog, <||>], "href=\"#tutorial/Ten\">Tenth Steps</a>"],
            StringContainsQ[sidebarHTML[catalog, <||>, True, False], "href=\"#tutorial/Ten\">10. Tenth Steps</a>"],
            {StringContainsQ[PacletPageFiles[catalog]["index.html"], "10. Tenth Steps"], StringContainsQ[PacletPageFiles[catalog, "StripTitleNumbers" -> False]["index.html"], "10. Tenth Steps"]}
        }
    ],
    {{"tutorial/One", "tutorial/Two", "tutorial/Ten", "tutorial/Intro", "tutorial/Use"}, True, True, {False, True}},
    TestID -> "Site: numbered tutorials in the order of their numbers, 10 after 2, shown without the numbering unless \"StripTitleNumbers\" -> False"
]

VerificationTest[
    {
        tutorialKey[fixturePage["tutorial", "Overview", "Zebra", ""]][[1]],
        railPage[Join[fixturePage["tutorial", "Tour", "A Name Overview", ""], <|"Overview" -> True|>]]["Title"],
        railPage[fixturePage["tutorial", "Intro", "Intro", ""]]["Title"],
        overviewNotebookQ /@ {Notebook[{}, TaggingRules -> {"Metadata" -> {"type" -> "Overview"}}], Notebook[{}, TaggingRules -> {"Metadata" -> {"type" -> "Tutorial"}}], Notebook[{}]}
    },
    {0, "A Name Overview", "Intro", {True, False, False}},
    TestID -> "Site: an Overview page, by its metadata or its name, keeps a title that says Overview"
]

VerificationTest[
    {
        placeholderQ[Notebook[{Cell["A Title", "Title"], Cell["More About", "TutorialMoreAboutSection"], Cell["Metadata", "MetadataSection"]}]],
        placeholderQ[Notebook[{Cell["A Title", "Title"], Cell["Some words.", "Text"], Cell["More About", "TutorialMoreAboutSection"]}]],
        placeholderQ[Notebook[{Cell["Some words.", "Text"]}]],
        withMetadata[<|"Kind" -> "tutorial", "Name" -> "T", "Title" -> "T", "Keywords" -> {}|>, <|"Stub" -> True, "Title" -> "T", "Keywords" -> {}|>]["Stub"]
    },
    {True, False, False, True},
    TestID -> "Catalog: a tutorial with nothing between its title and the template's closing sections is a placeholder"
]

VerificationTest[
    Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["mail-"] <> ".txt"}], result},
        Export[file, "To: team@site.example\nSubject: S\n\nbody", "Text"];
        result = {
            feedbackAddress["a@b.example"], feedbackAddress["To: team@site.example\nSubject: S\n\nbody"],
            feedbackAddress[File[file]], feedbackAddress["no address here"], feedbackAddress[None]
        };
        DeleteFile[file];
        result
    ],
    {"a@b.example", "team@site.example", "team@site.example", None, None},
    TestID -> "Site: the address an email setting names: itself, or its template's To: line"
]

VerificationTest[
    {
        sharedFeedbackEmail[None, {"Email" -> "a@b.example", "Domains" -> {"b.example"}}],
        sharedFeedbackEmail["To: t@x.example\nSubject: S\n\nbody", {"Domains" -> {}}],
        sharedFeedbackEmail["a@b.example", {"Email" -> "c@d.example"}],
        sharedFeedbackEmail[None, {}]
    },
    {
        {"a@b.example", {"Email" -> "a@b.example", "Domains" -> {"b.example"}}},
        {"To: t@x.example\nSubject: S\n\nbody", {"Domains" -> {}, "Email" -> "t@x.example"}},
        {"a@b.example", {"Email" -> "c@d.example"}},
        {None, {}}
    },
    TestID -> "Cloud: one email setting serves the site's fallback and the feedback pages"
]

VerificationTest[
    With[{files = PacletPageFiles[repositoryCatalog, "FeedbackEmail" -> "To: x@y.example\nSubject: S </script>\n\nbody"]},
        StringCount[files["page.html"], "</script>"] === StringCount[files["page.html"], "<script"]
    ],
    True,
    TestID -> "Site: an email template cannot end the page's script elements"
]

VerificationTest[
    StringCases[PacletPageFiles[repositoryCatalog, "Accent" -> RGBColor[0, 0.5, 0]]["index.html"], "--pp-accent: " ~~ Shortest[__] ~~ ";"],
    {"--pp-accent: #008000;"},
    TestID -> "Site: the accent color replaces the stylesheet's own"
]

VerificationTest[
    PacletPageFiles[repositoryCatalog, "Accent" -> "red; x"],
    $Failed,
    {PacletPageFiles::accent},
    TestID -> "Site: a bad accent is refused"
]

VerificationTest[
    PacletPageFiles[<|"Name" -> "x"|>],
    $Failed,
    {PacletPageFiles::catalog},
    TestID -> "Site: only a catalog makes a site"
]


(* === publishing === *)

VerificationTest[
    siteNameQ /@ {"PAdic", "Sites/PAdic-docs", "../x", "a b", "", "a//b"},
    {True, True, False, False, False, False},
    TestID -> "Cloud: site names are plain paths"
]

VerificationTest[
    {strictlyBeneathQ["https://c/obj/u/SiteSource", "https://c/obj/u/SiteSource/ref/X.nb"],
     strictlyBeneathQ["https://c/obj/u/SiteSource", "https://c/obj/u/SiteSourceManifest"],
     strictlyBeneathQ["https://c/obj/u/SiteSource", "https://c/obj/u/SiteSource"]},
    {True, False, False},
    TestID -> "Cloud: cleanup reaches only what is strictly beneath its directory"
]

VerificationTest[
    siteURL["https://c.example/obj/u/Site/"],
    "https://c.example/obj/u/Site",
    TestID -> "Cloud: a site by its URL"
]

VerificationTest[
    Module[{dir = CreateDirectory[], found},
        Scan[
            (CreateDirectory[DirectoryName[#], CreateIntermediateDirectories -> True]; Put[Notebook[{}], #]) &,
            FileNameJoin[Prepend[#, dir]] & /@ {{"Guides", "G.nb"}, {"ReferencePages", "Symbols", "S.nb"}, {"Tutorials", "T.nb"}, {"Tutorials", "Nested", "N.nb"}}
        ];
        found = Sort[Last /@ sourceNotebooks[dir]];
        DeleteDirectory[dir, DeleteContents -> True];
        found
    ],
    {"guide/G.nb", "ref/S.nb", "tutorial/T.nb"},
    TestID -> "Cloud: the authoring notebooks, one level deep per kind"
]

VerificationTest[
    Module[{dir = CreateDirectory[], found},
        Scan[
            (If[! DirectoryQ[DirectoryName[#]], CreateDirectory[DirectoryName[#], CreateIntermediateDirectories -> True]]; Put[Notebook[{}], #]) &,
            FileNameJoin[Prepend[#, dir]] & /@ {{"Documentation", "English", "Guides", "G.nb"}, {"ResourceDefinition.nb"}}
        ];
        found = Sort[Last /@ sourceNotebooks[dir, "Name"]];
        DeleteDirectory[dir, DeleteContents -> True];
        found
    ],
    {"guide/G.nb", "resource/Name.nb"},
    TestID -> "Cloud: a paclet's ResourceDefinition.nb is the source of its definition page"
]


(* === publishing the resource === *)

VerificationTest[
    Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["hero-"] <> ".nb"}], hero},
        Put[Notebook[{
            Cell[CellGroupData[{Cell["Title", "Title"]}, Open]],
            Cell[CellGroupData[{
                Cell["Hero Image", "Section", CellTags -> {"Headline Image", "HeroImage", "TemplateCellGroup"}],
                Cell[BoxData[RowBox[{"Graphics", "[", RowBox[{"Disk", "[", "]"}], "]"}]], "Input"],
                Cell[BoxData[GraphicsBox[DiskBox[{0, 0}]]], "Output"]
            }, Open]]
        }], file];
        hero = definitionHero[file];
        DeleteFile[file];
        hero
    ],
    Graphics[Disk[{0, 0}]],
    TestID -> "Publish: the definition notebook's hero image, from its HeroImage group"
]

VerificationTest[
    {heroImage[None, "x.nb"], heroImage[Graphics[Point[{0, 0}]], "x.nb"], heroImage["no-such-file.png", "x.nb"]},
    {None, Graphics[Point[{0, 0}]], None},
    TestID -> "Publish: a hero image given, none, or a file that is not there"
]

VerificationTest[
    Module[{dir = CreateDirectory[], hashes},
        CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
        Export[FileNameJoin[{dir, "Kernel", "A.wl"}], "x = 1", "Text"];
        hashes = {codeHash[dir]};
        Export[FileNameJoin[{dir, "Kernel", "A.wl"}], "x = 2", "Text"];
        AppendTo[hashes, codeHash[dir]];
        RenameFile[FileNameJoin[{dir, "Kernel", "A.wl"}], FileNameJoin[{dir, "Kernel", "B.wl"}]];
        AppendTo[hashes, codeHash[dir]];
        DeleteDirectory[dir, DeleteContents -> True];
        DuplicateFreeQ[hashes]
    ],
    True,
    TestID -> "Publish: the code hash changes with a file's content and its name"
]

(* === building: the conversions, with a converter that writes the page's text and the options
   it was given, fails on "FAIL", hangs before writing on "HANG" and after writing on "LATE", and
   reports a section using a name from an earlier one on "STALE" === *)

MarkdownToNotebook::section = "`1`, under \"`2`\": `3` come out unevaluated.";

fakeConvert[file_String, out_String, opts : OptionsPattern[]] := Module[{text = Import[file, "Text"]},
    If[ StringContainsQ[text, "FAIL"], Return[$Failed, Module]];
    If[ StringContainsQ[text, "HANG"], Pause[20]];
    (* never yields: the first time only on "STUCKONCE", which leaves a file the next conversion finds *)
    If[ StringContainsQ[text, "STUCKONCE"],
        With[{once = FileNameJoin[{$TemporaryDirectory, "stuck-once-" <> IntegerString[Hash[file], 36]}]},
            If[ FileExistsQ[once], DeleteFile[once], Put[1, once]; AbortProtect[While[True, Null]]]],
        If[ StringContainsQ[text, "STUCK"], AbortProtect[While[True, Null]]]
    ];
    If[ StringContainsQ[text, "STALE"], Message[MarkdownToNotebook::section, FileBaseName[file], "Scope", {"x"}]];
    Put[Notebook[{Cell[text, "Text"], Cell[ToString[{opts}, InputForm], "Text"]}], out];
    If[ StringContainsQ[text, "LATE"], Pause[20]];
    out
];

fixtureDocs[] := Module[{dir = CreateDirectory[]},
    Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Fixture\", \"Version\" -> \"1.0.0\"|>]", "Text"];
    CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
    Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := x", "Text"];
    Scan[CreateDirectory[FileNameJoin[{dir, "docs", #}], CreateIntermediateDirectories -> True] &, {"Symbols", "Guides", "Tutorials"}];
    Export[FileNameJoin[{dir, "docs", "Symbols", "foo-page.md"}], "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Foo\n---\n\nFoo text.\n", "Text"];
    Export[FileNameJoin[{dir, "docs", "Guides", "Main.md"}], "---\nTemplate: Guide\nTitle: \"The Main Guide\"\nURI: Pub/Fixture/guide/Main\n---\n\nGuide text.\n", "Text"];
    Export[FileNameJoin[{dir, "docs", "Tutorials", "Steps.md"}], "---\nTemplate: TechNote\nURI: Pub/Fixture/tutorial/Steps\n---\n\nTutorial text.\n", "Text"];
    dir
];

notebookText[file_String] := Cases[Get[file], Cell[t_String, "Text"] :> t, Infinity];

withFixtureDocs[body_] := Module[{dir = fixtureDocs[], result},
    result = body[dir];
    PacletDirectoryUnload[dir];
    DeleteDirectory[dir, DeleteContents -> True];
    result
];
SetAttributes[withFixtureDocs, HoldAll];

VerificationTest[
    Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["front-"] <> ".md"}], result},
        Export[file, "---\r\nTemplate: Symbol\r\nTitle: \"A: quoted\"\r\nURI: 'Pub/X/ref/A'\r\nKeywords: [a, b]\r\n  - item\r\n---\r\n\r\nText.\r\n", "Text"];
        result = frontMatter[file];
        DeleteFile[file];
        result
    ],
    <|"Template" -> "Symbol", "Title" -> "A: quoted", "URI" -> "Pub/X/ref/A", "Keywords" -> "[a, b]"|>,
    TestID -> "Build: a page's front matter is read without converting it"
]

VerificationTest[
    {relativePath["/a/b", "/a/b/c/d.md"], relativePath["/a/b", "/a/bc/d.md"], buildKernels[False], buildKernels[1]},
    {FileNameJoin[{"c", "d.md"}], "/a/bc/d.md", {}, {}},
    TestID -> "Build: page ids are relative to the source; one kernel or none converts here"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{first, second},
        first = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        second = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        {
            FileNameTake[#, -2] & /@ Sort[first["Notebooks"]],
            Length /@ KeyTake[first, {"Converted", "Evaluated", "Cached", "Failed"}],
            Length /@ KeyTake[second, {"Converted", "Evaluated", "Cached", "Failed"}],
            notebookText[FileNameJoin[{dir, "Documentation", "English", "ReferencePages", "Symbols", "Foo.nb"}]],
            Length[FileNames["*.nb", FileNameJoin[{dir, "build", "cache", "pages"}]]]
        }
    ]]],
    {
        {FileNameJoin[{"Guides", "Main.nb"}], FileNameJoin[{"Symbols", "Foo.nb"}], FileNameJoin[{"Tutorials", "Steps.nb"}]},
        <|"Converted" -> 3, "Evaluated" -> 3, "Cached" -> 0, "Failed" -> 0|>,
        <|"Converted" -> 0, "Evaluated" -> 0, "Cached" -> 3, "Failed" -> 0|>,
        {"---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Foo\n---\n\nFoo text.", "{\"UseCache\" -> False, \"EvaluateSeparator\" -> Automatic}"},
        3
    },
    TestID -> "Build: a first build converts every page and evaluates its examples; the next copies them from the cache"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{edited, code, tutorial},
        BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        Export[FileNameJoin[{dir, "docs", "Symbols", "foo-page.md"}], "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Foo\n---\n\nFoo text, edited.\n", "Text"];
        edited = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := 2 x", "Text"];
        code = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        tutorial = notebookText[FileNameJoin[{dir, "Documentation", "English", "Tutorials", "Steps.nb"}]];
        {
            FileNameTake /@ edited["Converted"], edited["Evaluated"],
            Length[code["Converted"]], Length[code["Evaluated"]], Last[tutorial],
            Length[FileNames["*.nb", FileNameJoin[{dir, "build", "cache", "pages"}]]]
        }
    ]]],
    {{"foo-page.md"}, {}, 3, 3, "{\"UseCache\" -> False, \"EvaluateSeparator\" -> None}", 3},
    TestID -> "Build: an edited page is converted with its examples cached; a change to the code evaluates every page afresh"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{options},
        BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "EvaluateSeparator" -> <|"Symbol" -> All|>];
        options = Last[notebookText[FileNameJoin[{dir, "Documentation", "English", #}]]] &;
        options /@ {FileNameJoin[{"ReferencePages", "Symbols", "Foo.nb"}], FileNameJoin[{"Tutorials", "Steps.nb"}], FileNameJoin[{"Guides", "Main.nb"}]}
    ]]],
    {
        "{\"UseCache\" -> False, \"EvaluateSeparator\" -> All}",
        "{\"UseCache\" -> False, \"EvaluateSeparator\" -> None}",
        "{\"UseCache\" -> False, \"EvaluateSeparator\" -> Automatic}"
    },
    TestID -> "Build: a tutorial runs in one context, whatever a by-template separator names for other pages"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{options},
        BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None,
            "ConverterOptions" -> <|"foo-*.md" -> {"Evaluate" -> False}, "Main.md" -> {"MathFont" -> "X"}|>];
        options = Last[notebookText[FileNameJoin[{dir, "Documentation", "English", #}]]] &;
        options /@ {FileNameJoin[{"ReferencePages", "Symbols", "Foo.nb"}], FileNameJoin[{"Guides", "Main.nb"}], FileNameJoin[{"Tutorials", "Steps.nb"}]}
    ]]],
    {
        "{\"UseCache\" -> False, \"EvaluateSeparator\" -> Automatic, \"Evaluate\" -> False}",
        "{\"UseCache\" -> False, \"EvaluateSeparator\" -> Automatic, \"MathFont\" -> \"X\"}",
        "{\"UseCache\" -> False, \"EvaluateSeparator\" -> None}"
    },
    TestID -> "Build: converter options by page, a file name or a pattern for one, reach only the pages they name"
]

VerificationTest[
    dependencyName /@ {"A", "B" -> "1.0+", {"C", "1.1+"}, 3},
    {"A", "B", "C"},
    TestID -> "Build: a dependency is named by itself, by a rule to its version, or by a list with its version"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{built},
        Export[FileNameJoin[{dir, "docs", "Symbols", "foo-page.md"}], "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Foo\n---\n\nFoo text. STALE\n", "Text"];
        built = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        {KeyMap[FileNameTake, built["Unevaluated"]], Length[built["Notebooks"]], Length[FileNames["*.nb", FileNameJoin[{dir, "build", "cache", "pages"}]]]}
    ]]],
    {<|"foo-page.md" -> {<|"Heading" -> "Scope", "Symbols" -> {"x"}|>}|>, 3, 2},
    {MarkdownToNotebook::section},
    TestID -> "Build: a page whose sections use names from earlier ones is reported, and stays out of the cache"
]

fakeConvertTwin[file_String, out_String, opts : OptionsPattern[]] := fakeConvert[file, out, opts];

VerificationTest[
    withFixtureDocs[Function[dir, Module[{first, second},
        first = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        second = BuildPacletDocumentation[dir, "Converter" -> fakeConvertTwin, "Definition" -> None];
        {Length[first["Evaluated"]], Length[second["Converted"]], Length[second["Evaluated"]]}
    ]]],
    {3, 3, 3},
    TestID -> "Build: another converter converts every page again and evaluates its examples afresh"
]

(* === building with the paclet's definitions scanned: a paclet of three functions, foo calling a
   private helper, the public ones read-protected, and pages whose code names them === *)

definitionFixture[pages_Association] := Module[{dir = CreateDirectory[], name = "Fx" <> IntegerString[RandomInteger[10^12], 36]},
    Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/" <> name <> "\", \"Version\" -> \"1.0.0\", \"PrimaryContext\" -> \"" <> name <> "`\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"" <> name <> "`\"}}|>]", "Text"];
    CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
    CreateDirectory[FileNameJoin[{dir, "docs", "Symbols"}], CreateIntermediateDirectories -> True];
    KeyValueMap[Export[FileNameJoin[{dir, "docs", "Symbols", #1 <> ".md"}], "---\nTemplate: Symbol\nURI: Pub/" <> name <> "/ref/" <> #1 <> "\n---\n\n" <> #2, "Text"] &, pages];
    <|"Directory" -> dir, "Code" -> FileNameJoin[{dir, "Kernel", name <> ".wl"}], "Context" -> name <> "`"|>
];
definitionCode[fixture_, helper_String, bar_String] := Export[fixture["Code"], StringRiffle[{
    "BeginPackage[\"" <> fixture["Context"] <> "\"]", "foo::usage = \"foo[x]\";", "bar::usage = \"bar[x]\";", "Begin[\"`Private`\"]",
    "foo[x_] := helper[x]", "helper[x_] := " <> helper, "bar[x_] := " <> bar, "End[]", "SetAttributes[{foo, bar}, {Protected, ReadProtected}]", "EndPackage[]"
}, "\n"], "Text"];
withDefinitionFixture[pages_, body_] := Module[{fixture = definitionFixture[pages], path = $ContextPath, result},
    definitionCode[fixture, "x + 1", "2 x"];
    result = body[fixture];
    (* the contexts the fixture's packages put on the path leave it, so that the next fixture's
       functions of the same names shadow nothing *)
    $ContextPath = Select[$ContextPath, MemberQ[path, #] &];
    PacletDirectoryUnload[fixture["Directory"]];
    DeleteDirectory[fixture["Directory"], DeleteContents -> True];
    result
];

VerificationTest[
    withDefinitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n", "bar" -> "```wl\nbar[1]\n```\n"|>, Function[fixture, Module[{build, runs = {}},
        build[] := BuildPacletDocumentation[fixture["Directory"], "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False];
        AppendTo[runs, build[]];
        AppendTo[runs, build[]];
        definitionCode[fixture, "x + 2", "2 x"];
        AppendTo[runs, build[]];
        definitionCode[fixture, "x + 2", "3 x"];
        AppendTo[runs, build[]];
        Export[FileNameJoin[{fixture["Directory"], "Assets", "table.txt"}], "1", "Text"];
        AppendTo[runs, build[]];
        {
            Sort[FileBaseName /@ #["Converted"]] & /@ runs,
            FileExistsQ[FileNameJoin[{fixture["Directory"], "build", "cache", "definitions.wxf"}]]
        }
    ]]],
    {{{"bar", "foo"}, {}, {"foo"}, {"bar"}, {"bar", "foo"}}, True},
    TestID -> "Build: with the definitions scanned, a change to one function converts again only the pages whose code reaches it; a file the loading does not read, every page"
]

VerificationTest[
    withDefinitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n"|>, Function[fixture, Module[{build},
        build[mode_] := FileBaseName /@ BuildPacletDocumentation[fixture["Directory"], "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False, "CacheKey" -> mode]["Converted"];
        {build["Code"], build["Code"], definitionCode[fixture, "x + 3", "2 x"]; build["Code"], build["Definitions"], definitionCode[fixture, "x + 3", "5 x"]; build["Code"]}
    ]]],
    {{"foo"}, {}, {"foo"}, {"foo"}, {"foo"}},
    TestID -> "Build: \"CacheKey\" -> \"Code\" keys every page by all of the paclet's code"
]

VerificationTest[
    withDefinitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n", "bar" -> "```wl\nbar[1]\n```\n"|>, Function[fixture, Module[
        {scan, context = fixture["Context"], page, before, after},
        page[name_] := PacletPageDependencies[fixture["Directory"], FileNameJoin[{fixture["Directory"], "docs", "Symbols", name <> ".md"}], "Converter" -> fakeConvert];
        scan = PacletDefinitionScan[fixture["Directory"]];
        before = page /@ {"foo", "bar"};
        definitionCode[fixture, "x + 7", "2 x"];
        after = page /@ {"foo", "bar"};
        {
            Sort[StringDelete[Keys[scan["Symbols"]], context]],
            Sort[StringDelete[#, context] & /@ List @@@ EdgeList[scan["Graph"]]],
            StringDelete[#["Reached"], context] & /@ before,
            MapThread[#1["Code"] === #2["Code"] &, {before, after}],
            Quiet[PacletDefinitionScan[$TemporaryDirectory], PacletDefinitionScan::dir]
        }
    ]]],
    {{"bar", "foo", "Private`helper"}, {{"foo", "Private`helper"}}, {{"foo", "Private`helper"}, {"bar"}}, {False, True}, $Failed},
    TestID -> "Scan: the symbols with definitions, the graph of their uses, and what a page names and reaches; its code changes with a definition it reaches"
]

VerificationTest[
    Module[{root = CreateDirectory[], path = $ContextPath, fixture, cache, build, first, moved, second},
        WithCleanup[
            fixture = definitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n", "bar" -> "```wl\nbar[1]\n```\n"|>];
            (* foo's helper reaches a value holding the paclet's directory, as a paclet's own root does *)
            Export[fixture["Code"], StringRiffle[{
                "BeginPackage[\"" <> fixture["Context"] <> "\"]", "foo::usage = \"foo[x]\";", "bar::usage = \"bar[x]\";", "Begin[\"`Private`\"]",
                "$root = ParentDirectory[DirectoryName[$InputFileName]];", "foo[x_] := helper[x]", "helper[x_] := {x, FileNameJoin[{$root, \"data\"}], $root}",
                "bar[x_] := 2 x", "End[]", "EndPackage[]"
            }, "\n"], "Text"];
            cache = FileNameJoin[{root, "cache"}];
            build[dir_] := FileBaseName /@ BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False, "CacheDirectory" -> cache]["Converted"];
            first = build[fixture["Directory"]];
            PacletDirectoryUnload[fixture["Directory"]];
            CreateDirectory[FileNameJoin[{root, "elsewhere", "deeper"}], CreateIntermediateDirectories -> True];
            moved = CopyDirectory[fixture["Directory"], FileNameJoin[{root, "elsewhere", "deeper", FileNameTake[fixture["Directory"]]}]];
            second = build[moved];
            {Sort[first], second},
            Scan[PacletDirectoryUnload, Select[{fixture["Directory"], moved}, StringQ]];
            $ContextPath = Select[$ContextPath, MemberQ[path, #] &];
            Scan[If[StringQ[#] && DirectoryQ[#], DeleteDirectory[#, DeleteContents -> True]] &, {fixture["Directory"], root}]
        ]
    ],
    {{"bar", "foo"}, {}},
    TestID -> "Build: the same sources at another path, with the same cache, convert nothing, though a definition a page reaches holds the paclet's directory"
]

VerificationTest[
    Module[{root = CreateDirectory[], path = $ContextPath, d = "Dx" <> IntegerString[RandomInteger[10^12], 36], c = "Cx" <> IntegerString[RandomInteger[10^12], 36],
            dep, pinned, newer, consumer, cache, build, first, second, after},
        WithCleanup[
            (* two copies of a dependency the consumer declares: one installed for the build into its
               dependency directory, pinned, and a newer one loaded from elsewhere, as one installed would be *)
            dep[sub_, version_, body_] := With[{dd = FileNameJoin[{root, sub, "Pub__" <> d <> "-" <> version}]},
                CreateDirectory[FileNameJoin[{dd, "Kernel"}], CreateIntermediateDirectories -> True];
                Export[FileNameJoin[{dd, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/" <> d <> "\", \"Version\" -> \"" <> version <> "\", \"PrimaryContext\" -> \"" <> d <>
                    "`\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"" <> d <> "`\"}}|>]", "Text"];
                Export[FileNameJoin[{dd, "Kernel", d <> ".wl"}], StringRiffle[{"BeginPackage[\"" <> d <> "`\"]", "f::usage = \"f[x]\";", "Begin[\"`Private`\"]",
                    "f[x_] := " <> body, "End[]", "EndPackage[]"}, "\n"], "Text"];
                dd
            ];
            pinned = dep["deps", "1.0", "x + 1"];
            newer = dep["newer", "2.0", "x + 2"];
            PacletDirectoryLoad[FileNameJoin[{root, "deps"}]];
            consumer = FileNameJoin[{root, "consumer"}];
            CreateDirectory[FileNameJoin[{consumer, "Kernel"}], CreateIntermediateDirectories -> True];
            CreateDirectory[FileNameJoin[{consumer, "docs", "Symbols"}], CreateIntermediateDirectories -> True];
            Export[FileNameJoin[{consumer, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/" <> c <> "\", \"Version\" -> \"1.0.0\", \"PrimaryContext\" -> \"" <> c <>
                "`\", \"Dependencies\" -> {\"Pub/" <> d <> "\"}, \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"" <> c <> "`\"}}|>]", "Text"];
            Export[FileNameJoin[{consumer, "Kernel", c <> ".wl"}], StringRiffle[{"BeginPackage[\"" <> c <> "`\", {\"" <> d <> "`\"}]", "foo::usage = \"foo[x]\";",
                "Begin[\"`Private`\"]", "foo[x_] := f[x]", "End[]", "EndPackage[]"}, "\n"], "Text"];
            Export[FileNameJoin[{consumer, "docs", "Symbols", "foo.md"}], "---\nTemplate: Symbol\nURI: Pub/" <> c <> "/ref/foo\n---\n\n```wl\nfoo[1]\n```\n", "Text"];
            cache = FileNameJoin[{root, "cache"}];
            build[] := Block[{$pinnedTargets = {ExpandFileName[FileNameJoin[{root, "deps"}]]}},
                FileBaseName /@ BuildPacletDocumentation[consumer, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False, "CacheDirectory" -> cache]["Converted"]];
            first = build[];
            PacletDirectoryLoad[newer];
            second = build[];
            after = Sort[Through[PacletFind["Pub/" <> d]["Version"]]];
            {first, second, after},
            Scan[PacletDirectoryUnload, {FileNameJoin[{root, "deps"}], newer, consumer}];
            $ContextPath = Select[$ContextPath, MemberQ[path, #] &];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {{"foo"}, {}, {"1.0", "2.0"}},
    TestID -> "Build: a newer copy of a declared dependency found elsewhere is set aside for the pinned copy, in the build's kernel and its scan, and found again after"
]

VerificationTest[
    Module[{root = CreateDirectory[], copy, loaded, unloaded},
        WithCleanup[
            copy[sub_] := With[{dd = FileNameJoin[{root, sub, "Pub__Lib-1.0"}]},
                CreateDirectory[dd, CreateIntermediateDirectories -> True];
                Export[FileNameJoin[{dd, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Lib\", \"Version\" -> \"1.0\"|>]", "Text"];
                DirectoryName[dd]
            ];
            loaded = copy["loaded"]; unloaded = copy["unloaded"];
            PacletDirectoryLoad[loaded];
            Block[{$pinnedTargets = ExpandFileName /@ {loaded, unloaded}},
                FileNameTake[#, -2] & /@ Rest[Most[buildCopies[FileNameJoin[{root, "consumer"}], {}]]]],
            PacletDirectoryUnload[loaded];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    {FileNameJoin[{"loaded", "Pub__Lib-1.0"}]},
    TestID -> "Build: the copies a build uses include the pinned ones in a dependency directory still loaded"
]

VerificationTest[
    Module[{root = CreateDirectory[], path = $ContextPath, name = "Fx" <> IntegerString[RandomInteger[10^12], 36], dir, info, build, runs = {}},
        WithCleanup[
            dir = FileNameJoin[{root, "consumer"}];
            CreateDirectory[FileNameJoin[{dir, "Kernel"}], CreateIntermediateDirectories -> True];
            CreateDirectory[FileNameJoin[{dir, "docs", "Symbols"}], CreateIntermediateDirectories -> True];
            (* a consumer that declares PacletPage with a minimum and a build's pin *)
            info[minimum_, pin_] := Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/" <> name <> "\", \"Version\" -> \"1.0.0\", \"PrimaryContext\" -> \"" <> name <>
                "`\", \"Dependencies\" -> {<|\"Name\" -> \"WolframInstitute/PacletPage\", \"Version\" -> \"" <> minimum <> "\", \"Pin\" -> <|\"Version\" -> \"" <> pin <>
                "\"|>|>}, \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"" <> name <> "`\"}}|>]", "Text"];
            (* foo uses nothing of the declaration; setup reads it as it runs *)
            Export[FileNameJoin[{dir, "Kernel", name <> ".wl"}], StringRiffle[{"BeginPackage[\"" <> name <> "`\"]", "foo::usage = \"foo[x]\";", "setup::usage = \"setup[]\";",
                "Begin[\"`Private`\"]", "foo[x_] := x + 1",
                "setup[] := WolframInstitute`PacletPage`PacletDependencyInformation[\"Pub/" <> name <> "\", \"Output\" -> \"Association\"]", "End[]", "EndPackage[]"}, "\n"], "Text"];
            Scan[Export[FileNameJoin[{dir, "docs", "Symbols", # <> ".md"}], "---\nTemplate: Symbol\nURI: Pub/" <> name <> "/ref/" <> # <> "\n---\n\n```wl\n" <> # <> "[]\n```\n", "Text"] &, {"foo", "setup"}];
            build[] := AppendTo[runs, Sort[FileBaseName /@ BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False]["Converted"]]];
            info["0.3+", "0.3.98"]; build[];
            info["0.3+", "0.3.99"]; build[];
            info["0.4+", "0.3.99"]; build[];
            runs,
            PacletDirectoryUnload[dir];
            $ContextPath = Select[$ContextPath, MemberQ[path, #] &];
            DeleteDirectory[root, DeleteContents -> True]
        ]
    ],
    (* setup's output, PacletDependencyInformation's, shows the pin and the minimum *)
    {{"foo", "setup"}, {"setup"}, {"setup"}},
    TestID -> "Build: moving a dependency's pin or minimum converts again only the pages whose code reads the declaration"
]

VerificationTest[
    Module[{root = CreateDirectory[], work = CreateDirectory[], d = "Dx" <> IntegerString[RandomInteger[10^12], 36], c = "Cx" <> IntegerString[RandomInteger[10^12], 36],
            paclet, dep, consumer, scan},
        WithCleanup[
            paclet[dd_, name_, code_List] := (
                CreateDirectory[FileNameJoin[{dd, "Kernel"}], CreateIntermediateDirectories -> True];
                Export[FileNameJoin[{dd, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/" <> name <> "\", \"Version\" -> \"1.0\", \"PrimaryContext\" -> \"" <> name <>
                    "`\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"" <> name <> "`\"}}|>]", "Text"];
                Export[FileNameJoin[{dd, "Kernel", name <> ".wl"}], StringRiffle[code, "\n"], "Text"];
                dd);
            (* a dependency whose loading loads the cloud's packages and reads the cloud's base, as one
               that reads a ResourceFunction on a cold cache does, and adds itself to $Packages *)
            dep = paclet[FileNameJoin[{root, "dep"}], d, {"BeginPackage[\"" <> d <> "`\"]", "f::usage = \"f[x]\";", "Begin[\"`Private`\"]",
                "Needs[\"CloudObject`\"]", "base = $CloudBase;", "f[x_] := {x, base}", "End[]", "EndPackage[]"}];
            consumer = paclet[FileNameJoin[{root, "consumer"}], c, {"BeginPackage[\"" <> c <> "`\", {\"" <> d <> "`\"}]", "g::usage = \"g[x]\";",
                "Begin[\"`Private`\"]", "g[x_] := f[x]", "End[]", "EndPackage[]"}];
            PacletDirectoryLoad[consumer];
            scan = scanPaclet[consumer, c <> "`", {dep}, <|"Excluded" -> {}, "Cache" -> FileNameJoin[{root, "cache"}], "Work" -> work, "Monitor" -> False, "Preferred" -> {consumer, dep}|>];
            {AssociationQ[scan], Lookup[Lookup[scan, "DependencyChanged", <||>], dep, Missing[]]},
            PacletDirectoryUnload[consumer];
            Scan[DeleteDirectory[#, DeleteContents -> True] &, {root, work}]
        ]
    ],
    {True, <||>},
    TestID -> "Scan: a dependency that loads Wolfram's cloud packages, or adds itself to $Packages, changes nothing of System` that another loading first, or a warm cache, would not"
]

VerificationTest[
    withDefinitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n"|>, Function[fixture, Module[{work = CreateDirectory[], in, out, run, result},
        in = FileNameJoin[{work, "in.wxf"}];
        out = FileNameJoin[{work, "out.wxf"}];
        (* a directory PacletDirectoryLoad was given that holds no PacletInfo of its own, as a repository's root *)
        Export[in, <|"Directories" -> {fixture["Directory"], DirectoryName[fixture["Directory"]]}, "Context" -> fixture["Context"], "Dependencies" -> {}|>, "WXF"];
        run = RunProcess[kernelScriptCommand[kernelExecutable[], $definitionScan, {in, out}], All, ""];
        result = {StringContainsQ[run["StandardOutput"] <> run["StandardError"], "badarg"], KeyExistsQ[Import[out, "WXF"], "Cones"]};
        DeleteDirectory[work, DeleteContents -> True];
        result
    ]]],
    {False, True},
    TestID -> "Scan: a directory with no PacletInfo among the scanned ones is passed over without a message"
]

VerificationTest[
    withDefinitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n", "bar" -> "```wl\nbar[1]\n```\n"|>, Function[fixture, Module[
        {dep = CreateDirectory[], name = "Dp" <> IntegerString[RandomInteger[10^12], 36], depCode, build, runs = {}},
        CreateDirectory[FileNameJoin[{dep, "Kernel"}]];
        depCode[version_String, f_String, g_String] := (
            Export[FileNameJoin[{dep, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/" <> name <> "\", \"Version\" -> \"" <> version <> "\", \"PrimaryContext\" -> \"" <> name <> "`\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"" <> name <> "`\"}}|>]", "Text"];
            Export[FileNameJoin[{dep, "Kernel", name <> ".wl"}], StringRiffle[{"BeginPackage[\"" <> name <> "`\"]", "depf", "depg", "Begin[\"`Private`\"]", "depf[x_] := " <> f, "depg[x_] := {" <> g <> ", \"table.txt\"}", "End[]", "EndPackage[]"}, "\n"], "Text"]
        );
        depCode["1.0.0", "x + 1", "x + 2"];
        (* foo reaches the dependency only when it runs, by a name it passes to code that joins the
           dependency's context to it; bar holds a word that is a dependency's name too, and names
           nothing *)
        Export[fixture["Code"], StringRiffle[{
            "BeginPackage[\"" <> fixture["Context"] <> "\"]", "foo", "bar", "Begin[\"`Private`\"]",
            "foo[x_] := (Needs[\"" <> name <> "`\"]; named[\"depf\"][x])", "named[n_String] := Symbol[\"" <> name <> "`\" <> n]", "bar[x_] := {2 x, \"depg\", \"own.txt\"}", "End[]", "EndPackage[]"
        }, "\n"], "Text"];
        build[] := Sort[FileBaseName /@ BuildPacletDocumentation[fixture["Directory"], "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False, "Dependencies" -> {dep}]["Converted"]];
        AppendTo[runs, build[]];
        depCode["1.0.0", "x + 10", "x + 2"]; AppendTo[runs, build[]];
        depCode["1.0.0", "x + 10", "x + 20"]; AppendTo[runs, build[]];
        depCode["1.1.0", "x + 10", "x + 20"]; AppendTo[runs, build[]];
        Export[FileNameJoin[{dep, "Kernel", "Later.wl"}], "1", "Text"]; AppendTo[runs, build[]];
        (* a file the dependency's depg names, which no page reaches, and one of the paclet's own
           that its bar names *)
        Export[FileNameJoin[{dep, "Kernel", "table.txt"}], "1", "Text"]; AppendTo[runs, build[]];
        Export[FileNameJoin[{fixture["Directory"], "Kernel", "own.txt"}], "1", "Text"]; AppendTo[runs, build[]];
        PacletDirectoryUnload[dep];
        DeleteDirectory[dep, DeleteContents -> True];
        runs
    ]]],
    {{"bar", "foo"}, {"foo"}, {}, {}, {"foo"}, {}, {"bar"}},
    TestID -> "Build: a dependency's symbols are in the cones, also one loaded on first use: a change to it converts only the pages that reach it, its new version alone none, and a file its loading does not read the pages that reach any of it"
]

VerificationTest[
    withDefinitionFixture[<|"counter" -> "```wl\ncounter[]\n```\n", "notes" -> "Its growth is $x/\\log x$.\n"|>, Function[fixture, Module[{code, build},
        code[start_String] := Export[fixture["Code"], StringRiffle[{
            "BeginPackage[\"" <> fixture["Context"] <> "\"]", "counter::usage = \"counter[]\";", "Begin[\"`Private`\"]",
            "Module[{x = " <> start <> "}, counter[] := (x = x + 1)]", "Module[{y = 1}, y + 1]", "End[]", "EndPackage[]"
        }, "\n"], "Text"];
        build[] := Sort[FileBaseName /@ BuildPacletDocumentation[fixture["Directory"], "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False]["Converted"]];
        code["0"];
        {build[], code["5"]; build[], DeleteFile[FileNameJoin[{fixture["Directory"], "build", "cache", "definitions.wxf"}]]; build[]}
    ]]],
    {{"counter", "notes"}, {"counter"}, {}},
    TestID -> "Build: a closure's state is in the cone of the function that names it, and never in that of a page whose math ends a name with $"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{calls = 0, counting = False},
        Internal`InheritedBlock[{exampleCode},
            DownValues[exampleCode] = Prepend[DownValues[exampleCode], HoldPattern[exampleCode[arguments___]] /; ! counting :> Block[{counting = True}, calls++; exampleCode[arguments]]];
            BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False]
        ];
        calls
    ]]],
    1,
    TestID -> "Build: the paclet's whole code is hashed once for every page, not once a page"
]

VerificationTest[
    withDefinitionFixture[<|"foo" -> "```wl\nfoo[1]\n```\n"|>, Function[fixture, Module[{printed = {}},
        Block[{$deployMonitor = True, Print = Function[Null, AppendTo[printed, {##}]; Null]},
            BuildPacletDocumentation[fixture["Directory"], "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> False]
        ];
        printed
    ]]],
    {},
    TestID -> "Build: the definitions scan prints nothing, also in a monitored deploy, where a page's example may build documentation"
]

(* a converter with the converter's parsing and cache functions, in a context of their own: its
   pages' code blocks and headings, and a cache whose entries are named by page, cell and the
   code of the cells since the last heading. It evaluates every cell when one has no entry, and
   writes how many of them had one. *)
PPFakeConverter`mdSep = "\n\n";
PPFakeConverter`resolveSource[file_String] := <|"Text" -> ByteArrayToString[ReadByteArray[file], "UTF-8"], "Base" -> DirectoryName[file], "Name" -> FileBaseName[file]|>;
PPFakeConverter`litParse[text_String] := <|"Metadata" -> <|"Template" -> "Symbol"|>, "Blocks" -> StringCases[text, {
    "## " ~~ heading : Except["\n"] .. :> <|"Type" -> "Heading", "Level" -> 2, "Text" -> heading|>,
    "```wl\n" ~~ code : Shortest[___] ~~ "\n```" :> <|"Type" -> "Code", "Code" -> code|>
}]|>;
PPFakeConverter`resolveIncludes[blocks_, _] := blocks;
PPFakeConverter`executableQ[b_Association] := b["Type"] === "Code";
PPFakeConverter`executableQ[_] := False;
PPFakeConverter`resetBoundaryQ[_, item_] := item["Type"] === "Heading";
PPFakeConverter`$setupHeadingQ = False &;
PPFakeConverter`stableHash[s_String] := Hash[s, "SHA256"];
PPFakeConverter`cumulativeHashes[mode_, items_List] := Module[{acc = "", hashes = {}},
    Scan[(If[PPFakeConverter`resetBoundaryQ[mode, #], acc = ""]; If[PPFakeConverter`executableQ[#], acc = acc <> PPFakeConverter`mdSep <> #["Code"]; AppendTo[hashes, PPFakeConverter`stableHash[acc]]]) &, items];
    hashes
];
PPFakeConverter`exampleCacheName[doc_String, index_Integer, hash_Integer] := "PPFake/" <> doc <> "/" <> IntegerString[index, 10, 3] <> "-" <> IntegerString[hash, 36];
PPFakeConverter`evaluatorTag[_] := "";
PPFakeConverter`$narrativeTemplates = {};
PPFakeConverter`convert[file_String, out_String, opts : OptionsPattern[]] := Module[{blocks, names, held},
    blocks = PPFakeConverter`litParse[PPFakeConverter`resolveSource[file]["Text"]]["Blocks"];
    names = MapIndexed[PPFakeConverter`exampleCacheName[FileBaseName[file], First[#2], #1] &, PPFakeConverter`cumulativeHashes[Automatic, blocks]];
    held = Select[names, ! MissingQ[PersistentSymbol[#, PPFakeConverter`$cacheLocation]] &];
    If[ ! TrueQ[Lookup[{opts}, "UseCache", True]] || Length[held] < Length[names], Scan[(PersistentSymbol[#, PPFakeConverter`$cacheLocation] = "result") &, names]];
    Put[Notebook[{Cell[ToString[{Length[held], Length[names]}], "Text"]}], out];
    out
];

VerificationTest[
    withDefinitionFixture[
        <|"foo" -> "## A\n\n```wl\nfoo[1]\n```\n\n## B\n\n```wl\nbar[1]\n```\n", "bar" -> "Unlike foo:\n\n```wl\nbar[2]\n```\n"|>,
        Function[fixture, Module[{store = CreateDirectory[], build, held, runs = {}},
            PPFakeConverter`$cacheLocation = PersistenceLocation["Local", store];
            build[] := BuildPacletDocumentation[fixture["Directory"], "Converter" -> PPFakeConverter`convert, "Definition" -> None, "Parallel" -> False];
            held[] := First /@ (notebookText[FileNameJoin[{fixture["Directory"], "Documentation", "English", "ReferencePages", "Symbols", # <> ".nb"}]] & /@ {"foo", "bar"});
            AppendTo[runs, {build[], held[]}];
            definitionCode[fixture, "x + 1", "4 x"];
            AppendTo[runs, {build[], held[]}];
            definitionCode[fixture, "x + 5", "4 x"];
            AppendTo[runs, {build[], held[]}];
            Export[FileNameJoin[{fixture["Directory"], "docs", "Symbols", "foo.md"}], "---\nTemplate: Symbol\n---\n\n## A\n\n```wl\nfoo[1]\n```\n\n## B\n\n```wl\nbar[3]\n```\n", "Text"];
            build[];
            With[{names = {"PPFake/foo/x", "PPFake/bar/x"}, kept = PersistenceLocation["Local", FileNameJoin[{fixture["Directory"], "build", "cache", "examples"}]]},
                AppendTo[runs, {Length[storedNames[kept, names]], Length[storedNames[PPFakeConverter`$cacheLocation, names]]}]
            ];
            (* another converter with the same cache: every page again, no example *)
            PPFakeConverter`convertTwin[file_String, out_String, opts : OptionsPattern[]] := PPFakeConverter`convert[file, out, opts];
            With[{twin = BuildPacletDocumentation[fixture["Directory"], "Converter" -> PPFakeConverter`convertTwin, "Definition" -> None, "Parallel" -> False]},
                runs = Insert[runs, {twin, {}}, -2]];
            DeleteDirectory[store, DeleteContents -> True];
            Append[{Sort[FileBaseName /@ #[[1, "Converted"]]], Sort[FileBaseName /@ #[[1, "Evaluated"]]], #[[2]]} & /@ Most[runs], Last[runs]]
        ]]
    ],
    {
        {{"bar", "foo"}, {"bar", "foo"}, {"{0, 2}", "{0, 1}"}},
        {{"bar", "foo"}, {"bar", "foo"}, {"{1, 2}", "{0, 1}"}},
        {{"bar", "foo"}, {"foo"}, {"{1, 2}", "{1, 1}"}},
        {{"bar", "foo"}, {}, {}},
        {3, 0}
    },
    TestID -> "Build: with the definitions scanned, only the cached results of the examples whose cells reach a changed function are deleted, so a page whose code does not evaluates nothing; the results are kept in the cache directory, and a whole build keeps only its pages' examples"
]

VerificationTest[
    Module[{dir = CreateDirectory[], converter, package, before = Names["Global`*"], off = offGeneralMessages[], result},
        converter = FileNameJoin[{dir, "MarkdownToNotebook.wl"}];
        package = FileNameJoin[{dir, "Namesake.wl"}];
        Export[converter, "converterHelper[x_] := x\nMarkdownToNotebook[file_, out_, ___] := converterHelper[out]\n", "Text"];
        Export[package, "BeginPackage[\"PacletPageNamesake`\"]\nconverterHelper::usage = \"a namesake of the converter's helper\"\nEndPackage[]\n", "Text"];
        On[General::shdw];
        result = Block[{$converterContext = "PacletPageTest`Converter`"},
            With[{f = ReleaseHold[converterLoad[converter]]},
                {Context[f], f["page.md", "page.nb"], Complement[Names["Global`*"], before], Check[Get[package]; "not shadowed", "shadowed", General::shdw]}
            ]
        ];
        matchGeneralMessages[off];
        $ContextPath = DeleteCases[$ContextPath, "PacletPageNamesake`"];
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {"PacletPageTest`Converter`", "page.nb", {}, "not shadowed"},
    TestID -> "Build: a converter file is read into a context of its own, whose helpers no package's namesakes shadow"
]

VerificationTest[
    Module[{dir = CreateDirectory[], file, spec, result},
        file = FileNameJoin[{dir, "Converter.wxf"}];
        PacletPageSaved`convert[x_] := PacletPageSaved`Private`helper[x] + 1;
        PacletPageSaved`Private`helper[x_] := 10 x;
        Export[file, <|"Name" -> "PacletPageSaved`convert", "Definition" -> Language`ExtendedFullDefinition[PacletPageSaved`convert], "ID" -> "saved-1"|>, "WXF"];
        ClearAll[PacletPageSaved`convert, PacletPageSaved`Private`helper];
        spec = converterSpec[File[file]];
        result = {spec["Function"], spec["Function"][2], spec["ID"], spec["Distribute"]};
        ClearAll[PacletPageSaved`convert, PacletPageSaved`Private`helper];
        result = Join[result, {ReleaseHold[spec["Load"]][3], FreeQ[spec["Load"], ResourceFunction | ResourceObject | CloudObject | URLRead | URLFetch]}];
        ClearAll[PacletPageSaved`convert, PacletPageSaved`Private`helper];
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {PacletPageSaved`convert, 21, "saved-1", None, 31, True},
    TestID -> "Build: a converter saved as its definitions is made from its file, in the building kernel and in each kernel that converts pages, with no network"
]

VerificationTest[
    Module[{dir = CreateDirectory[], pinned, result},
        pinned = FileNameJoin[{dir, "Converter.wxf"}];
        Export[pinned, <|"Name" -> "x`f", "Definition" -> {}, "ID" -> "pinned"|>, "WXF"];
        result = {
            Block[{$checkoutDirectory = dir, $pinnedConverter = pinned}, automaticConverter[]],
            Block[{$checkoutDirectory = dir, $pinnedConverter = FileNameJoin[{dir, "none.wxf"}]}, automaticConverter[]]
        };
        DeleteDirectory[dir, DeleteContents -> True];
        result === {File[pinned], $converterURL}
    ],
    True,
    TestID -> "Build: with no converter checkout beside it, PacletPage uses the converter it carries, and the published one only when it carries none"
]

VerificationTest[
    Module[{dir = CreateDirectory[], result},
        PacletPagePrepTest`$parserTried = False;
        PacletPagePrepTest`$parserReady = False;
        PacletPagePrepTest`vendoredParserDir[] := dir;
        PacletPagePrepTest`parserLoadedQ[] := True;
        $parserPreparation["PacletPagePrepTest`"];
        result = {PacletPagePrepTest`$parserTried, PacletPagePrepTest`$parserReady, FreeQ[$parserPreparation, PacletInstall | PacletSiteUpdate | URLRead | URLSubmit]};
        ClearAll["PacletPagePrepTest`*"];
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {True, True, True},
    TestID -> "Build: a kernel that converts pages prepares the converter's parser from what is at hand and marks its attempt made, so no conversion installs anything over the network"
]

VerificationTest[
    MatchQ[Import[$pinnedConverter, "WXF"], KeyValuePattern[{"URL" -> $converterURL, "Name" -> _String, "Definition" -> _Language`DefinitionList, "ID" -> _String}]],
    True,
    TestID -> "Build: this release of PacletPage carries a pinned published converter"
]

VerificationTest[
    docLinks[
        StringJoin[
            "<a href=\"https://r.example/D/guide/Sub.html\">Sub Guide <span class=\"special-character RightGuillemet\">&#187;</span></a>",
            "<a href=\"https://r.example/D/guide/Other.html\">Other Guide\[NonBreakingSpace]\[RightGuillemet]</a>",
            "<a href=\"https://r.example/D/guide/Bare.html\">\[RightGuillemet]</a>",
            "<a href=\"https://r.example/D/tutorial/Quotes.html\">\[LeftGuillemet]Quoted\[RightGuillemet] Words</a>"
        ],
        "https://r.example/D/"
    ],
    {{"guide", "Sub", "Sub Guide"}, {"guide", "Other", "Other Guide"}, {"guide", "Bare", ""}, {"tutorial", "Quotes", "\[LeftGuillemet]Quoted\[RightGuillemet] Words"}},
    TestID -> "Catalog: a page's title is its link's text without the guillemet a sub-guide heading ends in"
]

VerificationTest[
    Needs["PacletResource`"];
    Block[{PacletResource`Notebooks`Private`$packageURLTemplate = "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/`1`.html",
            PacletResource`Notebooks`Private`$pacletName = "Pub/Name", PacletResource`Notebooks`Private`$validRefs = None},
        Cases[
            Cell[BoxData[TemplateBox[{Cell[TextData["Plane Conics \[RightGuillemet]"]], "paclet:Pub/Name/guide/Conics"}, "OrangeLink", BaseStyle -> "GuideFunctionsSubsection"]],
                "GuideFunctionsSubsection"] /. orangeLinkRule[],
            TemplateBox[{text_, {URL[u_], None}, ___}, "HyperlinkTemplate"] :> {text, u}, Infinity]],
    {{Cell[TextData["Plane Conics \[RightGuillemet]"]], "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/guide/Conics.html"}},
    TestID -> "Publish: a sub-guide heading's guillemet stays inside its link on the deployed page, which the site shows"
]

VerificationTest[
    Block[{$ownerURLBase = "https://c.example/obj/me/", $ownerFetch = Function[{url, elements}, Switch[url,
            "https://c.example/obj/me/R/index.html", {200, ToCharacterCode["<html>page</html>", "UTF-8"], {{"Date", "today"}, {"Content-Type", "text/html; charset=utf-8"}}},
            _, {401, {}, ""}]]},
        With[{denied = <|"StatusCode" -> 401, "ContentType" -> "text/html"|>},
            {
                Block[{$readAsOwner = True}, ownerAnswer["https://c.example/obj/me/R/", True, denied]],
                Block[{$readAsOwner = True}, ownerAnswer["https://c.example/obj/me/R/", False, denied]],
                Block[{$readAsOwner = True}, ownerAnswer["https://c.example/obj/me/S/", True, denied]],
                Block[{$readAsOwner = True}, ownerAnswer["https://c.example/obj/other/R/", True, denied]],
                Block[{$readAsOwner = False}, ownerAnswer["https://c.example/obj/me/R/", True, denied]]
            }
        ]
    ],
    {
        <|"StatusCode" -> 200, "ContentType" -> "text/html; charset=utf-8", "Body" -> "<html>page</html>"|>,
        <|"StatusCode" -> 200, "ContentType" -> "text/html; charset=utf-8"|>,
        <|"StatusCode" -> 401, "ContentType" -> "text/html"|>,
        <|"StatusCode" -> 401, "ContentType" -> "text/html"|>,
        <|"StatusCode" -> 401, "ContentType" -> "text/html"|>
    },
    TestID -> "Catalog: a private page of the connected account is read through the session, its directory as index.html; another account's, a check's, and an unread one keep their 401"
]

VerificationTest[
    Module[{lines, value},
        lines = Internal`Bag[];
        value = Block[{$deployMonitor = True, $heartbeatSeconds = 1, $heartbeatPrint = Function[line, Internal`StuffBag[lines, line]]},
            withHeartbeat[Do[Total[RandomReal[1, 10^6]], 1500]; 7, "working"]];
        {value, Count[Internal`BagPart[lines, All], "working: 0 min"] >= 2, FreeQ[Tasks[], "working"], Block[{$deployMonitor = False}, withHeartbeat[1 + 1, "quiet"]]}
    ],
    {7, True, True, 2},
    TestID -> "Deploy: a long step of a monitored deploy prints a line every minute while it runs, and leaves no task behind"
]

VerificationTest[
    Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["deps-"] <> ".nb"}], notebook, results},
        notebook = Notebook[{Cell[TextData[{
            Cell[BoxData[TemplateBox[{Cell[TextData["Manifold"]], "paclet:Pub/Dep/ref/Manifold", "Pub/Dep Package Symbol"}, "PackageLink", BaseStyle -> "InlineFormula"]], "InlineFormula"],
            Cell[BoxData[TemplateBox[{Cell[TextData["Notes"]], "paclet:Pub/Dep/tutorial/Notes#2041"}, "RefLinkPlain", BaseStyle -> "InlineFormula"]], "InlineFormula"],
            Cell[BoxData[TemplateBox[{Cell[TextData["Own"]], "paclet:Pub/Main/ref/Own", "Pub/Main Package Symbol"}, "PackageLink", BaseStyle -> "InlineFormula"]], "InlineFormula"],
            Cell[BoxData[TemplateBox[{Cell[TextData["Plot"]], "paclet:ref/Plot"}, "RefLink", BaseStyle -> "InlineFormula"]], "InlineFormula"]}], "SeeAlso"]}];
        Put[notebook, file];
        results = {dependencyRefsIn[file, {"Pub/Dep"}], dependencyRefs[heldNotebook[file], {"Pub/Dep"}], dependencyRefsIn[file, {}], dependencyRefs[heldNotebook[file], {"Pub/Other"}]};
        DeleteFile[file];
        results
    ],
    {{"paclet:Pub/Dep/ref/Manifold", "paclet:Pub/Dep/tutorial/Notes"}, {"paclet:Pub/Dep/ref/Manifold", "paclet:Pub/Dep/tutorial/Notes"}, {}, {}},
    TestID -> "Publish: a page's links into the paclets it depends on are read from its notebook, without their fragments"
]

VerificationTest[
    Needs["PacletResource`"];
    With[{options = {"PacletName" -> "Pub/Main", "DocumentationHTMLRoot" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Main/Documentation"}},
        Block[{fetchStatuses = Function[urls, Map[Which[
                StringStartsQ[#, "https://c.example/"] && StringContainsQ[#, "/Here.html"], 200,
                ! StringStartsQ[#, "https://c.example/"] && StringContainsQ[#, "/Repo.html"], 200,
                True, 404] &, urls]]},
            With[{table = dependencyLinkTable[{"paclet:Pub/Dep/ref/Here", "paclet:Pub/Dep/ref/Repo", "paclet:Pub/Dep/ref/Gone"}, {"Pub/Dep"}, options]},
                {
                    table["paclet:Pub/Dep/ref/Here"],
                    StringEndsQ[table["paclet:Pub/Dep/ref/Repo"], "Pub/Dep/ref/Repo.html"] && StringStartsQ[table["paclet:Pub/Dep/ref/Repo"], "https://resources.wolframcloud.com/"],
                    table["paclet:Pub/Dep/ref/Gone"]
                }
            ]
        ]
    ],
    {"https://c.example/obj/u/DeployedResources/Paclet/Pub/Dep/Documentation/ref/Here.html", True, None},
    TestID -> "Publish: a link into a dependency leads to its page beside the paclet's own, else on the Paclet Repository, else nowhere"
]

VerificationTest[
    Needs["PacletResource`"];
    Block[{PacletResource`Notebooks`Private`$packageURLTemplate = "https://c.example/obj/u/DeployedResources/Paclet/Pub/Main/Documentation/`1`.html",
            PacletResource`Notebooks`Private`$pacletName = "Pub/Main", PacletResource`Notebooks`Private`$validRefs = None},
        Internal`InheritedBlock[{PacletResource`Notebooks`Private`makeEmbeddedPackageLink},
            installDependencyLinkRule[dependencyLinkRule[<|
                "paclet:Pub/Dep/ref/Manifold" -> "https://c.example/obj/u/DeployedResources/Paclet/Pub/Dep/Documentation/ref/Manifold.html",
                "paclet:Pub/Dep/tutorial/Notes" -> None|>]];
            Map[
                Cases[# /. PacletResource`Notebooks`Private`$embeddedLinkRules, TemplateBox[{_, {URL[u_], None}, ___}, "HyperlinkTemplate"] :> u, Infinity] &,
                {
                    TemplateBox[{Cell[TextData["Manifold"]], "paclet:Pub/Dep/ref/Manifold#12", "Pub/Dep Package Symbol"}, "PackageLink", BaseStyle -> "InlineFormula"],
                    TemplateBox[{Cell[TextData["Notes"]], "paclet:Pub/Dep/tutorial/Notes"}, "RefLinkPlain", BaseStyle -> "InlineFormula"],
                    TemplateBox[{Cell[TextData["Own"]], "paclet:Pub/Main/ref/Own", "Pub/Main Package Symbol"}, "PackageLink", BaseStyle -> "InlineFormula"]
                }
            ]
        ]
    ],
    {
        {"https://c.example/obj/u/DeployedResources/Paclet/Pub/Dep/Documentation/ref/Manifold.html#12"},
        {},
        {"https://c.example/obj/u/DeployedResources/Paclet/Pub/Main/Documentation/ref/Own.html"}
    },
    TestID -> "Publish: a link into a dependency is made a link to its published page, or its label alone; a link into the paclet stays its own"
]



VerificationTest[
    curlLines["2\t000\t\t\n0\t200\ttext/html; charset=UTF-8\t\n2\t302\ttext/html\thttps://b/c\n", 3],
    {{200, "text/html; charset=UTF-8", ""}, {0, "", ""}, {302, "text/html", "https://b/c"}},
    TestID -> "Read: curl's lines, in the order the transfers finished, are read into the requests' order, a later line for a request over an earlier one; no line is status 0"
]

VerificationTest[
    Module[{runs = {}, dir = CreateDirectory[], answers},
        Block[{
            curlExecutable = "curl" &,
            curlRun = Function[arguments, With[{config = ReadString[Last[arguments]]},
                AppendTo[runs, StringCount[config, "url = "]];
                Which[
                    StringContainsQ[config, "\"https://a/closed\""],
                        "0\t302\ttext/html\thttps://www.wolframcloud.com/j_spring_oauth_security_check?statusCode=401\n1\t301\t\thttps://a/moved-to\n2\t200\ttext/plain\t\n",
                    True,
                        "0\t200\ttext/html\t\n"
                ]
            ]]
        },
            answers = curlGet[{"https://a/closed", "https://a/moved", "https://a/plain"}, False, 4]
        ];
        DeleteDirectory[dir, DeleteContents -> True];
        {answers, runs}
    ],
    {{<|"StatusCode" -> 401, "ContentType" -> "text/html"|>, <|"StatusCode" -> 200, "ContentType" -> "text/html"|>, <|"StatusCode" -> 200, "ContentType" -> "text/plain"|>}, {3, 1}},
    TestID -> "Read: a redirect to the cloud's sign-in check is the status it names, as URLRead reads it, and any other redirect is requested again at its target"
]

VerificationTest[
    Module[{dir = CreateDirectory[], result},
        BinaryWrite[FileNameJoin[{dir, "3"}], ByteArray[{97, 169, 98}]]; Close[FileNameJoin[{dir, "3"}]];
        result = fileText[FileNameJoin[{dir, "3"}], "text/html"];
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    "a\[Copyright]b",
    TestID -> "Read: a body with no charset is a character a byte, as URLRead reads it"
]

VerificationTest[
    Module[{dir = CreateDirectory[], file, result},
        file = FileNameJoin[{dir, "body"}];
        BinaryWrite[file, StringToByteArray["\[Copyright] caf\[EAcute]", "UTF-8"]]; Close[file];
        result = {fileText[file, "text/html; charset=utf-8"], fileText[file, "text/html;charset=\"UTF-8\""], StringLength[fileText[file, "text/html"]], fileText[FileNameJoin[{dir, "none"}], "text/html"]};
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {"\[Copyright] caf\[EAcute]", "\[Copyright] caf\[EAcute]", 8, ""},
    TestID -> "Read: a body is decoded by the charset its content type names, and is a character a byte where it names none"
]

VerificationTest[
    Block[{curlExecutable = None &}, {FailureQ[First[curlGet[{"https://example.org/"}, False, 1]]], FailureQ[curlPost["https://example.org/", "r", {}]]}],
    {True, True},
    {PacletPage::nocurl, PacletPage::nocurl},
    TestID -> "Read: with no curl, every answer is a Failure and the reason is said, never a read through the kernel"
]

VerificationTest[
    If[ curlExecutable[] === None, True,
        With[{answers = curlGet[{"http://127.0.0.1:9/a", "http://127.0.0.1:9/b"}, True, 2]}, Length[answers] === 2 && AllTrue[answers, FailureQ]]
    ],
    True,
    TestID -> "Read: a curl that reaches nothing gives each url a Failure, in a process of its own"
]

VerificationTest[
    FreeQ[Join[DownValues[anonymousStatus], DownValues[anonymousBody], DownValues[anonymousStatuses], DownValues[fetchText], DownValues[fetchTexts],
        DownValues[fetchStatuses], DownValues[sentCheck]], URLRead | URLFetch | URLExecute | URLSubmit],
    True,
    TestID -> "Read: the check and the catalog read published pages through curl alone, with no transfer of the kernel's"
]

VerificationTest[
    {sameAccountQ[{"me@x.org", "me@x.org"}, "me@x.org"], sameAccountQ[{"me@x.org", "you@x.org"}, "me@x.org"],
        sameAccountQ[{"me@x.org", None}, "me@x.org"], sameAccountQ[{}, "me@x.org"], sameAccountQ[{"me@x.org"}, None]},
    {True, False, False, False, False},
    TestID -> "Cloud: cloud writes go to the parallel kernels only when every one acts as this kernel's account"
]

probeWrite::srverr = "Cloud server is not able to complete a request.";

(* the test runner takes every message before a message handler sees it, so the handler is called
   here as the message machinery calls it; the stop itself runs in stopOnCloudWriteFailure *)
VerificationTest[
    Block[{$deployStep = "Copying documentation notebooks"},
        {
            With[{failure = Catch[cloudWriteFailure[Hold[Message[probeWrite::srverr], True]], "cloudwrite"]},
                {FailureQ[failure], failure["Step"], failure["CloudMessage"]}],
            With[{failure = Catch[cloudWriteFailure[Hold[Message[probeWrite::srverr], False]], "cloudwrite"]}, FailureQ[failure]],
            cloudWriteFailure[Hold[Message[probeWrite::other], True]]
        }
    ],
    {{True, "Copying documentation notebooks", "probeWrite::srverr"}, True, Null},
    TestID -> "Publish: a failed cloud write, shown or captured, stops the deploy with a failure naming the step"
]

VerificationTest[
    stopOnCloudWriteFailure[PacletResource`Private`setDNCProgress["Deploying HTML"]; "deployed"],
    "deployed",
    TestID -> "Publish: a deploy with no failed write gives its result"
]

VerificationTest[
    cloudRelativePath[
        CloudObject["https://www.wolframcloud.com/obj/u/R/Documentation"],
        CloudObject["https://www.wolframcloud.com/obj/u/R/Documentation/English/Guides/My%20Guide.nb"]
    ],
    FileNameJoin[{"English", "Guides", "My Guide.nb"}],
    TestID -> "Publish: a deployed page is matched to the documentation file it came from"
]

(* the documentation a deploy writes, with the cloud stubbed *)
VerificationTest[
    Module[{dir = CreateDirectory[], write, keys},
        write[name_, uuid_, built_, text_] := Export[FileNameJoin[{dir, name}],
            "(* Content-type: application/vnd.wolfram.mathematica *)\n(*CacheID: " <> ToString[RandomInteger[10^6]] <> "*)\nNotebook[{Cell[\"" <> text <> "\", \"Text\", CellID -> " <> ToString[RandomInteger[10^9]] <> ", ExpressionUUID -> \"" <> uuid <> "\"]}, TaggingRules -> {\"Metadata\" -> {\"built\" -> \"" <> built <> "\", \"title\" -> \"T\"}}]\n", "Text"];
        write["a.nb", "6018d88b-58bd-4b3a-9a41-e01b06eb14aa", "{2026, 5, 27, 1, 26, 16.08}", "Some text"];
        write["b.nb", "16f8fa60-c06d-4452-990a-eee8e93b8841", "{2026, 10, 14, 9, 1, 2.5}", "Some text"];
        write["c.nb", "16f8fa60-c06d-4452-990a-eee8e93b8841", "{2026, 10, 14, 9, 1, 2.5}", "Other text"];
        Export[FileNameJoin[{dir, "d.png"}], "not an image", "Text"];
        keys = contentKey[FileNameJoin[{dir, #}]] & /@ {"a.nb", "b.nb", "c.nb", "d.png"};
        DeleteDirectory[dir, DeleteContents -> True];
        {keys[[1]] === keys[[2]], keys[[2]] === keys[[3]], StringQ[keys[[4]]]}
    ],
    {True, False, True},
    TestID -> "Publish: a notebook's content is the same whatever its cells' ids and the time it was built"
]

VerificationTest[
    changedFiles[
        <|"ref/A.nb" -> "ka", "ref/B.nb" -> "kb", "ref/C.nb" -> "kc", "guide/G.nb" -> "kg"|>,
        <|"ref/A.nb" -> True, "ref/B.nb" -> True, "guide/G.nb" -> True, "ref/Old.nb" -> True|>,
        <|"ref/A.nb" -> "ka", "ref/B.nb" -> "old", "ref/C.nb" -> "kc"|>
    ],
    {"ref/B.nb", "ref/C.nb", "guide/G.nb"},
    TestID -> "Publish: a documentation file is written when its content changed, the cloud lacks it, or the record does not have it"
]

VerificationTest[
    documentationDirectories[{FileNameJoin[{"ref", "A.nb"}], FileNameJoin[{"ref", "B.nb"}], FileNameJoin[{"English", "Guides", "G.nb"}], "top.nb"}],
    {"ref", "English", FileNameJoin[{"English", "Guides"}]},
    TestID -> "Publish: the documentation's directories, each once"
]

VerificationTest[
    Module[{dir = CreateDirectory[], root = CloudObject["https://c.example/obj/u/R"], copied = {}, records = <||>, permissions = {}, run, result},
        CreateDirectory[FileNameJoin[{dir, "ref"}]];
        Export[FileNameJoin[{dir, "ref", #}], "Notebook[{Cell[\"" <> # <> "\", \"Text\"]}]", "Text"] & /@ {"A.nb", "B.nb", "C.nb"};
        run[existing_, failing_] := Block[{
                $pages = newPages[], $deployedDocumentation = None, $rewrite = {},
                cloudFiles = Function[target, existing],
                readDeployRecord = Function[{r, kind}, Lookup[records, kind, <||>]],
                writeDeployRecord = Function[{r, kind, record}, records[kind] = record],
                CopyFile = Function[{from, to, opt}, If[StringEndsQ[from, failing], $Failed, AppendTo[copied, FileNameTake[from]]; to]],
                SetPermissions = Function[{object, p}, AppendTo[permissions, First[object]]; object],
                PacletResource`Private`addEmbeddedOptions = Identity
            },
            Catch[Length[replaceDocumentation[dir, root, "Public"]], "cloudwrite"]
        ];
        result = {
            run[<||>, "none"], copied, Sort[Keys[records["Documentation"]]],
            (copied = {}; run[<|FileNameJoin[{"ref", "A.nb"}] -> True, FileNameJoin[{"ref", "B.nb"}] -> True, FileNameJoin[{"ref", "C.nb"}] -> True|>, "none"]), copied,
            (Export[FileNameJoin[{dir, "ref", "B.nb"}], "Notebook[{Cell[\"changed\", \"Text\"]}]", "Text"]; copied = {};
             FailureQ[run[<|FileNameJoin[{"ref", "A.nb"}] -> True, FileNameJoin[{"ref", "B.nb"}] -> True|>, "C.nb"]]), copied,
            Sort[Keys[records["Documentation"]]],
            DeleteDuplicates[permissions]
        };
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {
        3, {"A.nb", "B.nb", "C.nb"}, Sort[{FileNameJoin[{"ref", "A.nb"}], FileNameJoin[{"ref", "B.nb"}], FileNameJoin[{"ref", "C.nb"}]}],
        3, {},
        True, {"B.nb"},
        Sort[{FileNameJoin[{"ref", "A.nb"}], FileNameJoin[{"ref", "B.nb"}]}],
        {"https://c.example/obj/u/R/Documentation", "https://c.example/obj/u/R/Documentation/ref"}
    },
    TestID -> "Publish: a deploy writes only the documentation that changed or the cloud lacks, and records only what it wrote"
]

VerificationTest[
    Module[{made = {}, run, signatures},
        run[existing_, keys_, sidebar_, recorded_] := Block[{
                $pages = Join[newPages[], <|"Root" -> CloudObject["https://c.example/obj/u/R"], "Existing" -> existing, "Keys" -> keys, "Signatures" -> recorded, "Recorded" -> recorded|>],
                PacletResource`Private`createDocumentationEmbedPage = Function[{as, side, url}, AppendTo[made, FileBaseName[url]]; CloudObject[url]],
                PacletResource`Private`shingleTemplate = Function[name, TemplateObject[{"<title>", TemplateSlot["Name"], "</title>", TemplateSlot["NavSidebar"]}]],
                PacletResource`Private`$cloudObjectOpts = {}
            },
            made = {};
            Scan[makePage[<|"Name" -> "R", "UUID" -> CreateUUID[]|>, sidebar, "https://c.example/obj/u/R/Documentation/ref/" <> # <> ".html"] &, {"A", "B"}];
            {made, $pages["Signatures"]}
        ];
        With[{
            all = <|FileNameJoin[{"ref", "A.nb"}] -> True, FileNameJoin[{"ref", "A.html"}] -> True, FileNameJoin[{"ref", "B.nb"}] -> True, FileNameJoin[{"ref", "B.html"}] -> True|>,
            keys = <|FileNameJoin[{"ref", "A.nb"}] -> "ka", FileNameJoin[{"ref", "B.nb"}] -> "kb"|>},
            signatures = Last[run[all, keys, "<nav/>", <||>]];
            {
                First[run[all, keys, "<nav/>", <||>]],
                First[run[all, keys, "<nav/>", signatures]],
                First[run[all, Append[keys, FileNameJoin[{"ref", "B.nb"}] -> "kb2"], "<nav/>", signatures]],
                First[run[KeyDrop[all, FileNameJoin[{"ref", "A.html"}]], keys, "<nav/>", signatures]],
                First[run[all, keys, "<nav>new page</nav>", signatures]]
            }
        ]
    ],
    {{"A", "B"}, {}, {"B"}, {"A"}, {"A", "B"}},
    TestID -> "Publish: a page is made again when its notebook, the sidebar or its template's values changed, or the cloud lacks it"
]

VerificationTest[
    Module[{made = {}, queued},
        Block[{
                $pages = Join[newPages[], <|"Root" -> CloudObject["https://c.example/obj/u/R"]|>],
                $pageQueue = {},
                parallelSameAccountQ = Function[False],
                writeDeployRecord = Function[{r, kind, record}, Null],
                PacletResource`Private`createDocumentationEmbedPage = Function[{as, side, url},
                    AppendTo[made, FileBaseName[url]]; If[StringContainsQ[url, "Bad"], $Failed, CloudObject[url]]],
                PacletResource`Private`shingleTemplate = Function[name, TemplateObject[{"<title>", TemplateSlot["Name"], "</title>"}]],
                PacletResource`Private`$cloudObjectOpts = {}
            },
            Scan[makePage[<|"Name" -> "R"|>, "<nav/>", "https://c.example/obj/u/R/Documentation/ref/" <> # <> ".html"] &, {"A", "Bad", "C"}];
            queued = made;
            makeQueuedPages[];
            {queued, made, Sort[FileBaseName /@ Keys[$pages["Signatures"]]], $pages["Made"], $pageQueue}
        ]
    ],
    {{}, {"A", "Bad", "C"}, {"A", "C"}, 2, {}},
    TestID -> "Publish: the pages the sidebar asks for are made once it is done, and a page not made keeps no signature"
]

VerificationTest[
    Module[{kernels = LaunchKernels[1], here, page, run, urls},
        (* a page's deploy: the stock delayed page of a template, deployed by a stub; the stock
           package is loaded here, as it is in a deploy, whose sidebar asks for the pages *)
        Needs["PacletResource`"];
        page[name_String] := Function[{as, side, url},
            PacletResource`Private`deployDelayedEmbedTemplate[url, TemplateObject[{"<p>", TemplateSlot["Name"], "</p>", side}], <|"Name" -> name|>, url <> "?static", {}]];
        urls = "https://c.example/obj/u/R/Documentation/ref/" <> # <> ".html" & /@ {"A", "B", "C"};
        run[there_] := (
            With[{make = there}, ParallelEvaluate[
                Needs["PacletResource`"];
                PacletResource`Private`createDocumentationEmbedPage = make;
                PacletResource`Private`deployDelayedEmbedTemplate = Function[{target, t, p, s, o}, CloudObject[target]];
                Null,
                kernels
            ]];
            here = {};
            Block[{
                    $pages = Join[newPages[], <|"Root" -> CloudObject["https://c.example/obj/u/R"]|>],
                    parallelSameAccountQ = Function[True],
                    writeDeployRecord = Function[{r, kind, record}, Null],
                    PacletResource`Private`createDocumentationEmbedPage = Function[{as, side, url}, AppendTo[here, FileBaseName[url]]; page["R"][as, side, url]],
                    PacletResource`Private`deployDelayedEmbedTemplate = Function[{target, t, p, s, o}, CloudObject[target]],
                    PacletResource`Private`$cloudObjectOpts = {}
                },
                makePages[<|"Name" -> "R"|>, "<nav/>", Transpose[{urls, {"sa", "sb", "sc"}}]];
                {here, FileBaseName /@ Keys[$pages["Signatures"]], $pages["Made"]}
            ]
        );
        WithCleanup[
            {run[page["R"]], run[page["Other"]]},
            CloseKernels[kernels]
        ]
    ],
    {{{"A"}, {"A", "B", "C"}, 3}, {{"A", "A", "B", "C"}, {"A", "B", "C"}, 3}},
    TestID -> "Publish: the pages are made on the parallel kernels when each makes a page's deploy as this kernel does, else here"
]

VerificationTest[
    Module[{writes = {}},
        Block[{writeDeployRecord = Function[{r, kind, record}, AppendTo[writes, record]]},
            Block[{$pages = Join[newPages[], <|"Root" -> CloudObject["https://c.example/obj/u/R"], "Signatures" -> <|"a" -> 1, "gone" -> 2|>, "Recorded" -> <|"a" -> 1, "gone" -> 2|>, "Seen" -> {"a"}|>]},
                savePages[False]; savePages[True]
            ];
            Block[{$pages = Join[newPages[], <|"Root" -> CloudObject["https://c.example/obj/u/R"], "Signatures" -> <|"a" -> 1|>, "Recorded" -> <|"a" -> 1|>, "Seen" -> {"a"}|>]},
                savePages[True]
            ]
        ];
        writes
    ],
    {<|"a" -> 1|>},
    TestID -> "Publish: the pages' record drops a page no longer shown once the deploy is complete, and is written only when it changed"
]

VerificationTest[
    Module[{deleted = {}},
        Block[{
                DeleteObject = Function[object, AppendTo[deleted, First[object]]; Null],
                $deployedDocumentation = <|"Target" -> CloudObject["https://c.example/obj/u/R/Documentation"],
                    "Files" -> {FileNameJoin[{"ref", "A.nb"}]},
                    "Existing" -> {FileNameJoin[{"ref", "A.nb"}], FileNameJoin[{"ref", "A.html"}], FileNameJoin[{"ref", "Gone.nb"}], FileNameJoin[{"ref", "Gone.html"}]}|>
            },
            {pruneDocumentation[], deleted}
        ]
    ],
    {2, {"https://c.example/obj/u/R/Documentation/ref/Gone.nb", "https://c.example/obj/u/R/Documentation/ref/Gone.html"}},
    TestID -> "Publish: the notebooks the new documentation lacks, and their pages, are deleted once the deploy is complete"
]

VerificationTest[
    {rewriteKinds[Automatic], rewriteKinds[All], rewriteKinds["Pages"], rewriteKinds[{"Documentation"}]},
    {{}, {"Documentation", "Pages"}, {"Pages"}, {"Documentation"}},
    TestID -> "Publish: \"Upload\" names what a deploy writes and makes again whatever its record says"
]

VerificationTest[
    Module[{f, flag},
        f[x_] := "stock";
        installRules[f, {HoldPattern[f[x_] /; TrueQ[flag]] :> "patched"}];
        {Block[{flag = True}, f[1]], f[1], installRules[f, {}]}
    ],
    {"patched", "stock", False},
    TestID -> "Publish: rules placed before a function's own apply first, and only where their condition holds"
]

(* the page transform, with the stock transform stubbed *)
VerificationTest[
    Module[{dir = CreateDirectory[], write, keys, valid, options},
        write[name_, uuid_, link_] := Export[FileNameJoin[{dir, name}],
            "Notebook[{Cell[BoxData[TemplateBox[{\"F\", \"" <> link <> "\"}, \"RefLink\"]], \"Text\", ExpressionUUID -> \"" <> uuid <> "\"]}]", "Text"];
        write["a.nb", "6018d88b-58bd-4b3a-9a41-e01b06eb14aa", "paclet:Pub/Sym/ref/F"];
        write["b.nb", "16f8fa60-c06d-4452-990a-eee8e93b8841", "paclet:Pub/Sym/ref/F"];
        write["c.nb", "16f8fa60-c06d-4452-990a-eee8e93b8841", "paclet:Other/ref/F"];
        options = {OverwriteTarget -> True, "PacletName" -> "Pub/Sym"};
        Block[{$transform = <|"Context" -> {}|>},
            keys = {
                transformKey[FileNameJoin[{dir, "a.nb"}], Automatic, options, <||>],
                transformKey[FileNameJoin[{dir, "b.nb"}], Automatic, options, <||>],
                transformKey[FileNameJoin[{dir, "b.nb"}], Automatic, options, <|"paclet:Pub/Sym/ref/F" -> True|>],
                transformKey[FileNameJoin[{dir, "c.nb"}], Automatic, options, <||>],
                transformKey[FileNameJoin[{dir, "c.nb"}], Automatic, options, <|"paclet:Other/ref/F" -> True|>]
            }
        ];
        DeleteDirectory[dir, DeleteContents -> True];
        {keys[[1]] === keys[[2]], keys[[2]] === keys[[3]], keys[[4]] === keys[[5]]}
    ],
    {True, False, True},
    TestID -> "Transform: a page's key ignores its cells' ids, and follows whether a page is there for each of its links into the paclet"
]

VerificationTest[
    Module[{dir = CreateDirectory[], cache, transformed = {}, run, result},
        cache = FileNameJoin[{dir, "cache"}];
        CreateDirectory[FileNameJoin[{dir, "docs"}]];
        Export[FileNameJoin[{dir, "docs", #}], "Notebook[{Cell[\"" <> # <> "\", \"Text\"]}]", "Text"] & /@ {"A.nb", "B.nb"};
        run[] := Block[{
                $transform = newTransform[cache, {}, {}, {}, {}],
                $pageTransform = Function[{file, type, options}, AppendTo[transformed, FileNameTake[file]]; Export[file, "Notebook[{Cell[\"made\", \"Text\"]}]", "Text"]]
            },
            transformed = {};
            {Map[FileNameTake, transformPages[FileNameJoin[{dir, "docs"}], Automatic, {OverwriteTarget -> True, "PackageURIs" -> {}}]], transformed, $transform["Cached"], $transform["Used"]}
        ];
        result = {run[], (Export[FileNameJoin[{dir, "docs", #}], "Notebook[{Cell[\"" <> # <> "\", \"Text\"]}]", "Text"] & /@ {"A.nb", "B.nb"}; run[])};
        result = {result[[1, 1]], result[[1, 2]], result[[2, 2]], result[[2, 3]], Import[FileNameJoin[{dir, "docs", "A.nb"}], "Text"], Length[FileNames["*.nb", cache]]};
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {{"A.nb", "B.nb"}, {"A.nb", "B.nb"}, {}, 2, "Notebook[{Cell[\"made\", \"Text\"]}]", 2},
    TestID -> "Transform: a page transformed before is taken from build/transform, the same as the transform made it"
]

VerificationTest[
    Module[{dir = CreateDirectory[], first, again, edited},
        CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := x", "Text"];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.0\"|>]", "Text"];
        first = sourceHash[dir, Automatic, <||>];
        CreateDirectory[FileNameJoin[{dir, "build"}]];
        Export[FileNameJoin[{dir, "build", "anything.wl"}], "1", "Text"];
        Export[FileNameJoin[{dir, ".hidden"}], "1", "Text"];
        again = sourceHash[dir, Automatic, <||>];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := 2 x", "Text"];
        edited = sourceHash[dir, Automatic, <||>];
        DeleteDirectory[dir, DeleteContents -> True];
        {first === again, first === edited}
    ],
    {True, False},
    TestID -> "Archive: the sources' hash leaves out build/ and hidden files, and follows every other file"
]

VerificationTest[
    Module[{dir = CreateDirectory[], archive, result},
        CreateDirectory[FileNameJoin[{dir, "build"}]];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.0\"|>]", "Text"];
        archive = FileNameJoin[{dir, "build", "Pub__Sym-1.0.0.paclet"}];
        Export[archive, "not really an archive", "Text"];
        Put[<|"Archive" -> FileNameTake[archive], "ArchiveHash" -> FileHash[archive, "SHA256", "HexString"], "Source" -> sourceHash[dir, Automatic, <||>], "Result" -> <|"Pages" -> 3|>|>,
            FileNameJoin[{dir, "build", "archive.wl"}]];
        result = {builtArchive[DeployPacletResource, dir, "Pub/Sym", Automatic, <||>]};
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.1\"|>]", "Text"];
        AppendTo[result, builtArchive[DeployPacletResource, dir, "Pub/Sym", Automatic, <||>]];
        DeleteDirectory[dir, DeleteContents -> True];
        {result[[1]]["Pages"], FileNameTake[result[[1]]["Archive"]], result[[2]]}
    ],
    {3, "Pub__Sym-1.0.0.paclet", $Failed},
    {DeployPacletResource::nobuild},
    TestID -> "Archive: a deploy takes the archive an earlier build made, only from the same sources"
]

VerificationTest[
    Module[{dir = CreateDirectory[], archive, stamp, result},
        CreateDirectory[FileNameJoin[{dir, "build"}]];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.0\"|>]", "Text"];
        archive = FileNameJoin[{dir, "build", "Pub__Sym-1.0.0.paclet"}];
        Export[archive, "not really an archive", "Text"];
        stamp[builder_] := Put[<|"Archive" -> FileNameTake[archive], "ArchiveHash" -> FileHash[archive, "SHA256", "HexString"], "Source" -> sourceHash[dir, Automatic, <||>],
            "Builder" -> builder, "Result" -> <|"Pages" -> 3|>|>, FileNameJoin[{dir, "build", "archive.wl"}]];
        stamp[builderKey[]];
        result = {recordedArchive[dir, Automatic, <||>, builderKey[]]};
        (* another builder's archive, or one recorded before builders were *)
        stamp["another PacletPage"];
        AppendTo[result, recordedArchive[dir, Automatic, <||>, builderKey[]]];
        AppendTo[result, recordedArchive[dir, Automatic, <||>, All]];
        stamp[builderKey[]];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.1\"|>]", "Text"];
        AppendTo[result, recordedArchive[dir, Automatic, <||>, builderKey[]]];
        DeleteDirectory[dir, DeleteContents -> True];
        {result[[1]]["Pages"], result[[2]], result[[3]]["Pages"], result[[4]], StringQ[builderKey[]], Lookup[Options[DeployPacletResource], "Build"]}
    ],
    {3, None, 3, None, True, True},
    TestID -> "Archive: with \"Build\" -> Automatic a deploy takes the recorded archive only from the same sources and the same builder"
]

VerificationTest[
    Module[{section = guideTreeSection[
            {<|"URL" -> "https://c.example/D/guide/Main.html", "Title" -> "Main"|>, <|"URL" -> "https://c.example/D/guide/A.html", "Title" -> "Area A"|>,
             <|"URL" -> "https://c.example/D/guide/B.html", "Title" -> "Area B"|>, <|"URL" -> "https://c.example/D/guide/Lone.html", "Title" -> "Lone"|>},
            <|"Main" -> {"A"}, "A" -> {"B", "Main"}, "B" -> {"A"}, "Lone" -> {}|>, "Main"]},
        {
            section["Label"],
            Cases[section["Links"], XMLElement["summary", {}, {XMLElement["a", _, {title_}]}] :> title, Infinity],
            Cases[section["Links"], XMLElement["a", {"href" -> href_}, _] :> FileBaseName[href], Infinity]
        }
    ],
    {"Guides", {"Area A"}, {"A", "B", "Lone"}},
    TestID -> "Publish: the pages' sidebar lists the guides as a tree under the root guide, cycles cut, the guides it misses after"
]

VerificationTest[
    {guideTreeRules[<|"Main" -> {}|>, "Main"], Length[guideTreeRules[<|"Main" -> {"A"}, "A" -> {}|>, "Main"]]},
    {{}, 1},
    TestID -> "Publish: a paclet with one guide keeps the stock list of guides"
]

VerificationTest[
    {failureBudget[0, 500], failureBudget[Automatic, 120], failureBudget[Automatic, 1200], failureBudget[5, 10], failureBudget[Infinity, 10]},
    {0, 3, 12, 5, Infinity},
    TestID -> "Archive: the pages an archive may lack: none by default, Automatic three or a hundredth of them"
]

(* the site rendered on this computer *)
localFixture[] := Module[{dir = CreateDirectory[], put},
    put[path_List, nb_] := With[{file = FileNameJoin[Join[{dir, "Documentation", "English"}, path]]},
        If[! DirectoryQ[DirectoryName[file]], CreateDirectory[DirectoryName[file], CreateIntermediateDirectories -> True]];
        Export[file, nb, "NB"]
    ];
    Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Name\", \"Version\" -> \"1.0.0\"|>]", "Text"];
    put[{"Guides", "Name.nb"}, Notebook[{Cell["Name Guide", "GuideTitle"], Cell[TextData[ButtonBox["Sub", BaseStyle -> "Link", ButtonData -> "paclet:Pub/Name/guide/Sub"]], "GuideFunctionsSection"]}]];
    put[{"Guides", "Sub.nb"}, Notebook[{Cell["Sub Guide", "GuideTitle"]}]];
    put[{"ReferencePages", "Symbols", "F.nb"}, Notebook[{Cell["F", "ObjectName"]}]];
    put[{"Tutorials", "T.nb"}, Notebook[{Cell["A Tutorial", "Title"], Cell["More About", "TutorialMoreAboutSection"]}]];
    dir
];

VerificationTest[
    Module[{dir = localFixture[], catalog},
        catalog = PacletPageCatalog[File[dir]];
        WithCleanup[
            {KeyTake[catalog, {"Name", "Kind", "RootGuide", "Version"}], Sort[Keys[catalog["Pages"]]],
                FileNameTake[catalog["Pages"]["ref/F"]["File"], -3], catalog["Pages"]["guide/Name"]["Guides"],
                FailureQ[PacletPageCatalog[File[$TemporaryDirectory]]]},
            DeleteDirectory[dir, DeleteContents -> True]]
    ],
    {<|"Name" -> "Pub/Name", "Kind" -> "Local", "RootGuide" -> "guide/Name", "Version" -> "1.0.0"|>,
        {"guide/Name", "guide/Sub", "ref/F", "tutorial/T"}, FileNameJoin[{"ReferencePages", "Symbols", "F.nb"}], {"guide/Sub"}, True},
    TestID -> "Catalog: a paclet directory's catalog is read from its documentation notebooks alone, each page with its notebook's file"
]

VerificationTest[
    Module[{dir = localFixture[], main, symbol, filtered, noSym, controlsAll, controlsSub, controlsNoHdr,
            xmlText, anchors, safeTags, allTags, loaded},
        xmlText[nodes_List] := StringJoin[Cases[nodes, s_String, Infinity]];
        anchors[expr_] := Cases[expr, XMLElement["a", attrs_List, kids_List] /; KeyExistsQ[Association[attrs], "data-id"] :>
            With[{a = Association[attrs]}, {xmlText[kids], a["href"], StringContainsQ[Lookup[a, "class", ""], "active"]}], Infinity];
        safeTags = {"div", "header", "nav", "main", "footer", "a", "span", "ul", "li", "details", "summary", "wbr",
            "input", "button", "svg", "rect", "path"};
        main = PacletPage[dir];
        symbol = PacletPage[dir, "paclet:Pub/Name/ref/F"];
        filtered = PacletPage[dir, "Filter" -> "tutorial"];
        noSym = PacletPage[dir, "SymbolsRail" -> False];
        controlsAll = PacletPage[dir, "LayoutControls" -> True];
        controlsSub = PacletPage[dir, "LayoutControls" -> {"LeftSidebar", "Fullscreen"}];
        controlsNoHdr = PacletPage[dir, "Header" -> False, "Footer" -> False, "LayoutControls" -> True];
        allTags = DeleteDuplicates[Cases[{main, controlsAll, controlsNoHdr}, XMLElement[t_String, _, _] :> t, Infinity]];
        PacletDirectoryLoad[dir];
        loaded = WithCleanup[
            {
                PacletPage["Pub/Name", "F"] === symbol,
                PacletPage["paclet:Pub/Name/ref/F#section"] === symbol,
                PacletPage["paclet:Pub/Name"] === main,
                PacletPage[First[PacletFind["Pub/Name"]], "F"] === symbol,
                PacletPage[File[dir], "F"] === symbol,
                PacletPage[PacletPageCatalog[File[dir]], "F"] === symbol
            },
            PacletDirectoryUnload[dir]
        ];
        WithCleanup[
            {
                MatchQ[main, XMLElement["div", {___, "class" -> "pp-shell", "data-paclet-page" -> "Pub/Name", "data-paclet-id" -> "guide/Name", "data-paclet-title" -> "Name Guide", ___}, _List]],
                anchors[main],
                Cases[main, XMLElement["div", {___, "class" -> "pp-nav-label", ___}, {label_String}] :> label, Infinity],
                FirstCase[main, XMLElement["main", _, {___, nb_Notebook, ___}] :> FirstCase[nb, Cell[c_, "GuideTitle", ___] :> c, None, Infinity], None, Infinity],
                FirstCase[symbol, XMLElement["main", _, {___, nb_Notebook, ___}] :> FirstCase[nb, Cell[c_, "ObjectName", ___] :> c, None, Infinity], None, Infinity],
                symbol === PacletPage[dir, "F"] === PacletPage[dir, "ref/F"],
                loaded,
                First /@ anchors[filtered],
                {
                    StringContainsQ[FirstCase[noSym, XMLElement["div", attrs_List, _] :> Lookup[Association[attrs], "class", ""], "", {0}], "pp-no-symbols"],
                    Cases[noSym, XMLElement["nav", {___, "class" -> "pp-side-right", ___}, _], Infinity] === {},
                    MemberQ[First /@ anchors[FirstCase[noSym, XMLElement["nav", {___, "class" -> "pp-side", ___}, _], None, Infinity]], "F"]
                },
                {
                    FirstCase[main, XMLElement["div", {___, "data-pp-layout-controls" -> v_, ___}, _] :> v, None, Infinity] === "left,right,fullscreen",
                    FirstCase[controlsAll, XMLElement["div", {___, "data-pp-layout-controls" -> v_, ___}, _] :> v, None, Infinity],
                    FirstCase[controlsSub, XMLElement["div", {___, "data-pp-layout-controls" -> v_, ___}, _] :> v, None, Infinity],
                    FirstCase[PacletPage[dir, "SymbolsRail" -> False, "LayoutControls" -> True], XMLElement["div", {___, "data-pp-layout-controls" -> v_, ___}, _] :> v, None, Infinity],
                    Cases[controlsNoHdr, XMLElement["header" | "footer", _, _], Infinity] === {},
                    FirstCase[controlsNoHdr, XMLElement["div", {___, "class" -> c_, ___, "data-pp-layout-controls" -> v_, ___}, _] :> {c, v}, None, Infinity]
                },
                SubsetQ[safeTags, allTags],
                {
                    MatchQ[PacletPage[dir, "ref/Nope"], Failure["PacletPageNotFound", _]],
                    MatchQ[PacletPage["NoSuchPublisher/NoSuchPaclet"], Failure["PacletNotFound", _]],
                    MatchQ[PacletPage[dir, "LayoutControls" -> "Bad"], Failure["InvalidOption", _]]
                }
            },
            DeleteDirectory[dir, DeleteContents -> True]]
    ],
    {True,
        {{"Name Guide", "paclet:Pub/Name/guide/Name", True}, {"Sub Guide", "paclet:Pub/Name/guide/Sub", False},
            {"A Tutorial", "paclet:Pub/Name/tutorial/T", False}, {"F", "paclet:Pub/Name/ref/F", False}},
        {"Guides", "Tutorials", "Symbols"}, "Name Guide", "F", True,
        {True, True, True, True, True, True},
        {"A Tutorial"},
        {True, True, True},
        {True, "left,right,fullscreen", "left,fullscreen", "left,fullscreen", True, {"pp-layout-controls pp-layout-controls-bar", "left,right,fullscreen"}},
        True,
        {True, True, True}},
    TestID -> "Page: a page of a local or installed paclet as the three-column XMLElement shell with its live notebook and optional layout controls"
]

VerificationTest[
    Module[{dir = localFixture[], catalog, html, expr, accentExpr, noFilter, customVer, noVer, noRoot},
        catalog = PacletPageCatalog[File[dir]];
        html = PacletPageFiles[catalog, "PageURL" -> "", "LayoutControls" -> True]["index.html"];
        expr = PacletPage[dir, "LayoutControls" -> True];
        accentExpr = PacletPage[dir, "Accent" -> "#1a66cc"];
        noFilter = PacletPage[dir, "Filter" -> False];
        customVer = PacletPage[dir, "VersionLabel" -> "preview"];
        noVer = PacletPage[dir, "VersionLabel" -> False];
        noRoot = PacletPage[dir, "RootGuide" -> False];
        WithCleanup[
            {
                (* Unified shell parity: identical page data-ids (HTML streams #side-right first), native filter input, splitters, and layout buttons across HTML site and WL expression *)
                Sort[StringCases[html, "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id]] ===
                    Sort[Cases[expr, XMLElement["a", {___, "data-id" -> id_, ___}, _] :> id, Infinity]],
                Cases[expr, XMLElement["button", {___, "data-pp-layout" -> k_, ___}, _] :> k, Infinity] ===
                    {"left", "right", "fullscreen", "fullscreen-exit"},
                ! FreeQ[expr, XMLElement["input", {___, "class" -> "pp-filter", ___}, {}]] &&
                    ! FreeQ[expr, XMLElement["div", {___, "class" -> "pp-split pp-split-left", ___}, {}]] &&
                    ! FreeQ[expr, XMLElement["div", {___, "class" -> "pp-split pp-split-right", ___}, {}]],
                (* Accent sets custom CSS variables on the shell *)
                StringContainsQ[FirstCase[accentExpr, XMLElement["div", attrs_List, _] :> Lookup[Association[attrs], "style", ""], "", {0}], "--pp-accent: #1a66cc;"],
                (* Filter -> False omits the filter slot *)
                Cases[noFilter, XMLElement["div", {___, "class" -> "pp-filter-slot", ___}, _], Infinity] === {},
                (* VersionLabel custom and False *)
                FirstCase[customVer, XMLElement["span", {___, "class" -> "pp-ver", ___}, {v_String}] :> v, None, Infinity],
                Cases[noVer, XMLElement["span", {___, "class" -> "pp-ver", ___}, _], Infinity] === {},
                (* RootGuide -> False omits root link from left rail while keeping children *)
                Cases[noRoot, XMLElement["a", {___, "data-id" -> id_, ___}, _] :> id, Infinity],
                (* Invalid Accent returns InvalidOption Failure *)
                MatchQ[PacletPage[dir, "Accent" -> "not-a-color"], Failure["InvalidOption", _]]
            },
            DeleteDirectory[dir, DeleteContents -> True]
        ]
    ],
    {True, True, True, True, True, "preview", True, {"guide/Sub", "tutorial/T", "ref/F"}, True},
    TestID -> "Shell: static HTML site and PacletPage XMLElement share one shell builder and option parity"
]

VerificationTest[
    Module[{dir = localFixture[], built, srcMd, fbMail, fbURL, noFbAuto, noTools, withRes},
        CreateDirectory[FileNameJoin[{dir, "docs", "Symbols"}], CreateIntermediateDirectories -> True];
        Export[FileNameJoin[{dir, "docs", "Symbols", "F.md"}], "---\nName: F\n---\n\n`F[x]` docs source.\n", "Text"];
        built = PacletPage[dir, "F"];
        srcMd = PacletPage[dir, "F", "View" -> "Source"];
        fbMail = PacletPage[dir, "F", "FeedbackURL" -> "", "FeedbackEmail" -> "docs@example.org"];
        fbURL = PacletPage[dir, "F", "FeedbackURL" -> "https://c.example/obj/u/SiteFeedback/Form"];
        noFbAuto = PacletPage[dir, "F", "FeedbackURL" -> ""];
        noTools = PacletPage[dir, "F", "Feedback" -> False, "ViewSource" -> False];
        withRes = PacletPage[dir, "F", "ResourceLink" -> "https://c.example/obj/u/Resource"];
        WithCleanup[
            {
                (* Default built view includes Feedback (with deduced URL) and View source in .pp-pagetools *)
                Cases[built, XMLElement[_, {___, "data-pp-tool" -> t_, ___}, {lbl_String}] :> {t, lbl}, Infinity],
                FirstCase[built, XMLElement["a", attrs_List, {"Feedback"}] /; MemberQ[attrs, "data-pp-tool" -> "feedback"] :>
                    With[{a = Association[attrs]}, {a["href"], a["data-pp-feedback-url"], a["data-pp-all-feedback"]}], None, Infinity],
                (* Source view renders the docs/Symbols/F.md source and flips button to View built page *)
                {
                    FirstCase[srcMd, XMLElement["button", {___, "data-pp-tool" -> "viewtoggle", ___, "data-view" -> v_, ___}, {lbl_String}] :> {v, lbl}, None, Infinity],
                    FirstCase[srcMd, XMLElement["main", _, {___, nb_Notebook, ___}] :> StringContainsQ[FirstCase[nb, Cell[s_String, "Program", ___] :> s, "", Infinity], "`F[x]` docs source."], False, Infinity]
                },
                (* FeedbackEmail without FeedbackURL produces RFC 6068 mailto: href and data-pp-feedback-to *)
                FirstCase[fbMail, XMLElement["a", attrs_List, {"Feedback"}] /; MemberQ[attrs, "data-pp-tool" -> "feedback"] :>
                    With[{a = Association[attrs]}, {StringStartsQ[a["href"], "mailto:docs@example.org?subject="], a["data-pp-feedback-to"]}], None, Infinity],
                (* Explicit FeedbackURL overrides deduced URL and sets All feedback link *)
                FirstCase[fbURL, XMLElement["a", attrs_List, {"Feedback"}] /; MemberQ[attrs, "data-pp-tool" -> "feedback"] :>
                    With[{a = Association[attrs]}, {a["href"], a["data-pp-all-feedback"]}], None, Infinity],
                (* Feedback -> Automatic with FeedbackURL -> "" and no email omits the Feedback button *)
                Cases[noFbAuto, XMLElement[_, {___, "data-pp-tool" -> t_, ___}, _] :> t, Infinity],
                (* Feedback -> False and ViewSource -> False omit .pp-pagetools *)
                Cases[noTools, XMLElement["div", {___, "class" -> "pp-pagetools", ___}, _], Infinity] === {},
                (* ResourceLink emits .pp-home link in header *)
                FirstCase[withRes, XMLElement["a", {___, "class" -> "pp-home", ___, "href" -> u_, ___}, _] :> u, None, Infinity],
                (* Invalid View / FeedbackEmail return InvalidOption Failure *)
                {
                    MatchQ[PacletPage[dir, "F", "View" -> "Bogus"], Failure["InvalidOption", _]],
                    MatchQ[PacletPage[dir, "F", "FeedbackEmail" -> 123], Failure["InvalidOption", _]]
                }
            },
            DeleteDirectory[dir, DeleteContents -> True]
        ]
    ],
    {
        {{"feedback", "Feedback"}, {"viewtoggle", "View source"}},
        {
            "https://www.wolframcloud.com/obj/pub/NameFeedback/Form",
            "https://www.wolframcloud.com/obj/pub/NameFeedback/Form",
            "https://www.wolframcloud.com/obj/pub/NameFeedback/All"
        },
        {{"source", "View built page"}, True},
        {True, "docs@example.org"},
        {"https://c.example/obj/u/SiteFeedback/Form", "https://c.example/obj/u/SiteFeedback/All"},
        {"viewtoggle"},
        True,
        "https://c.example/obj/u/Resource",
        {True, True}
    },
    TestID -> "Page: Feedback, View source/View built page, and ResourceLink match page.html and site.html"
]

VerificationTest[
    Module[{attFile, calls = {}, res1, res2, resCustom, res500, resNoThanks, attTmpDir},
        attFile = CreateFile[];
        Export[attFile, "sample note", "Text"];
        WithCleanup[
            {
                (* Default URL deduction from paclet name, URI, PublisherID, SiteURL, FeedbackURL, and missing publisher *)
                {
                    defaultFeedbackEndpoints["WolframInstitute/PacletPage"][["Form"]],
                    defaultFeedbackEndpoints["WolframInstitute/PacletPage"][["All"]],
                    defaultFeedbackEndpoints["paclet:WolframInstitute/TuringMachine/ref/Foo"][["Form"]],
                    defaultFeedbackEndpoints[<|"Name" -> "Tool", "PublisherID" -> "MyOrg"|>][["Form"]],
                    defaultFeedbackEndpoints[<|"Name" -> "Pub/Tool", "SiteURL" -> "https://c.example/obj/u/CustomSite"|>][["Form"]],
                    defaultFeedbackEndpoints[<|"Name" -> "Pub/Tool", "FeedbackURL" -> "https://c.example/obj/u/CustomFeedback/Form"|>][["Form"]],
                    defaultFeedbackEndpoints["BareNameWithoutPublisher"]
                },
                (* SubmitPacletPageFeedback posts expected multipart fields and cleans up inline attachment temp files *)
                Block[{curlPost},
                    curlPost[url_, referer_, fields_] := (
                        AppendTo[calls, <|
                            "URL" -> url,
                            "Referer" -> referer,
                            "Fields" -> Association[fields],
                            "Att2Exists" -> With[{f = Lookup[Association[fields], "Attachment2", None]},
                                If[MatchQ[f, File[_String]], attTmpDir = DirectoryName[First[f]]; FileExistsQ[First[f]], False]
                            ]
                        |>];
                        <|"StatusCode" -> 200, "Body" -> "<div class=\"pp-feedback-thanks\">Thank you</div>"|>
                    );
                    res1 = SubmitPacletPageFeedback[
                        "paclet:WolframInstitute/PacletPage/ref/PacletPage",
                        "Missing example for LayoutControls.",
                        "Where" -> "Basic Examples",
                        "Expected" -> "Show LeftSidebar only.",
                        "Email" -> "reader@example.org",
                        "View" -> "source"
                    ];
                    res2 = SubmitPacletPageFeedback[
                        "WolframInstitute/PacletPage",
                        "ref/PacletPage",
                        <|
                            "Feedback" -> "Screenshot attached.",
                            "Email" -> "",
                            "Attachments" -> {
                                File[attFile],
                                <|"Name" -> "shot.png", "Base64" -> "data:image/png;base64,iVBORw0KGgo="|>
                            }
                        |>
                    ];
                    resCustom = SubmitPacletPageFeedback[
                        "BareNameWithoutPublisher",
                        "Custom endpoint test.",
                        "Email" -> None,
                        "FeedbackURL" -> "https://c.example/custom-hook"
                    ];
                    {
                        res1,
                        calls[[1, "URL"]],
                        calls[[1, "Referer"]],
                        KeyTake[calls[[1, "Fields"]], {"Feedback", "Where", "Expected", "Email", "Page", "Title", "Link", "View", "Website"}],
                        res2["Submitted"],
                        calls[[2, "Fields"]]["AttachmentNames"] === FileNameTake[attFile] <> "\nshot.png",
                        MatchQ[calls[[2, "Fields"]]["Attachment1"], File[attFile]],
                        calls[[2, "Att2Exists"]],
                        StringQ[attTmpDir] && ! DirectoryQ[attTmpDir],
                        resCustom["FeedbackURL"],
                        calls[[3, "URL"]]
                    }
                ],
                (* Validation and HTTP failure responses *)
                {
                    MatchQ[SubmitPacletPageFeedback["WolframInstitute/PacletPage", "   "], Failure["EmptyFeedback", _]],
                    MatchQ[SubmitPacletPageFeedback["WolframInstitute/PacletPage", "Hi", "Email" -> "not-an-email"], Failure["InvalidEmail", _]],
                    MatchQ[SubmitPacletPageFeedback["BareNameWithoutPublisher", "Hi", "Email" -> None], Failure["NoFeedbackURL", _]],
                    Block[{curlPost = (<|"StatusCode" -> 500, "Body" -> "error"|> &)},
                        res500 = SubmitPacletPageFeedback["WolframInstitute/PacletPage", "Hi", "Email" -> None];
                        MatchQ[res500, Failure["FeedbackSubmitFailed", _]] && res500["StatusCode"] === 500
                    ],
                    Block[{curlPost = (<|"StatusCode" -> 200, "Body" -> "<html>unexpected</html>"|> &)},
                        resNoThanks = SubmitPacletPageFeedback["WolframInstitute/PacletPage", "Hi", "Email" -> None];
                        MatchQ[resNoThanks, Failure["FeedbackSubmitFailed", _]]
                    ]
                }
            },
            If[FileExistsQ[attFile], DeleteFile[attFile]]
        ]
    ],
    {
        {
            "https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/Form",
            "https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/All",
            "https://www.wolframcloud.com/obj/wolframinstitute/TuringMachineFeedback/Form",
            "https://www.wolframcloud.com/obj/myorg/ToolFeedback/Form",
            "https://c.example/obj/u/CustomSiteFeedback/Form",
            "https://c.example/obj/u/CustomFeedback/Form",
            None
        },
        {
            <|
                "Submitted" -> True,
                "Paclet" -> "WolframInstitute/PacletPage",
                "Page" -> "ref/PacletPage",
                "FeedbackURL" -> "https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/Form",
                "AllFeedbackURL" -> "https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/All"
            |>,
            "https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/Form",
            "https://www.wolframcloud.com/obj/wolframinstitute/PacletPagePage",
            <|
                "Feedback" -> "Missing example for LayoutControls.",
                "Where" -> "Basic Examples",
                "Expected" -> "Show LeftSidebar only.",
                "Email" -> "reader@example.org",
                "Page" -> "ref/PacletPage",
                "Title" -> "PacletPage",
                "Link" -> "https://www.wolframcloud.com/obj/wolframinstitute/PacletPage#ref/PacletPage",
                "View" -> "source",
                "Website" -> ""
            |>,
            True,
            True,
            True,
            True,
            True,
            "https://c.example/custom-hook",
            "https://c.example/custom-hook"
        },
        {True, True, True, True, True}
    },
    TestID -> "Feedback: defaultFeedbackEndpoints and SubmitPacletPageFeedback deduce URLs and post multipart payloads"
]

VerificationTest[
    Module[{dir = localFixture[], catalog},
        catalog = localCatalog["Pub/Name", localDocumentation[dir]];
        DeleteDirectory[dir, DeleteContents -> True];
        {
            KeyTake[catalog, {"Kind", "DocBase", "RootGuide", "Resource", "Title"}],
            Sort[Keys[catalog["Pages"]]],
            catalog["Pages"]["guide/Name"]["Guides"],
            {catalog["Pages"]["ref/F"]["Title"], catalog["Pages"]["ref/F"]["HTML"], catalog["Pages"]["tutorial/T"]["Stub"]},
            catalogQ[catalog]
        }
    ],
    {
        <|"Kind" -> "Local", "DocBase" -> "Documentation/", "RootGuide" -> "guide/Name", "Resource" -> "", "Title" -> "Name Guide"|>,
        {"guide/Name", "guide/Sub", "ref/F", "tutorial/T"},
        {"guide/Sub"},
        {"F", "Documentation/ref/F.html", True},
        True
    },
    TestID -> "Local: the catalog of a paclet's own notebooks, each page rendered under Documentation/"
]

VerificationTest[
    Module[{dir = localFixture[], files, config},
        files = PacletPageFiles[localCatalog["Pub/Name", localDocumentation[dir]], "PageURL" -> ""];
        DeleteDirectory[dir, DeleteContents -> True];
        config = configOf[files["index.html"]];
        {config["kind"], config["page"], config["routes"], StringContainsQ[config["hide"], ".menu-container"], config["home"]["id"]}
    ],
    {"local", "", {"Documentation/", "./", "paclet:Pub/Name/"}, True, "guide/Name"},
    TestID -> "Local: the shell frames the rendered pages themselves, their links resolved against the site"
]

VerificationTest[
    Module[{stage = CreateDirectory[], script, result},
        CreateDirectory[FileNameJoin[{stage, "html", "ref"}], CreateIntermediateDirectories -> True];
        CreateDirectory[FileNameJoin[{stage, "language-assets", "css"}], CreateIntermediateDirectories -> True];
        CreateDirectory[FileNameJoin[{stage, "common", "js"}], CreateIntermediateDirectories -> True];
        script = FileNameJoin[{stage, "common", "js", "lib.min.js"}];
        Export[script, "/*! lib */\n!function(){}();\n2025-04-29 15:21:33,133 org.owasp.dependencycheck.App:91\nDEBUG - Exit code: 0\nvar x=1;", "Text"];
        siteLayout[stage];
        result = {DirectoryQ[FileNameJoin[{stage, "Documentation", "ref"}]], DirectoryQ[FileNameJoin[{stage, "language-assets-v2", "css"}]], ReadString[script]};
        DeleteDirectory[stage, DeleteContents -> True];
        result
    ],
    {True, True, "/*! lib */\n!function(){}();\nvar x=1;"},
    TestID -> "Local: the pages under Documentation/, the stylesheets under the name they ask for, a script without a build log's lines"
]

VerificationTest[
    SyntaxQ[renderShareScript["/d/Documentation", "/w/share-1", <|"ref/F" -> "/d/Documentation/English/ReferencePages/Symbols/F.nb"|>, "Pub/Name", 300, "/w/r.wl", "/w/log.txt"]],
    True,
    TestID -> "Local: a rendering process's script is Wolfram Language"
]

VerificationTest[
    Module[{batches = {}},
        Block[{anonymousStatuses = Function[urls, AppendTo[batches, Length[urls]]; ConstantArray[<|"Status" -> 200, "Type" -> ""|>, Length[urls]]]},
            {
                Lookup[runChecks[{{pageCheck, "guide/A", "https://c.example/obj/u/A.nb"}, {htmlCheck, "guide/A", "https://c.example/A.html"}, {pageCheck, "ref/B", "https://c.example/obj/u/B.nb"}}], "OK"],
                batches
            }
        ]
    ],
    {{True, True, True}, {3}},
    TestID -> "Check: the pages' requests go together"
]

VerificationTest[
    Module[{dir = CreateDirectory[], slow, steady, result},
        slow = FileNameJoin[{dir, "slow.wl"}];
        steady = FileNameJoin[{dir, "steady.wl"}];
        Export[slow, "Pause[60];", "Text"];
        Export[steady, "Do[Put[i, " <> ToString[FileNameJoin[{dir, "steady-record.wl"}], InputForm] <> "]; Pause[1], {i, 5}];", "Text"];
        result = runKernelScripts[{
            <|"Script" -> slow, "Record" -> FileNameJoin[{dir, "slow-record.wl"}], "Deadline" -> 120, "Stall" -> 6|>,
            <|"Script" -> steady, "Record" -> FileNameJoin[{dir, "steady-record.wl"}], "Deadline" -> 120, "Stall" -> 6|>
        }, 2];
        DeleteDirectory[dir, DeleteContents -> True];
        Lookup[result, {slow, steady}]
    ],
    {"Stalled", "Finished"},
    TestID -> "Archive: a process that records nothing new for too long is stopped; one that keeps recording is not"
]

VerificationTest[
    Module[{dir = CreateDirectory[], hung, answering, marker, result},
        marker[name_] := FileNameJoin[{dir, name}];
        hung = marker["hung.wl"]; answering = marker["answering.wl"];
        Export[hung, "Put[0, " <> ToString[marker["hung-asked.wl"], InputForm] <> "]; Pause[60];", "Text"];
        Export[answering, "Put[0, " <> ToString[marker["answering-asked.wl"], InputForm] <> "]; Put[0, " <> ToString[marker["answering-answered.wl"], InputForm] <> "]; Pause[8];", "Text"];
        result = runKernelScripts[{
            <|"Script" -> hung, "Record" -> marker["hung-record.wl"], "Deadline" -> 120, "Stall" -> 100,
              "FrontEndAsked" -> marker["hung-asked.wl"], "FrontEndAnswered" -> marker["hung-answered.wl"], "FrontEndLimit" -> 3|>,
            <|"Script" -> answering, "Record" -> marker["answering-record.wl"], "Deadline" -> 120, "Stall" -> 100,
              "FrontEndAsked" -> marker["answering-asked.wl"], "FrontEndAnswered" -> marker["answering-answered.wl"], "FrontEndLimit" -> 3|>
        }, 2];
        DeleteDirectory[dir, DeleteContents -> True];
        Lookup[result, {hung, answering}]
    ],
    {"Stalled", "Finished"},
    TestID -> "Archive: a process whose front end does not answer soon after it is asked for is stopped; one whose front end answers runs on"
]

VerificationTest[
    Module[{dir = localFixture[], script},
        script = pageShareScript[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", {FileNameJoin[{"English", "Guides", "Name.nb"}]}, "/w/r.wl", "/w/log.txt", {}, {"/w/asked.wl", "/w/answered.wl"}];
        DeleteDirectory[dir, DeleteContents -> True];
        {SyntaxQ[script], StringPosition[script, "asked.wl"][[1, 1]] < StringPosition[script, "UsingFrontEnd["][[1, 1]] < StringPosition[script, "answered.wl"][[1, 1]] < StringPosition[script, "DocumentationBuildNotebooks"][[1, 1]]}
    ],
    {True, True},
    TestID -> "Archive: a page share asks for its front end after a marker and marks its answer before it builds"
]

VerificationTest[
    Module[{dir = localFixture[], plain = localFixture[], script, bare},
        Export[FileNameJoin[{dir, "PacletInfo.wl"}],
            "PacletObject[<|\"Name\" -> \"Pub/Name\", \"Version\" -> \"1.0.0\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"Pub`Name`\"}}|>]", "Text"];
        script = pageShareScript[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", {FileNameJoin[{"English", "Guides", "Name.nb"}]}, "/w/r.wl", "/w/log.txt", {}, {"/w/asked.wl", "/w/answered.wl"}];
        bare = pageShareScript[plain, FileNameJoin[{plain, "Documentation"}], "/b/Documentation", {FileNameJoin[{"English", "Guides", "Name.nb"}]}, "/w/r.wl", "/w/log.txt", {}, {"/w/asked.wl", "/w/answered.wl"}];
        DeleteDirectory[dir, DeleteContents -> True]; DeleteDirectory[plain, DeleteContents -> True];
        {
            SyntaxQ[script],
            StringPosition[script, "Needs[\"Pub`Name`\"]"] =!= {} && StringPosition[script, "Needs[\"Pub`Name`\"]"][[1, 1]] < StringPosition[script, "asked.wl"][[1, 1]] < StringPosition[script, "UsingFrontEnd["][[1, 1]],
            StringFreeQ[bare, "Needs[\"Pub"]
        }
    ],
    {True, True, True},
    TestID -> "Archive: a page share loads the paclet's context before it asks for its front end, and a paclet with no context loads nothing"
]

VerificationTest[
    Module[{dir = localFixture[], plain = localFixture[], work = CreateDirectory[], other = CreateDirectory[], got = {}, scripts},
        Export[FileNameJoin[{dir, "PacletInfo.wl"}],
            "PacletObject[<|\"Name\" -> \"Pub/Name\", \"Version\" -> \"1.0.0\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"Pub`Name`\"}}|>]", "Text"];
        Block[{runKernelScripts = Function[{runs, n, what}, AppendTo[got, {runs, n}]; <||>]},
            pacletLoadedOnce[dir, {}, work, 60];
            pacletLoadedOnce[plain, {}, other, 60]
        ];
        scripts = ReadString[#[[1, 1, "Script"]]] & /@ got;
        DeleteDirectory[#, DeleteContents -> True] & /@ {dir, plain, work, other};
        {
            got[[All, 2]], SyntaxQ /@ scripts,
            StringContainsQ[#, "Needs[\"DocumentationBuild`\"]"] & /@ scripts,
            StringContainsQ[#, "Needs[\"Pub`Name`\"]"] & /@ scripts,
            StringFreeQ[#, "UsingFrontEnd"] & /@ scripts
        }
    ],
    {{1, 1}, {True, True}, {True, True}, {True, False}, {True, True}},
    TestID -> "Archive: before its shares start the build's packages and the paclet are loaded once, in one kernel with no front end"
]

VerificationTest[
    Module[{dir = localFixture[], script},
        script = pageShareScript[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", {FileNameJoin[{"English", "Guides", "Name.nb"}]}, "/w/r.wl", "/w/log.txt", {}];
        DeleteDirectory[dir, DeleteContents -> True];
        SyntaxQ[script]
    ],
    True,
    TestID -> "Archive: a page share's script is Wolfram Language"
]

VerificationTest[
    Module[{dir = localFixture[], scripts, own},
        scripts = {
            pageShareScript[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", {FileNameJoin[{"English", "Guides", "Name.nb"}]}, "/w/r.wl", "/w/log.txt", {}],
            pacletBuildScript[dir, "/b", {}]
        };
        own = "Get[" <> wlString[FileNameJoin[{$packageDirectory, "PacletPage.wl"}]] <> "]";
        DeleteDirectory[dir, DeleteContents -> True];
        {StringContainsQ[#, own] & /@ scripts, StringFreeQ[#, "Needs[\"WolframInstitute`PacletPage`\"]"] & /@ scripts}
    ],
    {{True, True}, {True, True}},
    TestID -> "Archive: a script for another kernel loads the PacletPage that wrote it, by its file, not the newest one installed"
]

VerificationTest[
    Module[{dir = localFixture[], nb, first, second, text, again, got},
        nb = FileNameJoin[{dir, "ResourceDefinition.nb"}];
        Export[nb, Notebook[{Cell[CellGroupData[{
            Cell["Paclet Manifest", "Section", TaggingRules -> {"TemplateGroupName" -> "PacletManifest"}],
            Cell[BoxData[TemplateBox[{}, "PacletDirectoryChooserTemplate"]], "PacletDirectoryChooser"]
        }, Open]]}], "NB"];
        first = fillPacletManifest[nb, dir];
        text = ByteArrayToString[ReadByteArray[nb], "UTF-8"];
        second = fillPacletManifest[nb, dir];
        again = ByteArrayToString[ReadByteArray[nb], "UTF-8"];
        got = Get[nb];
        DeleteDirectory[dir, DeleteContents -> True];
        With[{ids = Cases[got, c : Cell[__, CellTags -> {___, "FileManager-PacletFiles", ___}, ___] :> Lookup[Cases[List @@ c, _Rule], CellID, None], Infinity]}, {
            first, second,
            StringContainsQ[text, "FileNameJoin[{NotebookDirectory[], \"PacletInfo.wl\"}]"], StringFreeQ[text, "pacletManifestPath"], text === again,
            Count[got, Cell[_, "FileManagerTopSpacer", ___, CellTags -> {"FileManagerCell", "FileManager-PacletFiles"}, ___], Infinity],
            Length[ids] > 1 && AllTrue[ids, IntegerQ] && DuplicateFreeQ[ids]
        }]
    ],
    {True, True, True, True, True, 1, True},
    TestID -> "Build: a definition notebook from markdown lists the paclet's files after its directory chooser, by paths relative to the notebook, each cell with its CellID, the same each build"
]

VerificationTest[
    Module[{dir = localFixture[], built = CreateDirectory[], page = FileNameJoin[{"English", "Guides", "Name.nb"}], script, file, tidied, again},
        script = pageShareScript[dir, FileNameJoin[{dir, "Documentation"}], built, {page}, "/w/r.wl", "/w/log.txt", {}];
        file = FileNameJoin[{built, page}];
        CreateDirectory[DirectoryName[file], CreateIntermediateDirectories -> True];
        Export[file, Notebook[{Cell["", "PageDelimiter"], Cell[CellGroupData[{Cell["Examples", "PrimaryExamplesSection"]}, Open]]}], "NB"];
        (* the script's last step, its share's pages tidied, with the list of its pages *)
        Block[{pages, inputDir}, ToExpression /@ Select[StringSplit[script, "\n"], StringStartsQ[#, "pages = " | "inputDir = "] || StringContainsQ[#, "tidyBuiltPage"] &]];
        tidied = Get[file];
        again = tidyBuiltPage[file];
        DeleteDirectory[dir, DeleteContents -> True];
        DeleteDirectory[built, DeleteContents -> True];
        {
            StringPosition[script, "tidyBuiltPage"][[1, 1]] > StringPosition[script, "Put["][[1, 1]],
            FreeQ[tidied, "PageDelimiter"],
            ! FreeQ[tidied, "PrimaryExamplesSection"],
            again
        }
    ],
    {True, True, True, False},
    TestID -> "Archive: a page share tidies its pages once its record is written, and a tidied page is only read again"
]

VerificationTest[
    Module[{dir = CreateDirectory[], page, same, other, tidied},
        page[file_, rules_] := Export[FileNameJoin[{dir, file}], Notebook[{Cell["text", "Text"]}, TaggingRules -> rules], "NB"];
        page["a.nb", {"Metadata" -> {"WorkflowDockedCell" -> "", "uri" -> "P/ref/A"}, "SearchTextTranslated" -> ""}];
        page["b.nb", {"Metadata" -> {"uri" -> "P/ref/A"}}];
        page["c.nb", {"Metadata" -> {"WorkflowDockedCell" -> "docked", "uri" -> "P/ref/A"}}];
        same = contentKey[FileNameJoin[{dir, "a.nb"}]] === contentKey[FileNameJoin[{dir, "b.nb"}]];
        other = contentKey[FileNameJoin[{dir, "c.nb"}]] =!= contentKey[FileNameJoin[{dir, "b.nb"}]];
        tidied = {tidyBuiltPage[FileNameJoin[{dir, "a.nb"}]], tidyBuiltPage[FileNameJoin[{dir, "a.nb"}]], tidyBuiltPage[FileNameJoin[{dir, "c.nb"}]]};
        With[{rules = Lookup[Options[Get[FileNameJoin[{dir, "a.nb"}]]], TaggingRules]},
            DeleteDirectory[dir, DeleteContents -> True];
            {same, other, tidied, rules}
        ]
    ],
    {True, True, {True, False, False}, {"Metadata" -> {"uri" -> "P/ref/A"}}},
    TestID -> "Archive: the empty entries the build's front end leaves on some builds are tidied away and change no key"
]

VerificationTest[
    Module[{dir = CreateDirectory[], source, built, cell, states, tidied},
        source = FileNameJoin[{dir, "source.nb"}];
        built = FileNameJoin[{dir, "built.nb"}];
        cell[style_, id_, opts___] := Cell[BoxData[ToString[id]], style, opts, CellID -> id];
        (* as converted: an example that shows its output alone, with a tutorial's Needs input in
           its group, an example closed to its input, a closed cell, a closed template section *)
        Export[source, Notebook[{
            Cell[CellGroupData[{cell["Input", 9], cell["Input", 1], cell["Output", 2]}, {3}]],
            Cell[CellGroupData[{cell["Input", 3], cell["Output", 4]}, Closed]],
            Cell[CellGroupData[{cell["Input", 5, CellOpen -> False], cell["Output", 6]}, Open]],
            Cell[CellGroupData[{cell["Section", 7], cell["Text", 8]}, Closed]]}], "NB"];
        (* as DocumentationBuild leaves it: every group and cell open, the Needs input moved out of
           the example's group *)
        Export[built, Notebook[{
            cell["Input", 9],
            Cell[CellGroupData[{cell["Input", 1], cell["Output", 2]}, Open]],
            Cell[CellGroupData[{cell["Input", 3], cell["Output", 4]}, Open]],
            Cell[CellGroupData[{cell["Input", 5], cell["Output", 6]}, Open]],
            Cell[CellGroupData[{cell["Section", 7], cell["Text", 8]}, Open]]}], "NB"];
        states[file_] := {Cases[Get[file], Cell[CellGroupData[_, state_], ___] :> state, Infinity],
            Cases[Get[file], c : Cell[_, _, ___, CellOpen -> False, ___] :> cellID[c], Infinity]};
        tidied = {tidyBuiltPage[built, source], tidyBuiltPage[built, source], tidyBuiltPage[built, FileNameJoin[{dir, "none.nb"}]]};
        With[{after = states[built]},
            DeleteDirectory[dir, DeleteContents -> True];
            {tidied, after}]
    ],
    {{True, False, False}, {{{2}, Closed, Open, Open}, {5}}},
    TestID -> "Build: a built page hides again the cells and example groups its converted page hides, which DocumentationBuild opens, and leaves the template's sections to the build"
]

VerificationTest[
    statusUpdate /@ {<|"id" -> "E1", "status" -> "fixed", "note" -> "n"|>, <|"ID" -> "E2", "Status" -> "open"|>},
    {<|"id" -> "E1", "status" -> "fixed", "note" -> "n"|>, <|"id" -> "E2", "status" -> "open", "note" -> ""|>},
    TestID -> "Feedback: an update's keys in either case"
]

VerificationTest[
    Module[{dir = CreateDirectory[], files, puts = 0, result},
        files = Table[With[{f = FileNameJoin[{dir, "N" <> IntegerString[i] <> ".nb"}]}, Export[f, "Notebook[{}] (* " <> IntegerString[i] <> " *)", "Text"]; f], {i, 60}];
        Block[{
                sourceNotebooks = Function[{d, short}, {#, FileNameTake[#]} & /@ files],
                FileExistsQ = Function[x, If[MatchQ[x, _CloudObject], False, System`FileExistsQ[x]]],
                mapBlocks = Function[{f, list}, Map[f, list]],
                CopyFile = Function[{from, to, opt}, to],
                CloudPut = Function[{expr, object, opt}, puts++; object]
            },
            result = publishSource[dir, "https://c.example/obj/u/SiteSource", "https://c.example/obj/u/SiteSourceManifest", "Public", "Name"]
        ];
        DeleteDirectory[dir, DeleteContents -> True];
        {result["Uploaded"], puts}
    ],
    {60, 3},
    TestID -> "Cloud: source notebooks upload in blocks, the manifest written after each"
]

VerificationTest[
    Module[{published = 0, exported = <||>, result},
        Block[{
                accountBase = ("https://c.example/obj/u" &),
                publishingAccountQ = Function[{head, account}, True],
                publishSource = Function[{dir, url, manifest, permissions, short, all}, published++; <|"URL" -> url|>],
                exportPage = Function[{html, url, permissions}, exported[url] = html; CloudObject[url]]
            },
            result = DeployPacletPage[cloudCatalog, "Site", "Source" -> "dir", "Feedback" -> False, "ViewSource" -> False]
        ];
        {published, result["Source"], KeyTake[configOf[exported["https://c.example/obj/u/SitePage"]], {"source", "viewSource"}]}
    ],
    {0, None, <|"source" -> "", "viewSource" -> False|>},
    TestID -> "Cloud: \"ViewSource\" -> False publishes no sources and shows none"
]

VerificationTest[
    Needs["PacletResource`"];
    Block[{PacletResource`Notebooks`Private`$packageURLTemplate = "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/`1`.html",
            PacletResource`Notebooks`Private`$pacletName = "Pub/Name", PacletResource`Notebooks`Private`$validRefs = None},
        Cases[
            Cell[BoxData[TemplateBox[{Cell[TextData[RowBox[{"Plane", " ", "Conics"}]]], "paclet:Pub/Name/guide/Conics"}, "OrangeLink", BaseStyle -> "GuideFunctionsSubsection"]],
                "GuideFunctionsSubsection"] /. orangeLinkRule[],
            TemplateBox[{text_, {URL[u_], None}, _, _, active_, BaseStyle -> base_, ___}, "HyperlinkTemplate"] :> {text, u, active, base}, Infinity]],
    {{Cell[TextData[RowBox[{"Plane", " ", "Conics"}]]], "https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/guide/Conics.html",
      {FontColor -> LightDarkSwitched[RGBColor[0.9686274509803922, 0.4666666666666667, 0.]]},
      {"Hyperlink", "GuideFunctionsSubsection", FontColor -> LightDarkSwitched[RGBColor[0.85098, 0.396078, 0.]]}}},
    TestID -> "Publish: a guide's sub-guide heading links to the deployed page, in the heading's style and the OrangeLink's orange"
]

(* the parallel kernels apply the rule without this package loaded *)
VerificationTest[
    Select[
        Union[Cases[orangeLinkRule[], s_Symbol :> HoldComplete[s], {0, Infinity}, Heads -> True]],
        MatchQ[#, HoldComplete[s_] /; StringStartsQ[Context[s], "WolframInstitute`PacletPage`"] && (DownValues[s] =!= {} || OwnValues[s] =!= {})] &
    ],
    {},
    TestID -> "Publish: the sub-guide heading link rule uses none of the package's definitions"
]

VerificationTest[
    Module[{dir = localFixture[], archive, files},
        CreateDirectory[FileNameJoin[{dir, "build"}]];
        Export[FileNameJoin[{dir, "build", "Built.nb"}], "Notebook[{}]", "Text"];
        CreateDirectory[FileNameJoin[{dir, ".git"}]];
        Export[FileNameJoin[{dir, ".git", "HEAD"}], "ref", "Text"];
        Export[FileNameJoin[{dir, "big.bin"}], "x", "Text"];
        archive = sourceArchive[dir, {"big.bin"}];
        files = Import[archive, {"ZIP", "FileNames"}];
        DeleteDirectory[DirectoryName[archive], DeleteContents -> True];
        DeleteDirectory[dir, DeleteContents -> True];
        {
            AnyTrue[files, StringContainsQ[#, "Documentation/English/Guides/Name.nb"] &],
            AnyTrue[files, StringContainsQ[#, "PacletInfo.wl"] &],
            AnyTrue[files, StringContainsQ[#, "/build/" | ".git" | "big.bin"] &]
        }
    ],
    {True, True, False},
    TestID -> "Archive: the source archive holds the paclet as authored, without build/, hidden files and what is left out"
]

VerificationTest[
    sourceArchiveNames["Paclets/", "Pub/Name", "1.2.0"],
    <|"Latest" -> "Paclets/Pub__Name-source.paclet", "Version" -> "Paclets/Pub__Name-source-1.2.0.paclet"|>,
    TestID -> "Archive: the source archive's names, the latest and the versioned"
]

VerificationTest[
    Module[{kernels = LaunchKernels[2], frontEnds},
        ParallelEvaluate[UsingFrontEnd[ToBoxes[Graphics[Disk[]]]]; 1, kernels];
        frontEnds = kernelFrontEnds[kernels];
        closeKernels[kernels];
        waitForExit[frontEnds, 10];
        {Length[frontEnds] >= Length[kernels], livingProcesses[frontEnds], closeKernels[{}], Intersection[Kernels[], kernels]}
    ],
    {True, {}, {}, {}},
    TestID -> "Build: parallel kernels close after the front ends they started are ended"
]

VerificationTest[
    {
        descendantProcesses[<|10 -> {1, "kernel"}, 11 -> {10, "fe"}, 12 -> {11, "helper"}, 20 -> {1, "other"}|>, {10}],
        frontEndProcessQ /@ {"/Applications/Wolfram.app/Contents/MacOS/MathematicaServer", "WolframNB", "WolframKernel"}
    },
    {{11, 12}, {True, True, False}},
    TestID -> "Build: a kernel's front ends are found among the processes it started"
]

VerificationTest[
    Module[{calls = 0, result},
        (* a cloud copy goes through CloudObject's own rules, which no definition for CopyFile precedes *)
        Block[{$cloudCopyPauses = {0, 0}, CopyFile = Function[{from, to, option}, If[++calls < 3, Message[CopyFile::cloudunknown]; $Failed, to]]},
            result = cloudCopy["/x", CloudObject["https://www.wolframcloud.com/obj/user/x"]]
        ];
        {calls, MatchQ[result, _CloudObject]}
    ],
    {3, True},
    TestID -> "Deploy: a file the cloud fails to write is sent again, and the attempts that failed show nothing"
]

VerificationTest[
    Module[{calls = 0, result},
        Block[{$cloudCopyPauses = {0, 0}, CopyFile = Function[{from, to, option}, ++calls; Message[CopyFile::cloudunknown]; $Failed]},
            result = cloudCopy["/x", CloudObject["https://www.wolframcloud.com/obj/user/x"]]
        ];
        {calls, result}
    ],
    {3, $Failed},
    {CopyFile::cloudunknown},
    TestID -> "Deploy: a file the cloud fails three times is not written, and only the last attempt's message shows"
]

VerificationTest[
    Needs["PacletResource`"];
    With[{patched = DownValues[PacletResource`Private`cloudExportPacletResourceContent] /. $cloudCopyRule},
        {FreeQ[patched, HoldPattern[CopyFile[_, _, OverwriteTarget -> True]]], ! FreeQ[patched, cloudCopy]}
    ],
    {True, True},
    TestID -> "Deploy: the stock deploy's archive upload is sent again when the cloud fails it"
]

VerificationTest[
    Module[{dir = CreateDirectory[], put, flagged},
        put[name_String, nb_Notebook] := With[{file = FileNameJoin[{dir, name}]},
            If[! DirectoryQ[DirectoryName[file]], CreateDirectory[DirectoryName[file], CreateIntermediateDirectories -> True]];
            Export[file, nb, "NB"]
        ];
        put["ReferencePages/Symbols/A.nb", Notebook[{Cell["A", "ObjectName"]}, ScreenStyleEnvironment -> "FutureObject"]];
        put["ReferencePages/Symbols/B.nb", Notebook[{Cell["E  X  C  I  S  E  D", "ExcisedFlag"], Cell["B", "ObjectName"]}]];
        put["ReferencePages/Symbols/C.nb", Notebook[{Cell["C", "ObjectName"]}]];
        put["ReferencePages/Symbols/D.nb", Notebook[{Cell["O  B  S  O  L  E  T  E", "ObsoleteFlag"], Cell["D", "ObjectName"]}]];
        flagged = flaggedPages[dir];
        DeleteDirectory[dir, DeleteContents -> True];
        flagged
    ],
    <|FileNameJoin[{"ReferencePages", "Symbols", "A.nb"}] -> "Future", FileNameJoin[{"ReferencePages", "Symbols", "B.nb"}] -> "Excised"|>,
    TestID -> "Build: the pages the documentation build leaves out for their flag, from the palette's screen environment or a flag cell; an obsolete page builds"
]

VerificationTest[
    {flaggedPages["/no/such/directory"], StringQ[DeployPacletResource::flagged], StringQ[BuildPacletDocumentation::flagged], SubmitPacletResource::flagged === DeployPacletResource::flagged},
    {<||>, True, True, True},
    TestID -> "Build: a directory with no pages has none flagged, and the builds say which they leave out"
]

VerificationTest[
    Module[{root = CreateDirectory[], before = DownValues[CreatePacletArchive], handed},
        handed = Internal`InheritedBlock[{CreatePacletArchive},
            installArchiveRule[root, "/b/Pub__Name-1.0.0.paclet"];
            {CreatePacletArchive[root <> $PathnameSeparator], CreatePacletArchive[root]}
        ];
        DeleteDirectory[root];
        {handed, DownValues[CreatePacletArchive] === before}
    ],
    {{"/b/Pub__Name-1.0.0.paclet", "/b/Pub__Name-1.0.0.paclet"}, True},
    TestID -> "Deploy: the scrape is handed the archive the build made and checked, and packs none over it"
]

VerificationTest[
    Module[{before = Kernels[], work = FileNameJoin[{$TemporaryDirectory, CreateUUID["page-"] <> ".nb"}], done, left},
        done = convertHere[<|"Function" -> ((LaunchKernels[1]; Export[#2, Notebook[{}], "NB"]) &)|>, <|"TimeConstraint" -> 120, "Monitor" -> False|>][
            <|"File" -> "page.md", "Work" -> work, "UseCache" -> True, "Options" -> {}, "Page" -> "page"|>];
        left = Complement[Kernels[], before];
        If[FileExistsQ[work], DeleteFile[work]];
        {done["OK"], left}
    ],
    {True, {}},
    TestID -> "Build: the parallel kernels a page's examples launch are closed after the page"
]

VerificationTest[
    Module[{kernels = LaunchKernels[1], shadowOff = MemberQ[offGeneralMessages[], "shdw"], off, before, matched, reversed},
        (* the building kernel as a script kernel is, General::shdw off, whatever this kernel was started as *)
        Off[General::shdw];
        off = offGeneralMessages[];
        DistributeDefinitions[matchGeneralMessages];
        before = First @ ParallelEvaluate[MemberQ[offGeneralMessages[], "shdw"], kernels];
        matched = First @ With[{o = off}, ParallelEvaluate[matchGeneralMessages[o]; offGeneralMessages[], kernels]];
        reversed = First @ With[{o = DeleteCases[off, "shdw"]}, ParallelEvaluate[matchGeneralMessages[o]; MemberQ[offGeneralMessages[], "shdw"], kernels]];
        CloseKernels[kernels];
        If[! shadowOff, On[General::shdw]];
        {MemberQ[off, "shdw"], before, matched === off, reversed}
    ],
    {True, False, True, False},
    TestID -> "Build: a parallel kernel takes the General message switches of the kernel that builds, a script kernel's General::shdw off"
]

VerificationTest[
    {
        processLayers[<|10 -> {1, "kernel", "S"}, 11 -> {10, "fe", "S"}, 12 -> {11, "helper", "S"}, 13 -> {10, "<defunct>", "Z"}, 20 -> {1, "other", "S"}|>, {10}],
        livingProcesses[<|11 -> {10, "fe", "S"}, 13 -> {10, "<defunct>", "Z+"}|>, {11, 13, 99}]
    },
    {{{11, 13}, {12}}, {11}},
    TestID -> "Build: the processes a process started come a generation at a time, and one exited is no longer living"
]

VerificationTest[
    Module[{script = FileNameJoin[{$TemporaryDirectory, CreateUUID["end-"] <> ".wl"}], out = FileNameJoin[{$TemporaryDirectory, CreateUUID["end-"] <> ".wl"}], result},
        Export[script, StringJoin[
            "Get[", wlString[FileNameJoin[{$packageDirectory, "Processes.wl"}]], "];\n",
            "LaunchKernels[1];\n",
            "ParallelEvaluate[UsingFrontEnd[ToBoxes[Graphics[Disk[]]]]];\n",
            "UsingFrontEnd[ToBoxes[Graphics[Circle[]]]];\n",
            "before = descendantProcesses[processTable[], {$ProcessID}];\n",
            $endScriptCode,
            (* one killed that the kernel has not reaped yet is listed, defunct, until the kernel exits: it is not left *)
            "Put[{Length[before], With[{table = processTable[]}, livingProcesses[table, descendantProcesses[table, {$ProcessID}]]]}, ", wlString[out], "];\n"
        ], "Text"];
        TimeConstrained[RunProcess[{kernelExecutable[], "-noprompt", "-script", script}], 240];
        result = If[FileExistsQ[out], Get[out], None];
        Scan[If[FileExistsQ[#], DeleteFile[#]] &, {script, out}];
        Replace[result, {{n_Integer, left_List} :> {n >= 3, left}, other_ :> other}]
    ],
    {True, {}},
    TestID -> "Build: a script ends its front ends and kernels before it exits, and none of them is left"
]

VerificationTest[
    Module[{dir = CreateDirectory[], file, done},
        file = FileNameJoin[{dir, "page.md"}];
        Export[file, "page", "Text"];
        Symbol["Global`pageProbe"];
        Global`keptProbe = 1;
        done = convertPage[
            Function[{source, work}, Global`pageProbe = 5; Global`madeProbe = 6; Global`keptProbe = 2; Export[work, "Notebook[{}]", "Text"]],
            <|"File" -> file, "Work" -> FileNameJoin[{dir, "page.nb"}], "UseCache" -> False, "Options" -> {}|>,
            60
        ];
        DeleteDirectory[dir, DeleteContents -> True];
        {done["OK"], OwnValues["Global`pageProbe"], OwnValues["Global`madeProbe"], Global`keptProbe}
    ],
    {True, {}, {}, 2},
    TestID -> "Build: the Global` symbols a page gives definitions to are cleared after it, and those defined before keep theirs"
]

VerificationTest[
    Module[{dir = CreateDirectory[], file, done, after},
        file = FileNameJoin[{dir, "page.md"}];
        Export[file, "page", "Text"];
        done = convertPage[Function[{source, work}, Abort[]], <|"File" -> file, "Work" -> FileNameJoin[{dir, "page.nb"}], "UseCache" -> False, "Options" -> {}|>, 60];
        after = "went on";
        DeleteDirectory[dir, DeleteContents -> True];
        {done["OK"], done["Result"], after}
    ],
    {False, $Aborted, "went on"},
    TestID -> "Build: an abort in a page's conversion ends as that page's failure, and the build goes on"
]

VerificationTest[
    Module[{dir = CreateDirectory[], tasks, lost},
        Export[FileNameJoin[{dir, "a.nb"}], "Notebook[{}]", "Text"];
        tasks = <|"File" -> # <> ".md", "Out" -> FileNameJoin[{dir, # <> ".nb"}]|> & /@ {"a", "b", "c"};
        lost = unaccountedPages[tasks, {"c.md"}];
        DeleteDirectory[dir, DeleteContents -> True];
        lost
    ],
    {"b.md"},
    TestID -> "Build: a planned page with no notebook that is not among the build's failures is lost, and a failed one is not"
]

VerificationTest[
    Module[{dir = CreateDirectory[], log, out = {}},
        log = FileNameJoin[{dir, "share-3.log"}];
        Export[log, StringRiffle[Table["line " <> IntegerString[i], {i, 250}], "\n"], "Text"];
        Block[{monitor = Function[s, AppendTo[out, s]]},
            unrecordedLogs[{<|"Label" -> "archive: share 3 of 8", "Pages" -> {"a.nb", "b.nb"}, "Record" -> FileNameJoin[{dir, "none.wl"}], "Log" -> log|>}]
        ];
        DeleteDirectory[dir, DeleteContents -> True];
        {Length[out], out[[2]], out[[3]], out[[4]], Last[out]}
    ],
    {203, "archive: share 3 of 8 left: a.nb, b.nb", "archive: share 3 of 8 built before it stopped: none", "archive: share 3 of 8 log: line 51", "archive: share 3 of 8 log: line 250"},
    TestID -> "Archive: a share that recorded none of its pages names them and prints the end of its log a line each"
]

VerificationTest[
    Module[{dir = CreateDirectory[], script, old, new, built},
        old = FileNameJoin[{dir, "b.nb"}]; Export[old, "Notebook[{}]", "Text"];
        Pause[1.2];
        script = FileNameJoin[{dir, "share-1.wl"}]; Export[script, "1", "Text"];
        Pause[1.2];
        new = FileNameJoin[{dir, "a.nb"}]; Export[new, "Notebook[{}]", "Text"];
        built = builtThisRun[<|"Script" -> script, "Pages" -> {"a.nb", "b.nb", "c.nb"}, "Progress" -> {new, old, FileNameJoin[{dir, "c.nb"}]}|>];
        DeleteDirectory[dir, DeleteContents -> True];
        built
    ],
    {"a.nb"},
    TestID -> "Archive: the pages a share built before it stopped are those newer than its script"
]

VerificationTest[
    Module[{work = CreateDirectory[], tmp, script, out, seen},
        tmp = FileNameJoin[{work, "share-1-tmp"}];
        out = FileNameJoin[{work, "seen.wl"}];
        script = FileNameJoin[{work, "share-1.wl"}];
        Export[script, "Put[$TemporaryDirectory, " <> ToString[out, InputForm] <> "];", "Text"];
        runKernelScripts[{<|"Script" -> script, "Deadline" -> 120, "TemporaryDirectory" -> tmp|>}, 1];
        seen = If[FileExistsQ[out], Get[out], None];
        DeleteDirectory[work, DeleteContents -> True];
        {runEnvironment[<||>], StringQ[seen] && FileNameSplit[ExpandFileName[seen]][[-1]] === "share-1-tmp"}
    ],
    {Inherited, True},
    TestID -> "Archive: a share's process has a temporary directory of its own, and a run without one inherits this kernel's environment"
]

VerificationTest[
    Module[{dir = localFixture[], work = CreateDirectory[], runs, alone},
        runs = pageShareRuns[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", Table["p" <> IntegerString[i] <> ".nb", {i, 100}], 4, 900, {}, work, "share"];
        alone = pageShareRuns[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", {"a.nb", "b.nb", "c.nb"}, 4, 900, {}, work, "alone", 1];
        DeleteDirectory[dir, DeleteContents -> True];
        DeleteDirectory[work, DeleteContents -> True];
        {Length /@ Lookup[runs, "Pages"], Lookup[First[runs], {"Deadline", "Stall"}], First[First[runs]["Progress"]], Lookup[alone, "Pages"], {First[runs]["Label"], First[alone]["Label"]}}
    ],
    {{13, 13, 13, 13, 13, 13, 13, 9}, {900 * 13 + 600, 1500}, FileNameJoin[{"/b/Documentation", "p1.nb"}], {{"a.nb"}, {"b.nb"}, {"c.nb"}}, {"archive: share 1 of 8 (13 pages)", "archive: alone 1 of 3 (a)"}},
    TestID -> "Archive: the changed pages come in two shares a process, each with its time and the pages it builds to watch; or a page a share"
]

VerificationTest[
    Module[{lines = {}},
        Block[{Kernels = Function[{}, {"k1", "k2"}], kernelAccounts = Function[{}, {"b@x.example", "b@x.example"}], cloudAccount = Function[{}, "a@x.example"],
                ParallelMap = Function[Null, "parallel"], $accountsNoted = {}, $deployMonitor = True, Print = Function[Null, AppendTo[lines, StringJoin[ToString /@ {##}]]]},
            {mapBlocks[# + 1 &, {1, 2}], mapBlocks[# + 1 &, {3}], lines}
        ]
    ],
    {{2, 3}, {4}, {"cloud: the parallel kernels act as b@x.example, not a@x.example: cloud writes go from this kernel"}},
    TestID -> "Cloud: cloud writes go from this kernel when its parallel kernels act as another account, said once"
]

VerificationTest[
    Module[{process, pid, children, start},
        (* a parent and a child that both start at once: a kernel's start, as a subkernel's, waits on
           a license, and on a busy machine ran past the minute the tree is waited for *)
        process = StartProcess[{"sh", "-c", "sleep 120 & wait"}];
        pid = process["PID"];
        start = AbsoluteTime[];
        While[ descendantProcesses[processTable[], {pid}] === {} && AbsoluteTime[] - start < 60, Pause[0.5]];
        children = descendantProcesses[processTable[], {pid}];
        killProcessTree[process];
        waitForExit[children, 10];
        {children =!= {}, livingProcesses[children]}
    ],
    {True, {}},
    TestID -> "Archive: a process killed takes the processes it started with it"
]

VerificationTest[
    staleDocumentation[
        {"guide/A.nb", "guide/A.html", "ref/B.nb", "ref/B.html", "ref/Gone.nb", "ref/Gone.html", "images/x.png"},
        {"guide/A.nb", "ref/B.nb"}
    ],
    {"ref/Gone.nb", "ref/Gone.html"},
    TestID -> "Publish: only the notebooks the new documentation lacks, and their pages, are stale"
]

VerificationTest[
    {stopsQ[False, False], stopsQ[False, True], stopsQ[True, False], stopsQ[True, True]},
    {True, False, False, False},
    TestID -> "Publish: a failed check stops a real run, and only reports in a dry run"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{failing, fixed, page = FileNameJoin[{dir, "docs", "Symbols", "foo-page.md"}]},
        BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := 3 x", "Text"];
        Export[page, "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Foo\n---\n\nFAIL\n", "Text"];
        failing = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        Export[page, "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Foo\n---\n\nFixed.\n", "Text"];
        fixed = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        {
            FileNameTake /@ failing["Failed"],
            Length[failing["Notebooks"]],
            notebookText[FileNameJoin[{dir, "Documentation", "English", "ReferencePages", "Symbols", "Foo.nb"}]][[2]],
            FileNameTake /@ fixed["Evaluated"]
        }
    ]]],
    {{"foo-page.md"}, 2, "{\"UseCache\" -> False, \"EvaluateSeparator\" -> Automatic}", {"foo-page.md"}},
    {BuildPacletDocumentation::page},
    TestID -> "Build: a page that fails keeps its last notebook, and evaluates afresh once it converts"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{result},
        Export[FileNameJoin[{dir, "docs", "Symbols", "hang.md"}], "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Hang\n---\n\nHANG\n", "Text"];
        Export[FileNameJoin[{dir, "docs", "Symbols", "late.md"}], "---\nTemplate: Symbol\nURI: Pub/Fixture/ref/Late\n---\n\nLATE\n", "Text"];
        result = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "TimeConstraint" -> 2];
        {Sort[FileNameTake /@ result["Failed"]], MemberQ[FileNameTake /@ result["Converted"], "late.md"]}
    ]]],
    {{"hang.md"}, True},
    {BuildPacletDocumentation::page},
    TestID -> "Build: a page past its time limit fails, unless it wrote its whole notebook first"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{again},
        BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        DeleteFile[FileNameJoin[{dir, "docs", "Tutorials", "Steps.md"}]];
        again = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Prune" -> True];
        {
            FileNameTake /@ again["Pruned"],
            Length[FileNames["*.nb", FileNameJoin[{dir, "build", "cache", "pages"}]]],
            Keys[Get[FileNameJoin[{dir, "build", "cache", "evaluated.wl"}]]]
        }
    ]]],
    {{"Steps.nb"}, 2, {FileNameJoin[{"Guides", "Main.md"}], FileNameJoin[{"Symbols", "foo-page.md"}]}},
    TestID -> "Build: a page whose markdown is gone leaves the documentation and the cache"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{other = fixtureDocs[], dependency = fixtureDocs[], hashes},
        PacletDirectoryLoad[other];
        hashes = {exampleCode[dir, Automatic], exampleCode[dir, {dependency}]};
        Export[FileNameJoin[{dependency, "Kernel", "Code.wl"}], "f[x_] := -x", "Text"];
        AppendTo[hashes, exampleCode[dir, {dependency}]];
        PacletDirectoryUnload[other];
        DeleteDirectory[other, DeleteContents -> True];
        DeleteDirectory[dependency, DeleteContents -> True];
        {hashes[[1]] === codeHash[{dir}], DuplicateFreeQ[hashes]}
    ]]],
    {True, True},
    TestID -> "Build: the examples' code is the paclet's own and its dependencies', not whatever else is loaded"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{result},
        Export[FileNameJoin[{dir, "docs", "STYLE.md"}], "# Notes for authors\n\nNo front matter.\n", "Text"];
        Export[FileNameJoin[{dir, "docs", "ResourceDefinition.md"}], "---\nTemplate: Paclet\nName: Pub/Fixture\n---\n\nThe paclet.\n", "Text"];
        result = BuildPacletDocumentation[dir, "Converter" -> fakeConvert];
        {Sort[FileNameTake /@ result["Skipped"]], Length[result["Notebooks"]], FileNameTake[result["Definition"]], DirectoryQ[FileNameJoin[{dir, "Documentation", "English", "Other"}]]}
    ]]],
    {{"ResourceDefinition.md", "STYLE.md"}, 3, "ResourceDefinition.nb", False},
    TestID -> "Build: markdown without a page template is no page; the definition is found in the source"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{result},
        Export[FileNameJoin[{dir, "ResourceDefinition.nb"}], "Notebook[{}]", "Text"];
        result = BuildPacletDocumentation[dir, "Converter" -> fakeConvert];
        result["Definition"]
    ]]],
    None,
    {BuildPacletDocumentation::nodefinition},
    TestID -> "Build: an old definition notebook with no markdown for it is reported"
]

VerificationTest[
    Module[{dir = CreateDirectory[], outside = CreateDirectory[], links},
        CreateDirectory[FileNameJoin[{dir, "LibraryResources", "MacOSX-ARM64"}], CreateIntermediateDirectories -> True];
        Export[FileNameJoin[{outside, "libruntime.dylib"}], "x", "Text"];
        Export[FileNameJoin[{dir, "LibraryResources", "MacOSX-ARM64", "libown.dylib"}], "y", "Text"];
        RunProcess[{"ln", "-s", FileNameJoin[{outside, "libruntime.dylib"}], FileNameJoin[{dir, "LibraryResources", "MacOSX-ARM64", "libruntime.dylib"}]}];
        RunProcess[{"ln", "-s", "libown.dylib", FileNameJoin[{dir, "LibraryResources", "MacOSX-ARM64", "libown.1.dylib"}]}];
        links = outsideLinks[dir];
        DeleteDirectory[dir, DeleteContents -> True];
        DeleteDirectory[outside, DeleteContents -> True];
        links
    ],
    {FileNameJoin[{"LibraryResources", "MacOSX-ARM64", "libruntime.dylib"}]},
    TestID -> "Archive: a symbolic link that leads outside the paclet is found; one within it is not"
]

VerificationTest[
    Module[{from = CreateDirectory[], to = CreateDirectory[], copied},
        Scan[CreateDirectory[FileNameJoin[{from, #}], CreateIntermediateDirectories -> True] &, {"Kernel", "build", FileNameJoin[{"LibraryResources", "X"}]}];
        Scan[Export[FileNameJoin[{from, #}], "x", "Text"] &, {FileNameJoin[{"Kernel", "A.wl"}], FileNameJoin[{"build", "B.paclet"}], FileNameJoin[{"LibraryResources", "X", "keep.dylib"}], FileNameJoin[{"LibraryResources", "X", "drop.dylib"}], "PacletInfo.wl"}];
        copyTree[from, to, "", Function[path, path === "build" || StringMatchQ[path, "LibraryResources/*/drop*"]]];
        copied = Sort[Map[relativePath[to, #] &, Select[FileNames["*", to, Infinity], ! DirectoryQ[#] &]]];
        DeleteDirectory[from, DeleteContents -> True];
        DeleteDirectory[to, DeleteContents -> True];
        copied
    ],
    Sort[{FileNameJoin[{"Kernel", "A.wl"}], FileNameJoin[{"LibraryResources", "X", "keep.dylib"}], "PacletInfo.wl"}],
    TestID -> "Archive: the staged copy leaves out build/ and the files excluded"
]

VerificationTest[
    {
        foreignShadowQ["SystemSymbolQ::shdw: Symbol SystemSymbolQ appears in multiple contexts {CompileUtilities`Symbols`, GeneralUtilities`}; definitions in context CompileUtilities`Symbols` may shadow or be shadowed by other definitions.", "Pub`Name`"],
        foreignShadowQ["f::shdw: Symbol f appears in multiple contexts {Pub`Name`, Global`}; definitions in context Pub`Name` may shadow or be shadowed by other definitions.", "Pub`Name`"],
        foreignShadowQ["Get::noopen: Cannot open X`.", "Pub`Name`"]
    },
    {True, False, False},
    TestID -> "Archive: a shadowing message among the kernel's own packages is not the paclet's"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{forced},
        BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None];
        forced = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "UseCache" -> False];
        Length /@ KeyTake[forced, {"Converted", "Evaluated", "Cached"}]
    ]]],
    <|"Converted" -> 3, "Evaluated" -> 3, "Cached" -> 0|>,
    TestID -> "Build: without the cache, every page is converted and evaluated afresh"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{before = Kernels[], parallel, serial},
        parallel = BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> 2];
        serial = Map[notebookText, Sort[parallel["Notebooks"]]];
        {parallel["Kernels"], Length[parallel["Converted"]], parallel["Failed"], Kernels[] === before, Length[DeleteDuplicates[serial]]}
    ]]],
    {2, 3, {}, True, 3},
    TestID -> "Build: on parallel kernels, launched for the build and closed after it"
]

VerificationTest[
    {StringMatchQ[processReport[$ProcessID], ___ ~~ "Kernel" ~~ ___ ~~ "% " ~~ __], processReport[None]},
    {True, None},
    TestID -> "Archive: a process about to be stopped is reported with its share of a processor and its state"
]

VerificationTest[
    With[{busy = StartProcess[{"sh", "-c", "while :; do :; done"}]},
        Pause[3];
        With[{sample = processSample[busy["PID"]], spinning = spinningProcesses[busy["PID"]][[All, 1]]},
            KillProcess[busy];
            {MatchQ[sample, {__String}], Length[sample] <= $sampleLines, spinning === {ToString[busy["PID"]]}, processSample[None]}
        ]
    ],
    {True, True, True, None},
    TestID -> "Archive: a process that spins is sampled before it is stopped, its threads and stacks in a few lines"
]

VerificationTest[
    {
        kernelLicenseArguments[{"WolframKernel", "-pwfile", "!lm.example", "-entitlement", "O-ABC", "-runfirst", "x"}, "O-ENV"],
        kernelLicenseArguments[{"WolframKernel", "-entitlement", "O-ABC", "-script", "x.wl"}, "O-ENV"],
        kernelLicenseArguments[{"WolframKernel", "-script", "x.wl"}, " O-ENV\n"],
        kernelLicenseArguments[{"WolframKernel", "-pwfile", "mathpass"}, $Failed],
        kernelLicenseArguments[{"WolframKernel"}, ""],
        Block[{kernelLicenseArguments = Function[{}, {"-pwfile", "!lm", "-entitlement", "O-ABC"}]}, kernelScriptCommand["k", "s.wl", {"a"}]]
    },
    {
        {"-pwfile", "!lm.example", "-entitlement", "O-ABC"}, {"-pwfile", "!cloudlm.wolfram.com", "-entitlement", "O-ABC"},
        {"-pwfile", "!cloudlm.wolfram.com", "-entitlement", "O-ENV"}, {}, {},
        {"k", "-noprompt", "-pwfile", "!lm", "-entitlement", "O-ABC", "-script", "s.wl", "a"}
    },
    TestID -> "Build: a kernel started here takes this kernel's on-demand license manager and entitlement, before its script"
]

VerificationTest[
    Module[{file = FileNameJoin[{$TemporaryDirectory, CreateUUID["log-"] <> ".log"}], tail},
        Export[file, "one\n\ntwo\nthree\n", "Text"];
        tail = {logTail[file, 2], logTail[file, 9], logTail[file <> ".none", 2], logTail[None, 2]};
        DeleteFile[file];
        tail
    ],
    {"two | three", "one | two | three", "(no log)", "(no log)"},
    TestID -> "Archive: the last lines of a process's log, for one that did not record its pages"
]

VerificationTest[
    Module[{a, b, c},
        a = HoldComplete[Notebook[{Cell[BoxData[InterpretationBox["env", Pub`Private`store$12143[Pub`Private`k$12144], "x"]], "Output"], Cell[BoxData["store$12143"], "Output"]}]];
        b = HoldComplete[Notebook[{Cell[BoxData[InterpretationBox["env", Pub`Private`store$9876[Pub`Private`k$9877], "x"]], "Output"], Cell[BoxData["store$9876"], "Output"]}]];
        c = HoldComplete[Notebook[{Cell[BoxData[InterpretationBox["env", Pub`Private`store$9876[Pub`Private`store$9876], "x"]], "Output"], Cell[BoxData["store$9876"], "Output"]}]];
        {
            withoutNoise[a] === withoutNoise[b], withoutNoise[a] === withoutNoise[c],
            withoutNoise[HoldComplete[Cell["a x$1 b$22", "Text"], f[Print[1]; y$7]]]
        }
    ],
    {True, False, HoldComplete[Cell["a x$1 b$22", "Text"], f[Print[1]; local[1]]]},
    TestID -> "Deploy: a page evaluated again, its Module locals numbered by another session, keys as before; held, and text left as it is"
]

VerificationTest[
    With[{message = Function[{n, p, m}, With[{text = "\"the term \" <> \"TypeTheoryEnvironment[Environment`Private`store$" <> n <> "] in Environment`Private`" <> p <> "$" <> m <> " and store$" <> n <> "\""},
            HoldComplete[Notebook[{Cell[BoxData[RowBox[{"Failure", "[", text, "]"}]], "Output"]}]]]]},
        {
            withoutNoise[message["12345", "k", "12346"]] === withoutNoise[message["987", "k", "1000"]],
            withoutNoise[message["12345", "store", "12346"]] === withoutNoise[message["12345", "store", "12345"]],
            withoutNoise[HoldComplete[Cell["costs $1000 and x$1234y, not a$12", "Text"]]]
        }
    ],
    {True, False, HoldComplete[Cell["costs $1000 and x$1234y, not a$12", "Text"]]},
    TestID -> "Deploy: a session's symbol named inside a message's text, numbered by another session, keys as before; other dollars left as they are"
]

VerificationTest[
    {transformCode[] === transformCode[], Internal`InheritedBlock[{pacletLinks}, pacletLinks[_] := 1; transformCode[]] =!= transformCode[],
        FreeQ[transformContext[{}, {}], PacletObject["WolframInstitute/PacletPage"]["Version"]]},
    {True, True, True},
    TestID -> "Deploy: a page's transform is keyed on the transform's own code, not on PacletPage's version"
]

VerificationTest[
    With[{pinned = pinRules["Pub/Sym", {"PubSym`f", "PubSym`g"}],
          plain = HoldComplete[Notebook[{Cell[BoxData[RowBox[{"Plus", "[", "1", "]"}]], "Input"]}]],
          showing = HoldComplete[Notebook[{Cell[BoxData[RowBox[{"f", "[", "1", "]"}]], "Input"]}]]},
        Map[
            Function[held, SameQ @@ Map[
                Function[version, Block[{$transform = newTransform["/no/cache", transformContext[{version}, pinned], {version}, pinned, {}]}, transformPageContext[held]]],
                {"1.0.0", "2.0.0"}
            ]],
            {plain, showing}
        ]
    ],
    {True, False},
    TestID -> "Deploy: a page that shows none of the paclet's symbols keeps its transform when the symbols' rules change"
]

VerificationTest[
    With[{showing = HoldComplete[Notebook[{Cell[BoxData[RowBox[{"f", "[", "1", "]"}]], "Input"]}]]},
        SameQ @@ Map[
            Function[version, Block[{$transform = newTransform["/no/cache", transformContext[{version}, {}], {version}, {}, {}]}, transformPageContext[showing]]],
            {"1.0.0", "2.0.0"}
        ]
    ],
    True,
    TestID -> "Deploy: the pages of a paclet that declares no symbols, which the transform boxes none of, keep their transform when its version changes"
]

VerificationTest[
    Module[{calls = 0, result},
        result = Block[{$cloudCopyPauses = {0, 0}, $cloudCopyTime = {1, 0},
                CopyFile = Function[{from, to, opt}, calls++; If[calls === 1, Pause[10]]; CloudObject["https://c.example/obj/u/x"]]},
            cloudCopy["/no/such/file", CloudObject["https://c.example/obj/u/x"]]];
        {calls, result}
    ],
    {2, CloudObject["https://c.example/obj/u/x"]},
    TestID -> "Deploy: a cloud upload that stalls is cut at its time limit and sent again"
]

VerificationTest[
    Module[{calls = {}, read},
        read = Function[{urls, n}, AppendTo[calls, {urls, n}];
            Map[If[Length[calls] === 1 && MemberQ[{"b", "d"}, #], Missing["HTTP", 401], "body " <> #] &, urls]];
        {Block[{$refetchPauses = {0, 0}}, refetched[read, {"a", "b", "c", "d"}]], calls}
    ],
    {{"body a", "body b", "body c", "body d"}, {{{"a", "b", "c", "d"}, 16}, {{"b", "d"}, 4}}},
    TestID -> "Catalog: pages that do not read are read again, four at a time, until they do"
]

VerificationTest[
    {
        leftOutCheck[<|"Pages" -> <|"ref/A" -> <||>, "ref/B" -> <||>|>, "LeftOut" -> {}|>, "https://s.example/"]["OK"],
        Lookup[leftOutCheck[<|"Pages" -> <|"ref/A" -> <||>|>, "LeftOut" -> {"https://r.example/D/ref/B.html"}|>, "https://s.example/"], {"OK", "Detail"}],
        leftOutCheck[<|"Pages" -> <|"ref/A" -> <||>|>|>, "https://s.example/"]["OK"]
    },
    {True, {False, "1 of 2 published pages left out: https://r.example/D/ref/B.html"}, True},
    TestID -> "Check: a site whose catalog left out published pages does not pass"
]

VerificationTest[
    Block[{seen = {}, $sitesRefreshed = False},
        Block[{readyToPublishQ = Function[Null, AppendTo[seen, {ResourceSystemClient`$AsyncronousResourceInformationUpdates, PacletManager`Services`Private`doWeeklyUpdate[]}]; False]},
            DeployPacletResource["/no/such/paclet/dir"]
        ];
        {seen, ResourceSystemClient`$AsyncronousResourceInformationUpdates =!= False, $sitesRefreshed}
    ],
    {{{False, Null}}, True, False},
    TestID -> "Deploy: neither the resource system's information nor the paclet sites are refreshed asynchronously during a deploy, and a deploy that stops early reads no site"
]

VerificationTest[
    {
        AllTrue[{pacletBuildScript, archiveLoadCode, pageShareScript}, MatchQ[DownValues[#], {HoldPattern[_ :> StringJoin[$quietKernelCode, ___]], ___}] &],
        StringContainsQ[ReadString[FileNameJoin[{$packageDirectory, "DefinitionScan.wls"}]], "doWeeklyUpdate[] := Null"],
        Block[{buildPacletDocumentation = Function[Null, {ResourceSystemClient`$AsyncronousResourceInformationUpdates, PacletManager`Services`Private`doWeeklyUpdate[]}]},
            BuildPacletDocumentation["/no/such"]]
    },
    {True, True, {False, Null}},
    TestID -> "Build: the kernels PacletPage starts, and a build, start no asynchronous site update or information refresh"
]

VerificationTest[
    Module[{package = FileNameJoin[{$packageDirectory, "PacletPage.wl"}], calls = {}},
        PacletSites[];
        Block[{$AllowInternet = True, $AllowDataUpdates = True, PacletManager`Services`Private`getPacletSiteData,
                PacletSiteUpdate = Function[Null, AppendTo[calls, {##}]; $Failed]},
            DownValues[PacletManager`Services`Private`getPacletSiteData] = {HoldPattern[PacletManager`Services`Private`getPacletSiteData[]] :>
                {{"https://never.example", "No cached index", None, {}, {}, None}}};
            Get[package]; calls]],
    {},
    TestID -> "Build: loading PacletPage leaves a stale site untouched even when data updates are allowed"
]

VerificationTest[
    Block[{PacletManager`Services`Private`getPacletSiteData},
        PacletSites[];
        DownValues[PacletManager`Services`Private`getPacletSiteData] = {HoldPattern[PacletManager`Services`Private`getPacletSiteData[]] :> {
            {"https://fresh.example", "F", None, Date[], Date[], None}, {"https://old.example", "O", None, {2020, 1, 1}, {2020, 1, 1}, None},
            {"https://never.example", "N", None, {}, {}, None}}};
        staleSites[]
    ],
    {"https://old.example", "https://never.example"},
    TestID -> "Build: the paclet sites last updated over three days ago, or never, are the stale ones considered for a build or deploy"
]

VerificationTest[
    Module[{calls = {}, site = First[PacletSites[]]},
        Block[{$sitesRefreshed = False, staleSites = ({"https://old.example"} &),
                PacletSiteUpdate = Function[{url, option}, AppendTo[calls, {url, option}]; site]},
            Block[{$AllowInternet = False, $AllowDataUpdates = True}, refreshPacletSites[]];
            Block[{$AllowInternet = True, $AllowDataUpdates = False}, refreshPacletSites[]];
            With[{before = calls},
                Block[{$AllowInternet = True, $AllowDataUpdates = True}, refreshPacletSites[]; refreshPacletSites[]];
                {before, calls}]]],
    {{}, {{"https://old.example", Asynchronous -> False}}},
    TestID -> "Build: stale sites are refreshed synchronously once, only when both internet and data updates are enabled"
]

VerificationTest[
    dependencyName /@ {"A/B", "C/D" -> "1.0", {"E/F", "2.0"}, <|"Name" -> "G/H", "Description" -> "x", "Resource" -> "https://example.org/r"|>, 7},
    {"A/B", "C/D", "E/F", "G/H"},
    TestID -> "Build: a dependency declared as an association is named by its Name"
]

(* a page whose code never yields is not stopped by TimeConstrained: its kernel is killed *)
VerificationTest[
    withFixtureDocs[Function[dir, Module[{before = Kernels[], built},
        Export[FileNameJoin[{dir, "docs", "Tutorials", "Zed.md"}], "---\nTemplate: TechNote\nURI: Pub/Fixture/tutorial/Zed\n---\n\nSTUCK\n", "Text"];
        (* the parallel tools report the kernel that was killed *)
        built = Quiet[
            BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> 2, "TimeConstraint" -> 5],
            {LinkObject::linkd, KernelObject::rdead, LaunchKernels::req, LaunchKernels::clone}
        ];
        {Length[built["Converted"]], FileNameTake /@ built["Failed"], Kernels[] === before}
    ]]],
    {3, {"Zed.md"}, True},
    {BuildPacletDocumentation::page},
    TimeConstraint -> 240,
    TestID -> "Build: a page that never yields is stopped by killing its parallel kernel, and the build goes on"
]

VerificationTest[
    withFixtureDocs[Function[dir, Module[{built},
        Export[FileNameJoin[{dir, "docs", "Tutorials", "Zed.md"}], "---\nTemplate: TechNote\nURI: Pub/Fixture/tutorial/Zed\n---\n\nSTUCKONCE\n", "Text"];
        built = Quiet[
            BuildPacletDocumentation[dir, "Converter" -> fakeConvert, "Definition" -> None, "Parallel" -> 2, "TimeConstraint" -> 5],
            {LinkObject::linkd, KernelObject::rdead, LaunchKernels::req, LaunchKernels::clone}
        ];
        {Length[built["Converted"]], built["Failed"]}
    ]]],
    {4, {}},
    TimeConstraint -> 240,
    TestID -> "Build: a page stopped past its time once is converted on its second try"
]

(* === examples on another kernel: a converter that evaluates each line of its page after "EVAL: "
   with the evaluator it is given, a "\n" in it a newline, and writes their outputs; a paclet
   whose package loads on any kernel but this one === *)

fakeEvaluatingConvert[file_String, out_String, opts : OptionsPattern[]] := Module[
    {text = Import[file, "Text"], evaluator = Lookup[Association[Cases[{opts}, _Rule]], "Evaluator", None], outs = {}},
    If[ AssociationQ[evaluator],
        evaluator["Start"][{"System`"}];
        outs = Flatten[Map[evaluator["Evaluate"][StringReplace[#, "\\n" -> "\n"], <||>]["outs"] &,
            StringCases[text, StartOfLine ~~ "EVAL: " ~~ code : Except["\n"] .. :> code]]]
    ];
    Put[Notebook[Prepend[Map[Cell[BoxData[#], "Output"] &, outs], Cell[text, "Text"]]], out];
    out
];

notebookOutputs[file_String] := Cases[Get[file], Cell[BoxData[b_], "Output"] :> b, Infinity];

remoteOnlyPaclet[] := Module[{dir = CreateDirectory[]},
    Export[FileNameJoin[{dir, "PacletInfo.wl"}],
        "PacletObject[<|\"Name\" -> \"Pub/RemoteOnly\", \"Version\" -> \"1.0.0\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> {\"RemoteOnly`\"}}}|>]",
        "Text"];
    CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
    Export[FileNameJoin[{dir, "Kernel", "RemoteOnly.wl"}],
        "If[$ProcessID === " <> ToString[$ProcessID] <> ", Abort[]];\nBeginPackage[\"RemoteOnly`\"];\nRemoteOnly`where[] := $ProcessID;\nEndPackage[];\n",
        "Text"];
    CreateDirectory[FileNameJoin[{dir, "docs", "Symbols"}], CreateIntermediateDirectories -> True];
    dir
];

remotePage[dir_String, name_String, lines_List] := Export[FileNameJoin[{dir, "docs", "Symbols", name <> ".md"}],
    "---\nTemplate: Symbol\nURI: Pub/RemoteOnly/ref/" <> name <> "\n---\n\n" <> StringRiffle[Map["EVAL: " <> # &, lines], "\n"] <> "\n", "Text"];

(* the kernels this one launched with -wstp that are still running *)
wstpChildren[] := Select[StringSplit[RunProcess[{"ps", "-Ao", "ppid=,command="}]["StandardOutput"], "\n"],
    StringStartsQ[StringTrim[#], ToString[$ProcessID] <> " "] && StringContainsQ[#, "-wstp"] &];

VerificationTest[
    {
        evaluationSpec[None, "Symbol", "/d", None],
        evaluationSpec[<|"Symbol" -> {"k"}|>, "Guide", "/d", None],
        KeyTake[evaluationSpec[{"k", "-wstp"}, "Symbol", "/d", "C`"], {"Command", "Scope", "Paclet", "Context", "Template", "Identity"}],
        Lookup[evaluationSpec[<|"Symbol" -> <|"Command" -> {"k"}, "Scope" -> "Template", "Identity" -> "v2"|>|>, "Symbol", "/d", None], {"Scope", "Identity"}],
        {loadCheckOf[True, None, "/d"], loadCheckOf[False, {"k"}, "/d"], loadCheckOf[True, {"k"}, "/d"]["Command"], loadCheckOf[True, <|"Guide" -> None, "Symbol" -> {"g"}|>, "/d"]["Command"],
            loadCheckOf[True, <|"Paclet" -> {"p"}, "Symbol" -> {"g"}|>, "/d"]["Command"], loadCheckOf[True, <|"Guide" -> None|>, "/d"]}
    },
    {
        None,
        None,
        <|"Command" -> {"k", "-wstp"}, "Scope" -> "Page", "Paclet" -> "/d", "Context" -> "C`", "Template" -> "Symbol", "Identity" -> "k -wstp"|>,
        {"Template", "v2"},
        {True, False, {"k"}, {"g"}, {"p"}, True}
    },
    TestID -> "Evaluation kernel: a spec by command, association or template, its identity and the load check it gives"
]

VerificationTest[
    Module[{task = <|"Page" -> "p", "File" -> FindFile["WolframInstitute`PacletPage`"], "Options" -> {}|>, key},
        key[evaluation_] := pageKey[Append[task, "Evaluation" -> evaluationSpec[evaluation, "Symbol", "/d", None]], "c", "code", {}];
        {
            pageKey[task, "c", "code", {}] === key[None],
            key[None] === key[{"k"}],
            key[{"k"}] === key[<|"Command" -> {"k"}|>],
            key[{"k"}] === key[{"k2"}],
            key[{"k"}] === key[<|"Command" -> {"k"}, "Scope" -> "Template"|>],
            key[{"k"}] === key[<|"Command" -> {"k"}, "Initialization" -> Hold[x = 1]|>],
            key[{"k"}] === key[<|"Command" -> {"k"}, "Directory" -> "/d"|>],
            key[{"k"}] === key[<|"Command" -> {"k2"}, "Identity" -> "k"|>]
        }
    ],
    {True, False, True, False, False, False, False, True},
    TestID -> "Evaluation kernel: a page evaluated on another kernel is cached by the kernel its identity names, its scope, initialization and directory, and no other page's key changes"
]

VerificationTest[
    {
        cellStatements["y = 7\nz = y + 1"],
        cellStatements["(* c *)\nf[x_] :=\n  x^2\n\n f[2]  (* after *)"],
        cellStatements["x = 1;"],
        cellStatements["1 +"],
        cellStatements["(* only a comment *)"],
        cellStatements["??Expand"],
        cellStatements["a = 1\n??a  (* about a *)\n\n1 + 2 (* note *)"]
    },
    {{"y = 7", "z = y + 1"}, {"f[x_] :=\n  x^2", "f[2]"}, {"x = 1;"}, {"1 +"}, {}, {"??Expand"}, {"a = 1", "??a", "1 + 2"}},
    TestID -> "Evaluation kernel: a cell is entered a statement at a time, divided at its line breaks as a front end divides it, without the comments around"
]

VerificationTest[
    linkCaptured[None, {
        Hold[TextPacket["hi\n"]], Hold[MessagePacket[Power, "infy"]], Hold[TextPacket["Power::infy: Infinite expression 1/0 encountered.\n"]],
        Hold[ExpressionPacket[BoxData[RowBox[{"a", "+", "b"}], StandardForm]]], Hold[OutputNamePacket["Out[1]= "]],
        Hold[ReturnExpressionPacket[BoxData[RowBox[{"1", "+", "x"}], StandardForm]]], Hold[ReturnTextPacket["2"]], Hold[InputNamePacket["In[2]:= "]]
    }],
    <|"outs" -> {RowBox[{"1", "+", "x"}], "\"2\""}, "msgs" -> {"Power::infy: Infinite expression 1/0 encountered."},
      "prints" -> {Cell["hi", "Print"], Cell[BoxData[RowBox[{"a", "+", "b"}]], "Print"]}, "cells" -> {}|>,
    TestID -> "Evaluation kernel: what comes back is taken as the converter takes an evaluation"
]

VerificationTest[
    Module[{link = LinkLaunch[launchLine[{kernelExecutable[], "-wstp", "-noinit"}]], captured},
        linkPackets[link, _InputNamePacket];
        linkEvaluated[link, Hold[Unprotect[Out]; Out[5] = Range[3]; Null]];
        captured = linkCaptured[link, {Hold[OutputNamePacket["Out[5]= "]], Hold[ReturnExpressionPacket[{1, 2, 3}]], Hold[InputNamePacket["In[6]:= "]]}];
        LinkClose[link];
        captured["outs"]
    ],
    {RowBox[{"{", RowBox[{"1", ",", "2", ",", "3"}], "}"}]},
    TestID -> "Evaluation kernel: a value that comes back itself, as a kernel answers a client that is not a front end, is that kernel's boxes of its Out[n]"
]

VerificationTest[
    Module[{dir = remoteOnlyPaclet[], built, a, b},
        remotePage[dir, "A", {"RemoteOnly`where[] =!= " <> ToString[$ProcessID], "y = 7\\ny + 1", "y", "$ProcessID"}];
        remotePage[dir, "B", {"{y0, y}", "$ProcessID"}];
        built = BuildPacletDocumentation[dir, "Converter" -> fakeEvaluatingConvert, "Definition" -> None, "Parallel" -> False,
            "EvaluationKernel" -> <|"Command" -> {kernelExecutable[], "-wstp", "-noinit"}, "Scope" -> "Template", "Initialization" -> Hold[Global`y0 = 1]|>];
        {a, b} = notebookOutputs /@ Sort[built["Notebooks"]];
        DeleteDirectory[dir, DeleteContents -> True];
        {Length[built["Converted"]], Most[a], First[b], Last[a] === Last[b], MemberQ[$Packages, "RemoteOnly`"], wstpChildren[]}
    ],
    {2, {"\"True\"", "\"7\"", "\"8\"", "\"7\""}, "\"{1, y}\"", True, False, {}},
    TestID -> "Evaluation kernel: the examples run where the paclet loads, a template's pages on one kernel, each from its initialization"
]

VerificationTest[
    Module[{dir = remoteOnlyPaclet[], died, silent},
        remotePage[dir, "A", {"Exit[]", "1 + 1"}];
        died = BuildPacletDocumentation[dir, "Converter" -> fakeEvaluatingConvert, "Definition" -> None, "Parallel" -> False, "UseCache" -> False,
            "EvaluationKernel" -> {kernelExecutable[], "-wstp", "-noinit"}];
        silent = Block[{$evaluationKernelStart = 3},
            BuildPacletDocumentation[dir, "Converter" -> fakeEvaluatingConvert, "Definition" -> None, "Parallel" -> False, "UseCache" -> False,
                "EvaluationKernel" -> {"/bin/sh", "-c", "exit 0"}]];
        DeleteDirectory[dir, DeleteContents -> True];
        {Length[died["Failed"]], Length[silent["Failed"]], wstpChildren[]}
    ],
    {1, 1, {}},
    {BuildPacletDocumentation::page, BuildPacletDocumentation::page},
    TestID -> "Evaluation kernel: a page whose kernel dies, or never answers, fails and leaves no kernel"
]

VerificationTest[
    Module[{dir = remoteOnlyPaclet[], out = CreateDirectory[], archive, report},
        archive = CreatePacletArchive[dir, out];
        report = archiveLoadCheckOnKernel[evaluationSpec[{kernelExecutable[], "-wstp", "-noinit"}, "Paclet", dir, None], archive, "1.0.0", "RemoteOnly`"];
        DeleteDirectory[out, DeleteContents -> True];
        DeleteDirectory[dir, DeleteContents -> True];
        {KeyTake[report, {"OK", "Loaded", "FromArchive", "Version", "Messages", "SymbolNames"}], MemberQ[$Packages, "RemoteOnly`"], wstpChildren[]}
    ],
    {<|"OK" -> True, "Loaded" -> True, "FromArchive" -> True, "Version" -> "1.0.0", "Messages" -> {}, "SymbolNames" -> {"RemoteOnly`where"}|>, False, {}},
    TestID -> "Evaluation kernel: the archive is checked to load alone on the evaluation kernel"
]

VerificationTest[
    pageNotebook["/t", "Symbol", <|"URI" -> "Pub/Name/ref/Foo"|>, "/docs/Area/foo-page.md"],
    FileNameJoin[{"/t", "ReferencePages", "Symbols", "Foo.nb"}],
    TestID -> "Publish: a page's notebook goes where its template says, named by its URI"
]


VerificationTest[
    {
        SyntaxQ[archiveLoadScript["/tmp/A \"quoted\" $x.paclet", "Pub`Name`"]],
        SyntaxQ[pacletBuildScript["/tmp/My Paclet", "/tmp/My Paclet/build"]],
        StringContainsQ[pacletBuildScript["/tmp/a\\b", "/tmp/c"], "\"/tmp/a\\\\b\""]
    },
    {True, True, True},
    TestID -> "Publish: the fresh-kernel scripts are Wolfram Language, paths quoted"
]


VerificationTest[
    With[{script = archiveLoadScript["/tmp/a.paclet", "A`", {"/deps/One", "/deps/Two"}]},
        {SyntaxQ[script], StringCount[script, "PacletDirectoryLoad[\"/deps/"], StringPosition[script, "/deps/One"][[1, 1]] < StringPosition[script, "ExtractPacletArchive"][[1, 1]]}
    ],
    {True, 2, True},
    TestID -> "Publish: a fresh kernel loads the caller's paclet directories, the dependencies, first"
]

VerificationTest[
    Block[{PacletDirectoryLoad = Function[{}, {"/src/Paclet", "/src/Paclet/", "/deps/One", "/no/such/dir"}]},
        loadedPacletDirectories["/src/Paclet"]
    ],
    {},
    TestID -> "Publish: never the paclet's own source, nor a directory that is not there"
]

(* the deploy fixes, installed with the cloud stubbed: the probe deploy fails, as on an account
   whose subscription rejects AppearanceElements *)
VerificationTest[
    Needs["PacletResource`"];
    ResourceSystemClient`Private`loadDeployResourceShingle[];
    Block[{$deployFixesInstalled = False, CloudObject = Function[Null, "PacletPageAppearanceProbe.html"], CloudDeploy = Function[Null, $Failed]},
        installDeployFixes[];
        {
            $deployFixesInstalled,
            DeployedResourceShingle`Private`processshingleData["Paclet", "id", <||>, None, "Details", "<div>a\nb</div>"],
            FreeQ[DownValues /@ deployFunctions[], AppearanceElements]
        }
    ],
    {True, "Details" -> "<p><div>a</p>\n<p>b</div></p>", True},
    TestID -> "Publish: the deploy fixes install: the Details section renders, AppearanceElements is dropped"
]

VerificationTest[
    Block[{$heroPatchInstalled = False, $apiErrorsShown = False},
        installHeroPatch[];
        showAPIErrors[];
        {$heroPatchInstalled, $apiErrorsShown}
    ],
    {True, True},
    TestID -> "Publish: the hero patch and the repository's error messages install"
]

(* the resource page's Examples section: drawn only with an example notebook, and an example
   notebook that holds no example left out of the deploy *)
VerificationTest[
    With[{file = examplesTemplate[], data = <|"Name" -> "P", "PublisherID" -> "Pub"|>},
        {StringQ[file],
         StringContainsQ[TemplateApply[XMLTemplate[File[file]], Append[data, "ExampleNotebookURL" -> "https://example.com/e.nb"]], "id=\"Examples\""],
         StringContainsQ[TemplateApply[XMLTemplate[File[file]], data], "id=\"Examples\""]}],
    {True, True, False},
    TestID -> "Publish: the resource page template draws its Examples section only with an example notebook"
]

VerificationTest[
    With[{none = Notebook[{Cell["Basic Examples", "Subsection"]}], some = Notebook[{Cell[BoxData["1"], "Input"]}]},
        Table[Block[{$examples = setting}, {examplesShownQ[none], examplesShownQ[some]}], {setting, {Automatic, True, False}}]],
    {{False, True}, {True, True}, {False, False}},
    TestID -> "Publish: an example notebook is shown when it holds an example, or as \"Examples\" says"
]

VerificationTest[
    Block[{$examplesPatchInstalled = False},
        installExamplesPatch[];
        {$examplesPatchInstalled,
         Block[{$examples = Automatic}, DeployedResourceShingle`Private`templateFile["Paclet"]] === examplesTemplate[],
         Block[{$examples = True}, DeployedResourceShingle`Private`templateFile["Paclet"]] === PacletResource`Private`shingleTemplateFile["PacletShingle"]}
    ],
    {True, True, True},
    TestID -> "Publish: the examples patch installs; the page template is the stock one for \"Examples\" -> True"
]

(* the resource page's symbols, with the stock deploy's own definitions *)
VerificationTest[
    Needs["PacletResource`"];
    Internal`InheritedBlock[{PacletResource`Private`fancyPacletSymbolBoxes},
        installSymbolBoxes["Pub/Sym", installForm["Pub/Sym", "1.2.3", "https://c.example/obj/u/Paclets/Pub__Sym-1.2.3.paclet", "https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"]];
        Cases[
            {
                PacletResource`Private`fancyPacletSymbolBoxes[HoldComplete["https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"], "Pub/Sym", "PubSym`f"],
                PacletResource`Private`fancyPacletSymbolBoxes[HoldComplete["https://wolfr.am/Pub-Sym"], "Pub/Sym", "PubSym`g"],
                PacletResource`Private`fancyPacletSymbolBoxes[HoldComplete["https://c.example/obj/u/DeployedResources/Paclet/Other/X"], "Other/X", "X`h"]
            },
            InterpretationBox[_, interpretation_, ___] :> HoldComplete[interpretation]
        ]
    ],
    {
        HoldComplete[PacletSymbol[If[Through[PacletFind["Pub/Sym"]["Version"]] === {"1.2.3"}, "Pub/Sym", PacletUninstall["Pub/Sym"]; PacletInstall["https://c.example/obj/u/Paclets/Pub__Sym-1.2.3.paclet"]], "PubSym`f"]],
        HoldComplete[PacletSymbol[If[Through[PacletFind["Pub/Sym"]["Version"]] === {"1.2.3"}, "Pub/Sym", PacletUninstall["Pub/Sym"]; PacletInstall["https://c.example/obj/u/Paclets/Pub__Sym-1.2.3.paclet"]], "PubSym`g"]],
        HoldComplete[PacletSymbol["https://c.example/obj/u/DeployedResources/Paclet/Other/X", "X`h"]]
    },
    TestID -> "Publish: with \"VersionPinned\" -> True, a symbol on the resource page copies as code that installs the version it was copied from, from that version's archive, in place of any other"
]

VerificationTest[
    Needs["PacletResource`"];
    Internal`InheritedBlock[{PacletResource`Private`fancyPacletSymbolBoxes},
        installSymbolBoxes["Pub/Sym", installForm["Pub/Sym", None, archiveURL["https://c.example/obj/u/Paclets", "Pub/Sym", None], "https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"]];
        Cases[
            {PacletResource`Private`fancyPacletSymbolBoxes[HoldComplete["https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"], "Pub/Sym", "PubSym`f"]},
            InterpretationBox[_, interpretation_, ___] :> HoldComplete[interpretation]
        ]
    ],
    {HoldComplete[PacletSymbol[If[PacletFind["Pub/Sym"] === {}, PacletInstall["https://c.example/obj/u/Paclets/Pub__Sym.paclet"], "Pub/Sym"], "PubSym`f"]]},
    TestID -> "Publish: a symbol on the resource page copies as code that names no version: it installs the latest archive where the paclet is not installed, and finds it by name after"
]

VerificationTest[
    {installForm["Pub/Sym", "1.2.3", None, "https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"], installForm["Pub/Sym", "1.2.3", None, None],
        archiveURL[None, "Pub/Sym", "1.2.3"]},
    {HoldComplete[If[Through[PacletFind["Pub/Sym"]["Version"]] === {"1.2.3"}, "Pub/Sym", PacletUninstall["Pub/Sym"];
        PacletInstall[ResourceObject["https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"]]]], None, None},
    TestID -> "Publish: without a published archive, the copied code installs the paclet from its resource"
]

VerificationTest[
    {installForm["Pub/Sym", None, None, "https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"], installForm["Pub/Sym", None, None, None],
        archiveURL["https://c.example/obj/u/Paclets/", "Pub/Sym", None], archiveURL[None, "Pub/Sym", None]},
    {HoldComplete[If[PacletFind["Pub/Sym"] === {}, PacletInstall[ResourceObject["https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"]], "Pub/Sym"]], None,
        "https://c.example/obj/u/Paclets/Pub__Sym.paclet", None},
    TestID -> "Publish: the copied code that names no version installs from the resource without a published archive, and from the latest archive with one"
]

VerificationTest[
    Lookup[Options[DeployPacletResource], "VersionPinned"],
    False,
    TestID -> "Publish: copied code names no version unless \"VersionPinned\" -> True"
]

VerificationTest[
    Needs["PacletResource`"];
    Internal`InheritedBlock[{PacletResource`Private`pacletDeclaredSymbols},
        {
            pinDeclaredSymbols["Pub/Sym", {"PubSym`f", "PubSym`g"}],
            PacletResource`Private`pacletDeclaredSymbols[<|"Name" -> "Pub/Sym", "Version" -> "0.1.0", "Extensions" -> {}|>],
            PacletResource`Private`pacletDeclaredSymbols["Pub/Sym"],
            pinDeclaredSymbols["Pub/Sym", {}]
        }
    ],
    {True, {"PubSym`f", "PubSym`g"}, {"PubSym`f", "PubSym`g"}, False},
    TestID -> "Publish: the resource page links the symbols the fresh build declares, whatever copy of the paclet the deploy finds"
]

VerificationTest[
    Module[{root = CreateDirectory[], result},
        Export[FileNameJoin[{root, "PacletInfo.wl"}],
            "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.0\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"PubSym`\", \"Symbols\" -> {\"PubSym`f\"}}, {\"Documentation\", \"Language\" -> \"English\"}}|>]", "Text"];
        result = {archiveSymbols[root], archiveSymbols[FileNameJoin[{root, "none"}]]};
        DeleteDirectory[root, DeleteContents -> True];
        result
    ],
    {{"PubSym`f"}, {}},
    TestID -> "Archive: the symbols a built paclet declares, none without its PacletInfo.wl"
]

VerificationTest[
    Needs["DefinitionNotebookClient`"];
    Internal`InheritedBlock[{DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ},
        With[{file = FindFile["WolframInstitute`PacletPage`"]},
            {
                DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ[file],
                codeLintOff[],
                DefinitionNotebookClient`Inspectors`PackagePrivate`codeInspectFileQ[file]
            }
        ]
    ],
    {True, True, False},
    TestID -> "Publish: the scrape leaves the paclet's code unlinted"
]

VerificationTest[
    Needs["DefinitionNotebookClient`"];
    Block[{DefinitionNotebookClient`CheckDefinitionNotebook = Function[notebook, Dataset[{
            <|"Level" -> "Error", "Tag" -> "NoPacletInfoFileCell"|>,
            <|"Level" -> "Error", "Tag" -> "InvalidExample"|>,
            <|"Level" -> "Warning", "Tag" -> "PacletCreatorMismatch"|>
        }]]},
        scrapeErrors[NotebookObject["fe", "nb"]]
    ],
    {"InvalidExample"},
    TestID -> "Publish: a failed scrape names the definition notebook's errors, not the PacletInfo cell a built one never has"
]

VerificationTest[
    Block[{$deployMonitor = True},
        Reap[Block[{Print = Function[Null, Sow[StringJoin[ToString /@ {##}]]]},
            stopOnCloudWriteFailure[PacletResource`Private`setDNCProgress["Deploying documentation"]; 1]
        ]]
    ],
    {1, {{"deploy: Deploying documentation"}}},
    TestID -> "Publish: a monitored deploy prints each step as it starts it"
]

VerificationTest[
    {
        releaseAllowedQ[DeployPacletResource, "1.0.0 at r", None, "abc"],
        releaseAllowedQ[DeployPacletResource, "1.0.0 at r", "abc", "abc"],
        releaseAllowedQ[DeployPacletResource, "1.0.0 at r", "abc", "def"],
        releaseAllowedQ[DeployPacletResource, "1.0.0 at r", "abc", None],
        releaseAllowedQ[DeployPacletResource, "1.0.0 at r", Missing["NoCommit"], "abc"]
    },
    {True, True, False, False, False},
    {DeployPacletResource::commit, DeployPacletResource::unknowncommit, DeployPacletResource::nocommit},
    TestID -> "Publish: the version gate lets a new version or the same commit through, and refuses the rest"
]

VerificationTest[
    Block[{$AllowInternet = False, cloudMeta},
        With[{location = CloudObject["https://c.example/obj/u/DeployedResources/Paclet/P/N"]},
            Table[
                cloudMeta[location] = metadata;
                deployedCommit[location, "1.0.0"],
                {metadata, {None,
                    <|"Name" -> "P/N", "PacletVersion" -> "0.9.0", "PacletCommit" -> "aaa"|>,
                    <|"Name" -> "P/N", "PacletVersion" -> "1.0.0", "PacletCommit" -> "bbb"|>,
                    <|"Name" -> "P/N", "PacletVersion" -> "1.0.0"|>,
                    <|"Name" -> "P/N"|>}}]]],
    {None, None, "bbb", Missing["NoCommit"], None},
    TestID -> "Publish: the commit a deployed resource records for this version"
]

VerificationTest[
    Module[{dir = CreateDirectory[], roots},
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Lib\", \"Version\" -> \"1.0.0\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"PubLib`\"}, {\"LibraryLink\"}}|>]", "Text"];
        roots = {libraryRoots[PacletObject[File[dir]]], libraryRoots[PacletObject[<|"Name" -> "P/N", "Version" -> "1"|>]]};
        DeleteDirectory[dir, DeleteContents -> True];
        roots
    ],
    {{"LibraryResources"}, {}},
    TestID -> "Archive: the library directories a paclet declares"
]

VerificationTest[
    Module[{root = CreateDirectory[], built, archive, out, declared, again, info},
        built = CreateDirectory[FileNameJoin[{root, "Pub__Sym"}]];
        CreateDirectory[FileNameJoin[{built, "Kernel"}]];
        Export[FileNameJoin[{built, "Kernel", "Sym.wl"}], "BeginPackage[\"PubSym`\"]; f; EndPackage[]", "Text"];
        Export[FileNameJoin[{built, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"Pub/Sym\", \"Version\" -> \"1.0.0\", \"Extensions\" -> {{\"Kernel\", \"Root\" -> \"Kernel\", \"Context\" -> \"PubSym`\"}}|>]", "Text"];
        archive = CreatePacletArchive[built, root];
        declared = declareSymbols[archive, built, {"PubSym`f"}, False];
        again = declareSymbols[archive, built, {"PubSym`g"}, False];
        out = CreateDirectory[];
        info = PacletObject[File[ExtractPacletArchive[archive, out]]]["Extensions"];
        DeleteDirectory[root, DeleteContents -> True];
        DeleteDirectory[out, DeleteContents -> True];
        {declared, again, Cases[info, {"Kernel", ___, "Symbols" -> s_, ___} :> s]}
    ],
    {1, None, {{"PubSym`f"}}},
    TestID -> "Archive: a paclet that declares no symbols gets its public ones, in the archive only"
]

VerificationTest[
    Module[{dir = CreateDirectory[], noisy, slow, result},
        noisy = FileNameJoin[{dir, "noisy.wl"}];
        slow = FileNameJoin[{dir, "slow.wl"}];
        Export[noisy, "Do[WriteString[\"stdout\", StringRepeat[\"x\", 1000] <> \"\\n\"]; WriteString[\"stderr\", StringRepeat[\"y\", 1000] <> \"\\n\"], {200}]; Put[42, " <> ToString[FileNameJoin[{dir, "done.wl"}], InputForm] <> "];", "Text"];
        Export[slow, "Pause[60];", "Text"];
        result = runKernelScripts[{<|"Script" -> noisy, "Deadline" -> 120|>, <|"Script" -> slow, "Deadline" -> 6|>}, 2];
        result = {Lookup[result, {noisy, slow}], FileExistsQ[FileNameJoin[{dir, "done.wl"}]]};
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {{"Finished", "TimedOut"}, True},
    TestID -> "Archive: page processes run side by side, a noisy one is not stalled, an overdue one is stopped"
]

VerificationTest[
    singleRuleBeforeExamples[Notebook[{
        Cell["notes", "Notes"],
        Cell["", "PageDelimiter"],
        Cell[CellGroupData[{Cell["Examples", "PrimaryExamplesSection"], Cell["1 + 1", "Input"]}, Open]],
        Cell["", "PageDelimiter"],
        Cell["See Also", "SeeAlsoSection"]
    }, WindowTitle -> "P"]],
    Notebook[{
        Cell["notes", "Notes"],
        Cell[CellGroupData[{Cell["Examples", "PrimaryExamplesSection"], Cell["1 + 1", "Input"]}, Open]],
        Cell["", "PageDelimiter"],
        Cell["See Also", "SeeAlsoSection"]
    }, WindowTitle -> "P"],
    TestID -> "Archive: the delimiter before Examples goes, which draws a rule of its own; others stay"
]

VerificationTest[
    Module[{dir = CreateDirectory[], git = gitExecutable[], clean, dirty, dirtier, result},
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"P/N\", \"Version\" -> \"1\"|>]", "Text"];
        result = If[ git === None,
            (* with no git on the PATH a paclet has no commit, and no message says so *)
            {gitCommit[dir] === None, True, True},
            RunProcess[{git, "-C", dir, "init", "-q"}];
            RunProcess[{git, "-C", dir, "add", "."}];
            RunProcess[{git, "-C", dir, "-c", "user.name=t", "-c", "user.email=t@example.org", "commit", "-q", "-m", "x"}];
            clean = gitCommit[dir];
            Export[FileNameJoin[{dir, "Notes.md"}], "a", "Text"];
            dirty = gitCommit[dir];
            Export[FileNameJoin[{dir, "Notes.md"}], "b", "Text"];
            dirtier = gitCommit[dir];
            {StringFreeQ[clean, "+"], StringStartsQ[dirty, clean <> "+dirty."], dirty =!= dirtier}
        ];
        DeleteDirectory[dir, DeleteContents -> True];
        result
    ],
    {True, True, True},
    TestID -> "Publish: an uncommitted paclet's commit carries a hash of its sources"
]

VerificationTest[
    Module[{dir = CreateDirectory[], before, after},
        CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := x", "Text"];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"P/N\", \"Version\" -> \"1.0.0\"|>]", "Text"];
        before = codeHash[dir];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"P/N\", \"Version\" -> \"1.0.1\"|>]", "Text"];
        after = codeHash[dir];
        DeleteDirectory[dir, DeleteContents -> True];
        before =!= after
    ],
    True,
    TestID -> "Build: a version bump changes the code the examples run against"
]

VerificationTest[
    Module[{calls = 0, catalog, once, waited},
        catalog = <|"Kind" -> "Deployed", "RootGuide" -> "guide/G",
            "Pages" -> <|"guide/G" -> <|"Kind" -> "guide", "Notebook" -> "https://example.org/obj/u/G.nb"|>|>|>;
        Block[{
            catalogQ = True &,
            anonymousBody = Function[url, "<html id=\"pp-config\">"],
            anonymousStatus = Function[url, calls++; <|"Status" -> If[calls <= 1, 404, 200], "Type" -> ""|>],
            anonymousStatuses = Function[urls, anonymousStatus /@ urls]
        },
            once = CheckPacletPage[catalog, "https://example.org/obj/u/Site", "Feedback" -> False];
            calls = 0;
            waited = CheckPacletPage[catalog, "https://example.org/obj/u/Site", "Feedback" -> False, "Wait" -> 15]
        ];
        {once["OK"], Lookup[once["Failed"], "Check"], waited["OK"], waited["Failed"]}
    ],
    {False, {"Page guide/G"}, True, {}},
    TestID -> "Check: a page the cloud has not rendered yet is checked again until it renders"
]

VerificationTest[
    Module[{catalog, report},
        catalog = <|"Kind" -> "Cloud", "RootGuide" -> "guide/G",
            "Pages" -> <|"guide/G" -> <|"Kind" -> "guide", "Notebook" -> "https://example.org/obj/u/R/Documentation/guide/G.nb",
                "HTML" -> "https://example.org/obj/u/R/Documentation/guide/G.html"|>|>|>;
        Block[{
            catalogQ = True &,
            anonymousBody = Function[url, "<html id=\"pp-config\">"],
            anonymousStatus = Function[url, <|"Status" -> If[StringEndsQ[url, ".html"], 404, 200], "Type" -> ""|>],
            anonymousStatuses = Function[urls, anonymousStatus /@ urls]
        },
            report = CheckPacletPage[catalog, "https://example.org/obj/u/Site", "Feedback" -> False]
        ];
        {report["OK"], Lookup[report["Failed"], "Check"]}
    ],
    {False, {"Page HTML guide/G"}},
    TestID -> "Check: a page whose notebook renders but whose own page is gone fails the check"
]

VerificationTest[
    {newerVersionQ["1.0.10", "1.0.9"], newerVersionQ["1.0.9", "1.0.9"], newerVersionQ["1.1", "1.0.9"], newerVersionQ["0.9.9", "1.0"], newerVersionQ["1.0.0.1", "1.0"]},
    {True, False, True, False, True},
    TestID -> "Publish: versions compare by their numbers"
]

VerificationTest[
    Block[{$CloudConnected = False}, {offlineDryRunQ[True], offlineDryRunQ[False]}],
    {True, False},
    TestID -> "Publish: a dry run without a cloud session runs offline; a deploy never does"
]


(* === feedback: a submission === *)

VerificationTest[
    {clip["  abc  ", 10], clip["abcdef", 3], clip[Missing["NotAvailable"], 5], clip[Null, 5]},
    {"abc", "abc", "", ""},
    TestID -> "Feedback: answers trimmed and clipped"
]

VerificationTest[
    {wlString["a\"b\\c"], SyntaxQ[wlString["a\"b\\c\nd"]]},
    {"\"a\\\"b\\\\c\"", True},
    TestID -> "Feedback: strings as code"
]

VerificationTest[
    {
        attachmentName[1, {"shot one.png"}, <||>, None],
        attachmentName[2, {"a.png"}, <|"OriginalFileName" -> "../../etc/passwd"|>, None],
        attachmentName[1, {}, <|"OriginalFileName" -> "/tmp/upload123"|>, None],
        attachmentName[3, {}, <||>, None]
    },
    {"shot_one.png", "passwd", "attachment-1", "attachment-3"},
    TestID -> "Feedback: attachment names: sent, original or numbered, never a path"
]

VerificationTest[
    With[{response = receiveFeedback[<|"Feedback" -> "Buy now", "Website" -> "http://spam.example"|>, "bin", "https://c.example/att", "T", ""]},
        {response["StatusCode"], StringContainsQ[response["Body"], "pp-feedback-thanks"]}
    ],
    {200, True},
    TestID -> "Feedback: a filled honeypot is thanked and stores nothing"
]

VerificationTest[
    Block[{DatabinAdd = Function[{bin, entry}, $Failed]},
        With[{response = receiveFeedback[<|"Feedback" -> "Typo"|>, "bin", "https://c.example/att", "T", "team@example.org"]},
            {response["StatusCode"], StringContainsQ[response["Body"], "id=\"pp-feedback-failed\""],
                StringContainsQ[response["Body"], "id=\"pp-feedback-thanks\""], StringContainsQ[response["Body"], "team@example.org"]}
        ]
    ],
    {500, True, False, True},
    TestID -> "Feedback: a submission that is not stored says so, with the address, and is no thank-you"
]

VerificationTest[
    Block[{Databin = fakeDatabin, DatabinAdd = Function[{bin, entry}, stored = entry; bin]},
        With[{response = receiveFeedback[<|"Feedback" -> "caf\[EAcute] \[Dash]", "Page" -> "ref/$X"|>, "bin", "https://c.example/att", "T", ""]},
            {response["StatusCode"], StringContainsQ[response["Body"], "id=\"pp-feedback-thanks\""],
                stored["Encoding"], stored["Feedback"], asciiQ[StringJoin[Select[Values[stored], StringQ]]], readEntry[stored]["Feedback"]}
        ]
    ],
    {200, True, "escaped", "caf\\u00e9 \\u2013", True, "caf\[EAcute] \[Dash]"},
    TestID -> "Feedback: a submission is stored as ASCII and thanked"
]

VerificationTest[
    Block[{Databin = fakeDatabin, DatabinAdd = Function[{bin, entry}, stored = entry; bin]},
        receiveFeedback[<|"Feedback" -> "caf" <> FromCharacterCode[{195, 169, 32, 226, 128, 147}], "Title" -> "Na" <> FromCharacterCode[{195, 175}] <> "ve"|>,
            "bin", "https://c.example/att", "T", ""];
        {stored["Feedback"], readEntry[stored]["Feedback"], readEntry[stored]["Title"]}
    ],
    {"caf\\u00e9 \\u2013", "caf\[EAcute] \[Dash]", "Na\[IDoubleDot]ve"},
    TestID -> "Feedback: text a browser posts as UTF-8 bytes is read back before it is stored"
]

VerificationTest[
    readEntry[<|"Feedback" -> "caf\\u00c3\\u00a9", "Encoding" -> "escaped"|>],
    <|"Feedback" -> "caf\[EAcute]"|>,
    TestID -> "Feedback: an entry stored with its UTF-8 bytes escaped reads back as text"
]

VerificationTest[
    With[{text = "caf\[EAcute] \[Dash] \\u00e9 typed, a \\ backslash, \|01f600, \n a new line"},
        {asciiQ[toStoredText[text]], fromStoredText[toStoredText[text]] === text}
    ],
    {True, True},
    TestID -> "Feedback: stored text reads back as it was written"
]

VerificationTest[
    {
        readEntry[<|"ID" -> "E", "Feedback" -> "caf\\u00e9", "Encoding" -> "escaped"|>],
        readEntry[<|"ID" -> "E", "Feedback" -> "caf" <> FromCharacterCode[{195, 169}]|>]
    },
    {<|"ID" -> "E", "Feedback" -> "caf\[EAcute]"|>, <|"ID" -> "E", "Feedback" -> "caf\[EAcute]"|>},
    TestID -> "Feedback: entries read back whether stored as ASCII or as bytes"
]


(* === feedback: the team === *)

VerificationTest[
    admittedQ[#, "Owner@Example.org", {"reader@other.org"}, {"team.example"}] & /@
        {"owner@example.org", "READER@other.org", "someone@team.example", "someone@evilteam.example", "someone@sub.team.example", None},
    {True, True, True, False, False, False},
    TestID -> "Feedback: the team is the account, the readers and the domains"
]

VerificationTest[
    repairText /@ {"caf" <> FromCharacterCode[{195, 169}], "caf\[EAcute]", "x" <> FromCharacterCode[{195}] <> "(", "plain"},
    {"caf\[EAcute]", "caf\[EAcute]", "x" <> FromCharacterCode[{195}] <> "(", "plain"},
    TestID -> "Feedback: the Databin's UTF-8 bytes are read back as text, anything else kept"
]

VerificationTest[
    fromPagesQ[#, {"https://c/obj/u/Site", "https://c/obj/u/SitePage"}] & /@
        {"https://c/obj/u/Site", "https://c/obj/u/SitePage?p=ref%2FX", "https://c/obj/u/Site#ref/X", "https://c/obj/u/SiteEvil", "https://c/obj/other/Site", None},
    {True, True, True, False, False, False},
    TestID -> "Feedback: requests from the site's own pages, by their Referer"
]

VerificationTest[
    {meAnswer["a@b.example", "https://c/obj/u/SitePage?p=x", {"https://c/obj/u/SitePage"}],
     meAnswer["a@b.example", "https://c/obj/u/Other", {"https://c/obj/u/SitePage"}],
     meAnswer[None, "https://c/obj/u/SitePage", {"https://c/obj/u/SitePage"}]},
    {"a@b.example", Null, Null},
    TestID -> "Feedback: Me tells the site's pages, and nobody else, who is signed in"
]

VerificationTest[
    {browserRequestQ[<|"sec-fetch-mode" -> "cors"|>], browserRequestQ[<|"user-agent" -> "Wolfram HTTPClient"|>]},
    {True, False},
    TestID -> "Feedback: a browser's request is recognized by its Sec-Fetch headers"
]

VerificationTest[
    Block[{storeStatuses = Function[{status, updates}, CloudObject[status]]},
        {
            setStatusAnswer["owner@example.org", <||>, <|"id" -> "E1", "status" -> "fixed", "note" -> "done"|>, settings]["StatusCode"],
            setStatusAnswer["owner@example.org", <|"sec-fetch-site" -> "same-origin"|>, <|"id" -> "E1", "status" -> "fixed"|>, settings]["StatusCode"],
            setStatusAnswer["someone@team.example", <||>, <|"id" -> "E1", "status" -> "closed"|>, settings]["StatusCode"],
            setStatusAnswer["stranger@else.example", <||>, <|"id" -> "E1", "status" -> "fixed"|>, settings]["StatusCode"]
        }
    ],
    {200, 403, 400, 403},
    TestID -> "Feedback: SetStatus: the team from a kernel, a known status"
]

VerificationTest[
    Module[{stored = {}},
        Block[{storeStatuses = Function[{status, updates}, AppendTo[stored, updates]; CloudObject[status]]},
            {
                setStatusAnswer["owner@example.org", <||>, <|"updates" -> "[{\"id\":\"E1\",\"status\":\"fixed\"},{\"id\":\"E2\",\"status\":\"decided\",\"note\":\"later\"}]"|>, settings]["StatusCode"],
                setStatusAnswer["owner@example.org", <||>, <|"updates" -> "[{\"id\":\"E1\",\"status\":\"fixed\"},{\"id\":\"E2\",\"status\":\"closed\"}]"|>, settings]["StatusCode"],
                setStatusAnswer["owner@example.org", <||>, <|"updates" -> "not json"|>, settings]["StatusCode"],
                stored
            }
        ]
    ],
    {200, 400, 400, {{<|"id" -> "E1", "status" -> "fixed", "note" -> ""|>, <|"id" -> "E2", "status" -> "decided", "note" -> "later"|>}}},
    {Import::jsontokenmismatch, Import::jsonhintposandchar},
    TestID -> "Feedback: SetStatus takes several statuses in one write, and stores none when one is wrong"
]

VerificationTest[
    Module[{sent = {}},
        Block[{URLExecute = Function[{request, format}, AppendTo[sent, request["Method"]]; "sent"]},
            {
                SetPacletPageFeedbackStatus["https://c.example/obj/u/Site", {"E1" -> "fixed", {"E2", "decided"}, {"E3", "follow-up", "ask"}, <|"ID" -> "E4", "Status" -> "open"|>}],
                SetPacletPageFeedbackStatus["https://c.example/obj/u/Site", {"E1" -> "fixed", "E2" -> "closed"}],
                SetPacletPageFeedbackStatus["https://c.example/obj/u/Site", {"E1" -> "fixed", 7}],
                sent,
                statusUpdate /@ {"E1" -> "fixed", {"E3", "follow-up", "ask"}, <|"ID" -> "E4", "Status" -> "open"|>}
            }
        ]
    ],
    {"sent", $Failed, $Failed, {"POST"},
        {<|"id" -> "E1", "status" -> "fixed", "note" -> ""|>, <|"id" -> "E3", "status" -> "follow-up", "note" -> "ask"|>, <|"id" -> "E4", "status" -> "open", "note" -> ""|>}},
    {SetPacletPageFeedbackStatus::status, SetPacletPageFeedbackStatus::update},
    TestID -> "Feedback: several statuses are checked first and sent in one request"
]

VerificationTest[
    {fileAnswer["owner@example.org", "../config", settings]["StatusCode"], fileAnswer["stranger@else.example", "a.png", settings]["StatusCode"]},
    {404, 403},
    TestID -> "Feedback: File: plain names, for the team"
]

VerificationTest[
    listAnswer["stranger@else.example", settings]["StatusCode"],
    403,
    TestID -> "Feedback: All: for the team"
]

VerificationTest[
    With[{html = listHTML[
            {<|"ID" -> "20261010-120000-abcdef12", "Submitted" -> "2026-10-10T12:00:00Z", "Feedback" -> "<script>alert(1)</script>",
               "Page" -> "ref/Foo", "Title" -> "Foo", "Attachments" -> "https://c/obj/u/SiteFeedback/attachments/20261010-120000-abcdef12-shot.png"|>},
            Append[settings, "Statuses" -> <|"20261010-120000-abcdef12" -> <|"Status" -> "fixed", "Note" -> "in 1.2"|>|>]
        ]},
        {
            StringContainsQ[html, "<script>alert"],
            StringContainsQ[html, "&lt;script&gt;alert(1)&lt;/script&gt;"],
            StringContainsQ[html, "<tr id=\"20261010-120000-abcdef12\">"],
            StringContainsQ[html, "class=\"copy badge s-fixed\""],
            StringContainsQ[html, "<img src=\"File?f=20261010-120000-abcdef12-shot.png\""],
            asciiQ[html]
        }
    ],
    {False, True, True, True, True, True},
    TestID -> "Feedback: the team's page: escaped entries, anchors, statuses, attachments"
]

VerificationTest[
    {
        SyntaxQ[statusSetter[settings["SetStatus"], "E1", "open", "a \"quoted\" note"]],
        SyntaxQ[entryLocator[<|"ID" -> "E1"|>, "bin123"]],
        entryLocator[<|"ID" -> "E1"|>, "bin123"]
    },
    {True, True, "SelectFirst[Dataset[Databin[\"bin123\"]], #ID === \"E1\" &]"},
    TestID -> "Feedback: the copied code is Wolfram Language"
]

VerificationTest[
    Module[{dir = CreateDirectory[], hashes},
        CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"T\"|>]", "Text"];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := x", "Text"];
        Export[FileNameJoin[{dir, "Kernel", "Usage.wl"}], "f::usage = \"one\";", "Text"];
        hashes = {codeHash[dir, {"Kernel/Usage.wl"}], codeHash[dir]};
        Export[FileNameJoin[{dir, "Kernel", "Usage.wl"}], "f::usage = \"two\";", "Text"];
        AppendTo[hashes, codeHash[dir, {"Kernel/Usage.wl"}]]; AppendTo[hashes, codeHash[dir]];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := 2 x", "Text"];
        AppendTo[hashes, codeHash[dir, {"Kernel/Usage.wl"}]];
        DeleteDirectory[dir, DeleteContents -> True];
        {hashes[[1]] === hashes[[3]], hashes[[2]] === hashes[[4]], hashes[[3]] === hashes[[5]]}
    ],
    {True, False, False},
    TestID -> "Build: a file the code hash leaves out changes no key, and any other file still does"
]

VerificationTest[
    Block[{ran = 0, held},
        held[x_] := (ran++; x);
        {digest[{Range[10], Cell[BoxData[ButtonBox["x", ButtonFunction :> FrontEndExecute[{held[1]}]]]]}] === digest[{Developer`FromPackedArray[Range[10]], Cell[BoxData[ButtonBox["x", ButtonFunction :> FrontEndExecute[{held[1]}]]]]}], ran}
    ],
    {True, 0},
    TestID -> "digest: packed arrays key as unpacked ones, and code held in the expression never runs"
]


VerificationTest[
    With[{o = DocumentationSearch`Skeletonizer`Private`skeletonizeFiles},
        {withoutLanguageData[Lookup[Options[o], "UseWolframLanguageData"]], Lookup[Options[o], "UseWolframLanguageData"]}
    ],
    {False, True},
    TestID -> "Publish: the search index asks WolframLanguageData nothing during a deploy, and the option is back after it"
]

VerificationTest[
    Module[{dir = CreateDirectory[], info, h},
        CreateDirectory[FileNameJoin[{dir, "Kernel"}]];
        info[v_, deps_] := Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\n    \"Name\" -> \"T\",\n    \"Version\" -> \"" <> v <> "\",\n    \"Dependencies\" -> " <> deps <> "\n|>]", "Text"];
        Export[FileNameJoin[{dir, "Kernel", "Code.wl"}], "f[x_] := x", "Text"];
        info["1.0.0", "{}"]; h = {codeHash[dir, {}, False], codeHash[dir]};
        info["1.0.1", "{}"]; h = Join[h, {codeHash[dir, {}, False], codeHash[dir]}];
        info["1.0.1", "{\"A\"}"]; AppendTo[h, codeHash[dir, {}, False]];
        DeleteDirectory[dir, DeleteContents -> True];
        {h[[1]] === h[[3]], h[[2]] === h[[4]], h[[3]] === h[[5]]}
    ],
    {True, False, False},
    TestID -> "Build: with CacheVersion False a new version alone changes no key, and another PacletInfo change still does"
]

VerificationTest[
    Module[{d = CloudObject["https://www.wolframcloud.com/obj/u/Doc"], calls = 0, twice, once, works},
        twice = Block[{$listingPause = 0, DirectoryQ = (True &), CloudObjects = Function[Null, calls++; $Failed]},
            {cloudListing[d], calls, cloudFiles[d], cloudDirectories[d]}];
        calls = 0;
        once = Block[{$listingPause = 0, DirectoryQ = (True &), CloudObjects = Function[Null, calls++; If[calls === 1, $Failed, {}]]},
            {cloudListing[d], calls}];
        works = Block[{$listingPause = 0, DirectoryQ = (True &),
                CloudObjects = Function[Null, If[Length[{##}] > 1, {}, {CloudObject[First[#1] <> "/a.nb"]}]]},
            cloudFiles[d]];
        {twice, once, works}
    ],
    {{$Failed, 2, $Failed, $Failed}, {{}, 2}, <|"a.nb" -> True|>},
    TestID -> "Cloud: a listing that fails is tried once more, and one that fails twice is $Failed, never an empty directory"
]

End[]
