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
            "<a href=\"https://r.example/D/ref/Foo.nb\">notebook</a>"
        ],
        "https://r.example/D/"
    ],
    {{"guide", "Main", "Main Guide"}, {"ref", "Foo", "Foo"}, {"ref", "$Bar", "$Bar"}, {"tutorial", "Intro", "Intro & More"}},
    TestID -> "Catalog: the documentation links of a page, once each, $ names decoded"
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
    {uriPageID["P/Q", "paclet:P/Q/ref/Foo#anchor"], uriPageID["P/Q", "paclet:P/Q/tutorial/$Var"], uriPageID["P/Q", "paclet:ref/Plus"], uriPageID["P/Q", "paclet:P/QR/ref/Foo"]},
    {"ref/Foo", "tutorial/$Var"},
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
                {"Documentation", "English", "Tutorials", "T.nb"}
            }
        ];
        ids = Sort[Keys[localDocumentation[dir]]];
        DeleteDirectory[dir, DeleteContents -> True];
        ids
    ],
    {"guide/G", "ref/$S", "tutorial/T"},
    TestID -> "Catalog: the pages of a built paclet's documentation"
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
    StringCases[symbolsHTML[repositoryCatalog, <||>], "data-id=\"" ~~ id : Except["\""] .. ~~ "\"" :> id],
    {"ref/apple", "ref/Foo", "ref/LongCamelCaseName", "ref/$Bar"},
    TestID -> "Site: symbols alphabetically, ignoring case, $ names last"
]

VerificationTest[
    StringContainsQ[symbolsHTML[repositoryCatalog, <||>], ">Long<wbr>Camel<wbr>Case<wbr>Name</a>"],
    True,
    TestID -> "Site: a long name may break at its CamelCase steps"
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
        {"resource/Name", "guide/Name", "ref/$Var"}
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
    {"guide/Name", "", {"guide/Name", "ref/$Var"}},
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
    Module[{dir = localFixture[], script},
        script = pageShareScript[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", {FileNameJoin[{"English", "Guides", "Name.nb"}]}, "/w/r.wl", "/w/log.txt", {}];
        DeleteDirectory[dir, DeleteContents -> True];
        SyntaxQ[script]
    ],
    True,
    TestID -> "Archive: a page share's script is Wolfram Language"
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
            Cell[BoxData[TemplateBox[{Cell[TextData["Conics"]], "paclet:Pub/Name/guide/Conics"}, "OrangeLink", BaseStyle -> "GuideFunctionsSubsection"]],
                "GuideFunctionsSubsection"] /. orangeLinkRule[],
            TemplateBox[{_, {URL[u_], None}, ___}, "HyperlinkTemplate"] :> u, Infinity]],
    {"https://c.example/obj/u/DeployedResources/Paclet/Pub/Name/Documentation/guide/Conics.html"},
    TestID -> "Publish: a guide's sub-guide heading links to the deployed page"
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
        {Length[frontEnds], livingProcesses[frontEnds], closeKernels[{}], Intersection[Kernels[], kernels]}
    ],
    {2, {}, {}, {}},
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
    Module[{kernels = LaunchKernels[1], off = offGeneralMessages[], before, matched, reversed},
        DistributeDefinitions[matchGeneralMessages];
        before = First @ ParallelEvaluate[MemberQ[offGeneralMessages[], "shdw"], kernels];
        matched = First @ With[{o = off}, ParallelEvaluate[matchGeneralMessages[o]; offGeneralMessages[], kernels]];
        reversed = First @ With[{o = DeleteCases[off, "shdw"]}, ParallelEvaluate[matchGeneralMessages[o]; MemberQ[offGeneralMessages[], "shdw"], kernels]];
        CloseKernels[kernels];
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
            "Put[{Length[before], descendantProcesses[processTable[], {$ProcessID}]}, ", wlString[out], "];\n"
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
    Module[{dir = localFixture[], work = CreateDirectory[], runs},
        runs = pageShareRuns[dir, FileNameJoin[{dir, "Documentation"}], "/b/Documentation", Table["p" <> IntegerString[i] <> ".nb", {i, 100}], 4, 900, {}, work, "share"];
        DeleteDirectory[dir, DeleteContents -> True];
        DeleteDirectory[work, DeleteContents -> True];
        {Length /@ Lookup[runs, "Pages"], Lookup[First[runs], {"Deadline", "Stall"}], First[First[runs]["Progress"]]}
    ],
    {{13, 13, 13, 13, 13, 13, 13, 9}, {900 * 13 + 600, 1500}, FileNameJoin[{"/b/Documentation", "p1.nb"}]},
    TestID -> "Archive: the changed pages come in two shares a process, each with its time and the pages it builds to watch"
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
    Module[{script = FileNameJoin[{$TemporaryDirectory, CreateUUID["tree-"] <> ".wl"}], process, pid, children, start},
        Export[script, "LaunchKernels[1]; Pause[120];", "Text"];
        process = StartProcess[{kernelExecutable[], "-noprompt", "-script", script}];
        pid = process["PID"];
        start = AbsoluteTime[];
        While[ descendantProcesses[processTable[], {pid}] === {} && AbsoluteTime[] - start < 60, Pause[0.5]];
        children = descendantProcesses[processTable[], {pid}];
        killProcessTree[process];
        waitForExit[children, 10];
        DeleteFile[script];
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
        installSymbolBoxes["Pub/Sym", installForm["Pub/Sym", "https://c.example/obj/u/Paclets/Pub__Sym.paclet", "https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"]];
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
        HoldComplete[PacletSymbol[If[PacletFind["Pub/Sym"] === {}, PacletInstall["https://c.example/obj/u/Paclets/Pub__Sym.paclet"], "Pub/Sym"], "PubSym`f"]],
        HoldComplete[PacletSymbol[If[PacletFind["Pub/Sym"] === {}, PacletInstall["https://c.example/obj/u/Paclets/Pub__Sym.paclet"], "Pub/Sym"], "PubSym`g"]],
        HoldComplete[PacletSymbol["https://c.example/obj/u/DeployedResources/Paclet/Other/X", "X`h"]]
    },
    TestID -> "Publish: a symbol on the resource page copies as code that installs the paclet once, from its archive"
]

VerificationTest[
    {installForm["Pub/Sym", None, "https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"], installForm["Pub/Sym", None, None]},
    {HoldComplete[If[PacletFind["Pub/Sym"] === {}, PacletInstall[ResourceObject["https://c.example/obj/u/DeployedResources/Paclet/Pub/Sym"]], "Pub/Sym"]], None},
    TestID -> "Publish: without a published archive, the copied code installs the paclet from its resource"
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
    Block[{cloudMeta},
        cloudMeta[CloudObject["new"]] = None;
        cloudMeta[CloudObject["other"]] = <|"Name" -> "P/N", "PacletVersion" -> "0.9.0", "PacletCommit" -> "aaa"|>;
        cloudMeta[CloudObject["same"]] = <|"Name" -> "P/N", "PacletVersion" -> "1.0.0", "PacletCommit" -> "bbb"|>;
        cloudMeta[CloudObject["bare"]] = <|"Name" -> "P/N", "PacletVersion" -> "1.0.0"|>;
        cloudMeta[CloudObject["old"]] = <|"Name" -> "P/N"|>;
        deployedCommit[CloudObject[#], "1.0.0"] & /@ {"new", "other", "same", "bare", "old"}
    ],
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
    Module[{dir = CreateDirectory[], clean, dirty, dirtier},
        Export[FileNameJoin[{dir, "PacletInfo.wl"}], "PacletObject[<|\"Name\" -> \"P/N\", \"Version\" -> \"1\"|>]", "Text"];
        RunProcess[{"git", "-C", dir, "init", "-q"}];
        RunProcess[{"git", "-C", dir, "add", "."}];
        RunProcess[{"git", "-C", dir, "-c", "user.name=t", "-c", "user.email=t@example.org", "commit", "-q", "-m", "x"}];
        clean = gitCommit[dir];
        Export[FileNameJoin[{dir, "Notes.md"}], "a", "Text"];
        dirty = gitCommit[dir];
        Export[FileNameJoin[{dir, "Notes.md"}], "b", "Text"];
        dirtier = gitCommit[dir];
        DeleteDirectory[dir, DeleteContents -> True];
        {StringFreeQ[clean, "+"], StringStartsQ[dirty, clean <> "+dirty."], dirty =!= dirtier}
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


End[]
