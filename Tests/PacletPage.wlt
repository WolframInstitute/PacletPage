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
    <|"Title" -> "A Title", "Keywords" -> {"k1", "k2", "s1"}|>,
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
    <|"Title" -> Missing["NoTitle"], "Keywords" -> {}|>,
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
            anonymousStatus = Function[url, calls++; <|"Status" -> If[calls <= 1, 404, 200], "Type" -> ""|>]
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
    Block[{storeStatus = Function[{status, id, value, note}, CloudObject[status]]},
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
