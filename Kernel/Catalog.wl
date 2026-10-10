(* ::Package:: *)

(* ==================== where a paclet's documentation is published ====================

   A paclet's documentation pages are published in one of two places:
   - the Paclet Repository, https://resources.wolframcloud.com/PacletRepository/resources/<Pub>/<Name>/:
     each page at .../<kind>/<Page>.html, which shows a notebook of the resource system's storage,
     named by the page's data-nb-uri attribute;
   - a paclet resource the publisher deployed to a cloud account, .../DeployedResources/Paclet/<Pub>/<Name>:
     each page at .../Documentation/<kind>/<Page>.html, beside its notebook <Page>.nb.
   kind is guide, ref or tutorial; a page's id in the site is <kind>/<Page>. Page names keep any
   "$" they have: the pages of $-named symbols are pages like any other. *)

$repositoryBase = "https://resources.wolframcloud.com/PacletRepository/resources/";

$pageKinds = {"guide", "ref", "tutorial"};

pacletNameQ[name_String] := StringMatchQ[name,
    RegularExpression["[A-Za-z0-9_-]+/[A-Za-z0-9_-]+"]];
pacletNameQ[_] := False;

resourceSpec[name_String] /; pacletNameQ[name] := <|
    "Kind" -> "Repository",
    "Name" -> name,
    "Resource" -> $repositoryBase <> name <> "/",
    "DocBase" -> $repositoryBase <> name <> "/"
|>;

resourceSpec[URL[url_String]] := resourceSpec[url];

resourceSpec[CloudObject[url_String, ___]] := resourceSpec[url];

resourceSpec[url_String] /; StringStartsQ[url, $repositoryBase] := With[
    {segments = StringSplit[StringDrop[url, StringLength[$repositoryBase]], "/"]},
    If[ Length[segments] >= 2 && pacletNameQ[segments[[1]] <> "/" <> segments[[2]]],
        resourceSpec[segments[[1]] <> "/" <> segments[[2]]],
        $Failed
    ]
];

resourceSpec[url_String] /; StringContainsQ[url, "/DeployedResources/Paclet/"] := Module[{parts, segments, base},
    parts = StringSplit[url, "/DeployedResources/Paclet/", 2];
    segments = If[Length[parts] === 2, StringSplit[Last[parts], "/"], {}];
    If[ Length[segments] < 2 || ! pacletNameQ[segments[[1]] <> "/" <> segments[[2]]], Return[$Failed, Module]];
    base = First[parts] <> "/DeployedResources/Paclet/" <> segments[[1]] <> "/" <> segments[[2]];
    <|
        "Kind" -> "Cloud",
        "Name" -> segments[[1]] <> "/" <> segments[[2]],
        "Resource" -> base <> "/",
        "DocBase" -> base <> "/Documentation/"
    |>
];

resourceSpec[_] := $Failed;

(* ==================== reading published pages ==================== *)

(* the body of a page, or Missing with the HTTP status (or the failure) when it cannot be read *)
responseText[response_Association] := If[response["StatusCode"] === 200, response["Body"], Missing["HTTP", response["StatusCode"]]];
responseText[other_] := Missing["NotRead", other];

fetchText[url_String] := responseText[URLRead[HTTPRequest[url], {"StatusCode", "Body"}, TimeConstraint -> 120]];

(* the HTTP status of many pages, sixteen at a time *)
fetchStatuses[urls_List] := Catenate @ Map[
    Function[block, URLRead[HTTPRequest /@ block, "StatusCode", TimeConstraint -> 120]],
    Partition[urls, UpTo[16]]
];

(* many pages, read sixteen at a time: a list of requests is read concurrently *)
fetchTexts[urls_List] := Catenate @ Map[
    Function[block, responseText /@ URLRead[HTTPRequest /@ block, {"StatusCode", "Body"}, TimeConstraint -> 120]],
    Partition[urls, UpTo[16]]
];

regexEscape[s_String] := StringReplace[s,
    c : ("\\" | "^" | "$" | "." | "|" | "?" | "*" | "+" | "(" | ")" | "[" | "]" | "{" | "}") :> "\\" <> c];

htmlUnescape[s_String] := StringReplace[s, {
    "&amp;" -> "&", "&lt;" -> "<", "&gt;" -> ">", "&quot;" -> "\"", "&#39;" -> "'", "&apos;" -> "'",
    "&nbsp;" -> " ",
    "&#x" ~~ h : (HexadecimalCharacter ..) ~~ ";" :> FromCharacterCode[FromDigits[h, 16]],
    "&#" ~~ d : (DigitCharacter ..) ~~ ";" :> FromCharacterCode[FromDigits[d]]
}];

(* the text of an HTML fragment: tags dropped, entities decoded, white space collapsed *)
plainText[s_String] := StringTrim @ StringReplace[
    htmlUnescape[StringReplace[s, RegularExpression["<[^>]*>"] -> " "]],
    Whitespace -> " "];

$fieldSeparator = "\:f8ff";

(* the documentation pages an HTML page links to under docBase, in page order, each once:
   {kind, page name, link text} *)
docLinks[html_String, docBase_String] := DeleteDuplicatesBy[
    Map[
        Function[fields, {fields[[1]], URLDecode[fields[[2]]], plainText[fields[[3]]]}],
        StringSplit[#, $fieldSeparator, All] & /@ StringCases[html,
            RegularExpression["(?s)<a\\s[^>]*?href=\"" <> regexEscape[docBase] <>
                "(guide|ref|tutorial)/([^\"/?#]+)\\.html\"[^>]*>(.*?)</a>"] ->
                "$1" <> $fieldSeparator <> "$2" <> $fieldSeparator <> "$3"]
    ],
    Take[#, 2] &
];

(* the notebook a Paclet Repository page shows *)
repositoryNotebook[html_String] := Replace[
    StringCases[html, RegularExpression["data-nb-uri=\"([^\"]+)\""] -> "$1", 1],
    {{url_String} :> htmlUnescape[url], _ -> Missing["NoNotebook"]}
];

(* ==================== the structure of a guide notebook ==================== *)

(* a value from rules or an association, nested keys followed in turn *)
ruleValue[expr_, {}, _] := expr;
ruleValue[a_Association, {key_, rest___}, default_] := If[KeyExistsQ[a, key], ruleValue[a[key], {rest}, default], default];
ruleValue[rules : {___Rule}, keys_List, default_] := ruleValue[Association[rules], keys, default];
ruleValue[_, _List, default_] := default;

(* the id of a page of paclet name that a paclet: URI addresses, or Nothing *)
uriPageID[name_String, uri_String] := Replace[
    StringCases[StringReplace[uri, "#" ~~ ___ ~~ EndOfString -> ""],
        StartOfString ~~ "paclet:" ~~ name ~~ "/" ~~ kind : ("guide" | "ref" | "tutorial") ~~ "/" ~~
            page : (Except["/"] ..) ~~ EndOfString :> kind <> "/" <> page],
    {{id_String} :> id, _ -> Nothing}
];

pacletURIs[expr_] := Cases[expr, uri_String /; StringStartsQ[uri, "paclet:"], Infinity];

(* the plain text of a cell's content *)
cellText[content_] := StringTrim @ StringReplace[StringJoin[Cases[{content}, s_String :> s, Infinity]], Whitespace -> " "];

(* a guide's children in the guide tree are the guides its section headings link to, in the order
   it lists them; its links are every page it links to. The tree comes from these links, never from
   a directory layout, and a guide may sit under every guide that links it. *)
guideStructure[name_String, nb_] := Module[{headings, title},
    headings = Cases[nb, Cell[c_, "GuideFunctionsSection" | "GuideFunctionsSubsection" | "GuideTOCLink", ___] :> c, Infinity];
    title = FirstCase[nb, Cell[c_, "GuideTitle", ___] :> cellText[c], Missing["NoTitle"], Infinity];
    <|
        "Children" -> Select[DeleteDuplicates[uriPageID[name, #] & /@ pacletURIs[headings]], StringStartsQ[#, "guide/"] &],
        "Links" -> DeleteDuplicates[uriPageID[name, #] & /@ pacletURIs[nb]],
        "Title" -> title
    |>
];

(* the title and the keywords a built documentation notebook records in its TaggingRules metadata *)
notebookMetadata[nb_] := With[{rules = FirstCase[nb, (TaggingRules -> rules_) :> rules, {}, {1}]},
    <|
        "Title" -> Replace[ruleValue[rules, {"Metadata", "title"}, Missing[]], Except[_String?(StringTrim[#] =!= "" &)] -> Missing["NoTitle"]],
        "Keywords" -> Select[Flatten[{ruleValue[rules, {"Metadata", "keywords"}, {}], ruleValue[rules, {"Metadata", "synonyms"}, {}]}], StringQ]
    |>
];

(* ==================== the built documentation, when it is at hand ==================== *)

(* page id -> notebook file, for a directory holding Documentation/English (a paclet), the
   English directory itself, or an installed paclet *)
localDocumentation[None | Automatic] := <||>;

localDocumentation[paclet_PacletObject] := localDocumentation[paclet["Location"]];

localDocumentation[dir_String] := Module[{root},
    root = SelectFirst[{FileNameJoin[{dir, "Documentation", "English"}], dir}, DirectoryQ[FileNameJoin[{#, "Guides"}]] || DirectoryQ[FileNameJoin[{#, "ReferencePages"}]] &, dir];
    Association @ Join[
        Map[("guide/" <> FileBaseName[#]) -> # &, FileNames["*.nb", FileNameJoin[{root, "Guides"}]]],
        Map[("ref/" <> FileBaseName[#]) -> # &, FileNames["*.nb", FileNameJoin[{root, "ReferencePages", "Symbols"}]]],
        Map[("tutorial/" <> FileBaseName[#]) -> # &, FileNames["*.nb", FileNameJoin[{root, "Tutorials"}]]]
    ]
];

localDocumentation[_] := <||>;

localNotebook[file_String] := If[FileExistsQ[file], Import[file, "NB"], Missing["NoFile", file]];

(* ==================== pages ==================== *)

(* a page with the keywords of its notebook's metadata, and its title when the page has none but
   its name: the metadata of an authoring notebook names the page rather than titling it *)
withMetadata[page_Association, meta_Association] := Join[page,
    If[StringQ[meta["Title"]] && page["Title"] === page["Name"], <|"Title" -> meta["Title"]|>, <||>],
    <|"Keywords" -> meta["Keywords"]|>
];

newPage[spec_Association, kind_String, page_String, title_String] := <|
    "Kind" -> kind,
    "Name" -> page,
    "Title" -> If[StringTrim[title] === "", page, title],
    "Keywords" -> {},
    "Guides" -> {},
    "HTML" -> spec["DocBase"] <> kind <> "/" <> page <> ".html",
    "Notebook" -> If[spec["Kind"] === "Cloud", spec["DocBase"] <> kind <> "/" <> page <> ".nb", Missing["Unresolved"]]
|>;

(* The definition notebook of a resource deployed to a cloud account sits beside its resource
   page; the site shows it as a page of its own, resource/<Name>, and opens on it. A Paclet
   Repository listing publishes no definition notebook. *)
definitionPages[spec_Association] /; spec["Kind"] === "Cloud" := With[
    {short = Last[StringSplit[spec["Name"], "/"]]},
    With[{notebook = spec["Resource"] <> short <> "-ResourceDefinition.nb"},
        If[ fetchStatuses[{notebook}] === {200},
            <|"resource/" <> short -> <|
                "Kind" -> "resource", "Name" -> short, "Title" -> spec["Name"], "Keywords" -> {}, "Guides" -> {},
                "HTML" -> spec["Resource"], "Notebook" -> notebook
            |>|>,
            <||>
        ]
    ]
];
definitionPages[_] := <||>;

pageKind[id_String] := First[StringSplit[id, "/", 2]];

pageName[id_String] := Last[StringSplit[id, "/", 2]];

(* the notebooks behind the pages ids, resolved: a Paclet Repository page names its notebook; a
   deployed resource keeps it beside the page, so its notebooks are known from the start *)
resolveNotebooks[pages_Association, ids_List] := Module[{pending, bodies},
    pending = Select[ids, MatchQ[pages[#]["Notebook"], _Missing] &];
    If[ pending === {}, Return[pages, Module]];
    bodies = fetchTexts[pages[#]["HTML"] & /@ pending];
    Join[pages, Association @ MapThread[
        #1 -> Append[pages[#1], "Notebook" -> If[StringQ[#2], repositoryNotebook[#2], Missing["NoNotebook"]]] &,
        {pending, bodies}
    ]]
];

(* the notebook expression of a page: the built file when it is at hand, else the published notebook *)
pageNotebookExpression[id_String, page_Association, local_Association] := Which[
    KeyExistsQ[local, id], localNotebook[local[id]],
    StringQ[page["Notebook"]], cloudNotebook[page["Notebook"]],
    True, Missing["NoNotebook"]
];

cloudNotebook[url_String] := With[{object = CloudObject[url]},
    If[TrueQ[FileExistsQ[object]], CloudGet[object], Missing["NoNotebook", url]]
];

(* ==================== the catalog ==================== *)

Options[PacletPageCatalog] = {
    "Documentation" -> None,
    "RootGuide" -> Automatic,
    "Title" -> Automatic,
    "Monitor" -> False
};

PacletPageCatalog[source_, opts : OptionsPattern[]] := Module[
    {spec, html, links, local, pages, listed, queue, level, walked = <||>, nb, structure, discovered, published, unpublished, monitor, root, title},
    spec = resourceSpec[source];
    If[ spec === $Failed, Message[PacletPageCatalog::resource, source]; Return[$Failed, Module]];
    monitor = TrueQ[OptionValue["Monitor"]];
    html = fetchText[spec["Resource"]];
    If[ ! StringQ[html], Message[PacletPageCatalog::page, spec["Resource"], Last[html]]; Return[$Failed, Module]];
    links = docLinks[html, spec["DocBase"]];
    If[ links === {}, Message[PacletPageCatalog::nopages, spec["Resource"]]; Return[$Failed, Module]];
    local = localDocumentation[OptionValue["Documentation"]];
    pages = Association @ Map[(#[[1]] <> "/" <> #[[2]]) -> newPage[spec, #[[1]], #[[2]], #[[3]]] &, links];
    listed = Keys[pages];

    (* walk the guides a level at a time: each one's children are its place in the tree, and a page
       only a guide links to joins the catalog *)
    queue = Select[Keys[pages], StringStartsQ[#, "guide/"] &];
    While[ queue =!= {},
        level = Select[DeleteDuplicates[queue], ! KeyExistsQ[walked, #] &];
        queue = {};
        Scan[(walked[#] = True) &, level];
        pages = resolveNotebooks[pages, level];
        Scan[
            Function[id,
                nb = pageNotebookExpression[id, pages[id], local];
                If[ MatchQ[nb, _Notebook],
                    structure = guideStructure[spec["Name"], nb];
                    pages[id] = Join[pages[id],
                        <|"Guides" -> DeleteCases[structure["Children"], id]|>,
                        If[StringQ[structure["Title"]], <|"Title" -> structure["Title"]|>, <||>]];
                    Scan[
                        Function[link,
                            If[ ! KeyExistsQ[pages, link],
                                pages[link] = newPage[spec, pageKind[link], pageName[link], pageName[link]];
                                If[pageKind[link] === "guide", AppendTo[queue, link]]
                            ]
                        ],
                        structure["Links"]
                    ]
                ];
                If[monitor, Print["PacletPageCatalog: guide ", id]]
            ],
            level
        ]
    ];

    (* a page only a guide links to joins the catalog when it is published: a guide may name a
       page that never got one, or link a page of another paclet under this paclet's name *)
    discovered = Complement[Keys[pages], listed];
    published = AssociationThread[discovered, MatchQ[#, 200] & /@ fetchStatuses[pages[#]["HTML"] & /@ discovered]];
    unpublished = Lookup[pages, Keys[Select[published, Not]], <||>][[All, "HTML"]];
    pages = KeyDrop[pages, Keys[Select[published, Not]]];

    (* the notebook behind every other page *)
    pages = resolveNotebooks[pages, Keys[pages]];
    If[monitor, Print["PacletPageCatalog: ", Length[pages], " pages resolved"]];
    Scan[
        Function[key, If[ ! StringQ[pages[key]["Notebook"]], Message[PacletPageCatalog::nonb, pages[key]["HTML"]]]],
        Keys[pages]
    ];
    pages = Select[pages, StringQ[#["Notebook"]] &];

    (* keywords, and titles a page lacks, from the built documentation *)
    Scan[
        Function[key, pages[key] = withMetadata[pages[key], notebookMetadata[localNotebook[local[key]]]]],
        Intersection[Keys[pages], Keys[local]]
    ];

    pages = Join[definitionPages[spec], pages];

    root = Replace[OptionValue["RootGuide"], {
        Automatic :> SelectFirst[
            {"guide/" <> Last[StringSplit[spec["Name"], "/"]], First[Select[Keys[pages], StringStartsQ[#, "guide/"] &], None]},
            KeyExistsQ[pages, #] &, None],
        guide_String :> If[StringStartsQ[guide, "guide/"], guide, "guide/" <> guide]
    }];
    title = Replace[OptionValue["Title"],
        Automatic :> If[KeyExistsQ[pages, root], pages[root]["Title"], Last[StringSplit[spec["Name"], "/"]]]];
    <|
        "Name" -> spec["Name"],
        "Title" -> title,
        "Kind" -> spec["Kind"],
        "Resource" -> spec["Resource"],
        "DocBase" -> spec["DocBase"],
        "RootGuide" -> root,
        "Pages" -> pages,
        (* pages a guide links to that are not published, such as pages added since the last deploy *)
        "Unpublished" -> unpublished,
        "Date" -> DateString[TimeZoneConvert[Now, 0], "ISODateTime"]
    |>
];

catalogQ[catalog_Association] := And @@ (KeyExistsQ[catalog, #] & /@ {"Name", "Title", "Kind", "Resource", "DocBase", "RootGuide", "Pages"});
catalogQ[_] := False;
