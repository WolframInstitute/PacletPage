(* ::Package:: *)

(* ==================== where a paclet's documentation is published ====================

   A paclet's documentation pages are published in one of two places:
   - the Paclet Repository, https://resources.wolframcloud.com/PacletRepository/resources/<Pub>/<Name>/:
     each page at .../<kind>/<Page>.html, which shows a notebook of the resource system's storage,
     named by the page's data-nb-uri attribute;
   - a paclet resource the publisher deployed to a cloud account, .../DeployedResources/Paclet/<Pub>/<Name>:
     each page at .../Documentation/<kind>/<Page>.html, beside its notebook <Page>.nb.
   kind is guide, ref or tutorial; a page's id in the site is <kind>/<Page>, where a reference page
   other than a symbol's has a Page of its kind and name, such as format/TTE. Page names keep any
   "$" they have: the pages of $-named symbols are pages like any other. *)

$repositoryBase = "https://resources.wolframcloud.com/PacletRepository/resources/";

$pageKinds = {"guide", "ref", "tutorial"};

(* A symbol's page id is ref/<Name>. The other reference pages a template makes are ref/<kind>/<Name>,
   as their URIs end and as the cloud publishes them: a format's ref/format/<Name>, a service's
   ref/service/<Name>, and so on; a message's is ref/message/<Symbol>/<tag>. *)
symbolPageQ[id_String] := StringMatchQ[id, "ref/" ~~ Except["/"] ..];
symbolPageQ[_] := False;
otherReferencePageQ[id_String] := StringStartsQ[id, "ref/"] && ! symbolPageQ[id];
otherReferencePageQ[_] := False;
referenceKind[id_String] := First[StringSplit[StringDrop[id, 4], "/", 2]];

(* the reference directories under Documentation/English/ReferencePages besides Symbols, by the kind
   their pages' ids name, and the heading the rail lists each kind under *)
$referenceDirectories = <|"Formats" -> "format", "Services" -> "service", "Devices" -> "device",
    "Interpreters" -> "interpreter", "Entities" -> "entity", "Characters" -> "character",
    "Messages" -> "message", "Programs" -> "program"|>;
referenceKindLabel[kind_String] := Replace[First[Keys[Select[$referenceDirectories, # === kind &]], None], None :> Capitalize[kind] <> "s"];

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

resourceSpec[HoldPattern[CloudObject[url_String, ___]]] := resourceSpec[url];

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

(* Published pages are read by an external curl, a block of them in one process, never by URLRead:
   Wolfram 15.0's transfer thread, libcurllink's, crashes the kernel now and then, the more often the
   more transfers it runs at once, and a site's catalog and check request hundreds. Every request is
   anonymous, as a reader's browser makes it, and has its own two minutes. Redirects are followed as
   URLRead follows them, up to six: one to the cloud's sign-in check ends there, with the status it
   names (401 for a page closed to strangers), never at the sign-in page it leads on to. Each
   answer is <|"StatusCode", "ContentType", "Body"|> (the body when asked for), or a Failure where
   nothing came back; with no curl, every answer is a Failure and PacletPage::nocurl says why. *)
PacletPage::nocurl = "No curl was found on the PATH; PacletPage reads published pages with it. Install curl, or put it on the PATH.";

curlExecutable[] := curlExecutable[] = Replace[
    Select[FileNameJoin[{#, If[$OperatingSystem === "Windows", "curl.exe", "curl"]}] & /@ curlDirectories[], FileExistsQ],
    {{curl_, ___} :> curl, {} -> None}
];
(* the PATH's directories, and on Windows its System32, where Windows 10 and later ship curl.exe *)
curlDirectories[] := DeleteDuplicates @ Join[
    StringSplit[Replace[Environment["PATH"], Except[_String] -> ""], If[$OperatingSystem === "Windows", ";", ":"]],
    If[ $OperatingSystem === "Windows" && StringQ[Environment["SystemRoot"]], {FileNameJoin[{Environment["SystemRoot"], "System32"}]}, {}]
];

(* the answers to GET requests of the urls, read by one curl at most n at a time; bodies too when
   body is True *)
curlGet[{}, _, _] := {};
curlGet[urls : {__String}, body_?BooleanQ, n_Integer] := With[{curl = curlExecutable[]},
    If[ curl === None,
        Message[PacletPage::nocurl];
        Map[Failure["NoCurl", <|"MessageTemplate" -> "No curl to read `1` with.", "MessageParameters" -> {#}|>] &, urls],
        Module[{dir = CreateDirectory[], current = urls, answers = ConstantArray[None, Length[urls]], pending = Range[Length[urls]], hops = 0, config, out, lines},
            config = FileNameJoin[{dir, "requests"}];
            While[ pending =!= {} && hops < 7,
                hops++;
                If[FileExistsQ[config], DeleteFile[config]];
                WriteString[config, StringJoin[Map[
                    "url = " <> curlQuoted[current[[#]]] <> "\noutput = " <> curlQuoted[FileNameJoin[{dir, IntegerString[#]}]] <> "\n" &, pending]]];
                Close[config];
                out = curlRun[{curl, "--silent", "--max-time", "120", "--parallel", "--parallel-max", IntegerString[Max[1, n]],
                    "--write-out", "%{urlnum}\t%{http_code}\t%{content_type}\t%{redirect_url}\n", "--config", config}];
                lines = curlLines[out, Length[pending]];
                pending = Catenate @ MapThread[
                    Function[{index, line}, Replace[line, {
                        (* a redirect to the sign-in check: the status it names, as URLRead gives it *)
                        {status_, type_, next_String} /; 300 <= status < 400 && StringContainsQ[next, "j_spring_oauth_security_check"] :> (
                            answers[[index]] = curlAnswer[signInStatus[next], type, If[TrueQ[body], "", None]]; {}),
                        {status_, _, next_String} /; 300 <= status < 400 && next =!= "" && hops < 7 :> (current[[index]] = next; {index}),
                        {status_, type_, _} /; status > 0 :> (
                            answers[[index]] = curlAnswer[status, type, If[TrueQ[body], fileText[FileNameJoin[{dir, IntegerString[index]}], type], None]]; {}),
                        _ -> {}
                    }]],
                    {pending, lines}
                ]
            ];
            DeleteDirectory[dir, DeleteContents -> True];
            MapThread[
                Replace[#1, {
                    None :> Failure["NoResponse", <|"MessageTemplate" -> "No answer from `1`.", "MessageParameters" -> {#2}|>],
                    answer : KeyValuePattern["StatusCode" -> 401] :> ownerAnswer[#2, body, answer]
                }] &,
                {answers, urls}
            ]
        ]
    ]
];

curlRun[arguments_List] := Replace[RunProcess[arguments, "StandardOutput"], Except[_String] -> ""];

(* A page of the account the kernel is connected to that answers an anonymous read with 401, as a
   private resource's pages do, read again through the kernel's cloud session, where the catalog
   asks for it: so that a resource published privately has its catalog and its site. A check of
   what anyone can read never asks. A directory's page is its index.html, and a read the session
   cannot make keeps the anonymous answer. *)
$readAsOwner = False;
contentType[headers_List] := First[Cases[headers, {name_String, value_String} /; ToLowerCase[name] === "content-type" :> value], ""];
$ownerFetch := CloudObject`Private`authenticatedURLFetch;
$ownerURLBase := If[TrueQ[$CloudConnected] && StringQ[$UserURLBase], URLBuild[{$CloudBase, "obj", $UserURLBase}] <> "/", None];
ownerAnswer[url_String, body_, answer_] := With[{base = $ownerURLBase},
    If[ ! TrueQ[$readAsOwner] || ! StringQ[base] || ! StringStartsQ[url, base], answer,
        Replace[$ownerFetch[If[StringEndsQ[url, "/"], url <> "index.html", url], {"StatusCode", "ContentData", "Headers"}], {
            {200, data_List, headers_List} :> With[{type = contentType[headers]}, curlAnswer[200, type, If[TrueQ[body], bytesText[ByteArray[data], type], None]]],
            _ -> answer
        }]
    ]
];

(* a string as curl's config file reads it: quoted, its backslashes and quotes escaped *)
curlQuoted[s_String] := "\"" <> StringReplace[s, {"\\" -> "\\\\", "\"" -> "\\\""}] <> "\"";

(* curl's lines, "index, status, content type, redirect" for each request in the order they finished,
   read into {status, content type, redirect} in the requests' order; {0, "", ""} for no line *)
curlLines[out_String, count_Integer] := Module[{lines = ConstantArray[{0, "", ""}, count], fields, index, status},
    Scan[
        Function[line,
            fields = StringSplit[line, "\t", All];
            If[ Length[fields] >= 2,
                index = Interpreter["Integer"][fields[[1]]];
                status = Interpreter["Integer"][fields[[2]]];
                If[ IntegerQ[index] && 0 <= index < count && IntegerQ[status],
                    lines[[index + 1]] = {status, If[Length[fields] >= 3, fields[[3]], ""], If[Length[fields] >= 4, fields[[4]], ""]}]
            ]
        ],
        StringSplit[out, "\n"]
    ];
    lines
];

curlAnswer[status_Integer, type_String, None] := <|"StatusCode" -> status, "ContentType" -> type|>;
curlAnswer[status_Integer, type_String, body_String] := <|"StatusCode" -> status, "ContentType" -> type, "Body" -> body|>;

(* the status a redirect to the cloud's sign-in check names, 401 where it names none *)
signInStatus[url_String] := Replace[
    Interpreter["Integer"][First[StringCases[url, "statusCode=" ~~ code : DigitCharacter .. :> code], ""]],
    Except[_Integer] -> 401
];

(* a body's text as URLRead gives it: decoded by the charset its content type names, UTF-8 or a
   single-byte one, and one character a byte where it names none; "" for no body *)
fileText[file_String, type_String] := If[ ! FileExistsQ[file], "", bytesText[ReadByteArray[file], type]];
bytesText[bytes_, type_String] := With[{charset = ToLowerCase[First[StringCases[type, RegularExpression["(?i)charset=\\s*\"?([^;\"\\s]+)"] -> "$1"], ""]]},
    Which[
        ! ByteArrayQ[bytes], "",
        MemberQ[{"utf-8", "utf8"}, charset], Quiet[ByteArrayToString[bytes, "UTF-8"], $CharacterEncoding::utf8],
        KeyExistsQ[$singleByteCharsets, charset], FromCharacterCode[Normal[bytes], $singleByteCharsets[charset]],
        True, FromCharacterCode[Normal[bytes]]
    ]
];
$singleByteCharsets = <|"iso-8859-1" -> "ISOLatin1", "latin1" -> "ISOLatin1", "us-ascii" -> "ASCII", "windows-1252" -> "WindowsANSI"|>;

(* the body of a page, or Missing with the HTTP status (or the failure) when it cannot be read *)
responseText[response_Association] := If[response["StatusCode"] === 200, response["Body"], Missing["HTTP", response["StatusCode"]]];
responseText[other_] := Missing["NotRead", other];

fetchText[url_String] := responseText[First[curlGet[{url}, True, 1]]];

(* the HTTP status of many pages, n at a time, sixteen by default; a Failure for one with no answer *)
fetchStatuses[urls_List, n_Integer : 16] := Catenate @ Map[
    Function[block, Replace[curlGet[block, False, n], answer_Association :> answer["StatusCode"], {1}]],
    Partition[urls, UpTo[n]]
];

(* many pages, read n at a time, sixteen by default, concurrently *)
fetchTexts[urls_List, n_Integer : 16] := Catenate @ Map[
    Function[block, responseText /@ curlGet[block, True, n]],
    Partition[urls, UpTo[n]]
];

(* Many pages, the ones not read read again, four at a time, after each pause: a page the cloud is
   redeploying, as the Paclet Repository does for days after it publishes, answers as private now
   and then, and the same page reads a moment later. Read gives each url its body or Missing. *)
$refetchPauses = {5, 20};
refetched[read_, urls_List] := Module[{results = read[urls, 16]},
    Do[
        With[{failed = Flatten[Position[results, _Missing, {1}, Heads -> False]]},
            If[failed === {}, Break[]];
            Pause[pause];
            results[[failed]] = read[urls[[failed]], 4]
        ],
        {pause, $refetchPauses}
    ];
    results
];
fetchTextsAgain[urls_List] := refetched[fetchTexts, urls];
fetchStatusesAgain[urls_List] := refetched[
    Function[{u, n}, Map[If[# === 200, 200, Missing["HTTP", #]] &, fetchStatuses[u, n]]],
    urls
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

(* a link's text as the title of the page it leads to: without the right guillemet that a guide's
   heading ends its link to another guide with, after a space or a non-breaking one *)
linkTitle[html_String] := StringDelete[plainText[html], (WhitespaceCharacter | "\[NonBreakingSpace]") ... ~~ "\[RightGuillemet]" ~~ EndOfString];

(* the documentation pages an HTML page links to under docBase, in page order, each once:
   {kind, page name, link title} *)
docLinks[html_String, docBase_String] := DeleteDuplicatesBy[
    Map[
        Function[fields, {fields[[1]], URLDecode[fields[[2]]], linkTitle[fields[[3]]]}],
        StringSplit[#, $fieldSeparator, All] & /@ StringCases[html,
            RegularExpression["(?s)<a\\s[^>]*?href=\"" <> regexEscape[docBase] <>
                "(guide|ref|tutorial)/([^\"?#]+?)\\.html\"[^>]*>(.*?)</a>"] ->
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
            page : ((Except["/"] ..) ~~ (("/" ~~ (Except["/"] ..)) ...)) ~~ EndOfString :> kind <> "/" <> page],
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

(* the title and the keywords a built documentation notebook records in its TaggingRules metadata,
   whether it is a placeholder: nothing between its title and the template's closing sections, and
   whether it is an Overview page *)
notebookMetadata[nb_] := With[{rules = FirstCase[nb, (TaggingRules -> rules_) :> rules, {}, {1}]},
    <|
        "Stub" -> placeholderQ[nb],
        "Overview" -> overviewNotebookQ[nb],
        "Title" -> Replace[ruleValue[rules, {"Metadata", "title"}, Missing[]], Except[_String?(StringTrim[#] =!= "" &)] -> Missing["NoTitle"]],
        "Keywords" -> Select[Flatten[{ruleValue[rules, {"Metadata", "keywords"}, {}], ruleValue[rules, {"Metadata", "synonyms"}, {}]}], StringQ]
    |>
];

placeholderQ[nb_Notebook] := With[{styles = Cases[nb, Cell[_, style_String, ___] :> style, Infinity]},
    With[{title = FirstPosition[styles, "Title" | "TechNoteTitle", None], close = FirstPosition[styles, $closingSections, None]},
        title =!= None && close =!= None && First[close] === First[title] + 1
    ]
];
placeholderQ[_] := False;

(* an Overview page, which its metadata types as one, authoring or built *)
overviewNotebookQ[nb_Notebook] := ruleValue[FirstCase[nb, (TaggingRules -> rules_) :> rules, {}, {1}], {"Metadata", "type"}, None] === "Overview";
overviewNotebookQ[_] := False;

$closingSections = "TutorialMoreAboutSection" | "RelatedTutorialsSection" | "TutorialsSection" | "MetadataSection";

(* ==================== the built documentation, when it is at hand ==================== *)

(* page id -> notebook file, for a directory holding Documentation/English (a paclet), the
   English directory itself, or an installed paclet *)
localDocumentation[None | Automatic] := <||>;

localDocumentation[paclet_PacletObject] := localDocumentation[paclet["Location"]];

localDocumentation[dir_String] := Module[{root},
    root = SelectFirst[
        {FileNameJoin[{dir, "Documentation", "English"}], FileNameJoin[{dir, "Documentation"}], dir},
        DirectoryQ[FileNameJoin[{#, "Guides"}]] || DirectoryQ[FileNameJoin[{#, "ReferencePages"}]] || DirectoryQ[FileNameJoin[{#, "Tutorials"}]] &,
        dir
    ];
    Association @ Join[
        Map[("guide/" <> FileBaseName[#]) -> # &, FileNames["*.nb", FileNameJoin[{root, "Guides"}]]],
        Map[("ref/" <> FileBaseName[#]) -> # &, FileNames["*.nb", FileNameJoin[{root, "ReferencePages", "Symbols"}]]],
        Catenate[KeyValueMap[Function[{folder, kind},
            Map[notebookPageID[#, "ref/" <> kind <> "/" <> FileBaseName[#]] -> # &, FileNames["*.nb", FileNameJoin[{root, "ReferencePages", folder}]]]],
            $referenceDirectories]],
        Map[("tutorial/" <> FileBaseName[#]) -> # &, FileNames["*.nb", FileNameJoin[{root, "Tutorials"}]]]
    ]
];

(* the id a reference page's notebook names in its URI, with the paclet's name taken off, else the
   one its directory and its file give *)
notebookPageID[file_String, fallback_String] := With[
    {uri = Replace[fileMetadata[file], {meta_Association :> meta["URI"], _ :> ruleValue[FirstCase[localNotebook[file], (TaggingRules -> rules_) :> rules, {}, {1}], {"Metadata", "uri"}, None]}]},
    If[StringQ[uri] && StringContainsQ[uri, "/ref/"], "ref/" <> Last[StringSplit[uri, "/ref/", 2]], fallback]
];

localDocumentation[_] := <||>;

localNotebook[file_String] := If[FileExistsQ[file], Import[file, "NB"], Missing["NoFile", file]];

(* the text of a notebook file's last metadata list within its last n bytes, or None *)
metadataText[file_String, size_Integer, n_Integer] := Module[{stream, tail, text, at},
    If[ n > 32768 && size <= n / 4, Return[None, Module]];
    stream = OpenRead[file, BinaryFormat -> True];
    tail = WithCleanup[SetStreamPosition[stream, Max[0, size - n]]; ReadByteArray[stream], Close[stream]];
    If[ ! ByteArrayQ[tail], Return[None, Module]];
    text = ByteArrayToString[tail, "ISO8859-1"];
    at = StringPosition[text, "\"Metadata\""];
    If[ at === {}, None, First[StringCases[StringTake[text, {at[[-1, 1]], Min[StringLength[text], at[[-1, 1]] + 32767]}], $metadataPattern, 1], None]]
];

(* held data with nothing to evaluate: strings and numbers, in lists and rules, and the symbols True,
   False, None and Null. Every head is one of these symbols, so String[...] is refused with any other
   call; an association is refused whole, as a search of an atomic one does not reach its keys. *)
inertQ[held_HoldComplete] := FreeQ[held, _Association] &&
    FreeQ[held, Except[HoldComplete | List | Rule | True | False | None | Null, _Symbol], Heads -> True];

(* A notebook file's title and keywords, as notebookMetadata gives them, read from the end of the
   file, where a notebook writes its options after its cells, without reading the cells: a large
   paclet's catalog reads thousands of pages. None where the file's last part holds no metadata. *)
$metadataPattern = RegularExpression["(?s)\"Metadata\"\\s*->\\s*\\{(?:[^{}\"]|\"(?:[^\"\\\\]|\\\\.)*\"|\\{(?:[^{}\"]|\"(?:[^\"\\\\]|\\\\.)*\")*\\})*\\}"];
fileMetadata[file_String] := Module[{size, found, rules},
    size = If[FileExistsQ[file], FileByteCount[file], 0];
    If[ size === 0, Return[None, Module]];
    (* the last 32 KB, then more, as a notebook of many cells keeps a longer outline after its options *)
    found = Catch[Scan[With[{text = metadataText[file, size, #]}, If[StringQ[text], Throw[text, "metadata"]]] &, {32768, 131072, 524288}]; None, "metadata"];
    If[ ! StringQ[found], Return[None, Module]];
    (* parsed held and taken only as data: metadata holding anything else, code that reading the
       catalog must not run, gives the page no title or keywords of its own, and the file, which
       reading whole would run it, is not read *)
    rules = Replace[ToExpression["{" <> found <> "}", InputForm, HoldComplete], {
        held : HoldComplete[{"Metadata" -> m_List}] /; inertQ[held] :> m, _HoldComplete -> {}, _ -> None}];
    If[ ! ListQ[rules], Return[None, Module]];
    <|
        "Title" -> Replace[ruleValue[{"Metadata" -> rules}, {"Metadata", "title"}, Missing[]], Except[_String?(StringTrim[#] =!= "" &)] -> Missing["NoTitle"]],
        "Keywords" -> Select[Flatten[{ruleValue[{"Metadata" -> rules}, {"Metadata", "keywords"}, {}], ruleValue[{"Metadata" -> rules}, {"Metadata", "synonyms"}, {}]}], StringQ],
        "URI" -> Replace[ruleValue[{"Metadata" -> rules}, {"Metadata", "uri"}, None], Except[_String] -> None]
    |>
];

(* ==================== pages ==================== *)

(* a page with the keywords of its notebook's metadata, and its title when the page has none but
   its name: the metadata of an authoring notebook names the page rather than titling it *)
withMetadata[page_Association, meta_Association] := Join[page,
    If[StringQ[meta["Title"]] && page["Title"] === page["Name"], <|"Title" -> meta["Title"]|>, <||>],
    <|"Keywords" -> meta["Keywords"]|>,
    If[page["Kind"] === "tutorial", <|"Stub" -> TrueQ[meta["Stub"]], "Overview" -> TrueQ[meta["Overview"]]|>, <||>]
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

(* The paclet version a site shows with "VersionLabel" -> True: the one a deployed resource's last
   deploy records, or the newest on the Paclet Repository; None when neither can be read. *)
publishedVersion[spec_Association] /; spec["Kind"] === "Cloud" := Replace[
    cloudMeta[CloudObject[StringDrop[spec["Resource"], -1]]],
    {meta_Association :> Replace[Lookup[meta, "PacletVersion", None], Except[_String] -> None], _ -> None}
];
publishedVersion[spec_Association] /; spec["Kind"] === "Repository" := Replace[
    Select[#["Version"] & /@ PacletFindRemote[spec["Name"], UpdatePacletSites -> True], StringQ],
    {{latest_, ___} :> latest, _ -> None}
];
publishedVersion[_] := None;

pageKind[id_String] := First[StringSplit[id, "/", 2]];

pageName[id_String] := Last[StringSplit[id, "/", 2]];

(* the notebooks behind the pages ids, resolved: a Paclet Repository page names its notebook; a
   deployed resource keeps it beside the page, so its notebooks are known from the start *)
resolveNotebooks[pages_Association, ids_List] := Module[{pending, bodies},
    pending = Select[ids, MatchQ[pages[#]["Notebook"], _Missing] &];
    If[ pending === {}, Return[pages, Module]];
    bodies = fetchTextsAgain[pages[#]["HTML"] & /@ pending];
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

PacletPageCatalog[File[dir_String], opts : OptionsPattern[]] := localPacletCatalog[dir, opts];
PacletPageCatalog[paclet_PacletObject, opts : OptionsPattern[]] := With[{dir = paclet["Location"]},
    If[StringQ[dir] && DirectoryQ[dir], localPacletCatalog[dir, opts],
        Failure["PacletPageCatalog", <|"MessageTemplate" -> "`1` has no local directory.", "MessageParameters" -> {paclet}|>]]
];
PacletPageCatalog[source_, opts : OptionsPattern[]] := Block[{$readAsOwner = True}, catalogOf[source, opts]];

(* The catalog of a paclet directory's own documentation, the notebooks BuildPacletDocumentation
   writes, read from the directory alone: no cloud, no front end. Each page has its notebook's file
   as "File", and the catalog the paclet's version and description. *)
localPacletCatalog[dir_String, opts : OptionsPattern[PacletPageCatalog]] := Module[{local, paclet, catalog, rootOpt, root},
    local = If[AnyTrue[{"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ[FileNameJoin[{dir, #}]] &], localDocumentation[dir], <||>];
    If[ local === <||>,
        Return[Failure["PacletPageCatalog", <|"MessageTemplate" -> "`1` is not a paclet directory with documentation notebooks.", "MessageParameters" -> {dir}|>], Module]];
    paclet = PacletObject[File[dir]];
    catalog = localCatalog[paclet["Name"], local];
    rootOpt = OptionValue["RootGuide"];
    root = Replace[rootOpt, {
        id_String /; KeyExistsQ[catalog["Pages"], id] :> id,
        id_String /; KeyExistsQ[catalog["Pages"], "guide/" <> id] :> "guide/" <> id,
        _ -> None
    }];
    Join[catalog,
        <|
            "Version" -> Replace[paclet["Version"], Except[_String] -> ""],
            "Description" -> Replace[paclet["Description"], Except[_String] -> ""],
            "PublisherID" -> Replace[paclet["PublisherID"], Except[_String] -> ""],
            "URL" -> Replace[paclet["URL"], Except[_String] -> ""],
            "FeedbackURL" -> Replace[paclet["FeedbackURL"], Except[_String] -> ""],
            "SiteURL" -> Replace[paclet["SiteURL"], Except[_String] -> ""],
            "Directory" -> dir,
            "Pages" -> Association[KeyValueMap[#1 -> Append[#2, "File" -> local[#1]] &, catalog["Pages"]]]
        |>,
        If[StringQ[root], <|"RootGuide" -> root|>, <||>],
        Replace[OptionValue["Title"], {title_String :> <|"Title" -> title|>, _ -> <||>}]]
];
Options[catalogOf] = Options[PacletPageCatalog];
catalogOf[source_, opts : OptionsPattern[PacletPageCatalog]] := Module[
    {spec, html, links, local, pages, listed, queue, level, walked = <||>, nb, structure, discovered, published, unpublished, leftOut, monitor, root, title},
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
    published = AssociationThread[discovered, MatchQ[#, 200] & /@ fetchStatusesAgain[pages[#]["HTML"] & /@ discovered]];
    unpublished = Lookup[pages, Keys[Select[published, Not]], <||>][[All, "HTML"]];
    pages = KeyDrop[pages, Keys[Select[published, Not]]];

    (* the notebook behind every other page *)
    pages = resolveNotebooks[pages, Keys[pages]];
    If[monitor, Print["PacletPageCatalog: ", Length[pages], " pages resolved"]];
    leftOut = Keys[Select[pages, ! StringQ[#["Notebook"]] &]];
    Scan[Message[PacletPageCatalog::nonb, pages[#]["HTML"]] &, leftOut];
    leftOut = Lookup[pages, leftOut, <||>][[All, "HTML"]];
    pages = Select[pages, StringQ[#["Notebook"]] &];

    (* keywords, and titles a page lacks, from the built documentation *)
    With[{keys = Intersection[Keys[pages], Keys[local]]},
        Do[
            pages[keys[[i]]] = withMetadata[pages[keys[[i]]], notebookMetadata[localNotebook[local[keys[[i]]]]]];
            If[ monitor && (Mod[i, 200] === 0 || i === Length[keys]), Print["PacletPageCatalog: metadata of ", i, " of ", Length[keys], " pages"]],
            {i, Length[keys]}
        ]
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
        (* published pages whose notebook could not be read, read again twice: the site lacks them *)
        "LeftOut" -> leftOut,
        "Version" -> publishedVersion[spec],
        "Date" -> DateString[TimeZoneConvert[Now, 0], "ISODateTime"]
    |>
];

catalogQ[catalog_Association] := And @@ (KeyExistsQ[catalog, #] & /@ {"Name", "Title", "Kind", "Resource", "DocBase", "RootGuide", "Pages"});
catalogQ[_] := False;
