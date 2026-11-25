#!/usr/bin/env wolframscript
(* Pins the converter this checkout of PacletPage carries: fetches the published MarkdownToNotebook
   and saves its definitions, the full name of its symbol and its ID as Assets/Converter.wxf, which
   builds then use with no network. With --parser and a full commit it also moves the pin of the
   parser the converter loads, the Wolfram/Parser entry of PacletInfo.wl that a build installs.
   Run it, and commit the files, when the published converter has changed and the next release is
   to use it; the release's tests and gate then test that one.

       wl -f pin-converter.wls [url] [--parser commit] *)

dir = DirectoryName[$InputFileName]
Get[FileNameJoin[{dir, "Kernel", "PacletPage.wl"}]]

args = Rest[$ScriptCommandLine]
parser = Replace[args, {{___, "--parser", commit_String, ___} :> commit, _ -> None}]
If[ parser =!= None && ! StringMatchQ[parser, RegularExpression["[0-9a-f]{40}"]],
    Print["--parser takes the parser's full 40-character commit, not ", parser]; Exit[1]]

url = FirstCase[args, u_String /; StringStartsQ[u, "https://"], WolframInstitute`PacletPage`Private`$converterURL]
pinned = WolframInstitute`PacletPage`Private`pinConverter[url]
Print["pinned: ", pinned["Name"], " from ", pinned["URL"], ", fetched ", pinned["Fetched"]]
Print["ID: ", pinned["ID"]]

(* the commit of the parser's pin, the first one after its name, replaced in the file's own text *)
If[ StringQ[parser],
    With[{info = FileNameJoin[{dir, "PacletInfo.wl"}], pattern = RegularExpression["(?s)(\"Name\" -> \"Wolfram/Parser\".*?\"Commit\" -> \")[0-9a-f]{40}"]},
        With[{text = ByteArrayToString[ReadByteArray[info], "UTF-8"]},
            If[ Length[StringPosition[text, pattern]] =!= 1, Print["PacletInfo.wl has no single Wolfram/Parser pin to move"]; Exit[1]];
            Export[info, StringToByteArray[StringReplace[text, pattern -> "$1" <> parser, 1], "UTF-8"], "Binary"];
            Print["parser pin: ", parser]
        ]
    ]
]
