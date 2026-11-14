#!/usr/bin/env wolframscript
(* Pins the converter this checkout of PacletPage carries: fetches the published MarkdownToNotebook
   and saves its definitions, the full name of its symbol and its ID as Assets/Converter.wxf, which
   builds then use with no network. Run it, and commit the file, when the published converter has
   changed and the next release is to use it; the release's tests and gate then test that one.

       wl -f pin-converter.wls       (or a converter's URL as its argument) *)

dir = DirectoryName[$InputFileName]
Get[FileNameJoin[{dir, "Kernel", "PacletPage.wl"}]]

url = Replace[$ScriptCommandLine, {{_, u_String, ___} :> u, _ -> WolframInstitute`PacletPage`Private`$converterURL}]
pinned = WolframInstitute`PacletPage`Private`pinConverter[url]
Print["pinned: ", pinned["Name"], " from ", pinned["URL"], ", fetched ", pinned["Fetched"]]
Print["ID: ", pinned["ID"]]
