Begin["DefinitionScan`"]

(* Run by BuildPacletDocumentation in a fresh kernel, and never read into PacletPage itself: loads a
   paclet and writes, as WXF, what the page cache's keys need of it.
       WolframKernel -noprompt -script DefinitionScan.wls in.wxf out.wxf
   in.wxf: <|"Directories" -> the paclet directories to load, "Context" -> the context to load|>.
   out.wxf:
     "Symbols"  -> each symbol of the contexts the loading made, by its full name, the hash of its
                   Language`ExtendedDefinition: its own, down, sub, up, N, format and default
                   values, its messages and its attributes, read with ReadProtected cleared; a
                   Locked and ReadProtected symbol's, the hash of the files the loading read;
     "Edges"    -> by full name, the symbols of those contexts its definition names: by full name,
                   or, for a name in a string or a name without its context, by its own name,
                   unless that is the name of a System` symbol;
     "Read"     -> the files the loading read;
     "Foreign"  -> the hash of the definitions of System` and Global` symbols the loading made
                   or changed, such as a MakeBoxes rule or SetOptions of a system function;
     "Changed"  -> the names of those symbols;
     "Contexts" -> the contexts the paclet's directories and its loading made;
   or <|"Failure" -> what went wrong|>. *)

$ProgressReporting = False;
arguments = Replace[$CommandLine, {{___, "-script", _, rest___} :> {rest}, _ -> {}}];
{in, out} = Take[arguments, -2];
input = Import[in, "WXF"];

(* a symbol's whole definition as text, every symbol in it with its context, the names a Module
   made, x$123, without their numbers, so that two loadings of the same code give the same text *)
definitionText[name_String] := Replace[ToExpression[name, InputForm, Hold], Hold[s_] :>
    Block[{$ContextPath = {"System`"}, $Context = "DefinitionScan`"},
        StringReplace[
            ToString[Language`ExtendedDefinition[s, ExcludedContexts -> {}], InputForm],
            RegularExpression["([A-Za-z][A-Za-z0-9]*)\\$[0-9]+"] -> "$1$"
        ]
    ]
];
definitionHash[name_String] := Hash[definitionText[name], "SHA256", "HexString"];

(* a symbol of the paclet's: its attributes and its definition, which ReadProtected hides, so that
   it is cleared first, in this kernel that nothing else uses. A Locked symbol's definition cannot
   be read: it stands for every file the loading read. *)
pacletText[name_String, unreadable_String] := Replace[ToExpression[name, InputForm, Hold], Hold[s_] :> With[{attributes = Attributes[s]},
    Which[
        MemberQ[attributes, Locked] && MemberQ[attributes, ReadProtected], ToString[{attributes, unreadable}, InputForm],
        MemberQ[attributes, ReadProtected], Unprotect[s]; ClearAttributes[s, ReadProtected]; ToString[attributes, InputForm] <> definitionText[name],
        True, definitionText[name]
    ]
]];

(* the paclet's contexts are the ones its directories and its loading make, those of the symbols
   its PacletInfo declares among them; a definition it makes elsewhere is one of System` or
   Global` symbols *)
foreignNames[] := Join[Names["System`*"], Names["Global`*"]];
before = AssociationMap[definitionHash, foreignNames[]];
contextsBefore = AssociationThread[Contexts[] -> True];
Scan[PacletDirectoryLoad, input["Directories"]];

read = {};
(* an event holds what Get was given, a file or a context, which FindFile resolves as Get does *)
recordRead = Function[Null, read = Join[read, Cases[{##}, name_String :> Replace[FindFile[name], {file_String :> ExpandFileName[file], _ -> Nothing}], Infinity]]];
Internal`AddHandler["GetFileEvent", recordRead];
loaded = Needs[input["Context"]] =!= $Failed && MemberQ[Contexts[], input["Context"]];
Internal`RemoveHandler["GetFileEvent", recordRead];

identifier = (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...) ~~ (("`" ~~ (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...)) ...);
ownName[name_String] := Last[StringSplit[name, "`"]];

If[ ! loaded,
    Export[out, <|"Failure" -> "Needs[\"" <> input["Context"] <> "\"] did not load it"|>, "WXF"],
    after = AssociationMap[definitionHash, foreignNames[]];
    (* the count of Module's names and the file being read change with any code *)
    changed = Sort[Select[DeleteCases[Keys[after], "$ModuleNumber" | "$InputFileName"], Lookup[before, #, None] =!= after[#] &]];
    contexts = Sort[Complement[Contexts[], Keys[contextsBefore]]];
    (* with no context on the path, Names gives every name in full *)
    names = Block[{$ContextPath = {}, $Context = "DefinitionScan`Names`"}, Catenate[Names[# <> "*"] & /@ contexts]];
    known = AssociationThread[names -> True];
    system = AssociationThread[StringReplace[Names["System`*"], StartOfString ~~ "System`" -> ""] -> True];
    byOwnName = GroupBy[names, ownName];
    references[token_String] := Which[
        KeyExistsQ[known, token], {token},
        KeyExistsQ[system, ownName[token]], {},
        True, Lookup[byOwnName, ownName[token], {}]
    ];
    unreadable = Hash[{#, FileHash[#, "SHA256"]} & /@ Union[read], "SHA256", "HexString"];
    (* one symbol at a time, so that only its hash and its edges are kept *)
    scanned = AssociationMap[
        Function[name, With[{text = pacletText[name, unreadable]},
            {Hash[text, "SHA256", "HexString"], DeleteDuplicates[Catenate[references /@ DeleteDuplicates[StringCases[text, identifier]]]]}
        ]],
        names
    ];
    Export[out, <|
        "Symbols" -> scanned[[All, 1]],
        "Edges" -> scanned[[All, 2]],
        "Read" -> Union[read],
        "Foreign" -> Hash[BinarySerialize[KeyTake[after, changed]], "SHA256", "HexString"],
        "Changed" -> changed,
        "Contexts" -> contexts
    |>, "WXF"]
];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Processes.wl"}]];
End[];
DefinitionScan`endScript[];
