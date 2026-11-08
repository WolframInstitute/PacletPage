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
     "Cones"    -> by full name, the digest of the symbol's cone: its own hash and those of every
                   symbol of those contexts its definition uses, a symbol of its code or a name in
                   a string of its code with no space, in full or by its own name unless that is
                   a System` symbol's, and of every symbol those use, and so on;
     "Uses", "Components", "Largest" -> the number of uses, of components of symbols that reach
                   each other, and the size of the largest;
     "Read"     -> the files the loading read;
     "Foreign"  -> the hash of the definitions of System` and Global` symbols the loading made
                   or changed, such as a MakeBoxes rule or SetOptions of a system function;
     "Changed"  -> the names of those symbols;
     "Contexts" -> the contexts the paclet's directories and its loading made;
   or <|"Failure" -> what went wrong|>. *)

$ProgressReporting = False;
(* a progress line on standard output, which the build prints when it is monitored *)
$start = AbsoluteTime[];
progress[parts___] := WriteString["stdout", ToString[Round[AbsoluteTime[] - $start]], " s: ", parts, "\n"];
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
definitionHash[name_String] := fingerprint[definitionText[name]];

(* a symbol of the paclet's: its attributes and its whole definition, held, which ReadProtected
   hides, so that it is cleared first, in this kernel that nothing else uses; None for a Locked
   symbol, whose definition cannot be read *)
pacletDefinition[name_String] := Replace[ToExpression[name, InputForm, Hold], Hold[s_] :> With[{attributes = Attributes[s]},
    Which[
        MemberQ[attributes, Locked] && MemberQ[attributes, ReadProtected], None,
        MemberQ[attributes, ReadProtected], Unprotect[s]; ClearAttributes[s, ReadProtected]; With[{d = Language`ExtendedDefinition[s, ExcludedContexts -> {}]}, HoldComplete[attributes, d]],
        True, With[{d = Language`ExtendedDefinition[s, ExcludedContexts -> {}]}, HoldComplete[attributes, d]]
    ]
]];

(* its fingerprint, the names it uses, and whether it has no definition at all. The names: every
   symbol of its code, and every name in a string of its code that has no space, as a name given to
   Symbol or ToExpression is. Its messages, usage among them, are in its fingerprint but name
   nothing: no evaluation reads them as code. A Locked symbol stands for every file the loading
   read, and names nothing. *)
pacletParts[name_String, unreadable_String] := With[{definition = pacletDefinition[name]},
    If[ definition === None,
        {fingerprint[name <> " " <> unreadable], {}, {}, False},
        With[{code = definition /. HoldPattern[Messages -> _] :> Nothing},
            {
                fingerprint[Block[{$ContextPath = {"System`"}, $Context = "DefinitionScan`"}, StringReplace[ToString[definition, InputForm], RegularExpression["([A-Za-z][A-Za-z0-9]*)\\$[0-9]+"] -> "$1$"]]],
                DeleteDuplicates @ Cases[code, s_Symbol /; AtomQ[Unevaluated[s]] :> Context[Unevaluated[s]] <> SymbolName[Unevaluated[s]], {0, Infinity}, Heads -> True],
                DeleteDuplicates @ Cases[code, text_String /; StringFreeQ[text, WhitespaceCharacter] :> text, {0, Infinity}],
                MatchQ[definition, HoldComplete[{}, Language`DefinitionList[] | Language`DefinitionList[_ -> {(_ -> {}) ...}]]]
            }
        ]
    ]
];

(* a text's fingerprint: 64 bits of CRC32 and Adler32, which cost a microsecond where SHA256 costs
   a quarter of a millisecond, whatever the text's length *)
fingerprint[text_String] := IntegerString[Hash[text, "CRC32"], 16, 8] <> IntegerString[Hash[text, "Adler32"], 16, 8];

(* the paclet's contexts are the ones its directories and its loading make, those of the symbols
   its PacletInfo declares among them; a definition it makes elsewhere is one of System` or
   Global` symbols *)
foreignNames[] := Join[Names["System`*"], Names["Global`*"]];
before = AssociationMap[definitionHash, foreignNames[]];
progress[IntegerString[Length[before]], " System` and Global` definitions hashed"];
contextsBefore = AssociationThread[Contexts[] -> True];
Scan[PacletDirectoryLoad, input["Directories"]];

read = {};
(* an event holds what Get was given, a file or a context, which FindFile resolves as Get does *)
recordRead = Function[Null, read = Join[read, Cases[{##}, name_String :> Replace[FindFile[name], {file_String :> ExpandFileName[file], _ -> Nothing}], Infinity]]];
Internal`AddHandler["GetFileEvent", recordRead];
loaded = Needs[input["Context"]] =!= $Failed && MemberQ[Contexts[], input["Context"]];
Internal`RemoveHandler["GetFileEvent", recordRead];
progress[input["Context"], If[loaded, " loaded, ", " did not load, "], IntegerString[Length[Union[read]]], " files read"];

identifier = (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...) ~~ (("`" ~~ (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...)) ...);
ownName[name_String] := Last[StringSplit[name, "`"]];
contextOf[name_String] := StringReplace[name, RegularExpression["[^`]*$"] -> ""];

If[ ! loaded,
    Export[out, <|"Failure" -> "Needs[\"" <> input["Context"] <> "\"] did not load it"|>, "WXF"],
    after = AssociationMap[definitionHash, foreignNames[]];
    (* the count of Module's names and the file being read change with any code *)
    changed = Sort[Select[DeleteCases[Keys[after], "$ModuleNumber" | "$InputFileName"], Lookup[before, #, None] =!= after[#] &]];
    contexts = Sort[Complement[Contexts[], Keys[contextsBefore]]];
    (* the paclets' own contexts: those under a primary context or a Kernel extension's context of
       a paclet the directories hold, not those of the Wolfram packages the loading autoloads; all
       the loading made where the paclets name none *)
    declared = DeleteDuplicates @ Select[Flatten @ Map[
        Function[dir, With[{paclet = PacletObject[File[dir]]},
            If[ ! PacletObjectQ[paclet], {},
                {paclet["PrimaryContext"], Cases[Replace[paclet["Extensions"], Except[_List] -> {}], {"Kernel", rules___} :> Lookup[Cases[{rules}, _Rule], "Context", {}]]}
            ]
        ]],
        input["Directories"]
    ], StringQ];
    contexts = If[ declared === {}, contexts, Select[contexts, StringStartsQ[#, declared] &]];
    inContexts = AssociationThread[contexts -> True];
    (* with no context on the path, Names gives every name in full; one call lists them all *)
    all = Block[{$ContextPath = {}, $Context = "DefinitionScan`Names`"}, Select[Names["*`*"], KeyExistsQ[inContexts, contextOf[#]] &]];
    (* A Module's local, x$123, is numbered by the run: one with no definition is left over and
       dropped, and those of one name with definitions, a closure's state, are one symbol, x$. *)
    temporaryQ[name_String] := StringMatchQ[ownName[name], RegularExpression["[A-Za-z][A-Za-z0-9]*\\$[0-9]+"]];
    definedQ[name_String] := ToExpression[name, InputForm, Function[s, OwnValues[s] =!= {} || DownValues[s] =!= {} || UpValues[s] =!= {} || SubValues[s] =!= {}, HoldAllComplete]];
    names = Select[all, ! temporaryQ[#] || definedQ[#] &];
    unreadable = fingerprint[StringRiffle[# <> " " <> FileHash[#, "SHA256", "HexString"] & /@ Union[read], "\n"]];
    (* one symbol at a time, so that only its fingerprint and the names it uses are kept, with a
       line every 15 s *)
    done = 0; next = AbsoluteTime[] + 15;
    scanned = Map[
        Function[name, With[{parts = pacletParts[name, unreadable]},
            done++;
            If[ AbsoluteTime[] > next, progress[IntegerString[done], " of ", IntegerString[Length[names]], " definitions read"]; next = AbsoluteTime[] + 15];
            parts
        ]],
        names
    ];
    (* A symbol with no definition at all, a pattern's name, is no symbol of the cones: one that
       gains a definition joins them, and the cones of the symbols that name it change. *)
    kept = Pick[names, scanned[[All, 4]], False];
    keptParts = Pick[scanned, scanned[[All, 4]], False];
    keyOf = If[temporaryQ[#], StringReplace[#, RegularExpression["\\$[0-9]+$"] -> "$"], #] & /@ kept;
    keys = DeleteDuplicates[keyOf];
    n = Length[keys];
    index = AssociationThread[keys -> Range[n]];
    keyIndex = Lookup[index, keyOf];
    known = AssociationThread[kept -> keyIndex];
    system = AssociationThread[StringReplace[Names["System`*"], StartOfString ~~ "System`" -> ""] -> True];
    byOwnName = GroupBy[kept, ownName];
    progress[IntegerString[Length[names]], " definitions read: ", IntegerString[n], " symbols with definitions in ", IntegerString[Length[contexts]], " contexts; ", IntegerString[Length[changed]], " System` and Global` symbols changed"];
    (* a name in a string: a symbol of the cones in full or by its own name, unless that is a
       System` symbol's *)
    stringReferences[token_String] := Which[
        KeyExistsQ[known, token], {known[token]},
        KeyExistsQ[system, ownName[token]], {},
        True, Lookup[known, Lookup[byOwnName, ownName[token], {}]]
    ];
    uses = Map[
        Function[parts, DeleteDuplicates[Join[
            Lookup[known, Select[parts[[2]], KeyExistsQ[known, #] &]],
            Catenate[stringReferences /@ DeleteDuplicates[Catenate[StringCases[parts[[3]], identifier]]]]
        ]]],
        keptParts
    ];
    (* by symbol: its fingerprint, or that of its locals' sorted fingerprints, and what they use *)
    grouped = GroupBy[Transpose[{keyIndex, keptParts[[All, 1]], uses}], First -> Rest];
    hashes = Table[With[{members = grouped[i]}, If[Length[members] === 1, members[[1, 1]], fingerprint[StringRiffle[Sort[members[[All, 1]]], "\n"]]]], {i, n}];
    pairs = Catenate[Table[Thread[{i, DeleteDuplicates[Catenate[grouped[i][[All, 2]]]]}], {i, n}]];
    progress[IntegerString[Length[pairs]], " uses"];
    (* Each symbol's cone, as a digest: the symbols that reach each other are one component, and a
       component's digest is that of its symbols' fingerprints and the digests of the components it
       reaches, from the components that reach none up. A change anywhere in a symbol's cone
       changes its digest. *)
    graph = AdjacencyGraph[Range[n], SparseArray[If[pairs === {}, {}, pairs -> 1], {n, n}]];
    components = ConnectedComponents[graph];
    component = ConstantArray[0, n];
    MapIndexed[(component[[#1]] = First[#2]) &, components];
    m = Length[components];
    links = If[ pairs === {}, {},
        With[{from = component[[pairs[[All, 1]]]], to = component[[pairs[[All, 2]]]]},
            DeleteDuplicates[Pick[Transpose[{from, to}], Unitize[from - to], 1]]
        ]
    ];
    reaches = Lookup[GroupBy[links, First -> Last], Range[m], {}];
    order = TopologicalSort[AdjacencyGraph[Range[m], SparseArray[If[links === {}, {}, links -> 1], {m, m}]]];
    digests = <||>;
    Scan[
        Function[c, digests[c] = fingerprint[StringRiffle[Join[
            Sort[keys[[#]] <> " " <> hashes[[#]] & /@ components[[c]]],
            Sort[Lookup[digests, reaches[[c]]]]
        ], "\n"]]],
        Reverse[order]
    ];
    progress[IntegerString[m], " components, the largest of ", IntegerString[Max[0, Length /@ components]], " symbols"];
    Export[out, <|
        "Symbols" -> AssociationThread[keys -> hashes],
        "Cones" -> AssociationThread[keys -> Lookup[digests, component]],
        "Uses" -> Length[pairs],
        "Components" -> m,
        "Largest" -> Max[Length /@ components],
        "Read" -> Union[read],
        "Foreign" -> Hash[BinarySerialize[KeyTake[after, changed]], "SHA256", "HexString"],
        "Changed" -> changed,
        "Contexts" -> contexts
    |>, "WXF"]
];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Processes.wl"}]];
End[];
DefinitionScan`endScript[];
