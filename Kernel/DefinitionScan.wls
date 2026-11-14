(* no asynchronous site update or information refresh in this kernel: see PacletPage's $quietKernelCode *)
PacletManager`Services`Private`doWeeklyUpdate[] := Null;
ResourceSystemClient`$AsyncronousResourceInformationUpdates = False;

Begin["DefinitionScan`"]

(* Run by BuildPacletDocumentation in a fresh kernel, and never read into PacletPage itself: loads a
   paclet and writes, as WXF, what the page cache's keys need of it.
       WolframKernel -noprompt -script DefinitionScan.wls in.wxf out.wxf
   in.wxf: <|"Directories" -> the paclet directories to load, "Context" -> the context to load,
   "Dependencies" -> the contexts of the paclet's dependencies, loaded after it,
   "DependencyDirectories" -> their directories, "Excluded" -> patterns of files left out|>.
   out.wxf:
     "Symbols"  -> each symbol of the contexts the loading made, by its full name, the hash of its
                   Language`ExtendedDefinition: its own, down, sub, up, N, format and default
                   values, its messages and its attributes, read with ReadProtected cleared; a
                   Locked and ReadProtected symbol's, the hash of the files the loading read;
     "Cones"    -> by full name, the digest of the symbol's cone: its own hash and those of every
                   symbol of those contexts its definition uses, a symbol of its code or a name in
                   a string of its code with no space, in full or by its own name unless that is
                   a System` symbol's, and of every symbol those use, and so on;
     "Edges"    -> for each symbol, in the order of "Symbols", the positions there of the symbols
                   it uses;
     "Uses", "Components", "Largest" -> the number of uses, of components of symbols that reach
                   each other, and the size of the largest;
     "Temporaries" -> the Module locals among the symbols, x$, which no name without its context
                   is;
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

(* a System` or Global` symbol's definition as text, every symbol in it with its context, the names
   a Module made, x$123, without their numbers, so that two loadings of the same code give the same
   text; without its messages, since the kernel gives a symbol a message's text the first time the
   message is issued, which depends on files and timing no paclet's code decides *)
definitionText[name_String] := Replace[ToExpression[name, InputForm, Hold], Hold[s_] :>
    Block[{$ContextPath = {"System`"}, $Context = "DefinitionScan`"},
        StringReplace[
            ToString[Language`ExtendedDefinition[s, ExcludedContexts -> {}] /. HoldPattern[Messages -> _] :> Nothing, InputForm],
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

(* its fingerprint, the names it uses, and whether it has no definition at all, a Temporary
   attribute aside, which the name a scoping construct renamed, head$, is left with. The names: every
   symbol of its code, and every name in a string of its code that has no space, as a name given to
   Symbol or ToExpression is. Its messages, usage among them, are in its fingerprint but name
   nothing: no evaluation reads them as code. A Locked symbol stands for every file the loading
   read, and names nothing. *)
pacletParts[name_String, unreadable_String] := With[{definition = pacletDefinition[name]},
    If[ definition === None,
        {fingerprint[name <> " " <> unreadable], {}, {}, False},
        With[{code = definition /. HoldPattern[Messages -> _] :> Nothing},
            {
                fingerprint[Block[{$ContextPath = {"System`"}, $Context = "DefinitionScan`"}, StringReplace[ToString[canonical[definition], InputForm], RegularExpression["([A-Za-z][A-Za-z0-9]*)\\$[0-9]+"] -> "$1$"]]],
                DeleteDuplicates @ Cases[code, s_Symbol /; AtomQ[Unevaluated[s]] :> Context[Unevaluated[s]] <> SymbolName[Unevaluated[s]], {0, Infinity}, Heads -> True],
                DeleteDuplicates @ Cases[code, text_String /; StringFreeQ[text, WhitespaceCharacter] :> text, {0, Infinity}, Heads -> True],
                MatchQ[definition, HoldComplete[{} | {Temporary}, Language`DefinitionList[] | Language`DefinitionList[_ -> {((Attributes -> {} | {Temporary}) | (_ -> {})) ...}]]]
            }
        ]
    ]
];

(* A definition with the objects whose text differs from one loading to the next in a form that
   does not: a Graph's text shows its adjacency matrix in the order it was built, and a sparse
   array its entries in theirs. Only an object itself is one, never the code that makes one. *)
canonical[definition_] := definition /.
    g_Graph /; AtomQ[Unevaluated[g]] :> RuleCondition[{"Graph", VertexList[g], Sort[EdgeList[g]], Sort[Options[g]]}] /.
    s_SparseArray /; AtomQ[Unevaluated[s]] :> RuleCondition[{"SparseArray", Dimensions[s], Sort[ArrayRules[s]]}];

(* a text's fingerprint: 64 bits of CRC32 and Adler32, which cost a microsecond where SHA256 costs
   a quarter of a millisecond, whatever the text's length *)
fingerprint[text_String] := IntegerString[Hash[text, "CRC32"], 16, 8] <> IntegerString[Hash[text, "Adler32"], 16, 8];

(* the paclet of a directory with a PacletInfo file, else None: a directory PacletDirectoryLoad was
   given may only hold paclets, and PacletObject[File[dir]] reports CreatePaclet::badarg for it *)
pacletOf[dir_String] := If[ FileExistsQ[FileNameJoin[{dir, "PacletInfo.wl"}]] || FileExistsQ[FileNameJoin[{dir, "PacletInfo.m"}]], PacletObject[File[dir]], None];

(* the contexts a paclet directory declares: its primary context and its Kernel extensions' *)
pacletContexts[dir_String] := With[{paclet = pacletOf[dir]},
    If[ ! PacletObjectQ[paclet], {},
        DeleteDuplicates @ Select[Flatten[{paclet["PrimaryContext"], Cases[Replace[paclet["Extensions"], Except[_List] -> {}], {"Kernel", rules___} :> Lookup[Cases[{rules}, _Rule], "Context", {}]]}], StringQ]
    ]
];
declared = DeleteDuplicates[Catenate[pacletContexts /@ input["Directories"]]];

(* The paclets' contexts are those under a declared one that the loading makes; a definition a
   loading makes elsewhere is one of System` or Global` symbols. A snapshot has, by name, each
   one's fingerprint and whether it has no definition to see, as a symbol that a package of
   Wolfram's defines on first use has until then. *)
foreignNames[] := Join[Names["System`*"], Names["Global`*"]];
bareQ[name_String] := ToExpression[name, InputForm, Function[s, MatchQ[Language`ExtendedDefinition[s, ExcludedContexts -> {}],
    Language`DefinitionList[] | Language`DefinitionList[_ -> {((Attributes -> _) | (Messages -> _) | (_ -> {})) ...}]], HoldAllComplete]];
snapshot[] := AssociationMap[{definitionHash[#], bareQ[#]} &, foreignNames[]];
(* What a loading changed, between two snapshots. Left out: the count of Module's names, the
   file being read and the state of the random generator, which change with any code, and what
   names the session; and a symbol with nothing to see before whose
   definition now names nothing of the paclets, which is Wolfram's own package loaded on its first
   use, by whichever code used it first. *)
changedBetween[old_Association, new_Association] := Sort @ Select[
    DeleteCases[Keys[new], "$ModuleNumber" | "$InputFileName" | "$RandomGeneratorState" | "$SessionID" | "$ProcessID" | "$CommandLine" | "$ScriptCommandLine"],
    Function[name, Lookup[old, name, {None, False}][[1]] =!= new[name][[1]] &&
        ! (KeyExistsQ[old, name] && old[name][[2]] && (declared === {} || StringFreeQ[definitionText[name], declared]))]
];
foreignHash[new_Association, names_List] := fingerprint[StringRiffle[# <> " " <> new[#][[1]] & /@ names, "\n"]];

(* a context loaded: Needs gives no failure, and the context, or one under it, is there. The
   random generator is seeded first: a definition may hold what a random function gave when it
   was made, a default RandomGraph[...], and holds the same in every scan. *)
loadedQ[context_String] := (SeedRandom[1]; Needs[context] =!= $Failed && Contexts[context <> "*"] =!= {});

(* A build server's kernel has no cloud session, and a dependency that reaches the cloud as it
   loads would wait for a sign-in at a terminal that is not there. Where the environment names an
   account, and this is no Mac, whose kernels start as the user's saved login, the kernel connects
   as that account, as the build's own kernels do there. *)
If[ $OperatingSystem =!= "MacOSX" && StringQ[Environment["WOLFRAM_CLOUD_USER"]] && StringQ[Environment["WOLFRAM_CLOUD_PASSWORD"]],
    progress[If[StringQ[CloudConnect[Environment["WOLFRAM_CLOUD_USER"], Environment["WOLFRAM_CLOUD_PASSWORD"]]], "connected to the cloud", "no cloud connection"]]
];

before = snapshot[];
progress[IntegerString[Length[before]], " System` and Global` definitions hashed"];
contextsBefore = AssociationThread[Contexts[] -> True];
Scan[PacletDirectoryLoad, input["Directories"]];

read = {};
(* an event holds what Get was given, a file or a context, which FindFile resolves as Get does *)
recordRead = Function[Null, read = Join[read, Cases[{##}, name_String :> Replace[FindFile[name], {file_String :> ExpandFileName[file], _ -> Nothing}], Infinity]]];
Internal`AddHandler["GetFileEvent", recordRead];
(* The dependencies, all the way down, are loaded first, also one the paclet loads only on first
   use: their symbols are symbols of the cones, and what each one's loading changes of System`
   and Global` is part of its own symbols. One that does not load is named in "Unloaded". *)
unloaded = {};
dependencyForeign = <||>;
dependencyChanged = <||>;
outranked = {};
Scan[
    Function[dependency, With[{context = First[pacletContexts[dependency], None]},
        If[ StringQ[context],
            progress["loading ", context];
            If[ ! loadedQ[context], AppendTo[unloaded, context]];
            (* a newer version installed outranks the directory given: what loaded is not it *)
            With[{found = With[{own = pacletOf[dependency]}, If[ PacletObjectQ[own], PacletObject[own["Name"]], None]]},
                If[ PacletObjectQ[found] && ExpandFileName[found["Location"]] =!= ExpandFileName[dependency],
                    AppendTo[outranked, context -> found["Version"]]; unloaded = Union[unloaded, {context}]]
            ];
            With[{now = snapshot[]},
                With[{names = changedBetween[before, now]},
                    dependencyChanged[dependency] = AssociationMap[now[#][[1]] &, names];
                    dependencyForeign[dependency] = foreignHash[now, names]
                ];
                before = now
            ]
        ]
    ]],
    Lookup[input, "DependencyDirectories", {}]
];
If[ Lookup[input, "DependencyDirectories", {}] =!= {},
    progress[IntegerString[Length[dependencyForeign] - Length[unloaded]], " dependencies loaded", If[unloaded === {}, "", ", not " <> StringRiffle[unloaded, ", "]]]];
loaded = loadedQ[input["Context"]];
Internal`RemoveHandler["GetFileEvent", recordRead];
(* the files read that are a paclet's: a loading also reads files of Wolfram's and temporary ones *)
read = With[{roots = ExpandFileName /@ input["Directories"]}, Select[Union[read], StringStartsQ[#, roots] && FileExistsQ[#] &]];
progress[input["Context"], If[loaded, " loaded, ", " did not load, "], IntegerString[Length[read]], " files read"];

identifier = (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...) ~~ (("`" ~~ (LetterCharacter | "$") ~~ ((WordCharacter | "$") ...)) ...);
ownName[name_String] := Last[StringSplit[name, "`"]];
contextOf[name_String] := StringReplace[name, RegularExpression["[^`]*$"] -> ""];

If[ ! loaded,
    Export[out, <|"Failure" -> "Needs[\"" <> input["Context"] <> "\"] did not load it"|>, "WXF"],
    after = snapshot[];
    changed = changedBetween[before, after];
    contexts = Sort[Complement[Contexts[], Keys[contextsBefore]]];
    (* the paclets' own contexts, not those of the Wolfram packages the loading autoloads; all the
       loading made where the paclets declare none *)
    contexts = If[ declared === {}, contexts, Select[contexts, StringStartsQ[#, declared] &]];
    inContexts = AssociationThread[contexts -> True];
    (* with no context on the path, Names gives every name in full; one call lists them all *)
    all = Block[{$ContextPath = {}, $Context = "DefinitionScan`Names`"}, Select[Names["*`*"], KeyExistsQ[inContexts, contextOf[#]] &]];
    (* A Module's local, x$123, is numbered by the run: one with no definition is left over and
       dropped, and those of one name with definitions, a closure's state, are one symbol, x$. *)
    temporaryQ[name_String] := StringMatchQ[ownName[name], RegularExpression["[A-Za-z][A-Za-z0-9]*\\$[0-9]+"]];
    definedQ[name_String] := ToExpression[name, InputForm, Function[s, OwnValues[s] =!= {} || DownValues[s] =!= {} || UpValues[s] =!= {} || SubValues[s] =!= {}, HoldAllComplete]];
    names = Select[all, ! temporaryQ[#] || definedQ[#] &];
    unreadable = fingerprint[StringRiffle[# <> " " <> FileHash[#, "SHA256", "HexString"] & /@ read, "\n"]];
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
    (* A Module's local that a definition's code names, a closure's state, lives as long as that
       definition; one that none names is left over from the loading, there in one run and gone in
       the next, and is no symbol of the cones. *)
    temporary = AssociationThread[kept -> (temporaryQ /@ kept)];
    codeNames = AssociationThread[kept -> keptParts[[All, 2]]];
    namedTemporaries = FixedPoint[
        Function[found, Union[found, Select[DeleteDuplicates[Catenate[Lookup[codeNames, found]]], TrueQ[Lookup[temporary, #, False]] &]]],
        Select[DeleteDuplicates[Catenate[Values[KeySelect[codeNames, ! temporary[#] &]]]], TrueQ[Lookup[temporary, #, False]] &]
    ];
    named = AssociationThread[namedTemporaries -> True];
    keep = (! temporary[#] || KeyExistsQ[named, #]) & /@ kept;
    kept = Pick[kept, keep];
    keptParts = Pick[keptParts, keep];
    keyOf = If[temporaryQ[#], StringReplace[#, RegularExpression["\\$[0-9]+$"] -> "$"], #] & /@ kept;
    keys = DeleteDuplicates[keyOf];
    n = Length[keys];
    index = AssociationThread[keys -> Range[n]];
    keyIndex = Lookup[index, keyOf];
    known = AssociationThread[kept -> keyIndex];
    system = AssociationThread[StringReplace[Names["System`*"], StartOfString ~~ "System`" -> ""] -> True];
    (* a name without its context is never a Module's local *)
    byOwnName = GroupBy[Select[kept, ! temporaryQ[#] &], ownName];
    progress[IntegerString[Length[names]], " definitions read: ", IntegerString[n], " symbols with definitions in ", IntegerString[Length[contexts]], " contexts; ", IntegerString[Length[changed]], " System` and Global` symbols changed"];
    (* Whose a symbol is: the paclet directory with the longest declared context its context is
       under. *)
    owners = Catenate[Function[dir, # -> dir & /@ pacletContexts[dir]] /@ input["Directories"]];
    ownerOf[name_String] := ownerOf[contextOf[name], "Context"];
    ownerOf[context_String, "Context"] := ownerOf[context, "Context"] = Replace[
        MaximalBy[Select[owners, StringStartsQ[context, First[#]] &], StringLength[First[#]] &, 1], {{_ -> dir_} :> dir, _ -> None}];
    pacletNames = Association[Function[dir, With[{paclet = pacletOf[dir]}, If[PacletObjectQ[paclet], paclet["Name"] -> dir, Nothing]]] /@ input["Directories"]];
    direct[dir_] := direct[dir] = With[{paclet = pacletOf[dir]},
        If[ ! PacletObjectQ[paclet], {},
            DeleteMissing[Lookup[pacletNames, Replace[Replace[paclet["Dependencies"], Except[_List] -> {}], {(name_String -> _) :> name, {name_String, ___} :> name}, {1}]]]]];
    reach[dir_] := reach[dir] = FixedPoint[Union[#, Catenate[direct /@ #]] &, {dir}];
    hiddenQ[name_String] := StringContainsQ[contextOf[name], "Private`" | "PackageScope`"];
    (* the paclet itself depends on every dependency it was given, declared in its PacletInfo or not *)
    main = ownerOf[input["Context"], "Context"];
    mayName[from_, name_String] := With[{to = ownerOf[name]},
        to === from || ((MemberQ[reach[from], to] || (from === main && MemberQ[Lookup[input, "DependencyDirectories", {}], to])) && ! hiddenQ[name])];
    (* The contexts a symbol's code gives a name: a context among its strings, the one it joins to
       a name for Symbol or NameQ, compares with Context[...] or puts on $ContextPath, and those of
       every symbol its code reaches, since a name passed on is given its context there. *)
    codeUses = Lookup[known, Select[#, Function[name, KeyExistsQ[known, name]]]] & /@ keptParts[[All, 2]];
    literals = Select[#, Function[text, KeyExistsQ[inContexts, text]]] & /@ keptParts[[All, 3]];
    codeAt = Lookup[GroupBy[Transpose[{keyIndex, codeUses}], First -> Last, DeleteDuplicates @* Catenate], Range[n], {}];
    literalAt = Lookup[GroupBy[Transpose[{keyIndex, literals}], First -> Last, DeleteDuplicates @* Catenate], Range[n], {}];
    codePairs = Catenate[MapThread[Thread[{#1, #2}] &, {Range[n], codeAt}]];
    codeComponents = ConnectedComponents[AdjacencyGraph[Range[n], SparseArray[If[codePairs === {}, {}, codePairs -> 1], {n, n}]]];
    codeComponent = ConstantArray[0, n];
    MapIndexed[(codeComponent[[#1]] = First[#2]) &, codeComponents];
    codeLinks = If[ codePairs === {}, {},
        With[{from = codeComponent[[codePairs[[All, 1]]]], to = codeComponent[[codePairs[[All, 2]]]]},
            DeleteDuplicates[Pick[Transpose[{from, to}], Unitize[from - to], 1]]
        ]
    ];
    codeReaches = Lookup[GroupBy[codeLinks, First -> Last], Range[Length[codeComponents]], {}];
    given = <||>;
    Scan[
        Function[c, given[c] = Union[Catenate[literalAt[[codeComponents[[c]]]]], Catenate[Lookup[given, codeReaches[[c]]]]]],
        Reverse[TopologicalSort[AdjacencyGraph[Range[Length[codeComponents]], SparseArray[If[codeLinks === {}, {}, codeLinks -> 1], {Length[codeComponents], Length[codeComponents]}]]]]
    ];
    (* A name in a string: a symbol of the cones in full; or by its own name, a symbol of the
       string's own paclet, or one in a context its code gives a name. A word that is also the name
       of another paclet's symbol, a property's key or a command's text, names nothing there.
       Never a System` symbol's name. *)
    stringReferences[from_, contextsGiven_List, token_String] := Which[
        KeyExistsQ[known, token], {known[token]},
        KeyExistsQ[system, ownName[token]], {},
        True, Lookup[known, Select[Lookup[byOwnName, ownName[token], {}], ownerOf[#] === from || MemberQ[contextsGiven, contextOf[#]] &]]
    ];
    uses = MapThread[
        Function[{name, parts, index}, With[{from = ownerOf[name], contextsGiven = given[codeComponent[[index]]]}, DeleteDuplicates[Join[
            Lookup[known, Select[parts[[2]], KeyExistsQ[known, #] &]],
            Catenate[stringReferences[from, contextsGiven, #] & /@ DeleteDuplicates[Catenate[StringCases[parts[[3]], identifier]]]]
        ]]]],
        {kept, keptParts, keyIndex}
    ];
    (* what the paclet's own pages may name without a context: its own symbols, and those of the
       paclets it depends on that are in no Private or PackageScope context *)
    seeds = DeleteDuplicates[Pick[keyOf, (! temporaryQ[#] && mayName[main, #]) & /@ kept]];
    (* by symbol: its fingerprint, or that of its locals' sorted fingerprints, and what they use *)
    grouped = GroupBy[Transpose[{keyIndex, keptParts[[All, 1]], uses}], First -> Rest];
    hashes = Table[With[{members = grouped[i]}, If[Length[members] === 1, members[[1, 1]], fingerprint[StringRiffle[Sort[members[[All, 1]]], "\n"]]]], {i, n}];
    (* What no definition shows of a paclet is the files its loading did not read. Such a file
       is part of the symbols whose code names it, as code that finds a file does: by a string
       that is a directory on its path below the paclet's, its name, its name without extension
       or its name among the paclet's assets. A dependency's file that no code names, and its
       PacletInfo without its version, are part of each of its symbols; the paclet's own that no
       code names is left to every page's key ("Attached" lists the others). *)
    wasRead = AssociationThread[read -> True];
    mainDirectory = ownerOf[input["Context"], "Context"];
    owned = GroupBy[Transpose[{ownerOf /@ kept, keyIndex, keptParts[[All, 3]]}], First -> Rest];
    assetNames[dir_String] := With[{paclet = pacletOf[dir]},
        If[ ! PacletObjectQ[paclet], <||>,
            Association @ Cases[Replace[paclet["Extensions"], Except[_List] -> {}], {"Asset", rules___} :> With[{options = Association[Cases[{rules}, _Rule]]},
                Cases[Flatten[{Lookup[options, "Assets", {}]}], ((name_String -> path_String) | {name_String, path_String}) :>
                    (ExpandFileName[FileNameJoin[{dir, Lookup[options, "Root", "."], path}]] -> name)]], Infinity]]];
    extra = <||>;
    attached = {};
    unnamed = <||>;
    Scan[
        Function[dir, Module[{files, byString, assets = assetNames[dir]},
            files = Select[
                Lookup[Lookup[input, "Files", <||>], dir, Sort @ Select[FileNames["*", Select[FileNameJoin[{dir, #}] & /@ {"Kernel", "Assets"}, DirectoryQ], Infinity], FileType[#] === File &]],
                ! KeyExistsQ[wasRead, ExpandFileName[#]] && ! StringMatchQ[StringTrim[StringDrop[#, StringLength[dir]], "/" | "\\"], Lookup[input, "Excluded", {}]] &];
            byString = GroupBy[Catenate[Function[{index, strings}, {#, index} & /@ strings] @@@ Lookup[owned, dir, {}]], First -> Last, DeleteDuplicates];
            unnamed[dir] = {};
            Scan[
                Function[file, With[
                    {tokens = DeleteDuplicates @ DeleteCases[Join[
                        Most[FileNameSplit[StringTrim[StringDrop[file, StringLength[dir]], "/" | "\\"]]],
                        {FileNameTake[file], FileBaseName[file]}, Lookup[assets, {ExpandFileName[file]}, Nothing]], "Kernel" | "Assets" | ""],
                     line = StringDrop[file, StringLength[dir]] <> " " <> FileHash[file, "SHA256", "HexString"]},
                    With[{namers = DeleteDuplicates[Catenate[Lookup[byString, tokens, {}]]]},
                        If[ namers === {},
                            AppendTo[unnamed[dir], line],
                            AppendTo[attached, file]; Scan[(extra[#] = Append[Lookup[extra, #, {}], line]) &, namers]
                        ]
                    ]
                ]],
                files
            ]
        ]],
        DeleteDuplicates[Prepend[Lookup[input, "DependencyDirectories", {}], mainDirectory]]
    ];
    dependencyParts = Map[
        Function[dependency,
            {
                pacletContexts[dependency],
                fingerprint[StringRiffle[Join[
                    StringDelete[ByteArrayToString[ReadByteArray[#], "UTF-8"], RegularExpression["\"Version\"\\s*->\\s*\"[^\"]*\"\\s*,?"]] & /@
                        Select[FileNameJoin[{dependency, #}] & /@ {"PacletInfo.wl", "PacletInfo.m"}, FileExistsQ],
                    {Lookup[dependencyForeign, dependency, ""]},
                    Lookup[unnamed, dependency, {}]
                ], "\n"]]
            }
        ],
        Lookup[input, "DependencyDirectories", {}]
    ];
    hashes = MapThread[
        Function[{key, hash, index}, With[{own = Select[dependencyParts, StringStartsQ[key, #[[1]]] &], files = Sort[Lookup[extra, index, {}]]},
            If[own === {} && files === {}, hash, fingerprint[hash <> " " <> StringRiffle[Join[own[[All, 2]], files], " "]]]]],
        {keys, hashes, Range[n]}
    ];
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
        "Edges" -> Table[DeleteDuplicates[Catenate[grouped[i][[All, 2]]]], {i, n}],
        "Components" -> m,
        "Temporaries" -> DeleteDuplicates[Pick[keyOf, temporaryQ /@ kept]],
        "Seeds" -> seeds,
        "Largest" -> Max[Length /@ components],
        "Read" -> read,
        "Unloaded" -> unloaded,
        "Outranked" -> outranked,
        "Attached" -> attached,
        "Foreign" -> foreignHash[after, changed],
        "Changed" -> changed,
        "ChangedHashes" -> AssociationMap[after[#][[1]] &, changed],
        "DependencyChanged" -> dependencyChanged,
        "Contexts" -> contexts
    |>, "WXF"]
];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Processes.wl"}]];
End[];
DefinitionScan`endScript[];
