(* ::Package:: *)

(* ==================== processes ====================
   Read into the package's private context, and into a script that builds or renders pages, which
   ends with endScript[]: nothing here uses the rest of the package. *)

(* the processes, by id: each one's parent's id, its command and its state, but for ps itself;
   none where there is no ps *)
processTable[] := If[ $OperatingSystem === "Windows", <||>,
    With[{run = RunProcess[{"ps", "-e", "-o", "pid=,ppid=,stat=,comm="}]},
        If[ ! AssociationQ[run] || run["ExitCode"] =!= 0, <||>,
            Association @ Map[
                Function[line, With[{fields = StringSplit[StringTrim[line], Whitespace, 4]},
                    If[ Length[fields] === 4 && StringMatchQ[fields[[1]] <> fields[[2]], DigitCharacter ..] && FileNameTake[fields[[4]]] =!= "ps",
                        FromDigits[fields[[1]]] -> {FromDigits[fields[[2]]], fields[[4]], fields[[3]]},
                        Nothing
                    ]]],
                StringSplit[run["StandardOutput"], "\n"]
            ]
        ]
    ]
];

(* the processes descended from the roots, a generation at a time: their children, the children's
   children, and so on *)
processLayers[table_Association, roots_List] := Module[{layers = {}, all = roots, frontier = roots},
    While[ (frontier = Complement[Select[Keys[table], MemberQ[frontier, First[table[#]]] &], all]) =!= {},
        AppendTo[layers, frontier];
        all = Join[all, frontier]
    ];
    layers
];

descendantProcesses[table_Association, roots_List] := Catenate[processLayers[table, roots]];

(* the processes among pids still running; one that has exited, but that its parent has not yet
   reaped, is not *)
livingProcesses[pids_List] := livingProcesses[processTable[], pids];
livingProcesses[table_Association, pids_List] := Select[Intersection[pids, Keys[table]], ! zombieQ[table[#]] &];

zombieQ[{_, _, state_String}] := StringStartsQ[state, "Z"];
zombieQ[_] := False;

(* whether the processes have all exited within the seconds given; once they have, their parents
   get a moment to reap them, since a process exited and not reaped when its parent exits is left
   to whatever adopts it, which may never reap it *)
waitForExit[pids_List, seconds_] := Module[{deadline = AbsoluteTime[] + seconds, table = processTable[]},
    While[ livingProcesses[table, pids] =!= {} && AbsoluteTime[] < deadline, Pause[0.1]; table = processTable[]];
    If[ livingProcesses[table, pids] === {},
        deadline = AbsoluteTime[] + 2;
        While[ Intersection[pids, Keys[table]] =!= {} && AbsoluteTime[] < deadline, Pause[0.1]; table = processTable[]]
    ];
    livingProcesses[table, pids] === {}
];

frontEndProcessQ[command_String] := StringMatchQ[FileNameTake[command], "WolframNB" | "MathematicaServer" | "Mathematica" | "WolframFrontEnd"];

(* the front ends among the processes descended from the roots *)
frontEndProcesses[table_Association, roots_List] := Select[descendantProcesses[table, roots], frontEndProcessQ[table[#][[2]]] &];

(* processes ended with KILL, each with the processes it started: the youngest generation first,
   and each one once its parents have reaped the one before, so that none is left to a parent that
   does not reap *)
endProcesses[_Association, {}] := Null;
endProcesses[table_Association, pids_List] := Scan[
    Function[generation, RunProcess[Join[{"kill", "-KILL"}, IntegerString /@ generation]]; waitForExit[generation, 5]],
    Reverse[Prepend[processLayers[table, pids], pids]]
];

(* The end of a script that builds or renders pages: everything it started ended with KILL, its
   front ends first, since a front end that quits, or loses its kernel, can crash on its way out;
   then its kernels, which this kernel reaps once killed, where a kernel left to exit by itself
   after CloseKernels it may not; then the kernels closed *)
endScript[] := (
    With[{table = processTable[]}, endProcesses[table, frontEndProcesses[table, {$ProcessID}]]];
    With[{table = processTable[]}, endProcesses[table, First[processLayers[table, {$ProcessID}], {}]]];
    CloseKernels[];
);
