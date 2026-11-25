#!/usr/bin/env wolframscript
(* Runs the test files in Tests/ against this paclet: loads the paclet, runs TestReport,
   prints the failures and a summary, and exits 0 when every test passes, 1 otherwise. *)

$ProgressReporting = False

dir = DirectoryName[$InputFileName]
PacletDirectoryLoad[dir]
Needs["WolframInstitute`PacletPage`"]

report = MemoryConstrained[TestReport @ FileNames["*.wlt", FileNameJoin[{dir, "Tests"}]], 4 * 10^9, $Aborted]
If[ ! MatchQ[report, _TestReportObject], Print["the tests did not run: ", report]; Exit[1]]

passed = Length @ report["TestsSucceededIndices"]
failed = Length @ report["TestsFailedIndices"]
total = passed + failed

If[ failed > 0,
    Print[""];
    Print["FAILED:"];
    Scan[
        Function[idx,
            With[{r = report["TestResults"][idx]},
                Print["  ", r["TestID"]];
                Print["    expected: ", ToString[r["ExpectedOutput"], InputForm]];
                Print["    actual:   ", ToString[r["ActualOutput"], InputForm]];
                If[ r["ExpectedMessages"] =!= r["ActualMessages"],
                    Print["    messages: ", ToString[r["ActualMessages"], InputForm]]
                ]
            ]
        ],
        report["TestsFailedIndices"]
    ]
]

Print[""]
Print["==== summary ====  ", passed, " / ", total, " pass"]
If[failed > 0, Exit[1], Exit[0]]
