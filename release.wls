#!/usr/bin/env wolframscript
(* Releases PacletPage with PublishPacletPage, in one kernel and so one cloud account: builds the
   documentation (whose tutorial publishes the site), deploys the paclet resource, which other
   paclets' builds install PacletPage from, publishes the site around it with the authoring
   notebooks as its sources, and checks it.

       wl -f release.wls             (or --dry-run: the build and the resource, not deployed)

   The account is PACLETPAGE_ACCOUNT, by default the Wolfram Institute's. *)

$ProgressReporting = False

dir = DirectoryName[$InputFileName]
PacletDirectoryLoad[dir]
Needs["WolframInstitute`PacletPage`"]

account = Replace[Environment["PACLETPAGE_ACCOUNT"], Except[_String?(StringLength[#] > 0 &)] -> "wolframinstitute@wolframinstitute.org"]
result = PublishPacletPage[dir, "PacletPage",
    "Account" -> account,
    "DryRun" -> MemberQ[Rest[$ScriptCommandLine], "--dry-run"],
    "Monitor" -> True,
    "SiteOptions" -> {"FeedbackOptions" -> {"Domains" -> {"wolframinstitute.org", "wolfram.com"}}}
]
If[ ! AssociationQ[result], Exit[1]]
Print["documentation: ", Length[result["Build"]["Notebooks"]], " pages, failed ", result["Build"]["Failed"]]
Print["resource: ", result["Resource"]["Resource"], If[result["Resource"]["Deployed"], "", " (dry run)"]]
If[ AssociationQ[result["Site"]], Print["site: ", result["Site"]["Site"]]]
If[ AssociationQ[result["Check"]],
    Scan[Print["check failed: ", #["Check"], ": ", #["Detail"]] &, Select[Normal[result["Check"]["Checks"]], ! TrueQ[#["OK"]] &]];
    Print["check: ", If[TrueQ[result["Check"]["OK"]], "everything is reachable", "problems found"]];
    Exit[If[TrueQ[result["Check"]["OK"]], 0, 1]]
]
