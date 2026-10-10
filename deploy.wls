#!/usr/bin/env wolframscript
(* Deploys the PacletPage paclet resource with DeployPacletResource: a fresh PacletBuild checked to
   hold every documentation page, then the resource, publicly, in the connected account.

       wl -f deploy.wls              (or --dry-run: everything but the deploy)

   The account is PACLETPAGE_ACCOUNT, by default the Wolfram Institute's; a session connected as
   anyone else is refused. *)

$ProgressReporting = False

dir = DirectoryName[$InputFileName]
PacletDirectoryLoad[dir]
Needs["WolframInstitute`PacletPage`"]

account = Replace[Environment["PACLETPAGE_ACCOUNT"], Except[_String?(StringLength[#] > 0 &)] -> "wolframinstitute@wolframinstitute.org"]
result = DeployPacletResource[dir, "Account" -> account, "DryRun" -> MemberQ[Rest[$ScriptCommandLine], "--dry-run"]]
If[ ! AssociationQ[result], Exit[1]]
Print[If[result["Deployed"], "deployed: ", "dry run, would deploy to: "], result["Resource"], " (", result["Pages"], " documentation pages)"]
