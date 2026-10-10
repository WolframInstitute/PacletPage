#!/usr/bin/env wolframscript
(* Builds PacletPage's documentation: BuildPacletDocumentation converts docs/ into
   Documentation/English and ResourceDefinition.md into ResourceDefinition.nb. Each section of a
   page, and each example between ---, runs in a clean context, as the converter does by default.
   The tutorial publishes PacletPage's own site, so this publishes too; see release.wls for the
   whole release. *)

$ProgressReporting = False

dir = DirectoryName[$InputFileName]
PacletDirectoryLoad[dir]
Needs["WolframInstitute`PacletPage`"]

(* the tutorial publishes PacletPage's site as it is built, so the account is checked first *)
account = Replace[Environment["PACLETPAGE_ACCOUNT"], Except[_String?(StringLength[#] > 0 &)] -> "wolframinstitute@wolframinstitute.org"]
If[ ! WolframInstitute`PacletPage`Private`publishingAccountQ[PublishPacletPage, account], Exit[1]]

result = BuildPacletDocumentation[dir]
If[ ! AssociationQ[result], Exit[1]]
Print[Length[result["Notebooks"]], " pages", If[StringQ[result["Definition"]], " and the definition notebook", ""],
    ": ", Length[result["Converted"]], " converted, ", Length[result["Cached"]], " from the cache, ",
    Length[result["Evaluated"]], " with their examples evaluated afresh"]
If[ result["Failed"] =!= {}, Print["failed: ", result["Failed"]]; Exit[1]]
