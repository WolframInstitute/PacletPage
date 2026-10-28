#!/usr/bin/env wolframscript
(* Publishes a documentation site for a published paclet, then checks it.

     wl -f publish.wls RESOURCE NAME [options]

   RESOURCE   "Publisher/Name" on the Paclet Repository, or the URL of a deployed paclet resource
   NAME       the site's name in the connected cloud account, such as "PAdic" or "Sites/PAdic"

   options
     --documentation DIR   the built documentation, for keywords and titles
     --source DIR          the authoring notebooks, for the toggle between each page and its source
     --domains A,B         email domains whose signed-in accounts read the feedback
     --readers X,Y         Wolfram IDs that read the feedback
     --email ADDRESS       the address the feedback falls back to
     --account ID          refuse to publish unless this Wolfram ID is connected
     --no-feedback         publish no feedback form
     --version-label       show the paclet's published version under the site's name
     --footer              a footer naming the PacletPage the site was made with
     --private             publish everything private, for a trial run
     --pages N|all         how many pages the check reads (10)

   Prints what it published and the check's failures; exits 1 when publishing fails or the check
   finds a problem. *)

$ProgressReporting = False

PacletDirectoryLoad[DirectoryName[$InputFileName]]
Needs["WolframInstitute`PacletPage`"]

arguments = Rest[$ScriptCommandLine]
flags = {"--no-feedback", "--private", "--version-label", "--footer"}
valued = {"--documentation", "--source", "--domains", "--readers", "--email", "--account", "--pages"}

(* the positional arguments and the options, by name *)
parse[{}, positional_, named_] := {positional, named}
parse[{flag_String, rest___}, positional_, named_] /; MemberQ[flags, flag] := parse[{rest}, positional, Append[named, flag -> True]]
parse[{option_String, value_String, rest___}, positional_, named_] /; MemberQ[valued, option] :=
    parse[{rest}, positional, Append[named, option -> value]]
parse[{option_String, ___}, _, _] /; StringStartsQ[option, "--"] := (Print["unknown or incomplete option ", option]; Exit[2])
parse[{argument_String, rest___}, positional_, named_] := parse[{rest}, Append[positional, argument], named]

{positional, named} = parse[arguments, {}, <||>]
If[ Length[positional] =!= 2,
    Print["usage: wl -f publish.wls RESOURCE NAME [--documentation DIR] [--source DIR] [--domains A,B] [--readers X,Y] [--email ADDRESS] [--account ID] [--no-feedback] [--version-label] [--footer] [--private] [--pages N|all]"];
    Exit[2]
]
{resource, name} = positional

list[key_] := StringTrim /@ StringSplit[Lookup[named, key, ""], ","]
private = TrueQ[Lookup[named, "--private", False]]
pages = Replace[Lookup[named, "--pages", "10"], {
    "all" -> All,
    n_String :> Replace[Interpreter["Integer"][n], Except[_Integer?Positive] :> (Print["--pages takes a positive number or all"]; Exit[2])]
}]

Print["catalog of ", resource]
catalog = PacletPageCatalog[resource, "Documentation" -> Lookup[named, "--documentation", None], "Monitor" -> True]
If[ ! AssociationQ[catalog], Exit[1]]
Print[Length[catalog["Pages"]], " pages, root guide ", catalog["RootGuide"]]

result = DeployPacletPage[catalog, name,
    "Feedback" -> ! TrueQ[Lookup[named, "--no-feedback", False]],
    "FeedbackOptions" -> Join[
        {"Domains" -> list["--domains"], "Readers" -> list["--readers"], "Email" -> Lookup[named, "--email", None]},
        If[private, {Permissions -> "Private"}, {}]
    ],
    "Source" -> Lookup[named, "--source", None],
    "Account" -> Lookup[named, "--account", Automatic],
    "VersionLabel" -> TrueQ[Lookup[named, "--version-label", False]],
    "Footer" -> TrueQ[Lookup[named, "--footer", False]],
    Permissions -> If[private, "Private", "Public"]
]
If[ ! AssociationQ[result], Exit[1]]
Print["published: ", result["Site"]]
Print["frame:     ", result["Page"]]
If[ AssociationQ[result["Feedback"]],
    Print["feedback:  ", result["Feedback"]["Form"]];
    Print["team page: ", result["Feedback"]["All"]]
]
If[ AssociationQ[result["Source"]],
    Print["source:    ", result["Source"]["URL"], " (", result["Source"]["Uploaded"], " uploaded, ", result["Source"]["Removed"], " removed, ",
        Length[result["Source"]["Failed"]], " failed)"]
]

If[ private,
    Print["private site: the anonymous check is skipped"];
    Exit[0]
]

check = CheckPacletPage[catalog, name, "Pages" -> pages, "Feedback" -> AssociationQ[result["Feedback"]]]
Scan[
    Print["check failed: ", #["Check"], ": ", #["Detail"], " (", #["URL"], ")"] &,
    Select[Normal[check["Checks"]], ! TrueQ[#["OK"]] &]
]
Print["check: ", If[TrueQ[check["OK"]], "everything is reachable", "problems found"]]
Exit[If[TrueQ[check["OK"]], 0, 1]]
