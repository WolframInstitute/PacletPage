(* ::Package:: *)

(* ==================== unpublishing a paclet's site and resource ====================

   UnpublishPacletPage[dir, name] gives everything PublishPacletPage deployed for the paclet in dir
   under the site name in the connected account, and deletes it when asked. The roots are named as
   the publish names them (Cloud.wl): the site's siblings <name>, <name>Page, <name>Feedback,
   <name>Source and <name>SourceManifest, the paclet resource's directory under DeployedResources,
   and in each archive directory the paclet's own archives, <Publisher>__<Name>.paclet and those
   whose name goes on with a "-". Only those objects and what lies beneath them are listed, so
   another site or paclet whose name starts with the same characters is never touched. The
   feedback form's Databin, named in <name>Feedback/config, is reported, and deleted only with
   "DeleteDatabin" -> True. *)

Options[UnpublishPacletPage] = {"Account" -> Automatic, "Delete" -> False, "DeleteDatabin" -> False, "ArchiveDirectories" -> {"Paclets"}};

UnpublishPacletPage::cloud = "Not connected to the Wolfram Cloud.";
UnpublishPacletPage::account = "Connected to the Wolfram Cloud as `1`, not as `2`; connect as `2` to unpublish this site.";
UnpublishPacletPage::name = "`1` is not a site name.";
UnpublishPacletPage::paclet = "`1` is neither a paclet directory nor a paclet name.";

(* the paclet's name: from its directory, or given *)
unpublishPacletName[dir_String] /; DirectoryQ[dir] := Replace[PacletObject[File[dir]], {paclet_PacletObject :> paclet["Name"], _ -> None}];
unpublishPacletName[name_String] /; StringMatchQ[name, RegularExpression["[A-Za-z0-9_-]+(/[A-Za-z0-9_-]+)?"]] := name;
unpublishPacletName[_] := None;

(* the roots the publish deploys under: the site's siblings and the resource's directory *)
unpublishRoots[name_String, paclet_String] := Join[
    siteURL[name] <> # & /@ {"", "Page", "Feedback", "Source", "SourceManifest"},
    (* the resource system names the directory with the name's / encoded, which the cloud reads as a / *)
    {
        Needs["ResourceSystemClient`"];
        ResourceSystemClient`Private`loadDeployResourceShingle[];
        Replace[DeployedResourceShingle`Private`defaultCloudObjectLocation["Paclet", paclet], {object_CloudObject :> URLDecode[First[object]], _ -> Nothing}]
    }
];

(* an object and every object beneath it, deepest first; none where nothing is there *)
cloudTree[url_String] := With[{object = CloudObject[url]},
    Which[
        TrueQ[DirectoryQ[object]],
            Append[Join @@ (cloudTree[First[#]] & /@ Select[CloudObjects[object], strictlyBeneathQ[url, First[#]] &]), url],
        TrueQ[FileExistsQ[object]], {url},
        True, {}
    ]
];

(* the paclet's own archives in the archive directory dir of the account *)
unpublishArchives[paclet_String, dir_String] := With[{root = accountBase[] <> "/" <> StringTrim[dir, "/"], base = StringReplace[paclet, "/" -> "__"]},
    If[ TrueQ[DirectoryQ[CloudObject[root]]],
        Join @@ (cloudTree /@ Select[First /@ CloudObjects[CloudObject[root]],
            strictlyBeneathQ[root, #] && StringMatchQ[StringDelete[#, StartOfString ~~ root ~~ "/"], (base ~~ ".paclet") | (base ~~ "-" ~~ ___)] &]),
        {}
    ]
];

(* the Databin the site's feedback form writes to, named in its private config *)
feedbackDatabinOf[name_String] := With[{config = CloudObject[siteURL[name] <> "Feedback/config"]},
    If[TrueQ[FileExistsQ[config]], Replace[CloudGet[config], {KeyValuePattern["Databin" -> bin_String] :> bin, _ -> None}], None]
];

UnpublishPacletPage[dirOrPaclet_String, name_String, opts : OptionsPattern[]] := Catch[Module[{paclet, roots, objects, bin, results, binResult},
    If[! siteNameQ[name], Message[UnpublishPacletPage::name, name]; Throw[$Failed, unpublish]];
    paclet = unpublishPacletName[dirOrPaclet];
    If[! StringQ[paclet], Message[UnpublishPacletPage::paclet, dirOrPaclet]; Throw[$Failed, unpublish]];
    If[! publishingAccountQ[UnpublishPacletPage, OptionValue["Account"]], Throw[$Failed, unpublish]];
    roots = unpublishRoots[name, paclet];
    objects = DeleteDuplicates[Join[Join @@ (cloudTree /@ roots), Join @@ (unpublishArchives[paclet, #] & /@ OptionValue["ArchiveDirectories"])]];
    bin = feedbackDatabinOf[name];
    If[ ! TrueQ[OptionValue["Delete"]],
        Throw[<|"Paclet" -> paclet, "Site" -> name, "Roots" -> roots, "Objects" -> objects, "Databin" -> bin, "Deleted" -> False|>, unpublish]
    ];
    results = AssociationMap[! FailureQ[DeleteObject[CloudObject[#]]] &, objects];
    binResult = If[ StringQ[bin] && TrueQ[OptionValue["DeleteDatabin"]], ! FailureQ[DeleteObject[Databin[bin]]], None];
    <|
        "Paclet" -> paclet, "Site" -> name, "Roots" -> roots, "Objects" -> objects, "Deleted" -> True,
        "Failed" -> Keys[Select[results, Not]], "Databin" -> bin, "DatabinDeleted" -> binResult
    |>
], unpublish];
