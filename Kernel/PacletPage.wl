(* ::Package:: *)

(* PacletPage publishes a documentation site around a paclet whose resource pages are already
   published, either on the Paclet Repository or as a resource deployed to a cloud account, and
   maintains it: the site's pages, its feedback form and the team pages that read what the form
   collects, the permissions each of them needs, and a check that everything is reachable.

   Catalog.wl    the published pages: titles, URLs, the notebook behind each page, the guide tree
   Site.wl       the site's HTML, CSS and JavaScript, from the templates in Assets/Web
   Cloud.wl      publishing: the account, the pages, the source notebooks, cleanup, permissions
   Feedback.wl   the feedback form, its Databin and the team's pages
   Check.wl      reachability and version checks
   Publish.wl    the pipeline: markdown -> documentation notebooks -> paclet resource -> site
   Local.wl      the site rendered on this computer, without the cloud *)

BeginPackage["WolframInstitute`PacletPage`"]

PacletPageCatalog::usage = "PacletPageCatalog[resource] gives the catalog of the documentation pages of a published paclet: \
each page's title, its published page, the notebook behind it, and the guide tree. resource is \"Publisher/Name\" or a \
Paclet Repository URL for a paclet on the repository, or the URL or CloudObject of a paclet resource deployed to a cloud account.";

BuildPacletPageSite::usage = "BuildPacletPageSite[dir] renders the documentation notebooks of the paclet in dir to HTML \
on this computer and writes the site around them, under dir/build/site, without the cloud.";

PacletPageFiles::usage = "PacletPageFiles[catalog] gives the site's pages for catalog, an association of the file \
names \"index.html\" and \"page.html\" with their HTML.";

DeployPacletPage::usage = "DeployPacletPage[catalog, name] publishes the site for catalog under name in the connected \
cloud account, with its feedback form, and gives the URLs of what it published.";

DeployPacletPageFeedback::usage = "DeployPacletPageFeedback[name] publishes the feedback form of the site name and the \
team pages that list what it collects.";

PacletPageFeedback::usage = "PacletPageFeedback[name] gives the feedback the site name has collected, newest first, \
with the status of each entry.";

SetPacletPageFeedbackStatus::usage = "SetPacletPageFeedbackStatus[name, id, status, note] records that the feedback \
entry id of the site name is \"open\", \"decided\", \"fixed\", \"follow-up\" or \"not a defect\", with note.
SetPacletPageFeedbackStatus[name, {update1, update2, ...}] records several statuses at once, each id -> status, \
{id, status}, {id, status, note} or an entry with \"ID\", \"Status\" and \"Note\".";

SetPacletPagePermissions::usage = "SetPacletPagePermissions[dir, permissions] sets permissions on the cloud directory \
dir and on every directory beneath it.";

CheckPacletPage::usage = "CheckPacletPage[catalog, name] checks that the site name, the pages it frames and its \
feedback form are reachable, and gives a report.";

BuildPacletDocumentation::usage = "BuildPacletDocumentation[dir] converts the markdown documentation of the paclet in \
dir into documentation notebooks under Documentation/English, and its ResourceDefinition.md into the definition \
notebook its resource is deployed from.";

DeployPacletResource::usage = "DeployPacletResource[dir] builds the paclet in dir afresh, checks the archive holds \
every documentation page, and deploys the paclet resource publicly to the connected cloud account.";

SubmitPacletResource::usage = "SubmitPacletResource[dir] builds the paclet in dir afresh, checks its definition \
notebook and the archive, and submits the paclet to the Wolfram Paclet Repository, without a front end.";

PublishPacletPage::usage = "PublishPacletPage[dir, name] builds the documentation of the paclet in dir, deploys its \
resource, publishes the site name around it and checks the site; with \"Submit\" -> True it also submits the paclet \
to the Paclet Repository.";

PacletPageCatalog::resource = "`1` names no published paclet resource: give \"Publisher/Name\", a Paclet Repository URL, \
or the URL of a resource under DeployedResources/Paclet.";
PacletPageCatalog::page = "The published page `1` could not be read (HTTP `2`).";
PacletPageCatalog::nopages = "The published page `1` links to no documentation pages.";
PacletPageCatalog::nonb = "No notebook was found behind `1`; the page is left out.";
PacletPageFiles::catalog = "`1` is not a catalog that PacletPageCatalog gives.";
DeployPacletPage::cloud = "Not connected to the Wolfram Cloud.";
DeployPacletPage::account = "Connected to the Wolfram Cloud as `1`, not as `2`; connect as `2` to publish this site.";
DeployPacletPage::export = "Publishing `1` failed: `2`.";
DeployPacletPage::source = "`1` is not a directory of documentation notebooks.";
DeployPacletPageFeedback::databin = "The feedback Databin could not be created or found: `1`.";
DeployPacletPageFeedback::deploy = "Publishing the feedback page `1` failed: `2`.";
SetPacletPageFeedbackStatus::status = "`1` is not a feedback status; use one of `2`.";
SetPacletPageFeedbackStatus::update = "`1` is not a status update: give id -> status, {id, status}, {id, status, note}, or an entry with \"ID\", \"Status\" and \"Note\".";
SetPacletPagePermissions::dir = "`1` is not a cloud directory.";

Begin["`Private`"]

$packageDirectory = DirectoryName[$InputFileName];

$webDirectory = FileNameJoin[{ParentDirectory[$packageDirectory], "Assets", "Web"}];

Scan[Get[FileNameJoin[{$packageDirectory, #}]] &, {"Catalog.wl", "Site.wl", "Cloud.wl", "Feedback.wl", "Check.wl", "Publish.wl", "Local.wl"}];

End[]

EndPackage[]
