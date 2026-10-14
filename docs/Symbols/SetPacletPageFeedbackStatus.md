---
Template: Symbol
Name: SetPacletPageFeedbackStatus
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/SetPacletPageFeedbackStatus
Keywords: [feedback, status, triage]
SeeAlso: [PacletPageFeedback, DeployPacletPageFeedback]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`SetPacletPageFeedbackStatus[name, id, status, note]` records that the feedback entry `id` of the site `name` is `"open"`, `"decided"`, `"fixed"`, `"follow-up"` or `"not a defect"`, with `note`.

`SetPacletPageFeedbackStatus[name, id, status]` records the status with no note.

`SetPacletPageFeedbackStatus[name, {update1, update2, ...}]` records several statuses at once.

## Details & Options

- `SetPacletPageFeedbackStatus` goes through the site's `SetStatus` endpoint, so any member of the team can use it from a signed-in Wolfram kernel.
- `name` is a site of the connected account, or the URL of a site in another account.
- Each Status button on the site's `All` page copies the same call, filled in with the entry's current status and note.
- The result is the status recorded, as an association.
- An update is `id -> status`, `{id, status}`, `{id, status, note}` or an entry with `"ID"`, `"Status"` and `"Note"`, such as a row of `PacletPageFeedback`, or the same keys in lowercase. All the updates are checked first, and sent in one request that stores them in one write; when one is not a status, none is stored. The result is the list of statuses recorded.

## Basic Examples

An example entry, sent the way the site's Feedback panel sends it:

```wl
URLRead[HTTPRequest["https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/Form", <|"Method" -> "POST", "Body" -> {
    "Feedback" -> "An example entry sent by the SetPacletPageFeedbackStatus page.",
    "Page" -> "ref/SetPacletPageFeedbackStatus", "Title" -> "SetPacletPageFeedbackStatus", "View" -> "built page"
}|>]]
```

The example entries the documentation has sent that are still open:

```wl
open = PacletPageFeedback["PacletPage"][Select[StringStartsQ[#Feedback, "An example entry"] && #Status === "open" &], {"ID", "Page", "Status"}]
```

Record that each is not a defect, so the team's list of open entries holds only readers' feedback:

```wl
SetPacletPageFeedbackStatus["PacletPage", #, "not a defect", "an example sent by the documentation"] & /@ Normal[open[All, "ID"]]
```

Where they stand now:

```wl
PacletPageFeedback["PacletPage"][Select[MemberQ[Normal[open[All, "ID"]], #ID] &], {"ID", "Status", "Note"}]
```

## Possible Issues

A status that is not one of the five gives a message:

```wl
SetPacletPageFeedbackStatus["PacletPage", "20261010-120000-abcdef12", "closed"]
```
