---
Template: Symbol
Name: PacletPageFeedback
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageFeedback
Keywords: [feedback, Databin, triage]
SeeAlso: [SetPacletPageFeedbackStatus, DeployPacletPageFeedback]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`PacletPageFeedback[name]` gives the feedback the site `name` has collected, newest first, with the status of each entry.

## Details & Options

- `PacletPageFeedback` reads the site's Databin, which only the publishing account can read; the rest of the team reads the same entries on the site's `All` page.
- Each entry has the fields the form collects, `"Feedback"`, `"Where"`, `"Expected"`, `"Email"` and `"Attachments"`, the page's `"Page"`, `"Title"`, `"Link"`, `"View"` and `"Notebook"`, its `"ID"` and `"Submitted"` time, and its `"Status"` and `"Note"`.
- An entry no status names is `"open"`.

## Basic Examples

A reader's feedback, sent the way the site's Feedback panel sends it:

```wl
URLRead[HTTPRequest["https://www.wolframcloud.com/obj/wolframinstitute/PacletPageFeedback/Form", <|"Method" -> "POST", "Body" -> {
    "Feedback" -> "An example entry sent by the PacletPageFeedback page.",
    "Page" -> "ref/PacletPageFeedback", "Title" -> "PacletPageFeedback", "View" -> "built page"
}|>]]
```

The feedback the site has collected. It holds what readers wrote and their addresses, so this page shows only the example entries:

```wl
PacletPageFeedback["PacletPage"][Select[StringStartsQ[#Feedback, "An example entry"] &]]
```

How many of them stand where:

```wl
PacletPageFeedback["PacletPage"][Select[StringStartsQ[#Feedback, "An example entry"] &]][Counts, "Status"]
```
