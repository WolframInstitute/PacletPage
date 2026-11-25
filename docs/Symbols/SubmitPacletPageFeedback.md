---
Template: Symbol
Name: SubmitPacletPageFeedback
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/SubmitPacletPageFeedback
Keywords: [feedback, Databin, submit, documentation, bug report]
SeeAlso: [PacletPageFeedback, DeployPacletPageFeedback, SetPacletPageFeedbackStatus, PacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

<code>[SubmitPacletPageFeedback]()[*spec*, *feedback*]</code> submits documentation feedback for the paclet *spec* to the paclet's published feedback form, recording it in the same `Databin` as the published documentation site.

<code>[SubmitPacletPageFeedback]()[*spec*, *page*, *feedback*]</code> records the feedback against the documentation page *page* of *spec*.

## Details & Options

- *spec* is a paclet name such as `"WolframInstitute/PacletPage"`, a `PacletObject`, a paclet directory or `File[dir]`, a local catalog from `PacletPageCatalog`, a `paclet:` URI such as `"paclet:WolframInstitute/PacletPage/ref/PacletPage"`, or a deployed site or feedback form URL.
- When *spec* is a `paclet:` URI that includes a page, that page is used automatically unless *page* is given explicitly.
- The feedback form URL is deduced from the paclet's `"FeedbackURL"` or `"SiteURL"` metadata when present, or otherwise from its `"PublisherID"` and short name as `https://www.wolframcloud.com/obj/<lowercase-publisher>/<ShortName>Feedback/Form`.
- *feedback* is either a non-empty text string or an `Association` containing `"Feedback"` along with any of `"Where"`, `"Expected"`, `"Email"`, `"Page"`, `"Title"`, `"Link"`, `"View"`, `"Notebook"`, and `"Attachments"`. Keys inside *feedback* take precedence over options of the same name.
- On success, `SubmitPacletPageFeedback` returns an association `<|"Submitted" -> True, "Paclet" -> ..., "Page" -> ..., "FeedbackURL" -> ..., "AllFeedbackURL" -> ...|>`.
- Empty feedback returns `Failure["EmptyFeedback", ...]`, an invalid email address returns `Failure["InvalidEmail", ...]`, a paclet without a deducible feedback URL returns `Failure["NoFeedbackURL", ...]`, and an unsuccessful HTTP submission returns `Failure["FeedbackSubmitFailed", ...]`.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"FeedbackURL"` | `Automatic` | explicit feedback form URL, overriding the URL deduced from *spec* |
| `"Where"` | `""` | section, example, or cell where the problem appears |
| `"Expected"` | `""` | what the page should show or do instead |
| `"Email"` | `Automatic` | optional reply email address; `Automatic` uses `$WolframID` when signed in, `None` or `""` submits anonymously |
| `"Title"` | `Automatic` | page title recorded with the entry |
| `"Link"` | `Automatic` | page link recorded with the entry; by default the published site anchor or `paclet:` URI |
| `"View"` | `"built page"` | view the reader was looking at, such as `"built page"` or `"source"` |
| `"Notebook"` | `""` | optional notebook path or URL recorded with the entry |
| `"Attachments"` | `{}` | up to 3 attachments (up to 20 MB each), each a `File[path]`, file path string, or `<|"Name" -> name, "Base64" -> ...|>` / `<|"Name" -> name, "Content" -> ...|>` association |

## Basic Examples

Submit feedback for a paclet page by its `paclet:` URI:

```wl
#| eval: false
SubmitPacletPageFeedback[
    "paclet:WolframInstitute/PacletPage/ref/PacletPage",
    "The LayoutControls option could show an example with LeftSidebar only.",
    "Where" -> "Basic Examples"
]
```

Submit structured feedback with an attachment:

```wl
#| eval: false
SubmitPacletPageFeedback[
    "WolframInstitute/PacletPage",
    "ref/PacletPage",
    <|
        "Feedback" -> "Sidebar splitter jumps slightly on first drag.",
        "Where" -> "Left rail",
        "Expected" -> "Smooth resize from the current width.",
        "Attachments" -> {File["screenshot.png"]}
    |>
]
```
