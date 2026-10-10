---
Template: Symbol
Name: DeployPacletPageFeedback
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/DeployPacletPageFeedback
Keywords: [feedback, form, Databin, team, permissions]
SeeAlso: [DeployPacletPage, PacletPageFeedback, SetPacletPageFeedbackStatus]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`DeployPacletPageFeedback[name]` publishes the feedback form of the site `name` and the team pages that list what it collects.

## Details & Options

- `DeployPacletPage` calls `DeployPacletPageFeedback`; call it alone to change the form or the team.
- Everything lives in the directory `name <> "Feedback"`:

| Object | Permissions | Purpose |
|---|---|---|
| `Form` | public | the form: what is wrong, where, what was expected, an email, up to three files, and the page's identity |
| `All` | the team | every entry, newest first, with its status, attachments and the code that finds it |
| `File` | the team | one attachment at a time |
| `SetStatus` | the team | changes an entry's status, from a Wolfram kernel |
| `Me` | public | the signed-in reader's Wolfram ID, for the site's own pages only |
| `config`, `status`, `attachments` | private | the Databin's id, the statuses and the uploaded files |

- Each submission becomes an entry of a Databin, created on the first deploy and kept in `config`, so a redeploy keeps writing to the same Databin. Its id stays out of the result.
- The team is the publishing account, the IDs in `"Readers"` and every signed-in ID in a domain of `"Domains"`. Cloud permissions let only signed-in Wolfram IDs reach the team's pages; their own code then admits the team, since cloud permissions cannot name a domain.
- The form takes no cloud security token, since the site's panel posts to it directly. A hidden field that people leave empty turns a robot's submission into a thank-you that stores nothing; every field is clipped to a fixed length and a file over 20 MB is dropped.
- A browser posts the panel's text as UTF-8 bytes that the cloud hands over one byte per character; the form reads them back as text. Text is stored in the Databin as ASCII, every character past ASCII written as `\uXXXX`, and read back as it was written.
- `SetStatus` refuses a request a browser sends, so no web page can change a status on a reader's behalf.
- The following options can be given:

| Option | Default | Description |
|---|---|---|
| `"Title"` | `Automatic` | the name the pages give the documentation |
| `"Domains"` | `{}` | email domains whose signed-in accounts are on the team |
| `"Readers"` | `{}` | Wolfram IDs on the team |
| `"Email"` | `None` | the address a failed submission is told to write to |
| `"Account"` | `Automatic` | the Wolfram ID that must be connected |
| `Permissions` | `Automatic` | `Automatic` for the permissions above; any other value for every object, such as `"Private"` for a trial run |

## Basic Examples

Publish the feedback of the site `"PacletPage"`, readable by the accounts of two email domains:

```wl
feedback = DeployPacletPageFeedback["PacletPage", "Title" -> "Paclet Documentation Sites", "Domains" -> {"wolframinstitute.org", "wolfram.com"}]
```

The form, which the site's Feedback panel posts to:

```wl
Hyperlink[feedback["Form"]]
```

Who is on the team that may read the entries:

```wl
feedback["Team"]
```
