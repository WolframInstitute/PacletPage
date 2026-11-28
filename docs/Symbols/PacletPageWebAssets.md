---
Template: Symbol
Name: PacletPageWebAssets
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletPageWebAssets
Keywords: [paclet, documentation, website, shell, stylesheet, script, Wolfram Studio]
SeeAlso: [PacletPage, PacletPageFiles, DeployPacletPage]
RelatedGuides: [PacletPage]
RelatedTutorials: [PublishingADocumentationSite]
---

## Usage

`PacletPageWebAssets[]` gives the files a documentation site's shell is made with: an association of `"ShellCSS"`, the stylesheet, `"ShellJS"`, the script module, and `"Version"`, the version of PacletPage they belong to.

## Details & Options

- The shell is what surrounds a page: the header with its filter and its Copy link button, the rail of guides and tutorials, the rail of symbols with its A-Z index, the splitters between them, the layout buttons, and the Feedback panel.
- A published site loads these two files, and so does a host that shows a paclet's pages in the same shell, such as Wolfram Studio, so the shell looks and behaves the same in both.
- `"ShellCSS"` and `"ShellJS"` are absolute paths of the files of the PacletPage running, from its `Assets/Web` directory.
- `"ShellJS"` is an ES module. It exports `mountPacletShell(root, options)`, which gives a shell its behavior: the filter, the symbol index, the layout buttons, the splitters, the active page, and the Feedback panel. It returns an object with `destroy()`, which takes away everything mounting added, `setActive(id)`, which marks the page `id` as the one shown, and `refresh()`, which reads the shell's elements again after a host replaces them. A shell mounted again gives the same object.
- The options are callbacks through which a host takes over what a published site does itself: `onNavigate` opens a page, `onFeedback` sends the Feedback panel's report, `onToggleView` switches between a page and its source, `onCopy` copies text, `getPageUrl` gives the address the header's Copy link button copies and `onCopyLink` hears that it was copied, `getState` and `setState` keep the rails' widths and the layout, `defaultEmail` fills the report's address, and `t` translates the shell's text. Without them the shell does what a published site does.
- `"ShellCSS"` draws the shell in the colors of its custom properties, such as `--pp-bg`, `--pp-ink`, `--pp-line` and `--pp-accent`, light by default and dark where the document's root has `data-theme="dark"`. A host sets them to its own colors.

## Basic Examples

The shell's files and the version they belong to:

```wl
Keys[PacletPageWebAssets[]]
```

They are the stylesheet and the script module of this PacletPage:

```wl
FileNameTake /@ KeyTake[PacletPageWebAssets[], {"ShellCSS", "ShellJS"}]
```

## Scope

Both files are there to load:

```wl
FileExistsQ /@ KeyTake[PacletPageWebAssets[], {"ShellCSS", "ShellJS"}]
```

The version is a PacletPage version, the one whose files these are:

```wl
StringMatchQ[PacletPageWebAssets[]["Version"], DigitCharacter .. ~~ ("." ~~ DigitCharacter ..) ...]
```
