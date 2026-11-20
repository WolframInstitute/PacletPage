---
Template: Symbol
Name: PacletDependencyDialog
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletDependencyDialog
Keywords: [notebook, setup, desktop, optional libraries]
SeeAlso: [PacletDependencyInformation, InstallPacletDependencies, UpdatePacletDependencies, UninstallPacletDependencies]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement]
---

## Usage

<code>[PacletDependencyDialog]()[*owner*]</code> opens the desktop notebook dialog for the dependencies of *owner*.

<code>[PacletDependencyDialog]()[*owner*, "Optional"]</code> shows only the optional libraries declared by *owner*.

<code>[PacletDependencyDialog]()[*owner*, "Notify"]</code> issues a desktop setup message when optional dependencies are missing and the user has not dismissed setup for this paclet.

## Details & Options

- Opening the dialog reads the local installation. **Check for updates** looks up compatible releases for installed libraries. **Upgrade** appears beside a library when a newer release is available. **Install** looks up the release when you choose a missing library.
- **Check readiness** runs the tests supplied by the consumer for installed libraries that can run in this kernel. Each footer button becomes available when at least one library qualifies. Installing or removing a library updates both buttons.
- A running check shows a spinner and **Cancel check**. Cancellation takes effect after the current library. The footer summarizes completed checks. Hover over a row’s failure or setup notice to read the details.
- Installed optional libraries offer **Uninstall**, except for PacletPage itself.
- Rows follow declaration order. Each row shows the short library name and the consumer's description or, when available, the description from the installed library. Authors can declare a description so users can read it before installation. Rows can also include the download size, license and setup links.
- Use the copy control to take the corresponding install, update or uninstall command to another notebook.
- An operation keeps the dialog open. A transfer shows progress and offers cancellation. Opening setup again selects the existing dialog for that consumer.
- The `"Notify"` form is suitable for a consumer’s optional desktop startup hook. Dismissing setup records a local preference for that consumer. Scripts may use the other dependency functions directly.

## Basic Examples

Open the dependency dialog for the consumer from the Dependency Management tutorial in a desktop notebook.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer"]
```
