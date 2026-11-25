---
Template: Symbol
Name: PacletDependencyDialog
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/ref/PacletDependencyDialog
Keywords: [notebook, setup, desktop, optional libraries]
SeeAlso: [PacletDependencyInformation, InstallPacletDependencies, UpdatePacletDependencies, UninstallPacletDependencies]
RelatedGuides: [PacletDependencyManagement]
RelatedTutorials: [DependencyManagement, UsingTheDependencyDialog]
---

## Usage

<code>[PacletDependencyDialog]()[*owner*]</code> opens the desktop notebook dialog for the dependencies of *owner*, required and optional.

<code>[PacletDependencyDialog]()[*owner*, *opts*]</code> customizes the dialog for the consumer.

<code>[PacletDependencyDialog]()[*owner*, "Notify"]</code> issues a desktop setup message when optional dependencies are missing and the user has not dismissed setup for this paclet.

## Details & Options

- The dialog follows the light or dark appearance of the notebook that opened it, including its notebook theme when supported.
- `"Title" -> "text"` sets the dialog heading. The window title shows the consumer name followed by this heading. The default `Automatic` uses **Library dependencies** when the dialog includes a required library and **Optional libraries** otherwise. A custom title must contain non-whitespace text.
- `"Description" -> "text"` sets the introduction below the heading. The default `Automatic` displays “Install, upgrade, or remove libraries as needed.” An empty string hides the introduction. An offline notice takes precedence over the introduction.
- Calling the dialog again for the same consumer applies the supplied options to the open window. Omitted options use their defaults. After a kernel restart, reopening setup reads the local installation again in the same window.
- Opening the dialog reads the local installation. **Check for updates** looks up compatible releases for installed libraries. **Upgrade** appears when a newer release is available or a user installation needs a different Git commit at the same version. **Install** looks up the release when you choose a missing library.
- **Check readiness** runs the tests supplied by the consumer for installed libraries that can run in this kernel. Each footer button becomes available when at least one library qualifies. Installing or removing a library updates both buttons.
- A running check shows a spinner and **Cancel check**. Cancellation takes effect after the current library. The footer summarizes completed checks. Hover over a row’s failure or setup notice to read the details.
- **Uninstall** removes user-installed copies of optional libraries. PacletPage protects its own installation. A Git dependency supplied by an external checkout has no **Uninstall** action.
- Rows follow declaration order. Each row shows the short library name and the consumer's description or, when available, the description from the installed library. Authors can declare a description so users can read it before installation. Rows can also include the download size, license and setup links.
- Use the copy control to run the corresponding install, update or uninstall command in another notebook. Returning to the dialog refreshes the installed versions and available actions.
- An operation keeps the dialog open. A transfer shows progress and offers cancellation. Opening setup again selects the existing dialog for that consumer.
- The `"Notify"` form is suitable for a consumer’s optional desktop startup hook. Dismissing setup records a local preference for that consumer. Scripts may use the other dependency functions directly.

## Basic Examples

Open the dependency dialog for the consumer from the Dependency Management tutorial in a desktop notebook.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer"]
```
