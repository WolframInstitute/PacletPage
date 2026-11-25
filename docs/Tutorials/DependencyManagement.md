---
Template: TechNote
Name: DependencyManagement
Title: Dependency Management
Context: WolframInstitute`PacletPage`
Paclet: WolframInstitute/PacletPage
URI: WolframInstitute/PacletPage/tutorial/DependencyManagement
Keywords: [dependencies, optional libraries, paclet metadata, installation, PureMath, setup]
RelatedGuides: [PacletDependencyManagement, PacletPage]
---

## Declare an optional computation

Suppose `Demo/Consumer` offers proofs through TypeTheory. Its ordinary computations can run before the user installs the proof engine. Put the following declaration in the consumer's `PacletInfo.wl`. Its `"Dependencies"` list names PacletPage, which the consumer needs to call its setup functions, and describes the proof engine, which `"Optional" -> True` marks as a library the consumer works without.

```wl
consumer = PacletObject[<|
    "Name" -> "Demo/Consumer",
    "Version" -> "1.0.0",
    "PrimaryContext" -> "Demo`Consumer`",
    "Extensions" -> {{"Kernel", "Root" -> "Kernel", "Context" -> "Demo`Consumer`"}},
    "Dependencies" -> {
        "WolframInstitute/PacletPage",
        <|"Name" -> "WolframInstitute/TypeTheory",
          "Optional" -> True,
          "Description" -> "Type checking for the Calculus of Inductive Constructions.",
          "Resource" -> "https://www.wolframcloud.com/obj/wolframinstitute/DeployedResources/Paclet/WolframInstitute/TypeTheory",
          "License" -> "MIT",
          "URL" -> "https://github.com/WolframInstitute/TypeTheory#loading",
          "Check" -> "Demo`Consumer`TypeTheoryReadyQ"|>
    }
|>];
```

Save the `PacletObject[…]` expression as the contents of the file. The `consumer` binding here also lets you inspect the declaration before registering the paclet.

```wl
Keys[PacletDependencyInformation[consumer]]
```

<!-- => {"WolframInstitute/PacletPage", "WolframInstitute/TypeTheory"} -->

A bare dependency name uses the configured paclet sites. The other sources are a public Wolfram Cloud paclet `"Resource"`, a local `"Archive"`, such as `"Resources/Algebra.paclet"`, relative to the consumer directory, or a GitHub repository at a commit, `"Git"` with `"Commit"`, and `"Path"` where the paclet is not at the repository's root. Choose one source per library. Cloud resources may select their archive by system. Each dependency appears once in the list.

| Declaration | Meaning |
|---|---|
| `"Name"` | Exact paclet name, including its publisher when applicable |
| `"Optional"` | `True` for a library the consumer works without; a dependency is required by default |
| `"Version"` | A numeric version such as `"1.2.0"`, a minimum such as `"1.2+"`, a version family such as `"1.2.*"`, or `"*"` for any version |
| `"Platforms"` | List of supported system IDs. Omitting the field allows every platform |
| `"Description"`, `"License"` | What the library provides and its license or account requirements |
| `"DownloadSize"` | Display text or an association of system IDs to display text |
| `"URL"`, `"SetupLinks"` | Setup instructions and an optional association of additional link labels to HTTPS URLs |
| `"Check"` | Fully qualified name of a zero-argument readiness function in the consumer's kernel context |
| `"Git"`, `"Commit"`, `"Path"` | GitHub repository URL, commit ID (7 to 40 hexadecimal digits, either case), and optional paclet subdirectory |

This declaration installs Wolfram/Parser from the `Parser` directory at the specified commit.

```wl
#| eval: false
"Dependencies" -> {
    <|
        "Name" -> "Wolfram/Parser",
        "Optional" -> True,
        "Git" -> "https://github.com/sw1sh/WolframParser",
        "Commit" -> "5e05149ead1c292ebd39d02eee85f1c0b8059f55",
        "Path" -> "Parser"
    |>
}
```

The version and runtime requirements come from `PacletInfo.wl`, or from `PacletInfo.m` when the repository uses the legacy filename. The source manifest must give these as literal values. **Check for updates** verifies the requirements before offering a Git release.

## Check that a computation works

Put the check in the consumer's kernel package. This one checks whether the installed engine can assign a type to a natural number. Keeping a library's context local to the check lets a notebook continue to resolve unqualified symbols as before.

```wl
#| eval: false
Demo`Consumer`TypeTheoryReadyQ[] := Block[{$ContextPath = $ContextPath},
    Needs["WolframInstitute`TypeTheory`"] === Null &&
        With[{term = Symbol["WolframInstitute`TypeTheory`TypeTheoryTerm"],
              type = Symbol["WolframInstitute`TypeTheory`TypeTheoryType"]},
            MatchQ[type[term[2 + 3]], HoldPattern[term[NonNegativeIntegers, _]]]]
]
```

Register the consumer directory with `PacletDirectoryLoad` during development. Installed consumers can be addressed directly by name. Call the check explicitly or let installation run it. A failed check produces **Needs setup**. Expand the report or hover over that notice in the dialog to read the consumer’s diagnostic, such as a missing toolchain, license or system executable.

```wl
#| eval: false
PacletDependencyInformation["Demo/Consumer", "WolframInstitute/TypeTheory", "CheckReadiness" -> True]
```

## Install and update

Open the dialog from a desktop notebook. It reads the local installation. Choose **Install** beside a missing library to fetch its release. Once a library is installed, **Check for updates** looks up compatible releases for it and the other installed libraries. **Check readiness** runs any applicable tests supplied by the consumer. Each button becomes available as soon as a library qualifies.

The footer names the library being checked. **Upgrade** appears for a newer release or a changed Git pin at the same version in the user repository. **Cancel check** stops after the current library and keeps completed results.

When Wolfram internet access is disabled, you can still check a local archive, a matching installed Git pin, or a bare name with a compatible release in the cached site index. Sources that need internet access show **Offline**. A version lookup that times out after ten seconds shows **Unavailable** and the check continues. Hover over a failure notice for details.

The [Using the Dependency Dialog](paclet:WolframInstitute/PacletPage/tutorial/UsingTheDependencyDialog) tutorial shows the dialog in each of these states.

Click **Install** or **Upgrade** to start a download. The library’s row shows progress above **Cancel**. A percentage appears when the publisher supplies the archive size. If you cancel, the row keeps its installed version and you can retry in the same window. You can also copy the command and run it in another notebook. When you return, the dialog refreshes the installed versions and available actions.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer"]
```

For a setup command specific to proofs, give the dialog a heading and an introduction that say what the libraries enable. The dialog still lists every dependency. The title also appears in the window’s title bar. Use `"Description" -> ""` to omit the introduction.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer",
    "Title" -> "Proof libraries",
    "Description" -> "Install TypeTheory to check proofs in this notebook."]
```

A script can apply the same operations to one name, a list of names, `All`, `"Required"`, or `"Optional"`. Pass `"CheckReadiness" -> False` when installing for headless documentation builds. The following cells change the current user's installation when evaluated.

```wl
#| eval: false
InstallPacletDependencies["Demo/Consumer", "Required", "CheckReadiness" -> False]
```

```wl
#| eval: false
InstallPacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

```wl
#| eval: false
UpdatePacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

```wl
#| eval: false
UninstallPacletDependencies["Demo/Consumer", "WolframInstitute/TypeTheory"]
```

Version-based updates retain an equal or newer compatible version. With a Git source, PacletPage uses `WolframInstitute/GitLink` for shallow fetch and sparse path checkout when installed, falling back to GitHub archives otherwise. Changing the pin can replace a user-installed library even when both commits declare the same version. A matching pin can be reused offline. Restart the kernel when a loaded library changes. Uninstalling removes user-installed copies, which other consumers may also use.

## Bundle dependencies inside a published paclet

To ship required (or all) dependencies inside a published `.paclet` archive for offline installation, pass `"BundleDependencies" -> "Required"` (or `All`, or a list of names) to [DeployPacletResource](), [SubmitPacletResource](), or [PublishPacletPage](). PacletPage packs compatible installed copies into `Assets/Dependencies/` in the staged build and registers `{"Dependencies", "Dependencies"}` in the paclet's `"Asset"` extension. When a user runs [InstallPacletDependencies](), matching bundled archives in `owner["AssetLocation", "Dependencies"]` (including `.pacletpage-git.json` receipts for Git-pinned dependencies) are installed offline first without network access.

For headless documentation builds that need every declared library (both required and optional) available so all examples evaluate, pass `"InstallDependencies" -> All` to [BuildPacletDocumentation]() or [PublishPacletPage](). At runtime load, a consumer installs only `"Required"` dependencies automatically and leaves `"Optional"` dependencies to [PacletDependencyDialog]().

## Make setup work on a fresh installation

Installing a consumer archive can leave its required dependencies absent. A consumer can ship a small loader that works before PacletPage is available. When preparing the consumer distribution, copy the loader from an installed PacletPage release with dependency management. Here `dir` is the consumer directory and `Resources` is part of its distribution.

```wl
#| eval: false
manager = PacletObject["WolframInstitute/PacletPage"];
resources = FileNameJoin[{dir, "Resources"}];
If[!DirectoryQ[resources], CreateDirectory[resources]];
CopyFile[manager["AssetLocation", "DependencyLoader"], FileNameJoin[{resources, "LoadDependencyManager.wl"}]]
```

For offline setup, put the publisher's PacletPage release archive beside the loader and name it `PacletPage.paclet`. Here `archive` is the path to that release file.

```wl
#| eval: false
CopyFile[archive, FileNameJoin[{resources, "PacletPage.paclet"}]]
```

The loader prefers a compatible installed copy. On first use of a bundled archive, it extracts it into the user's cache and checks its compatibility with the running Wolfram system before loading it. Other consumers can reuse the same cached release. The loader checks the interface the paclet supplies, so two builds with the same version can be distinguished. If neither copy supplies the manager, the returned failure gives the publisher's install command. A kernel that already loaded an older package receives a restart instruction.

## Connect an existing consumer

PureMath can declare GAPLink and LeanLink and keep their runtime checks next to the computations that use them. A Lean check can call `` WolframInstitute`PureMath`PackageScope`leanLinkReadyQ[] ``. The following wrapper loads the manager when a user opens setup.

```wl
#| eval: false
PureMathSetup[] := With[{owner = PacletObject["WolframInstitute/PureMath"]},
    Replace[Get[FileNameJoin[{owner["Location"], "Resources", "LoadDependencyManager.wl"}]],
        Null :> WolframInstitute`PacletPage`PacletDependencyDialog[owner]]]
```

A consumer that wants a desktop reminder can call `PacletDependencyDialog[owner, "Notify"]` at startup. Dismissing the dialog records the preference for that consumer. Batch kernels can load the consumer and run ordinary computations independently of desktop setup.
