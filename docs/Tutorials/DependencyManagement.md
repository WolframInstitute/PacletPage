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

Suppose `Demo/Consumer` offers proofs through TypeTheory. Its ordinary computations can run before the user installs the proof engine. Put the following declaration in the consumer's `PacletInfo.wl`. The standard `"Dependencies"` list names PacletPage so that the consumer can call its setup functions. The separate `"OptionalDependencies"` list describes the proof engine.

```wl
consumer = PacletObject[<|
    "Name" -> "Demo/Consumer",
    "Version" -> "1.0.0",
    "PrimaryContext" -> "Demo`Consumer`",
    "Extensions" -> {{"Kernel", "Root" -> "Kernel", "Context" -> "Demo`Consumer`"}},
    "Dependencies" -> {"WolframInstitute/PacletPage"},
    "OptionalDependencies" -> {
        <|"Name" -> "WolframInstitute/TypeTheory",
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

A bare dependency name uses the configured paclet sites. The other sources are a public Wolfram Cloud paclet `"Resource"` or a local `"Archive"`, such as `"Resources/Algebra.paclet"`, relative to the consumer directory. Choose one source per library. Cloud resources may select their archive by system. Each dependency appears once across both lists.

| Declaration | Meaning |
|---|---|
| `"Name"` | Exact paclet name, including its publisher when applicable |
| `"Version"` | A numeric version such as `"1.2.0"`, a minimum such as `"1.2+"`, a version family such as `"1.2.*"`, or `"*"` for any version |
| `"Platforms"` | List of supported system IDs. Omitting the field allows every platform |
| `"Description"`, `"License"` | What the library provides and its license or account requirements |
| `"DownloadSize"` | Display text or an association of system IDs to display text |
| `"URL"`, `"SetupLinks"` | Setup instructions and an optional association of additional link labels to HTTPS URLs |
| `"Check"` | Fully qualified name of a zero-argument readiness function in the consumer's kernel context |

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

Register the consumer directory with `PacletDirectoryLoad` during development. Installed consumers can be addressed directly by name. Call the check explicitly or let installation run it. A failed check produces **Needs setup**. A missing toolchain, license or system executable can require a separate setup step even after installation succeeds.

```wl
#| eval: false
PacletDependencyInformation["Demo/Consumer", "WolframInstitute/TypeTheory", "CheckReadiness" -> True]
```

## Install and update

Open the dialog from a desktop notebook. It reads the local installation. Choose **Install** beside a missing library to fetch its release. Once a library is installed, **Check for updates** looks up compatible releases for it and the other installed libraries. **Check readiness** runs any applicable tests supplied by the consumer. Each button becomes available as soon as a library qualifies.

The footer names the library being checked. A newer release adds **Upgrade** to that library’s row. **Cancel check** stops after the current library and keeps completed results. When Wolfram internet access is disabled, cloud releases show **Offline**. You can still check a local archive. A version lookup that times out after ten seconds shows **Unavailable** and the check continues. Hover over a failure notice for details.

Click **Install** or **Upgrade** to start a download. The library’s row shows progress above **Cancel**. A percentage appears when the publisher supplies the archive size. If you cancel, the row keeps its installed version and you can retry in the same window. You can also copy the command and run it in another notebook. When you return, the dialog refreshes the installed versions and available actions.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer"]
```

For a setup command specific to proofs, show only optional libraries and explain what they enable. The title also appears in the window’s title bar. Use `"Description" -> ""` to omit the introduction.

```wl
#| eval: false
PacletDependencyDialog["Demo/Consumer", "Optional",
    "Title" -> "Proof libraries",
    "Description" -> "Install TypeTheory to check proofs in this notebook."]
```

A script can apply the same operations to one name, a list of names or `All`. Each selected name has its own result. The following cells change the current user's installation when evaluated.

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

An update retains an equal or newer compatible version. Restart the kernel when a loaded library changes. Uninstalling removes user-installed copies, which other consumers may also use.

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
