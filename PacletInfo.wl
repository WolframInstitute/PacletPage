(* ::Package:: *)

PacletObject[<|
    "Name" -> "WolframInstitute/PacletPage",
    "Description" -> "Build and publish a paclet's documentation, from markdown to a published paclet with its documentation site",
    "Creator" -> "Nikolay Murzin",
    "PublisherID" -> "WolframInstitute",
    "License" -> "MIT",
    "Version" -> "0.3.5",
    "WolframVersion" -> "14.3+",
    "PrimaryContext" -> "WolframInstitute`PacletPage`",
    "Extensions" -> {
        {
            "Kernel",
            "Root" -> "Kernel",
            "Context" -> "WolframInstitute`PacletPage`",
            "Symbols" -> {
                "WolframInstitute`PacletPage`PacletPageCatalog",
                "WolframInstitute`PacletPage`PacletPageFiles",
                "WolframInstitute`PacletPage`DeployPacletPage",
                "WolframInstitute`PacletPage`DeployPacletPageFeedback",
                "WolframInstitute`PacletPage`PacletPageFeedback",
                "WolframInstitute`PacletPage`SetPacletPageFeedbackStatus",
                "WolframInstitute`PacletPage`SetPacletPagePermissions",
                "WolframInstitute`PacletPage`CheckPacletPage",
                "WolframInstitute`PacletPage`BuildPacletDocumentation",
                "WolframInstitute`PacletPage`DeployPacletResource",
                "WolframInstitute`PacletPage`SubmitPacletResource",
                "WolframInstitute`PacletPage`PublishPacletPage"
            }
        },
        {
            "Asset",
            "Assets" -> {{"Web", "Assets/Web"}, {"Example", "Assets/Example"}, {"Hero", "Assets/Hero.wxf"}}
        },
        {
            "Documentation",
            "Language" -> "English"
        }
    }
|>]
