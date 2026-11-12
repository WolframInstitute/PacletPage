(* ::Package:: *)

BeginPackage["Demo`Greetings`"]

Greet::usage = "Greet[name] greets name.";
Wave::usage = "Wave[name] waves at name.";

Begin["`Private`"]

Greet[name_String] := "Hello, " <> name <> mark[]

mark[] := "!"

Wave[name_String] := "*waves at " <> name <> "*"

End[]

EndPackage[]
