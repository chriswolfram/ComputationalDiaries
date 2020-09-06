(* ::Package:: *)

BeginPackage["ComputationalDiaries`ReferenceText`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*Implementation*)


(*MX files generated in astronomicalDiariesScraping-02.nb*)


tabletTexts = Import[FileNameJoin[{DirectoryName[$InputFileName],"tabletTexts.mx"}]];


getTabletLines::unknownTablet = "`` is not a valid tablet ID.";


getTabletLines[] := tabletTexts
getTabletLines[tabletID_] :=
	Lookup[tabletTexts, tabletID, Message[getTabletLines::unknownTablet,tabletID];$Failed]


showTablet[tabletID_] := With[{lines = getTabletLines[tabletID]},
	If[lines === $Failed, $Failed,
		Grid[lines/.Delimiter->{},Alignment->Left,Frame->All]
	]
]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
