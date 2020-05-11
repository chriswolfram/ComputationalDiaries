(* ::Package:: *)

BeginPackage["ComputationalDiaries`ReferenceText`"];


getTabletLines::usage =
	"Uses a reference copy of Oracc adsd to give the content of a tablet.";
showTablet::usage = "Uses getTabletLines to show the content of the specified tablet.";


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
