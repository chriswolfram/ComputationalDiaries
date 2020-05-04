(* ::Package:: *)

BeginPackage["ComputationalDiaries`Constants`"];


ClearAll["ComputationalDiaries`Constants*"];


GetNormalStars::usage = "GetNormalStars[] returns the list of normal stars.
GetNormalStars[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] returrns the normal star associated with the name \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\).";
NormalStarQ::usage =
	"NormalStarQ[\!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\)] returns True if \!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"is\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"a\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"normal\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"star\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"and\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"False\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\"otherwise\",\nFontSlant->\"Plain\"]\)\!\(\*
StyleBox[\".\",\nFontSlant->\"Plain\"]\)";


Begin["`Private`"];


(* ::Subsection:: *)
(*Normal stars*)


normalStarMap = <|
	"EtaPiscium"->Entity["Star","EtaPiscium"],
	"BetaArietis"->Entity["Star","Sheratan"],
	"AlphaArietis"->Entity["Star","Hamal"],
	"EtaTauri"->Entity["Star","Alcyone"],
	"AlphaTauri"->Entity["Star","Aldebaran"],
	"BetaTauri"->Entity["Star","Alnath"],
	"ZetaTauri"->Entity["Star","ZetaTauri"],
	"EtaGeminorum"->Entity["Star","Propus"],
	"MuGeminorum"->Entity["Star","Tejat"],
	"GammaGeminorum"->Entity["Star","Alhena"],
	"AlphaGeminorum"->Entity["Star","Castor"],
	"BetaGeminorum"->Entity["Star","Pollux"],
	"EtaCancri"->Entity["Star","EtaCancri"],
	"ThetaCancri"->Entity["Star","ThetaCancri"],
	"GammaCancri"->Entity["Star","AsellusBorealis"],
	"DeltaCancri"->Entity["Star","AsellusAustralis"],
	"EpsilonLeonis"->Entity["Star","EpsilonLeonis"],
	"AlphaLeonis"->Entity["Star","Regulus"],
	"RhoLeonis"->Entity["Star","RhoLeonis"],
	"ThetaLeonis"->Entity["Star","Chort"],
	"BetaVirginis"->Entity["Star","Alaraph"],
	"GammaVirginis"->Entity["Star","Porrima"],
	"AlphaVirginis"->Entity["Star","Spica"],
	"AlphaLibrae"->Entity["Star","Alpha1Librae"],
	"BetaLibrae"->Entity["Star","Zubeneshamali"],
	"DeltaScorpii"->Entity["Star","Dschubba"],
	"BetaScorpii"->Entity["Star","Beta2Scorpii"],
	"AlphaScorpii"->Entity["Star","Antares"],
	"ThetaOphiuchi"->Entity["Star","ThetaOphiuchi"],
	"BetaCapricorni"->Entity["Star","Dabih"],
	"GammaCapricorni"->Entity["Star","Nashira"],
	"DeltaCapricorni"->Entity["Star","DenebAlgiedi"],
	"PiScorpii"->Entity["Star","PiScorpii"] (*not mentioned in H&S intro, but appears in texts*)
|>;


GetNormalStars[] := Values[normalStarMap]
GetNormalStars[name_String] := normalStarMap[name]


NormalStarQ[obj_] := MemberQ[Values[normalStarMap],obj]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
