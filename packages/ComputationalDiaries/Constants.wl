(* ::Package:: *)

BeginPackage["ComputationalDiaries`Constants`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*Inferred*)


InferredPattern[patt_] := patt | Inferred[patt]


IgnoreInferred[d_] := d/.Inferred[e_]:>e


inferredTag
SetAttributes[WrapInferred,HoldFirst];
WrapInferred[expr_] :=
	With[{o = Reap[expr,inferredTag]},
		If[o[[2]] === {}, o[[1]], Inferred[o[[1]]]]
	]
CheckInferred[Inferred[expr_]] := (Sow[True, inferredTag]; expr)
CheckInferred[expr_] := expr


(* ::Subsection:: *)
(*DiaryChronology*)


$DiaryChronology = {};


DiaryCheckChronology[chron:{{_DiaryBabylonianDate,_DiaryJulianDate}...}] :=
	Enclose[Module[{months},
		months = GroupBy[chron, {#[[1]]["Year"],#[[1]]["Month"]}&];
		Function[m,
			ConfirmAssert[
				SameQ@@(DiaryJulianDate[DatePlus[#[[2]]["DateObject"],-#[[1]]["Day"]]]&/@m),
				Failure["InvalidChronology", <|
					"MessageTemplate"->"Pairs `` are not consistent.",
					"MessageParameters"->{m},
					"InvalidPairs"->m|>]]
		]/@months;
		Success["ValidChronology",<|"Message"->"Chronology is consistent."|>]
	], "Information"]
DiaryCheckChronology[_] :=
	Failure["InvalidChronology",
		<|"Message"->"Chronology is not a list of pairs of Babylonian and Julian dates."|>]
DiaryCheckChronology[] := DiaryCheckChronology[$DiaryChronology]


(* ::Subsection:: *)
(*DiaryMergeMissing*)


DiaryMergeMissing[missings_] :=
	Which[
		MemberQ[missings,Missing[]],Missing[],
		MemberQ[missings,Missing["Destroyed"]],Missing["Destroyed"],
		MemberQ[missings,Missing["Unmentioned"]],Missing["Unmentioned"],
		True,Missing[]
	]


(* ::Subsection:: *)
(*DiaryNormalStar*)


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


DiaryNormalStar[] := Values[normalStarMap]
DiaryNormalStar[name_String] := normalStarMap[name]


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
