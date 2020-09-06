(* ::Package:: *)

BeginPackage["ComputationalDiaries`Constants`", {"ComputationalDiaries`"}];


Begin["`Private`"];


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
