(* ::Package:: *)

BeginPackage["ComputationalDiaries`DiaryObservation`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*DiaryObservation*)


DiaryObservation[data_]["Data"] := data
DiaryObservation[data_]["Type"] := data["Type"]
DiaryObservation[data_]["Content"] := data["Content"]
DiaryObservation[data_]["Date"] := data["Date"]
DiaryObservation[data_]["Provenance"] := data["Provenance"]
DiaryObservation[data_][other_] /; KeyExistsQ[data["Content"],other] :=
	data["Content"][other]


(* ::Subsection:: *)
(*Type specific methods*)


negativeRelations = {"Below","South","InFrontOf","West"};


relationOrientation["InFrontOf"|"Behind"|"East"|"West"] := "Horizontal"
relationOrientation["Above"|"Below"|"North"|"South"] := "Vertical"
relationOrientation[_] := Missing[]


obv_DiaryObservation["Distances"]/;obv["Type"]==="RelativePosition" :=
	obv["Displacements"][[All,1]]
obv_DiaryObservation["Relations"]/;obv["Type"]==="RelativePosition" :=
	obv["Displacements"][[All,2]]
obv_DiaryObservation[p:"IdealDegrees"|"RealDegrees"]/;obv["Type"]==="RelativePosition" :=
	Catch@Module[{groupedDisplacements},
		groupedDisplacements = GroupBy[obv["Displacements"],relationOrientation[#[[2]]]&];
		If[Max[Length/@groupedDisplacements]>1,Throw[Missing[]]];
		Function[orientation, Module[{dist,rel},
			{dist,rel} = Lookup[groupedDisplacements[[All,1]],orientation,Missing[]];
			If[dist[p]===Missing["Unmentioned"], 0,
				If[MissingQ[dist[p]]||MissingQ[rel], combineMissings[{dist[p],rel}],
					dist[p]*If[MemberQ[negativeRelations,rel],-1,1]
				]
			]
		]]/@{"Horizontal","Vertical"}
	]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
