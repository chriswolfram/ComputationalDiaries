(* ::Package:: *)

BeginPackage["ComputationalDiaries`DatasetConstruction`", {"ComputationalDiaries`"}];


Begin["`Private`"];


(* ::Subsection:: *)
(*Dataset construction*)


toDataset[e_] := e
toDataset[e:(_List|_Association)] := toDataset/@e
toDataset[e_DiaryObservation] :=
	toDataset/@AssociationMap[e, {"Type","Content","Date","Provenance"}]
toDataset[e_DiaryJulianDate] := toDataset/@AssociationMap[e, {"Year","Month","Day"}]
toDataset[e_DiaryBabylonianDate] := toDataset/@AssociationMap[e, {"Year","Month","Day"}]
toDataset[e_DiaryCombinedDate] :=
	toDataset/@AssociationMap[e, {"JulianDate","BabylonianDate","Time","DateObject"}]
toDataset[e_DiaryDistance] :=
	toDataset/@AssociationMap[e, {"Cubits","Fingers","TotalCubits","IdealDegrees","RealDegrees"}]
toDataset[e_DiaryDuration] :=
	toDataset/@AssociationMap[e, {"Degrees","ArcMinutes","Minutes"}]
toDataset[e_DiaryCapacity] :=
	toDataset/@AssociationMap[e, {"Kur","Pan","Sut","Qa","TotalQa"}]


DiaryDataset[obs_,opts___] := Dataset[toDataset[obs],opts]


(* ::Subsection:: *)
(*End*)


End[];
EndPackage[];
