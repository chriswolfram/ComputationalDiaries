(* ::Package:: *)

pink 


BeginPackage["ComputationalDiaries`"];


(* ::Subsection:: *)
(*Constants*)


GeneralUtilities`SetUsage[DiaryMergeMissing, "
DiaryMergeMissing[{m$1,m$2,$$}] algebraically combines missing values m$1, m$2, $$."];


GeneralUtilities`SetUsage[DiaryNormalStar, "
DiaryNormalStar[] returns the list of Normal Stars.
DiaryNormalStar[name$] returns the Normal Star associated with the specified name$."];


(* ::Subsection:: *)
(*DiaryTypes*)


GeneralUtilities`SetUsage[DiaryDate, "
DiaryDate[$$] represents a date in a combination of the Julian and Babylonian calendars."];


GeneralUtilities`SetUsage[DiaryDistance, "
DiaryDistance[{cubits$,fingers$}] represents a distance on the sky in cubits (K\[CapitalUGrave]\[CapitalSHacek]) and fingers (\[CapitalSHacek]I or U)."];


GeneralUtilities`SetUsage[DiaryDuration, "
DiaryDuration[{deg$,nin$}] represents a duration in degrees and NINDA."];


GeneralUtilities`SetUsage[DiaryCapacity, "
DiaryCapacity[{kur$,pan$,sut$,qa$}] represents a capacity of a commodity in kurru, p\[ABar]nu, s\:016btu, and qa."];


(* ::Subsection:: *)
(*DiaryObservation*)


GeneralUtilities`SetUsage[DiaryObservation, "
DiaryObservation[$$] represents an observation in the Astronomical Diaries."];


(* ::Subsection:: *)
(*AstronomicalPosition*)


GeneralUtilities`SetUsage[DiaryAstronomicalPosition, "
DiaryAstronomicalPosition[obj$,dateSpec$] computes the position of obj$ at time dateSpec$ relative to the ecliptic."];


(* ::Subsection:: *)
(*InputFormat*)


GeneralUtilities`SetUsage[DiaryParse, "
DiaryParse[data$] processes data$, returning a list of DiaryObservations.
"];


GeneralUtilities`SetUsage[DiaryCurationPalette, "
DiaryCurationPalette[] launches the curation palette.
"];


GeneralUtilities`SetUsage[DiaryInputDate, "
DiaryInputDay[day$,time$] represents a date and time in the input format.
"];


GeneralUtilities`SetUsage[DiaryInputDate, "
DiaryInputDay[day$,time$] represents a date and time in the input format.
"];


GeneralUtilities`SetUsage[DiaryInputTemplate, "
DiaryInputTemplate[t$] returns the input format template specified by t$.
DiaryInputTemplate[\"Row\",l$] returns a template row of the elements of l$.
DiaryInputTemplate[\"Column\",l$] returns a template column of the elements of l$.
DiaryInputTemplate[\"Missing\",n$] returns a template supporting selection of n$ or Missing.
"];


(* ::Subsection:: *)
(*ReferenceText*)


GeneralUtilities`SetUsage[DiaryTabletData, "
DiaryTabletData[tablet$] returns metadata associated with the specified tablet.
DiaryTabletData[] returns an association containing metadata for all known tablets.
"];


GeneralUtilities`SetUsage[DiaryTabletText, "
DiaryTabletText[tablet$] returns the text of the specified tablet.
DiaryTabletText[] returns an association containing text for all known tablets.
"];


GeneralUtilities`SetUsage[DiaryTabletTable, "
DiaryTabletTable[tablet$] makes a visual representation of the specified tablet.
"];


(* ::Subsection:: *)
(*Needs*)


Needs["ComputationalDiaries`Constants`"];
Needs["ComputationalDiaries`DiaryTypes`"];
Needs["ComputationalDiaries`DiaryObservation`"];
Needs["ComputationalDiaries`AstronomicalPosition`"];
Needs["ComputationalDiaries`InputFormat`"];
Needs["ComputationalDiaries`ReferenceText`"];


Begin["`Private`"];


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
