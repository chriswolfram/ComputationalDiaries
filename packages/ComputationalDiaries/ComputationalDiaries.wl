(* ::Package:: *)

BeginPackage["ComputationalDiaries`"];


(* ::Subsection:: *)
(*Constants*)


GeneralUtilities`SetUsage[DiaryMergeMissing, "
DiaryMergeMissing[{m$1,m$2,$$}] algebraically combines missing values m$1, m$2, $$."];


GeneralUtilities`SetUsage[DiaryNormalStar, "
DiaryNormalStar[] returns the list of Normal Stars.
DiaryNormalStar[name$] returns the Normal Star associated with the specified name$."];


GeneralUtilities`SetUsage[$DiaryChronology, "
$DiaryChronology represents the current chronology, represented by pairs of coincident \
dates in Babylonian and Julian calendars."];


GeneralUtilities`SetUsage[DiaryCheckChronology, "
DiaryCheckChronology[chron$] checks whether the chronology chron$ is consistent.
DiaryCheckChronology[] checks $DiaryChronology."];


GeneralUtilities`SetUsage[Inferred, "
Inferred[data$] represents data that has been partially or totally inferred."];


GeneralUtilities`SetUsage[InferredPattern, "
InferredPattern[patt$] matches patt$ and Inferred[patt$]."];


GeneralUtilities`SetUsage[IgnoreInferred, "
IgnoreInferred[expr$] replaces all instances of Inferred[e$] with e$."];


GeneralUtilities`SetUsage[WrapInferred, "
WrapInferred[expr$] evaluates expr$, wrapping the result in Inferred if CheckInferred \
was applied to anything with head Inferred."];


GeneralUtilities`SetUsage[CheckInferred, "
CheckInferred[expr$] returns expr$
CheckInferred[Inferred[expr$]] returns expr$, notifying WrapInferred that it was inferred."];


(* ::Subsection:: *)
(*DiaryTypes*)


GeneralUtilities`SetUsage[DiaryJulianDate, "
DiaryJulianDate[{year$,month$,day$}] represents a date in the Julian calendar."];


GeneralUtilities`SetUsage[DiaryBabylonianDate, "
DiaryBabylonianDate[{{king$,year$},month$,day$}] represents a date in the Babylonian calendar."];


GeneralUtilities`SetUsage[DiaryCombinedDate, "
DiaryCombinedDate[$$] represents a date and time in a combination of the Julian and Babylonian calendars."];


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
(*DatasetConstruction*)


GeneralUtilities`SetUsage[DiaryDataset, "
DiaryDataset[obs$] constructs a Dataset from a list of DiaryObservations obs$."];


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
Needs["ComputationalDiaries`DatasetConstruction`"];
Needs["ComputationalDiaries`InputFormat`"];
Needs["ComputationalDiaries`ReferenceText`"];


Begin["`Private`"];


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
