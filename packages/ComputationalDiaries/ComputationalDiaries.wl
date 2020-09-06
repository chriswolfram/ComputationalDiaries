(* ::Package:: *)

BeginPackage["ComputationalDiaries`"];


(* ::Subsection:: *)
(*Constants*)


combineMissings::usage = "Algebraically combines Missing[]s.";
getNormalStars::usage = "getNormalStars[] returns the list of normal stars.
getNormalStars[\!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\)] returrns the normal star associated with the name \!\(\*
StyleBox[\"name\",\nFontSlant->\"Italic\"]\).";
normalStarQ::usage = 
	"normalStar[\!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\)] returns True of \!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\) is a normal star and False otherwise.";


(* ::Subsection:: *)
(*DiaryTypes*)


DiaryDate::usage = "Represents a date in a combination of the Julian and Babylonian calendars.";
DiaryDistance::usage = "Represents a distance in the sky in cubits (K\[CapitalUGrave]\[CapitalSHacek]) and fingers (\[CapitalSHacek]I or U).";
DiaryDuration::usage = "Represents a duration in Babylonian units.";
DiaryCapacity::usage =
	"Represents an amount of barely, dates, mustard, cress, or sesame in MarketRates observations.";


(* ::Subsection:: *)
(*DiaryObservation*)


DiaryObservation::usage = "Represents an observation in the Astronomical Diaries.";


(* ::Subsection:: *)
(*AstronomicalPosition*)


astronomicalPosition::usage =
	"astronomicalPosition[\!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"date\",\nFontSlant->\"Italic\"]\)] computes the ecliptic coordinantes for \!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\) on \!\(\*
StyleBox[\"date\",\nFontSlant->\"Italic\"]\).";


(* ::Subsection:: *)
(*InputFormat*)


processInputFormat::usage = "Canonicalizes the input format.";
launchCurationPalette::usage = "Opens a palette with useful templates for Diary curation.";
inputMonthDay::usage =
	"inputMonthDay[\!\(\*
StyleBox[\"day\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"time\",\nFontSlant->\"Italic\"]\)] represents a day and time of the month for the input format.";
replaceButton::usage = "Makes a button that replaces its parent box when pressed.";
replaceButtonTemplate::usage =
	"Makes a collection of replaceButtons for importing common formats.";


(* ::Subsection:: *)
(*ReferenceText*)


getTabletLines::usage =
	"Uses a reference copy of Oracc adsd to give the content of a tablet.";
showTablet::usage = "Uses getTabletLines to show the content of the specified tablet.";


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
