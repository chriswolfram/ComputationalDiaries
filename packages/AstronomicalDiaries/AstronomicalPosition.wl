(* ::Package:: *)

BeginPackage["AstronomicalDiaries`AstronomicalPosition`",{
	"AstronomicalDiaries`DiaryTypes`",
	"AstronomicalDiaries`Constants`"
}];


ClearAll["AstronomicalDiaries`AstronomicalPosition`*"];


AstronomicalPosition::usage =
	"AstronomicalPosition[\!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\),\!\(\*
StyleBox[\"date\",\nFontSlant->\"Italic\"]\)] computes the ecliptic coordinantes for \!\(\*
StyleBox[\"obj\",\nFontSlant->\"Italic\"]\) on \!\(\*
StyleBox[\"date\",\nFontSlant->\"Italic\"]\).";


Begin["`Private`"];


ephemPath = FileNameJoin[{DirectoryName[$InputFileName],"ephemerides"}];


pythonSession = StartExternalSession["Python"];
ExternalEvaluate[pythonSession,StringTemplate["
import os
import json
from skyfield.api import Star, load
from skyfield.data import hipparcos

# Set working directory to point at ephemerides
os.chdir(\"``\")

# Load stellar movement data from Hipparcos
with load.open(hipparcos.URL) as f:
    df = hipparcos.load_dataframe(f)

# Load ephemerides from JPL DE422 for planetary movement
planets = load(\"de422.bsp\")
earth = planets[\"earth\"]
ts = load.timescale()
"][ephemPath]];


synodicPosition = ExternalFunction[pythonSession,"
def object_position(name,day):
	t = ts.tdb(jd=day)
	obj = planets[name]
	astrometric = earth.at(t).observe(obj)
	lat,long,_ = astrometric.ecliptic_latlon(epoch=t)
	return [long.degrees, lat.degrees]
"];


normalStarPosition = ExternalFunction[pythonSession,"
def star_position(hip,day):
	t = ts.tdb(jd=day)
	obj = Star.from_dataframe(df.loc[hip])
	astrometric = earth.at(t).observe(obj)
	lat,long,_ = astrometric.ecliptic_latlon(epoch=t)
	return [long.degrees, lat.degrees]
"];


normalStarHIPNumbers = EntityValue[GetNormalStars[],"HipparcosNumber","EntityAssociation"];


synodicObjectNames = <|
	Entity["PlanetaryMoon","Moon"]->"moon",
	Entity["Star","Sun"]->"sun",
	Entity["Planet","Mercury"]->"mercury",
	Entity["Planet","Venus"]->"venus",
	Entity["Planet","Mars"]->"mars",
	Entity["Planet","Jupiter"]->"jupiter barycenter",
	Entity["Planet","Saturn"]->"saturn barycenter"
|>;


AstronomicalPosition::invalidObject = "`` is not a valid normal star or synodic object.";


AstronomicalPosition[obj_, jd_?NumberQ] :=
	Which[
		KeyExistsQ[synodicObjectNames, obj],synodicPosition[synodicObjectNames[obj],jd],
		KeyExistsQ[normalStarHIPNumbers, obj],normalStarPosition[normalStarHIPNumbers[obj],jd],
		True,Message[AstronomicalPosition::invalidObject, obj];$Failed
	]
AstronomicalPosition[obj_, date_?DateObjectQ] := AstronomicalPosition[obj,JulianDate[date]]
AstronomicalPosition[obj_, date_DiaryDate] := AstronomicalPosition[obj,date["JulianDate"]]
AstronomicalPosition[obj_, date_?MissingQ] := date
AstronomicalPosition[obj_?MissingQ, date_] := obj


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
