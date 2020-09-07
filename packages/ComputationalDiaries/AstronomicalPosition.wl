(* ::Package:: *)

BeginPackage["ComputationalDiaries`AstronomicalPosition`", {"ComputationalDiaries`"}];


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


normalStarHIPNumbers = EntityValue[DiaryNormalStar[],"HipparcosNumber","EntityAssociation"];


synodicObjectNames = <|
	Entity["PlanetaryMoon","Moon"]->"moon",
	Entity["Star","Sun"]->"sun",
	Entity["Planet","Mercury"]->"mercury",
	Entity["Planet","Venus"]->"venus",
	Entity["Planet","Mars"]->"mars",
	Entity["Planet","Jupiter"]->"jupiter barycenter",
	Entity["Planet","Saturn"]->"saturn barycenter"
|>;


DiaryAstronomicalPosition::invalidObject = "`` is not a valid normal star or synodic object.";


DiaryAstronomicalPosition[obj_, jd_?NumberQ] :=
	Which[
		KeyExistsQ[synodicObjectNames, obj],synodicPosition[synodicObjectNames[obj],jd],
		KeyExistsQ[normalStarHIPNumbers, obj],normalStarPosition[normalStarHIPNumbers[obj],jd],
		MissingQ[obj], obj,
		True,Message[DiaryAstronomicalPosition::invalidObject, obj];$Failed
	]
DiaryAstronomicalPosition[obj_, date_?DateObjectQ] := DiaryAstronomicalPosition[obj,JulianDate[date]]
DiaryAstronomicalPosition[obj_, date_DiaryCombinedDate] := DiaryAstronomicalPosition[obj,date["DateObject"]]
DiaryAstronomicalPosition[obj_, date_DiaryJulianDate] := DiaryAstronomicalPosition[obj,date["DateObject"]]
DiaryAstronomicalPosition[obj_, date_DiaryBabylonianDate] :=
	Enclose[DiaryAstronomicalPosition[obj,Confirm[DiaryJulianDate[date]]["DateObject"]],"Expression"]
DiaryAstronomicalPosition[obj_, date_?MissingQ] := date


(* ::Subsection:: *)
(*End*)


End[];


EndPackage[];
