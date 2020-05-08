(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["GlobalSettings`"];

q::usage="position of figure";
w::usage="position of figure";
e::usage="position of figure";


r::usage="uplift on average price for firm";



NR::usage="Number of Retailers";
NP::usage="Number of Producers";

aAdaptable::usage="name representation for the case where an adaptable \!\(\*
StyleBox[\"a\",\nFontSlant->\"Italic\"]\) is used (1)";
aFixed::usage="name representation for the case where a fixed \!\(\*
StyleBox[\"a\",\nFontSlant->\"Italic\"]\) is used (1)";
unweighted::usage="name representation for the case where the mean is not weighted by the volume (0)";
weighted::usage="name representation for the case where the mean is weighted by the volume (1)";
showplane::usage="name representation for the case where a plane is shown in 3D graph (1)";
notshowplane::usage="name representation for the case where a plane is not shown in 3D graph (0)";
color::usage="name representation for the case where a plane is colored in a 3D graph (1)";
notcolor::usage="name representation for the case where a plane is not colored in a 3D graph (0)";
normal::usage="name representation for the case when a normal distribution is used (1)";
lowResolution::usage="name representation for the case when a reduced data set (1/100: 1021 data points) is created with a draw number of 50 000.";
highResolution::usage="name representation for the case when the full data set is created with a draw number of 1 000 000.";
showcounting::usage="leave set to 1 to have the progression of the calculation is printed to the console";
showcounting2::usage="set to 1 to have a detailed progression of the calculation printed to the console";
LengthFunc::usage="function calculates the number of different levels to show in the 2D graphs";
weightmethod::usage="indicates that the weight method used, unweighted (0) or weighted (1)";


Begin["`Private`"];
q=4;
w=1;
e=1;


lowResolution=0;
highResolution=1;
r=1.2;
NR=20. ;
NP=20. ;

aAdaptable=0;
 aFixed=1;
weighted=1;
 unweighted=0;
showplane=1;
notshowplane=0;
color=1;
notcolor=0;
normal=1;
showcounting2=0;
showcounting=1;
(*showcounting2 = 0;*)
weightmethod= unweighted;
LengthFunc[start_,end_,step_]:=1+IntegerPart[(end-start)/step];
End[];
EndPackage[];
