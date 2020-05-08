(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["CreateGraphH34`", {"CoreComputations`", "GlobalSettings`"}];
CreateGraphH34::usage="CreateGraphH34[]";

It::usage="Iterator function that loops over the values of c, standard deviation of demand and mean demand";
amethod::usage="Shows the method of a for the current iteration. 0->Adaptable (aAdaptable), 1->Fixed (aFixed).";
printMessages::usage="prints informative messages during calculation";
Pr2DdropSD::usage="represents the data in a format for graphing the forward premium against the mean demand for levels of standard deviation";
doallprints3D::usage="handles 3D graph printing";
Pr2DdropED::usage="represents the data in a format for graphing the forward premium against the standard deviationfor levels of  mean demand ";
PrNumbers::usage="prints numbers for the experiment";
printGraph::usage="notused";
printGraphca::usage="notused";
printGraphca2::usage="notused";
printGraphc::usage="notused";
formatOutput::usage="Function that Formats the output from the CoreComputations into vectors that can be used to create graphs";
InitializeOutput::usage="Creates and initialize the vectors  that are needed  to create graphs";
startc::usage="start value for the cost convexity parameter (ccp)";
endc::usage="end value for the cost convexity parameter (ccp)";
stepc::usage="step value for the cost convexity parameter (ccp)";
ccp::usage="value for the cost convexity parameter (ccp)";
ndrawStandard::usage="number of random draws";
ndrawHigh::usage="number of random draws";
ndraw::usage="number of random draws";


Begin["`Private`"];
CreateGraphH34[hypothesesSet_, resolution_]:=( 
absListED={};
mydata2={};
amethod=aFixed;
version=0;
ndrawStandard=1000000;
ndrawHigh=1 ndrawStandard;
ndraw=ndrawStandard;

Speak["START!"];
q=4;w=1;e=1;
Print["start"];


Do[
{
SetParameters[hypothesis, resolution];
Do[
{

Do[
{
InitializeOutput;
time=DateString[AbsoluteTime[]];
It[startED,endED,stepED,
startSD,endSD,stepSD,
ccp,
amethod,weightmethod,
normal,NP,NR,ndraw];
edLength= LengthFunc[startED,endED, stepED];
sdLength=LengthFunc[startSD,endSD, stepSD];

time=AbsoluteTime[];
printMessages[amethod,weightmethod];
gr={Thickness[.4],Line[{{0,0},{1,0}}]};
Print[""];
Print[Style[ "Hypothesis "<>ToString[
If[hypothesis==9,"4 (in the Appendix)",hypothesis]],Bold, Larger]];

Which[hypothesis==3,
{
Print["Figure ", If[amethod==0,"1","2"],": Simulation addressing the hypotheses with ", If[amethod==0,"an adaptive","a fixed"] ," variable costs parameter"];
EDForwPremDrop=Map[ "ED=" ,Union[Drop[ForwPremAbs2DV,None,-2][[All,1]]]];
ForwPremAbsDropED=Transpose[Partition[Partition[Flatten[Map[Reverse, Drop[ForwPremAbs2DV,None,{1}]],2],2],edLength]];
ForwPremRelDropED=Transpose[Partition[Partition[Flatten[Map[Reverse, Drop[ForwPremRel2DV,None,{1}]],2],2],edLength]];
Print["The forward premium as a function of the mean demand (for different levels of the standard deviation of demand), c=",ccp," a=",a, " amethod=",amethod];
Print[Pr2DdropED[ForwPremAbsDropED,EDForwPremDrop]];
},
hypothesis ==4,
{
Print["Figure ", If[amethod==0,"1","2"],": Simulation addressing the hypotheses with ", If[amethod==0,"an adaptive","a fixed"] ," variable costs parameter"];
SDForwPremDrop=Map[ "SD=" ,Union[Drop[ForwPremAbs2DV,None,2][[All,1]]]];
ForwPremAbsDropSD= Partition[Drop[ForwPremAbs2DV,None,{3}],edLength];
ForwPremRelDropSD= Partition[Drop[ForwPremRel2DV,None,{3}],edLength];
Print[ "The forward premium as a function of the standard deviation of demand (for different levels of the mean demand), c=",ccp," a=",a, " amethod=",amethod];Print[Pr2DdropSD[ForwPremAbsDropSD,SDForwPremDrop]];
}
,
hypothesis ==1,
{
Print[ "c=",ccp," a=",a, " amethod=",amethod, " weightmethod=",weightmethod," ndraw=",ndraw];
formatOutput;
filename= "Amethod_"<>ToString[amethod]<>" Weightmethod_"<>ToString[weightmethod]<>" c_"<>ToString[ccp]<>" Version_"<> ToString[version]<>"_" <>ToString[Round[AbsoluteTime[]]]<> ".csv";
Print["Filename:", Style[filename, {Bold, Italic, Red}]];
Export[filename,mydata2];
Print[DateString[AbsoluteTime[]]];


},
hypothesis ==9,
{
If[ccp==4,{
Print[ "c=",ccp," a=",a, " amethod=",amethod, " weightmethod=",weightmethod];
doallprints3D[amethod,4,weightmethod]};,
{
Print["No 3D-graph needed for c=",ccp, "time:",DateString[AbsoluteTime[]]]
};]

}


];
Print["total time:",Round[AbsoluteTime[]-time]]

},{ccp,startc,endc,stepc}];

},{amethod,{aFixed}}];

},{hypothesis,hypothesesSet}];
)


It[startED_,endED_,stepED_,
startSD_,endSD_,stepSD_,
ccp_,
amethod_,weightmethod_,
distr_,NPf_,NRf_,
ndraw_]:=
( 
edLength= LengthFunc[startED,endED, stepED];
sdLength=LengthFunc[startSD,endSD, stepSD];
Print["startED:",startED,"-",endED,"(",stepED,") ","startSD:",startSD,"-",endSD,"(",stepSD,") ","ccp:",ccp," ndraw=",ndraw];
Print["a_method:",amethod," weightmethod:",weightmethod, " distribution:",normal," NP/NR:",NP,"/",NR];

(*h=ccp;*)
RA=0.8/2^ccp ; (* Rrisk aversion parameter *)
For[j=startSD,j<=endSD,j=j+stepSD,
{
For[i=startED,i<=endED,i=i+stepED,
{
time=AbsoluteTime[];
startTime=DateString[AbsoluteTime[]];
resultvector=TheoryOutcome2[i,j,ccp,weightmethod, distr,amethod,NPf,NRf,RA, ndraw];
storeVars[resultvector]
}
]
}
]
;
)



storeVars[resultvector_]:=( 
AppendTo[amethodV,{resultvector[[1,1]]}];
AppendTo[aV,{resultvector[[1,2]]}];
AppendTo[flexV,{resultvector[[1,3]]}];
AppendTo[weightedV,{resultvector[[1,4]]}];
AppendTo[RAV,{resultvector[[1,5]]}];
AppendTo[NPV,{resultvector[[1,6]]}];
AppendTo[PRV,{resultvector[[1,7]]}];
AppendTo[cV,{resultvector[[1,8]]}];
AppendTo[EDV,{resultvector[[1,9]]}];
AppendTo[sdV,{resultvector[[1,10]]}];

AppendTo[EPV,{resultvector[[1,11]]}];
AppendTo[priceForwardV,{resultvector[[1,12]]}];
AppendTo[EDApproxV,{resultvector[[1,13]]}];
AppendTo[sdApproxV,{resultvector[[1,14]]}];
AppendTo[sdPriceV,{resultvector[[1,15]]}];
AppendTo[VarPriceV,{(resultvector[[1,15]])^2}];
AppendTo[EPxV,{resultvector[[1,16]]}];
AppendTo[costExpectedV,{resultvector[[1,17]]}];
AppendTo[varReProfitV,{resultvector[[1,18]]}];
AppendTo[sdReProfitV,{resultvector[[1,19]]}];

AppendTo[covPxP1V,{resultvector[[1,20]]}];
AppendTo[covPx1P1V,{resultvector[[1,21]]}];
AppendTo[SkewV,{resultvector[[1,22]]}];
AppendTo[SkewBLV,{resultvector[[1,23]]}];
AppendTo[expectedProfitTotalV,{resultvector[[1,24]]}];
AppendTo[sdPrProfitV,{resultvector[[1,25]]}];
AppendTo[expectedReProfitV,{resultvector[[1,26]]}];
AppendTo[expectedReProfitExPoV,{resultvector[[1,27]]}];
AppendTo[sdReProfitExPoV,{resultvector[[1,28]]}];
AppendTo[profitmarginExPoV,{resultvector[[1,29]]}];
AppendTo[vExPoV,{resultvector[[1,30]]}];

AppendTo[optForwPositionREV,{resultvector[[1,31]]}];
AppendTo[optForwPositionPRV,{resultvector[[1,32]]}];
AppendTo[optForwPositionPR2V,{resultvector[[1,33]]}];
AppendTo[VarDemandV,{resultvector[[1,34]]}];
AppendTo[profitmarginV,{resultvector[[1,35]]}];
(* AppendTo[expectedProfitTotalV,{resultvector[[1,37]]}];*)
AppendTo[sdProfitTotalV,{resultvector[[1,36]]}];
AppendTo[NRV,{resultvector[[1,37]]}];
AppendTo[ndrawV,{resultvector[[1,38]]}];
AppendTo[expectedPrProfitV,{resultvector[[1,39]]}];

AppendTo[startTimeV,{startTime}];
(* used in 2D_ PrintGraphs_ DRY_ 2017.04.13c.nb *)
AppendTo[ForwPremAbs2DV,{resultvector[[1,9]],(resultvector[[1,12]]-resultvector[[1,11]]),{resultvector[[1,10]]}}];
AppendTo[ForwPremRel2DV,{resultvector[[1,9]], 100 (resultvector[[1,12]]-resultvector[[1,11]])/resultvector[[1,11]],resultvector[[1,10]]}];
AppendTo[counterV,{resultvector[[1,9]],resultvector[[1,10]]}];
AppendTo[countercV,{resultvector[[1,10]],resultvector[[1,8]]}];
AppendTo[counterEDV,{resultvector[[1,9]]}];
AppendTo[counterSDV,{resultvector[[1,10]]}];
AppendTo[textPriceForwardV,{{"ED:",resultvector[[1,9]],"sd:",resultvector[[1,10]]},resultvector[[1,12]]} ];
AppendTo[textForwardPremiumRelativeV,{{"ED:",resultvector[[1,9]],"sd:",resultvector[[1,10]]}, (resultvector[[1,12]]-resultvector[[1,11]])/resultvector[[1,11]] }];
AppendTo[textSkewBLV,{{"ED:",resultvector[[1,9]],"sd:",resultvector[[1,10]]},resultvector[[1,24]]}];
AppendTo[textMeanPriceV,{{"ED:",resultvector[[1,9]],"sd:",resultvector[[1,10]]},resultvector[[1,11]]}];
AppendTo[textSdPriceV,{{"ED:",resultvector[[1,9]],"sd:",resultvector[[1,10]]},resultvector[[1,15]]}];
AppendTo[textVarPriceV,{{"ED:",resultvector[[1,9]],"sd:",resultvector[[1,10]]},resultvector[[1,16]]}];
)




Pr2DdropED[pfDrop_,EDpfDrop_]:=( time=AbsoluteTime[];
Print[DateString[time]];
Print["from CC! ccp=",ccp," weighted=",weightmethod," a=",a, "amethod=",amethod];
grfFR=ListPlot[pfDrop,Joined->True,InterpolationOrder->3, 
PlotLegends->Placed[SwatchLegend[EDpfDrop,LegendMarkers->Graphics[gr],LegendMarkerSize->10],Above],
PlotStyle->{Thickness[0.015]},DataRange->{{startSD,endSD},Automatic},Ticks->{Range[0,endSD,10],Automatic},FrameTicks->{{Range[startSD,endSD,stepSD],None},Automatic}, TicksStyle->Directive[FontSize->20],PlotRangePadding->None, AxesOrigin->{startSD,Automatic}];
grfFRl=ListLinePlot[{{100,-600},{100,600}},PlotStyle->{Black, Thickness[0.015],Dashed}];
(*Show[grfFR,grfFRl]*)
Show[grfFR]
)


Pr2DdropSD[pfDrop_,SDpfDrop_]:=( time=AbsoluteTime[];
Print[DateString[time]];
Print["ccp=",ccp," weighted=",weightmethod," a=",a, "amethod=",amethod];
grfFR=ListPlot[pfDrop,Joined->True,InterpolationOrder->3, 
PlotLegends->Placed[SwatchLegend[SDpfDrop,LegendMarkers->Graphics[gr],LegendMarkerSize->10],Above],
PlotStyle->{Thickness[0.015]},DataRange->{{startED,endED},{-100,100}},Ticks->{Range[startED,endED,25],Automatic},FrameTicks->{{Range[startED,endED,stepED],None},Automatic}, TicksStyle->Directive[FontSize->20],PlotRangePadding->None, AxesOrigin->{startED,Automatic}];
grfFRl=ListLinePlot[{{100,-600},{100,600}},PlotStyle->{Black, Thickness[0.015],Dashed}];
(*Show[grfFR,grfFRl]*)
Show[grfFR]
)


Pr2DdropSD[pfDrop_,SDpfDrop_,ylowPlotRange_,yhighPlotRange_]:=( time=AbsoluteTime[];
Print[DateString[time]];
grfFR=ListPlot[pfDrop,Joined->True,InterpolationOrder->3, 
PlotLegends->Placed[SwatchLegend[SDpfDrop,LegendMarkers->Graphics[gr],LegendMarkerSize->10],Above],PlotRange->{Automatic,{ylowPlotRange,yhighPlotRange}},
PlotStyle->{Thickness[0.015]},DataRange->{{startED,endED},{-100,100}},Ticks->{Range[startED,endED,25],Automatic},FrameTicks->{{Range[startED,endED,stepED],None},Automatic}, TicksStyle->Directive[FontSize->20],PlotRangePadding->None, AxesOrigin->{startED,Automatic}];
grfFRl=ListLinePlot[{{100,-600},{100,600}},PlotStyle->{Black, Thickness[0.015],Dashed}];
(*Show[grfFR,grfFRl]*)
Show[grfFR]
)


(*Remove[doallprints,doallprints3D];*)
doallprints3D[amethod_,hypo_,weightmethod_]:=( 
(* The Print graph command for Figure 1a and Figure 2a, 2c, 2e (the relative forward premium as %) *)
If[hypo==3
,{
Print["H3"];
If[weightmethod==unweighted,
{
Print["UNWEIGHTED"];
Print["Figure 1a (the \!\(\*
StyleBox[\"relative\",\nFontWeight->\"Plain\",\nFontSlant->\"Italic\"]\) forward premium)"];
Print["best for H3-unweighted-relative"];
gFLEXR=printGraphca2[2,endc, 1,endSD, -50,80, 100 (priceForwardV-EPV)/EPV ,showplane,color];
Print[gFLEXR];

Print["Figure 1b (the forward premium)"];
Print["best for H3-unweighted-absolute"];
gFLEXR=printGraphca2[2,endc,     1,endSD,-15,45,priceForwardV-EPV ,showplane,color];
Print[gFLEXR];

(*Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraphca[2,endc,      1,endSD,-25,25,profitmarginV,showplane ,color];
Print[gFLEXR];

Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraphca[2,endc,      1,endSD,-25,25,profitmarginV,showplane ,notcolor];
Print[gFLEXR];

Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraphca[2,endc,      1,endSD,-40,40,profitmarginExPoV,showplane ,color];
Print[gFLEXR];*)




},
{
Print["WEIGHTED"];
Print["Figure 1a (the \!\(\*
StyleBox[\"relative\",\nFontWeight->\"Plain\",\nFontSlant->\"Italic\"]\) forward premium)"];
Print["best for H3-weighted-relative"];
gFLEXR=printGraphca2[2,endc, 1,endSD, -80,10, 100 (priceForwardV-EPV)/EPV ,showplane,color];
Print[gFLEXR];

Print["Figure 1b (the forward premium)"];
Print["best for H3-weighted-absolute"];
gFLEXR=printGraphca2[2,endc,     1,endSD,-40,10,priceForwardV-EPV ,showplane,color];
Print[gFLEXR];

(*Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraphca[2,endc,      1,endSD,-25,25,profitmarginV,showplane ,notcolor];
Print[gFLEXR];*)



(*Print["best for H3-weighted-scaledup relative"];
gFLEXR=printGraphca[2,endc, 1,endSD,-4000,10000,  100 (priceForwardV-EPV)/EPV ,notshowplane,color];
Print[gFLEXR];
Print["best for H3-weighted-scaledup-absolute"];
gFLEXR=printGraphca[2,endc,     1,endSD,-5000,10000,priceForwardV-EPV ,showplane,color];
Print[gFLEXR];*)

Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraphca2[2,endc,      1,endSD,0,100,profitmarginExPoV,showplane ,color];
Print[gFLEXR];

Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraphca2[2,endc,      1,endSD,-1000,3500,profitmarginExPoV,showplane ,color];
Print[gFLEXR];


}
];


(*q=3.4922445740185832`;w=1.9231636431320918`;e=1.4510924977315034`;
Print["Absolute FP weighted - different angle"];
gFLEXR=printGraphc2[2,endc,         1,endSD,PFV-EPV ,notshowplane,color]
gFLEXR=printGraph[startED,endED,         startSD,endSD,PFV-EPV ,notshowplane,color]*)
q=4;w=1;e=1;
(*gFLEXR=printGraphc[75,125,         1,40,profitmarginV ,0]*)
(*gFLEXR=printGraphP[75,125,         1,40, 100 (PFV-EPV)/EPV ]*)
(*Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraphca[2,endc,      1,endSD,-25,25,profitmarginV,showplane ,notcolor];
Print[gFLEXR];


Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraphca[2,endc,      1,endSD,0,100,profitmarginExPoV,showplane ,color];
Print[gFLEXR];
gFLEXR=printGraphca[2,endc,      1,endSD,profitmarginExPoV,showplane ,color];
Print[gFLEXR];*)}

];

If[hypo==4
,
{
Print["Figure A1: Simulations addressing Hypothesis 4 with 3-D plots"];
(*Print["amethod:",amethod];
Print["aAdaptable:",aAdaptable];*)
If[amethod==aAdaptable
,{
If[weightmethod==unweighted,
{
(*Print["UNWEIGHTED"];*)
Print[""];
Print["Figure A1.a) (Adaptive variable cost parameter, the \!\(\*
StyleBox[\"relative\",\nFontWeight->\"Plain\",\nFontSlant->\"Italic\"]\) forward premium)"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-10,60,100 (priceForwardV-EPV)/EPV,showplane,color]; Print[gFLEXR];
Print[""];
Print["Figure A1.b) (Adaptive variable cost parameter, the forward premium)"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-5,50, (priceForwardV-EPV),showplane,color];Print[gFLEXR]
(*Print[gFLEXR];*)
(*Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-25,25,profitmarginV,showplane ,color];
Print[gFLEXR];
Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-25,25,profitmarginExPoV,showplane ,color];
Print[gFLEXR];*)
},
(*If[weightmethod\[Equal]weighted,*)
{
(*Print["WEIGHTED"];*)
(*Print["Figure 2c (constant supply, the relative forward premium)"];*)
Print[""];
Print["Figure 2c (constant supply, the relative forward premium)"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-80,5,100 (priceForwardV-EPV)/EPV,showplane,color];
Print[gFLEXR];
Print[""];
Print["Figure 2d (constant supply, the forward premium)"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-40,5, (priceForwardV-EPV),showplane,color];
(*Print[gFLEXR]*)
(*Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-25,25,profitmarginV,showplane ,notcolor];
Print[gFLEXR];
Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-5,90,profitmarginExPoV,showplane ,color];
Print[gFLEXR];
*)
(*Print["SCALED-UP"];
Print["relative FP"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-1500,2500,100 (priceForwardV-EPV)/EPV,showplane,color]; Print[gFLEXR];
Print["Absolute FP"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-5000,5000, (priceForwardV-EPV),showplane,color];Print[gFLEXR];
Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-25,25,profitmarginV,showplane ,notcolor];
Print[gFLEXR];
Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR=printGraph[startED,endED,startSD,endSD,-5,2000,profitmarginExPoV,showplane ,color];
Print[gFLEXR];*)
}
];
}
,{
If[weightmethod==unweighted,
{
(*Print["UNWEIGHTED"];*)
(*Print["relative FP"];*)
Print[""];
Print["Figure A1.c) (Constant variable cost parameter, the \!\(\*
StyleBox[\"relative\",\nFontWeight->\"Plain\",\nFontSlant->\"Italic\"]\) forward premium)"];
gFLEXR1=printGraph[startED,endED,startSD,endSD,-10,39,100 (priceForwardV-EPV)/EPV,showplane,color]; Print[gFLEXR1];
Print[""];
Print["Figure A1.d) (Constant variable cost parameter, the forward premium)"];
gFLEXR2=printGraph[startED,endED,startSD,endSD,-10,20, (priceForwardV-EPV),showplane,color];Print[gFLEXR2]
(*Print["relative exp ex ante Profit as percentage of exp cost"];
gFLEXR3=printGraph[startED,endED,startSD,endSD,-25,25,profitmarginV,showplane ,color];
Print[gFLEXR3];
Print["relative exp ex post Profit as percentage of exp cost"];
gFLEXR4=printGraph[startED,endED,startSD,endSD,-70,25,profitmarginExPoV,showplane ,color];
Print[gFLEXR4];*)
},
(*If[weightmethod\[Equal]weighted,*)
{
Print["WEIGHTED"];
Print["relative FP"];
gFLEXR5=printGraph[startED,endED,startSD,endSD,-80,5,100 (priceForwardV-EPV)/EPV,showplane,color]; Print[gFLEXR5];
Print["Absolute FP"];
gFLEXR6=printGraph[startED,endED,startSD,endSD,-60,5, (priceForwardV-EPV),showplane,color];Print[gFLEXR6];


}
];
}
]
}

];


Print[DateString[AbsoluteTime[]]];
Print["total time:",Round[AbsoluteTime[]-time]];
)

printMessages[amethod,weightmethod]:=( 
Print["--------------------------------------------------------------------------------"];
If[amethod== aAdaptable,Print["Flex a"];,Print["Fix a"];];
If[weightmethod== unweighted,Print["unweighted"];,Print["weighted"];];
Print["ccp=",ccp];

Speak[ccp];)




PrNumbers:=( 
Print["ForwardPremiumRelativeV",textForwardPremiumRelativeV//MatrixForm];
Print["PriceForward",textPriceForwardV//MatrixForm];
Print["MeanPrice",textMeanPriceV//MatrixForm];
Print["SdPrice",textSdPriceV//MatrixForm];
Print["VarPrice",textVarPriceV//MatrixForm];
Print["Skew",textSkewV//MatrixForm];
Print["profitmarginV",profitmarginV//MatrixForm];
Print["expectedProfitRetailerV",expectedProfitRetailerV//MatrixForm];
Print["costExpectedV",costExpectedV//MatrixForm];
Print["PR - retail price",PRV//MatrixForm];
Print["PRoverMeanPriceV",PRoverMeanPriceV//MatrixForm];

(*Print["profitTotalV",profitTotalV//MatrixForm];
Print["costTotalV",costTotalV//MatrixForm];*)

)



Pr2Drest[pfDrop_,edLength_]:=( time=AbsoluteTime[];
Print[DateString[time]];
grfFR=ListPlot[Partition[pfDrop,edLength],Joined->True,InterpolationOrder->3, 
PlotLegends->Placed[SwatchLegend[SDpfDrop,LegendMarkers->Graphics[gr],LegendMarkerSize->10],Above],
PlotStyle->{Thickness[0.015]},DataRange->{{50,150},{-100,100}},Ticks->{Range[50,150,25],Automatic},FrameTicks->{{Range[50,150,25],None},Automatic}, TicksStyle->Directive[FontSize->20],PlotRangePadding->None,PlotRange->{{50,150},{-100,100}}, AxesOrigin->{50,Automatic}];
grfFRl=ListLinePlot[{{100,-100},{100,100}},PlotStyle->{Black, Thickness[0.015],Dashed}];
Show[grfFR,grfFRl]
)





printGraph2[startED_,endED_,startSD_,endSD_,DimV_,showplane_,color_]:=( time=AbsoluteTime[];
Print[DateString[time]];
aqq=ListPlot3D[Join[counterV,DimV,2],ColorFunction->ColorData["TemperatureMap"],FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},Automatic},AxesEdge->{{1,-1},{1,-1},{1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{q,w,e}];
awq=ListPlot3D[Join[counterV,DimV,2],PlotStyle->{Opacity[0.75],Blue},FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},Automatic},AxesEdge->{{1,-1},{1,-1},{1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{q,w,e}];

 (* q,w,e define the viewpoint from which the graph will be viewed *)
If[showplane==1,
If[color==1,
Show[aqq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ],
Show[awq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ] ],
If[color==1,
Show[aqq] ,
Show[awq]  ] ]
)


printGraph[startED_,endED_,startSD_,endSD_,startz_,endz_,DimV_,showplane_,color_]:=( time=AbsoluteTime[];
Print[DateString[time]];
aqq=ListPlot3D[Join[counterV,DimV,2],ColorFunction->ColorData["TemperatureMap"],FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},{startz,endz}},AxesEdge->{{1,-1},{1,-1},{1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{q,w,e}];
awq=ListPlot3D[Join[counterV,DimV,2],PlotStyle->{Opacity[0.75],Blue},FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},{startz,endz}},AxesEdge->{{1,-1},{1,-1},{1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{q,w,e}];

 (* q,w,e define the viewpoint from which the graph will be viewed *)
If[showplane==1,
If[color==1,
Show[aqq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ],
Show[awq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ] ],
If[color==1,
Show[aqq] ,
Show[awq]  ] ]
);


printGraphca[startSD_,endSD_,startED_,endED_,DimV_,showplane_,color_]:=( time=AbsoluteTime[];
Print[DateString[time]];
(* does not contain startz_,endz_, *)
aqq=ListPlot3D[Join[countercV,DimV,2],ColorFunction->ColorData["TemperatureMap"],FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},Automatic},AxesEdge->{{-1,-1},{1,-1},{-1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{w,-q,e}];
awq=ListPlot3D[Join[countercV,DimV,2],PlotStyle->{Opacity[0.75],Blue},FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},Automatic},AxesEdge->{{-1,-1},{1,-1},{-1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{w,-q,e}];


 (* q,w,e define the viewpoint from which the graph will be viewed *)
If[showplane==1,
If[color==1,
Show[aqq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ],
Show[awq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ] ],
If[color==1,
Show[aqq] ,
Show[awq]  ] ]
)

printGraphca2[startSD_,endSD_,startED_,endED_,startz_,endz_,DimV_,showplane_,color_]:=( time=AbsoluteTime[];
Print[DateString[time]];

aqq=ListPlot3D[Join[countercV,DimV,2],ColorFunction->ColorData["TemperatureMap"],FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},{startz,endz}},AxesEdge->{{-1,-1},{1,-1},{-1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{w,-q,e}];
awq=ListPlot3D[Join[countercV,DimV,2],PlotStyle->{Opacity[0.75],Blue},FaceGrids->
{{{0,1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{0,-1,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}},
{{-1,0,0},{{0,0,0,0},{-20,{0,{Thick,Dashed}},20,40}}}},FaceGridsStyle->Directive[{Thick,Black,Dotted}],Ticks->Automatic,InterpolationOrder->13,PlotRange->{{startED,endED},{startSD,endSD},{startz,endz}},AxesEdge->{{-1,-1},{1,-1},{-1,-1}},BoxRatios->{1,1,1},PlotLegends->Automatic,ViewPoint->{w,-q,e}];


 (* q,w,e define the viewpoint from which the graph will be viewed *)
If[showplane==1,
If[color==1,
Show[aqq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ],
Show[awq,Graphics3D[{Gray,Opacity[0.3],InfinitePlane[{{0,0,0},{1,1,0},{1,0,0} }  ] } ] ] ],
If[color==1,
Show[aqq] ,
Show[awq]  ] ]
);


formatOutput:=( 
OstartTimeV=startTimeV;PrependTo[OstartTimeV,{"startTime"}];
OndrawV=ndrawV;PrependTo[OndrawV,{"ndraw"}];
OtimeV=timeV;PrependTo[OtimeV,{"timeV"}];
OaV=aV;PrependTo[OaV,{"a"}];
ORAV=RAV;PrependTo[ORAV,{"RA"}];
OPriceForwardV=priceForwardV;PrependTo[OPriceForwardV,{"price_Forward"}];
OEPV=EPV;PrependTo[OEPV,{"price_Expected"}];
(*OEDV=EDV;PrependTo[OEDV,{"demand_Expected"}];*)

OsdV=sdV;PrependTo[OsdV,{"sd"}];
OsdApproxV=sdApproxV;PrependTo[OsdApproxV,{"sdApprox"}];

OVarDemandV=VarDemandV; PrependTo[OVarDemandV,{"varDemand"}];

OEDV=EDV; PrependTo[OEDV,{"EDV"}];
OEDApproxV=EDApproxV; PrependTo[OEDApproxV,{"EDApprox"}];



OSkewV=SkewV;PrependTo[OSkewV,{"skew"}];
OSkewBLV=SkewBLV;PrependTo[OSkewBLV,{"skew_Nonstandard_BL"}];
OVarPriceV=VarPriceV;PrependTo[OVarPriceV,{"VarPrice"}];
OcV=cV;PrependTo[OcV,{"c"}];
(*Ob1BLV=b1BLV;PrependTo[Ob1BLV,{"b1BL"}];
Ob2BLV=b2BLV;PrependTo[Ob2BLV,{"b2BL"}];*)
OprofitmarginV =profitmarginV; PrependTo[OprofitmarginV,{"profit_Margin"}];
OprofitmarginExPoV =profitmarginExPoV; PrependTo[OprofitmarginExPoV,{"profitmarginExPo"}];

OweightedV =weightedV; PrependTo[OweightedV,{"weighted"}];
(*OtextSkewV =textSkewV; PrependTo[OtextSkewV,{"textSkew"}];*)
OPRV=PRV; PrependTo[OPRV,{"retail_Rate"}];

OsdReProfitV = sdReProfitV;PrependTo[OsdReProfitV,{"sdProfit_RE"}];
(*OsdProfitRetailerV = sdProfitRetailerV;PrependTo[OsdProfitRetailerV,{"sd_Profit_RE"}];*)

OsdPrProfitV =sdPrProfitV;PrependTo[OsdPrProfitV,{"sdProfit_PR"}];
(*OsdProfitProducerV =sdProfitProducerV;PrependTo[OsdProfitProducerV,{"sd_Profit_PR"}];*)


OexpectedPrProfitV =expectedPrProfitV; PrependTo[OexpectedPrProfitV,{"expectedProfit_PR"}];
OexpectedReProfitV =expectedReProfitV; PrependTo[OexpectedReProfitV,{"expectedProfit_RE"}];
(*OexpectedProfitRetailerV =expectedProfitRetailerV; PrependTo[OexpectedProfitRetailerV,{"expectedProfit_RE"}];*)


OexpectedProfitTotalV =expectedProfitTotalV; PrependTo[OexpectedProfitTotalV,{"expectedProfit_Total"}];

OsdProfitTotalV =sdProfitTotalV; PrependTo[OsdProfitTotalV,{"sdProfit_Total"}];
OoptForwPositionREV = optForwPositionREV;PrependTo[OoptForwPositionREV,{"OoptForwPosition_RE"}];
OoptForwPositionPRV = optForwPositionPRV;PrependTo[OoptForwPositionPRV,{"OoptForwPosition_PR"}];
(*OcounterEDV =counterEDV;PrependTo[OcounterEDV,{"OcounterEDV"}];*)
(*OcounterSDV =counterSDV;PrependTo[OcounterSDV,{"OcounterSDV"}];*)
ONRV =NRV;PrependTo[ONRV,{"NR"}];
ONPV =NPV;PrependTo[ONPV,{"NP"}];
(*OvV =vV;PrependTo[OvV,{"v"}];*)


OexpectedReProfitExPoV =expectedReProfitExPoV;PrependTo[OexpectedReProfitExPoV,{"expectedReProfitExPo"}];

OvExPo =vExPoV;PrependTo[OvExPo,{"vExPo"}];
OsdReProfitExPoV=sdReProfitExPoV; PrependTo[OsdReProfitExPoV, {"sdReProfitExPo"}];


mydata2=Join[ONPV,ONRV,OEDV,OsdV,OEDApproxV,OsdApproxV,OweightedV,OPRV,OcV,OaV,ORAV,OndrawV,OPriceForwardV,OEPV,OVarDemandV,OSkewV,OSkewBLV,OVarPriceV,OprofitmarginV,OprofitmarginExPoV,OexpectedPrProfitV,OsdPrProfitV,OexpectedReProfitV,OsdReProfitV,OsdReProfitExPoV,OexpectedProfitTotalV,OsdProfitTotalV,OoptForwPositionREV,OoptForwPositionPRV,OstartTimeV,2];
)




InitializeOutput:=( 
amethodV={};aV={};flexV={};weightedV={};RAV={}; NPV={}; PRV={};cV={};EDV={};sdV={}; 
EPV={};priceForwardV={};EDApproxV={};sdApproxV={};sdPriceV={}; VarPriceV={};EPxV={};costExpectedV={};varReProfitV={};sdReProfitV={};
covPxP1V={}; covPx1P1V={}; SkewV={}; SkewBLV={}; expectedProfitTotalV={}; sdPrProfitV={}; expectedReProfitV={}; expectedProfitTotalV={}; expectedReProfitExPoV={}; sdReProfitExPoV={};
profitmarginExPoV={};vExPoV={};optForwPositionREV={};optForwPositionPRV={};optForwPositionPR2V={};VarDemandV={};profitmarginV={};sdProfitTotalV = {};NRV={};ndrawV={};
expectedPrProfitV={};

QFprodV={};timeFrameV={};
PWvarV={};covPxP1tay2V={};covPx1P1tay2V={};covPxP1tay3V={};covPx1P1tay3V={};covPxP1eV={};covPx1P1eV={};
PF3DV={};EPeV={};
PReV={};
counterV={};VarV={};VareV={};SkeweV={};sdDemandV={};treatmentV={};timeV={};
b1BLV={};b2BLV={};
counterV={};countercV={};counterEDV ={}; counterSDV ={};
PWskewV={};signcoV={};ProfV={};ForwPremAbs2DV={};ForwPremRel2DV={};
PF2DeV={};PFR2DeV={};
costTotalV={};
EDdataV={};
expectedProfitProducerV={};
Pdata1V={};
SkewEDV={};
profitTotalV={};
(*VarEDV={};*)
b1V={};
b2V={};

EP2DVe={};
SDP2DVe={};
expectedProfitRetailerV={};
PRoverMeanPriceV={};

ForwPremRel2DV={};
textPriceForwardV={};
textPriceForwardRelativeV={};
textMeanPriceV={};
textSdPriceV={};
textForwardPremiumRelativeV={};
textVarPriceV={};
textSkewBLV={};
mydata2={};
(*sdProfitRetailerV = {};
sdProfitProducerV = {};*)
(*expectedDemandV ={};*)
startTimeV={};
)

SetParameters[hypothesis_, resolution_]:=(
Which[
hypothesis==1,
{
If[resolution== highResolution,
{
	ndraw=1000000;
	startc=2.0;
	endc=5.0;
	stepc=1.0;
	startED =50.0;
	endED = 150.0;
	stepED = .2;
	startSD =1.0;
	endSD = 40.0;
	stepSD =.1;
	}
	,
		{
	ndraw=10000;
	startc=2.0;
	endc=5.0;
	stepc=1.0;
	startED =50.0;
	endED = 150.0;
	stepED = .2;
	startSD =1.0;
	endSD = 40.0;
	stepSD =.2;
	};
	];}
	,
hypothesis==3,
{
	startc=2.0;
	endc=5.0;
	stepc=1.0;
	startED =50.0;
	endED = 150.0;
	stepED = 25.0;
	startSD =1.0;
	endSD = 40.0;
	stepSD =3.0;},
hypothesis==4,
{
startc=2.0;
endc=5.0;
stepc=1.0;
startED =50.0;
endED = 150.0;
stepED = (100.0/10.0);
startSD :=5.0;
endSD = 35.0;
stepSD =10.0;
},
hypothesis==9,
{
startc=4.0;
endc=5.0;
stepc=10.0;
startED =75.0;
endED = 125.0;
stepED = (50.0/20.0);
startSD :=1.0;
endSD = 40.0;
stepSD =1.5;
}];
)

End[];
EndPackage[];
