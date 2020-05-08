(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["CoreComputations`", "GlobalSettings`"];
TheoryOutcome2::usage="Function that numerically approximates  the forward premium and other statistics for given demand and supply parameters, using a large sample";

a::usage="The value of the variable cost parameter";
c::usage="The value of the cost convexity parameter";
ED::usage="The value of the mean demand";
sd::usage="The value of the standard deviation of demand";




Begin["`Private`"];

TheoryOutcome2=Compile[{{ED, _Real},{sd, _Real},{c, _Real},{weighted, _Integer},  {distr, _Integer},{amethod, _Integer},{NP, _Integer},{NR, _Integer},{RA, _Real},{ndraw, _Integer}},
Block[{},{
If[amethod==aAdaptable,{a=30.0 (NP/ED)^(c-1.0)},{a=30.0 (NP/100.0)^(c-1)}];
If[showcounting2==1,Print["c:",c," ED:",ED,"sd:",sd,"- a:",a," - ",DateString[AbsoluteTime[]]]];
NN=(NP+NR)/RA;
x=1.0/(c-1.0);
F=0;
normal=1;
uniform=0;
r=1.2;
Pw[QD_]=a (QD/NP)^(c-1.0);
costFunction[QD_]=a /c (QD/NP)^c+F;
SeedRandom[1];
If[distr==uniform
,demandData=RandomVariate[TransformedDistribution[ QD,QD\[Distributed]UniformDistribution[{20,100}]],ndraw];
,demandData= RandomVariate[TransformedDistribution[ QD,QD\[Distributed]TruncatedDistribution[{0,\[Infinity]},NormalDistribution[ED,sd]]],ndraw];
];
(*priceData1=Map[Pw,demandData];*)
priceData1=Pw[demandData];
costData=priceData1 * demandData;
EDApprox=Mean[demandData];
sdApprox= StandardDeviation[demandData];
demandTotal =EDApprox ndraw;
costTotal= Total[costData];
costVariance=Variance[costData];
EP=Mean[priceData1];
Which[
weighted==0,PR=r EP;,
weighted==1,PR=r * (1/demandTotal)* costTotal; ,
weighted==2,PR=r *(1/demandTotal)* costTotal+(0.00000001  RA costVariance); ,
weighted==3,Print[( - bb +(bb^2 -4 aa cc)^.5)/ (2 aa)];PR=1.0 *Abs[( - bb +(bb^2 -4 aa cc)^.5)/ (2 aa)];,
weighted==4,Print["PR=",PR];
];
profitReData=(PR-priceData1) * (demandData/NR);
expectedReProfit =Mean[profitReData];
sdReProfit =StandardDeviation[profitReData];
varReProfit =sdReProfit^2.0;
costExpected = Mean[costData];
(*profitPrData=(costData/NP) - Map[costFunction,demandData];*)
profitPrData=(costData/NP) - costFunction[demandData];
profitTotalData = profitPrData + profitReData;
expectedProfitTotal = Mean[profitTotalData];
sdProfitTotal = StandardDeviation[profitTotalData];
expectedPrProfit =Mean[profitPrData];
sdPrProfit =StandardDeviation[profitPrData];
PRoverMeanPrice=PR/EP;
profitmargin=100  (NR expectedReProfit /costExpected);
datax=priceData1^x;

datax1=priceData1^(x+1.0);
EPx=Mean[datax];
Skew=Mean[{Skewness[priceData1]}];
VarPrice=Variance[priceData1];  
(*covPxP1=Mean[{Covariance[datax,priceData1]}];*)
covPxP1=Mean[{Covariance[datax,priceData1]}];
covPx1P1=Mean[{Covariance[datax1,priceData1]}];
SkewBL=Skew * (VarPrice^(3./2.));

VarDemand=sd^2.0;
sdPrice=VarPrice^.5;
v=expectedReProfit-RA (varReProfit);
	(*BL theoretical*)
priceForward=EP+NP/(NN c a^x) (-c PR  covPxP1+covPx1P1);

optForwPositionRE =  ((NP/( RA NN c a^x) (-c PR  covPxP1+covPx1P1) )+ Mean[{Covariance[(profitReData),priceData1]}])/VarPrice;
optForwPositionPR =((NP/(RA NN c a^x) (-c PR  covPxP1+covPx1P1)) +Mean[{Covariance[(profitPrData),priceData1]}])/VarPrice ;
optForwPositionPR2 =((NP/(RA NN c a^x) (-c PR  covPxP1+covPx1P1)) +(covPx1P1/a^x-covPx1P1/(c a^x) ))/VarPrice ;

profitReDataExPo = profitReData+optForwPositionRE (priceForward -EP);
expectedReProfitExPo =Mean[profitReDataExPo];
sdReProfitExPo =StandardDeviation[profitReDataExPo];
profitmarginExPo=100  (NR expectedReProfitExPo /costExpected);
vExPo=expectedReProfitExPo-RA (sdReProfitExPo^2);
(*{flex,weighted,optForwPositionRE,optForwPositionPR,optForwPositionPR2}*)
{amethod,a,a,weighted,RA, NP, PR,c,ED,sd,
 EP,priceForward,EDApprox,sdApprox,sdPrice, 
(*VarPrice,*)
EPx,costExpected,varReProfit,sdReProfit,
covPxP1,covPx1P1,Skew,SkewBL,expectedProfitTotal, sdPrProfit,expectedReProfit,expectedReProfitExPo,sdReProfitExPo, profitmarginExPo,

vExPo,optForwPositionRE,optForwPositionPR,optForwPositionPR2,VarDemand,profitmargin,sdProfitTotal,NR, ndraw,expectedPrProfit}

}]
,
RuntimeAttributes->{Listable},Parallelization->True,CompilationTarget->"C"
]

End[];
EndPackage[];
