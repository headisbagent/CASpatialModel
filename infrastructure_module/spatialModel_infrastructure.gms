$title spatialModel_infrastructure

$offlisting
options
	limrow = 0,
	limcol = 0,
	solprint = off,
	sysout = off,
	profile = 1
	;

sets
	i 				Station Type Index
	r 				Region Index
	v				Vehicle Index
	VtoR(v,r)		Vehicle to Region Mapping
	;

parameters
	demand(v)		Total energy demand by vehicle
	capacity(i)		Station capacity per day
	cost(i)			Cost to install station
	;

integer variable
	station(i,r)	Number of stations of type i installed in region r
	;

positive variable
	fueled(r,v)		Amount of energy provided to vehicle v in region r
	;

variable
	totalCost
	;

$gdxin inputs
$load i r v VtoR demand capacity cost
$gdxin

equations
	obj				Objective function
	constraint1 	Vehicle fueling must be greater than energy demand
	constraint2 	Daily fueling capacity must be greater than fuel dispensed
	;

obj..
	totalCost =e= sum((i,r),station(i,r)*cost(i))+sum((r,v),fueled(r,v));

constraint1(v)..
	sum(r$VtoR(v,r),fueled(r,v))-demand(v) =g= 0;

constraint2(r)..
	sum(i,station(i,r)*capacity(i))-sum(v,fueled(r,v)) =g= 0;

model
	spatialModel_infrastructure /obj,constraint1,constraint2/
	;


options
	mip = cplex
	solvelink = 0
	reslim = 50000
	;

$onecho > cplex.opt
workmem 32000
aggind 10
names no
memoryemphasis 1
threads=4
$offecho

spatialModel_infrastructure.optFile = 1;
spatialModel_infrastructure.holdfixed = 1;

solve
	spatialModel_infrastructure
	using mip
	minimizing totalCost
	;

display
	totalCost.l
	station.l
	fueled.l
	;

Execute_Unload "outputs.gdx"
