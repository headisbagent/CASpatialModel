$title spatialModel_infrastructure

$offlisting
options
	limrow = 0,
	limcol = 0,
	solprint = off,
	sysout = off,
	profile = 3
	;

sets
	i 				Station Type Index
	r 				Region Index
	t 				Hourly Index
	v				Vehicle Index
	VtoR(v,r)		Vehicle to Region Mapping
	;

parameters
	demand(v)				Total energy demand by vehicle
	dailyCapacity(i)		Station capacity per day
	cost(i)					Cost to install station
	hourlyCapacity(i)		Station capacity per hour
	existingStations(i,r)	Existing stations
	maxStations(r)			Max number of stations in a region
	;

positive variable
	fueled(r,v,t)		Amount of energy provided to vehicle v in region r
*	station(i,r)		Number of stations of type i installed in region r
	;

integer variable
	station(i,r)	Number of stations of type i installed in region r
	;

variable
	totalCost
	;

$gdxin inputs
$load i r v t VtoR demand dailyCapacity hourlyCapacity cost existingStations maxStations
$gdxin

equations
	obj				Objective function
	constraint1 	Vehicle fueling must be greater than energy demand
	constraint2 	Daily fueling capacity must be greater than fuel dispensed
	constraint3		Hourly fueling capacity constraint
	constraint4		Maximum number of stations by region constraint
	;

obj..
	totalCost =e= sum((i,r),station(i,r)*cost(i))+sum((r,v,t),fueled(r,v,t)$demand(v));

*obj..
*	totalCost =e= sum((i,r),station(i,r)*cost(i))+sum((r,v),fueled(r,v));

constraint1(v)..
	sum(t,sum(r$VtoR(v,r),fueled(r,v,t)$demand(v)))-demand(v) =g= 0;

*constraint1(v)..
*	sum(r$VtoR(v,r),fueled(r,v))-demand(v) =g= 0;

constraint2(r)..
	sum(i,station(i,r)*dailyCapacity(i)+existingStations(i,r)*dailyCapacity(i))-sum((v,t),fueled(r,v,t)$demand(v)) =g= 0;

*constraint2(r)..
*	sum(i,station(i,r)*dailyCapacity(i))-sum(v,fueled(r,v)) =g= 0;

constraint3(r,t)..
	sum(v,fueled(r,v,t)$demand(v))-sum(i,station(i,r)*hourlyCapacity(i)+existingStations(i,r)*hourlyCapacity(i)) =l= 0;

constraint4(r)..
	maxStations(r)-sum(i,station(i,r)) =g= 0;

model
	spatialModel_infrastructure /obj,constraint1,constraint2,constraint3,constraint4/
	;


options
	mip = cplex
*	lp = cplex
	solvelink = 0
	reslim = 50000
	;

$onecho > cplex.opt
workmem 48000
aggind 10
names no
memoryemphasis 1
threads=16
solvefinal 0
nodesel 2
varsel 3
nodefileind 2
$offecho

spatialModel_infrastructure.optFile = 1;
spatialModel_infrastructure.holdfixed = 1;

solve
	spatialModel_infrastructure
	using mip
*	using lp
	minimizing totalCost
	;

display
	totalCost.l
	station.l
	fueled.l
	;

Execute_Unload "outputs.gdx"
