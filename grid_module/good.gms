$title good

$offlisting
options
	limrow = 0,
	limcol = 0,
	solprint = off,
	sysout = off,
	profile = 3
	;

sets
	g				Generators
	gas(g) 			Gas generators
	hydro(g)		Hydro generators
	t 				Time Period (hour)
	d 				Time Period (day)
	r				Region
	ca(r)			CA regions
	gtor(g,r)		Generator to region mapping
	ttod(t,d)		Hour to day mapping
	;

alias(r,o,p);

parameters
	genCost(g)			Cost of generation [$ per MWh]
	demandLoad(r,t)		Baseload electricity demand [MWh]
	maxGen(g)			Capacity of dispatchable generation [MW]
	solarCap(r)			Capacity of solar generation [MW]
	windCap(r)			Capacity of wind generation [MW]
	solarCF(r,t)		Capacity factor of solar generation [unitless]
	windCF(r,t)			Capacity factor of wind generation [unitless]
	transCap(r,o)		Capacity of transmission line [MW]
	transCost(r,o)		Wheeling costs for transmission [$ per MW]
	h2Demand(r,d)		Hydrogen demand from transportation sector [kg]
	storExisting(r)		Amount of storage from previous time period [kg]
	pemExisting(r)		Amount of electrolyzer capacity from previous time period [MW]
	percentRenew(r)		Renewable Portfolio Standards by region [unitless]
	;

scalar
	transLoss 		Transmission efficiency [unitless] /.972/
	storageLossIn	PEM efficiency [kg per MW] /18.2/
	storageLossOut	H2 CT efficiency [MW per kg] /.025/
	solarCost 		Solar capacity cost [$ per MW] /80000/
	windCost 		Wind capacity cost [$ per MW] /160000/
	storCost		Storage capacity cost [$ per kg] /<<storCostValue>>/
	pemCost			PEM electrolyzer cost [$ per MW] /<<pemCostValue>>/
	waterCost 		Cost of water [$ per kg] /.009/
	importLimit		Transmission import limit [MWh] /80000000/
	;

variable
	systemCost
	;

positive variable
	generation(g,t) Generator operation [MW]
	trans(r,t,o)	Transmission operation (from region r to o) [MW]
	solarNew(r)		New solar capacity built [MW]
	windNew(r)		New wind capacity built [MW]
	storSOC(r,t)	Storage SOC [kg]
	storIn(r,t)		Amount into storage [MW]
	storOut(r,t)	Amount out of storage [kg]
	storCap(r)		Storage capacity [kg]
	pemCap(r)		Electrolyzer capacity [MW]
	fuelH2(r,t)		H2 for fueling [kg]
	;

$gdxin inputs
$load g gas hydro t d r ca gtor ttod demandLoad genCost maxGen solarCap windCap solarCF windCF transCap transCost h2Demand storExisting pemExisting percentRenew
$gdxin

*Variable limits
	generation.up(g,t) = maxGen(g);
	trans.up(r,t,o) = transCap(r,o);
	storCap.lo(r) = storExisting(r);
	pemCap.lo(r) = pemExisting(r);

equations
	obj							Objective function minimizing total cost
	genToDemand					Generation must equal load
	renewableReq				Annual renewable requirement
	storageSOC					Tracking storage levels
	hydrogenDemand				Hydrogen production must meet demand
	maxStorage					Storage capacity constraint
	storageFlowIn				Storage input limits
	storageFlowOut				Storage output limits
	transLimit					Import limit into CA
	;

obj..
systemCost =e= sum((g,t),generation(g,t)*genCost(g))+sum((r,t,o),trans(r,t,o)*transCost(r,o))+sum(r,solarCost*solarNew(r)+windCost*windNew(r)+(storCap(r)-storExisting(r))*storCost+(pemCap(r)-pemExisting(r))*pemCost)+sum((r,t),fuelH2(r,t)*waterCost);

genToDemand(t,r)..
	sum(g$gtor(g,r),generation(g,t))+(solarCap(r)+solarNew(r))*solarCF(r,t)+(windCap(r)+windNew(r))*windCF(r,t)+(sum(o,trans(o,t,r))*transLoss-sum(p,trans(r,t,p)))-storIn(r,t)+storageLossOut*storOut(r,t)-demandLoad(r,t)-fuelH2(r,t)/storageLossIn =g= 0;

renewableReq(r)..
	sum(t,(solarCap(r)+solarNew(r))*solarCF(r,t)+(windCap(r)+windNew(r))*windCF(r,t)+sum(hydro$gtor(hydro,r),generation(hydro,t))+storOut(r,t)*storageLossOut)*(1-percentRenew(r))-percentRenew(r)*sum(t,(sum(g$gtor(g,r),generation(g,t)))) =g= 0;

storageSOC(r,t)..
	storSOC(r,t)-storSOC(r,t-1)-storIn(r,t-1)*storageLossIn+storOut(r,t-1) =e= 0;

*hydrogenDemand(r,d)..
*	sum(t$ttod(t,d),fuelH2(r,t))-h2Demand(r,d) =g= 0;

hydrogenDemand(r)..
	sum(t,fuelH2(r,t))-sum(d,h2Demand(r,d)) =g= 0;

maxStorage(r,t)..
	storCap(r)-storSOC(r,t) =g= 0;

storageFlowIn(r,t)..
	pemCap(r)-storIn(r,t)-fuelH2(r,t)/storageLossIn =g= 0;

storageFlowOut(r,t)..
	(sum(gas$gtor(gas,r),maxGen(gas)-generation(gas,t)))-storOut(r,t)*storageLossOut =g= 0;

transLimit..
	importLimit-sum((r,t,ca),trans(r,t,ca)) =g= 0;

model
	good /obj,genToDemand,renewableReq,storageSOC,hydrogenDemand,maxStorage,storageFlowIn,storageFlowOut,transLimit/
	;

options
	lp = cplex
	solvelink = 0
	reslim = 999999
	;

$onecho > cplex.opt
workmem 48000
aggind 10
names no
memoryemphasis 1
threads=12
$offecho
good.optFile = 1;
good.holdfixed = 1;

solve
	good
	using lp
	minimizing systemCost
	;
	
Execute_Unload "outputs.gdx";