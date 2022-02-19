## Loading input grid data ##
rps <- fread('inputs/inputs_rps.csv')
rps.all <- fread('inputs/inputs_rpsAll.csv')
rps.100pct.CA <- fread('inputs/inputs_rps100pct.csv')
load <- fread('inputs/inputs_load.csv')
generators.other <- fread('inputs/raw_generators_other.csv')
generators.solar.wind <- fread('inputs/raw_generators_solar_wind.csv')
generators.aggregated <- fread('inputs/raw_generators_aggregated.csv')
transmission <- fread('inputs/inputs_transmission.csv')
solar.cf <- fread('inputs/inputs_solarCF.csv')
wind.cf <- fread('inputs/inputs_windCF.csv')

prep.inputs.grid <- function(tInput,yr,scenario,rpsOption) {
	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	inputs$sets$g <- generators.aggregated$g
	inputs$sets$gas <- generators.aggregated[FuelType=='NaturalGas']$g
	inputs$sets$hydro <- generators.aggregated[FuelType=='Hydro']$g
	inputs$sets$r <- as.character(unique(generators.aggregated$RegionSimple))
	inputs$sets$ca <- c('CA_N','CA_LA','CA_SD')
	inputs$sets$gtor <- generators.aggregated[,list(g,RegionSimple)]
	names(inputs$sets$gtor) <- c('g','r')

	generators.Cost <- generators.aggregated[,list(g,FuelCostTotal)]
	names(generators.Cost) <- c('g','value')
	inputs$parameters$genCost <- generators.Cost

	transCap <- transmission[,list(r1,r2,transCap)]
	transCap <- transCap[,.(transCap=sum(transCap)),by=.(r1,r2)]
	names(transCap) <- c('r','o','value')
	transCost <- transmission[,list(r1,r2,transCost)]
	transCost <- transCost[,.(transCost=sum(transCost)),by=.(r1,r2)]
	names(transCost) <- c('r','o','value')

	inputs$parameters$transCap <- transCap
	inputs$parameters$transCost <- transCost

	## RPS ##
	if(rpsOption=='rpsBase') {
		percentRenew <- rps[year==yr,.(r,value)]
	} else if(rpsOption=='rpsAll') {
		percentRenew <- rps.all[year==yr,.(r,value)]
	} else if(rpsOption=='rps100') {
		percentRenew <- rps.100pct.CA[year==yr,.(r,value)]
	}
	inputs$parameters$percentRenew <- percentRenew

	## Generation Capacities ##
	generators.Cap <- generators.aggregated
	generators.Cap <- generators.Cap[,list(g,Capacity)]
	names(generators.Cap)[names(generators.Cap)=='Capacity'] <- 'value'
	inputs$parameters$maxGen <- generators.Cap

	## Renewables ##
	solar.Cap <- generators.solar.wind[FuelType=='Solar',.(Capacity=sum(Capacity)),by=RegionSimple]
	solar.Cap <- solar.Cap[,.(r=RegionSimple,value=Capacity)]
	if(yr>2025) {
		years <- seq(yr-5,2025,by=-5)
		for(year in years) {
			existingSolar <- data.table(gdx(paste0('runFiles/',year,'_',scenario,'_1-365/outputs.gdx'))['solarNew'])
			solar.Cap <- rbind(solar.Cap,existingSolar)
		}
		solar.Cap <- solar.Cap[,.(value=sum(value)),by=r]
	}
	inputs$parameters$solarCap <- solar.Cap

	wind.Cap <- generators.solar.wind[FuelType=='Wind',.(Capacity=sum(Capacity)),by=RegionSimple]
	wind.Cap <- wind.Cap[,.(r=RegionSimple,value=Capacity)]
	if(yr>2025) {
		years <- seq(yr-5,2025,by=-5)
		for(year in years) {
			existingWind <- data.table(gdx(paste0('runFiles/',year,'_',scenario,'_1-365/outputs.gdx'))['windNew'])
			wind.Cap <- rbind(wind.Cap,existingWind)
		}
		wind.Cap <- wind.Cap[,.(value=sum(value)),by=r]
	}
	inputs$parameters$windCap <- wind.Cap

	inputs$parameters$solarCF <- solar.cf[,.(r,t,value=solarCF)]
	inputs$parameters$windCF <- wind.cf[,.(r,t,value=windCF)]

	## Storage and PEM Existing Capacity ##
	if(yr==2025) {
		pemExisting <- data.table(r=as.character(unique(generators.aggregated$RegionSimple)),value=0)
		storExisting <- data.table(r=as.character(unique(generators.aggregated$RegionSimple)),value=0)
	} else if(yr>2025) {
		pemExisting <- data.table(gdx(paste0('runFiles/',yr-5,'_',scenario,'_1-365/outputs.gdx'))['pemCap'])
		storExisting <- data.table(gdx(paste0('runFiles/',yr-5,'_',scenario,'_1-365/outputs.gdx'))['storCap'])
	}
	inputs$parameters$pemExisting <- pemExisting
	inputs$parameters$storExisting <- storExisting

	## Demand Load ##
	inputs$parameters$demandLoad <- load[,.(r,t,value=demandLoad*(1.005^(yr-2025)))]

	return(inputs)
}