### Comments Here ###

#####################

source('base.r')

hdv.proportions <- fread('inputs/hdv_lh_proportions.csv')
ldv.proportions <- fread('inputs/ldv_proportions.csv')
hdv.proportions.high <- fread('inputs/hdv_lh_proportions_high.csv')
ldv.proportions.high <- fread('inputs/ldv_proportions_high.csv')

route.overlap <- fread('cluster/route_overlap.csv')
ld.comm.all <- fread('inputs/ld_comm.csv')
ld.priv.all <- fread('inputs/ld_priv.csv')
cluster.map <- fread('cluster/cluster_all.csv')
cluster.all <- merge(x=cluster.map,y=route.overlap,by='v')

max.stations <- fread('inputs/max_stations.csv')

generate.all.ld <- function(input,proportion) {
	output <- data.table()
	for(y in 2020:2050) {
		if(y==2020) {
			hold <- sample_n(input,size=sum(input$trips)*proportion[Year==y]$Proportion*5.86,weight=input$prob,replace=TRUE)
			hold <- hold[,.(year=y,h2=sum(h2)),by=v]
			previous <- hold
			if(nrow(previous)>0) {previous[,year:=y+1]}
		} else if(y%in%2021:2050) {
			hold <- sample_n(input,size=sum(input$trips)*(proportion[Year==y]$Proportion-proportion[Year==(y-1)]$Proportion)*5.86,weight=input$prob,replace=TRUE)
			hold <- rbind(hold[,.(year=y,h2=sum(h2)),by=v],previous)
			hold <- hold[,.(h2=sum(h2)),by=.(v,year)]
			previous <- hold
			previous$year <- y+1
		}
		output <- rbind(output,hold)
	}
	output <- merge(x=output,y=cluster.map,by='v')
	output <- output[,.(h2=sum(h2)),by=.(cluster,year)]
	return(output)
}

# ld.comm.low.demand.all <- generate.all.ld(ld.comm.all,hdv.proportions)
# ld.priv.low.demand.all <- generate.all.ld(ld.priv.all,ldv.proportions)

# ld.total.low.demand.all <- rbind(ld.comm.low.demand.all,ld.priv.low.demand.all)
# ld.total.low.demand.all <- ld.total.low.demand.all[,.(h2=sum(h2)),by=.(cluster,year)]
# fwrite(ld.total.low.demand.all,file='inputs/totalDemand_ld_low.csv',row.names=FALSE)

# ld.comm.high.demand.all <- generate.all.ld(ld.comm.all,hdv.proportions.high)
# ld.priv.high.demand.all <- generate.all.ld(ld.priv.all,ldv.proportions.high)

# ld.total.high.demand.all <- rbind(ld.comm.high.demand.all,ld.priv.high.demand.all)
# ld.total.high.demand.all <- ld.total.high.demand.all[,.(h2=sum(h2)),by=.(cluster,year)]
# fwrite(ld.total.high.demand.all,file='inputs/totalDemand_ld_high.csv',row.names=FALSE)

buffer <- fread('inputs/buffer.csv')
buffer <- buffer[,.(v,taz=`TAZ12,`)]
sd.priv.all <- fread('inputs/sd_priv.csv')
cluster.buffer <- fread('cluster/cluster_all_buffer.csv')
cluster.buffer[,cluster:=cluster+1000]
cluster.buffer.all <- merge(x=cluster.buffer,y=buffer,by='v')

generate.all.sd <- function(input,proportion) {
	output <- data.table()
	for(y in 2020:2050) {
		if(y==2020) {
			hold <- sample_n(input,size=nrow(input)*proportion[Year==y]$Proportion,replace=TRUE)
			hold <- hold[,.(year=y,h2=sum(h2)),by=v]
			previous <- hold
			if(nrow(previous)>0) {
				previous$year <- y+1
			}
		} else if(y%in%2021:2050) {
			hold <- sample_n(input,size=nrow(input)*(proportion[Year==y]$Proportion-proportion[Year==(y-1)]$Proportion),replace=TRUE)
			hold <- rbind(hold[,.(year=y,h2=sum(h2)),by=v],previous)
			hold <- hold[,.(h2=sum(h2)),by=.(v,year)]
			previous <- hold
			previous$year <- y+1
		}
		output <- rbind(output,hold)
	}
	output <- merge(x=output,y=cluster.buffer,by='v')
	output <- output[,.(h2=sum(h2)),by=.(cluster,year)]
	return(output)
}

# sd.priv.low.demand.all <- generate.all.sd(sd.priv.all,ldv.proportions)
# fwrite(sd.priv.demand.all,file='inputs/totalDemand_sd_low.csv',row.names=FALSE)

# sd.priv.high.demand.all <- generate.all.sd(sd.priv.all,ldv.proportions.high)
# fwrite(sd.priv.high.demand.all,file='inputs/totalDemand_sd_high.csv',row.names=FALSE)

aggregate.demand <- function(yearInput,adoptionScenario) {
	if(adoptionScenario=='base') {
		sd.demand <- fread('inputs/totalDemand_sd_low.csv')
		ld.demand <- fread('inputs/totalDemand_ld_low.csv')
	} else if(adoptionScenario=='high') {
		sd.demand <- fread('inputs/totalDemand_sd_high.csv')
		ld.demand <- fread('inputs/totalDemand_ld_high.csv')
	}
	#total.demand <- rbind(sd.demand,ld.demand)
	total.demand <- ld.demand
	output <- total.demand[year==yearInput]
	return(output)
}

prepTravelInputs <- function(year,adoptionScenario,existingStations) {
	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	existingStations$taz <- as.numeric(existingStations$r)

	total.demand <- aggregate.demand(year,adoptionScenario)

	existingStations.clusters <- merge(x=unique(cluster.all[,.(cluster,taz)]),y=existingStations,by='taz',allow.cartesian=TRUE)$cluster
	existingStations.buffer.clusters <- merge(x=unique(cluster.buffer.all[,.(cluster,taz)]),y=existingStations,by='taz')$cluster

	inputs$sets$r <- unique(c(unique(cluster.all[cluster%in%c(total.demand$cluster,existingStations$r),taz]),unique(cluster.buffer.all[cluster%in%c(total.demand$cluster,existingStations$r),taz])))
	inputs$sets$v <- unique(c(unique(total.demand$cluster),existingStations.clusters,existingStations.buffer.clusters))
	inputs$sets$t <- 1:24
	inputs$sets$VtoR <- unique(rbind(unique(cluster.all[cluster%in%c(total.demand$cluster,existingStations$r),.(v=cluster,r=taz)]),unique(cluster.buffer.all[cluster%in%c(total.demand$cluster,existingStations$r),.(v=cluster,r=taz)])))

	inputs$parameters$demand <- total.demand[,.(v=cluster,value=h2)]

	return(inputs)
}

prepStationInputs <- function(stationInputFile) {
	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	station_inputs <- fread(stationInputFile)

	inputs$sets$i <- station_inputs$i

	inputs$parameters$dailyCapacity <- station_inputs[,list(i,value=dailyCapacity)]
	inputs$parameters$hourlyCapacity <- station_inputs[,list(i,value=hourlyCapacity)]
	inputs$parameters$cost <- station_inputs[,list(i,value=cost)]
	inputs$parameters$maxStations <- max.stations

	return(inputs)
}

runScenario <- function(scenarioName,stationInputFile,existingStations,year,adoptionScenario) {
	station <- prepStationInputs(stationInputFile)
	travel <- prepTravelInputs(year,adoptionScenario,existingStations)

	inputs <- list()
	inputs$sets <- c(station$sets,travel$sets)
	inputs$parameters <- c(station$parameters,travel$parameters)

	inputs$parameters$existingStations <- existingStations

	dir.create(paste0('runFiles/',scenarioName),showWarnings=FALSE)
	write.gdx(paste0('runFiles/',scenarioName,'/inputs.gdx',sep=''),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))
	spatialModel_infrastructure.gms <- readLines('spatialModel_infrastructure.gms')
	writeLines(spatialModel_infrastructure.gms,con=paste0('runFiles/',scenarioName,'/spatialModel_infrastructure.gms'))
}

runGams <- function(scenarioName) {
	print(paste('Running: ',scenarioName))
	start.time <- Sys.time()
	setwd(paste0(base.dir,'/runFiles/',scenarioName))
	gams('spatialModel_infrastructure.gms')
	print('Full results:')
	print.lst.status('spatialModel_infrastructure.lst')
	print(Sys.time()-start.time)
	setwd(base.dir)
}

grabStations <- function(year,scenario.gen) {
	new.stations <- data.table(gdx(paste0('runFiles/',year,'_',scenario.gen,'/outputs.gdx'))['station'])
	old.stations <- data.table(gdx(paste0('runFiles/',year,'_',scenario.gen,'/outputs.gdx'))['existingStations'])
	output <- rbind(new.stations,old.stations)
	output <- output[,.(value=sum(value)),by=.(i,r)]
	output <- output[value>0,]
	return(output)
}

runAll.Scenarios <- function(adoptionScenario,yearsForesight) {
	for(yr in seq(2020,2050,by=yearsForesight)) {
		if(adoptionScenario=='base') {
			scenario <- paste0(yr,'_base')
			scenario.gen <- 'base'
		} else if(adoptionScenario=='high') {
			scenario <- paste0(yr,'_high')
			scenario.gen <- 'high'
		}
		if(yr==2020) {
			existingStations <- data.table(i=1,r=1677,value=0)
		} else if(yr>2020) {
			existingStations <- grabStations(yr-yearsForesight,scenario.gen)
		}
		runScenario(scenarioName=scenario,stationInputFile='inputs/station_inputs.csv',existingStations=existingStations,year=yr,adoptionScenario=adoptionScenario)
		runGams(scenarioName=scenario)
	}
}

runAll.Scenarios('base',5)
runAll.Scenarios('high',5)

parseStationOutputs <- function(scenarioGen,years) {
	output <- data.table()
	for(yr in years) {
		stations <- data.table(gdx(paste0('runFiles/',yr,'_',scenarioGen,'/outputs.gdx'))['station'])
		existingStations <- data.table(gdx(paste0('runFiles/',yr,'_',scenarioGen,'/outputs.gdx'))['existingStations'])
		stations <- rbind(stations,existingStations)
		stations <- stations[,.(value=sum(value)),by=.(i,r)]
		stations[,year:=yr]
		output <- rbind(output,stations)
	}
	fwrite(output,file=paste0('results/outputs_aggregateStations_',scenarioGen,'.csv'),row.names=FALSE)
}

parseFuelingOutputs <- function(scenarioGen,years) {
	output <- data.table()
	for(yr in years) {
		fueling <- data.table(gdx(paste0('runFiles/',yr,'_',scenarioGen,'/outputs.gdx'))['fueled'])
		fueling <- fueling[,.(fueling=sum(value)),by=r]
		fueling[,year:=yr]
		output <- rbind(output,fueling)
	}
	fwrite(output,file=paste0('results/outputs_aggregateFueling_',scenarioGen,'.csv'),row.names=FALSE)
}

parseStationOutputs('base',seq(2025,2050,by=5))
parseStationOutputs('high',seq(2025,2050,by=5))
parseFuelingOutputs('base',seq(2025,2050,by=5))
parseFuelingOutputs('high',seq(2025,2050,by=5))