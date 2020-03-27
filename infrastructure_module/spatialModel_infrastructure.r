### Comments Here ###

#####################

source('base.r')

createRandomInputs <- function(numRegions,numVehicles,maxDemand) {
	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	inputs$sets$r <- paste0('r.',1:numRegions)
	inputs$sets$v <- paste0('v.',1:numVehicles)

	VtoR <- data.table()
	for(veh in inputs$sets$v) {
		r.hold <- sample(inputs$sets$r,sample(1:length(inputs$sets$r),1),replace=FALSE)
		VtoR.hold <- data.table('v'=veh,'r'=r.hold)
		VtoR <- rbind(VtoR,VtoR.hold)
	}
	inputs$sets$VtoR <- VtoR

	inputs$parameters$demand <- data.table('v'=inputs$sets$v,'value'=runif(n=length(inputs$sets$v),min=1,max=maxDemand))

	return(inputs)
}

prepStationInputs <- function(stationInputFile) {
	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	station_inputs <- fread(stationInputFile)

	inputs$sets$i <- station_inputs$i

	inputs$parameters$capacity <- station_inputs[,list(i,value=capacity)]
	inputs$parameters$cost <- station_inputs[,list(i,value=cost)]

	return(inputs)
}

runScenario <- function(scenarioName,numRegions,numVehicles,maxDemand,stationInputFile) {
	station <- prepStationInputs(stationInputFile)
	other <- createRandomInputs(numRegions,numVehicles,maxDemand)

	inputs <- list()
	inputs$sets <- c(station$sets,other$sets)
	inputs$parameters <- c(station$parameters,other$parameters)

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

diagnosticPlot <- function(scenarioName) {
	outputs.VtoR <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['VtoR'])
	outputs.demand <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['demand'])
	outputs.station <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['station'])
	outputs.fueled <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['fueled'])

	region.map <- data.table(r=unique(outputs.VtoR$r),x=runif(n=length(unique(outputs.VtoR$r)),min=0,max=100),y=runif(n=length(unique(outputs.VtoR$r)),min=0,max=100))

	vehicle.map <- merge(x=outputs.VtoR,y=region.map,by='r',all.x=TRUE)
	vehicle.map <- merge(x=vehicle.map,y=outputs.demand[,.(v,demand=value)],by='v',all.x=TRUE)
	station.map <- merge(x=outputs.station[value>0,.(i,r,numStations=value)],y=region.map,by='r',all.x=TRUE)
	station.map <- merge(x=station.map,y=outputs.fueled[value>0,.(fueled=sum(value)),by=r],by='r',all.x=TRUE)

	plotSave <- ggplot()+
		geom_path(data=vehicle.map,aes(x=x,y=y,group=v,size=demand),alpha=.5)+
		geom_point(data=station.map,aes(x=x,y=y,size=fueled*3),colour='purple',alpha=.2)+
		geom_point(data=station.map,aes(x=x,y=y,colour=i,size=numStations*20),alpha=.5)+
		scale_radius(range=c(3,40))+
		theme_bw()

	return(plotSave)
}


runScenario(scenarioName='test',numRegions=50,numVehicles=300,maxDemand=100,stationInputFile='inputs/station_inputs.csv')
runGams(scenarioName='test')
diagnosticPlot('test')