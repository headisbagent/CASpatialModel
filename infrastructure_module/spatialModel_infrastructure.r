### Comments Here ###

#####################

source('base.r')

routes <- fread('datafiles/TAZCentroid_50Samples_ShortestRoutes.csv')
routes <- routes[Beg!=End,]
intersect.taz <- fread('datafiles/TAZ_intersect.csv')

routes <- merge(x=routes,y=intersect.taz,by='Route')

taz <-  st_read(file.path('TAZCentroid_50Samples','TAZ_t1.shp'))
taz.centroids <- st_read(file.path('TAZCentroid_50Samples','TAZ_t1_centroid.shp'))

# Function to prepare the TAZ regions that a vehicle passes through when traveling from Beg to End

# mergeTAZ.intersect <- function() {
# 	files <- list.files('dataFiles/IntersectingTAZtxt_ID')

# 	output <- data.table()
# 	for(f in files) {
# 		hold <- read.csv(paste0('dataFiles/IntersectingTAZtxt_ID/',f))
# 		hold <- gsub('\\s+',' ',hold[1,])
# 		hold <- substr(hold,2,nchar(hold))
# 		hold <- gsub(' ',',',hold)
# 		temp <- data.table('Route'=gsub('\\.csv','',f),'Intersect'=hold)
# 		output <- rbind(output,temp)
# 	}
	
# 	fwrite(output,file='dataFiles/TAZ_intersect.csv')
# }

prepTravelInputs <- function(numVehicles,efficiency) {
	inputs <- list()
	inputs$sets <- list()
	inputs$parameters <- list()

	inputs$sets$r <- paste0(unique(routes$Beg))
	inputs$sets$v <- paste0('v.',1:numVehicles)

	route.assign <- routes[sample(1:nrow(routes),size=numVehicles,replace=TRUE)]
	VtoR <- data.table()
	for(veh in 1:numVehicles) {
		r.hold <- as.numeric(unlist(strsplit(route.assign[veh,Intersect],',')))
		VtoR.hold <- data.table('v'=inputs$sets$v[veh],'r'=paste0(r.hold))
		VtoR <- rbind(VtoR,VtoR.hold)
	}
	inputs$sets$VtoR <- VtoR

	inputs$parameters$demand <- data.table('v'=inputs$sets$v,'value'=route.assign$Total_Mile*efficiency)

	#print(inputs)

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

runScenario <- function(scenarioName,numVehicles,stationInputFile,efficiency) {
	station <- prepStationInputs(stationInputFile)
	other <- prepTravelInputs(numVehicles,efficiency)

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

runScenario(scenarioName='test',numVehicles=5000,efficiency=0.1,stationInputFile='inputs/station_inputs.csv')
runGams(scenarioName='test')

outputs.VtoR <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['VtoR'])
outputs.demand <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['demand'])
outputs.station <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['station'])
outputs.fueled <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['fueled'])

route.intersect <- taz[taz$ID%in%outputs.VtoR$r,]

stations.install <- merge(x=taz.centroids,y=outputs.station,by.x='ID',by.y='r')
stations.install <- stations.install[stations.install$value>0,]

plotSave <- ggplot()+
	geom_sf(data=taz,color='black')+
	geom_sf(data=route.intersect,fill='red')+
	geom_sf(data=stations.install,aes(color=i,size=value))+
	coord_sf()+
	theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
ggsave(plotSave,file='figures/test_case.png')