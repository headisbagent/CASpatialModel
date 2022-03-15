
ca.regions <- c('WEC_CALN','WEC_LADW','WEC_SDGE','WECC_IID','WECC_SCE','WECC_SF')

ipm <- st_read(file.path('ipm_simple','ipm_simple.shp'))
taz.id <- st_read(file.path('shps','TAZ_IDonly_sf.gpkg'))

region.intersect <- fread('inputs/TAZ_IPregion.csv')
region.key <- fread('inputs/inputs_region_key.csv')
region.intersect <- merge(x=region.intersect,y=region.key,by.x='IPM_Region',by.y='RegionName')
region.intersect <- region.intersect[,.(County,TAZ12,IPM_Region=RegionSimple)]

h2Demand.adjustment <- fread('inputs/h2Demand_adjustment.csv')

get.dailyDemand <- function(scenario,dInput,yr) {
	inputs <- list()
	inputs$parameters <- list()

	demand <- data.table(gdx(paste0('../infrastructure_module/runFiles/',scenario,'/outputs.gdx'))['fueled'])
	demand <- demand[,.(demand=sum(value)),by=r]
	demand[,r:=as.numeric(r)]
	demand <- merge(x=demand,y=region.intersect,by.x='r',by.y='TAZ12')
	demand[IPM_Region=='PNW',IPM_Region:='CA_N']
	demand <- demand[,.(demand=sum(demand)),by='IPM_Region']
	names(demand) <- c('r','value')
	final <- data.table()
	for(day in dInput) {
		hold <- demand
		hold[,d:=day]
		final <- rbind(final,hold)
	}
	final <- final[,.(r,d,value)]
	final <- merge(x=final,y=h2Demand.adjustment,by='d',all.x=TRUE)
	final[,value.adj:=value*finalAdjust]
	final <- final[,.(r,d,value=value.adj)]

	inputs$parameters$h2Demand <- final

	freeStorageH2 <- final[,.(value=max(value)),by=r]

	inputs$parameters$freeStorageH2 <- freeStorageH2

	if(grepl('base',scenario,fixed=TRUE)|grepl('low',scenario,fixed=TRUE)) {
		h2Stationary <- fread('inputs/inputs_h2Stationary_base.csv')
		inputs$parameters$h2Stationary <- h2Stationary[Year==yr,.(r,value)]
	} else if(grepl('high',scenario,fixed=TRUE)) {
		h2Stationary <- fread('inputs/inputs_h2Stationary_high.csv')
		inputs$parameters$h2Stationary <- h2Stationary[Year==yr,.(r,value)]
	}

	return(inputs)
}