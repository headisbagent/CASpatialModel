
ca.regions <- c('WEC_CALN','WEC_LADW','WEC_SDGE','WECC_IID','WECC_SCE','WECC_SF')

ipm <- st_read(file.path('ipm_simple','ipm_simple.shp'))
taz.id <- st_read(file.path('shps','TAZ_IDonly_sf.gpkg'))

region.intersect <- fread('inputs/TAZ_IPregion.csv')
region.key <- fread('inputs/inputs_region_key.csv')
region.intersect <- merge(x=region.intersect,y=region.key,by.x='IPM_Region',by.y='RegionName')
region.intersect <- region.intersect[,.(County,TAZ12,IPM_Region=RegionSimple)]

scenario <- 'CSTDM_trips10_distance150_prop01'

get.dailyDemand <- function(scenario,dInput) {
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

	inputs$parameters$h2Demand <- final
	return(inputs)
}