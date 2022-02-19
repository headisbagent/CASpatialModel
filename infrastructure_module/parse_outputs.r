source('base.R')

truckNetwork <- st_read(file.path('shps','CSTDM_sf.gpkg'))
taz.centroid <- st_read(file.path('shps','TAZ_centroid_sf.gpkg'))
taz.id <- st_read(file.path('shps','TAZ_IDonly_sf.gpkg'))

### Plotting examples of mobility simulation ###
hdv.proportions <- fread('inputs/hdv_lh_proportions.csv')
ldv.proportions <- fread('inputs/ldv_proportions.csv')
hdv.proportions.high <- fread('inputs/hdv_lh_proportions_high.csv')
ldv.proportions.high <- fread('inputs/ldv_proportions_high.csv')

total.proportions <- rbind(hdv.proportions[,.(Year,Proportion,Type='HDV SC1')],ldv.proportions[,.(Year,Proportion,Type='LDV SC1')],hdv.proportions.high[,.(Year,Proportion,Type='HDV High')],ldv.proportions.high[,.(Year,Proportion,Type='LDV High')])

plotSave <- ggplot(data=total.proportions,aes(x=Year,y=Proportion,color=Type))+
	geom_line()+
	scale_color_brewer(name='Vehicle Type',palette='Set1')+
	theme_bw()
ggsave(plotSave,file='figures/technology_adoption.pdf',height=6,width=9)
ggsave(plotSave,file='figures/technology_adoption.png',height=6,width=9)

route.overlap <- fread('cluster/route_overlap.csv')
ld.comm.all <- fread('inputs/ld_comm.csv')
ld.priv.all <- fread('inputs/ld_priv.csv')

sample.ld <- function(input,proportion) {
	output <- sample_n(input,size=sum(input$trips)*proportion,weight=input$prob,replace=TRUE)
	output <- output[,.(trips=ceiling(sum(trips))),by=v]
	return(output)
}

aggregate.demand <- function(yr,scenario.gen) {
	if(scenario.gen=='base') {
		hdv.proportion <- hdv.proportions[Year==yr,]$Proportion
		ldv.proportion <- ldv.proportions[Year==yr,]$Proportion
	} else if(scenario.gen=='high') {
		hdv.proportion <- hdv.proportions.high[Year==yr,]$Proportion
		ldv.proportion <- ldv.proportions.high[Year==yr,]$Proportion
	}
	ld.comm.demand <- sample.ld(ld.comm.all,hdv.proportion)
	ld.priv.demand <- sample.ld(ld.priv.all,ldv.proportion)
	total.demand <- rbind(ld.comm.demand,ld.priv.demand)
	return(total.demand)
}

prep.plotData <- function(yr,scenario.gen) {
	trip.demand <- aggregate.demand(yr,scenario.gen)
	trip.demand <- merge(x=trip.demand,y=route.overlap,by='v')
	trip.demand <- trip.demand[,.(trips=sum(trips)),by=taz]
	demand.quantiles <- quantile(trip.demand$trips,probs=c(.2,.4,.6,.8,1))
	trip.demand[,trip.quantiles:=ifelse(trips>=0&trips<demand.quantiles[1],paste0('0-',round(demand.quantiles[1])),ifelse(trips>=demand.quantiles[1]&trips<demand.quantiles[2],paste0(round(demand.quantiles[1]),'-',round(demand.quantiles[2])),ifelse(trips>=demand.quantiles[2]&trips<demand.quantiles[3],paste0(round(demand.quantiles[2]),'-',round(demand.quantiles[3])),ifelse(trips>=demand.quantiles[3]&trips<demand.quantiles[4],paste0(round(demand.quantiles[3]),'-',round(demand.quantiles[4])),paste0(round(demand.quantiles[4]),'-',round(demand.quantiles[5]))))))]
	trip.demand$trip.quantiles <- factor(trip.demand$trip.quantiles,levels=c(paste0('0-',round(demand.quantiles[1])),paste0(round(demand.quantiles[1]),'-',round(demand.quantiles[2])),paste0(round(demand.quantiles[2]),'-',round(demand.quantiles[3])),paste0(round(demand.quantiles[3]),'-',round(demand.quantiles[4])),paste0(round(demand.quantiles[4]),'-',round(demand.quantiles[5]))))

	taz.trip.demand <- merge(x=taz.id,y=trip.demand,by.x='TAZ12',by.y='taz',all.x=TRUE)
	taz.trip.demand$trip.quantiles[is.na(taz.trip.demand$trip.quantiles)] <- levels(taz.trip.demand$trip.quantiles)[1]
	taz.trip.demand <- st_transform(taz.trip.demand,4269)
	return(taz.trip.demand)
}

plot.tripScenarios <- function(yr,scenario.gen) {
	taz.trip.demand <- prep.plotData(yr,scenario.gen)

	sf.window <- as(spatstat::as.owin(c(xmin=-122.8,xmax=-121.7,ymin=37.1,ymax=38)),"SpatialPolygons")
	la.window <-  as(spatstat::as.owin(c(xmin=-118.6,xmax=-117.3,ymin=33.4,ymax=34.4)),"SpatialPolygons")
	sd.window <-  as(spatstat::as.owin(c(xmin=-117.35,xmax=-116.8,ymin=32.5,ymax=32.9)),"SpatialPolygons")

	ca.window <- ggplot()+
		geom_sf(data=taz.trip.demand,aes(fill=trip.quantiles),color=NA)+
		scale_fill_brewer(name='Number of Trips',palette='Blues')+
		geom_polygon(data=sf.window,aes(x=long,y=lat,group=group),fill=NA,color='red',size=1)+
		geom_polygon(data=la.window,aes(x=long,y=lat,group=group),fill=NA,color='blue4',size=1)+
		geom_polygon(data=sd.window,aes(x=long,y=lat,group=group),fill=NA,color='forestgreen',size=1)+
		coord_sf(xlim=c(-126,xmax=-114),ylim=c(32.5,42.5))+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank())

	sf.trips <- ggplot()+
		geom_sf(data=taz.trip.demand,aes(fill=trip.quantiles),color=NA)+
		scale_fill_brewer(name='Number of Long\nDistance Trips',palette='Blues')+
		coord_sf(xlim=c(-122.8,-121.7),ylim=c(37.1,38),expand=FALSE)+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='red',size=1.5,fill=NA),legend.position='none')

	la.trips <- ggplot()+
		geom_sf(data=taz.trip.demand,aes(fill=trip.quantiles),color=NA)+
		scale_fill_brewer(name='Number of Long\nDistance Trips',palette='Blues')+
		coord_sf(xlim=c(-118.6,-117.3),ylim=c(33.4,34.4),expand=FALSE)+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='blue4',size=1.5,fill=NA),legend.position='none')

	sd.trips <-  ggplot()+
		geom_sf(data=taz.trip.demand,aes(fill=trip.quantiles),color=NA)+
		scale_fill_brewer(name='Number of Long\nDistance Trips',palette='Blues')+
		coord_sf(xlim=c(-117.35,-116.8),ylim=c(32.5,32.9),expand=FALSE)+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='forestgreen',size=1.5,fill=NA),legend.position='none')

	ca.trips <- ca.window+
		annotation_custom(grob=ggplotGrob(sf.trips),xmin=-120,xmax=-115.5,ymin=36.5,ymax=45)+
		annotation_custom(grob=ggplotGrob(sd.trips),xmin=-117.5,xmax=-113,ymin=35.5,ymax=39)+
		annotation_custom(grob=ggplotGrob(la.trips),xmin=-126,xmax=-121,ymin=32,ymax=36)
	ggsave(ca.trips,file=paste0('figures/ca_ld_trips_',scenario.gen,'_',yr,'.pdf'),height=10,width=9)
	ggsave(ca.trips,file=paste0('figures/ca_ld_trips_',scenario.gen,'_',yr,'.png'),height=10,width=9)	
}

for(yr in seq(2020,2050,by=5)) {
	for(scen in c('base','high')) {
		plot.tripScenarios(yr,scen)
	}
}

plotSave <- ggplot()+
	geom_sf(data=taz.id,color='lightgrey',fill='NA')+
	geom_sf(data=truckNetwork,color='red')+
	coord_sf()+
	theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
ggsave(plotSave,file='figures/roadNetwork.png',height=5.5,width=5)
ggsave(plotSave,file='figures/roadNetwork.pdf',height=5.5,width=5)

plotSave <- ggplot()+
	geom_sf(data=st_transform(taz.id,4269),color='black',fill='NA',size=1)+
	geom_sf(data=st_transform(truckNetwork,4269),color='red')+
	coord_sf(xlim=c(-118.05,-118.0),ylim=c(33.95,34.0),expand=FALSE)+
	theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
ggsave(plotSave,file='figures/roadNetwork_zoomed.png',height=6,width=6)
ggsave(plotSave,file='figures/roadNetwork_zoomed.pdf',height=6,width=6)

plotSave <- ggplot()+
	geom_sf(data=st_transform(taz.id,4269),color='black',fill='NA')+
	coord_sf(xlim=c(-118.4,-118.0),ylim=c(33.7,34.1),expand=FALSE)+
	theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
ggsave(plotSave,file='figures/roadNetwork_la.png',height=6,width=6)
ggsave(plotSave,file='figures/roadNetwork_la.pdf',height=6,width=6)

station.key <- fread('inputs/station_inputs.csv')
station.key.unique <- unique(station.key[,.(dailyCapacity,hourlyCapacity)])
station.key.unique <- station.key.unique[order(dailyCapacity,hourlyCapacity)]
station.key.unique[,order.station:=1:nrow(station.key.unique)]
station.key <- merge(x=station.key,y=station.key.unique,by=c('dailyCapacity','hourlyCapacity'),all.x=TRUE)

# plotSave <- ggplot()+
# 	geom_sf(data=taz.id,color='black',fill='NA')+
# 	coord_sf()+
# 	theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
# ggsave(plotSave,file='figures/tazNetwork.png',height=5.5,width=5)
# ggsave(plotSave,file='figures/tazNetwork.pdf',height=5.5,width=5)

generate.stationMaps <- function(scenarioName) {
	outputs.VtoR <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['VtoR'])
	outputs.demand <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['demand'])
	outputs.station <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['station'])
	outputs.existingStations <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['existingStations'])
	outputs.station <- rbind(outputs.station,outputs.existingStations)
	outputs.station <- outputs.station[,.(value=sum(value)),by=.(i,r)]
	outputs.station[,i:=as.numeric(i)]
	outputs.station <- merge(x=outputs.station,y=station.key,by='i')
	outputs.station[,capacity:=paste(dailyCapacity,'daily,',hourlyCapacity,'hourly')]
	outputs.station[,capacity:=factor(capacity,levels=unique(outputs.station$capacity[order(outputs.station$order.station)]))]

	outputs.fueled <- data.table(gdx(paste0('runFiles/',scenarioName,'/outputs.gdx'))['fueled'])
	outputs.fueled <- outputs.fueled[value>0,.(demand=sum(value)),by=r]
	demand.quantiles <- quantile(outputs.fueled$demand,probs=c(.2,.4,.6,.8,1))
	outputs.fueled[,fuel.quantiles:=ifelse(demand>=0&demand<demand.quantiles[1],paste0('0-',round(demand.quantiles[1])),ifelse(demand>=demand.quantiles[1]&demand<demand.quantiles[2],paste0(round(demand.quantiles[1]),'-',round(demand.quantiles[2])),ifelse(demand>=demand.quantiles[2]&demand<demand.quantiles[3],paste0(round(demand.quantiles[2]),'-',round(demand.quantiles[3])),ifelse(demand>=demand.quantiles[3]&demand<demand.quantiles[4],paste0(round(demand.quantiles[3]),'-',round(demand.quantiles[4])),paste0(round(demand.quantiles[4]),'-',round(demand.quantiles[5]))))))]
	outputs.fueled[,fuel.quantiles:=factor(fuel.quantiles,levels=c(paste0('0-',round(demand.quantiles[1])),paste0(round(demand.quantiles[1]),'-',round(demand.quantiles[2])),paste0(round(demand.quantiles[2]),'-',round(demand.quantiles[3])),paste0(round(demand.quantiles[3]),'-',round(demand.quantiles[4])),paste0(round(demand.quantiles[4]),'-',round(demand.quantiles[5]))))]

	route.intersect <- taz.id[taz.id$TAZ12%in%unique(as.numeric(outputs.VtoR$r)),]

	stations.install <- merge(x=taz.centroid,y=outputs.station,by.x='TAZ12',by.y='r')
	stations.install <- stations.install[stations.install$value>0,]
	stations.install <- st_transform(stations.install,4269)

	stations.install.forPlot <- data.table(stations.install)[,.(Count=sum(value)),by=capacity]

	plotSave <- ggplot(data=stations.install.forPlot,aes(x=capacity,y=Count,fill=capacity))+
		geom_bar(stat='identity')+
		xlab('Station Attributes')+
		ylab('Count of Stations')+
		scale_fill_brewer(palette='Dark2')+
		theme_bw()+
		theme(legend.position="none",axis.text.x=element_text(angle=45,vjust=1,hjust=1))
	ggsave(plotSave,file=paste0('figures/station_count_',scenarioName,'.png'),height=6.,width=9)
	ggsave(plotSave,file=paste0('figures/station_count_',scenarioName,'.pdf'),height=6.,width=9)

	stations.fueled <- merge(x=taz.id,y=outputs.fueled,by.x='TAZ12',by.y='r')
	stations.fueled <- st_transform(stations.fueled,4269)

	taz.id.transform <- st_transform(taz.id,4269)
	route.intersect.transform <- st_transform(route.intersect,4269)

	sf.window <- as(spatstat::as.owin(c(xmin=-122.8,xmax=-121.7,ymin=37.1,ymax=38)),"SpatialPolygons")
	la.window <-  as(spatstat::as.owin(c(xmin=-118.6,xmax=-117.3,ymin=33.4,ymax=34.4)),"SpatialPolygons")
	sd.window <-  as(spatstat::as.owin(c(xmin=-117.35,xmax=-116.8,ymin=32.5,ymax=32.9)),"SpatialPolygons")

	if(max(stations.install$value)>1) {
		ca.window <- ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='NA',color='black')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity,size=factor(value)))+
			geom_polygon(data=sf.window,aes(x=long,y=lat,group=group),fill=NA,color='red',size=1)+
			geom_polygon(data=la.window,aes(x=long,y=lat,group=group),fill=NA,color='blue4',size=1)+
			geom_polygon(data=sd.window,aes(x=long,y=lat,group=group),fill=NA,color='forestgreen',size=1)+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations',guide='none')+
			coord_sf(xlim=c(-126,xmax=-114),ylim=c(32.5,42.5))+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
		sf <- ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='NA',color='black')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity,size=factor(value)))+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations')+
			coord_sf(xlim=c(-122.8,-121.7),ylim=c(37.1,38),expand=FALSE)+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='red',size=1.5,fill=NA),legend.position='none')

		la <- ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='NA',color='black')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity,size=factor(value)))+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations')+
			coord_sf(xlim=c(-118.6,-117.3),ylim=c(33.4,34.4),expand=FALSE)+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='blue4',size=1.5,fill=NA),legend.position='none')

		sd <-  ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='NA',color='black')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity,size=factor(value)))+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations')+
			coord_sf(xlim=c(-117.35,-116.8),ylim=c(32.5,32.9),expand=FALSE)+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='forestgreen',size=1.5,fill=NA),legend.position='none')
	} else{
		ca.window <- ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='gray')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity))+
			geom_polygon(data=sf.window,aes(x=long,y=lat,group=group),fill=NA,color='red',size=1)+
			geom_polygon(data=la.window,aes(x=long,y=lat,group=group),fill=NA,color='blue4',size=1)+
			geom_polygon(data=sd.window,aes(x=long,y=lat,group=group),fill=NA,color='forestgreen',size=1)+
			scale_size(name='Number of Stations',guide='none')+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			coord_sf(xlim=c(-126,xmax=-114),ylim=c(32.5,42.5))+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
		sf <- ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='gray')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity))+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations')+
			coord_sf(xlim=c(-122.8,-121.7),ylim=c(37.1,38),expand=FALSE)+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='red',size=1.5,fill=NA),legend.position='none')

		la <- ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='gray')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity))+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations')+
			coord_sf(xlim=c(-118.6,-117.3),ylim=c(33.4,34.4),expand=FALSE)+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='blue4',size=1.5,fill=NA),legend.position='none')

		sd <-  ggplot()+
			geom_sf(data=taz.id.transform,color='lightgrey',fill='NA')+
			geom_sf(data=route.intersect.transform,fill='gray')+
			geom_sf(data=stations.fueled,aes(fill=fuel.quantiles))+
			geom_sf(data=stations.install,aes(shape=capacity,color=capacity))+
			scale_fill_brewer(name='H2 Daily\nDemand (kg)',palette='Blues')+
			scale_shape(name='Station Capacity (kg)')+
			scale_color_brewer(name='Station Capacity (kg)',palette='Dark2')+
			scale_size_discrete(name='Number of Stations')+
			coord_sf(xlim=c(-117.35,-116.8),ylim=c(32.5,32.9),expand=FALSE)+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank(),panel.grid.major=element_blank(),panel.border=element_rect(color='forestgreen',size=1.5,fill=NA),legend.position='none')
		}
	
	ca <- ca.window+
		annotation_custom(grob=ggplotGrob(sf),xmin=-120,xmax=-115.5,ymin=36.5,ymax=45)+
		annotation_custom(grob=ggplotGrob(sd),xmin=-117.5,xmax=-113,ymin=35.5,ymax=39)+
		annotation_custom(grob=ggplotGrob(la),xmin=-126,xmax=-121,ymin=32,ymax=36)
	ggsave(ca,file=paste0('figures/station_map_',scenarioName,'.pdf'),height=5.5,width=9)
	ggsave(ca,file=paste0('figures/station_map_',scenarioName,'.png'),height=5.5,width=9)
}

generate.allMaps <- function() {
	for(f in list.files('runFiles')) {
		generate.stationMaps(f)	
	}
}

generate.allMaps()

plot.stationCount <- function() {
	base.stations <- fread('results/outputs_aggregateStations_base.csv')
	base.stations <- base.stations[,.(count=sum(value)),by=year]
	base.stations[,scenario:='Low']
	high.stations <- fread('results/outputs_aggregateStations_high.csv')
	high.stations <- high.stations[,.(count=sum(value)),by=year]
	high.stations[,scenario:='High']

	stations <- rbind(base.stations,high.stations)

	plotSave <- ggplot(data=stations,aes(x=year,y=count,color=scenario))+
		geom_line()+
		xlab('Year')+
		ylab('Total Number of H2 Stations')+
		scale_color_discrete(name='H2 Adoption\nScenario')+
		theme_bw()
	ggsave(plotSave,file='figures/totalStationCounts.pdf',height=6,width=9)
	ggsave(plotSave,file='figures/totalStationCounts.png',height=6,width=9)
}

plot.fuelingDemand <- function() {
	base.fueling <- fread('results/outputs_aggregateFueling_base.csv')
	base.fueling <- base.fueling[,.(fueling=sum(fueling)),by=year]
	base.fueling[,scenario:='Low']
	high.fueling <- fread('results/outputs_aggregateFueling_high.csv')
	high.fueling <- high.fueling[,.(fueling=sum(fueling)),by=year]
	high.fueling[,scenario:='High']

	fueling <- rbind(base.fueling,high.fueling)

	plotSave <- ggplot(data=fueling,aes(x=year,y=fueling*365/10^9,color=scenario))+
		geom_line()+
		xlab('Year')+
		ylab('Annual H2 Fueling Demand\n(millions of tons)')+
		scale_color_discrete(name='H2 Adoption\nScenario')+
		theme_bw()
	ggsave(plotSave,file='figures/totalFuelingDemand.pdf',height=6,width=9)
	ggsave(plotSave,file='figures/totalFuelingDemand.png',height=6,width=9)
}