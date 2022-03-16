source('base.R')
source('prep-inputs-grid.R')
source('prep-inputs-mobility.R')

ca.regions <- c('CA_N','CA_LA','CA_SD')

FuelType.key <- data.table(FuelType=c('Biomass','Coal','Fwaste','Geothermal','Hydro','LF Gas','MSW','NaturalGas','Non-fossil','Nuclear','Oil','Pet. Coke','Pumps','Solar','Tires','Waste Coal','Wind'),FuelType.simple=c('Biomass','Coal','Biomass','Geothermal','Hydro','NaturalGas','Biomass','NaturalGas','Other','Nuclear','Oil','Coal','Hydro','Solar','Coal','Coal','Wind'))

fetchOutputs <- function(scenario,yr) {
	scenario.name <- paste(yr,scenario,sep='_')

	variable.names <- c('generation','trans','solarNew','windNew','storSOC','storIn','storOut','storCap','pemCap','fuelH2','freeH2','genCost','demandLoad','maxGen','solarCap','windCap','solarCF','windCF','transCap','transCost','h2Demand','h2Stationary','freeStorageH2')

	output <- list()
	for(i in 1:length(variable.names)) {
		output[[i]] <- data.table(gdx(paste0('runFiles/',scenario.name,'/outputs.gdx'))[variable.names[i]])[,year:=yr]
	}
	names(output) <- variable.names
	return(output)
}

fetchOutputs.allYears <- function(scenario) {
	hold <- list()
	years <- seq(2025,2050,by=5)
	for(i in 1:length(years)) {
		hold[[i]] <- fetchOutputs(scenario,years[i])
	}
	output <- lapply(seq_along(hold[[1]]),function(x) rbind(hold[[1]][[x]],hold[[2]][[x]],hold[[3]][[x]],hold[[4]][[x]],hold[[5]][[x]],hold[[6]][[x]]))
	names(output) <- c('generation','trans','solarNew','windNew','storSOC','storIn','storOut','storCap','pemCap','fuelH2','freeH2','genCost','demandLoad','maxGen','solarCap','windCap','solarCF','windCF','transCap','transCost','h2Demand','h2Stationary','freeStorageH2')
	return(output)
}

# Simplifying IPM map
# ipm <- st_read(file.path('ipm_v6_regions','IPM_Regions_201770405.shp'))
# ipm.wecc <- ipm[ipm$IPM_Region%in%c("WEC_BANC","WEC_CALN","WEC_LADW","WEC_SDGE","WECC_AZ","WECC_CO","WECC_ID","WECC_IID","WECC_MT","WECC_NM","WECC_NNV","WECC_PNW","WECC_SCE","WECC_SNV","WECC_UT","WECC_WY"),]

# region.key <- data.table('IPM_Region'=c("WEC_BANC","WEC_CALN","WEC_LADW","WEC_SDGE","WECC_AZ","WECC_CO","WECC_ID","WECC_IID","WECC_MT","WECC_NM","WECC_NNV","WECC_PNW","WECC_SCE","WECC_SNV","WECC_UT","WECC_WY"),'simple'=c('CA_N','CA_N','CA_LA','CA_SD','AZNM','KMPA','NWPE','CA_SD','NWPE','AZNM','NWPE','PNW','CA_LA','NWPE','NWPE','KMPA'))

# ipm.wecc <- merge(x=ipm.wecc,y=region.key,by='IPM_Region')
# ipm.wecc.simple <- ipm.wecc %>%
# 	group_by(simple) %>%
# 	summarise(geometry=sf::st_union(geometry)) %>%
# 	ungroup()

# ipm.wecc.simple <- rmapshaper::ms_simplify(input=as(ipm.wecc.simple,'Spatial')) %>%
# 	st_as_sf()

# st_write(ipm.wecc.simple,'wecc_simple/wecc_simple.shp')

wecc.simple <- st_read(file.path('wecc_simple','wecc_simple.shp'))
wecc.centroids <- st_centroid(wecc.simple)

plot.GeneratorsMap <- function() {
	generators.all <- rbind(generators.other,generators.solar.wind)
	forPlot <- generators.all[,.(FuelType,Capacity,LAT,LON)]
	forPlot <- st_as_sf(forPlot,coords=c('LON','LAT'),crs=4269)
	forPlot <- merge(x=forPlot,y=FuelType.key,by='FuelType')

	forPlot.wecc <- st_transform(wecc.simple,crs=4269)

	plotSave <- ggplot()+
		geom_sf(data=forPlot.wecc,fill=NA)+
		geom_sf(data=forPlot,aes(fill=FuelType.simple,size=Capacity),colour='black',shape=21,alpha=.7)+
		scale_fill_manual(name='Fuel Type',values=c('Nuclear'='#bebada','Coal'='#d9d9d9','Hydro'='#80b1d3','NaturalGas'='#fb8072','Biomass'='#b3de69','Geothermal'='#fdb462','Other'='#fccde5','Oil'='#bc80bd','Wind'='#8dd3c7','Solar'='#ffffb3'))+
		scale_size_continuous(name='Capacity [MW]')+
		geom_sf_text(data=wecc.centroids,aes(label=simple),size=5)+
		coord_sf()+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())+
		guides(fill=guide_legend(override.aes=list(size=10)))
	ggsave(plotSave,file='figures/generator_map.pdf',height=7,width=9)
	ggsave(plotSave,file='figures/generator_map.png',height=7,width=9)	
}

##################################################################################
# Plotting outputs 																 #
##################################################################################
# Generation mix (annual bar: fill by fuel, facet by region)
# Example of dispatch, customizable # days (area: facet by region; separate years)
# Example of detailed dispatch, 10 days with load/export/import/h2prod details (area: facet by region, lines for demand side)
# Total curtailment in WECC by hour (line: color by year)
# Transmission map (sf, arrow lines: total amount and saturation of capacity)
# Total transmission, CA imports (bar: total by year)
# Capacity factor (line, ribbon: facet by fuel)
# New capacity installed (line/bar: capacity each year, facet by technology)
# Cumulative capacity (bar: cum capacity by year, facet by technology)
# Hourly PEM operation (line: facet by year)
# Hourly storage SOC (line: facet by year)
# Hourly utilization of PEM (bar: fill by type by year, facet by region)
# Hourly marginal generation costs (line: color by region, facet by year)
# Hourly average emissions factor (line: facet by year)
# Map of total annual pollutant emissions, avg hourly emissions (sf, point: color by pollutant, separate maps by year)
###################################################################################

parseGeneration <- function(input) {
	generation <- input$generation
	output <- merge(x=generation[,.(g=as.numeric(g),t=as.numeric(t),value,year)],y=generators.aggregated[,.(g,Capacity)],by='g')
	output[,CF:=value/Capacity]
	output <- merge(x=output[,.(g,t,CF,year)],y=generators.other[,.(g,r=RegionSimple,FuelType,FuelCostTotal,Capacity,LAT,LON,PLCO2RTA,PLNOXRTA,PLSO2RTA,PLN2ORTA,PLCH4RTA)],by='g',all.x=TRUE,allow.cartesian=TRUE)
	output[,generation:=CF*Capacity]

	solar.totalCap <- rbind(input$solarCap,input$solarNew)[,.(Capacity=sum(value)),by=.(r,year)]
	solar.generation <- merge(x=input$solarCF[,.(r,t=as.numeric(t),CF=value,year)],y=solar.totalCap,by=c('r','year'),all.x=TRUE,allow.cartesian=TRUE)
	solar.generation[,generation:=Capacity*CF]
	solar.generation[,FuelType:='Solar']

	wind.totalCap <- rbind(input$windCap,input$windNew)[,.(Capacity=sum(value)),by=.(r,year)]
	wind.generation <- merge(x=input$windCF[,.(r,t=as.numeric(t),CF=value,year)],y=wind.totalCap,by=c('r','year'),all.x=TRUE,allow.cartesian=TRUE)
	wind.generation[,generation:=Capacity*CF]
	wind.generation[,FuelType:='Wind']

	output <- rbind(output,solar.generation,wind.generation,fill=TRUE)
	output <- merge(x=output,y=FuelType.key,by='FuelType')
	output$FuelType.simple <- factor(output$FuelType.simple,levels=c('Solar','Wind','Oil','Other','Geothermal','Biomass','NaturalGas','Hydro','Coal','Nuclear'))
	return(output)
}

plot.GenerationMix <- function(input,scenario) {
	forPlot <- input[,.(generation=sum(generation)/10^6),by=.(FuelType.simple,r,year)]

	plotSave <- ggplot(data=forPlot,aes(x=year,y=generation,fill=FuelType.simple))+
		geom_bar(stat='identity')+
		scale_fill_manual(name='Fuel Type',values=c('Nuclear'='#bebada','Coal'='#d9d9d9','Hydro'='#80b1d3','NaturalGas'='#fb8072','Biomass'='#b3de69','Geothermal'='#fdb462','Other'='#fccde5','Oil'='#bc80bd','Wind'='#8dd3c7','Solar'='#ffffb3'))+
		xlab('Year')+
		ylab('Annual Generation (TWh)')+
		theme_bw()+
		facet_wrap(r~.,scales='free_y')
	ggsave(plotSave,file=paste0('figures/grid_generationMix_',scenario,'.pdf'),height=8,width=12)
	ggsave(plotSave,file=paste0('figures/grid_generationMix_',scenario,'.png'),height=8,width=12)
}

plot.exampleDispatch <- function(input,days,scenario) {
	timePeriod <- ((min(days)-1)*24):(max(days)*24)
	forPlot <- input[t%in%timePeriod,.(generation=sum(generation)),by=.(t,r,year,FuelType.simple)]
	for(yr in unique(forPlot$year)) {
		plotSave <- ggplot(data=forPlot[year==yr],aes(x=t,y=generation/1000,fill=FuelType.simple))+
			geom_area()+
			scale_fill_manual(name='Fuel Type',values=c('Nuclear'='#bebada','Coal'='#d9d9d9','Hydro'='#80b1d3','NaturalGas'='#fb8072','Biomass'='#b3de69','Geothermal'='#fdb462','Other'='#fccde5','Oil'='#bc80bd','Wind'='#8dd3c7','Solar'='#ffffb3'))+
			xlab('Hour of the Year')+
			ylab('Generation (GW)')+
			theme_bw()+
			facet_wrap(r~.,scales='free_y')
		ggsave(plotSave,file=paste0('figures/dispatch_examples/dispatch_',scenario,'_yr',yr,'_days',min(days),'-',max(days),'.pdf'),height=8,width=12)
		ggsave(plotSave,file=paste0('figures/dispatch_examples/dispatch_',scenario,'_yr',yr,'_days',min(days),'-',max(days),'.png'),height=8,width=12)
	}
}

plot.detailedDispatch <- function(input,input.gen,days,scenario) {
	timePeriod <- ((min(days)-1)*24):(max(days)*24)
	
	# Supply-side
	forPlot.gen <- input.gen[t%in%timePeriod,.(generation=sum(generation)),by=.(t,r,year,Source=FuelType.simple)]
	forPlot.imports <- input$trans[as.numeric(t)%in%timePeriod,.(generation=sum(value),Source='Imports'),by=.(t=as.numeric(t),r=o,year)]
	forPlot.storOut <- input$storOut[as.numeric(t)%in%timePeriod,.(t=as.numeric(t),r,year,generation=value*.025,Source='H2 CT')]

	forPlot.supply <- rbind(forPlot.gen,forPlot.imports,forPlot.storOut)
	forPlot.supply$Source <- factor(forPlot.supply$Source,levels=c('Imports','H2 CT','Solar','Wind','Oil','Other','Geothermal','Biomass','NaturalGas','Hydro','Coal','Nuclear'))

	missing.vals <- CJ(t=timePeriod,r=unique(forPlot.supply$r),year=unique(forPlot.supply$year),Source=unique(forPlot.supply$Source))
	forPlot.supply <- merge(x=forPlot.supply,y=missing.vals,by=c('t','r','year','Source'),all.y=TRUE)
	forPlot.supply[is.na(generation),generation:=0]

	# Demand-side
	forPlot.load <- input$demandLoad[as.numeric(t)%in%timePeriod,.(t=as.numeric(t),r,year,demand=value,Type='Load')]
	forPlot.exports <- input$trans[as.numeric(t)%in%timePeriod,.(exports=sum(value)),by=.(t=as.numeric(t),r,year)]
	forPlot.exports <- merge(x=forPlot.load,y=forPlot.exports,by=c('t','r','year'))
	forPlot.exports <- forPlot.exports[,.(t,r,year,demand=demand+exports,Type='Load+Exports')]

	storIn <- input$storIn
	storIn$t <- as.numeric(storIn$t)
	freeH2 <- input$freeH2
	freeH2$t <- as.numeric(freeH2$t)
	forPlot.pem <- merge(x=storIn,y=freeH2,by=c('r','t','year'))
	forPlot.pem <- forPlot.pem[t%in%timePeriod,.(t,r,year,pem=value.x+value.y/18.2)]
	forPlot.pem <- merge(x=forPlot.exports,y=forPlot.pem,by=c('t','r','year'))
	forPlot.pem <- forPlot.pem[,.(t,r,year,demand=demand+pem,Type='Load+Exports+PEM')]

	forPlot.demand <- rbind(forPlot.load,forPlot.exports,forPlot.pem)

	for(yr in unique(forPlot.supply$year)) {
		plotSave <- ggplot()+
			geom_area(data=forPlot.supply[year==yr],aes(x=t,y=generation/1000,fill=Source))+
			scale_fill_manual(name='Supply Source',values=c('Nuclear'='#bebada','Coal'='#d9d9d9','Hydro'='#80b1d3','NaturalGas'='#fb8072','Biomass'='#b3de69','Geothermal'='#fdb462','Other'='#fccde5','Oil'='#bc80bd','Wind'='#8dd3c7','Solar'='#ffffb3','H2 CT'='#1f78b4','Imports'='firebrick'))+
			geom_line(data=forPlot.demand[year==yr],aes(x=t,y=demand/1000,linetype=Type))+
			scale_linetype_manual(name='Demand Source',values=c('dotted','longdash','solid'))+
			xlab('Hour of the Year')+
			ylab('Generation (GW)')+
			theme_bw()+
			facet_wrap(r~.,scales='free_y')+
			theme(legend.position='bottom')
		ggsave(plotSave,file=paste0('figures/dispatch_examples/detailedDispatch_',scenario,'_yr',yr,'_days',min(days),'-',max(days),'.pdf'),height=8,width=12)
		ggsave(plotSave,file=paste0('figures/dispatch_examples/detailedDispatch_',scenario,'_yr',yr,'_days',min(days),'-',max(days),'.png'),height=8,width=12)
	}
}

plot.curtailment <- function(input,input.generation,scenario) {
	total.transmissionLoss <- input$trans[,.(trans.loss=sum(value)*(1-.972)),by=.(t=as.numeric(t),year)]
	total.generation <- input.generation[,.(generation=sum(generation)),by=.(t,year)]
	total.h2ct <- input$storOut[,.(h2CT=sum(value)*.025),by=.(t=as.numeric(t),year)]

	total.demand.electricity <- input$demandLoad[,.(demand.load=sum(value)),by=.(t=as.numeric(t),year)]
	total.demand.h2 <- input$storIn[,.(demand.h2.load=sum(value)),by=.(t=as.numeric(t),year)]
	total.demand.freeH2 <- input$freeH2[,.(demand.freeH2.load=sum(value)/18.2),by=.(t=as.numeric(t),year)]

	forPlot <- merge(x=total.transmissionLoss,y=total.generation,by=c('t','year'))
	forPlot <- merge(x=forPlot,y=total.h2ct,by=c('t','year'))
	forPlot <- merge(x=forPlot,y=total.demand.electricity,by=c('t','year'))
	forPlot <- merge(x=forPlot,y=total.demand.h2,by=c('t','year'))
	forPlot <- merge(x=forPlot,y=total.demand.freeH2,by=c('t','year'))
	forPlot[,curtailment:=generation+h2CT-trans.loss-demand.load-demand.h2.load-demand.freeH2.load]

	plotSave <- ggplot(data=forPlot[curtailment>=0],aes(x=t,y=curtailment/1000))+
		geom_line()+
		xlab('Hour of the year')+
		ylab('Excess Generation in WECC (GW)')+
		theme_bw()+
		facet_wrap(year~.)
	ggsave(plotSave,file=paste0('figures/curtailment_hourly_',scenario,'.pdf'),height=8,width=12)
	ggsave(plotSave,file=paste0('figures/curtailment_hourly_',scenario,'.png'),height=8,width=12)

	total.curtailment <- forPlot[,.(curtailment=sum(curtailment)),by=year]
	plotSave <- ggplot(data=total.curtailment,aes(x=year,y=curtailment/10^6))+
		geom_bar(stat='identity')+
		xlab('Year')+
		ylab('Total WECC Curtailment (TWh)')+
		theme_bw()
	ggsave(plotSave,file=paste0('figures/curtailment_total_',scenario,'.pdf'),height=6,width=9)
	ggsave(plotSave,file=paste0('figures/curtailment_total_',scenario,'.png'),height=6,width=9)
}

plot.transmission <- function(input,scenario) {
	total.transmission <- input$trans[value>0,.(transmission=sum(value)),by=.(r,o,year)]
	total.transmission <- merge(x=total.transmission,y=input$transCap[,.(r,o,transCap=value,year)],by=c('r','o','year'))
	total.transmission[,saturation:=transmission/(transCap*8760)]

	wecc.centroids.1 <- wecc.centroids[rep(seq_len(nrow(wecc.centroids)),each=nrow(wecc.centroids)),]
	names(wecc.centroids.1) <- c('from','from.geometry')
	st_geometry(wecc.centroids.1) <- 'from.geometry'
	wecc.centroids.2 <- do.call('rbind',replicate(nrow(wecc.centroids),wecc.centroids,simplify=FALSE))
	names(wecc.centroids.2) <- c('to','to.geometry')
	st_geometry(wecc.centroids.2) <- 'to.geometry'
	wecc.centroid.pairs <- cbind(wecc.centroids.1,wecc.centroids.2)
	wecc.centroid.lines <- lapply(X=1:nrow(wecc.centroid.pairs),FUN=function(x) {
		pair <- st_combine(c(wecc.centroid.pairs$to.geometry[x],wecc.centroid.pairs$from.geometry[x]))
		line <- st_cast(pair,'LINESTRING')
		return(line)
	})
	wecc.centroid.pairs <- cbind(wecc.centroid.pairs,do.call(c,wecc.centroid.lines))
	st_geometry(wecc.centroid.pairs) <- 'geometry'
	wecc.centroid.pairs <- wecc.centroid.pairs[,c(1,2,5)]

	for(yr in unique(total.transmission$year)) {
		forPlot <- merge(x=total.transmission[year==yr],y=wecc.centroid.pairs,by.x=c('r','o'),by.y=c('from','to'))
		forPlot <- st_as_sf(forPlot)

		line.midpoints <- st_line_midpoints(forPlot)
		line.midpoints <- cbind(line.midpoints,forPlot[,c('r','o','transmission','transCap','saturation')])
		line.midpoints$label <- paste0(line.midpoints$r,'/',line.midpoints$o,': ',round(line.midpoints$transmission/10^6,2),' TWh (',round(line.midpoints$saturation*100),'%)')
		line.midpoints$nudge.x <- 0
		line.midpoints$nudge.y <- 30000
		for(row in 1:nrow(line.midpoints)) {
			if(row>1) {
				for(sub.row in 1:(row-1)) {
					if(line.midpoints$r[sub.row]==line.midpoints$o[row]&line.midpoints$o[sub.row]==line.midpoints$r[row]) {
						line.midpoints$nudge.y[row] <- -30000
					}
				}
			}
		}
		plotSave <- ggplot()+
			geom_sf(data=wecc.simple,fill=NA)+
			geom_sf(data=forPlot)+
			geom_sf_text(data=wecc.centroids,aes(label=simple),size=5)+
			geom_sf_text(data=line.midpoints,aes(label=label),position=position_nudge(y=line.midpoints$nudge.y),size=3)+
			coord_sf()+
			theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
		ggsave(plotSave,file=paste0('figures/transmission_map_',scenario,'_',yr,'.pdf'),height=8,width=8)
		ggsave(plotSave,file=paste0('figures/transmission_map_',scenario,'_',yr,'.png'),height=8,width=8)
	}
}

plot.transmission.ca.imports <- function(input,scenario) {
	trans.ca.imports <- input$trans[o%in%ca.regions]
	trans.ca.imports <- trans.ca.imports[,.(trans=sum(value)),by=.(r,o,year)]

	plotSave <- ggplot(data=trans.ca.imports,aes(x=year,y=trans/10^6,fill=r))+
		geom_bar(stat='identity',position='dodge')+
		xlab('Year')+
		ylab('Annual electricity imported into CA Region (TWh)')+
		theme_bw()+
		facet_wrap(o~.,scales='free_y')
	ggsave(plotSave,file=paste0('figures/transmission_ca_imports_',scenario,'.pdf'),height=6,width=9)
	ggsave(plotSave,file=paste0('figures/transmission_ca_imports_',scenario,'.png'),height=6,width=9)
}

plot.renewableCF <- function(input) {
	forPlot.solarCF <- input$solarCF[year==2025,.(hour=as.numeric(t)%%24,r,value)]
	forPlot.solarCF <- forPlot.solarCF[,.(cf.mean=mean(value),cf.5thP=quantile(value,.05),cf.95thP=quantile(value,.95)),by=.(hour,r)]
	forPlot.windCF <- input$windCF[year==2025,.(hour=as.numeric(t)%%24,r,value)]
	forPlot.windCF <- forPlot.windCF[,.(cf.mean=mean(value),cf.5thP=quantile(value,.05),cf.95thP=quantile(value,.95)),by=.(hour,r)]
	plotSave <- ggplot(data=forPlot.solarCF,aes(x=hour))+
		geom_ribbon(aes(ymin=cf.5thP,ymax=cf.95thP),fill='yellow',alpha=.6)+
		geom_line(aes(y=cf.mean),color='orange',size=2)+
		xlab('Hour of the Day')+
		ylab('Annual Average Solar Capacity Factor')+
		theme_bw()+
		facet_wrap(r~.)
	ggsave(plotSave,file='figures/capacityFactor_solar.pdf',height=8,width=12)
	ggsave(plotSave,file='figures/capacityFactor_solar.png',height=8,width=12)

	plotSave <- ggplot(data=forPlot.windCF,aes(x=hour))+
		geom_ribbon(aes(ymin=cf.5thP,ymax=cf.95thP),fill='skyblue',alpha=.6)+
		geom_line(aes(y=cf.mean),color='dodgerblue4',size=2)+
		xlab('Hour of the Day')+
		ylab('Annual Average Solar Capacity Factor')+
		theme_bw()+
		facet_wrap(r~.)
	ggsave(plotSave,file='figures/capacityFactor_wind.pdf',height=8,width=12)
	ggsave(plotSave,file='figures/capacityFactor_wind.png',height=8,width=12)
}

plot.capacityExpansion <- function(input,scenario) {
	solar.new <- input$solarNew
	solar.new$tech <- 'Solar (GW)'
	wind.new <- input$windNew
	wind.new$tech <- 'Wind (GW)'
	pem.new <- input$pemCap
	pem.new <- pem.new[,.(value=value-lag(value),year),by=r]
	pem.new$tech <- 'PEM (GW)'
	stor.new <- input$storCap
	stor.new <- stor.new[,.(value=value-lag(value),year),by=r]
	stor.new$tech <- 'H2 Storage (tonnes)'

	forPlot <- rbind(solar.new,wind.new,pem.new,stor.new)
	plotSave <- ggplot(data=forPlot,aes(x=year,y=value/1000,fill=r,group=r))+
		geom_bar(stat='identity',position='dodge')+
		scale_fill_discrete(name='Region')+
		xlab('Year')+
		ylab('New capacity installed (GW or tonnes)')+
		theme_bw()+
		facet_wrap(tech~.,scales='free_y')
	ggsave(plotSave,file=paste0('figures/capacityExpansion_',scenario,'.pdf'),height=6,width=9)
	ggsave(plotSave,file=paste0('figures/capacityExpansion_',scenario,'.png'),height=6,width=9)
}

plot.cumulative.capacity <- function(input,scenario) {
	solar.total <- rbind(input$solarCap,input$solarNew)
	solar.total <- solar.total[,.(value=sum(value)),by=.(r,year)]
	solar.total$tech <- 'Solar (GW)'
	wind.total <- rbind(input$windCap,input$windNew)
	wind.total <- wind.total[,.(value=sum(value)),by=.(r,year)]
	wind.total$tech <- 'Wind (GW)'
	pem.total <- input$pemCap
	pem.total$tech <- 'PEM (GW)'
	stor.total <- input$storCap
	stor.total$tech <- 'H2 Storage (tonnes)'

	forPlot <- rbind(solar.total,wind.total,pem.total,stor.total)
	plotSave <- ggplot(data=forPlot,aes(x=year,y=value/1000,fill=r,group=r))+
		geom_bar(stat='identity',position='dodge')+
		scale_fill_discrete(name='Region')+
		xlab('Year')+
		ylab('Cumulative Capacity (GW or tonnes)')+
		theme_bw()+
		facet_wrap(tech~.,scales='free_y')
	ggsave(plotSave,file=paste0('figures/capacity_total_',scenario,'.pdf'),height=6,width=9)
	ggsave(plotSave,file=paste0('figures/capacity_total_',scenario,'.png'),height=6,width=9)
}

plot.pemOperation.annual <- function(input,scenario) {
	storIn <- input$storIn
	storIn$t <- as.numeric(storIn$t)
	forPlot <- storIn[,.(pem=sum(value)),by=.(year,t)]

	plotSave <- ggplot(data=forPlot,aes(x=t,y=pem))+
		geom_line()+
		xlab('Hour of the Year')+
		ylab('Electolyzer operation (MW)')+
		theme_bw()+
		facet_wrap(year~.)
	ggsave(plotSave,file=paste0('figures/pem_operation_',scenario,'.pdf'),height=6,width=9)
	ggsave(plotSave,file=paste0('figures/pem_operation_',scenario,'.png'),height=6,width=9)
}

plot.storOperation.annual <- function(input,scenario) {
	storSOC <- input$storSOC
	storSOC$t <- as.numeric(storSOC$t)

	for(yr in unique(storSOC$year)) {
		plotSave <- ggplot(data=storSOC[year==yr],aes(x=t,y=value/10^6))+
			geom_line()+
			xlab('Hour of the Year')+
			ylab('Storage Use (thousands of tonnes)')+
			theme_bw()+
			facet_wrap(r~.,scales='free_y')
		ggsave(plotSave,file=paste0('figures/storSOC_',scenario,'_',yr,'.pdf'),height=8,width=12)
		ggsave(plotSave,file=paste0('figures/storSOC_',scenario,'_',yr,'.png'),height=8,width=12)
	}
}

plot.h2EndUse <- function(input,scenario) {
	h2.combustion <- input$storOut[,.(end.use=sum(value),type='Combustion'),by=.(r,year)]
	h2.transport <- input$h2Demand[,.(end.use=sum(value),type='Transportation'),by=.(r,year)]
	h2.stationary <- input$h2Stationary[,.(end.use=sum(value),type='Stationary'),by=.(r,year)]
	forPlot <- rbind(h2.combustion,h2.transport,h2.stationary)

	plotSave <- ggplot(data=forPlot,aes(x=year,y=end.use/10^6,fill=type))+
		geom_bar(stat='identity',position='stack')+
		scale_fill_discrete(name='H2 End Use')+
		xlab('Year')+
		ylab('H2 Utilization\n(thousands of tonnes)')+
		theme_bw()+
		facet_wrap(r~.)
	ggsave(plotSave,file=paste0('figures/h2_enduse_',scenario,'.pdf'),height=8,width=12)
	ggsave(plotSave,file=paste0('figures/h2_enduse_',scenario,'.png'),height=8,width=12)
}

plot.genCosts.avgHourly <- function(input.gen,scenario) {
	forPlot <- input.gen[generation>0,.(Cost=max(FuelCostTotal,na.rm=TRUE)),by=.(t,r,year)]

	plotSave <- ggplot(data=forPlot,aes(x=t,y=Cost,color=r))+
		geom_line()+
		scale_color_discrete(name='Region')+
		xlab('Hour of the Year')+
		ylab('Cost of Generation ($/MWh)')+
		theme_bw()+
		guides(colour=guide_legend(override.aes=list(size=3)))+
		facet_wrap(year~.)
	ggsave(plotSave,file=paste0('figures/genCost_',scenario,'.pdf'),height=8,width=12)
	ggsave(plotSave,file=paste0('figures/genCost_',scenario,'.png'),height=8,width=12)
}

plot.emissionsFactor.avgHourly <- function() {
	
}

plot.pollutantEmissions.map <- function() {
	
}

plot.hydrogenCT <- function(input,input.gen,scenario) {
	hydrogenCT <- input$storOut
	hydrogenCT$generation <- hydrogenCT$value*.025
	hydrogenCT$t <- as.numeric(hydrogenCT$t)
	hydrogenCT$tech <- 'Hydrogen CT'
	gasCT <- input.gen[FuelType=='NaturalGas',.(generation=sum(generation)),by=.(t,year,r)]
	gasCT$tech <- 'Natural Gas CT'
	totalCT.capacity <- input.gen[FuelType=='NaturalGas'&t==1,.(Capacity=sum(Capacity)),by=.(year,r)]

	forPlot <- rbind(hydrogenCT[,.(t,year,r,generation,tech)],gasCT)
	for(yr in unique(forPlot$year)) {
		plotSave <- ggplot(data=forPlot[year==yr],aes(x=t,y=generation/1000,fill=tech))+
			geom_area()+
			scale_fill_discrete(name='CT Type')+
			geom_hline(data=totalCT.capacity[year==yr],aes(yintercept=Capacity/1000))+
			xlab('Hour of the Year')+
			ylab('CT Generation (GW)')+
			theme_bw()+
			facet_wrap(r~.,scales='free_y')
		ggsave(plotSave,file=paste0('figures/ct_type_',scenario,'_',yr,'.pdf'),height=8,width=12)
		ggsave(plotSave,file=paste0('figures/ct_type_',scenario,'_',yr,'.png'),height=8,width=12)
	}
}

write.Results <- function(input,input.gen,scenarioLabel) {
	gen.loc <- unique(rbind(generators.other[,.(LAT,LON,RegionName)],generators.solar.wind[,.(LAT,LON,RegionName)]))
	gen.loc <- gen.loc[!duplicated(gen.loc[,.(LAT,LON)]),]
	gen.all <- merge(x=input.gen,y=gen.loc,by=c('LAT','LON'),all.x=TRUE)
	gen.all[FuelType%in%c('Solar','Wind'),FuelCostTotal:=0]

	proportions <- gen.all[!is.na(RegionName),.(generation=sum(generation,na.rm=TRUE)),by=.(year,r,RegionName)]
	proportions[,total.gen:=sum(generation),by=.(year,r)]
	proportions[,fraction:=generation/total.gen]

	wholesale.price <- gen.all[!is.na(RegionName)&generation>0,.(WholesalePrice=max(FuelCostTotal,na.rm=TRUE)),by=.(t,year,r=RegionName)]
	fwrite(wholesale.price,file=paste0('results/wholesale_prices_',scenarioLabel,'.csv'),row.names=FALSE)

	pemCap <- merge(x=input$pemCap,y=proportions,by=c('r','year'))
	pemCap <- pemCap[,.(r=RegionName,year,pemCap=value*fraction)]
	fwrite(pemCap,file=paste0('results/pemCap_',scenarioLabel,'.csv'),row.names=FALSE)

	storCap <- merge(x=input$storCap,y=proportions,by=c('r','year'))
	storCap <- storCap[,.(r=RegionName,year,storCap=value*fraction)]
	fwrite(storCap,file=paste0('results/storCap_',scenarioLabel,'.csv'),row.names=FALSE)

	fuelH2 <- merge(x=input$fuelH2,y=proportions,by=c('r','year'),all.x=TRUE,allow.cartesian=TRUE)
	fuelH2 <- fuelH2[,.(r=RegionName,year,t,fuelH2=value*fraction)]
	fwrite(fuelH2,file=paste0('results/fuelH2_',scenarioLabel,'.csv'),row.names=FALSE)

	freeH2 <- merge(x=input$freeH2,y=proportions,by=c('r','year'),all.x=TRUE,allow.cartesian=TRUE)
	freeH2 <- freeH2[,.(r=RegionName,year,t,fuelH2=value*fraction)]
	fwrite(freeH2,file=paste0('results/freeH2_',scenarioLabel,'.csv'),row.names=FALSE)

	h2CT <- merge(x=input$storOut,y=proportions,by=c('r','year'),all.x=TRUE,allow.cartesian=TRUE)
	h2CT <- h2CT[,.(r=RegionName,year,t,h2CT=value*fraction)]
	fwrite(h2CT,file=paste0('results/h2CT_',scenarioLabel,'.csv'),row.names=FALSE)
}

plot.all <- function(scenario,scenarioLabel) {
	scenario.run <- fetchOutputs.allYears(scenario)
	scenario.run.gen <- parseGeneration(scenario.run)

	write.Results(scenario.run,scenario.run.gen,scenarioLabel)
	plot.renewableCF(scenario.run)
	plot.GenerationMix(scenario.run.gen,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,78:80,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,170:172,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,264:266,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,354:356,scenarioLabel)
	plot.detailedDispatch(scenario.run,scenario.run.gen,74:84,scenarioLabel)
	plot.detailedDispatch(scenario.run,scenario.run.gen,166:176,scenarioLabel)
	plot.detailedDispatch(scenario.run,scenario.run.gen,260:270,scenarioLabel)
	plot.detailedDispatch(scenario.run,scenario.run.gen,350:360,scenarioLabel)
	plot.curtailment(scenario.run,scenario.run.gen,scenarioLabel)
	plot.transmission(scenario.run,scenarioLabel)
	plot.transmission.ca.imports(scenario.run,scenarioLabel)
	plot.capacityExpansion(scenario.run,scenarioLabel)
	plot.cumulative.capacity(scenario.run,scenarioLabel)
	plot.pemOperation.annual(scenario.run,scenarioLabel)
	plot.storOperation.annual(scenario.run,scenarioLabel)
	plot.h2EndUse(scenario.run,scenarioLabel)
	plot.genCosts.avgHourly(scenario.run.gen,scenarioLabel)
	plot.hydrogenCT(scenario.run,scenario.run.gen,scenarioLabel)
}

plot.all('highDemand_1-365','highDemand')
plot.all('lowDemand_1-365','lowDemand')

plot.all('highDemand_noCurtail_1-365','highDemand_noCurtail')
plot.all('highDemand_noH2Demand_1-365','highDemand_noH2Demand')
plot.all('highDemand_noCurtail_noH2Demand_1-365','highDemand_noCurtail_noH2Demand')
plot.all('highDemand_lowH2cost_1-365','highDemand_lowH2cost')
plot.all('highDemand_highRenewCost_1-365','highDemand_highRenewCost')
plot.all('highDemand_lowH2cost_highRenewCost_1-365','highDemand_lowH2cost_highRenewCost')
plot.all('highDemand_rpsAll_1-365','highDemand_rpsAll')
plot.all('highDemand_rps100_1-365','highDemand_rps100')
