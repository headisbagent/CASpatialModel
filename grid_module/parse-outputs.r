source('base.R')
source('prep-inputs-grid.R')
source('prep-inputs-mobility.R')

ca.regions <- c('CA_N','CA_LA','CA_SD')

fetchOutputs <- function(scenario,yr) {
	scenario.name <- paste(yr,scenario,sep='_')

	variable.names <- c('generation','trans','solarNew','windNew','storSOC','storIn','storOut','storCap','pemCap','fuelH2','genCost','demandLoad','maxGen','solarCap','windCap','solarCF','windCF','transCap','transCost','h2Demand')

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
	names(output) <- c('generation','trans','solarNew','windNew','storSOC','storIn','storOut','storCap','pemCap','fuelH2','genCost','demandLoad','maxGen','solarCap','windCap','solarCF','windCF','transCap','transCost','h2Demand')
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

	forPlot.wecc <- st_transform(wecc.simple,crs=4269)

	plotSave <- ggplot()+
		geom_sf(data=forPlot.wecc,fill=NA)+
		geom_sf(data=forPlot,aes(color=FuelType,size=Capacity))+
		geom_sf_text(data=wecc.centroids,aes(label=simple),size=5)+
		coord_sf()+
		theme_bw()%+replace% theme(plot.background=element_blank(),panel.background=element_blank(),panel.border=element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),axis.title=element_blank())
	ggsave(plotSave,file='figures/generator_map.pdf',height=7,width=9)
	ggsave(plotSave,file='figures/generator_map.png',height=7,width=9)	
}

##################################################################################
# Plotting outputs 																 #
##################################################################################
# 1. Generation mix (annual bar: fill by fuel, facet by region)
# 2. Example of dispatch, 1 week (area: facet by region; separate years)
# 3. Total curtailment in WECC by hour (line: color by year)
# 4. Transmission map (sf, arrow lines: total amount and saturation of capacity)
# 5. Total transmission, CA imports (bar: total by year)
# 6. Capacity factor (line, ribbon: facet by fuel)
# 7. New capacity installed (line/bar: capacity each year, facet by technology)
# 8. Cumulative capacity (bar: cum capacity by year, facet by technology)
# 9. Hourly PEM operation (line: facet by year)
# 10. Hourly storage SOC (line: facet by year)
# 11. Hourly utilization of PEM (bar: fill by type by year, facet by region)
# 12. Hourly marginal generation costs (line: color by region, facet by year)
# 13. Hourly average emissions factor (line: facet by year)
# 14. Map of total annual pollutant emissions, avg hourly emissions (sf, point: color by pollutant, separate maps by year)
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
	return(output)
}

plot.GenerationMix <- function(input,scenario) {
	forPlot <- input[,.(generation=sum(generation)/10^6),by=.(FuelType,r,year)]
	plotSave <- ggplot(data=forPlot,aes(x=year,y=generation,fill=FuelType))+
		geom_bar(stat='identity')+
		xlab('Year')+
		ylab('Annual Generation (TWh)')+
		theme_bw()+
		facet_wrap(r~.,scales='free_y')
	ggsave(plotSave,file=paste0('figures/grid_generationMix_',scenario,'.pdf'),height=8,width=12)
	ggsave(plotSave,file=paste0('figures/grid_generationMix_',scenario,'.png'),height=8,width=12)
}

plot.exampleDispatch <- function(input,days,scenario) {
	timePeriod <- ((min(days)-1)*24):(max(days)*24)
	forPlot <- input[t%in%timePeriod,.(generation=sum(generation)),by=.(t,r,year,FuelType)]
	for(yr in unique(forPlot$year)) {
		plotSave <- ggplot(data=forPlot[year==yr],aes(x=t,y=generation/1000,fill=FuelType))+
			geom_area()+
			xlab('Hour of the Year')+
			ylab('Generation (GW)')+
			theme_bw()+
			facet_wrap(r~.,scales='free_y')
		ggsave(plotSave,file=paste0('figures/dispatch_examples/dispatch_',scenario,'_yr',yr,'_days',min(days),'-',max(days),'.pdf'),height=8,width=12)
		ggsave(plotSave,file=paste0('figures/dispatch_examples/dispatch_',scenario,'_yr',yr,'_days',min(days),'-',max(days),'.png'),height=8,width=12)
	}
	
}

plot.curtailment <- function(input,input.generation,scenario) {
	total.transmissionLoss <- input$trans[,.(trans.loss=sum(value)*(1-.972)),by=.(t=as.numeric(t),year)]
	total.generation <- input.generation[,.(generation=sum(generation)),by=.(t,year)]
	total.demand.electricity <- input$demandLoad[,.(demand.load=sum(value)),by=.(t=as.numeric(t),year)]
	total.demand.h2 <- input$fuelH2[,.(demand.h2.load=sum(value)/18.2),by=.(t=as.numeric(t),year)]

	forPlot <- merge(x=total.transmissionLoss,y=total.generation,by=c('t','year'))
	forPlot <- merge(x=forPlot,y=total.demand.electricity,by=c('t','year'))
	forPlot <- merge(x=forPlot,y=total.demand.h2,by=c('t','year'))
	forPlot[,curtailment:=generation-trans.loss-demand.load-demand.h2.load]

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
	fuelH2 <- input$fuelH2
	fuelH2$t <- as.numeric(fuelH2$t)
	
	forPlot <- merge(x=storIn,y=fuelH2,by=c('r','t','year'))
	forPlot[,pem:=value.x+value.y/18.2]
	forPlot <- forPlot[,.(pem=sum(pem)),by=.(year,t)]

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

plot.utilizationPEM <- function(input,scenario) {
	storIn <- input$storIn[,.(production=sum(value),type='H2 Storage'),by=.(r,year)]
	fuelH2 <- input$fuelH2[,.(production=sum(value)/18.2,type='H2 Fuel'),by=.(r,year)]
	forPlot <- rbind(storIn,fuelH2)
	forPlot <- merge(x=forPlot,y=input$pemCap,by=c('r','year'))
	forPlot <- forPlot[value>0]
	forPlot[,Utilization:=production/(value*8760)]

	plotSave <- ggplot(data=forPlot,aes(x=year,y=Utilization,fill=type))+
		geom_bar(stat='identity',position='stack')+
		xlab('Year')+
		ylab('PEM Utilization')+
		theme_bw()+
		facet_wrap(r~.)
	ggsave(plotSave,file=paste0('figures/pem_utilization_',scenario,'.pdf'),height=8,width=12)
	ggsave(plotSave,file=paste0('figures/pem_utilization_',scenario,'.png'),height=8,width=12)
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

plot.all <- function(scenario,scenarioLabel) {
	scenario.run <- fetchOutputs.allYears(scenario)
	scenario.run.gen <- parseGeneration(scenario.run)

	plot.GenerationMix(scenario.run.gen,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,78:80,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,170:172,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,264:266,scenarioLabel)
	plot.exampleDispatch(scenario.run.gen,354:356,scenarioLabel)
	plot.curtailment(scenario.run,scenario.run.gen,scenarioLabel)
	plot.transmission(scenario.run,scenarioLabel)
	plot.transmission.ca.imports(scenario.run,scenarioLabel)
	plot.capacityExpansion(scenario.run,scenarioLabel)
	plot.cumulative.capacity(scenario.run,scenarioLabel)
	plot.pemOperation.annual(scenario.run,scenarioLabel)
	plot.storOperation.annual(scenario.run,scenarioLabel)
	plot.utilizationPEM(scenario.run,scenarioLabel)
	plot.genCosts.avgHourly(scenario.run.gen,scenarioLabel)
	plot.hydrogenCT(scenario.run,scenario.run.gen,scenarioLabel)
}

plot.all('highDemand_ctH2_1-365','highDemand')
plot.all('lowDemand_ctH2_1-365','lowDemand')
plot.all('highDemand_ctH2_lowCosts_1-365','highDemand_lowCost')
plot.all('highDemand_ctH2_noCurtail_1-365','highDemand_noCurtail')