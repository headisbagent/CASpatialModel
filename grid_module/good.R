### Comments Here ###

#####################

source('base.R')
source('prep-inputs-grid.R')
source('prep-inputs-mobility.R')

### Pre-process inputs for all scenarios ###
inputs <- list()
inputs$sets <- list()
inputs$parameters <- list()
t <- 1:(24*365)
d <- 1:365
ttod <- data.table(t=t,d=rep(d,each=24))

runScenario <- function(yr,scenario,scenarioDemand,dInput,storCostInput,pemCostInput,rpsOption,gmsScenarioFileName) {
	scenarioName <- paste0(yr,'_',scenario,'_',min(dInput),'-',max(dInput))
	dir.create(paste('runFiles/',scenarioName,sep=''),showWarnings=FALSE)

	tInput <- ttod$t[ttod$d%in%dInput]
	time.sets <- list(t=t[t%in%tInput],d=d[d%in%dInput],ttod=ttod[t%in%tInput])

	inputs.grid <- prep.inputs.grid(tInput,yr,scenario,rpsOption)
	inputs.mobility <- get.dailyDemand(scenarioDemand,dInput)
	inputs$sets <- c(time.sets,inputs.grid$sets)
	inputs$parameters <- c(inputs.mobility$parameters,inputs.grid$parameters)
	write.gdx(paste0('runFiles/',scenarioName,'/inputs.gdx',sep=''),params=lapply(inputs$parameters,as.data.frame,stringsAsFactors=F),sets=lapply(inputs$sets,as.data.frame,stringsAsFactors=F))

	good.gms <- readLines(gmsScenarioFileName)
	good.gms <- gsub(pattern='<<storCostValue>>',replace=storCostInput,x=good.gms)
	good.gms <- gsub(pattern='<<pemCostValue>>',replace=pemCostInput,x=good.gms)
	writeLines(good.gms,con=paste('runFiles/',scenarioName,'/good.gms',sep=''))

	startTime <- proc.time()
	print(paste0('Starting run: ',scenarioName))
	setwd(paste0('runFiles/',scenarioName))
	gams('good.gms')
	setwd('../..')
	print(proc.time()-startTime)
	print('Model results:')
	print.lst.status(paste0('runFiles/',scenarioName,'/good.lst'))
}

# If you want to run/test a single year run, syntax is as follows:
# runScenario(2025,'lowDemand_ctH2','2025_base',1:365,storCostInput=.2,pemCostInput=30000,'baseRPS','good.gms')

# Note there are three RPS options:
# 'rpsBase' - baseline RPS with existing RPS requirements, 'rpsAll' - all regions have identical RPS requirements to CA, 'rps100' - CA now has RPS requirements that go all the way to 100%
runScenario.allYears <- function(scenario,scenarioDemand,storCostInput,pemCostInput,rpsOption,gmsScenarioFileName) {
	for(yr in seq(2025,2050,by=5)) {
		runScenario(yr,scenario,paste0(yr,'_',scenarioDemand),1:365,storCostInput,pemCostInput,rpsOption,gmsScenarioFileName)
	}
}

# If you want to run across all years, syntax is as follows:
# (note that scenarioDemand is listed as either "base" or "high" for low and high H2 demand respectively)
# runScenario.allYears('lowDemand','base',storCostInput=.2,pemCostInput=30000,'baseRPS','good.gms')
