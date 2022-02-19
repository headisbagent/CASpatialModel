source('base.r')


ld.comm.trips <- fread('../../CSTDM_OD/LDCVM/tazTrips2040.csv')
ld.comm.trips <- ld.comm.trips[,.(trips=sum(trips)),by=.(origin,destination)]

ld.priv.trips <- fread('../../CSTDM_OD/LDPTM/LDPTM_Trips.csv')
ld.priv.trips <- ld.priv.trips[Person==1,.(trips=.N),by=.(from=I,to=J)]

trips.ext <- fread('../../CSTDM_OD/ETM/trips_Ext.csv')
ld.comm.trips.ext <- trips.ext[ActorType%in%c('Heavy','Medium','Heavy_E-E'),.(trips=sum(Trip)),by=.(from=I,to=J)]
ld.priv.trips.ext <- trips.ext[ActorType%in%c('CarLong','CarLocal'),.(trips=sum(Trip)),by=.(from=I,to=J)]

routes.1 <- fread('datafiles/parsed_100000/parsed_100000_0.csv')
routes.2 <- fread('datafiles/parsed_100000/parsed_100000_1.csv')
routes.ext <- fread('datafiles/parsed_100000/parsed_100000_ext.csv')

all.routes <- rbind(routes.1,routes.2,routes.ext)
all.routes <- all.routes[,.(v=.GRP,`TAZ12,`,distance),by=.(from,to)]
# fwrite(all.routes,file='all_routes.csv',row.names=FALSE)

# start <- proc.time()
# similarity.matrix <- data.table()
# for(grp in unique(all.routes.50$v)) {
# 	comparison.group <- unique(all.routes.50[`TAZ12,`%in%all.routes[v==grp]$`TAZ12,`,]$v)
# 	hold <- data.table(v1=grp,v2=comparison.group)
# 	similarity.matrix <- rbind(similarity.matrix,hold)
# }
# proc.time()-start

# start <- proc.time()
# similarity.matrix <- similarity.matrix[,.(overlap=sum(all.routes.50[v==v1]$`TAZ12,`%in%all.routes.50[v==v2]$`TAZ12,`)),by=.(v1,v2)]
# proc.time()-start

# routes <- rbind(routes.1,routes.2)
# routes <- routes[,.(v=.GRP,`TAZ12,`,distance),by=.(from,to)]
# routes.ext <- routes.ext[,.(v=.GRP,`TAZ12,`,distance),by=.(from,to)]


ld.comm.routes.raw <- merge(x=all.routes,y=ld.comm.trips,by.x=c('from','to'),by.y=c('origin','destination'))
ld.comm.routes <- ld.comm.routes.raw[trips>4,]
ld.comm.routes[,h2:=distance/1000/1.6*.083]

ld.priv.routes.raw <- merge(x=all.routes,y=ld.priv.trips,by=c('from','to'))
ld.priv.routes <- ld.priv.routes.raw[trips>1]
ld.priv.routes[,h2:=distance/1000/1.6*.0122]

ld.comm.routes.ext.raw <- merge(x=all.routes,y=ld.comm.trips.ext,by=c('from','to'),all.x=TRUE)
ld.comm.routes.ext <- ld.comm.routes.ext.raw[trips>3,]
ld.comm.routes.ext[,h2:=distance/1000/1.6*.083]
ld.comm.routes.ext <- ld.comm.routes.ext[complete.cases(ld.comm.routes.ext)]

ld.priv.routes.ext.raw <- merge(x=all.routes,y=ld.priv.trips.ext,by=c('from','to'),all.x=TRUE)
ld.priv.routes.ext <- ld.priv.routes.ext.raw[trips>3,]
ld.priv.routes.ext[,h2:=distance/1000/1.6*.0122]
ld.priv.routes.ext <- ld.priv.routes.ext[complete.cases(ld.priv.routes.ext)]

all.ld <- rbind(ld.comm.routes,ld.priv.routes,ld.comm.routes.ext,ld.priv.routes.ext)
all.ld <- all.ld[,.(h2=sum(h2),distance=mean(distance)),by=.(from,to,v,`TAZ12,`)]
all.ld <- all.ld[distance>80000,]

include.v <- unique(all.ld$v)

# The code below clusters the routes among overlapping zones within each of the routes

	sm <- data.table()
	for(f in list.files('sm_output')) {
		print(f)
		hold <- fread(paste0('sm_output/',f))
		hold <- hold[overlap!=1,]
		sm <- rbind(sm,hold)
	}
	colnames(sm) <- c('i','j','value')
	sm <- sm[i%in%include.v&j%in%include.v,]
	#sm <- sm[!(i%in%exclude.v)&!(j%in%exclude.v)]

	cluster.all <- function() {
		sm.matrix <- data.table(dcast(sm,i~j,value.var='value'))
		sm.matrix <- as.matrix(sm.matrix,rownames='i')
		sm.matrix[is.na(sm.matrix)] <- 0
		sm.matrix <- sm.matrix*-1
		sm.matrix[upper.tri(sm.matrix,diag=TRUE)] <- NA
		sm.dist <- as.dist(sm.matrix)

		cluster.res <- hclust(sm.dist,method='complete')
		cluster.assignment <- cutree(cluster.res,500)
		cluster.assignment <- data.table(v=names(cluster.assignment),cluster=cluster.assignment)
		fwrite(cluster.assignment,file='cluster/cluster_all.csv',row.names=FALSE)
	}

	all.clusters <- fread('cluster/cluster_all.csv')

	route.overlap <- data.table()
	for(c in unique(all.clusters$cluster)) {
		v.hold <- all.clusters[cluster==c]$v
		taz.list <- list()
		for(vi in v.hold) {
			taz.hold <- all.routes[v==vi]$`TAZ12,`
			taz.list <- c(taz.list,list(taz.hold))
		}
		routes.hold <- Reduce(intersect,taz.list)
		overlap.hold <- data.table(v=rep(v.hold,each=length(routes.hold)),taz=rep(routes.hold,length(v.hold)))
		route.overlap <- rbind(route.overlap,overlap.hold)
	}

	fwrite(route.overlap,file='cluster/route_overlap.csv',row.names=FALSE)

ld.comm.all <- unique(rbind(ld.comm.routes,ld.comm.routes.ext)[,.(v,trips,h2)])
ld.comm.all <- ld.comm.all[v%in%unique(route.overlap$v)]
ld.comm.all[,prob:=trips/sum(trips)]
fwrite(ld.comm.all,file='inputs/ld_comm.csv',row.name=FALSE)

ld.priv.all <- unique(rbind(ld.priv.routes,ld.priv.routes.ext)[,.(v,trips,h2)])
ld.priv.all <- ld.priv.all[v%in%unique(route.overlap$v)]
ld.priv.all[,prob:=trips/sum(trips)]
fwrite(ld.priv.all,file='inputs/ld_priv.csv',row.name=FALSE)



buffer <- fread('datafiles/parsed_100000_TAZ_5Buffer/parsed_100000_0_flip.csv')

out <- data.table()
for(f in list.files('../../CSTDM_OD/SDPTM')){
	hold <- fread(paste0('../../CSTDM_OD/SDPTM/',f))
	out <- rbind(out,hold)
}
out <- out[Mode%in%c('HOV2','SOV','HOV3')]
out <- out[,.(Dist,HomeZone)]
sd.key <- data.table(HomeZone=unique(out$HomeZone))
sd.key[,v:=1:nrow(sd.key)+1000000]
out <- merge(x=out,y=sd.key,by='HomeZone')
out[,h2:=Dist*.0122]
fwrite(out,file='inputs/sd_priv.csv',row.names=FALSE)

buffer <- merge(x=buffer,y=sd.key,by.x='bUfferTAZ',by.y='HomeZone')
fwrite(buffer,file='inputs/buffer.csv',row.names=FALSE)

library(doSNOW)
runParallelProcess <- function(numberOfThreads,startIndex,increment=99) {
	buffer <- fread('inputs/buffer.csv')
	buffer <- buffer[,.(v,taz=`TAZ12,`)]

	generateSimilarityMatrix <- function(grp) {
		comparison.group <- unique(buffer$v)
		hold <- data.table(v1=grp,v2=comparison.group)
		hold[,overlap:=sum(buffer[v==v1]$taz%in%buffer[v==v2]$taz),by=.(v1,v2)]
		return(hold)
	}

	# Initializing parallelization process
	c1 <- makeCluster(numberOfThreads)
	registerDoSNOW(c1)
	clusterEvalQ(c1,c(library(data.table)))

	# Generating similarity matrix
	similarity.matrix <- foreach(i=unique(buffer$v)[startIndex:(startIndex+increment)]) %dopar% generateSimilarityMatrix(i)

	# Combining parallel runs
	final <- rbindlist(similarity.matrix)

	# Writing output to csv file
	dir.create('sm_output_buffer/',showWarnings=FALSE)
	fwrite(final,file=paste0('sm_output_buffer/similarityMatrix_',startIndex,'.csv'),row.names=FALSE)
}

for(index in seq(from=1,to=5400,by=100)) {
	start.time <- proc.time()
	print(index)
	runParallelProcess(6,index)
	gc()
	proc.time()-start.time
}

runParallelProcess(6,5401,10)


# The code below clusters the routes among overlapping zones within each of the routes

	sm.buffer <- data.table()
	for(f in list.files('sm_output_buffer')) {
		print(f)
		hold <- fread(paste0('sm_output_buffer/',f))
		hold <- hold[overlap!=1,]
		sm.buffer <- rbind(sm.buffer,hold)
	}
	colnames(sm.buffer) <- c('i','j','value')
	sm.buffer <- sm.buffer[i%in%include.v&j%in%include.v,]
	#sm <- sm[!(i%in%exclude.v)&!(j%in%exclude.v)]

	cluster.all.buffer <- function() {
		sm.matrix <- data.table(dcast(sm.buffer,i~j,value.var='value'))
		sm.matrix <- as.matrix(sm.matrix,rownames='i')
		sm.matrix[is.na(sm.matrix)] <- 0
		sm.matrix <- sm.matrix*-1
		sm.matrix[upper.tri(sm.matrix,diag=TRUE)] <- NA
		sm.dist <- as.dist(sm.matrix)

		cluster.res <- hclust(sm.dist,method='complete')
		cluster.assignment <- cutree(cluster.res,500)
		cluster.assignment <- data.table(v=names(cluster.assignment),cluster=cluster.assignment)
		fwrite(cluster.assignment,file='cluster/cluster_all_buffer.csv',row.names=FALSE)
	}

	all.clusters.buffer <- fread('cluster/cluster_all_buffer.csv')

	route.overlap.buffer <- data.table()
	for(c in unique(all.clusters.buffer$cluster)) {
		v.hold <- all.clusters.buffer[cluster==c]$v
		taz.list <- list()
		for(vi in v.hold) {
			taz.hold <- all.routes[v==vi]$`TAZ12,`
			taz.list <- c(taz.list,list(taz.hold))
		}
		routes.hold <- Reduce(intersect,taz.list)
		overlap.hold <- data.table(v=rep(v.hold,each=length(routes.hold)),taz=rep(routes.hold,length(v.hold)))
		route.overlap <- rbind(route.overlap,overlap.hold)
	}

	fwrite(route.overlap,file='cluster/route_overlap.csv',row.names=FALSE)