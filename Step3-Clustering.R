########################## 1. extract the day and method of calculating distance ##########################
#get distance calculated by defined methods
dataExtract <- function(distance, method){
  
  if (method == 'om') {diss <- distance$data.om}
  else if (method == 'ham') {diss <- distance$data.ham}
  else{diss <- NULL
  print(paste("Input method is not valide:", method))}
  
  if(is.null(diss)){
    return(NULL)
  } else {
    data.seq <- distance$data.seq
    UserId = distance$UserId
    date = distance$date
    output <- list(diss = as.matrix(diss),
                   data.seq = data.seq,
                   UserId = UserId,
                   curdate = date)
    return(output)
  }
}

#read distance data
distance = readRDS("distanceWeekend_bookchapter_v2.rds")
dist.om <- dataExtract(distance,method = 'om')
dist.ham <- dataExtract(distance,method = 'ham.')
#for example, if we decide to use optimal distance. we assign dist.om to temp.data for further analysis
temp.data <-  dist.om

########################## 2. pick the optimal clustering method and number of clusters ##########################
#data clustering based on distance
CreateClusters <- function(dist.list, n.cluster, method = "hc"){
  ## apply hierarchical clustering
  if(method == "hc") {
    h.cluster <- hclust(as.dist(dist.list$diss), method = "ward.D2")
    clust <- cutree(h.cluster, k = n.cluster)
  } 
  ## apply pam method
  else if (method == "pam") {
    clust <- wcKMedoids(dist.list$diss, k = n.cluster, initialclust = h.cluster)
    # transfer medoid to 1:n
    ref.table <- 1:length(unique(clust$clustering))
    names(ref.table) <- levels(as.factor(clust$clustering))
    clust <- ref.table[as.character(clust$clustering)]
  }
  ## input method does not exsit, return NA value
  else{clust = NULL}
  
  # get a subset of the many measures for the results above
  if(is.null(clust)) {
    return (NULL)}
  else{
    # output cluster labels, cluster quality and output the distance matrix we input
    clust.quality <- wcClusterQuality(dist.list$diss, clustering = clust)
    results <- list(clust = clust, clust.quality = clust.quality, object = dist.list)
    return(results)
  }
} 

# method - hierarchical clustering
# you could also try pam clustering, etc.
temp.cluster <- hclust(as.dist(temp.data$diss), method = 'ward.D2')

tempRange <- as.clustrange(object = temp.cluster,
                           diss = temp.data$diss,
                           ncluster = 10)
round(summary(tempRange, max.rank = 3), 3)
dev.off()
plot(tempRange, stat = c('PBC', 'HG', 'ASWw', 'CH', 'RHC'), 
     norm='zscore', lwd = 3,
     xlab = "number of clusters", ylab = "zscore")


# get clustering result based on the optimal number of clusters
temp.clust.results <- CreateClusters(temp.data, 6, "hc")


########################## 3. sequence alignment result visualization ##########################
timeInterval = 5 * 60   #5 mins * 60s
stringLength = round(24 * 60 * 60 / timeInterval)

#get sequence
clust.plot.seq <- temp.clust.results$object$data.seq

#get user
clust.plot.user = data.frame(UserId=temp.clust.results$object$UserId,
                             date=temp.clust.results$object$curdate)

#get cluster results
clust.plot.clust <- cbind(UserId = names(temp.clust.results$clust),
                          clust = temp.clust.results$clust)

#merge user with cluster results
clust.plot.user = merge(clust.plot.user, clust.plot.clust,
                        by.x=c("UserId"), by.y=c("UserId"),all.x = TRUE,all.y=FALSE)

#format x-labels as %H:%M starting from 3 am.
clust.plot.xlabs = format(seq.POSIXt(as.POSIXct(Sys.Date()), 
                                     as.POSIXct(Sys.Date()+1), 
                                     by = paste0((60*24/stringLength), " min")),
                          "%H:%M", tz="GMT")

clust.plot.xlabs = clust.plot.xlabs[c(37:288,1:37)]  #change

#figure margin size
par(mar=c(3, 5, 3, 4.5))
#customize color
pal = c("seagreen4","#8DD3C7","#CCEBC5","#ab6ecc","#BEBADA","lavender","coral1",
        "#FFED6F","lightgoldenrodyellow","#e8ded5")

#initial version visualization
seqplot(clust.plot.seq, group=clust.plot.user$clust,
        type = "d", xtstep=12, cpal = pal,
        xtlab = clust.plot.xlabs, cex.lab=2.5, cex.axis=2.5,
        use.layout=TRUE, cols = 2, rows = 3,
        border = NA, with.legend=F,
        main =  NULL, cex.main = 3 ,cex.legend=1.5, ncol=4, legend.prop=.2)

#you can recode the cluster to adjust the order of clusters
clust.plot.user$clust <- recode(
  clust.plot.user$clust,
  `2` = "7",
  `4` = "2",
  `5` = "4",
  `3` = "5",
  `7` = "3"
)

#you can re-label each cluster to assign name for each identified pattern
clust.plot.user$clust <- factor(
  clust.plot.user$clust,levels = c('1','2','3','4','5','6'),
  labels=c('1 - Mostly at Home (mostly with Household Tasks)', 
           '2 - Mostly at Home (mostly without Household Tasks)',
           '3 - Regular Work (mostly with Household Tasks at Home)',
           '4 - Regular Work (mostly without Household Tasks at Home)',
           '5 - OutHome in the Evening', 
           '6 - Mostly OutHome'))

#final version visualization
seqplot(clust.plot.seq, group=clust.plot.user$clust,
        type = "d", xtstep=12, cpal = pal,
        xtlab = clust.plot.xlabs, cex.lab=2.5, cex.axis=2.5,
        use.layout=TRUE, cols = 2, rows = 3,
        border = NA, with.legend=F,
        main =  NULL, cex.main = 3 ,cex.legend=1.5, ncol=4, legend.prop=.2)