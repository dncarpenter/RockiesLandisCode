#### Prep working environment
setwd('Y:/Rockies_Landis')
library(rgdal)
# library(raster)
# library(rgeos)
# library(maptools)


######## I. Select FIA plots for creatin initial communities
#### Screen RMS FIA plots
# source('RM states FIA plots')
## prep spatial data i.e. counties & ecological subsections intersecting or w/in RM study region
# source('RM FIA spatial data prep.r')
rms.plots <- read.csv('Initial Communities/Data/RM states FIA plots.csv')
# nrow(rms.plots) # ~202k plots

################
#### 1. select all sampled plots (forested or non-forested, for intial communinities)
rms.spld <- rms.plots[rms.plots$PLOT_STATUS_CD %in% 1:2,]
# nrow(rms.spld) # ~197k plots
write.csv(rms.spld, 'Initial Communities/Data/RM states FIA plots.csv', row.names=F)
## make SPDF of sampled FIA plots in RM states, transform into propers CRS, & plot
rms.spld.xy <- rms.spld[!is.na(rms.spld$LON) & !is.na(rms.spld$LAT), c('LON','LAT')]
rms.spld.spdf <- SpatialPointsDataFrame(coords = rms.spld.xy, data=rms.spld[!is.na(rms.spld$LON) & !is.na(rms.spld$LAT),],
                                        proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
rms.spld.spdf <- spTransform(rms.spld.spdf, CRSobj = CRS(proj4string(rm.rgn)))
plot(rms.spld.spdf, col='gray', pty=20, cex=0.01)
plot(rm.rgn, lwd=2, add=T)
##############

#### A. Rocky Mountain study region: include only plots that fall geographically in the RM study region (i.e. in RM counties & ecological units)
### 1. Rocky Mountain county filter
rm.ctys <- readOGR('spatial/InitialCommunities', layer='Rocky_Mountain_counties') # load RM counties
## select RM state plots that fall in RM counties; omit extranesous cols from rm.ctys incorporated by merge
rmcp <- merge(rms.plots, rm.ctys@data, by.x=c('STATENM','COUNTYNM'), by.y=c('STATE_NAME','NAME'))[,1:ncol(rms.plots)]
nrow(rmcp) # ~103k plots
## make SPDFs for RM counties FIA plots & plot
rmcp.xy <- rmcp[!is.na(rmcp$LAT),c('LON','LAT')]
rmcp.spdf <- SpatialPointsDataFrame(coords = rmcp.xy, data=rmcp[!is.na(rmcp$LAT),], proj4string = CRS(proj4string(rm.ctys)))
par(mar=c(rep(0.5, 4)))
plot(rmcp.spdf, pty=20, col='gray', cex=0.01)
plot(rm.ctys, add=T)

### 2. Rocky Mountain ecological subsection filter
rm.ecossecs <- readOGR('spatial/InitialCommunities', layer='Rocky_Mountain_ecological_subsections') # load RM ecosubsecs
## select RM state plots that fall in RM counties
rmcep <- rmcp[rmcp$ECOSUBCD %in% rm.ecossecs@data$MAP_UNIT_S,]
nrow(rmcep) # ~80k plots
## make SPDFs for RM county & ecosubsec & plot
rmcep.xy <- rmcep[!is.na(rmcep$LAT),c('LON','LAT')]
rmcep.spdf <- SpatialPointsDataFrame(coords = rmcep.xy, data=rmcep[!is.na(rmcep$LAT),], proj4string = CRS(proj4string(rm.ctys)))
plot(rmcep.spdf, pty=20, col='yellow', cex=0.01, add=T)
plot(rm.ecossecs, border='green3', add=T)

#### B. Plot Status: include only sampled plots
## 1. select all sampled plot (forested or non-forested, for intial communinities)
rmceps <- rmcep[rmcep$PLOT_STATUS_CD %in% 1:2,]
write.csv(rmceps, file = 'Initial communities/Data/Rockies_initial_communities_FIA_plots.csv', row.names = F)
nrow(rmceps) # ~77k plots
## make SPDFs for RM sampled FIA plots and plot
rmceps.xy <- rmceps[,c('LON','LAT')]
rmceps.spdf <- SpatialPointsDataFrame(coords = rmceps.xy, data=rmceps, proj4string = CRS(proj4string(rm.ctys)))
plot(rmceps.spdf, col='purple', pty=20, cex=0.01, add=T)
writeOGR(rmceps.spdf, dsn = 'spatial/InitialCommunities', layer='Rockies_initial_communities_FIA_plots', driver='ESRI Shapefile', overwrite_layer=T)

## plot RM counties, ecosubsecs, and study region
plot(rm.ctys, add=T)
plot(rm.ecossecs, border='green3', add=T)