#### Prep working environment
setwd('Y:/')
library(rgdal)

#### Dowload FIA data for Rocky Mountain States (not finished; currently downloaded manually)
## download and unzip FIA data; see http://statistics.berkeley.edu/computing/r-reading-webpages and https://github.com/hadley/rvest
# fiadatamart <- readLines('http://apps.fs.fed.us/fiadb-downloads/datamart.html')
# download.file('http://apps.fs.fed.us/fiadb-downloads/datamart.html')
# grep(".zip", fiadatamart)
 
#### Assemble data set from FIA plots in Rocky Mountain states
## Files with Rocky Mountain states' FIA plot and county data 
rms.pfs <- list.files('Data/FIAdata/', pattern = '^.._PLOT.csv', recursive = T, full.names = T)
rms.cfs <- list.files('Data/FIAdata/', pattern = '^.._COUNTY.csv', recursive = T, full.names = T)
rms.tfs <- list.files('Data/FIAdata/', pattern = '^.._TREE.csv', recursive = T, full.names = T)
## load FIA plot, county, & state data
fia.sc <- read.csv('Data/FIAdata/STATE_CODES.csv') # table of FIA state codes & names, from FIA Phase 2 User Guide (v.6.0.2), Appdx B
fia.ct <- read.csv(rms.cfs[1]) # County Table template
fia.pdt <- read.csv(rms.pfs[1]) # Plot Table template
## make empty df for FIA plot, county, & state data
rms.plots <- NULL
## cycle through RM states FIA plot tables & merge w/ state & county data (<10secs)
for (i in 1:length(rms.pfs)) {
    ctys.i <- read.csv(rms.cfs[i]) # load county table for state i
    st.ct.i <- merge(fia.sc, ctys.i, all.y=T) # add state info to create state+county df
    plts.i <- read.csv(rms.pfs[i]) # load FIA plot table for state i
    st.ct.pt.i <- merge(st.ct.i, plts.i, by='COUNTYCD') # add plot data to state_country df
    attach(st.ct.pt.i)  # make cleaned-up df for plot data, including removal of whitespace in ecosubsec names
    scp.i <- data.frame(STATECD=STATECD.x, STATEABRV=droplevels(STATEABRV), STATENM=droplevels(STATENM), UNITCD=UNITCD.x, CTY_CN, 
                        COUNTYCD, COUNTYNM, ECOSUBCD=sub(pattern=' (.*)', x=ECOSUBCD, replacement='\\1'), EMAP_HEX, PLOT, PLT_CN=CN.y,
                        LAT, LON, ELEV, MEASYEAR, MEASMON, MEASDAY, SRV_CN, INVYR, CYCLE, REMPER, PLOT_STATUS_CD, 
                        PLOT_NONSAMPLE_REASN_CD, NF_SAMPLING_STATUS_CD, WATERCD, RDDISTCD, KINDCD, DESIGNCD, 
                        MANUAL, QA_STATUS, SAMP_METHOD_CD, P2VEG_SAMPLING_STATUS_CD, P2VEG_SAMPLING_LEVEL_DETAIL_CD)
    detach(st.ct.pt.i)
    rms.plots <- rbind(rms.plots, scp.i) ## Add to RM state FIA plot df
    }
## write full RM states FIA data set; takes 5-10secs to write (32mb); necessary to write?
write.csv(rms.plots, 'Rockies_Landis/Initial Communities/Data/RM states FIA plots.csv', row.names = F)

## plot RM states FIA plots
rms.plots.xy <- rms.plots[!is.na(rms.plots$LON) & !is.na(rms.plots$LAT), c('LON','LAT')]
rms.plots.spdf <- SpatialPointsDataFrame(coords = rms.plots.xy, data=rms.plots[!is.na(rms.plots$LON) & !is.na(rms.plots$LAT),],
                                        proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
rms.plots.spdf <- spTransform(rms.plots.spdf, CRSobj = CRS("+proj=laea +lat_0=45.5 +lon_0=-114.125 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs"))
plot(rms.plots.spdf, col='gray', pty=20, cex=0.01)

########
# source('RM inital communities spatial data prep.r')
## RM study region
rm.rgn <- readOGR(dsn = 'Rockies_Landis/spatial/InitialCommunities', layer='RM_study_region')
plot(rm.rgn, lwd=2, add=T)
## RM counties
rm.ctys <- readOGR(dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_counties')
plot(rm.ctys, add=T)
##
rm.cty.plots <- merge(rms.plots.spdf, rm.ctys, by.x=c('COUNTYNM','STATENM'), by.y=c('NAME','STATE_NAME'), all.x=F)
    
plot(rm.cty.plots, col='orange', pty=20, cex=0.01, add=T)
## Load USFS Ecological Subsections shapefile
ecossecs <- readOGR('GISdata/USFS','USFS_US_subsections')
ecossecs <- spTransform(ecossecs, CRSobj=CRS(rm.crs))
## select eco subsections that intersect the RM study region and plot
rm.ecossecs <- ecossecs[which(gIntersects(rm.rgn, ecossecs, byid = T)),]
plot(rm.ecossecs, lwd=1, border='green4', add=T)
plot(rm.rgn, lwd=2, add=T)
writeOGR(rm.ecossecs, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecological_subsections',
         driver='ESRI Shapefile', overwrite_layer=T)

rm.ep.cty.dslv <- readOGR(dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecoprov_x_counties_dissolve')
plot(rm.ep.cty.dslv, add=T)


#### Explore RMS FIA plots
# str(rms.plots) # ~202k plots
# dim(unique(rms.plots[,c('LAT','LON')]))
# dim(unique(rms.plots[,c('STATECD','UNITCD','COUNTYCD','PLOT')]))
# table(rms.plots$STATENM, exclude = NULL)
# table(rms.plots$MEASYEAR, exclude = NULL)
# table(rms.plots$STATENM, rms.plots$MEASYEAR, exclude = NULL)
# table(rms.plots$PLOT_STATUS_CD, exclude = NULL)
# table(rms.plots$PLOT_STATUS_CD, rms.plots$PLOT_NONSAMPLE_REASN_CD, exclude = NULL)
# table(rms.plots$PLOT_STATUS_CD, rms.plots$NF_SAMPLING_STATUS_CD, exclude = NULL)
# table(rms.plots$MEASYEAR[rms.plots$STATENM=='Wyoming'], rms.plots$PLOT_STATUS_CD[rms.plots$STATENM=='Wyoming'], exclude = NULL)
# wy.2004 <- rms.plots$PLT_CN[rms.plots$STATENM=='Wyoming' & rms.plots$MEASYEAR==2004]
