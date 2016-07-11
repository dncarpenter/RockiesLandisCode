###### I. Prep working environment ######
# setwd('/Users/dunbar/OneDrive - University of New Mexico/Rockies_Landis')
# setwd('C:/Dunbar/OneDrive for Business 1/Rockies_Landis')
setwd('Y:/Rockies_Landis')
library(rgdal)
# library(raster) 
# library(rgeos)
# library(maptools)


###### II. Prep & lead data ######
#### 1. Assemble & load FIA plot data from RM states
# source('RM states FIA plots')
rms.plots <- read.csv('Initial Communities/Data/RM states FIA plots.csv')

#### 2. Prep & load spatial data
# source('RM inital communities spatial data prep.r')
rm.rgn <- readOGR(dsn = 'spatial/InitialCommunities', layer='RM_study_region')
plot(rm.rgn, lwd=2)

######## III. Screen RMS FIA plots to make set for estimating study region species composition ########
## total plots in RM states
# nrow(rms.plots) # 202k plots
#### 1. select all sampled plots (forested or non-forested, for intial communinities)
rms.spld <- rms.plots[rms.plots$PLOT_STATUS_CD %in% 1:2,]
# nrow(rms.spld) # ~197k plots

#### 2. select all sampled plot w/ >=1 forested condition (to estimate tree spp composition)
rms.sf <- rms.spld[rms.spld$PLOT_STATUS_CD == 1,]
# nrow(rms.sf) # ~60k plots
## make SPDFs for RM sampled & forested FIA plots and plot
rms.sf.xy <- rms.sf[!is.na(rms.sf$LON) & !is.na(rms.sf$LAT), c('LON','LAT')]
rms.sf.spdf <- SpatialPointsDataFrame(coords = rms.sf.xy, data=rms.sf[!is.na(rms.sf$LON) & !is.na(rms.sf$LAT),],
                                      proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
rms.sf.spdf <- spTransform(rms.sf.spdf, CRSobj = CRS(proj4string(rm.rgn)))
plot(rms.sf.spdf, pty=20, col='green', cex=0.01, add=T)

## 3. select all forested, sampled plots located in RM ecosubsecs (extends outside RM study region)
rm.ecossecs <- readOGR(dsn='spatial/InitialCommunities', layer='RM_ecosubsecs')
rmssf.ess <- rms.sf[rms.sf$ECOSUBCD %in% rm.ecossecs$MAP_UNIT_S,]
# nrow(rmssf.ess) # ~37k plots

## 4. select all forested, sampled RM ecosubsecs located in RM counties (extends outside RM study region)
rm.ctys <- readOGR(dsn='spatial/InitialCommunities', layer='RM_counties')
rmssf.ess$CTY_ST <- 
  apply()
rmssfe.ctys <- rmssf.ess[rmssf.ess$CTY_CN %in% rm.ctys$MAP_UNIT_S,]
# nrow(rmssf.ecossecs) # ~37k plots



write.csv(rmcepf, file = 'Initial communities/Data/Rockies_spp_comp_FIA_plots.csv', row.names = F)
writeOGR(rmcepf.spdf, dsn = 'spatial/InitialCommunities', layer='Rockies_spp_comp_FIA_plots', driver='ESRI Shapefile', overwrite_layer=T)


## select plots that contain sampled forest, & simplify df attributes
rm.fps <- subset(rm.plots, PLOT_STATUS_CD==1, select=c(CN, STATENM, STATECD, UNITCD, COUNTYNM, COUNTYCD, PLOT, INVYR, CYCLE,
                                                       MEASYEAR, MEASMON, MEASDAY, LAT, LON, ELEV, ECOSUBCD))
table(rm.fps$STATENM, rm.fps$CYCLE, exclude = NULL)
## select only the most recent record for plots that have been resampled
rm.fps$Select <- FALSE # create attribute to mark selected records
## cycle though states, FIA units, counties, and plot IDs (all needed to ID unique plots)
for (i in 1:length(unique(rm.fps$STATECD))) {
  rm.fps.s <- rm.fps[rm.fps$STATECD==unique(rm.fps$STATECD)[i],]
  for (j in 1:length(unique(rm.fps.s$UNITCD))) {
    rm.fps.su <- rm.fps.s[rm.fps.s$UNITCD==unique(rm.fps.s$UNITCD)[j],]
    for (k in 1:length(unique(rm.fps.su$COUNTYCD))) {
      rm.fps.suc <- rm.fps.su[rm.fps.su$COUNTYCD==unique(rm.fps.su$COUNTYCD)[k],]
      for (l in 1:length(unique(rm.fps.suc$PLOT))) {
        rm.fps.sucp <- rm.fps.suc[rm.fps.suc$PLOT==unique(rm.fps.suc$PLOT)[l],]
        # if only one record exists, select; otherwise select the most recent measurement year, or inventory year in case of tie
        if(nrow(rm.fps.sucp)==1) { rm.fps$Select[rm.fps$CN==rm.fps.sucp$CN] <- TRUE } else {  
          if(length(rm.fps.sucp$MEASYEAR==max(rm.fps.sucp$MEASYEAR))==1) { 
            rm.fps$Select[rm.fps$CN==rm.fps.sucp$CN[which.max(rm.fps.sucp$MEASYEAR)]] <- TRUE } else {
              rm.fps$Select[rm.fps$CN==rm.fps.sucp$CN[which.max(rm.fps.sucp$INVYR)]] <- TRUE } } 
      }  }  }  }
## explore plot record selection
head(rm.fps)
table(rm.fps$Select)
dim(unique(rm.fps[, c('LAT','LON')]))
dim(unique(rm.fps[rm.fps$Select==T, c('LAT','LON')]))
table(rm.fps$STATENM, rm.fps$Select, exclude = NULL)
table(rm.fps$MEASYEAR, rm.fps$Select, exclude = NULL)
table(rm.fps$STATENM[rm.fps$Select], rm.fps$CYCLE[rm.fps$Select], exclude = NULL)
table(rm.fps$STATENM[rm.fps$Select], rm.fps$MEASYEAR[rm.fps$Select], exclude = NULL)
table(rm.fps$STATENM[rm.fps$Select], rm.fps$INVYR[rm.fps$Select], exclude = NULL)
table(rm.fps$STATENM[rm.fps$Select], rm.fps$MEASYEAR[rm.fps$Select], rm.fps$CYCLE[rm.fps$Select], exclude = NULL)
table(rm.fps$MEASYEAR, rm.fps$STATENM, rm.fps$Select, exclude = NULL)
barplot(height = as.numeric(table(rm.fps$MEASYEAR[rm.fps$Select==T])), names.arg = names(table(rm.fps$MEASYEAR[rm.fps$Select==T])))

#########
rm.tfs <- list.files('Data/FIAdata/', pattern = '^.._TREE.csv', recursive = T, full.names = T)
## cycle through FIA TREE tables (from each state) and select those falling in RM counties and ecological subsections
for (i in 1:length(rm.tfs)) {
  tree.i <- read.csv(rm.tfs[i]) # load FIA plot table for state i
  st.i.ctys <- merge(rm.state, ctys.i, all.y=T) # add state info to create state+county df
  ## Rocky Mountain county filter
  rm.ctys.st.i <- rm.ctys$NAME[rm.ctys$STATENAME %in% st.i.ctys$STATENM] # RM counties in state
  rm.ctys.i <- st.i.ctys[st.i.ctys$COUNTYNM %in% rm.ctys.st.i,] # subset RM counties from state+county df
  rm.cp.i <- merge(plts.i, rm.ctys.i[,c('STATENM','COUNTYCD','COUNTYNM')], by="COUNTYCD") # FIA plots in RM counties
  ## Rocky Mountain ecosubsec filter
  rm.cp.i$ECOSUBCD <- sub(pattern = ' (.*)', x = rm.cp.i$ECOSUBCD, replacement = '\\1') # remove whitespace in ecosubsec names
  rm.cep.i <- rm.cp.i[rm.cp.i$ECOSUBCD %in% rm.ecossecs$MAP_UNIT_S,] # select FIA plots (in RM counties) in RM ecosubsecs
  ## Add to RM regional FIA plot df
  rm.plots <- rbind(rm.plots, rm.cep.i)
}

## select most recent records & only those since 2005
rm.rfps <- rm.fps[rm.fps$Select & rm.fps$MEASYEAR>=2005, -which(names(rm.fps)=='Select')]
## explore selection
dim(unique(rm.rfps))
st.fia.plt <- as.data.frame(table(rm.rfps$STATENM, exclude = NULL))
table(rm.rfps$MEASYEAR, exclude = NULL)
table(rm.rfps$ECOSUBCD, exclude = NULL)
##
write.csv(rm.rfps, 'Dunbar/Rockies_Landis/initial communities/RM_recent_forested_FIA_plots.csv', row.names = F)
write.csv(rm.rfps, 'C:/Dunbar/OneDrive for Business 1/dissertation research/RM_recent_forested_FIA_plots.csv', row.names = F)
##
library(foreign)
rms.fa <- read.dbf(file = 'Dunbar/Rockies_Landis/spatial/rm_state_forest_area.dbf')
rms.fa2 <- data.frame(State=c('Arizona','Idaho','New Mexico','Utah','Washington','Wyoming','Colorado','Montana'), 
                      ForArea=rms.fa$FOREST___W/(1000^2))
rms.fia.fa <- merge(st.fia.plt, rms.fa2, by.x='Var1', by.y='State')
rms.fia.fa$plot.dens <- rms.fia.fa$Freq/rms.fia.fa$ForArea

## create spatial from RM recently surveyed forests plots
rm.rfps.xy <- rm.rfps[,c('LON','LAT')]
rm.rfps.spdf <- SpatialPointsDataFrame(coords = rm.rfps.xy, data=rm.rfps, proj4string = CRS(proj4string(rm.rgn)))
plot(rm.rgn)
plot(rm.rfps.spdf, pty=20, cex=0.01, add=T)
writeOGR(rm.rfps.spdf, dsn = '/Dunbar/Rockies_Landis/spatial/InitialCommunities', layer='RM_recent_forested_FIA_plots', driver='ESRI Shapefile', overwrite_layer=T)

#### trees/plot
for (j in 1:nrow(scp.i)) {
  tr.ij <- tr.i[tr.i$PLT_CN==scp.i$PLT_CN[j],]
  scp.i$TREECOUNT[j] <- sum(tr.ij$STATUSCD %in% 1:2, na.rm = T)
  scp.i$LIVETREECOUNT[j] <- sum(tr.ij$STATUSCD == 1, na.rm = T)
}

#### Potentially forested area in each state; to estimate sampling intensity
plot(rm.ctys, border='blue')
plot(rm.ecossecs, add=T)

rm.ctys.dslv <- unionSpatialPolygons(rm.ctys, IDs = rm.ctys@data$LSAD)
plot(rm.ctys.dslv, lwd=2)

rm.ecossecs.dslv <- unionSpatialPolygons(rm.ecossecs, IDs = rm.ecossecs@data$MAP_TYPE)
plot(rm.ecossecs.dslv, add=T, lwd=2, border='green')
plot(rm.rgn, border='gray', add=T)
## load gpu data and select only state features; omit counties etc.
usfs.gpu <- readOGR('Y:/GISdata/USFS_Ecoregions/S_USA.ALPGeopoliticalUnit','S_USA.ALPGeopoliticalUnit')
rm.states <- usfs.gpu[usfs.gpu$TYPENAMERE=='State' & usfs.gpu$NAMEABBREV %in% c('NM','AZ','UT','CO','WY','MT','ID','WA'),] 
plot(rm.states)

rm.ce.dlv <- gIntersection(rm.ctys.dslv, rm.ecossecs.dslv)
plot(rm.ce.dlv, add=T, border='purple', lwd=2)


rm.st.fia <- gIntersection(spgeom1 = rm.states, spgeom2 = rm.ce.dlv, byid = T, drop_lower_td = T)
rm.st.fia <- SpatialPolygonsDataFrame(rm.st.fia, data=data.frame(State=c('AZ','ID','NM','UT','WA','WY','CO','MT'), row.names = names(rm.st.fia)))
writeOGR(rm.st.fia, dsn = 'Dunbar/Rockies_Landis/spatial', layer = 'RM_states_FIA_area', driver = 'ESRI Shapefile', overwrite_layer = T)
plot(rm.ctys, border='grey')
plot(rm.ecossecs, border='green', add=T)
plot(rm.plots.spdf, pty=20, cex=0.01, col='gray', add=T)
plot(rm.rgn, border='purple', lwd=2, add=T)
plot(rm.st.fia, lwd=2, add=T)

library(raster)
rm.nglc <- raster(x='C:/Dunbar/Rockies_Landis/spatial/RM_Nat_GAP_Landcover_NVC_cla.tif')
plot(rm.nglc)
##
rm.states <- usfs.gpu[usfs.gpu$TYPENAMERE=='State' & usfs.gpu$NAMEABBREV %in% c('NM','AZ','UT','CO','WY','MT','ID','WA'),]
plot(rm.states, add=T)

for.area <- extract(rm.for2, rm.states, fun=sum, na.rm=T)


####
nm.tree <- read.csv('Y:/Data/FIAdata/NM/NM_TREE.csv')
wy.tree <- read.csv('Y:/Data/FIAdata/WY/WY_TREE.csv')
wy.tr.04 <- wy.tree[wy.tree$PLT_CN %in% wy.2004,]

rm.nm.tree <- nm.tree[nm.tree$PLT_CN %in% rm.fia$CN,]

spcd <- read.csv('Y:/Data/FIAdata/FIADB_REFERENCE/REF_SPECIES.csv')
rm.nm.tree2 <- merge(rm.nm.tree, spcd[,1:2], all.x = T, by='SPCD')
rm.nm.tree2$COMMON_NAME <- factor(rm.nm.tree2$COMMON_NAME, unique(rm.nm.tree2$COMMON_NAME))
par(mar=c(3,9,1,1))
barplot(table(rm.nm.tree2$COMMON_NAME), las=1, cex.names = 0.7,horiz = T)