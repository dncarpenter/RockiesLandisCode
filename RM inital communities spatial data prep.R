#### Prep working environment
setwd('Y:/')
library(rgdal)
library(rgeos)
library(raster)
library(RColorBrewer)

#### 1. Coordinate Reference System
## Lambert Azimuthal Equal Area projection centered in central Rockies; used for all spatial data
rm.crs <- '+proj=laea +lat_0=45.5 +lon_0=-114.125 +x_0=0 +y_0=0 +a=6371007 +b=6371007 +units=m +no_defs'

#### 2. Rocky Mountain Study region
## RM study region shapefile (load, reproject, plot, save); created via 'Rockies delineation' ArcGIS model
rm.rgn <- readOGR(dsn = 'Rockies_Landis/spatial/RockiesDelineation', layer = 'RockyMtn_Region')
rm.rgn <- spTransform(rm.rgn, CRSobj=CRS(rm.crs)) 
par(mar=rep(0.5,4))
plot(rm.rgn, lwd=2)
writeOGR(rm.rgn, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_study_region', driver='ESRI Shapefile', overwrite_layer=T)
                      
#### 3. Counties in Rocky Mountain Study Region
## Load & project US Counties shapefile (from USFS geopolitical boundaries layer)
counties <- readOGR('GISdata/US_Counties','US_counties_ESRI')
counties <- spTransform(counties, CRSobj=CRS(rm.crs))
## Select counties that intersect the RM study region; plot & save
rm.ctys <- counties[which(gIntersects(rm.rgn, counties, byid = T)),]
plot(rm.ctys, border='gray80', add=T)
writeOGR(rm.ctys, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_counties', driver='ESRI Shapefile', overwrite_layer=T)
## Dissolve RM counties into single polygon; plot & save
rm.ctys@data$Region <- rep('RM',nrow(rm.ctys@data))
rm.ctys.dslv <- aggregate(rm.ctys, by='Region')
plot(rm.ctys.dslv, border='gray30', add=T)
writeOGR(rm.ctys.dslv, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_counties_dissolve', driver='ESRI Shapefile', overwrite_layer=T)

#### 4. Ecological Subsections in Rocky Mountain Study Region
## Load & project USFS Ecological Subsections shapefile
ecossecs <- readOGR('GISdata/USFS','USFS_US_subsections')
ecossecs <- spTransform(ecossecs, CRSobj=CRS(rm.crs))
## Select ecosubsecs that intersect the RM study region; plot & save
rm.ecossecs <- ecossecs[which(gIntersects(rm.rgn, ecossecs, byid = T)),]
plot(rm.ecossecs, lwd=1, border='green4', add=T)
writeOGR(rm.ecossecs, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecosubsecs', driver='ESRI Shapefile', overwrite_layer=T)
## Dissolve RM ecosubsecs into single polygon; plot & save
rm.ecossecs.dslv <- aggregate(rm.ecossecs, by='AREA')
plot(rm.ecossecs.dslv, border='green4', lwd=2, add=T)
writeOGR(rm.ecossecs.dslv, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecosubsecs_dissolve', driver='ESRI Shapefile', overwrite_layer=T)

#### 5. Ecological Provinces in the Rocky Mountain Study Region
## Obtain ecoprov codes from ecosubsec map unit codes
rm.ecossecs@data$EcoProvCode <- sub(pattern = '(.*)[[:upper:]].*', replacement = '\\1', x = rm.ecossecs@data$MAP_UNIT_S)
## Dissolve ecosubsec SPDF by ecoprov code
rm.ecoprov <- aggregate(rm.ecossecs, by='EcoProvCode')
## Make ecoprov code-code lookup table from ecosec shapefile & merge w/ ecoprov SPDF
usfs.ecosecs <- readOGR('GISdata/USFS','USFS_US_sections')
usfs.ecosecs <- spTransform(usfs.ecosecs, CRSobj=CRS(rm.crs))
ecoprov.lut <- unique(data.frame(EcoProvCode=as.character(sub('-*([[:upper:]]*...).', '\\1', usfs.ecosecs@data$ECOCODE)), 
                                 EcoProvName=as.character(usfs.ecosecs@data$PROVINCE), stringsAsFactors = F))
rm.ecoprov@data <- merge(rm.ecoprov@data, ecoprov.lut, all.x=T)
## Plot ecoprov SPDF (w/ RM ecosubsecs & study region) & save
## note that dissolved ecoprov SPDF would be identival to dissolved ecosubsec SPDF
ecoprov.pal <- brewer.pal(n = nrow(rm.ecoprov), name = 'Paired')
plot(rm.ecoprov, col=ecoprov.pal)
plot(rm.ecossecs, lwd=0.2, border='gray', add=T)
plot(rm.rgn, lwd=3, add=T)
writeOGR(rm.ecoprov, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecoprovs', driver='ESRI Shapefile', overwrite_layer=T)

#### 6. Area in Rocky Mountain Counties & Ecological Provinces/Subsections
## Clip RM ecoprov SPDF by RM counties SPDF
rm.ep.ctyclip <- gIntersection(rm.ctys.dslv, rm.ecoprov, byid = T, drop_lower_td = T)
row.names(rm.ep.ctyclip) <- row.names(rm.ecoprov)
rm.ep.cty <- SpatialPolygonsDataFrame(Sr = rm.ep.ctyclip, data = rm.ecoprov@data)
## Plot RM ecoprov x county SPDFs & save
plot(rm.ep.cty, col=ecoprov.pal)
plot(rm.ecossecs.dslv, lwd=1.5, border='gray35', add=T)
plot(rm.ctys.dslv, lwd=1.5, add=T)
plot(rm.rgn, lwd=2, add=T)
writeOGR(rm.ep.cty, dsn = 'Rockies_Landis/spatial/InitialCommunities', layer = 'RM_ecoprov_x_counties', driver='ESRI Shapefile', overwrite_layer=T)
## Dissolve ecoprov x county SPDF into single polygon; plot & save
rm.ep.cty@data$Region <- rep('RM',nrow(rm.ep.cty@data))
rm.ep.cty.dslv <- aggregate(rm.ep.cty, by='Region')
plot(rm.ep.cty.dslv, border='gray', lwd=1.5, add=T)
writeOGR(rm.ep.cty.dslv, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecoprov_x_counties_dissolve', 
         driver='ESRI Shapefile', overwrite_layer=T)

#### 7. Total Area and Forested Area in Rocky Mountain Ecolocical Provinces
## Clip RM ecoprov SPDF by RM study region
rm.ep.rgnclip <- gIntersection(rm.rgn, rm.ecoprov, byid = T, drop_lower_td = T)
row.names(rm.ep.rgnclip) <- row.names(rm.ecoprov)
rmrgn.ecoprov <- SpatialPolygonsDataFrame(Sr = rm.ep.rgnclip, data=rm.ecoprov@data)
## Calculate ecoprove areas (km^2 & %) & add to SPDF
rmrgn.ecoprov@data$Area.sqkm <- round(gArea(rmrgn.ecoprov, byid=T)/(1000^2))
rmrgn.ecoprov@data$Area.pct <- round(100*gArea(rmrgn.ecoprov, byid=T)/sum(gArea(rmrgn.ecoprov, byid=T)), 2)
## Calculate ecoprov forested areas (km^2 & %) using potental forest layer (made w/ 'Rockies delineation' ArcGIS model) & add to spdf
rm.pf <- raster('Rockies_Landis/spatial/RockiesDelineation/rmpf_rs300m')
rm.pf <- projectRaster(rm.pf, res=300, crs=CRS(rm.crs), method ='ngb')# takes a few mins
rm.pf.ep <- extract(rm.pf, rmrgn.ecoprov) # takes 5-10mins; warning msgs
rmrgn.ecoprov@data$ForArea.sqkm <- unlist(lapply(rm.pf.ep, function(x){round((0.3^2)*sum(x, na.rm=TRUE))})) # forested km^2in each ecoprov
rmrgn.ecoprov@data$ForArea.pct <- unlist(lapply(rm.pf.ep, function(x){round(100*mean(x, na.rm=TRUE),1)} )) # % of area forested in each ecoprov
rmrgn.ecoprov@data$PctForArea <- round(100*rmrgn.ecoprov@data$ForArea.sqkm/sum(rmrgn.ecoprov@data$ForArea.sqkm), 1) # % of all RM forest area in each ecoprov
rmrgn.ecoprov@data
## Plot RM region ecoprovs & save
plot(rm.pf) # takes a few secs
ecoprov.palt <- sapply(ecoprov.pal, FUN = function(x) {sub('(.......)', paste('\\1',as.hexmode(round(0.5*255)), sep=''), x)}) # semitransparent palette 
plot(rmrgn.ecoprov, col=ecoprov.palt, lwd=1.5, add=T)
plot(rm.ep.cty, border='gray30', add=T)
plot(rm.rgn, lwd=2, add=T)
writeOGR(rmrgn.ecoprov, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecological_provinces', driver='ESRI Shapefile', overwrite_layer=T)

#### 8. Area in Northern & Southern Rocky Mountain Counties & Ecological Subsections
## Load SRM & NRM regions (manually created from RM_study_region.shp) & reproject
srm.rgn <- readOGR('Rockies_Landis/spatial/RockiesDelineation','South_RockyMtn_Region')
nrm.rgn <- readOGR('Rockies_Landis/spatial/RockiesDelineation','North_RockyMtn_Region')
srm.rgn <- spTransform(srm.rgn, CRSobj=CRS(rm.crs))
nrm.rgn <- spTransform(nrm.rgn, CRSobj=CRS(rm.crs))
## Subset RM counties intersecting SRM & NRM regions to designate SRM/NRM counties (overlap of 2 SE ID counties)
srm.ctys <- rm.ctys[gIntersects(srm.rgn, rm.ctys, byid = T, returnDense = F)[[1]],]
nrm.ctys <- rm.ctys[gIntersects(nrm.rgn, rm.ctys, byid = T, returnDense = F)[[1]],]
## Dissolve NRM & SRM counties into single polygons
srm.ctys@data$Region <- rep('SRM', nrow(srm.ctys@data))
nrm.ctys@data$Region <- rep('NRM', nrow(nrm.ctys@data))
srm.ctys.dslv <- aggregate(srm.ctys, by='Region')
nrm.ctys.dslv <- aggregate(nrm.ctys, by='Region')
## Load NRM & SRM ecosubsecs (manually selected in ArcGIS from RM_ecosubsecs.shp such that ecosubsecs in the overlapping counties are assigned to one region only)
srm.ess <- readOGR('Rockies_Landis/spatial/InitialCommunities','SRM_ecosubsecs')
nrm.ess <- readOGR('Rockies_Landis/spatial/InitialCommunities','NRM_ecosubsecs')
## Dissolve NRM & SRM ecosubsecs into single polygons
srm.ess.dslv <- aggregate(srm.ess, by='AREA')
nrm.ess.dslv <- aggregate(nrm.ess, by='AREA')
## Clip dissolved SRM & NRM ecosubsecs by dissolved SRM & NRM counties
srm.ess.ctyclip <- gIntersection(srm.ess.dslv, srm.ctys.dslv, byid = T, drop_lower_td = T)
nrm.ess.ctyclip <- gIntersection(nrm.ess.dslv, nrm.ctys.dslv, byid = T, drop_lower_td = T)
row.names(srm.ess.ctyclip) <- row.names(srm.ess.dslv)
row.names(nrm.ess.ctyclip) <- row.names(nrm.ess.dslv)
srm.ess.ctys <- SpatialPolygonsDataFrame(Sr = srm.ess.ctyclip, data = srm.ess.dslv@data)
nrm.ess.ctys <- SpatialPolygonsDataFrame(Sr = nrm.ess.ctyclip, data = nrm.ess.dslv@data)
## Merge SRM & NRM ecosubsec x county areas
rmsr.ess.ctys <- union(srm.ess.ctys, nrm.ess.ctys)
## Plot & export
plot(rm.ep.cty.dslv, lwd=3)
plot(srm.ctys.dslv, add=T)
plot(nrm.ctys.dslv, add=T)
plot(srm.ess.dslv, border='green1', lwd=2, add=T)
plot(nrm.ess.dslv, border='green4', lwd=2, add=T)
plot(srm.ess, border='green1', add=T)
plot(nrm.ess, border='green4', add=T)
plot(srm.ess.ctys, lwd=3, border='red', add=T)
plot(nrm.ess.ctys, lwd=3, border='blue', add=T)
# plot(rm.ess.ctys, lwd=3, border='gold', add=T)
plot(rm.ep.cty.dslv, lwd=3, add=T)
writeOGR(nrm.ctys, dsn='Rockies_Landis/spatial/InitialCommunities', layer='NRM_counties', driver='ESRI Shapefile', overwrite_layer=T)
writeOGR(srm.ctys, dsn='Rockies_Landis/spatial/InitialCommunities', layer='SRM_counties', driver='ESRI Shapefile', overwrite_layer=T)
writeOGR(rmsr.ess.ctys, dsn='Rockies_Landis/spatial/InitialCommunities', layer='RM_ecossecs_x_counties', driver='ESRI Shapefile', overwrite_layer=T)
writeOGR(nrm.ess.ctys, dsn='Rockies_Landis/spatial/InitialCommunities', layer='NRM_ecossecs_x_counties', driver='ESRI Shapefile', overwrite_layer=T)
writeOGR(srm.ess.ctys, dsn='Rockies_Landis/spatial/InitialCommunities', layer='SRM_ecossecs_x_counties', driver='ESRI Shapefile', overwrite_layer=T)