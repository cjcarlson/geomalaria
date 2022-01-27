
library(sp)
library(sf)
library(raster)
library(fasterize)
library(rgdal)

fastestize <- function(poly,raster) {
  require(fasterize)
  y <- disaggregate(raster,10)
  p <- st_as_sf(poly)
  z <- fasterize(p,y)
  x <- raster::aggregate(z,10,fun='sum')/100
  return(x)
}

SHAPEFILE <- readOGR(layer="GBD-Adjusted",dsn='D:/DECIMALS/Regions')

SSA <- c('Sub-Saharan Africa (East)', 
         'Sub-Saharan Africa (Central)',
         'Sub-Saharan Africa (Southern)', 
         'Sub-Saharan Africa (West)')

LAM <- c('Latin America (Tropical)',
         'Latin America (Central)',
         'Latin America (Andean)')

SEA <- c('Asia (South)', 
         'Asia (Southeast)',
         'Asia (East)')


setwd('C:/Users/cjcar/Dropbox/GeoMalaria/MeanR0Grids')

blank <- rotate(raster('Africa-g3run1.tif')*0)

ALL <- fastestize(SHAPEFILE[SHAPEFILE$Region %in% c(SSA,LAM,SEA),], blank)*0
SSA <- fastestize(SHAPEFILE[SHAPEFILE$Region %in% SSA,], blank)*0
LAM <- fastestize(SHAPEFILE[SHAPEFILE$Region %in% LAM,], blank)*0
SEA <- fastestize(SHAPEFILE[SHAPEFILE$Region %in% SEA,], blank)*0

############ Iterative chunk for cobbling

for (name in c('g3run1','g3run2','g3run3',
                 'rcp4run1','rcp4run2','rcp4run3',
                 'glens8run1','glens8run2','glens8run3',
                 'rcp8run1','rcp8run2','rcp8run3')) {
  
  tifname <- paste(name,'.tif',sep='')
  part1 <- rotate(raster(paste('Africa',tifname,sep='-'))) 
  part2 <- rotate(raster(paste('LatAm',tifname,sep='-'))) 
  part3 <- rotate(raster(paste('Asia',tifname,sep='-'))) 
  part1 <- part1 + SSA
  part2 <- part2 + LAM
  part3 <- part3 + SEA
  whole <- sum(part1, part2, part3, na.rm = TRUE) + ALL
  writeRaster(whole, tifname)
  
}
