

raster_tibble <- function(df) {
  rasterFromXYZ(df[,c(2,3,1)]) # maybe change the 2,3,1 to a 'lon' 'lat' and a "varname = " argument, and add to westbound?
}

library(raster)
library(tidyverse)
library(tidync)

classer <- read_csv("C:/Users/cjcar/Dropbox/GeoMalaria/R0Curves/map_Angambiae_Pvivax_08_12_20.csv")

classer %>% mutate(templow = temp - 0.55, 
                   temphigh = temp + 0.05) %>% 
  select(templow, temphigh, R0_med) -> classer

classer[nrow(classer)+1,] <- list(-100,min(classer$templow),0)
classer[nrow(classer)+1,] <- list(max(classer$temphigh),500,0)

outdir <- 'C:/Users/cjcar/Dropbox/GeoMalaria/MeanR0Grids'

for (i in c(1:6)) {
  
  if(i==1) {
    raw <- 'tas_day_HadGEM2-ES_G3_r1i1p1_20191201-20891230_1deg.nc'
    outname <- 'LatAm-g3run1'
  }
  
  if(i==2) {
    raw <- 'tas_day_HadGEM2-ES_G3_r2i1p1_20191201-20891230_1deg.nc'
    outname <- 'LatAm-g3run2'
  }
  
  if(i==3) {
    raw <- 'tas_day_HadGEM2-ES_G3_r3i1p1_20191201-20891230_1deg.nc'
    outname <- 'LatAm-g3run3'
  }
  
  if(i==4) {
    raw <- 'tas_day_HadGEM2-ES_rcp45_r1i1p1_20051201-20991230_1deg.nc'
    outname <- 'LatAm-rcp4run1'
  }
  
  if(i==5) {
    raw <- 'tas_day_HadGEM2-ES_rcp45_r2i1p1_20051201-21001230_1deg.nc'
    outname <- 'LatAm-rcp4run2'
  }
  
  if(i==6) {
    raw <- 'tas_day_HadGEM2-ES_rcp45_r3i1p1_20051201-21001230_1deg.nc'
    outname <- 'LatAm-rcp4run3'
  }

setwd('D:/DECIMALS/MeanTemps/')

ncmeta::nc_atts(raw, "time") %>% tidyr::unnest(cols = c(value))

tidync(raw) %>%
  hyper_filter(time = time %in% c((2070-1860)*360 + 29.5 + 1:365)) %>%
  hyper_tibble() -> temp

temp %>%
  mutate(tas = (tas - 273.15)) -> temp

temp %>% dplyr::select(tas, lon, lat) %>% 
  raster_tibble()  -> temp.r

r0 <- reclassify(temp.r, as.matrix(classer))

setwd(outdir)
writeRaster(r0, paste(outname, 'tif', sep='.'))

}

# Switch to vivax/falciparum by region
# Show differences across climate runs
