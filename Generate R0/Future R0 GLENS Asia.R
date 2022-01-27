

raster_tibble <- function(df) {
  rasterFromXYZ(df[,c(2,3,1)]) # maybe change the 2,3,1 to a 'lon' 'lat' and a "varname = " argument, and add to westbound?
}

library(raster)
library(tidyverse)
library(tidync)

classer <- read_csv("C:/Users/cjcar/Dropbox/GeoMalaria/R0Curves/map_Anstephensi_Pvivax_08_12_20.csv")

classer %>% mutate(templow = temp - 0.55, 
                   temphigh = temp + 0.05) %>% 
  select(templow, temphigh, R0_med) -> classer

classer[nrow(classer)+1,] <- list(-100,min(classer$templow),0)
classer[nrow(classer)+1,] <- list(max(classer$temphigh),500,0)

outdir <- 'C:/Users/cjcar/Dropbox/GeoMalaria/MeanR0Grids'

for (i in c(1:6)) {
  
  if(i==1) {
    raw <- './tasmean_control/control.001.tasmean_20100101-20990630_1deg.nc'
    outname <- 'Asia-rcp8run1'
  }
  
  if(i==2) {
    raw <- './tasmean_control/control.002.tasmean_20100101-20980811_1deg.nc'
    outname <- 'Asia-rcp8run2'
  }
  
  if(i==3) {
    raw <- './tasmean_control/control.003.tasmean_20100101-21000630_1deg.nc'
    outname <- 'Asia-rcp8run3'
  }
  
  if(i==4) {
    raw <- './tasmean_feedback/feedback.001.tasmean_20200101-20991231_1deg.nc'
    outname <- 'Asia-glens8run1'
  }
  
  if(i==5) {
    raw <- './tasmean_feedback/feedback.002.tasmean_20200101-20991231_1deg.nc'
    outname <- 'Asia-glens8run2'
  }
  
  if(i==6) {
    raw <- './tasmean_feedback/feedback.003.tasmean_20200101-20991231_1deg.nc'
    outname <- 'Asia-glens8run3'
  }

setwd('C:/Users/cjcar/Dropbox/GLENS_DataForColin')
ncmeta::nc_atts(raw, "time") %>% tidyr::unnest(cols = c(value))

tidync(raw) %>%
  hyper_filter(time = time %in% c((2070-2010)*365 + 1:365)) %>%
  hyper_tibble() -> temp

temp %>%
  mutate(TREFHT = (TREFHT - 273.15)) -> temp

temp %>% dplyr::select(TREFHT, lon, lat) %>% 
       raster_tibble()  -> temp.r

r0 <- reclassify(temp.r, as.matrix(classer))

setwd(outdir)
writeRaster(r0, paste(outname, 'tif', sep='.'), overwrite = TRUE)

}

# Switch to vivax/falciparum by region
# Show differences across climate runs
