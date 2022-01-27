
r0t <- function(T, na.rm=TRUE) {
  if(is.na(T)) return(NA)
  if(T<=14) {return(0)}
  a = 0.000203*T*(T-11.7)*((42.3-T)^0.5)
  bc = -0.54*T*T + 25.2*T - 206
  p = -0.000828*T*T + 0.0367*T + 0.522 # e^-mu
  mu = -1*log(p)
  PDR = 0.000111*T*(T-14.7)*((34.4-T)^0.5) # 1/EIP
  pEA = -0.00924*T*T + 0.453*T - 4.77
  MDR = 0.000111*T*(T-14.7)*((34-T)^0.5) # 1/tauEA
  EFD = -0.153*T*T + 8.61*T - 97.7
  
  R0 = (((a^2)*bc*(p^(1/PDR))*EFD*pEA*MDR)/(mu^3))^(1/2)
  if(is.nan(R0)){return(0)}
  return(R0)
}

raster_tibble <- function(df) {
  rasterFromXYZ(df[,c(2,3,1)]) # maybe change the 2,3,1 to a 'lon' 'lat' and a "varname = " argument, and add to westbound?
}

temps <- seq(20, 30, by=0.0005)
r0 <- sapply(temps, r0t)
max_r0 <- max(r0)

library(raster)
library(tidyverse)
library(tidync)


outdir <- 'C:/Users/cjcar/Dropbox/GeoMalaria/MeanR0Grids'

for (i in c(1:6)) {
  
  if(i==1) {
    raw <- 'tas_day_HadGEM2-ES_G3_r1i1p1_20191201-20891230_1deg.nc'
    outname <- 'Africa-g3run1'
  }
  
  if(i==2) {
    raw <- 'tas_day_HadGEM2-ES_G3_r2i1p1_20191201-20891230_1deg.nc'
    outname <- 'Africa-g3run2'
  }
  
  if(i==3) {
    raw <- 'tas_day_HadGEM2-ES_G3_r3i1p1_20191201-20891230_1deg.nc'
    outname <- 'Africa-g3run3'
  }
  
  if(i==4) {
    raw <- 'tas_day_HadGEM2-ES_rcp45_r1i1p1_20051201-20991230_1deg.nc'
    outname <- 'Africa-rcp4run1'
  }
  
  if(i==5) {
    raw <- 'tas_day_HadGEM2-ES_rcp45_r2i1p1_20051201-21001230_1deg.nc'
    outname <- 'Africa-rcp4run2'
  }
  
  if(i==6) {
    raw <- 'tas_day_HadGEM2-ES_rcp45_r3i1p1_20051201-21001230_1deg.nc'
    outname <- 'Africa-rcp4run3'
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

r0raw <- calc(temp.r, r0t)

r0 <- r0raw/max_r0

setwd(outdir)
writeRaster(r0, paste(outname, 'tif', sep='.'))

}

# Switch to vivax/falciparum by region
# Show differences across climate runs
