
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

outdir <- 'C:/Users/cjcar/Dropbox/GeoMalaria/CurrentR0Grids'

for (i in c(1:6)) {
  
  if(i==1) {
    raw <- './tasmean_control/control.001.tasmean_20100101-20990630_1deg.nc'
    outname <- 'Africa-rcp8run1-2020'
  }
  
  if(i==2) {
    raw <- './tasmean_control/control.002.tasmean_20100101-20980811_1deg.nc'
    outname <- 'Africa-rcp8run2-2020'
  }
  
  if(i==3) {
    raw <- './tasmean_control/control.003.tasmean_20100101-21000630_1deg.nc'
    outname <- 'Africa-rcp8run3-2020'
  }
  
  if(i==4) {
    raw <- './tasmean_feedback/feedback.001.tasmean_20200101-20991231_1deg.nc'
    outname <- 'Africa-glens8run1-2020'
  }
  
  if(i==5) {
    raw <- './tasmean_feedback/feedback.002.tasmean_20200101-20991231_1deg.nc'
    outname <- 'Africa-glens8run2-2020'
  }
  
  if(i==6) {
    raw <- './tasmean_feedback/feedback.003.tasmean_20200101-20991231_1deg.nc'
    outname <- 'Africa-glens8run3-2020'
  }

setwd('C:/Users/cjcar/Dropbox/GLENS_DataForColin')
ncmeta::nc_atts(raw, "time") %>% tidyr::unnest(cols = c(value))

tidync(raw) %>%
  hyper_filter(time = time %in% c((2020-2010)*365 + 1:365)) %>%
  hyper_tibble() -> temp

temp %>%
  mutate(TREFHT = (TREFHT - 273.15)) -> temp

temp %>% dplyr::select(TREFHT, lon, lat) %>% 
       raster_tibble()  -> temp.r

r0raw <- calc(temp.r, r0t)

r0 <- r0raw/max_r0

setwd(outdir)
writeRaster(r0, paste(outname, 'tif', sep='.'), overwrite = TRUE)

}

# Switch to vivax/falciparum by region
# Show differences across climate runs
