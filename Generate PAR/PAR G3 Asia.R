
library(tidync) 
library(raster)
library(rgdal)
library(sf)

################## THIS SETS THE RIGHT PARTS RUNNING

for (run in c(1:6)) {

if (run == 1) {
unstablename <- 'Asia_unstable_G3_1'
stablename <- 'Asia_stable_G3_1'
tasmax_name <- 'D:/DECIMALS/Temperature/G3 tasmax/tasmax_day_HadGEM2-ES_G3_r1i1p1_20191201-20891230_1deg.nc'
tasmin_name <- 'D:/DECIMALS/Temperature/G3 tasmin/tasmin_day_HadGEM2-ES_G3_r1i1p1_20191201-20891230_1deg.nc'
precip_name <- 'D:/DECIMALS/Precip/G3/pr_Amon_HadGEM2-ES_G3_r1i1p1_201912-208912_Month1deg.nc'
}

if (run == 2) {
unstablename <- 'Asia_unstable_G3_2'
stablename <- 'Asia_stable_G3_2'
tasmax_name <- 'D:/DECIMALS/Temperature/G3 tasmax/tasmax_day_HadGEM2-ES_G3_r2i1p1_20191201-20891230_1deg.nc'
tasmin_name <- 'D:/DECIMALS/Temperature/G3 tasmin/tasmin_day_HadGEM2-ES_G3_r2i1p1_20191201-20891230_1deg.nc'
precip_name <- 'D:/DECIMALS/Precip/G3/pr_Amon_HadGEM2-ES_G3_r2i1p1_201912-208912_Month1deg.nc'
}

if (run == 3) {
unstablename <- 'Asia_unstable_G3_3'
stablename <- 'Asia_stable_G3_3'
tasmax_name <- 'D:/DECIMALS/Temperature/G3 tasmax/tasmax_day_HadGEM2-ES_G3_r3i1p1_20191201-20891230_1deg.nc'
tasmin_name <- 'D:/DECIMALS/Temperature/G3 tasmin/tasmin_day_HadGEM2-ES_G3_r3i1p1_20191201-20891230_1deg.nc'
precip_name <- 'D:/DECIMALS/Precip/G3/pr_Amon_HadGEM2-ES_G3_r3i1p1_201912-208912_Month1deg.nc'
}

if (run == 4) {
unstablename <- 'Asia_unstable_R4_1'
stablename <- 'Asia_stable_R4_1'
tasmax_name <- 'D:/DECIMALS/Temperature/RCP45 tasmax/tasmax_day_HadGEM2-ES_rcp45_r1i1p1_20051201-20991230_1deg.nc'
tasmin_name <- 'D:/DECIMALS/Temperature/RCP45 tasmin/tasmin_day_HadGEM2-ES_rcp45_r1i1p1_20051201-20991230_1deg.nc'
precip_name <- 'D:/DECIMALS/Precip/RCP45/pr_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-212411_Month1deg.nc'
}    
  
if (run == 5) {
unstablename <- 'Asia_unstable_R4_2'
stablename <- 'Asia_stable_R4_2'
tasmax_name <- 'D:/DECIMALS/Temperature/RCP45 tasmax/tasmax_day_HadGEM2-ES_rcp45_r2i1p1_20051201-21001230_1deg.nc'
tasmin_name <- 'D:/DECIMALS/Temperature/RCP45 tasmin/tasmin_day_HadGEM2-ES_rcp45_r2i1p1_20051201-21001230_1deg.nc'
precip_name <- 'D:/DECIMALS/Precip/RCP45/pr_Amon_HadGEM2-ES_rcp45_r2i1p1_200512-210012_Month1deg.nc'
}   
     
if (run == 6) {
unstablename <- 'Asia_unstable_R4_3'
stablename <- 'Asia_stable_R4_3'
tasmax_name <- 'D:/DECIMALS/Temperature/RCP45 tasmax/tasmax_day_HadGEM2-ES_rcp45_r3i1p1_20051201-21001230_1deg.nc'
tasmin_name <- 'D:/DECIMALS/Temperature/RCP45 tasmin/tasmin_day_HadGEM2-ES_rcp45_r3i1p1_20051201-21001230_1deg.nc'
precip_name <- 'D:/DECIMALS/Precip/RCP45/pr_Amon_HadGEM2-ES_rcp45_r3i1p1_200512-210012_Month1deg.nc'
}

# Load in some functions

c2k <- 273.15
tmax1 <- 32.5 + c2k
tmin1 <- 15.7 + c2k

fastestize <- function(poly,raster) {
  require(fasterize)
  y <- disaggregate(raster,10)
  p <- st_as_sf(poly)
  z <- fasterize(p,y)
  x <- raster::aggregate(z,10,fun='sum')/100
  return(x)
}

raster_tibble <- function(df) {
  rasterFromXYZ(df[,c(2,3,1)])
}

# D2 for G3, D0 for GLENS

tidync(tasmax_name) %>% activate("D2") %>% hyper_array() -> times1; plot(times1$time)
tidync(tasmin_name) %>% activate("D2") %>% hyper_array() -> times2; plot(times2$time)

range(times1)
range(times2)

### CODE FOR 2020 IS BORKEN (THESE START 2020 FIX IN AM)
# FUNCTIONS
#lonrange <- c(144, 247)
#latrange <- c(-46, 47)

SHAPEFILE <- readOGR(layer="GBD-Adjusted",dsn='D:/DECIMALS/Regions')

SSA <- c('Asia (South)', 
         'Asia (Southeast)',
         'Asia (East)')

REGIONS <- SHAPEFILE[SHAPEFILE$Region %in% SSA,]

ncmeta::nc_atts(tasmax_name, "time") %>% tidyr::unnest(cols = c(value))


for (year in c(2020, 2030, 2040, 2050, 2060, 2070, 2080)) {
# run without 2020 for run 1 AND change the 2020 to a 2030 below

### MAXS

# Starts at 2010 for GLENS
# Starts at 2019 ?? for G3
  
tidync(tasmax_name) %>% #activate("D2") %>% hyper_array() %>% as.vector()
  hyper_filter(time = (time %in% c((year-1860)*360 + 30.5 + 1:365))) %>%
  hyper_tibble() -> tasmax

maxstack <- raster::stack(lapply(c(min(tasmax$time):max(tasmax$time)), function(i) {
  print(i)
  tasmax %>% dplyr::filter(time==i) %>% raster_tibble()}
))


maxstack2 <- (maxstack > tmin1 & maxstack < tmax1)

### MINS

tidync(tasmin_name) %>% 
  hyper_filter(time = (time %in% c((year-1860)*360 + 30.5 + 1:365))) %>% # starts in 2010
  hyper_tibble() -> tasmin


minstack <- raster::stack(lapply(c(min(tasmin$time):max(tasmin$time)), function(i) {
  print(i) 
  tasmin %>% dplyr::filter(time==i) %>% raster_tibble()}
))

minstack2 <- (minstack > tmin1 & minstack < tmax1)

### COMBINE

mask.two <- ((maxstack2 + minstack2)==2)
mask.two <- stackApply(mask.two, indices=rep(1,365), fun=sum)
mask.two <- raster::rotate(mask.two)
plot(mask.two)

stable <- mask.two>180
unstable <- mask.two>30

###############################################################################
###############################################################################
###############################################################################
###############################################################################

tidync(precip_name) %>% 
  hyper_filter(time = time %in% c((year-1860)*360 + 30 + 1:365)) %>% # The 1:365 takes care of the +15
  hyper_tibble() -> ppt

pptstack <- raster::stack(lapply(unique(ppt$time), function(i) {
  print(i) 
  ppt %>% dplyr::filter(time==i) %>% raster_tibble()}
))

#pptstack <- pptstack*86400000 # Add in GLENS
pptsum <- stackApply(pptstack, indices=rep(1,12), fun=sum)
mask.p1 <- (pptsum>250)
mask.p1 <- raster::rotate(mask.p1)
plot(mask.p1[[1]]); maps::map('world',add=TRUE)

unstable <- ((unstable+mask.p1)==2)
stable <- ((stable+mask.p1)==2)

plot(unstable[[1]]); maps::map('world',add=TRUE)
plot(stable[[1]]); maps::map('world',add=TRUE)

###########################################################
############# REGION TIME
###########################################################

SSA <- c('Asia (South)', 
         'Asia (Southeast)',
         'Asia (East)')

vivax <- fastestize(REGIONS, unstable[[1]])
asiaS <- fastestize(REGIONS[REGIONS$Region=='Asia (South)',], unstable[[1]])
asiaSE <- fastestize(REGIONS[REGIONS$Region=='Asia (Southeast)',], unstable[[1]])
asiaE <- fastestize(REGIONS[REGIONS$Region=='Asia (East)',], unstable[[1]])

unstable.af <- trim(mask(unstable, vivax))
stable.af <- trim(mask(stable, vivax))

plot(unstable.af[[1]])

###########################################################
############# SSP POPULATIONS
###########################################################

setwd('D:/DECIMALS/Populations')

ncfilename <- gsub('2020', year, "ssp2_2020.nc")
ssp <- raster(ncfilename)
ssp <- aggregate(ssp, 8, fun=sum)
ssp <- resample(ssp, unstable.af)
unstable.pop <- unstable.af*ssp
stable.pop <- stable.af*ssp

unstable.df <- data.frame(years=c(year),
                          asiaS=cellStats(unstable.pop*asiaS, stat='sum'),
                          asiaSE=cellStats(unstable.pop*asiaSE, stat='sum'),
                          asiaE=cellStats(unstable.pop*asiaE, stat='sum'))

stable.df <- data.frame(years=c(year),
                          asiaS=cellStats(stable.pop*asiaS, stat='sum'),
                          asiaSE=cellStats(stable.pop*asiaSE, stat='sum'),
                          asiaE=cellStats(stable.pop*asiaE, stat='sum'))

if(year==2020){
    big.unstable.df <- unstable.df
    big.stable.df <- stable.df 
} else {
    big.unstable.df <- rbind(big.unstable.df, unstable.df)
    big.stable.df <- rbind(big.stable.df, stable.df)
}

# END OF BIG DECADES FOR LOOP 

}

setwd('C:/Users/cjcar/Dropbox/GeoMalaria/PARTables')
write.csv(big.unstable.df, paste(unstablename,'.csv',''))
write.csv(big.stable.df, paste(stablename,'.csv',''))

rm(list=ls())

} # ends the run loop
