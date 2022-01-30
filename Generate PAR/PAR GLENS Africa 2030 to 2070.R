
library(tidync)
library(raster)
library(rgdal)
library(sf)

################## THIS SETS THE RIGHT PARTS RUNNING

for (run in c(1:6)) {

  if (run == 1) {
    unstablename <- 'Africa_unstable_R8_1'
    stablename <- 'Africa_stable_R8_1'
    tasmax_name <- 'D:/DECIMALS/GLENS/control_run1_TREFHTMX_20100101-20990630_1deg.nc'
    tasmin_name <- 'D:/DECIMALS/GLENS/control_run1_TREFHTMN_20100101-20990630_1deg.nc'
    precip_name <- 'D:/DECIMALS/GLENS/PRECT_monsum_control_001_2010-2098.nc'
  }

  if (run == 2) {
    unstablename <- 'Africa_unstable_R8_2'
    stablename <- 'Africa_stable_R8_2'
    tasmax_name <- 'D:/DECIMALS/GLENS/control_run2_TREFHTMX_20100101-20980811_1deg.nc'
    tasmin_name <- 'D:/DECIMALS/GLENS/control_run2_TREFHTMN_20100101-20980811_1deg.nc'
    precip_name <- 'D:/DECIMALS/GLENS/control_run2_pr_201001-210006_1deg.nc'
  }

  if (run == 3) {
    unstablename <- 'Africa_unstable_R8_3'
    stablename <- 'Africa_stable_R8_3'
    tasmax_name <- 'D:/DECIMALS/GLENS/control_run3_TREFHTMX_20100101-21000630_1deg.nc'
    tasmin_name <- 'D:/DECIMALS/GLENS/control_run3_TREFHTMN_20100101-21000630_1deg.nc'
    precip_name <- 'D:/DECIMALS/GLENS/control_run3_pr_201001-209808_1deg.nc'
  }

  if (run == 4) {
    unstablename <- 'Africa_unstable_GL_1'
    stablename <- 'Africa_stable_GL_1'
    tasmax_name <- 'D:/DECIMALS/GLENS/feedback_run1_TREFHTMX_20200101-20991231_1deg.nc'
    tasmin_name <- 'D:/DECIMALS/GLENS/feedback_run1_TREFHTMN_202020200101-20991231_1deg.nc'
    precip_name <- 'D:/DECIMALS/GLENS/feedback_run1_pr_20200101-20991231_1deg.nc'
  }

  if (run == 5) {
    unstablename <- 'Africa_unstable_GL_2'
    stablename <- 'Africa_stable_GL_2'
    tasmax_name <- 'D:/DECIMALS/GLENS/feedback_run2_TREFHTMX_20200101-20991231_1deg.nc'
    tasmin_name <- 'D:/DECIMALS/GLENS/feedback_run2_TREFHTMN_202020200101-20991231_1deg.nc'
    precip_name <- 'D:/DECIMALS/GLENS/feedback_run2_pr_20200101-20991231_1deg.nc'
  }

  if (run == 6) {
    unstablename <- 'Africa_unstable_GL_3'
    stablename <- 'Africa_stable_GL_3'
    tasmax_name <- 'D:/DECIMALS/GLENS/feedback_run3_TREFHTMX_20200101-20991231_1deg.nc'
    tasmin_name <- 'D:/DECIMALS/GLENS/feedback_run3_TREFHTMN_202020200101-20991231_1deg.nc'
    precip_name <- 'D:/DECIMALS/GLENS/feedback_run3_pr_20200101-20991231_1deg.nc'
  }

  # Load in some functions

  c2k <- 273.15
  tmax1 <- 34 + c2k
  tmin1 <- 17 + c2k

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

  tidync(tasmax_name) %>% activate("D0") %>% hyper_array() -> times1; plot(times1$time)
  tidync(tasmin_name) %>% activate("D0") %>% hyper_array() -> times2; plot(times2$time)

  range(times1)
  range(times2)

  ### CODE FOR 2020 IS BORKEN (THESE START 2020 FIX IN AM)
  # FUNCTIONS
  #lonrange <- c(144, 247)
  #latrange <- c(-46, 47)

  SHAPEFILE <- readOGR(layer="GBD-Adjusted",dsn='D:/DECIMALS/Regions')

  SSA <- c('Sub-Saharan Africa (East)',
           'Sub-Saharan Africa (Central)',
           'Sub-Saharan Africa (Southern)',
           'Sub-Saharan Africa (West)')

  REGIONS <- SHAPEFILE[SHAPEFILE$Region %in% SSA,]

  ncmeta::nc_atts(tasmax_name, "time") %>% tidyr::unnest(cols = c(value))

  for (year in  c(2030, 2040, 2050, 2060, 2070, 2080)) {
    # run without 2020 for run 1 AND change the 2020 to a 2030 below

    ### MAXS

    # Starts at 2010 for GLENS
    # Starts at 2019 ?? for G3

    tidync(tasmax_name) %>% #activate("D2") %>% hyper_array() %>% as.vector()
      hyper_filter(time = (time %in% c((year-2010)*365 + 1:365))) %>%
      hyper_tibble() -> tasmax

    maxstack <- raster::stack(lapply(c(min(tasmax$time):max(tasmax$time)), function(i) {
      print(i)
      tasmax %>% dplyr::filter(time==i) %>% raster_tibble()}
    ))


    maxstack2 <- (maxstack > tmin1 & maxstack < tmax1)

    ### MINS

    tidync(tasmin_name) %>%
      hyper_filter(time = (time %in% c((year-2010)*365 + 1:365))) %>%
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
      hyper_filter(time = (time %in% c((year-2010)*365 + (1:(365*2))/2))) %>% # The 1:365 takes care of the +15
      hyper_tibble() -> ppt

    pptstack <- raster::stack(lapply(unique(ppt$time), function(i) {
      print(i)
      ppt %>% dplyr::filter(time==i) %>% raster_tibble()}
    ))

    pptstack <- rotate(pptstack)

    if(run==1) {pptstack <- resample(pptstack, stable)}

    if(!(run==1)) {pptstack <- pptstack*86400000} # Add in GLENS
    pptsum <- stackApply(pptstack, indices=rep(1,12), fun=sum)
    mask.p1 <- (pptsum>250)
    #mask.p1 <- raster::rotate(mask.p1)
    plot(mask.p1[[1]]); maps::map('world',add=TRUE)

    unstable <- ((unstable+mask.p1)==2)
    stable <- ((stable+mask.p1)==2)

    plot(unstable[[1]]); maps::map('world',add=TRUE)
    plot(stable[[1]]); maps::map('world',add=TRUE)

    ###########################################################
    ############# REGION TIME
    ###########################################################

    SSA <- c('Sub-Saharan Africa (East)',
             'Sub-Saharan Africa (Central)',
             'Sub-Saharan Africa (Southern)',
             'Sub-Saharan Africa (West)')

    falc <- fastestize(REGIONS, unstable[[1]])
    africaE <- fastestize(REGIONS[REGIONS$Region=='Sub-Saharan Africa (East)',], unstable[[1]])
    africaC <- fastestize(REGIONS[REGIONS$Region=='Sub-Saharan Africa (Central)',], unstable[[1]])
    africaS <- fastestize(REGIONS[REGIONS$Region=='Sub-Saharan Africa (Southern)',], unstable[[1]])
    africaW <- fastestize(REGIONS[REGIONS$Region=='Sub-Saharan Africa (West)',], unstable[[1]])

    unstable.af <- trim(mask(unstable, falc))
    stable.af <- trim(mask(stable, falc))

    plot(unstable.af[[1]])

    ###########################################################
    ############# SSP POPULATIONS
    ###########################################################

    setwd('D:/DECIMALS/Populations')

    ncfilename <- gsub('2020', year, "ssp5_2020.nc")
    ssp <- raster(ncfilename)
    ssp <- aggregate(ssp, 8, fun=sum)
    ssp <- resample(ssp, unstable.af)
    unstable.pop <- unstable.af*ssp
    stable.pop <- stable.af*ssp

    unstable.df <- data.frame(years=c(year),
                              africaE=cellStats(unstable.pop*africaE, stat='sum'),
                              africaC=cellStats(unstable.pop*africaC, stat='sum'),
                              africaS=cellStats(unstable.pop*africaS, stat='sum'),
                              africaW=cellStats(unstable.pop*africaW, stat='sum'))

    stable.df <- data.frame(years=c(year),
                            africaE=cellStats(stable.pop*africaE, stat='sum'),
                            africaC=cellStats(stable.pop*africaC, stat='sum'),
                            africaS=cellStats(stable.pop*africaS, stat='sum'),
                            africaW=cellStats(stable.pop*africaW, stat='sum'))

    if(year==2030){
      big.unstable.df <- unstable.df
      big.stable.df <- stable.df
    } else {
      big.unstable.df <- rbind(big.unstable.df, unstable.df)
      big.stable.df <- rbind(big.stable.df, stable.df)
    }

    # END OF BIG DECADES FOR LOOP

  }

  setwd('C:/Users/cjcar/Desktop/PARblank')
  write.csv(big.unstable.df, paste(unstablename,'.csv',''))
  write.csv(big.stable.df, paste(stablename,'.csv',''))

  rm(list=ls())

} # ends the run loop
