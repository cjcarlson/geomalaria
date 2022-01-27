
library(tidync) 
library(dplyr)
library(raster)
library(rasterVis)
library(maps)
library(maptools)
library(viridis)

raster_tibble <- function(df) {
  rasterFromXYZ(df[,c(2,3,1)]) # maybe change the 2,3,1 to a 'lon' 'lat' and a "varname = " argument, and add to westbound?
}

s <- lapply(c(1:6), function(i) {
  if(i==1) {
    raw <- './tasmean_control/control.001.tasmean_20100101-20990630_1deg.nc'
  }
  if(i==2) {
    raw <- './tasmean_control/control.002.tasmean_20100101-20980811_1deg.nc'
  }
  if(i==3) {
    raw <- './tasmean_control/control.003.tasmean_20100101-21000630_1deg.nc'
  }
  if(i==4) {
    raw <- './tasmean_feedback/feedback.001.tasmean_20200101-20991231_1deg.nc'
  }
  if(i==5) {
    raw <- './tasmean_feedback/feedback.002.tasmean_20200101-20991231_1deg.nc'
  }
  if(i==6) {
    raw <- './tasmean_feedback/feedback.003.tasmean_20200101-20991231_1deg.nc'
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
  print(i)
  return(temp.r)
})

s2 <- lapply(c(1:6), function(i) {
  if(i==1) {
    raw <- './tasmean_control/control.001.tasmean_20100101-20990630_1deg.nc'
  }
  if(i==2) {
    raw <- './tasmean_control/control.002.tasmean_20100101-20980811_1deg.nc'
  }
  if(i==3) {
    raw <- './tasmean_control/control.003.tasmean_20100101-21000630_1deg.nc'
  }
  if(i==4) {
    raw <- './tasmean_feedback/feedback.001.tasmean_20200101-20991231_1deg.nc'
  }
  if(i==5) {
    raw <- './tasmean_feedback/feedback.002.tasmean_20200101-20991231_1deg.nc'
  }
  if(i==6) {
    raw <- './tasmean_feedback/feedback.003.tasmean_20200101-20991231_1deg.nc'
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
  print(i)
  return(temp.r)
})

s <- stack(s)
rcp8520 <- mean(s[[1:3]])
glens20 <- mean(s[[4:6]])

s2 <- stack(s2)
rcp8570 <- mean(s2[[1:3]])
glens70 <- mean(s2[[4:6]])

## Create a SpatialLines object
countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))

mapTheme <- plasmaTheme(region = plasma(10),
                        #layout.widths = list(right.padding = 10),
                        axis.line = list(col = "transparent"),
                        tick = list(col = 'transparent'))

p.strip <- list(cex=1.5, lines=1)
levelplot(rotate(stack(rcp8520, glens20, rcp8570, glens70)),  
          main = NULL,
          maxpixels = 1e10,
          margin = FALSE,
          par.settings = mapTheme,
          scales = list(x = list(draw = FALSE),
                        y = list(draw = FALSE)),
          names.attr=c("RCP 8.5 (2020)", "GLENS (2020)","RCP 8.5 (2070)", "GLENS (2070)"), 
          par.strip.text=p.strip,
          at = seq(-45, 40, by=1),
          colorkey=list(space = "bottom")) + latticeExtra::layer(sp.lines(countries))


diverge0 <- function(p, ramp) {
  # p: a trellis object resulting from rasterVis::levelplot
  # ramp: the name of an RColorBrewer palette (as character), a character 
  #       vector of colour names to interpolate, or a colorRampPalette.
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in% 
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(rev(brewer.pal(11, ramp))))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp)) 
    stop('ramp should be either the name of a RColorBrewer palette, ', 
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p[[grep('^legend', names(p))]][[1]]$args$key$col <- ramp(1000)[zlim[-length(zlim)]]
  p$panel.args.common$col.regions <- ramp(1000)[zlim[-length(zlim)]]
  p
}

p <- levelplot(rotate(stack(rcp8570 - rcp8520, glens70 - glens20)),  
          main = NULL,
          maxpixels = 1e10,
          margin = FALSE,
          par.settings = mapTheme,
          scales = list(x = list(draw = FALSE),
                        y = list(draw = FALSE)),
          names.attr=c("RCP 8.5 (2070 - 2020)", "GLENS (2070 - 2020)"), 
          par.strip.text=p.strip,
          #at = seq(-45, 40, by=1),
          colorkey=list(space = "bottom")) + latticeExtra::layer(sp.lines(countries))
p <- diverge0(p, 'RdBu')
p