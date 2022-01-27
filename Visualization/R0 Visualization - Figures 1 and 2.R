
setwd('C:/Users/cjcar/Dropbox/GeoMalaria/MeanR0Grids')
#setwd('/Users/christophertrisos/Dropbox/GeoMalaria/MeanR0Grids')

library(raster)
library(gdalUtils) # I added this because it shaves time off projectRaster

g3_1 <- raster('g3run1.tif')
g3_2 <- raster('g3run2.tif')
g3_3 <- raster('g3run3.tif')

r4_1 <- raster('rcp4run1.tif')
r4_2 <- raster('rcp4run2.tif')
r4_3 <- raster('rcp4run3.tif')

g3 <- mean(g3_1, g3_2, g3_3)
r4 <- mean(r4_1, r4_2, r4_3)

#g3 <- rotate(g3)
#r4 <- rotate(r4)

gl_1 <- raster('glens8run1.tif')
gl_2 <- raster('glens8run2.tif')
gl_3 <- raster('glens8run3.tif')

r8_1 <- raster('rcp8run1.tif')
r8_2 <- raster('rcp8run2.tif')
r8_3 <- raster('rcp8run3.tif')

gl <- mean(gl_1, gl_2, gl_3)
r8 <- mean(r8_1, r8_2, r8_3)

#gl <- rotate(gl)
#r8 <- rotate(r8)

# clip to continent

con <- raster('C:/Users/cjcar/Dropbox/GeoMalaria/continents-final.tif')
#con <- raster('/Users/christophertrisos/Dropbox/GeoMalaria/continents-final.tif')
con <- con*0
con <- aggregate(con, 4)

g3 <- g3 + con
gl <- gl + con
r4 <- r4 + con
r8 <- r8 + con

#assign crs to rasters
crs(g3) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(gl) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(r4) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(r8) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#Finally, reproject to winkel tripel or whatever else

#pick a projection
mollweide<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
wintri<-"+proj=wintri +datum=WGS84 +no_defs +over" #winkel tripel

projchoice = mollweide

writeRaster(g3, 'g3r0.tif', overwrite = TRUE)
writeRaster(gl, 'glr0.tif', overwrite = TRUE)
writeRaster(r4, 'r4r0.tif', overwrite = TRUE)
writeRaster(r8, 'r8r0.tif', overwrite = TRUE)

g3 <- gdalwarp(srcfile = 'g3r0.tif',
                   dstfile = 'g3r0-proj.tif', 
                   t_srs = projchoice,
                   output_Raster = TRUE,
                   overwrite = TRUE,
                   verbose = TRUE)

gl <- gdalwarp(srcfile = 'glr0.tif',
               dstfile = 'glr0-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

r4 <- gdalwarp(srcfile = 'r4r0.tif',
               dstfile = 'r4r0-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

r8 <- gdalwarp(srcfile = 'r8r0.tif',
               dstfile = 'r8r0-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

######################## VISUALIZE
library(maps)
library(maptools)
#Cannot have ggplot loaded for this

## Read in a RasterLayer
tmax <- getData('worldclim', var='tmax', res=10)[[6]]

## Create a SpatialLines object
countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))

#function to clean the countries map for reprojection 
#see (https://gis.stackexchange.com/questions/151601/lines-on-reprojected-sp-objects-with-mollweide-projection)
cleanSpLinesForProjection <- function(w){
  slot(w, "lines") <- lapply(slot(countries, "lines"), function(x) {
    coords <- slot(x, "Lines")[[1]]@coords
    
    #get the rows with too large longitude values
    rIDX <- which(coords >= 180, arr.ind=T)[,1]
    
    #if there are some, replace them
    if(length(rIDX>0)) coords <- coords[-rIDX,]
    
    #replace the slot
    slot(x, "Lines")[[1]]@coords <- coords
    x
  })
  
  w
}

#reproject the country outlines
#countries.wintri<-spTransform(countries,wintri)#gives a messy picture
countries<-spTransform(cleanSpLinesForProjection(countries),projchoice) #gives a clean one : )

# make the maps
library(RColorBrewer)
library(rasterVis)
library(viridis)

lattice.options(
  layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=0)),
  layout.widths=list(left.padding=list(x=0), right.padding=list(x=0.1))
)

#changed to plasma theme
mapTheme <- plasmaTheme(region = plasma(10),
                        layout.widths = list(right.padding = 10),
                        axis.line = list(col = "transparent"),
                        tick = list(col = 'transparent'))

p1 <- levelplot(g3,
          main = '(E) G3 scenario (2070)',      
          maxpixels = 1e10,
          margin = FALSE,
          par.settings = mapTheme,
          scales = list(x = list(draw = FALSE),
                        y = list(draw = FALSE)),
          zlim = c(0, 1)) + layer(sp.lines(countries))

p2 <- levelplot(r4,
                main = '(C) RCP 4.5 scenario (2070)',   
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))

p4 <- levelplot(gl,
                main = '(F) GLENS scenario (2070)',   
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))

p5 <- levelplot(r8,
                main = '(D) RCP 8.5 scenario (2070)',   
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))


#https://gist.github.com/johnbaums/306e4b7e69c87b1826db

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


#cols <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(11) #reversed colours becuase not using diverge

#cols <- colorRampPalette(brewer.pal(11, "RdBu"))(11)

mapTheme <- rasterTheme(#region = cols,
                        layout.widths = list(right.padding = 10),
                        axis.line = list(col = "transparent"),
                        tick = list(col = 'transparent'))

p3 <- levelplot(g3 - r4,
                main = '(A) G3 versus RCP 4.5 (2070)',   # main = 'Intervention impact (vs 4.5)',   
                maxpixels = 1e10,
                margin = FALSE,
                at = seq(-1, 1, by = 0.1),
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))
p3 <- diverge0(p3, 'RdBu')

p6 <- levelplot(gl - r8,
                main = '(B) GLENS versus RCP 8.5 (2070)',   # main = 'Intervention impact (vs 8.5)',   
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))
p6 <- diverge0(p6, 'RdBu')


library(gridExtra)
# #png(file=paste0(getwd(),"/R0figure/R0maps.png"))
# grid.arrange(p2, p5, p1, p4, p3, p6, nrow = 3)
# #dev.off()

######################################## ADD 2020


setwd('C:/Users/cjcar/Dropbox/GeoMalaria/CurrentR0Grids')

g3_1 <- raster('g3run1-2020.tif')
g3_2 <- raster('g3run2-2020.tif')
g3_3 <- raster('g3run3-2020.tif')

r4_1 <- raster('rcp4run1-2020.tif')
r4_2 <- raster('rcp4run2-2020.tif')
r4_3 <- raster('rcp4run3-2020.tif')

g3p <- mean(g3_1, g3_2, g3_3)
r4p <- mean(r4_1, r4_2, r4_3)

#g3 <- rotate(g3)
#r4 <- rotate(r4)

gl_1 <- raster('glens8run1-2020.tif')
gl_2 <- raster('glens8run2-2020.tif')
gl_3 <- raster('glens8run3-2020.tif')

r8_1 <- raster('rcp8run1-2020.tif')
r8_2 <- raster('rcp8run2-2020.tif')
r8_3 <- raster('rcp8run3-2020.tif')

glp <- mean(gl_1, gl_2, gl_3)
r8p <- mean(r8_1, r8_2, r8_3)

#gl <- rotate(gl)
#r8 <- rotate(r8)

# clip to continent

con <- raster('C:/Users/cjcar/Dropbox/GeoMalaria/continents-final.tif')
con <- con*0
con <- aggregate(con, 4)

g3p <- g3p + con
glp <- glp + con
r4p <- r4p + con
r8p <- r8p + con

#assign crs to rasters
crs(g3p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(glp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(r4p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(r8p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#Finally, reproject to winkel tripel or whatever else

#pick a projection
mollweide<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
wintri<-"+proj=wintri +datum=WGS84 +no_defs +over" #winkel tripel

projchoice = mollweide

writeRaster(g3p, 'g3r0-2020.tif', overwrite = TRUE)
writeRaster(glp, 'glr0-2020.tif', overwrite = TRUE)
writeRaster(r4p, 'r4r0-2020.tif', overwrite = TRUE)
writeRaster(r8p, 'r8r0-2020.tif', overwrite = TRUE)


g3p <- gdalwarp(srcfile = 'g3r0-2020.tif',
               dstfile = 'g3r0-2020-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

glp <- gdalwarp(srcfile = 'glr0-2020.tif',
               dstfile = 'glr0-2020-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

r4p <- gdalwarp(srcfile = 'r4r0-2020.tif',
               dstfile = 'r4r0-2020-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

r8p <- gdalwarp(srcfile = 'r8r0-2020.tif',
               dstfile = 'r8r0-2020-proj.tif', 
               t_srs = projchoice,
               output_Raster = TRUE,
               overwrite = TRUE,
               verbose = TRUE)

#changed to plasma theme
mapTheme <- plasmaTheme(region = plasma(10),
                        layout.widths = list(right.padding = 10),
                        axis.line = list(col = "transparent"),
                        tick = list(col = 'transparent'))

p7 <- levelplot(r4p,
                main = '(A) RCP 4.5 present day (2020)',      
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))

p8 <- levelplot(r8p,
                main = '(B) RCP 8.5 present day (2020)',   
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))

grid.arrange(p7, p8, p2, p5, p1, p4, nrow = 3)

# plot(gl - r8p)
# plot(g3 - r4p)

p9 <- levelplot(g3 - r4p,
                main = '(C) G3 (2070) versus present day (2020)',      
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))

p10 <- levelplot(gl - r8p,
                main = '(D) GLENS (2070) versus present day (2020)',   
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = mapTheme,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1)) + layer(sp.lines(countries))

p9 <- diverge0(p9, 'RdBu')
p10 <- diverge0(p10, 'RdBu')

grid.arrange(p3, p6, p9, p10, nrow = 2)
 