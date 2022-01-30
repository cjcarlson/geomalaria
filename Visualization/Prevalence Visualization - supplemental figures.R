
library(raster); library(tidyverse); library(fasterize); library(rgdal); library(sf); library(magrittr)

setwd('C:/Users/cjcar/Dropbox/GeoMalaria')

# GENERATE THE REGIONS

SHAPEFILE <- st_read(layer="GBD-Adjusted",dsn='D:/DECIMALS/Regions')

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

# LOAD IN THE APPROPRIATE LAYERS AND CLIP

fal00 <- raster("Revision/The new analysis/2020_GBD2019_Global_PfPR_2000.tif")
fal19 <- raster("Revision/The new analysis/2020_GBD2019_Global_PfPR_2019.tif")

blank <- fal00*0
SSA <- fasterize(SHAPEFILE[SHAPEFILE$Region %in% SSA,], blank)*0
LAM <- fasterize(SHAPEFILE[SHAPEFILE$Region %in% LAM,], blank)*0
SEA <- fasterize(SHAPEFILE[SHAPEFILE$Region %in% SEA,], blank)*0

fal00 <- trim(fal00+SSA)
fal19 <- trim(fal19+SSA)
diff <- fal19 - fal00

r4p <- (raster("CurrentR0Grids/r8r0-2020.tif") + 
        raster("CurrentR0Grids/r4r0-2020.tif") +
        raster("CurrentR0Grids/g3r0-2020.tif") +
        raster("CurrentR0Grids/glr0-2020.tif"))/4
r4f <- resample(r4p, fal00)
r4f <- r4f+SSA

# Check the correlation structure 

df <- tibble(r0 = values(r4f),
             pr00 = values(fal00),
             pr19 = values(fal19))

df %>% drop_na -> df
cor(df, method = "spearman")

# VIVAX IS HARDER

viv00 <- raster("Revision/The new analysis/2020_GBD2019_Global_PvPR_2000.tif")
viv19 <- raster("Revision/The new analysis/2020_GBD2019_Global_PvPR_2019.tif")

viv00lam <- trim(viv00 + LAM)
viv00sea <- trim(viv00 + SEA)
viv19lam <- trim(viv19 + LAM)
viv19sea <- trim(viv19 + SEA)

difflam <- viv19lam-viv00lam
diffsea <- viv19sea-viv00sea

r4lam <- resample(r4p, viv00lam)
r4lam <- r4lam + LAM

dflam <- tibble(r0 = values(r4lam),
                pr00 = values(viv00lam),
                pr19 = values(viv19lam))
dflam %>% drop_na -> dflam
cor(dflam, method = "spearman")

r4sea <- resample(r4p, viv00sea)
r4sea <- r4sea + SEA

dfsea <- tibble(r0 = values(r4sea),
                pr00 = values(viv00sea),
                pr19 = values(viv19sea))
dfsea %>% drop_na -> dfsea
cor(dfsea, method = "spearman")

# Make some maps

library(scales)
palr0t <- colorRampPalette(viridis_pal(option = "viridis")(10))(50)
palboy <- colorRampPalette(viridis_pal(option = "plasma")(10))(50)
paldiv <- rev(colorRampPalette(RColorBrewer::brewer.pal(10, "RdBu"))(50))

par(oma = c(1,1,1,1), mar = c(3,3,3,2))  
par(mfrow = c(2,2))

plot(fal00, main = expression('Prevalence (2000)'), col = palr0t)
plot(fal19, main = expression('Prevalence (2019)'), col = palr0t)
zl <- max(abs(na.omit(values(diff))))
plot(diff, main = expression('Change in prevalence (2019-2000)'), col = paldiv, zlim = c(-zl, zl))
plot(r4f, main = expression('Predicted R'[0]*'(T) (2020)'), col = palboy)

plot(viv00sea, main = expression('Prevalence (2000)'), col = palr0t)
plot(viv19sea, main = expression('Prevalence (2019)'), col = palr0t)
zl <- max(abs(na.omit(values(diffsea))))
plot(diffsea, main = expression('Change in prevalence (2019-2000)'), col = paldiv, zlim = c(-zl, zl))
plot(r4sea, main = expression('Predicted R'[0]*'(T) (2020)'), col = palboy)

plot(viv00lam, main = expression('Prevalence (2000)'), col = palr0t)
plot(viv19lam, main = expression('Prevalence (2019)'), col = palr0t)
zl <- max(abs(na.omit(values(difflam))))
plot(difflam, main = expression('Change in prevalence (2019-2000)'), col = paldiv, zlim = c(-zl, zl))
plot(r4lam, main = expression('Predicted R'[0]*'(T) (2020)'), col = palboy)