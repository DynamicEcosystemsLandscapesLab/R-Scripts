library(raster)
library(tidyverse)
library(rgdal)
library(dplyr)
library(sf)
library(spatial.tools)
library(gstat)

gss_layers <- sf::st_layers(dsn = "E:/TCSI/Inputs/Soils/SSURGO/soils_GSSURGO_mbr_3713803_01/soils/gssurgo_g_ca/gSSURGO_CA.gdb")
#print(gss_layers)

gssurgo_all <- sf::st_read(dsn = "E:/TCSI/Inputs/Soils/SSURGO/soils_GSSURGO_mbr_3713803_01/soils/gssurgo_g_ca/gSSURGO_CA.gdb", layer = "Valu1")
gssurgo_muagg <- sf::st_read(dsn = "E:/TCSI/Inputs/Soils/SSURGO/soils_GSSURGO_mbr_3713803_01/soils/gssurgo_g_ca/gSSURGO_CA.gdb", layer = "muaggatt")
gssurgo_comp <- sf::st_read(dsn = "E:/TCSI/Inputs/Soils/SSURGO/soils_GSSURGO_mbr_3713803_01/soils/gssurgo_g_ca/gSSURGO_CA.gdb", layer = "component")
gssurgo_chorizon <- sf::st_read(dsn = "E:/TCSI/Inputs/Soils/SSURGO/soils_GSSURGO_mbr_3713803_01/soils/gssurgo_g_ca/gSSURGO_CA.gdb", layer = "chorizon")
gssurgo_comprest <- sf::st_read(dsn = "E:/TCSI/Inputs/Soils/SSURGO/soils_GSSURGO_mbr_3713803_01/soils/gssurgo_g_ca/gSSURGO_CA.gdb", layer = "corestrictions")

gssurgo <- raster("E:/EPIC/Klamath_inputs/MapunitRaster_10m_klamath_150m.tif")
gssurgo@extent <- sierra_mask@extent
gssurgo@crs <- sierra_mask@crs
gssurgo
writeRaster(gssurgo, "E:/EPIC/Klamath_inputs/MapunitRaster_10m_klamath_150m_2.tif")

gssurgo <- readGDAL("E:/EPIC/Klamath_inputs/MapunitRaster_10m_klamath_150m_2.tif")
plot(gssurgo)
#gbbox

sierra_mask <- raster("E:/EPIC/Klamath_inputs/klamath_mask_CAonly_150m.tif")
sierra_mask[sierra_mask > 0] <- 1
plot(sierra_mask)
sierra_mask
kcrs <-sierra_mask@crs

ggssurgo_m <- as.data.frame(gssurgo, xy = T)
colnames(ggssurgo_m) <- c("mukey","x", "y")
ggssurgo_m$mukey <- as.character(ggssurgo_m$mukey) 
ggssurgo_m <- left_join(ggssurgo_m, gssurgo_all, by = "mukey")
ggssurgo_m <- left_join(ggssurgo_m, gssurgo_muagg, by = "mukey")
ggssurgo_m <- left_join(ggssurgo_m, gssurgo_comp, by = "mukey")
#ggssurgo_m <- ggssurgo_m %>% drop_na(mukey)
glimpse(ggssurgo_m)

which(colnames(ggssurgo_m) == "cokey")

ggssurgo_mukey <- ggssurgo_m[,c(1:3,207)]
ggssurgo_c <- left_join(ggssurgo_mukey, gssurgo_chorizon, by = "cokey")
ggssurgo_comprest <- left_join(ggssurgo_mukey, gssurgo_comprest, by = "cokey")
colnames(ggssurgo_c)
glimpse(ggssurgo_c)
colnames(ggssurgo_m)
colnames(ggssurgo_comprest)

####SAND###############
perc_sand <- ggssurgo_c[,c(2:3, which(colnames(ggssurgo_c) == "sandtotal_r"))]
psr <- rasterFromXYZ(perc_sand, crs = kcrs)
psr <- resample(psr, sierra_mask)
psr
plot(psr)
#psr <- modify_raster_margins(psr, extent_delta = c(0,1,0,1), value = NA)
psr[psr == 0 ] <- 1

csmean <- cellStats(psr, "mean")
psr[is.na(psr)] <- 0

psr2 <- mask(psr, sierra_mask)
plot(psr2)
freq(psr2)
psr2[psr2 == 0] <- csmean

psr3 <- psr2/100
writeRaster(psr3, "E:/EPIC/Klamath_inputs/soils/percsand.tif", datatype = "FLT4S")

####CLAY###################
perc_clay <- ggssurgo_c[,c(2:3, which(colnames(ggssurgo_c) == "claytotal_r"))]
psr <- rasterFromXYZ(perc_clay, crs = kcrs)
psr <- resample(psr, sierra_mask)
#psr <- modify_raster_margins(psr, extent_delta = c(0,1,0,1), value = NA)
psr[psr == 0 ] <- 1

csmean <- cellStats(psr, "mean")
psr[is.na(psr)] <- 0

psr2 <- mask(psr, sierra_mask)
psr2[psr2 == 0] <- csmean

psr3 <- psr2/100
plot(psr3)
writeRaster(psr3, "E:/EPIC/Klamath_inputs/soils/percclay.tif", datatype = "FLT4S")



###wilt_point####
wp <- ggssurgo_c[,c(2:3, which(colnames(ggssurgo_c) == "wfifteenbar_r"))]
psr <- rasterFromXYZ(wp, crs = kcrs)
psr <- resample(psr, sierra_mask)
#psr <- modify_raster_margins(psr, extent_delta = c(0,1,0,1), value = NA)
psr[psr == 0 ] <- 1

csmean <- cellStats(psr, "mean")
psr[is.na(psr)] <- 0

psr2 <- mask(psr, sierra_mask)
psr2[psr2 == 0] <- csmean

psr3 <- psr2/100
psr3
plot(psr3)
writeRaster(psr3, "E:/EPIC/Klamath_inputs/soils/wiltpoint.tif", datatype = "FLT4S")

####field_cap#######
fc <- ggssurgo_c[,c(2:3, which(colnames(ggssurgo_c) == "wthirdbar_r"))]
psr <- rasterFromXYZ(fc, crs = kcrs)
psr <- resample(psr, sierra_mask)
#psr <- modify_raster_margins(psr, extent_delta = c(0,1,0,1), value = NA)
psr[psr == 0 ] <- 1

csmean <- cellStats(psr, "mean")
psr[is.na(psr)] <- 0

psr2 <- mask(psr, sierra_mask)
psr2[psr2 == 0] <- csmean

psr3 <- psr2/100
psr3
plot(psr3)
writeRaster(psr3, "E:/EPIC/Klamath_inputs/soils/fieldcap.tif", datatype = "FLT4S")

####SOIL_DRAIN############
drain_class <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "drainagecl"))]
levels(as.factor(drain_class$drainagecl))

drain_class$drainagecl[drain_class$drainagecl == "Excessively drained"] <- 1
drain_class$drainagecl[drain_class$drainagecl == "Somewhat excessively drained"] <- 0.875
drain_class$drainagecl[drain_class$drainagecl == "Well drained"] <- 0.75
drain_class$drainagecl[drain_class$drainagecl == "Moderately well drained"] <- 0.625
drain_class$drainagecl[drain_class$drainagecl == "Somewhat poorly drained"] <- 0.375
drain_class$drainagecl[drain_class$drainagecl == "Poorly drained"] <- 0.25
drain_class$drainagecl[drain_class$drainagecl == "Very poorly drained"] <- 0.125
drain_class$drainagecl[drain_class$drainagecl == "Subaqueous"] <- 0
drain_class$drainagecl <- as.numeric(drain_class$drainagecl)

psr <- rasterFromXYZ(drain_class, crs = kcrs)
psr <- resample(psr, sierra_mask)
#psr <- modify_raster_margins(psr, extent_delta = c(0,1,0,1), value = NA)
freq(psr)
psr[psr == 0 ] <- 0.0001

csmean <- cellStats(psr, "mean")
csmean
psr[is.na(psr)] <- 0

psr2 <- mask(psr, sierra_mask)
psr2[psr2 == 0] <- csmean

psr3 <- psr2
psr3
psr3[psr3 == 0.0001 ] <- 0
plot(psr3)
writeRaster(psr3, "E:/EPIC/Klamath_inputs/soils/soildrain.tif", datatype = "FLT4S")

###soil_depth####
depth <- ggssurgo_comprest[,c(2:3, which(colnames(ggssurgo_comprest) == "resdept_r"))]
psr <- rasterFromXYZ(depth, crs = kcrs)
psr <- resample(psr, sierra_mask)
#psr <- modify_raster_margins(psr, extent_delta = c(0,1,0,1), value = NA)
psr[psr == 0 ] <- 1

csmean <- cellStats(psr, "mean")
psr[is.na(psr)] <- 0

psr2 <- mask(psr, sierra_mask)
psr2[psr2 == 0] <- csmean

psr3 <- psr2
plot(psr3)
writeRaster(psr3, "E:/EPIC/Klamath_inputs/soils/depth.tif", datatype = "INT2S")

###base_frac + storm_frac#########
plot(sierra_mask)
basefrac <- sierra_mask * 0.01
plot(basefrac)

writeRaster(basefrac, "E:/EPIC/Klamath_inputs/soils/basefrac.tif", datatype = "FLT4S")

storm_frac <- basefrac

writeRaster(storm_frac, "E:/EPIC/Klamath_inputs/soils/stormfrac.tif", datatype = "FLT4S")

###som1csoil######
som1c_05 <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "soc0_5"))]
som1c_520 <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "soc5_20"))]
psr5 <- rasterFromXYZ(som1c_05, crs = kcrs)
psr5 <- resample(psr, sierra_mask)
#psr5 <- modify_raster_margins(psr5, extent_delta = c(0,1,0,1), value = NA)
#psr5[psr5 == 0 ] <- 1
plot(psr5)
csmean <- cellStats(psr5, "mean")
csmean
psr5[is.na(psr5)] <- 0

psr6 <- mask(psr5, sierra_mask)
plot(psr6)
freq(psr6)
psr6[psr6 == 0] <- csmean
plot(psr6)

psr20 <- rasterFromXYZ(som1c_520, crs = kcrs)
psr20 <- resample(psr20, sierra_mask)
#psr20 <- modify_raster_margins(psr20, extent_delta = c(0,1,0,1), value = NA)
psr20[psr20 == 0 ] <- 1
plot(psr20)
csmean <- cellStats(psr20, "mean")
psr20[is.na(psr20)] <- 0

psr21 <- mask(psr20, sierra_mask)
plot(psr21)
freq(psr21)
psr21[psr21 == 0] <- csmean
plot(psr21)

som1csoil <- psr6 + psr21
plot(som1csoil)

writeRaster(som1csoil, "E:/EPIC/Klamath_inputs/soils/som1csoil.tif", datatype = "INT2S")

####SOM2c soil ###############
som1c_50 <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "soc20_50"))]
som1c_100 <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "soc50_100"))]
som1c_150 <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "soc100_150"))]
psr50 <- rasterFromXYZ(som1c_50, crs = kcrs)
psr50 <- resample(psr50, sierra_mask)
#psr50 <- modify_raster_margins(psr50, extent_delta = c(0,1,0,1), value = NA)
psr50[psr50 == 0 ] <- 1
plot(psr50)
csmean <- cellStats(psr50, "mean")
psr50[is.na(psr50)] <- 0

psr60 <- mask(psr50, sierra_mask)
plot(psr60)
freq(psr60)
psr60[psr60 == 0] <- csmean
plot(psr60)

psr100 <- rasterFromXYZ(som1c_100, crs = kcrs)
psr100 <- resample(psr100, sierra_mask)
#psr100 <- modify_raster_margins(psr100, extent_delta = c(0,1,0,1), value = NA)
psr100[psr100 == 0 ] <- 1
plot(psr100)
csmean <- cellStats(psr100, "mean")
psr100[is.na(psr100)] <- 0

psr101 <- mask(psr100, sierra_mask)
plot(psr101)
freq(psr101)
psr101[psr101 == 0] <- csmean
plot(psr101)

psr150 <- rasterFromXYZ(som1c_150, crs = kcrs)
psr150 <- resample(psr150, sierra_mask)
#psr150 <- modify_raster_margins(psr150, extent_delta = c(0,1,0,1), value = NA)
psr150[psr150 == 0 ] <- 1
plot(psr150)
csmean <- cellStats(psr150, "mean")
psr150[is.na(psr150)] <- 0

psr151 <- mask(psr150, sierra_mask)
plot(psr151)
freq(psr151)
psr151[psr151 == 0] <- csmean
plot(psr151)

som2csoil <- psr60 + psr101 + psr151
plot(som2csoil)

writeRaster(som2csoil, "E:/EPIC/Klamath_inputs/soils/som2csoil.tif", datatype = "INT2S")

###som3c############
som1c_50 <- ggssurgo_m[,c(2:3, which(colnames(ggssurgo_m) == "soc150_999"))]
psr50 <- rasterFromXYZ(som1c_50, crs = kcrs)
psr50 <- resample(psr50, sierra_mask)
#psr50 <- modify_raster_margins(psr50, extent_delta = c(0,1,0,1), value = NA)
psr50[psr50 == 0 ] <- 1
plot(psr50)
csmean <- cellStats(psr50, "mean")
psr50[is.na(psr50)] <- 0

psr60 <- mask(psr50, sierra_mask)
plot(psr60)
freq(psr60)
psr60[psr60 == 0] <- csmean
plot(psr60)
psr60[psr60 > 11837] <- 11837
writeRaster(psr60, "E:/EPIC/Klamath_inputs/soils/som3csoil.tif", datatype = "INT2S")

###som1n#####
som1csoil <- raster("E:/EPIC/Klamath_inputs/soils/som1csoil.tif")
plot(som1csoil)
som1nsoil <- som1csoil/20
plot(som1nsoil)
som1csoil
writeRaster(som1nsoil, "E:/EPIC/Klamath_inputs/soils/som1nsoil.tif", datatype = "INT2S")

###som2n#####
som2csoil <- raster("E:/EPIC/Klamath_inputs/soils/som2csoil.tif")
plot(som2csoil)
som2nsoil <- som2csoil/20
plot(som2nsoil)
som2csoil
writeRaster(som2nsoil, "E:/EPIC/Klamath_inputs/soils/som2nsoil.tif", datatype = "INT2S")

###som3n#####
som3csoil <- raster("E:/EPIC/Klamath_inputs/soils/som3csoil.tif")
plot(som3csoil)
som3nsoil <- som3csoil/20
plot(som3nsoil)
som3csoil
writeRaster(som3nsoil, "E:/EPIC/Klamath_inputs/soils/som3nsoil.tif", datatype = "INT2S")

#####som1csurf####
litter <- raster("E:/TCSI/Inputs/Carbon/RDS-2013-0004/Data/carbon_lt_mg_ha.img")
litter_epic <- projectRaster(litter, sierra_mask)
litter_epic <- mask(litter_epic, sierra_mask)
plot(litter_epic)

som1csurf <- litter_epic*100
plot(som1csurf)

writeRaster(som1csurf, "E:/EPIC/Klamath_inputs/soils/som1csurf.tif", datatype = "INT2S")

###som1nsurf####
som1nsurf <- som1csurf/25
plot(som1nsurf)

writeRaster(som1nsurf, "E:/EPIC/Klamath_inputs/soils/som1nsurf.tif", datatype = "INT2S")

##dwdsurf########
dwd <- raster("E:/TCSI/Inputs/Carbon/RDS-2013-0004/Data/carbon_dd_mg_ha.img")
dwd_epic <- projectRaster(dwd, sierra_mask)
dwd_epic <- mask(dwd_epic, sierra_mask)
plot(dwd_epic)

surfacedead <- dwd_epic*100
plot(surfacedead)

writeRaster(surfacedead, "E:/EPIC/Klamath_inputs/soils/surfacedead.tif", datatype = "INT2S")

###deadcoarseroots####
dcr <- raster("E:/TCSI/Inputs/Carbon/RDS-2013-0004/Data/carbon_bg_mg_ha.img")
dcr_epic <- projectRaster(dcr, sierra_mask)
dcr_epic <- mask(dcr_epic, sierra_mask)
plot(dcr_epic)

dcr_tot <- dcr_epic*100
plot(dcr_tot)

writeRaster(dcr_tot, "E:/EPIC/Klamath_inputs/soils/deadcoarseroots.tif", datatype = "INT2S")

####too memory intensive--cannot allocate vector....####
sierra_mask[is.na(sierra_mask)] <- 0
sierra_mask[sierra_mask == 1] <- 0

grd <- as.data.frame(sierra_mask, xy = T)
names(grd)       <- c("x", "y")
coordinates(grd) <- c("x", "y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

mod1 <- gstat::idw0(formula = sandtotal_r ~ 1, data = psr_sgdf, newdata = grd)
psr3 <- interpolate(sierra_mask, mod1)
freq(psr2)
psr_sgdf <- as(psr2, 'SpatialGridDataFrame')

psr_sgdf@bbox <- sierra_mask@bbox
psr2

#######error thrown due to NAs produced by integer overflow######

library(fields) 
xy <- data.frame(xyFromCell(psr2, 1:ncell(psr2)))
v <- getValues(psr2)
tps <- Tps(xy, v)
p <- raster(r)


