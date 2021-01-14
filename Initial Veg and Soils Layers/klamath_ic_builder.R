##required packages
library(tidyverse)
library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(raster)
library(sf)
library(akima)
library(spatial.tools)

##RILEY et al.'s tree list
tc_lookup <- read.csv("E:/IC/RDS-2019-0026_Data/Data/TL_CN_Lookup.txt")
tree_table <- read.csv("E:/IC/RDS-2019-0026_Data/Data/Tree_table_CONUS.txt")
canv_tree_table <- subset(tree_table, State_Abbreviation == "CA" | State_Abbreviation == "NV" | State_Abbreviation == "WA" | State_Abbreviation == "OR" | State_Abbreviation == "AZ" | State_Abbreviation == "WY" | State_Abbreviation == "ID" | State_Abbreviation == "UT")
rm(tree_table)

##LANDIS species codes, longevity check, species selector based on study area
sierra_sp_long <- read.csv("E:/EPIC/Landis_species_codes.csv")

##raster initial communities map
sierra_tree_list <- raster("E:/EPIC/Klamath_inputs/klamath_tree_list.tif")
#sierra_ecos <- raster("E:/EPIC/Klamath_inputs/klamath_12k_ecoregions_150m.tif")
#sierra_ecos[sierra_ecos > 1] <- 1
#sierra_ecos[sierra_ecos == 0] <- NA
#sierra_tl_mask <- mask(sierra_tree_list, sierra_ecos)
plot(sierra_tree_list)
sierra_tl_mask <- sierra_tree_list
sierra_tl_mask[is.na(sierra_tl_mask)] <- 0
sierra_tl_mask
fstl <- freq(sierra_tl_mask)
stl_mapcodes <- as_tibble(fstl[,1])
colnames(stl_mapcodes) <- "tl_id"
dataType(sierra_tl_mask)
sierra_tl_mask
writeRaster(sierra_tl_mask, "E:/EPIC/Klamath_inputs/klamath_IC.tif", datatype = "INT4S")

##Function to roundup ages to nearest 10
roundUp <- function(x,to=10){
  to*(x%/%to + as.logical(x%%to))
}

##FIA data source
dirFIA <- paste("C:/Users/cjmaxwe3/Downloads/FIA/") 

##lookup table linking FIA species codes and names
spec.codes <- read_csv("C:/Users/cjmaxwe3/Downloads/FIA/REF_SPECIES/REF_SPECIES.csv") %>%
  dplyr::select(SPCD, COMMON_NAME, SPECIES_SYMBOL, GENUS, SPECIES)
shrub.codes <- read_csv(paste0(dirFIA,"CA/landis_shrub_codes.csv")) ##for binning subplots into shrub groups

####CA data############
ca_plp <- read_csv(paste(dirFIA,"CA/CA_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

ca_plots <- read_csv(paste(dirFIA,"CA/CA_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

ca_tb <- read_csv(paste(dirFIA,"CA/CA_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

ca_trees <- read_csv(paste(dirFIA,"CA/CA_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(ca_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(ca_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ* 2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

ca_shrubs <- read_csv(paste(dirFIA,"CA/CA_P2VEG_SUBPLOT_SPP.csv",sep="")) %>%
  dplyr::select(CN,PLT_CN,SUBP,VEG_SPCD,GROWTH_HABIT_CD,LAYER,COVER_PCT) %>%
  filter(GROWTH_HABIT_CD == "SH") %>%
  left_join(shrub.codes, by = "VEG_SPCD") %>%
  filter(!is.na(Type)) %>%
  mutate(growth_layer = case_when(LAYER == 1 ~ 0.25,
                                  LAYER == 2 ~ 0.5,
                                  LAYER == 3 ~ 0.75,
                                  LAYER == 4 ~ 1)) %>%
  mutate(biomass = (1.372014 * COVER_PCT + 2.576618)/10) %>%
  mutate(age = roundUp((COVER_PCT/100 * 40))) %>%
  distinct(Type, age, biomass, .keep_all = T) %>%
  right_join(ca_plots,by="PLT_CN") %>%
  dplyr::select(PLT_CN, LAT, LON, Type, biomass, age) %>%
  filter(!is.na(Type)) %>%
  group_by(PLT_CN, LAT, LON, Type, age) %>%
  summarise(biomass = sum(biomass))


#dplyr::select(tl_id, Type, age, biomass) %>%
#dplyr::rename(CN=PLT_CN) %>%
#  left_join(canv_tree_table, by = c("CN")) %>%
#  filter(!is.na(tl_id)) %>%

####OR data##########
or_plp <- read_csv(paste(dirFIA,"OR/OR_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

or_plots <- read_csv(paste(dirFIA,"OR/OR_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

or_tb <- read_csv(paste(dirFIA,"OR/OR_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

or_trees <- read_csv(paste(dirFIA,"OR/OR_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(or_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(or_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

####WA data##########
wa_plp <- read_csv(paste(dirFIA,"WA/WA_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

wa_plots <- read_csv(paste(dirFIA,"WA/WA_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

wa_tb <- read_csv(paste(dirFIA,"WA/WA_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

wa_trees <- read_csv(paste(dirFIA,"WA/WA_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(wa_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(wa_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

####NV data######
nv_plp <- read_csv(paste(dirFIA,"NV/NV_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

nv_plots <- read_csv(paste(dirFIA,"NV/NV_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

nv_tb <- read_csv(paste(dirFIA,"NV/NV_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

nv_trees <- read_csv(paste(dirFIA,"NV/NV_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(nv_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(nv_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

#####WY data#############
wy_plp <- read_csv(paste(dirFIA,"WY/WY_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

wy_plots <- read_csv(paste(dirFIA,"WY/WY_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

wy_tb <- read_csv(paste(dirFIA,"WY/WY_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

wy_trees <- read_csv(paste(dirFIA,"WY/WY_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(wy_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(wy_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

####AZ data#######
az_plp <- read_csv(paste(dirFIA,"AZ/AZ_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

az_plots <- read_csv(paste(dirFIA,"AZ/AZ_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

az_tb <- read_csv(paste(dirFIA,"AZ/AZ_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

az_trees <- read_csv(paste(dirFIA,"AZ/AZ_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(az_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(az_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

#####ID data#############
id_plp <- read_csv(paste(dirFIA,"ID/ID_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

id_plots <- read_csv(paste(dirFIA,"ID/ID_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

id_tb <- read_csv(paste(dirFIA,"ID/ID_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

id_trees <- read_csv(paste(dirFIA,"ID/ID_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(id_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(id_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ *2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

####AZ data#######
ut_plp <- read_csv(paste(dirFIA,"UT/UT_PLOTSNAP.csv",sep="")) %>%
  dplyr::select(INVYR,CN,EXPVOL,EXPCURR) %>%
  filter(!is.na(EXPVOL)) %>%
  dplyr::rename(PLT_CN=CN) %>%
  distinct(PLT_CN,.keep_all = T)

ut_plots <- read_csv(paste(dirFIA,"UT/UT_Plot.csv",sep="")) %>%
  dplyr::select(CN,ECOSUBCD,LAT,LON,ELEV) %>%
  dplyr::rename(PLT_CN=CN) 

ut_tb <- read_csv(paste(dirFIA,"UT/UT_TREE_REGIONAL_BIOMASS.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(TRE_CN,REGIONAL_DRYBIOT)

ut_trees <- read_csv(paste(dirFIA,"UT/UT_TREE.csv",sep=""),guess_max = 100000) %>%
  dplyr::select(CN, PLT_CN, PLOT, TREE, BHAGE, TOTAGE, DIA, CARBON_AG, SPCD, TPA_UNADJ) %>%
  dplyr::rename(TRE_CN=CN) %>%
  right_join(ut_plots,by="PLT_CN") %>%
  #  left_join(ca_tb,by="TRE_CN") %>%
  left_join(spec.codes,by=c("SPCD")) %>%
  left_join(ut_plp,by="PLT_CN") %>%
  #  filter(!is.na(SpecCode)) %>%
  filter(!is.na(TRE_CN)) %>%
  filter(!is.na(DIA)) %>%
  filter(INVYR > 2005) %>%
  mutate(SpecCode2 = as.factor(SPECIES_SYMBOL)) %>%
  mutate(AG_biomass_gm2 = CARBON_AG * TPA_UNADJ * 2 * 0.1121) %>%
  mutate(TOTAGE2 = BHAGE + 10)

###LINK up state tree tables#####  
trees.all <- rbind(ca_trees, nv_trees, or_trees, wa_trees, wy_trees, az_trees, id_trees, ut_trees) #%>%
#  mutate(old1 = case_when(TOTAGE2 < 250 ~ 0,
#                          TOTAGE2 >= 250 ~ 1))

##impute missing age data###
##estimate missing total age from diameter and species  
m0 <- lm(log(TOTAGE2) ~ log(DIA) * SPCD , data = trees.all, na.action = na.exclude) #does a terrible job
summary(m0)
#plot(m0)
actual <- trees.all$TOTAGE2[!is.na(trees.all$TOTAGE2)]
plot(actual, exp(m0$fitted.values))

##separate and rejoin tree data
trees_missing <- subset(trees.all, is.na(TOTAGE2))
trees_notmissing <- subset(trees.all, !is.na(TOTAGE2))
trees_notmissing$TOTAGE2 <- roundUp(trees_notmissing$TOTAGE2)
trees_predict <- as_tibble(exp(predict.lm(m0, trees_missing)))
trees_fixed <- cbind(trees_missing, trees_predict)
trees_fixed <- trees_fixed[-24]
names(trees_fixed)[names(trees_fixed) == 'value'] <- 'TOTAGE2'
trees_fixed$TOTAGE2 <- roundUp(trees_fixed$TOTAGE2)
trees_total <- rbind(trees_fixed, trees_notmissing)
trees_total$AG_biomass_gm2 <- round(trees_total$AG_biomass_gm2)
names(trees_total)[names(trees_total) == 'PLT_CN'] <- 'CN'

##consistentcy check for predicted ages
trees_total2 <- left_join(trees_total, sierra_sp_long, by = c("SPECIES_SYMBOL" = "VEG_SPCD")) %>%
  mutate(TOTAGE3 = ifelse(TOTAGE2 > Longevity, Longevity, TOTAGE2)) %>%
  filter(!is.na(Longevity)) %>%
  filter(Location2 == "Klamath")

##workspace cleanup
rm(id_plots, ut_plots, id_plp, ut_plp, id_tb, ut_tb, ut_trees, id_trees, ca_plots, or_plots, nv_plots, wa_plots, wy_plots, az_plots, ca_plp, ca_tb, ca_trees,wy_plp, wy_tb, wy_trees, az_plp, az_tb, az_trees, or_plp, or_tb, or_trees, wa_plp, wa_tb, wa_trees, nv_plp, nv_tb, nv_trees, m0, trees.all, trees_fixed, trees_missing, trees_notmissing, trees_predict, trees_total)

##join FIA tree data to spatial Riley tree data
CA_sierra_join <- full_join(trees_total2, canv_tree_table, by = c("CN", "PLOT", "TREE", "SPCD", "DIA", "INVYR")) %>%
  filter(!is.na(tl_id)) %>%
  right_join(stl_mapcodes, by = "tl_id") %>%
  dplyr::select(tl_id, SpecCode, TOTAGE3, AG_biomass_gm2) %>%
  group_by(tl_id, SpecCode, TOTAGE3) %>%
  summarise(sum(AG_biomass_gm2)) %>%
  na.omit()

colnames(CA_sierra_join) <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")

###csv output/text file input
write.csv(CA_sierra_join, "E:/EPIC/Klamath_inputs/ca_klamath_join_v1.csv")

CA_sierra_outputs <- CA_sierra_join %>%
  group_by(SpeciesName) %>%
  summarise(Totalbiomass = sum(CohortBiomass))

CA_sierra_biom2 <- CA_sierra_outputs %>%
  mutate(SpeciesName = fct_reorder(SpeciesName, desc(Totalbiomass))) %>%
  ggplot(aes(x = SpeciesName, y = Totalbiomass)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))
plot(CA_sierra_biom2)

###shrub distributions####
sa_coords <- cbind(ca_shrubs$LON, ca_shrubs$LAT)
sp.shrubs.all <- SpatialPointsDataFrame(coords = sa_coords, data = ca_shrubs,
                                        proj4string = CRS("+proj=longlat +ellps=WGS84"))  

sp.shrubs.all.aea <- spTransform(sp.shrubs.all, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))  

#sierra_tl_mask <- raster("E:/EPIC/Sierra Landis Parameter files/sierra_IC.tif")
#sierra_tl_mask <- modify_raster_margins(sierra_tl_mask, extent_delta = c(0,1,0,1), value = NA)
#sierra_tl_mask@extent <- nofxnorseed@extent
shrub_distro <- sierra_tl_mask

shrubtypes <- c("FX_R_SEED", "NOFX_NOR_SEED", "NOFX_R_SEED")
i <- shrubtypes[1]
ic_shrub_input <- NULL

for(i in shrubtypes){
  typeI <- subset(sp.shrubs.all.aea, Type == i)
  plot(typeI)
  adf <- as.data.frame(typeI)
  steps <- 150
  age2 <- with(as.data.frame(typeI), interp(coords.x1, coords.x2, age,
                                            xo = seq(min(coords.x1), max(coords.x1), length = steps),
                                            yo = seq(min(coords.x2), max(coords.x2), length = steps),
                                            duplicate = "mean",
                                            jitter = 300, jitter.iter = 3, jitter.random = T))
  age2r <- raster(age2)
  
  biom2 <- with(as.data.frame(typeI), interp(coords.x1, coords.x2, biomass,
                                             xo = seq(min(coords.x1), max(coords.x1), length = steps),
                                             yo = seq(min(coords.x2), max(coords.x2), length = steps),
                                             duplicate = "mean",
                                             jitter = 300, jitter.iter = 3, jitter.random = T))
  biom2r <- raster(biom2)
  
#  shrub_distro <- raster(paste0("E:/EPIC/shrubs/",i,"_distro.tif"))
  
  tcsi_typeI_age <- crop(age2r, shrub_distro)
  tcsi_typeI_age <- resample(tcsi_typeI_age, shrub_distro)
  tcsi_tI_age_type_mask <- mask(tcsi_typeI_age, shrub_distro)
  
  tcsi_typeI_biom <- crop(biom2r, shrub_distro)
  tcsi_typeI_biom <- resample(tcsi_typeI_biom, shrub_distro)
  tcsi_tI_biom_type_mask <- mask(tcsi_typeI_biom, shrub_distro)
  
  age_input <- zonal(tcsi_tI_age_type_mask, sierra_tl_mask, "mean")
  biom_input <- zonal(tcsi_tI_biom_type_mask, sierra_tl_mask, "mean")
  
  mapcode_input <- cbind(age_input[,1],i,age_input[,2],biom_input[,2])
  ic_shrub_input <- rbind(mapcode_input, ic_shrub_input)
}  
ic_shrub_input <- as.data.frame(ic_shrub_input)
ic_shrub_input$CohortAge[is.nan(ic_shrub_input$CohortAge)] <- NA
ic_shrub_input$CohortBiomass[is.nan(ic_shrub_input$CohortBiomass)] <- NA
colnames(ic_shrub_input) <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")

###csv output/text file input
write.csv(ic_shrub_input, "E:/EPIC/Klamath_inputs/ca_klamath_shrubs.csv")
