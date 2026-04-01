# Script to identify the primary vegetation type for each MTBS perimeter

library(tidyverse)
library(sf)
library(ggplot2)
library(raster)
library(exactextractr)
library(lme4)
library(viridis)
library(cowplot)

# Set the environment to read everything in as UTC
Sys.setenv(TZ = "UTC")

# Read in the National Land Cover Database classes
nlcd <- raster("NLCD/Annual_NLCD_LndCov_2002_CU_C1V1.tif")
nlcd

# Read in the MTBS data set clipped to our region and time period
mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp") %>% st_transform(crs = st_crs(nlcd))

# Read in the L3 Ecoregion shapefile

eco <- st_read("ecoregions/us_eco_l3.shp") %>% st_transform(crs = 5070)

# Read in the Western United States shapefile

wus <- st_read("TIGER/western_us.shp")

N <- nrow(mtbs)

#str(mtbs)

# When do we first get Aqua MODIS returns?
modis <- st_read("FIRMS/western_us_fire_archive_M-C61_608988.shp")
#str(modis)

modis_aqua <- dplyr::filter(modis, SATELLITE == "Aqua") %>% arrange(ACQ_DATE)
#modis_aqua[1,]

start_date <- modis_aqua[1,]$ACQ_DATE

mtbs_aqua <- dplyr::filter(mtbs, Ig_Date >= start_date) %>% arrange(Ig_Date)

## Table 1 Values BEGIN

N_aqua <- nrow(mtbs_aqua) #6794 within a valid time frame

# Read in VIIRS to check first valid potential date for MTBS perimeters
viirs <- st_read("FIRMS/western_us_fire_archive_SV-C2_608992.shp")

viirs_check <- st_read("FIRMS/western_us_fire_archive_J1V-C2_608990.shp")

# Double-check which satellite is included in this file
viirs <- viirs %>% arrange(ACQ_DATE)
#unique(viirs$SATELLITE) # Suomi-NPP

# Check which satellite is included in this file
viirs_check <- viirs_check %>% arrange(ACQ_DATE)
#unique(viirs_check$SATELLITE) # NOAA20

# For Suomi-NPP (the satellite for which we calculated speed estimates),
# what are the valid dates?

viirs_start_date <- viirs[1,]$ACQ_DATE

mtbs_snpp <- dplyr::filter(mtbs, Ig_Date >= viirs_start_date) %>% arrange(Ig_Date)

N_snpp <- nrow(mtbs_snpp)
N_snpp

# Next, let's look at how many unique fires from MTBS have MODIS and VIIRS active
# fire pixels (for Aqua and Suomi-NPP only, since these are the ones for which
# we generated speed)

modis_afp <- st_read("data/active_fire_pixel_data_modis.geojson")
viirs_afp <- st_read("data/active_fire_pixel_data_viirs.geojson")

# Check how many unique MTBS fires have associated active fire pixels
modis_afp_aqua <- dplyr::filter(modis_afp, SATELLITE == "Aqua")
#str(modis_afp_aqua)

aqua_unique <- unique(modis_afp_aqua$New_ID)

length(aqua_unique)

# Check how many unique MTBS fires have associated active fire pixels
viirs_afp_snpp <- dplyr::filter(viirs_afp, SATELLITE == "N")
#str(viirs_afp_snpp)

snpp_unique <- unique(viirs_afp_snpp$New_ID)

length(snpp_unique)

# How many overlap? These should be the same... right?

sum(snpp_unique %in% aqua_unique)

sum(aqua_unique %in% snpp_unique)

# Next, we want to know how many unique MTBS perimeters have speed estimates
# (These are already filtered for Aqua and Suomi-NPP)
viirs_speed <- read.csv("data/burn_direction_viirs.csv")
modis_speed <- read.csv("data/burn_direction_modis.csv")

viirs_speed_unique <- viirs_speed %>% dplyr::filter(!is.na(v_xy)) %>% pull(New_ID) %>% unique()
length(viirs_speed_unique)

modis_speed_unique <- modis_speed %>% dplyr::filter(!is.na(v_xy)) %>% pull(New_ID) %>% unique()
length(modis_speed_unique)

# Check overlap
sum(viirs_speed_unique %in% modis_speed_unique)
sum(modis_speed_unique %in% viirs_speed_unique)

## Table 1 Values END

# Let's find out the primary class for each of these MTBS perimeters that
# we searched

nlcd_info <- levels(nlcd)[[1]]

r <- res(nlcd) %>% min()
crs_obj <- st_crs(nlcd)

str(nlcd_info)

nlcd_names <- colnames(nlcd_info) %>% gsub(pattern = " ", replacement = "_", x = .)
colnames(nlcd_info) <- nlcd_names

Pixel_Value <- c(11:12, 21:24, 31, 41:43, 51:52, 71:74, 81:82, 90, 95)
#NLCD_Land_Cover_Class <- c("Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land", "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Dwarf Scrub", "Shrub/Scrub", "Grassland/Herbaceous", "Sedge/Herbaceous", "Lichens", "Moss", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands")
NLCD_Class <- c(rep("Water", 2), rep("Developed", 4), "Barren", rep("Forest", 3), rep("Shrubland", 2), rep("Herbaceous", 4), rep("Cultivated", 2), rep("Wetlands", 2))
nlcd_key <- data.frame(NLCD_Class, Pixel_Value)

nlcd_filter <- dplyr::filter(nlcd_key, Pixel_Value %in% nlcd_info$Pixel_Value)
nlcd_info <- left_join(nlcd_info, nlcd_filter, by = "Pixel_Value")
nlcd_classes <- unique(NLCD_Class)

## What if, instead, we calculated the NLCD fraction along each maximum speed
## vector per fire? This would more directly relate to the portion of the fire
## footprint where this speed is occurring...

viirs_speed_drop_na <- viirs_speed %>% dplyr::filter(!is.na(v_xy)) 

modis_speed_drop_na <- modis_speed %>% dplyr::filter(!is.na(v_xy))

viirs_list <- list()
unique_id <- unique(viirs_speed_drop_na$New_ID)
for (i in 1:length(unique_id)){
  temp_df <- dplyr::filter(viirs_speed_drop_na, New_ID == unique_id[i])
  idx <- which.max(temp_df$v_xy)
  viirs_list[[i]] <- temp_df[idx,]
}

viirs_summary <- do.call(rbind.data.frame, viirs_list) %>% arrange(Ig_Date)
str(viirs_summary)

modis_list <- list()
unique_id <- unique(modis_speed_drop_na$New_ID)
for (i in 1:length(unique_id)){
  temp_df <- dplyr::filter(modis_speed_drop_na, New_ID == unique_id[i])
  idx <- which.max(temp_df$v_xy)
  modis_list[[i]] <- temp_df[idx,]
}

modis_summary <- do.call(rbind.data.frame, modis_list) %>% arrange(Ig_Date)
str(modis_summary)

nlcd_class <- vector(length = nrow(viirs_summary))
row_list <- list()

for (i in 1:nrow(viirs_summary)){

  temp_df <- viirs_summary[i,]

  if (i == 1){ # This strategy requires the data to be in chronological order
    previous_year <- year(temp_df$Ig_Date)-1
    nlcd_fname <- paste0("NLCD/Annual_NLCD_LndCov_", as.character(previous_year), "_CU_C1V1.tif")
    nlcd <- raster(nlcd_fname)
  }

  if (i > 1){
    year_check <- year(temp_df$Ig_Date)-1
    if (year_check > previous_year){
      previous_year <- year_check
      nlcd_fname <- paste0("NLCD/Annual_NLCD_LndCov_", as.character(previous_year), "_CU_C1V1.tif")
      nlcd <- raster(nlcd_fname)
    }
  }

  geo_obj <- st_linestring(matrix(c(temp_df$x1, temp_df$y1, temp_df$x2, temp_df$y2), ncol = 2, byrow = TRUE))
  temp_geo <- st_sf(geometry = st_sfc(geo_obj, crs = 5070))

  #ggplot() + geom_sf(data = temp_geo)

  temp_geo_buffer <- st_buffer(temp_geo, dist = r/2) %>% st_transform(crs = crs_obj)

  # Extract the raster values around the buffered speed vector
  values_temp <- exact_extract(nlcd, temp_geo_buffer)[[1]] %>% drop_na()

  #N_total <- nrow(values_temp)

  colnames(values_temp) <- c("Pixel_Value", "coverage_fraction")

  N_total <- sum(values_temp$coverage_fraction)

  values_temp <- left_join(values_temp, nlcd_key, by = "Pixel_Value")

  values_df <- values_temp %>% group_by(NLCD_Class) %>% summarize(fraction = sum(coverage_fraction)/N_total)

  idx <- which.max(values_df$fraction)
  nlcd_class[i] <- values_df$NLCD_Class[idx]

  aug_df <- data.frame(NLCD_Class = nlcd_classes, fraction = 0) %>% dplyr::filter(!(NLCD_Class %in% values_df$NLCD_Class))

  class_df <- rbind.data.frame(values_df, aug_df) %>% arrange(NLCD_Class) %>% rbind.data.frame(data.frame(NLCD_Class = "N_total", fraction = N_total))

  row_temp <- class_df %>%
    pivot_wider(names_from = NLCD_Class, values_from = fraction) %>% mutate(New_ID = temp_df$New_ID)

  row_list[[i]] <- row_temp %>% arrange

}

viirs_df <- do.call(rbind.data.frame, row_list)
str(viirs_df)


viirs_class <- right_join(viirs_summary, viirs_df, by = "New_ID")

viirs_class$nlcd_class <- nlcd_class

str(viirs_class)

write.csv(viirs_class, "viirs_speed_veg_class.csv")


# Same for MODIS

nlcd_class <- vector(length = nrow(modis_summary))
row_list <- list()

for (i in 1:nrow(modis_summary)){

  temp_df <- modis_summary[i,]

  if (i == 1){ # This strategy requires the data to be in chronological order
    previous_year <- year(temp_df$Ig_Date)-1
    nlcd_fname <- paste0("NLCD/Annual_NLCD_LndCov_", as.character(previous_year), "_CU_C1V1.tif")
    nlcd <- raster(nlcd_fname)
  }

  if (i > 1){
    year_check <- year(temp_df$Ig_Date)-1
    if (year_check > previous_year){
      previous_year <- year_check
      nlcd_fname <- paste0("NLCD/Annual_NLCD_LndCov_", as.character(previous_year), "_CU_C1V1.tif")
      nlcd <- raster(nlcd_fname)
    }
  }

  geo_obj <- st_linestring(matrix(c(temp_df$x1, temp_df$y1, temp_df$x2, temp_df$y2), ncol = 2, byrow = TRUE))
  temp_geo <- st_sf(geometry = st_sfc(geo_obj, crs = 5070))

  #ggplot() + geom_sf(data = temp_geo)

  temp_geo_buffer <- st_buffer(temp_geo, dist = r/2) %>% st_transform(crs = crs_obj)

  # Extract the raster values around the buffered speed vector
  values_temp <- exact_extract(nlcd, temp_geo_buffer)[[1]] %>% drop_na()

  #N_total <- nrow(values_temp)

  colnames(values_temp) <- c("Pixel_Value", "coverage_fraction")

  N_total <- sum(values_temp$coverage_fraction)

  values_temp <- left_join(values_temp, nlcd_key, by = "Pixel_Value")

  values_df <- values_temp %>% group_by(NLCD_Class) %>% summarize(fraction = sum(coverage_fraction)/N_total)

  idx <- which.max(values_df$fraction)
  nlcd_class[i] <- values_df$NLCD_Class[idx]

  aug_df <- data.frame(NLCD_Class = nlcd_classes, fraction = 0) %>% dplyr::filter(!(NLCD_Class %in% values_df$NLCD_Class))

  class_df <- rbind.data.frame(values_df, aug_df) %>% arrange(NLCD_Class) %>% rbind.data.frame(data.frame(NLCD_Class = "N_total", fraction = N_total))

  row_temp <- class_df %>%
    pivot_wider(names_from = NLCD_Class, values_from = fraction) %>% mutate(New_ID = temp_df$New_ID)

  row_list[[i]] <- row_temp %>% arrange

}

modis_df <- do.call(rbind.data.frame, row_list)


modis_class <- right_join(modis_summary, modis_df, by = "New_ID")

modis_class$nlcd_class <- nlcd_class

str(modis_class)

write.csv(modis_class, "modis_speed_veg_class.csv")


nlcd_class <- vector(length = N_aqua)
row_list <- list()

for (i in 1:N_aqua){

  mtbs_temp <- mtbs_aqua[i,]

  if (i == 1){ # This strategy requires the data to be in chronological order
    previous_year <- year(mtbs_temp$Ig_Date)-1
    nlcd_fname <- paste0("NLCD/Annual_NLCD_LndCov_", as.character(previous_year), "_CU_C1V1.tif")
    nlcd <- raster(nlcd_fname)
  }

  if (i > 1){
    year_check <- year(mtbs_temp$Ig_Date)-1
    if (year_check > previous_year){
      previous_year <- year_check
      nlcd_fname <- paste0("NLCD/Annual_NLCD_LndCov_", as.character(previous_year), "_CU_C1V1.tif")
      nlcd <- raster(nlcd_fname)
    }
  }

  values_temp <- exact_extract(nlcd, mtbs_temp)[[1]] %>% drop_na()


  colnames(values_temp) <- c("Pixel_Value", "coverage_fraction")
  N_total <- sum(values_temp$coverage_fraction)

  values_temp <- left_join(values_temp, nlcd_key, by = "Pixel_Value")

  values_df <- values_temp %>% group_by(NLCD_Class) %>% summarize(fraction = sum(coverage_fraction)/N_total)

  idx <- which.max(values_df$fraction)
  nlcd_class[i] <- values_df$NLCD_Class[idx]

  aug_df <- data.frame(NLCD_Class = nlcd_classes, fraction = 0) %>% dplyr::filter(!(NLCD_Class %in% values_df$NLCD_Class))

  class_df <- rbind.data.frame(values_df, aug_df) %>% arrange(NLCD_Class) %>% rbind.data.frame(data.frame(NLCD_Class = "N_total", fraction = N_total))

  row_temp <- class_df %>%
    pivot_wider(names_from = NLCD_Class, values_from = fraction) %>% mutate(New_ID = mtbs_temp$New_ID)

  row_list[[i]] <- row_temp %>% arrange

}

row_df <- do.call(rbind.data.frame, row_list)


mtbs_class <- left_join(mtbs_aqua, row_df)

mtbs_class$nlcd_class <- nlcd_class

str(mtbs_class)

st_write(mtbs_class, "mtbs_veg_class.gpkg", append = FALSE)

mtbs_class <- st_read("mtbs_veg_class.gpkg")
str(mtbs_class)

nrow(mtbs_class)
length(unique(mtbs_class$New_ID))

## Calculations for Table 2 - BEGIN

# Determine how many MTBS perimeters during the Aqua collection time are in each
# veg. category
mtbs_class_aqua <- dplyr::filter(mtbs_class, New_ID %in% mtbs_aqua$New_ID)
table(mtbs_class_aqua$nlcd_class)

# Determine which land types have active fire pixels Aqua MODIS
mtbs_class %>% dplyr::filter(New_ID %in% aqua_unique) %>% pull(nlcd_class) %>% table()

# Determine which land types have speed estimates
mtbs_class %>% dplyr::filter(New_ID %in% modis_speed_unique) %>% pull(nlcd_class) %>% table()

# Do the same calculations for VIIRS (Suomi-NPP)
mtbs_class_snpp <- dplyr::filter(mtbs_class, New_ID %in% mtbs_snpp$New_ID)
table(mtbs_class_snpp$nlcd_class)

# Determine which land types have active fire pixels Aqua MODIS
mtbs_class %>% dplyr::filter(New_ID %in% snpp_unique) %>% pull(nlcd_class) %>% table()

# Determine which land types have speed estimates
mtbs_class %>% dplyr::filter(New_ID %in% viirs_speed_unique) %>% pull(nlcd_class) %>% table()

modis_valid <- modis_speed %>% dplyr::filter(!is.na(v_xy))

mtbs_valid <- dplyr::filter(mtbs_class, New_ID %in% modis_speed_unique)
mtbs_valid

modis_valid_unique <- modis_valid %>% group_by(New_ID) %>% summarize(max_v_xy = max(v_xy, na.rm = TRUE))

mtbs_speed <- left_join(mtbs_valid, modis_valid_unique, by = "New_ID")

table_valid <- table(mtbs_valid$nlcd_class)

#table_total
table_valid

#table_total/sum(table_total)
table_valid/sum(table_valid)

## Calculations for Table 2 - END

# So, we have the speed and class for MODIS speed vectors, VIIRS speed vectors
# and MTBS perimeters...

# We can also summarize by ecoregion and join by New_ID

modis_ecoregion <- read.csv("modis_speed_by_ecoregion.csv")
str(modis_ecoregion)

eco_df <- dplyr::select(modis_ecoregion, New_ID, ecorenamed)

modis_class_df <- right_join(modis_class, eco_df, by = "New_ID")
str(modis_class_df)

unique_ecoregions <- modis_class_df$ecorenamed %>% unique()

eco_list <- list()

q <- c(0.50, 0.90, 0.95)
qnames <- as.character(100*q) 

for (i in 1:length(unique_ecoregions)){
  temp_df <- dplyr::filter(modis_class_df, ecorenamed == unique_ecoregions[i]) %>% dplyr::filter(nlcd_class %in% c("Forest", "Herbaceous", "Shrubland"))
  N <- nrow(temp_df)
  temp_f <- dplyr::filter(temp_df, nlcd_class == "Forest") 
  temp_s <- dplyr::filter(temp_df, nlcd_class == "Shrubland")
  temp_g <- dplyr::filter(temp_df, nlcd_class == "Herbaceous")
  
  N_f <- nrow(temp_f)
  f_f <- N_f/N
  if (f_f > 0){
    q_f <- quantile(temp_f$v_xy, q)
  } else{
    q_f <- c(NA, NA, NA)
  }
  
  N_s <- nrow(temp_s)
  f_s <- N_s/N
  if (f_s > 0){
    q_s <- quantile(temp_s$v_xy, q)
  } else{
    q_s <- c(NA, NA, NA)
  }
  
  N_g <- nrow(temp_g)
  f_g <- N_g/N
  if (f_g > 0){
    q_g <- quantile(temp_g$v_xy, q)
    
  } else{
    q_g <- c(0, 0, 0)
  }
  
  names(q_f) <- paste0("F", qnames)
  names(q_s) <- paste0("S", qnames)
  names(q_g) <- paste0("G", qnames)
  
  f_df <- data.frame(N_f, f_f, t(q_f))
  s_df <- data.frame(N_s, f_s, t(q_s))
  g_df <- data.frame(N_g, f_g, t(q_g))
  

  
  new_df <- data.frame(ecoregion = unique_ecoregions[i], N) %>% cbind.data.frame(f_df, s_df, g_df)
  
  # Fastest of each percentile explored
  names(q_f) <- paste0("Q", qnames)
  names(q_s) <- paste0("Q", qnames)
  names(q_g) <- paste0("Q", qnames)
  check_df <- rbind.data.frame(t(q_f), t(q_s), t(q_g))
  veg_types <-  c("Forest", "Shrubland", "Herbaceous")
  rownames(check_df) <- veg_types
  
  idx_veg <- which.max(c(f_f, f_s, f_g))
  
  idx_1 <- which.max(check_df$Q50)
  idx_2 <- which.max(check_df$Q90)
  idx_3 <- which.max(check_df$Q95)
  
  new_df <- mutate(new_df, q50_max = veg_types[idx_1], q90_max = veg_types[idx_2], q95_max = veg_types[idx_3], primary_fire_veg = veg_types[idx_veg])
  
  eco_list[[i]] <- new_df
}

## Supplementary Table S1

ecoregion_df <- do.call(rbind.data.frame, eco_list) %>% arrange(desc(N))
ecoregion_df
