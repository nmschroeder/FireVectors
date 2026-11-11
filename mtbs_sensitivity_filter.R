# Create a filtered data set of the Western US MTBS Perimeters that have
# matches in NIROPS for the sensitivity analysis

library(dplyr)
library(sf)


nirops_mtbs_match <- st_read("NIROPS/nirops_mtbs_match.shp")

mtbs_wus_2000_2024 <- st_read("MTBS/mtbs_wus_2000_2024.shp")

mtbs_filter <- mtbs_wus_2000_2024 %>% dplyr::filter(New_ID %in% nirops_mtbs_match$New_ID)

# These are the MTBS fires that have matches in NIROPS
str(mtbs_filter)

# We can do one more layer of filtering to collect only fires that have
# speed estimates derived from MODIS AND VIIRS

modis_speed <- read.csv("data/burn_direction_modis.csv")
idx <- modis_speed$d_xy %>% is.na()
modis_speed <- modis_speed[!idx,]

viirs_speed <- read.csv("data/burn_direction_viirs.csv")
idx2 <- viirs_speed$d_xy %>% is.na()
viirs_speed <- viirs_speed[!idx,]


modis_id <- unique(modis_speed$New_ID)
viirs_id <- unique(viirs_speed$New_ID)

mtbs_filter_speed <- mtbs_filter %>% dplyr::filter(New_ID %in% modis_id & New_ID %in% viirs_id)
str(mtbs_filter_speed)

# Okay, so there are nrow(mtbs_filter_speed) matches with speed estimates for us to use for the sensitivity analysis

st_write(mtbs_filter_speed, "MTBS/mtbs_sensitivity_analysis.shp", append = FALSE)
         