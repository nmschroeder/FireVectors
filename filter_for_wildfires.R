# File to filter data sets for wildfires

library(sf)
library(dplyr)

setwd("~/Documents/R/FireVectors")

data_dir <- "~/Data/FireVectors/downloaded_2026-11-07"

out_dir <- "~/Data/FireVectors/"

mtbs_dir <- "MTBS"

mtbs <- list.files(path = mtbs_dir, pattern = glob2rx("mtbs_wus*shp"), full.names = TRUE) %>% st_read()

mtbs_wildfire <- dplyr::filter(mtbs, Incid_Type == "Wildfire")
nrow(mtbs_wildfire)

keep_id <- mtbs_wildfire$New_ID

fnames <- list.files(data_dir)

# Filter for only New_ID values that correspond to the "Wildfire" category
for (i in 1:length(fnames)){
  if (str_detect(fnames[i], ".csv")){
    temp_df <- read.csv(paste0(data_dir, "/", fnames[i]))
    temp_df <- dplyr::filter(temp_df, New_ID %in% keep_id)
    write.csv(temp_df, paste0(out_dir, fnames[i]))
  }
  
  if (str_detect(fnames[i], ".geojson")){
    temp_df <- st_read(paste0(data_dir, "/", fnames[i]))
    temp_df <- dplyr::filter(temp_df, New_ID %in% keep_id)
    st_write(temp_df, paste0(out_dir, fnames[i]))
  }
    
}
