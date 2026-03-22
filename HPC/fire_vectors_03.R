#!/usr/bin/env Rscript

# File to collate the outputs of viirs_01.R 
# These include burn direction vectors and fire perimeters for each available
# time step
# Nicole Hemming-Schroeder, October 10, 2024
# Updated 12/2/24

library(dplyr)
library(sf)
library(lubridate)
library(stringr)

out_dir <- "/scratch/alpine/nihe1301/fire_speed"
setwd(out_dir)

## VIIRS

pixel_files <- list.files(pattern = glob2rx("active_fire_pixel_data_viirs_*.geojson"))

pixel_process <- function(fname){
  taskID <- str_extract(fname, pattern = "[0-9]+") %>% as.integer()
  temp_df <- st_read(fname)
  temp_df <- mutate(temp_df, New_ID = taskID)
  return(temp_df)
}

pixel_list <- lapply(pixel_files, pixel_process)

pixel_df <- do.call(rbind.data.frame, pixel_list)

save_fname <- "active_fire_pixel_data_viirs.geojson"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
if (file.exists(save_output_fname)){
  # If this file already exists, then delete it to write over it
  unlink(save_output_fname)
}
st_write(pixel_df, save_output_fname)

## MODIS

pixel_files <- list.files(pattern = glob2rx("active_fire_pixel_data_modis_*.geojson"))

pixel_list <- lapply(pixel_files, pixel_process)

pixel_df <- do.call(rbind.data.frame, pixel_list)

save_fname <- "active_fire_pixel_data_modis.geojson"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
if (file.exists(save_output_fname)){
  # If this file already exists, then delete it to write over it
  unlink(save_output_fname)
}
st_write(pixel_df, save_output_fname)


