#!/usr/bin/env Rscript

# File to collate the outputs of viirs_01.R 
# These include burn direction vectors and fire perimeters for each available
# time step
# Nicole Hemming-Schroeder, October 10, 2024
# Updated 12/2/24

library(dplyr)
library(sf)
library(lubridate)

out_dir <- "/scratch/alpine/nihe1301/fire_speed"
setwd(out_dir)

## VIIRS

direction_files <- list.files(pattern = glob2rx("burn_direction_viirs_*.geojson"))
polygon_files <- list.files(pattern = glob2rx("perim_polygons_viirs_*.geojson"))

direction_list <- lapply(direction_files, read.csv)

bug_fix <- function(temp_df){
  if (!("data" %in% colnames(temp_df))){
    temp_df <- mutate(temp_df, data = 1)
  }
  return(temp_df)
}

bug_fix2 <- function(temp_df){
  if (!("SATELLITE" %in% colnames(temp_df))){
    temp_df <- mutate(temp_df, SATELLITE = NA)
  }
  return(temp_df)
}


direction_list <- lapply(direction_list, bug_fix)


direction_df <- do.call(rbind.data.frame, direction_list)

# Convert from character string back to a date object
direction_df$time_start <- as.POSIXct(as.numeric(direction_df$time_start), origin = "1970-01-01", tz = "UTC")

save_fname <- "burn_direction_viirs.csv"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
save_check <- list.files(path = out_dir, pattern = save_fname)
  if (length(save_check)>0){
    # If this file already exists, then delete it to write over it
    unlink(save_output_fname)
  }
write.csv(direction_df, save_output_fname)


polygon_list <- lapply(polygon_files, st_read)
polygon_list <- lapply(polygon_files, bug_fix2)
polygon_df <- do.call(rbind.data.frame, polygon_list)

# Convert from character string back to a date object
polygon_df$summary_date <- as.POSIXct(as.numeric(polygon_df$summary_date), origin = "1970-01-01", tz = "UTC")

save_fname <- "perim_polygons_viirs.geojson"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
if (file.exists(save_output_fname)){
  # If this file already exists, then delete it to write over it
  unlink(save_output_fname)
}
st_write(polygon_df, save_output_fname)


## MODIS

direction_files <- list.files(pattern = glob2rx("burn_direction_modis_*.geojson"))
polygon_files <- list.files(pattern = glob2rx("perim_polygons_modis_*.geojson"))

direction_list <- lapply(direction_files, st_read)
direction_list <- lapply(direction_list, bug_fix)
direction_df <- do.call(rbind.data.frame, direction_list)

# Convert from character string back to a date object
direction_df$time_start <- as.POSIXct(as.numeric(direction_df$time_start), origin = "1970-01-01", tz = "UTC")


save_fname <- "burn_direction_modis.geojson"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
if (file.exists(save_output_fname)){
  # If this file already exists, then delete it to write over it
  unlink(save_output_fname)
}
st_write(direction_df, save_output_fname)

polygon_list <- lapply(polygon_files, st_read)
polygon_df <- do.call(rbind.data.frame, polygon_list)

# Convert from character string back to a date object
polygon_df$time_start <- as.POSIXct(as.numeric(polygon_df$time_start), origin = "1970-01-01", tz = "UTC")


save_fname <- "perim_polygons_modis.geojson"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
if (file.exists(save_output_fname)){
  # If this file already exists, then delete it to write over it
  unlink(save_output_fname)
}
st_write(polygon_df, save_output_fname)

