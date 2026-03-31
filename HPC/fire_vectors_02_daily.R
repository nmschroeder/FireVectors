#!/usr/bin/env Rscript

# File to collate the outputs of fire_vectors_01.R 
# These include burn direction vectors and fire perimeters for each available
# time step
# Nicole Hemming-Schroeder, October 10, 2024
# Updated March 25, 2026

library(dplyr)
library(sf)
library(lubridate)

out_dir <- "/scratch/alpine/nihe1301/fire_speed/"
setwd(out_dir)

direction_files <- list.files(pattern = glob2rx("burn_direction_*_daily.csv"))

direction_list <- lapply(direction_files, read.csv)

direction_df <- do.call(rbind.data.frame, direction_list)

save_fname <- "burn_direction_daily.csv"
save_output_fname <- paste0(out_dir, "/", save_fname)
# Check if file exists and unlink it if it does
save_check <- list.files(path = out_dir, pattern = save_fname)
  if (length(save_check)>0){
    # If this file already exists, then delete it to write over it
    unlink(save_output_fname)
  }
write.csv(direction_df, save_output_fname)
