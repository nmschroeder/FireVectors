library(sf)
library(dplyr)
library(raster)
library(stringr)
library(lubridate)
library(nngeo)
library(ggplot2)

taskID <- 627

mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp")

# Where to start runs beginning in 2020?
check <- year(mtbs$Ig_Date)
idx <- which(check >= 2020)
idx[1]

mtbs_irwinID <- unique(mtbs$irwinID) %>% toupper()

# Data directory for NIROPS fire perimeters
perimeter_dir <- "NIROPS"

# Read in NIROPS
fname_nirops <- list.files(perimeter_dir, pattern = glob2rx("NIROPS*.shp"), full.names = TRUE)

nirops <- st_read(fname_nirops) 

nirops_irwinID <- unique(nirops$IRWINID) %>% toupper()

check <- sum(nirops_irwinID %in% mtbs_irwinID)
check

nirops$IrwinID <- nirops$IRWINID %>% toupper() 

nirops_matched <- nirops %>% dplyr::filter(IrwinID %in% mtbs_irwinID)
str(nirops_matched)

nirops_list <- list()

nirops_matched_ids <- unique(nirops_matched$IrwinID)
N <- length(nirops_matched_ids)

# Here we are going through each IrwinID within NIROPS that has been
# matched to MTBS and appending the corresponding New_ID from FireVectors
for (i in 1:N){
  temp_df <- dplyr::filter(nirops_matched, IrwinID == nirops_matched_ids[i])
  temp_mtbs <- dplyr::filter(mtbs, irwinID == nirops_matched_ids[i])
  New_ID <- temp_mtbs$New_ID[1]
  temp_df$New_ID <- New_ID
  nirops_list[[i]] <- temp_df
}

nirops_df <- do.call(rbind.data.frame, nirops_list)
nirops_df <- nirops_df %>% dplyr::select(-IRWINID)
st_write(nirops_df, "NIROPS/nirops_mtbs_match.shp")

