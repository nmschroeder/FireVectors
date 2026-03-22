# Create abbreviated MTBS data set for MODIS and VIIRS active fire pixel
# data

library(dplyr)
library(sf)
library(raster)

# ## Local
# perim_dir <- "~/Data/mtbs_perimeter_data"
# 
# # VIIRS data directory
# data_dir <- "~/Data/VIIRS"

## HPC
# Data directory where VIIRS data is located
data_dir <- "/projects/nihe1301/fire/VIIRS"

# Data directory for final fire perimeters
perim_dir <- "MTBS"

fname <- list.files(path = perim_dir, pattern = ".shp$", full.names = TRUE)

# mtbs <- st_read(fname) %>% st_transform(crs = 5070)
# 
# start_date <- as.Date("2000-10-31") # Based on oldest MODIS/VIIRS returns from FIRMS
# 
# states <- st_read("~/Data/TIGER/tl_2012_us_state.shp") %>% st_transform(crs = 5070)
# 
# western_conus <- c("Washington", "Oregon", "California", "Idaho", "Nevada",
#                    "Arizona", "Utah", "New Mexico", "Colorado", "Wyoming", "Montana")
# 
# western_us <- dplyr::filter(states, NAME %in% western_conus)
# w_us <- st_union(western_us)
# 
# # Check for holes (Utah..)
# plot(w_us)
# 
# mtbs_2000 <- dplyr::filter(mtbs, Ig_Date > start_date)
# 
# # Let's look only at the fires that are entirely within the US, so that we
# # don't have some perimeters with missing VIIRS data
# #idx <- st_intersects(mtbs_2000, w_us, sparse = FALSE)
# idx <- st_contains_properly(w_us, mtbs_2000, sparse = FALSE)
# 
# mtbs_wus_2000_2024 <- mtbs_2000[idx,]
# 
# # Write a file for MTBS fires within the western United States between
# # Nov. 1, 2000, and March 31, 2024
# mtbs_wus_2000_2024$New_ID <- 1:nrow(mtbs_wus_2000_2024)
# st_write(mtbs_wus_2000_2024, paste0(perim_dir, "/western_us/mtbs_wus_2000_2024.shp"), append = FALSE)
# st_write(w_us, "western_us.shp", append = FALSE)
w_us <- st_read("western_us.shp")

# Read in VIIRS and clip to buffered Western United States 

# List all the files in the VIIRS directory
fnames <- list.files(path = data_dir, pattern = glob2rx("fire_archive_*.shp"), full.names = TRUE, recursive = TRUE)

# Create a function to clip the VIIRS data to a region of interest (ROI) within 
# a given buffer to speed up reading in the data for the ROI

clip_viirs <- function(fname, roi, d_buffer){
  # Buffer the ROI
  roi <- roi %>% st_transform(crs = 5070) %>% st_buffer(dist = d_buffer) %>% 
    st_transform(crs = 4326)
  
  # Read in the VIIRS data
  data_temp <- st_read(fname)
  
  # Determine which returns intersect our ROI
  idx <- st_intersects(roi, data_temp, sparse = FALSE)
  
  # Clip the data to only the points within our ROI
  data_temp <- data_temp[idx,]
  
  # Extract the file name string
  fname_new <- str_extract(fname, pattern = "fire_archive_(.+).shp")
  
  # Create a new file name by appending our ROI to the old name
  fname_new <- paste0(data_dir, "/western_us_", fname_new)
  
  # Save the new file to the data directory
  st_write(data_temp, fname_new)
  
  return(print("Complete"))
}

for (i in 1:length(fnames)){
  clip_viirs(fnames[i], roi = w_us, d_buffer = 1e4)
}
