#!/usr/bin/env Rscript
user_inputs <- commandArgs(trailingOnly = TRUE)

# Check format (for _daily, we are only going to do Terra MODIS night time returns)
taskID <- as.numeric(user_inputs[[1]])
instrument <- "MODIS"
satname <- "Terra"

## HPC

# Output directory
out_dir <- "/scratch/alpine/nihe1301/fire_speed"

# Working directory
wd <- "/home/nihe1301/fire_speed"

# ## BEGIN LOCAL TROUBLESHOOTING (2026-02-23)
# taskID <- 6347 # 4490 for North Start Fire (troubleshooting dAdt < 0 ); 6880 for Bolt Creek # 6212 for August Complex; 6347 for East Troublesome; 2075 for Zaca Fire (2007)
# instrument <- "MODIS"
# satname <- "Terra" # Use for computing speed (all VIIRS will be used for perims)
# wd <- "~/Documents/R/fire_speed"
# out_dir <- wd
# 
# ## END LOCAL TROUBLESHOOTING

print(taskID)
print(instrument)
print(satname)


# File to go through each MTBS final fire perimeter and 
# compute the fire polygons and fire velocity for each fire

# Nicole Hemming-Schroeder, October 10, 2024
# Last updated March 28, 2026, 5:10 PM MDT

library(sf)
library(dplyr)
library(raster)
library(tidyr)
library(concaveman)
library(stringr)
library(lubridate)
library(nngeo)
library(units)

################################ USER INPUTS ###################################

# How many days before MTBS ignition date to look for active fire pixels?
days_before <- 30

# How many days after MTBS ignition date to look for active fire pixels?
days_after <- 180

# Data directory where active fire pixel data are located
data_dir <- instrument

# Data directory for final fire perimeters
perimeter_dir <- "MTBS"

# Determine your fraction of points per x meters
x <- 100
points_per_meter <- 2/x

# How far do we let a fire spot before we call it a new ignition?
spot_threshold <- 4000 # meters

# By what fraction must a suggested fire path overlap the next fire perimeter?
r_overlap <- 0.75

# Number of active fire pixels required to intersect the final fire
# perimeter
n_pixel <- 4

# Number of days with no active fire pixels before we end the fire
N_days <- 14

# Maximum number of clusters within a fire
max_clusters <- 1000

# FYI: There is a lot of hard coding for EPSG:5070 throughout
crs_utm <- CRS("EPSG:5070")

# Elevation input
elev <- raster("LC20_Elev_220.tif") 

################################################################################
set.seed(taskID)

# Collect the resolution of the elevation raster
ds <- res(elev)[1]

# Set working directory
setwd(wd)

# Read in MTBS 
fname_perims <- list.files(perimeter_dir, pattern = ".shp$", full.names = TRUE)

# Pull the MTBS row corresponding to the task array
final_perim <- st_read(fname_perims) %>% dplyr::filter(New_ID == taskID) %>% st_transform(crs = 4326) 

# Elevation raster CRS
elev_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

elev_ext <- final_perim %>% st_transform(crs = 5070) %>% st_buffer(dist = 10000) %>% st_transform(crs = elev_crs) %>% extent()

# Crop to a buffered extent of the polygon
elev <- crop(elev, elev_ext)

# Project the elevation raster into the specified CRS
elev_utm <- projectRaster(elev, crs = crs_utm)
elev_utm[elev_utm == -9999] <- NA

# Let's read in each fire name from MTBS
fire_name <- final_perim$Incid_Name

# List all the files in the active fire pixel directory
if (instrument == "MODIS"){
  char_match <- "M"
}

if (instrument == "VIIRS"){
  char_match <- "V"
}

fnames <- list.files(path = data_dir, 
                     pattern = glob2rx(paste0("western_us_fire_archive_*", 
                                              char_match, "-*.shp")), 
                     full.names = TRUE)

# Function to convert a date string to decimal date
date2decimal <- function(date_string){
  date_values <- str_split(date_string, pattern = "-| |:")[[1]] %>% as.numeric()
  year <- date_values[1]
  mon <- date_values[2]
  day <- date_values[3]
  hr <- date_values[4]
  mm <- date_values[5]
  hr <- hr + mm/60
  
  
  month_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (year %% 4 == 0){
    month_days[2] <- 29
  }
  
  total_days <- sum(month_days)
  if (mon > 1){
    dec_year <- year + (sum(month_days[1:(mon-1)]) + day + hr/24)/total_days
  } else{
    dec_year <- year + (day + hr/24)/total_days
  }
  return(dec_year)
}

# Create a function to select a concavity ratio based on the number
# of points in a cluster of active fire pixels
select_ratio <- function(n_points){
  r_val <- max(0.8*(0.25)^(n_points/600), 0.2)
  return(r_val)
}
# # Troubleshooting elevation distance
# L <- L_df[2,]
# xy <- st_coordinates(L)[,1:2] %>% as.data.frame() %>% st_as_sf(coords = c("X", "Y"), crs =5070) 
# pt1 <- xy[1,]
# pt2 <- xy[2,]
# ggplot() + geom_sf(data = pt1, color ='green') + geom_sf(data = pt2, color = 'red') + geom_sf(data = L)
# 

elevation_distance <- function(pt1, pt2, elev_utm, crs_utm){
  # For now, we're taking the resolution of the elevation raster file
  # and sampling about 100 times per pixel to approximate the arc length
  # integral
  
  class_check1 <- class(pt1)
  
  if (!(("sf" %in% class_check1) & ("data.frame" %in% class_check1))){
    pt1 <- st_as_sf(pt1)
  }
  
  pt1 <- dplyr::select(pt1, x)
  
  class_check2 <- class(pt2)
  
  if (!(("sf" %in% class_check2) & ("data.frame" %in% class_check2))){
    pt2 <- st_as_sf(pt2)
  }
  
  pt2 <- dplyr::select(pt2, x)
  
  ds <- res(elev_utm)[1]/100
  
  # Compute the xy-distance between the two points in meters
  d_p1_p2 <- st_distance(pt1, pt2) %>% as.numeric()
  
  # Create a linestring between the two proposed points
  L <- dplyr::select(pt1, x) %>% rbind.data.frame(pt2) %>% summarize() %>% st_cast("LINESTRING")
  #L <- rbind(st_coordinates(pt1), st_coordinates(pt2)) %>% st_linestring() %>% st_geometry() %>% st_as_sf(crs = crs_utm)
  
  #ggplot() + geom_sf(data = L) + geom_sf(data = pt1, color = 'darkgreen') + geom_sf(data = pt2, color = 'red')
  
  # Determine how many sampled points of length ds can fit on the linestring
  k <- floor(as.numeric(d_p1_p2/ds))
  
  #p_test <- ggplot() + geom_sf(data = L) + geom_sf(data = pt1, color = 'red') + geom_sf(data = pt2, color = 'darkgreen')
  
  # Create a regular sample (will start ds from point 1)
  if (k > 0){
    sample_pts <- st_sample(L, size = k, type = 'regular')
    # Use pt1 and the sample_pts for the algorithm
    s_pts <- sample_pts %>% st_cast("POINT") %>% st_as_sf()
    
    # This will have changed a small amount during sampling
    ds <- st_distance(s_pts[1,], s_pts[2,]) %>% as.numeric()
    
    # Find the xy-distance between pt1 and the first sample point
    dsp1 <- st_distance(pt1, s_pts[1,]) %>% as.numeric()
    
    # Find the xy-distance between pt2 and the last sample point
    dsp2 <- st_distance(pt2, s_pts[k,]) %>% as.numeric()
    
    # Add point 1 and point 2 to the sample points in order (1st and last)
    s_pts_all <- rbind.data.frame(pt1, s_pts, pt2)
    
    # Create an array with the xy-distance between each point
    ds <- c(dsp1, rep(ds, times = k-1), dsp2)
    
    # Collect the z-values for the sampled points
    z <- raster::extract(elev_utm, s_pts_all)
    
    # Find the change in z between each point
    dz <- abs(z[2:(k+2)] - z[1:(k+1)])
    
    # Find the length of each segment in 3D space
    dl <- sqrt(ds^2 + dz^2)
    
    # Sum up the little lengths to estimate the length of the entire segment
    # projected along the elevation raster
    l <- sum(dl)
    
  } else{
    # The segment is not long enough to create points along it, so 
    # just use the end points
    s_pts <- rbind.data.frame(pt1, pt2)
    
    # Collect the z-values for the sampled points
    z <- raster::extract(elev_utm, s_pts)
    
    # Find the change in z between each point
    dz <- abs(z[2] - z[1])
    l <- sqrt(d_p1_p2^2 + dz^2)
  }
  
  # Note that the sampled points are not necessarily the same distance from
  # the end points as they are from each other
  #p_test + geom_sf(data = sample_pts, color = 'magenta')
  
  # Return the distance traveled including changes in elevation
  return(l)
}


# Create a function to read in active fire pixel data, clip to our time and region of 
# interest, and convert to EPSG 5070 (CONUS Albers NAD83)

read_data <- function(fname, final_perim){
  
  instrument <- grepl("_M-", fname) %>% ifelse(yes = "MODIS", no = "VIIRS")
  
  if (year(final_perim$Ig_Date)<2012 & instrument == "VIIRS"){
    # VIIRS does not have data prior to 2012, so skip the rest
    temp_df <- NA
  } else{
    
    
    temp_df <- st_read(fname) 
    cnames <- colnames(temp_df)
    if ("TYPE" %in% cnames){
      temp_df <- temp_df %>% dplyr::select(-TYPE)
    }
    
    # Filter for temporal window around an event's ignition date
    start_date <- final_perim$Ig_Date - days(days_before)
    end_date <- start_date + days(days_after)
    temp_df <- dplyr::filter(temp_df, ACQ_DATE >= start_date & 
                               ACQ_DATE <= end_date) 
    data_cnames <- colnames(temp_df)
    
    # There still may be no data for the specific time and region of interest,
    # even if the the instrument is collecting data at this time other places
    if (nrow(temp_df)>0){
      
      temp_df <- temp_df %>% st_transform(crs = 5070)
      
      temp_df$ACQ_DATETIME <- as.POSIXct(paste0(temp_df$ACQ_DATE, " ", temp_df$ACQ_TIME), tz = "UTC",
                                         format="%Y-%m-%d %H%M")
      temp_df$DATE_DECIMAL <- sapply(temp_df$ACQ_DATETIME, date2decimal)
      # Crop to the bounding box of the input fire perimeter
      idx <- final_perim %>% st_transform(crs = 5070) %>% st_buffer(dist = 10000) %>%
        st_intersects(temp_df, sparse = FALSE)
      
      # If there are at least n_pixel data points within the final fire perimeter,
      # use these to make fire perimeter time series
      data_flag <- sum(idx) >= n_pixel
      temp_df <- temp_df[idx,]
      
    } else{
      temp_df <- NA
    }
    
  }
  
  return(temp_df)
}

# Apply this function
data_list <- lapply(fnames, read_data, final_perim = final_perim) 

time_group <- function(temp_data){
  # Initialize a vector for the start time of each grouped set of measurements
  time_start <- vector(length = nrow(temp_data))
  
  # Arrange by the aquisition date time
  temp_data <- arrange(temp_data, ACQ_DATETIME)
  
  # Start with the first time stamp
  time_start[1] <- temp_data$ACQ_DATETIME[1]
  
  
  if (nrow(temp_data) > 1){
    
    # For the rest of the time stamps 
    for (i in 2:nrow(temp_data)){
      
      # Let's check the time for the next row
      check_time <- temp_data$ACQ_DATETIME[i]
      delta_time <- difftime(check_time, time_start[i-1], units = "secs")
      
      # Let's group returns that are within 6 minutes (detected at slightly different
      # times from the same swath scan)
      if (delta_time <= 10*60){
        
        time_start[i] <- time_start[i-1]
        
        # Else we keep everything that has been initialized already and move
        # on to the next row without setting an end time.
        
      }else {
        
        time_start[i] <- temp_data$ACQ_DATETIME[i]
        
      } 
    }
    
  }
  
  time_start <- as.POSIXct(time_start, tz = "UTC")
  temp_data_updated <- temp_data %>% mutate(time_start)
  
  
  return(temp_data_updated)
}

# Which list entries have at least one row (e.g. have data)?
data_check <- lapply(data_list, nrow)

# Let's create a function that clusters the grouped data and preserves the data frame components 
draw_fire_perimeters <- function(temp_df){
  temp_df <- temp_df %>% arrange(ACQ_DATETIME)
  instrument <- temp_df$INSTRUMENT[1]
  ratio_temp <- select_ratio(nrow(temp_df))
  max_membership <- temp_df$membership %>% max()
  pgon <- st_coordinates(temp_df) %>% 
    st_multipoint() %>% 
    st_concave_hull(xy, ratio = ratio_temp, allow_holes = FALSE) %>%
    st_sfc(crs = st_crs(temp_df)) %>%
    st_as_sf() %>%
    rename(geometry = x)
  #pgon <- concaveman(temp_df, concavity = 2) %>% rename(geometry = polygons)
  summary_df <- temp_df[nrow(temp_df),] %>% st_drop_geometry() %>%
    dplyr::select(summary_date, SATELLITE, INSTRUMENT, VERSION, DAYNIGHT)
  summary_df <- mutate(summary_df, 
                       ratio = ratio_temp, 
                       membership = max_membership,
                       MEAN_BRIGHTNESS = mean(temp_df$BRIGHTNESS),
                       #MEAN_CONFIDENCE = mean(temp_df$CONFIDENCE),
                       MEAN_SCAN = mean(temp_df$SCAN),
                       MEAN_TRACK = mean(temp_df$TRACK),
                       MEAN_BRIGHT_T31 = mean(temp_df$BRIGHT_T31),
                       N_pts = nrow(temp_df))
  final_df <- cbind.data.frame(summary_df, pgon) %>% st_as_sf()
  return(final_df)
}

# Function to group multiple polygons from a single timestamp into
# one multipolygon object with summarized characteristics

multipolygon_group <- function(temp_df){
  pgons <- temp_df$geometry
  mpgon <- pgons %>% st_combine() %>% st_cast("MULTIPOLYGON")
  new_df <- temp_df[1,]
  new_df$membership <- temp_df$membership %>% max()
  new_df$MEAN_BRIGHTNESS <- temp_df$MEAN_BRIGHTNESS %>% mean()
  #new_df$MEAN_CONFIDENCE <- temp_df$MEAN_CONFIDENCE %>% mean()
  new_df$MEAN_SCAN <- temp_df$MEAN_SCAN %>% mean()
  new_df$MEAN_TRACK <- temp_df$MEAN_TRACK %>% mean()
  new_df$MEAN_BRIGHT_T31 <- temp_df$MEAN_BRIGHT_T31 %>% mean()
  new_df$N_pts <- temp_df$N_pts %>% sum()
  new_df$geometry <- mpgon
  return(new_df)
}

# Look for any entries that are NULL (if you try to do.call(c, data_check))
# too soon, it will just get rid of the list entry with a NULL entry
null_check <- lapply(data_check, is.null)
null_check <- do.call(c, null_check)
if (any(null_check)){
  data_check[null_check] <- 0
}

data_check <- do.call(c, data_check)

## Are any of the rows at least one (are there any active fire pixels)
active_pixel <- any(data_check >= 1)

# Group the close time entries together
data_list_df <- do.call(rbind.data.frame, data_list[data_check>0])

if (nrow(data_list_df)>0){
  fire_data <- time_group(data_list_df)
  data_available <- 1
} else{
  fire_data <- NA
  data_available <- 0
}

## Check time information for the fire_data data set

# Flag whether or not there is data available from MODIS and/or VIIRS 
if (data_available == 1){
  # Save the intersecting active fire pixel data to check results
  save_fname <- paste0("active_fire_pixel_data_", tolower(instrument), "_", 
                       str_pad(taskID, width = 5, pad = "0"), "_daily.geojson")
  save_output_fname <- paste0(out_dir, "/", save_fname)
  if (file.exists(save_output_fname)){
    # If this file already exists, then delete it to write over it
    unlink(save_output_fname)
  }
  st_write(fire_data, save_output_fname)
}

# If there is active fire pixel data available
if (data_available == 1){
  
  ## Part one, draw polygons around the groups of points
  # Check the dates
  fire_data <- arrange(fire_data, time_start)
  unique_dates <- unique(fire_data$time_start) %>% sort()
  N_dates <- length(unique_dates)
  
  if (N_dates > 1){

    # Create a vector of the time differences
    dt <- c(0, difftime(unique_dates[2:N_dates], unique_dates[1:(N_dates-1)], units = "hours"))
    
    # Remove any results where the return is more than N_days since the last return
    day_check <- dt < 24*N_days
    date_check <- unique_dates <= final_perim$Ig_Date
    idx <- day_check | date_check
    
    for (i in 1:length(idx)){
      if (any(!idx[1:i])){
        idx[i] <- FALSE
      }
    }
    
    # Remove returns after the end date
    unique_dates <- unique_dates[idx]
    N_dates <- length(unique_dates)
    
  }
  
  fire_data <- dplyr::filter(fire_data, time_start %in% unique_dates)
  
  fire_data_union <- list()
  
  for (i in 1:N_dates){
    temp_df <- dplyr::filter(fire_data, time_start <= unique_dates[i])
    temp_df <- mutate(temp_df, summary_date = unique_dates[i])
    fire_data_union[[i]] <- temp_df
  }
  
  fire_data_df <- do.call(rbind.data.frame, fire_data_union)
  
  #sf_use_s2(FALSE)
  
  # The number of time groups
  N <- length(unique_dates)
  
  # Now we need to go through each date and cluster
  
  fire_cluster_list <- list()
  
  for (i in 1:length(unique_dates)){
    
    # What is the current fire_data_df?
    sites_df <- dplyr::filter(fire_data_df, summary_date == unique_dates[i])
    
    #ggplot(data = sites_df) + geom_sf()
    
    ## New seed growing clustering technique
    
    # Initialize k <- 1
    k <- 1
    
    str(sites_df)
    
    if (nrow(sites_df) == 1){
      xy <- st_coordinates(sites_df)[,1:2] %>% t() %>% as.data.frame()
    } else{
      xy <- st_coordinates(sites_df)[,1:2] %>% as.matrix() %>% as.data.frame()
    }
    
    xy_sf <- sites_df$geometry
    
    N <- dim(xy)[1]
    
    Nsims <- 20
    Nits <- 100
    sim_max <- vector(length = Nsims)*NA
    m_list <- list()
    

    nsim <- 1
    
    
    membership <- vector(length = N)
    
    idx <- sample(1:N, size = N, replace = FALSE)
    
    # Create a random arrangement of the points at time t
    xy_nsim <- sites_df[idx,]
    
    membership[1] <- 1
    cluster_xy <- xy_nsim[1,]
    idx_start <- 1
    flag <- TRUE
    
    while (flag){
      
      for (j in 1:Nits){
        
        
        w <- max(cluster_xy$SCAN, cluster_xy$TRACK)*1000*1.5
        units(w) <- 'm'      
        
        if (nrow(cluster_xy) <= 100){
          
          D <- st_distance(cluster_xy, xy_nsim) 
        } else{
          
          ratio_temp <- select_ratio(nrow(cluster_xy))
          
          cluster_pgon <- cluster_xy %>% 
            st_coordinates() %>% 
            st_multipoint() %>% 
            st_concave_hull(cluster_xy, ratio = ratio_temp, allow_holes = FALSE) %>%
            st_sfc(crs = st_crs(cluster_xy)) %>%
            st_as_sf() %>%
            rename(geometry = x)
          D <- st_distance(cluster_pgon, xy_nsim)
        }
        
        
        idx_m <- D <= w
        
        idx_m <- which(apply(idx_m, MARGIN = 2, FUN = any))
        
        if (any(!(idx_m %in% idx_start))){
          cluster_xy <- xy_nsim[idx_m,]
          idx_start <- idx_m
        } else{
          break
        }
        
      }
      
      membership[idx_m] <- k
      flag <- any(membership == 0)
      if (flag){
        k <- k + 1
        idx_values <- which(membership == 0)
        idx_start <- idx_values[1]
        cluster_xy <- xy_nsim[idx_start,]
      }
      
      if (k == max_clusters + 1){
        flag <- FALSE
      }
      
    }
    
    # Note, it is now possible for the membership value to be zero.
    # Do not include these in the perimeters - they are unsorted.
    temp_df <- mutate(xy_nsim, membership) 
    
    #ggplot() + geom_sf(data = temp_df, mapping = aes(color = as.factor(membership)))
    
    fire_cluster_list[[i]] <- temp_df
    
  }
  
  fire_cluster_df <- do.call(rbind.data.frame, fire_cluster_list)
  #ggplot() + geom_sf(data = fire_cluster_df, mapping = aes(color = as.factor(membership))) +
  #  facet_wrap(~summary_date)
  
  # New clustering scheme goes below
  sites_split <- fire_cluster_df %>% group_by(summary_date, membership) %>% group_map(~draw_fire_perimeters(.x), .keep = TRUE)
  
  sites_pgon_df <- do.call(rbind.data.frame, sites_split)
  # Note that the memberships are recreated every time... this avoids having to 
  # glom multiple clusters together and keep track of the numbering
  
   
  #ggplot() + geom_sf(data = fire_cluster_df, mapping = aes(color = as.factor(membership))) +
  #  geom_sf(data = sites_pgon_df, fill = NA, mapping = aes(color = as.factor(membership))) + facet_wrap(~summary_date)

  
  idx <- which(st_geometry_type(sites_pgon_df) != "POLYGON")
  
  sites_pgon_df[idx,]$geometry <- st_buffer(sites_pgon_df[idx,]$geometry, dist = 10)
  
  
  #ggplot() + geom_sf(data = fire_cluster_df) + 
  #  geom_sf(data = sites_pgon_df, mapping = aes(color = as.factor(summary_date)), fill = NA) + geom_sf(data = mtbs_perim, fill = NA, color = 'red')
  
  sites_multipgon_df <- sites_pgon_df %>% group_by(summary_date) %>% group_map(~multipolygon_group(.x), .keep = TRUE)
  
  sites_mp_df <- do.call(rbind.data.frame, sites_multipgon_df)
  
  #sites_copy <- sites_mp_df
  
  # Clean up the polygons working backwards
  N <- nrow(sites_mp_df)
  if (N > 1){
    for (i in (N-1):1){
      # For a given polygon (working backwards), only include the part of the polygon
      # that intersects with the next polygon
      sites_mp_temp1 <- st_make_valid(sites_mp_df$geometry[i])
      sites_mp_temp2 <- st_make_valid(sites_mp_df$geometry[i+1])
      new_sites_mp_temp1 <- st_intersection(sites_mp_temp1, sites_mp_temp2)
      
      
      # check <- tryCatch(st_collection_extract(new_sites_mp_temp1, "POLYGON"), error = function(e){tryCatch(st_collection_extract(new_sites_mp_temp1, "LINESTRING"), error = function(e){st_collection_extract(new_sites_mp_temp1, "POINT")})})

      geo_type <- st_geometry_type(new_sites_mp_temp1)

      if (geo_type != "POLYGON" & geo_type != "MULTIPOLYGON"){
        new_sites_mp_temp1 <- st_buffer(new_sites_mp_temp1, dist = 1)
      }
      
      sites_mp_df$geometry[i] <- new_sites_mp_temp1

      
      
    }
  }

  perim_df <- sites_mp_df %>% mutate(Incid_Name = final_perim$Incid_Name,
                                     Incid_Type = final_perim$Incid_Type,
                                     Ig_Date = final_perim$Ig_Date,
                                     New_ID = final_perim$New_ID,
                                     irwinID = final_perim$irwinID,
                                     perim_avail = TRUE)
  
  
} else{ 
  
  # If there is no active fire pixel data, then create an empty geometry for
  # this event
  
  #empty_df <- st_geometrycollection() %>% st_sfc(crs = 5070)
  empty_df <- st_sf(geometry = st_sfc(st_polygon(), crs = 5070))
  
  temp_df <- data.frame(summary_date = NA,
                        INSTRUMENT = NA,
                        SATELLITE = NA,
                        VERSION = NA,
                        DAYNIGHT = NA,
                        ratio = NA,
                        membership = NA,
                        MEAN_BRIGHTNESS = NA,
                        #MEAN_CONFIDENCE = NA,
                        MEAN_SCAN = NA, 
                        MEAN_TRACK = NA,
                        MEAN_BRIGHT_T31 = NA,
                        perim_avail = FALSE,
                        N_pts = 0,
                        Incid_Name = final_perim$Incid_Name,
                        Incid_Type = final_perim$Incid_Type,
                        New_ID = final_perim$New_ID,
                        Ig_Date = final_perim$Ig_Date,
                        irwinID = final_perim$irwinID)
  
  
  perim_df <- empty_df %>% cbind.data.frame(temp_df)

  
}



# Add the area-aggregated FRP (TOTAL_FRP) here

N_perim <- nrow(perim_df)
TOTAL_FRP <- vector(length = N_perim)
AREA_FRP <- vector(length = N_perim)

if (data_available == 1){
  for (i in 1:N_perim){
    
    temp_pixel <- dplyr::filter(fire_data, time_start == perim_df$summary_date[i])
    if (instrument == "MODIS"){
      temp_pixel <- dplyr::filter(temp_pixel, CONFIDENCE > 0.30)
    }
    if (instrument == "VIIRS"){
      temp_pixel <- dplyr::filter(temp_pixel, CONFIDENCE %in% c("n", "h"))
    }
    
    if (nrow(temp_pixel)>0){
      TOTAL_FRP[i] <- sum(temp_pixel$FRP)
      AREA_FRP[i] <- sum((temp_pixel$SCAN)*(temp_pixel$TRACK))
    } else{
      TOTAL_FRP[i] <- NA
      AREA_FRP[i] <- NA
    }
    
  }
} else{
  TOTAL_FRP <- NA
  AREA_FRP <- NA
}

N_p <- nrow(perim_df)
perim_df <- dplyr::mutate(perim_df, TOTAL_FRP, AREA_FRP) %>% arrange(summary_date) %>% mutate(event_perim = 1:N_p)

pgon_name <- paste0("perim_polygons_", tolower(instrument), "_", 
                    str_pad(taskID, width = 5, pad = "0"), "_daily.geojson")
pgon_output_name <- paste0(out_dir, "/", pgon_name) 

if (file.exists(pgon_output_name)){
  unlink(pgon_output_name)
}

st_write(perim_df, pgon_output_name)

# # Check the result
# ggplot() + geom_sf(data = perim_df, fill = NA) +
#   geom_sf(data = fire_data)
  

# Next we compute the speed between subsequent polygons... in this code
# we want to pick (at least for now) polygons that are approximately matching
# intervals apart with respect to MODIS active fire pixels...
# So in the case of MODIS, let's use Aqua (SATELLITE == 'Aqua'), and in the case 
# of VIIRS, let's use Suomi NPP (SATELLITE == 'N'). Let's specify these in
# satname, so we will filter for SATELLITE == satname

# For the file ending in _daily.R, we want to compare to NIROPS, so we will
# instead use night time returns for Terra MODIS only.

if (data_available == 1 & N_p > 1){
  
  temp_data <- perim_df %>% dplyr::filter(SATELLITE == satname, DAYNIGHT == "N")
  
  # Initialize a vector for the start time of each grouped set of measurements
  time_start <- list()
  
  # Start with the first time stamp
  time_start[[1]] <- temp_data$summary_date[1]
  
  # Initialize an index counter to keep track of the beginning of each new
  # group starting with the first one
  idx_start <- 1 %>% as.integer()
  
  # Let's only keep the most nadir returns when multiple scans exist
  temp_list <- list()
  temp_list[[1]] <- temp_data[1,]
  
  if (nrow(temp_data) > 1){
    # Initialize a reference pixel size
    pixel_size <- temp_data$MEAN_SCAN[1]*temp_data$MEAN_TRACK[1]
  
    # For the rest of the time stamps
    for (i in 2:nrow(temp_data)){
  
      # Let's check the time for the next row
      check_time <- temp_data$summary_date[i]
      delta_time <- difftime(check_time, time_start[[idx_start]], units = "secs")
  
      # Let's say the change in time is less than a given number of hours apart.
      # When only using one satellite, this can mean that there are multiple scans
      # picking up the same fire. Let's only take the most nadir of them
      if (delta_time < 6*3600){
  
        pixel_check <- temp_data$MEAN_SCAN[i]*temp_data$MEAN_TRACK[i]
  
        if (pixel_check < pixel_size){
          # If the candidate pixel difference from estimated nadir is smaller, we keep it
          pixel_size <- pixel_check
  
          time_start[[idx_start]] <- temp_data$summary_date[i]
  
          # And we write over the current temp list to keep this one instead
          temp_list[[idx_start]] <- temp_data[i,]
  
        }
  
        # Else we keep everything that has been initialized already and move
        # on to the next row without setting an end time.
  
      }else {
  
  
        # And start a new time group
        idx_start <- idx_start + as.integer(1)
  
        # Reinitialize the start time, pixel size and current temp list element
        time_start[[idx_start]] <- temp_data$summary_date[i]
        pixel_size <- temp_data$MEAN_SCAN[i]*temp_data$MEAN_TRACK[i]
        temp_list[[idx_start]] <- temp_data[i,]
      }
    }
    time_start <- do.call(c, time_start)
    temp_data_updated <- do.call(rbind.data.frame, temp_list)
    temp_data_updated <- temp_data_updated %>% mutate(time_start)
  
  } else{
    time_start <- time_start[[1]]
    temp_data_updated <- temp_data %>% mutate(time_start)
  }
  
  filtered_perim_df <- temp_data_updated
  
  colnames(filtered_perim_df)
  
  
  #ggplot() + geom_point(data = filtered_perim_df, mapping = aes(x = time_start, y = TOTAL_FRP)) +
  #  xlim(as.POSIXct("2020-10-13 00:00:00", tz = "UTC"), as.POSIXct("2020-10-25 00:00:00", tz = "UTC"))
  
  # We should first check if we have data and subsequent polygons...
  
  N_perim <- nrow(filtered_perim_df)

}




if (data_available == 1 & exists("N_perim")){
  if (N_perim > 1){
    
    vector_flag <- 1
    
    dt <- difftime(filtered_perim_df$summary_date[2:nrow(filtered_perim_df)], 
                   filtered_perim_df$summary_date[1:(nrow(filtered_perim_df)-1)], 
                   units = "hours") %>% as.numeric()
    
    filtered_perim_df$dt <- c(NA, dt)
    
    count <- 1
    new_ignition <- FALSE
    spot_check <- FALSE
    fire_spread_list <- list()
    
    for (i in 2:nrow(filtered_perim_df)){
      
      # Assign the starting and ending polygons (pgon1 and pgon2, respectively)
      
      pgon1 <- filtered_perim_df[(i-1),] 
      pgon2 <- filtered_perim_df[i,]
      
      if (st_geometry_type(pgon1) == "GEOMETRYCOLLECTION"){
        pgon1 <- pgon1 %>% st_collection_extract("POLYGON") %>% st_remove_holes()
      } else{
        pgon1 <- tryCatch(pgon1 %>% st_cast("POLYGON") %>% st_remove_holes(), error = function(e){st_buffer(pgon1, dist = 1) %>% st_remove_holes()})
        
      }
      
      if (st_geometry_type(pgon2) == "GEOMETRYCOLLECTION"){
        pgon2 <- pgon2 %>% st_collection_extract("POLYGON") %>% st_remove_holes()
      } else{
        pgon2 <- tryCatch(pgon2 %>% st_cast("POLYGON") %>% st_remove_holes(), error = function(e){st_buffer(pgon2, dist = 1) %>% st_remove_holes()})
      }
      
      event_perim_1 <- filtered_perim_df[(i-1),]$event_perim
      event_perim_2 <- filtered_perim_df[i,]$event_perim
      
      # Determine how many perimeters there are for each time point
      N1 <- nrow(pgon1)
      N2 <- nrow(pgon2)
      
      # Determine which of the previous polygons intersect the current ones
      # Rows refer to polygons in pgon1 (previous) and columns refer to polygons in 
      # pgon2 (current)
      pgon_grid <- (st_intersects(pgon1, pgon2, sparse = FALSE) & !st_touches(pgon1, pgon2, sparse = FALSE)) | t(st_contains_properly(st_buffer(pgon2, dist = 10), pgon1, sparse = FALSE))
      
      # Example grid
      # FALSE TRUE  FALSE
      # TRUE  FALSE FALSE
      
      # At this point, we may want to check each TRUE to ensure there is actual
      # overlap or switch to a different check (st_contains?)
      
      for (j in 1:N2){
        
        # Convert the jth polygon in polygon 2 to a multilinestring
        pgon2_ls <- pgon2[j,] %>% st_cast("MULTILINESTRING")
        
        # Calculate the area of this polygon and convert to sq. km
        A2 <- st_area(pgon2[j,]) %>% set_units(km^2) %>% as.numeric()
        
        # Create a buffer around the current polygon 
        pgon2_buffer <- pgon2[j,] %>% st_buffer(dist = 200)
        pgon2_temp <- pgon2[j,]
        
        # If none of the previous polygons are inside this one, then the fire
        # may have spotted from ember throw, BUT it may also be a new ignition
        spot_check <- !any(pgon_grid[,j])
        
        
        if (spot_check){
          
          d_check <- st_distance(pgon2_temp, pgon1) %>% as.numeric()
          if (all(d_check > spot_threshold)){
            p <- 0 # Anything greater than 4 km away from previous fire perimeters will be considered new ignition
            
          } else{
            
            # If there is a previous speed vector, we can check its direction (not included here...) 
            if (i > 2){
              
              # Assign to the closest previous polygon for now
              # Placeholder for additional spotting code 
              idx_match <- which.min(d_check)
              p <- 0.6
              
            } else{
              p <- 0
              
            }
            
          }
          
          if (p < 0.5){
            spot_check <- FALSE
            new_ignition <- TRUE
          }
        }
        
        if (!new_ignition){ # Otherwise, we do not add a new row to fire_spread_list for new ignitions
          
          if (spot_check){
            idx_grid <- idx_match #1:N1
            # Collect all the previous polygons and find the longest perimeter length to determine
            # the number of sample points we need to use to compare with the current one (might be
            # much smaller than the previous, since it's from ember throw)
            
            # Some spots are small, so we need more points to get an accurate estimate
            # of the shortest distance between the spot and the previous perimeter
            M1 <- pgon1 %>% st_cast("MULTILINESTRING") %>% st_length() %>% max() %>% as.numeric() * points_per_meter 
            M1 <- as.integer(M1)
            M2 <- as.integer(st_length(pgon2_ls)*points_per_meter)
            M <- max(c(M1, M2, 1))
            
          } else {
            
            # Need to add a new ignition flag to address the case where there are
            # no previous polygons inside this one
            
            # Otherwise, determine which of the previous polygons are inside the
            # current one to loop through them and find the optimal distances
            idx_grid <- which(pgon_grid[,j])
            
            # Let M be the number of points to sample based on the perimeter of polygon 2
            M <- as.integer(st_length(pgon2_ls) * points_per_meter)
            M <- max(c(M, 1)) # Just in case M gets assigned to zero...
            
          }
          
          # Create vectors that include the date and change in time
          time_start <- rep(pgon1$summary_date[1], times = M)
          time_end <- rep(pgon2$summary_date[1], times = M)
          dt <- rep(pgon2$dt[1], times = M)
          
          # Sample multilinestring Y, M times
          Y <- st_sample(pgon2_ls, size = M, type = 'regular') %>% st_cast("POINT") %>% st_as_sf() 
          
          # For some reason, it doesn't like this to follow the above in one line sometimes
          Y <- Y %>% mutate(pt_id = 1:nrow(Y))
          
          # Now we want to find the distances from each of these previous polygons
          # to the next one and use the shortest distances
          
          # Initialize a list for storing linestrings between matched points
          L_list <- list()
          
          D <- array(data = NA, dim = c(M, M, length(idx_grid)))
          
          X_list <- list()
          
          for (k in 1:length(idx_grid)){
            
            idx_val <- idx_grid[k]
            
            # Convert the kth polygon from the previous perimeter(s) to a multilinestring
            #pgon1_check <- pgon1[idx_val,]
            pgon1_ls <- pgon1[idx_val,] %>% st_cast("MULTILINESTRING")
            
            # Sample the linestring
            X_temp <- st_sample(pgon1_ls, size = M, type = 'regular') %>% st_cast("POINT") %>% st_as_sf() %>% mutate(pt_id = 1:M)
            
            # In the abbreviated version, we only want the shortest distance from polygon 1 to
            # polygon 2
            D[,,k] <- st_distance(X_temp, Y) 
            X_list[[k]] <- X_temp %>% mutate(k, idx_val)
          }
          
          X_df <- do.call(rbind.data.frame, X_list)
          
          
          # For each point in Y, we want to know the closest point in X
          D_min <- array(data = NA, dim = c(nrow(Y), length(idx_grid)))
          D_min_xlocs <- array(data = NA, dim = c(nrow(Y), length(idx_grid)))
          
          for (jj in 1:nrow(Y)){
            for (k in 1:length(idx_grid)){
              #Y_min_xloc[jj] <- which.min(D[,jj,])
              xlocs_temp <- which(D[,jj,k] == min(D[,jj,k]))[1]
              D_min_xlocs[jj,k] <- xlocs_temp
              D_min[jj,k] <- min(D[,jj,k])
            }
          }
          
          # You still need to determine which of the previous perimeters is closest
          # and what the associated distance is
          
          D_min_by_yloc <- array(data = NA, dim = c(nrow(Y), 1))
          D_min_xloc <- array(data = NA, dim = c(nrow(Y), 1))
          D_min_kloc <- array(data = NA, dim = c(nrow(Y), 1))
          
          for (jj in 1:nrow(Y)){
            D_min_by_yloc[jj] <- min(D_min[jj,])
            D_min_kloc[jj] <- which.min(D_min[jj,])
            D_min_xloc[jj] <- D_min_xlocs[jj, D_min_kloc[jj]]
          }
          
          # Now we have an array of the minimum distances between each point in 
          # Y and each starting perimeter Xp
          
          # Now we want the MAXIMUM of these minima to find the fastest rate 
          # of spread
          fastest_jk <- which(D_min_by_yloc == max(D_min_by_yloc), arr.ind = TRUE)
          if (nrow(fastest_jk)>1){
            fastest_jk <- fastest_jk[1,]
          }
          
          # Need indices here instead of D_min[fastest_jk]
          D_fastest_val <- D_min_by_yloc[fastest_jk[1], fastest_jk[2]]
          yloc <- fastest_jk[1]
          kloc <- D_min_kloc[yloc]
          xloc <- D_min_xloc[yloc]
          
          pt1_df <- X_df %>% filter(k == kloc & pt_id == xloc) 
          pt1 <- pt1_df %>% dplyr::select(x)
          pt2 <- Y[yloc,] %>% dplyr::select(x)
          
          # Create a line string from the two points
          L_temp <- rbind(pt1, pt2) %>% st_as_sfc() %>% st_as_sf(crs = crs_utm) %>% 
            summarize() %>% 
            st_cast("LINESTRING")
          
          #check_path <- st_contains(pgon2, L_temp, sparse = FALSE)
          
          # Make this more flexible
          test_overlap <- st_intersection(pgon2, L_temp) %>% st_length() %>% as.numeric()
          test_original <- L_temp %>% st_length() %>% as.numeric()
          L_compare <- test_overlap/test_original
          
          #if (!any(check_path)){
          if (max(L_compare) < r_overlap){
            # If the line segment does not fall within polygon 2, then look for
            # a path that does fall within polygon 2
            X_dist <- st_distance(X_df, pt2) %>% c()
            X_df_temp <- X_df %>% mutate(X_dist) %>% arrange(X_dist)
            flag <- 0
            for (r in 1:nrow(X_df_temp)){
              pt1 <- X_df_temp[r,]
              L_temp <- pt1 %>% dplyr::select(x) %>% rbind(pt2) %>% st_as_sfc() %>% 
                st_as_sf(crs = crs_utm) %>% 
                summarize() %>% 
                st_cast("LINESTRING")
              #flag <- st_contains(pgon2_temp, L_temp, sparse = FALSE)*1.0
              test_overlap <- st_intersection(pgon2, L_temp) %>% st_length() %>% as.numeric()
              test_original <- L_temp %>% st_length() %>% as.numeric()
              L_compare <- test_overlap/test_original
              flag <- max(L_compare) < r_overlap
              if (flag == 1){
                #D_fastest_val <- pt1$X_dist
                break
              }
            }
            if (flag == 0){
              #D_fastest_val <- pt1$X_dist
              pt1 <- X_df_temp[1,] %>% dplyr::select(x)
              #L_temp <- rbind(pt1, pt2) %>% st_as_sfc() %>% st_as_sf(crs = crs_utm) %>% summarize() %>% st_cast("LINESTRING")
            }
          }
          
          #L_list[[1]] <- L_temp
          
          # Assign A1 based on kloc
          #idx_match <- idx_grid[kloc]
          
          if (!new_ignition & !spot_check){
            A1 <- pgon1[idx_grid,] %>% st_intersection(pgon2_temp) %>% st_area() %>% sum() %>% set_units(km^2) %>% as.numeric()
          } else{
            A1 <- 0
          }
          
          delta_t <- dt[1] # hours
          d_xy <- as.numeric(0.001*st_distance(pt1, pt2)) # km
          v_xy <- d_xy/delta_t # km/hr
          d_elev <- 0.001 * elevation_distance(pt1, pt2, elev_utm, crs_utm) # km
          v_elev <- d_elev/delta_t # km/hr
          dA <- A2-A1 # km2
          
          # Account for small numerical issues in comparing the areas - how many sq. meters
          # of error do we want to allow? Maybe on the order of a Landsat pixel? 1000 sq. meters?
          if (dA<0){
            if (abs(dA) < 0.001){
              dA <- 0
            } else{
              dA <- NA
            }
          }
          
          # ## Troubleshooting BEGIN
          # if (dA<0){
          #   stop("negative change in area found")
          # }
          # 
          # ## Troubleshooting END
          dAdt <- dA/delta_t #km2/hr
          
          time_start <- time_start[1]
          time_end <- time_end[1]
          dt <- dt[1]
          
          #L_sf <- lapply(L_list, st_as_sf)
          #L <- do.call(rbind.data.frame, L_sf)
          
          #X_fit <- pt1_df %>% dplyr::select(-idx_val)
          #Y_fit <- Y[yloc,]
          
          # Add coordinates here
          xy1 <- st_coordinates(pt1)
          x1 <- xy1[1]
          y1 <- xy1[2]
          
          xy2 <- st_coordinates(pt2)
          x2 <- xy2[1]
          y2 <- xy2[2]
          
          temp_df <- data.frame(x1, y1, x2, y2, time_start, time_end, dt, dA, dAdt, d_xy, v_xy, d_elev, v_elev, spot = spot_check, burn_avail = TRUE)
          
          fire_spread_list[[count]] <- temp_df
          
          count <- count + 1
          
          print(paste0("Working on ", count))
          
        } else{
          new_ignition <- FALSE
        }
        
      }
      
      
      # Let's summarize
      
      fire_spread_df <- do.call(rbind.data.frame, fire_spread_list) %>%
        mutate(data = active_pixel,
               Incid_Name = final_perim$Incid_Name,
               Incid_Type = final_perim$Incid_Type,
               Ig_Date = final_perim$Ig_Date,           
               New_ID = final_perim$New_ID,
               irwinID = final_perim$irwinID)
      
    }
    
    
  } 
    
    
    
}

if (!(exists("vector_flag"))){
  
  fire_spread_df <- data.frame(x1 = NA, y1 = NA, x2 = NA, y2 = NA, 
                               time_start = NA,
                               time_end = NA,
                               time_mean = NA,
                               dt = NA,
                               d_xy = NA,
                               v_xy = NA,
                               d_elev = NA,
                               v_elev = NA,
                               dA = NA,
                               dAdt = NA, 
                               data = active_pixel,
                               spot = FALSE,
                               burn_avail = FALSE,
                               Incid_Name = final_perim$Incid_Name,
                               Incid_Type = final_perim$Incid_Type,
                               New_ID = final_perim$New_ID,
                               Ig_Date = final_perim$Ig_Date,
                               irwinID = final_perim$irwinID)
  
}

if (exists("vector_flag")){
  dt_mean_sec <- round((fire_spread_df$dt)*3600*0.5)
  time_mean <- fire_spread_df$time_start + seconds(dt_mean_sec)
  
  fire_spread_df <- mutate(fire_spread_df, time_mean)

}

burn_name <- paste0("burn_direction_", tolower(instrument), "_", 
                    str_pad(taskID, width = 5, pad = "0"),"_daily.csv")
burn_output_name <- paste0(out_dir, "/", burn_name)


if (file.exists(burn_output_name)){
  unlink(burn_output_name)
}

write.csv(fire_spread_df, burn_output_name, row.names = FALSE)

