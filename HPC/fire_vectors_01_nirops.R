#!/usr/bin/env Rscript
user_inputs <- commandArgs(trailingOnly = TRUE)

# Check format
taskID <- as.numeric(user_inputs[[1]])

## HPC

# Output directory
out_dir <- "/scratch/alpine/nihe1301/fire_speed/NIROPS"

# Working directory
wd <- "/home/nihe1301/fire_speed"

## BEGIN LOCAL TROUBLESHOOTING (2026-03-25)
library(ggplot2)
library(ggspatial)
theme_set(theme_minimal(base_size = 14))
taskID <- 120 # Each taskID is assigned to a unique fire - this one is the East Troublesome

wd <- "~/Documents/R/FireVectors"
setwd(wd)
out_dir <- wd

## END LOCAL TROUBLESHOOTING


# Compute fire velocity for NIROPS perimeters

# Nicole Hemming-Schroeder, September 3, 2025
# Revised, March 25, 2026


library(sf)
library(dplyr)
library(raster)
library(stringr)
library(lubridate)
library(nngeo)

################################ USER INPUTS ###################################

# Data directory for NIROPS fire perimeters
perimeter_dir <- "NIROPS"

# Determine your fraction of points per x meters
x <- 100
points_per_meter <- 2/x

# How far do we let a fire spot before we call it a new ignition?
spot_threshold <- 4000 # meters

# By what fraction must a suggested fire path overlap the next fire perimeter?
r_overlap <- 0.75

# FYI: There is a lot of hard coding for EPSG:5070 throughout
crs_utm <- CRS("EPSG:5070")

# Elevation input
elev <- raster("LC20_Elev_220.tif") 

################################################################################
set.seed(0)

# Set working directory
setwd(wd)

# Read in NIROPS
fname_perims <- list.files(perimeter_dir, pattern = glob2rx("nirops_mtbs*.shp"), full.names = TRUE)

perims <- st_read(fname_perims) 

unique_id <- unique(perims$New_ID) %>% sort()

# Read in MTBS
mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp")

# Collect the resolution of the elevation raster
ds <- res(elev)[1]

# Elevation raster CRS
elev_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Set up elevation-based distance function

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

N_nirops <- length(unique_id)

set.seed(taskID*100)
#for (taskID in 1:length(unique_id)){


temp_df <- dplyr::filter(perims, New_ID == unique_id[taskID]) %>% arrange(UTC)

final_perim <- mtbs %>% dplyr::filter(New_ID == unique_id[taskID])

#ggplot() + geom_sf(data = temp_df, mapping = aes(color = UTC), fill = NA) +
#  geom_sf(data = final_perim, fill = NA, color = 'red')


elev_ext <- temp_df[nrow(temp_df),] %>% st_transform(crs = 5070) %>% st_buffer(dist = 10000) %>% st_transform(crs = elev_crs) %>% extent()

# Crop to a buffered extent of the polygon
elev_crop <- crop(elev, elev_ext)

# Project the elevation raster into the specified CRS
elev_utm <- projectRaster(elev_crop, crs = crs_utm)
elev_utm[elev_utm == -9999] <- NA


# Next we compute the speed between subsequent polygons

filtered_perim_df <- temp_df %>% mutate(summary_date = UTC) %>% st_transform(crs = 5070)


filtered_perim_df$date <- filtered_perim_df$UTC %>% as.POSIXct(tz = "UTC")
filtered_perim_df$hr <- hour(filtered_perim_df$date)

str(filtered_perim_df)

filtered_perim_df <- filtered_perim_df %>% dplyr::filter(hr >=1, hr <= 9)

N_perim <- nrow(filtered_perim_df)

if (N_perim > 1){
  
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
    
    # FALSE TRUE  FALSE
    # TRUE  FALSE FALSE
    
    # # TROUBLESHOOTING START
    # temp_pgon1 <- mutate(pgon1, id = 1:nrow(pgon1))
    # temp_pgon2 <- mutate(pgon2, id = 1:nrow(pgon2))
    # 
    # ggplot() + geom_sf(data = temp_pgon1, mapping = aes(fill = as.factor(id))) + scale_fill_discrete(name = "Polygon")
    # ggplot() + geom_sf(data = temp_pgon2, mapping = aes(fill = as.factor(id))) + scale_fill_discrete(name = "Polygon")
    # # TROUBLESHOOTING END
    for (j in 1:N2){
      
      # Convert the jth polygon in polygon 2 to a multilinestring
      pgon2_ls <- pgon2[j,] %>% st_cast("MULTILINESTRING")
      
      # Create a buffer around the current polygon 
      pgon2_buffer <- pgon2[j,] %>% st_buffer(dist = 200)
      pgon2_temp <- pgon2[j,]
      
      # If none of the previous polygons are inside this one, then the fire
      # may have spotted from ember throw, BUT it may also be a new ignition
      spot_check <- !any(pgon_grid[,j])
      #ggplot() + geom_sf(data = pgon1, fill = NA) + geom_sf(data = pgon2_temp, color = 'red', fill = NA)
      
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
        
        #ggplot() + geom_sf(data = Y)
        
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
          
          
          #ggplot() + geom_sf(data = Y) + geom_sf(data = X_temp, color = 'red')
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
            D_min_xlocs[jj,k] <- which(D[,jj,k] == min(D[,jj,k]))
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
        
        #ggplot() + geom_sf(data = Y) + geom_sf(data = X_df) + geom_sf(data = pt1, color = 'darkgreen', size = 5) + geom_sf(data = pt2, color = 'darkred', size = 5) 
        
        # Create a line string from the two points
        L_temp <- rbind(pt1, pt2) %>% st_as_sfc() %>% st_as_sf(crs = crs_utm) %>% 
          summarize() %>% 
          st_cast("LINESTRING")
        
        #check_path <- st_contains(pgon2, L_temp, sparse = FALSE)
        
        # Make this more flexible
        test_overlap <- st_intersection(pgon2, L_temp) %>% st_length() %>% as.numeric()
        test_original <- L_temp %>% st_length() %>% as.numeric()
        L_compare <- test_overlap/test_original
        
        #ggplot() + geom_sf(data = pgon1, fill = NA) + geom_sf(data = L_temp) + geom_sf(data = pgon2, fill = NA) + geom_sf(data = pt1, color = 'darkgreen', size = 5) + geom_sf(data = pt2, color = 'darkred', size = 5) 
        
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

            pt1 <- X_df_temp[1,] %>% dplyr::select(x)
          }
        }
        

        delta_t <- dt[1] # hours
        d_xy <- as.numeric(0.001*st_distance(pt1, pt2)) # km
        v_xy <- d_xy/delta_t # km/hr
        d_elev <- 0.001 * elevation_distance(pt1, pt2, elev_utm, crs_utm) # km
        v_elev <- d_elev/delta_t # km/hr
        
        time_start <- time_start[1]
        time_end <- time_end[1]
        dt <- dt[1]
        
        # Add coordinates here
        xy1 <- st_coordinates(pt1)
        x1 <- xy1[1]
        y1 <- xy1[2]
        
        xy2 <- st_coordinates(pt2)
        x2 <- xy2[1]
        y2 <- xy2[2]
        
        temp_df <- data.frame(x1, y1, x2, y2, time_start, time_end, dt, d_xy, v_xy, d_elev, v_elev, spot = spot_check, burn_avail = TRUE)
        
        fire_spread_list[[count]] <- temp_df
        
        count <- count + 1
        
        print(paste0("Working on ", count))
        
      } else{
        new_ignition <- FALSE
      }
      
    }
    
    
    # Let's summarize
    
    fire_spread_df <- do.call(rbind.data.frame, fire_spread_list) %>%
      mutate(data = NA,
             Incid_Name = final_perim$Incid_Name,
             Incid_Type = final_perim$Incid_Type,
             Ig_Date = final_perim$Ig_Date,           
             New_ID = final_perim$New_ID,
             irwinID = final_perim$irwinID)
    
    dt_mean_sec <- round((fire_spread_df$dt)*3600*0.5)
    time_mean <- as.POSIXct(fire_spread_df$time_start, tz = "UTC") + seconds(dt_mean_sec)
    
    fire_spread_df <- mutate(fire_spread_df, time_mean)
    
  }
  
  
} else{
  
  fire_spread_df <- data.frame(x1 = NA, y1 = NA, x2 = NA, y2 = NA, 
                               time_start = NA,
                               time_end = NA,
                               time_mean = NA,
                               dt = NA,
                               d_xy = NA,
                               v_xy = NA,
                               d_elev = NA,
                               v_elev = NA,
                               data = NA,
                               spot = FALSE,
                               burn_avail = FALSE,
                               Incid_Name = final_perim$Incid_Name,
                               Incid_Type = final_perim$Incid_Type,
                               New_ID = final_perim$New_ID,
                               Ig_Date = final_perim$Ig_Date,
                               irwinID = final_perim$irwinID)
  
}

burn_output_name <- paste0(out_dir, "/burn_direction_nirops_", 
                    str_pad(taskID, width = 5, pad = "0"),".csv")

if (file.exists(burn_output_name)){
  unlink(burn_output_name)
}

write.csv(fire_spread_df, burn_output_name, row.names = FALSE)

str(fire_spread_df)

idx <- which(fire_spread_df$d_xy > 1)

p <- ggplot() + geom_sf(data = filtered_perim_df, color = 'black', fill = NA) +
  geom_segment(fire_spread_df[idx,], mapping = aes(x = x1, y = y1, xend = x2, yend = y2, color = v_xy),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"), linewidth = 1.2) +
  scale_color_viridis_c(name = "Speed (km/hr)", begin = 0, end = 0.6) +
  annotation_scale(location = "bl",        # bottom left
                   width_hint = 0.2, text_cex = 12/11) + xlab("") + ylab("") +
  annotation_north_arrow(
    location = "br",              # "tl", "tr", "bl", or "br"
    which_north = "true",         # or "grid"
    pad_x = unit(0.15, "in"),
    pad_y = unit(0.15, "in"),
    style = north_arrow_fancy_orienteering(text_size = 14),
  )

p

save_fname <- paste0("NIROPS/events/nirops_test_", str_pad(taskID, width = 5, pad = "0"), ".png")
ggsave(save_fname, plot = p, width = 8, height = 4, dpi = 600)


