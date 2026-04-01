library(dplyr)
library(sf)
library(ggplot2)
library(lubridate)

Sys.setenv(TZ = "UTC")

# Set working directory
setwd("~/Documents/R/FireVectors")

theme_set(
  theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(
      # Axis text (tick labels) and titles
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      
      # Plot titles and subtitles
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 10),
      
      # Legend titles and text
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
)



# Read in speed vector data sets
nirops_vectors <- read.csv("NIROPS/burn_direction_nirops.csv")
modis_vectors <- read.csv("NIROPS/burn_direction_daily.csv")
#viirs_vectors <- read.csv("data/burn_direction_viirs.csv")
#modis_vectors <- read.csv("data/burn_direction_modis.csv")

# Read in perimeter progression data sets
nirops_perims <- st_read("NIROPS/nirops_mtbs_match.shp") %>% 
  dplyr::filter(IrwinID %in% nirops_vectors$irwinID)
# viirs_perims <- st_read("data/perim_polygons_viirs.geojson") %>% 
#   dplyr::filter(New_ID %in% nirops_vectors$New_ID)
modis_perims <- st_read("data/perim_polygons_modis.geojson") %>% 
  dplyr::filter(New_ID %in% nirops_vectors$New_ID)

# Pull a key from the NIROPS speed vector data set to match Irwin ID to FireVectors New_ID
nirops_key <- dplyr::select(nirops_vectors, IrwinID = irwinID, New_ID = New_ID) %>% unique()
nirops_perims <- right_join(nirops_perims, nirops_key, by = "IrwinID")

# Look at structure of NIROPS speed vector data set
str(nirops_vectors)

# Pull the unique New_ID values to match to FireVectors
nirops_ids <- nirops_vectors %>% pull(New_ID) %>% unique() %>% sort()

# # Filter FireVectors data derived from VIIRS
# viirs_filter <- viirs_vectors %>% dplyr::filter(New_ID %in% nirops_ids)
# 
# # Number of unique fire events available for VIIRS
# N_viirs <- viirs_filter %>% pull(New_ID) %>% unique() %>% length()

# Filter FireVectors data derived from MODIS
modis_filter <- modis_vectors %>% dplyr::filter(New_ID %in% nirops_ids)

# Number of unique fire events available for MODIS
N_modis <- modis_filter %>% pull(New_ID) %>% unique() %>% length()

# For FireVectors VIIRS and MODIS data sets, compute the daily average max
# speed and burn direction associated with the maximum fire speed

#viirs_filter$day <- as.Date(viirs_filter$time_mean)
modis_filter$day <- as.Date(modis_filter$time_mean)
nirops_vectors$day <- as.Date(nirops_vectors$time_mean)

# Need a function to compute the direction; do you want to do a confusion matrix
# with direction categories?

compute_angle <- function(x1, y1, x2, y2){
  theta_deg <- atan2(y2 - y1, x2 - x1) * 180 / pi
  return(theta_deg)
}

direction_classifier <- function(theta_deg){
  dir <- ifelse(theta_deg > -22.5 & theta_deg <= 22.5,  "E",
                ifelse(theta_deg >  22.5 & theta_deg <= 67.5,  "NE",
                       ifelse(theta_deg >  67.5 & theta_deg <= 112.5, "N",
                              ifelse(theta_deg > 112.5 & theta_deg <= 157.5, "NW",
                                     ifelse(theta_deg > 157.5 | theta_deg <= -157.5, "W",
                                            ifelse(theta_deg > -157.5 & theta_deg <= -112.5, "SW",
                                                   ifelse(theta_deg > -112.5 & theta_deg <= -67.5,  "S",
                                                          ifelse(theta_deg > -67.5 & theta_deg <= -22.5,   "SE",
                                                                 NA))))))))
  
  return(dir)
}

# Classify each 
#viirs_filter$theta_deg <- compute_angle(viirs_filter$x1, viirs_filter$y1, viirs_filter$x2, viirs_filter$y2)
#viirs_filter$dir <- direction_classifier(viirs_filter$theta_deg)
modis_filter$theta_deg <- compute_angle(modis_filter$x1, modis_filter$y1, modis_filter$x2, modis_filter$y2)
modis_filter$dir <- direction_classifier(modis_filter$theta_deg)
nirops_vectors$theta_deg <- compute_angle(nirops_vectors$x1, nirops_vectors$y1, nirops_vectors$x2, nirops_vectors$y2)
nirops_vectors$dir <- direction_classifier(nirops_vectors$theta_deg)

# Need to aggregate to a daily summary - sometimes there are multiple fires
# going on at the same time within the same event and there are likely also cases
# for NIROPS (and definitely for the others) where there are multiple time steps
# in a day

#viirs_list <- list()
modis_list <- list()
nirops_list <- list()
#viirs_count <- 1
modis_count <- 1
nirops_count <- 1

for (i in 1:length(nirops_ids)){

  # # Pull the specific event for VIIRS
  # temp_df <- dplyr::filter(viirs_filter, New_ID == nirops_ids[i])
  # 
  # # Next, pull each individual day of the event
  # temp_days <- unique(temp_df$day)
  # 
  # # For each day, pull summary statistics
  # for (j in 1:length(temp_days)){
  #   temp_df_by_day <- dplyr::filter(temp_df, day == temp_days[j])
  #   
  #   idx_th <- which.max(temp_df_by_day$v_xy)
  #   temp_th <- temp_df_by_day$theta_deg[idx_th]
  #   
  #   # Need to aggregate by time step (some days have multiple)
  #   temp_times <- unique(temp_df_by_day$time_mean)
  #   
  #   for (k in 1:length(temp_times)){
  #     temp_df_by_time <- dplyr::filter(temp_df_by_day, time_mean == temp_times[k])
  #     idx <- which.max(temp_df_by_time$v_xy)
  #     temp_save <- temp_df_by_time[idx,] %>% rename(max_speed = v_xy)
  #     temp_save$max_th <- temp_th
  #     viirs_list[[viirs_count]] <- temp_save
  #     viirs_count <- viirs_count + 1
  #   }
  #   
  # }
  
  # MODIS
  # Pull the specific event 
  temp_df <- dplyr::filter(modis_filter, New_ID == nirops_ids[i])
  
  # Next, pull each individual day of the event
  temp_days <- unique(temp_df$day)
  
  # For each day, pull summary statistics
  for (j in 1:length(temp_days)){
    temp_df_by_day <- dplyr::filter(temp_df, day == temp_days[j])
    
    idx_th <- which.max(temp_df_by_day$v_xy)
    temp_th <- temp_df_by_day$theta_deg[idx_th]
    
    # Need to aggregate by time step (some days have multiple)
    temp_times <- unique(temp_df_by_day$time_mean)

    
    for (k in 1:length(temp_times)){
      temp_df_by_time <- dplyr::filter(temp_df_by_day, time_mean == temp_times[k])
      idx <- which.max(temp_df_by_time$v_xy)
      temp_save <- temp_df_by_time[idx,] %>% rename(max_speed = v_xy)
      temp_save$max_th <- temp_th
      modis_list[[modis_count]] <- temp_save
      modis_count <- modis_count + 1
    }
    
  }
  
  # NIROPS
  # Pull the specific event 
  temp_df <- dplyr::filter(nirops_vectors, New_ID == nirops_ids[i])
  
  # Next, pull each individual day of the event
  temp_days <- unique(temp_df$day)
  
  # For each day, pull summary statistics
  for (j in 1:length(temp_days)){
    temp_df_by_day <- dplyr::filter(temp_df, day == temp_days[j])
    
    idx_th <- which.max(temp_df_by_day$v_xy)
    temp_th <- temp_df_by_day$theta_deg[idx_th]
    
    # Need to aggregate by time step (some days have multiple)
    temp_times <- unique(temp_df_by_day$time_mean)
    for (k in 1:length(temp_times)){
      temp_df_by_time <- dplyr::filter(temp_df_by_day, time_mean == temp_times[k])
      idx <- which.max(temp_df_by_time$v_xy)
      temp_save <- temp_df_by_time[idx,] %>% rename(max_speed = v_xy)
      temp_save$max_th <- temp_th
      nirops_list[[nirops_count]] <- temp_save
      nirops_count <- nirops_count + 1
    }
    
  }
  
  
}

# Collate the results
#viirs_df <- do.call(rbind.data.frame, viirs_list)
modis_df <- do.call(rbind.data.frame, modis_list)
nirops_df <- do.call(rbind.data.frame, nirops_list)

#str(viirs_df)

# Group by New_ID and day to summarize the maximum speed estimates over
# a 24 hour period

# viirs_summary <- viirs_df %>% group_by(New_ID, day) %>% summarize(dt = sum(dt), max_speed = mean(max_speed), max_th = max_th[1]) %>% dplyr::filter(dt > 20 & dt < 28)
# viirs_summary
# 
# viirs_summary <- viirs_summary %>% rename(dt_viirs = dt, max_speed_viirs = max_speed, max_th_viirs = max_th)

modis_summary <- modis_df %>% group_by(New_ID, day) %>% summarize(dt = sum(dt), max_speed = mean(max_speed), max_th = max_th[1]) %>% dplyr::filter(dt > 20 & dt < 28)
modis_summary

modis_summary <- modis_summary %>% rename(dt_modis = dt, max_speed_modis = max_speed, max_th_modis = max_th)

nirops_summary <- nirops_df %>% group_by(New_ID, day) %>% summarize(dt = sum(dt), max_speed = mean(max_speed), max_th = max_th[1]) %>% dplyr::filter(dt > 20 & dt < 28)
nirops_summary

nirops_summary <- nirops_summary %>% rename(dt_nirops = dt, max_speed_nirops = max_speed, max_th_nirops = max_th)

#summary_df <- viirs_summary %>% inner_join(modis_summary, by = c("New_ID", "day")) %>% inner_join(nirops_summary, by = c("New_ID", "day"))

summary_df <- modis_summary %>% inner_join(nirops_summary, by = c("New_ID", "day"))
str(summary_df)

N_examples <- unique(summary_df$New_ID) %>% length()
N_examples

#m <- lm(max_speed_viirs ~ max_speed_nirops, data = summary_df)
m <- lm(max_speed_modis ~ max_speed_nirops, data = summary_df)
summary(m)

R2 <- summary(m)$r.squared

ggplot() + geom_point(data = summary_df, mapping = aes(x = max_speed_nirops, y = max_speed_modis), alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = 'red') + 
  xlim(c(-0.01, 1.2)) +
  ylim(c(-0.01, 1.2)) +
  xlab("NIROPS Daily Max. Speed (km h\u207B\u00B9)") +
  ylab("FireVectors (MODIS) Daily Max. Speed (km h\u207B\u00B9)") +
  annotate("text", x = 0, y = 0.2, 
           label = sprintf("R^2 == %.2f", R2),
           parse = TRUE,
           hjust = 0, size = 5) +
  coord_equal()

ggsave("modis_nirops_comparison.png", dpi = 600, width = 101, height = 89, units = "mm")


