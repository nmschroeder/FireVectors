library(dplyr)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggpattern)
library(scales)
library(ggspatial)
library(cowplot)
library(lubridate)
library(tidyverse)

# Read in the L3 Ecoregion shapefilex

eco <- st_read("ecoregions/us_eco_l3.shp") %>% st_transform(crs = 5070)

# Read in the Western United States shapefile

wus <- st_read("TIGER/western_us.shp")

# Read in MTBS data
mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp")

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



# Read in the modis and viirs perimeters

viirs_perims <- st_read("data/perim_polygons_viirs.geojson") %>% dplyr::filter(perim_avail == 1)
str(viirs_perims)

modis_perims <- st_read("data/perim_polygons_modis.geojson") %>% dplyr::filter(perim_avail == 1)
str(modis_perims)

# Read in the modis and viirs burn direction with fire growth rate added (for speed estimates)

viirs <- read.csv("data/burn_direction_viirs.csv") %>% dplyr::filter(burn_avail == TRUE)

viirs$time_start <- as.POSIXct(viirs$time_end, tz = "UTC") - dhours(viirs$dt)

modis <- read.csv("data/burn_direction_modis.csv") %>% dplyr::filter(burn_avail == TRUE)
modis$time_start <- as.POSIXct(modis$time_end, tz = "UTC") - dhours(modis$dt)
modis_fgr_max <- modis %>% group_by(New_ID) %>% summarize(max_dAdt = max(dAdt, na.rm = TRUE))

# Use the starting vector point to intersect the ecoregions for each fire

# Steps:

# temp_df <- dplyr::filter(modis, Incid_Name == "EAST TROUBLESOME")
# 

# Group by New_ID
# Create function that takes a burn direction data frame for a specific event 
# and arranges by summary_date
# Collect the max FGR 
# Collect the x1 and y1 for the max FGR
# Assign to ecoregion in data frame with one row (max values and ecoregion appended)

id_ecoregion <- function(temp_df){
  temp_df <- arrange(temp_df, time_start)
  idx <- which.max(temp_df$v_xy)
  temp_max <- temp_df[idx,]
  pt <- st_point(x = c(temp_max$x1, temp_max$y1), dim = "XY") %>% st_sfc(crs = 5070)
  idx_ecoregion <- st_intersects(eco, pt, sparse = FALSE)
  eco_name <- eco[idx_ecoregion,]$US_L3NAME
  temp_max <- temp_max %>% mutate(ecoregion = eco_name)
  return(temp_max)
}

# Apply function to each event to match the event to an ecoregion 
modis_list <- modis %>% group_by(New_ID) %>% group_map(~id_ecoregion(.x), .keep = TRUE)

# Row bind the resulting list together into a single data frame
modis_df <- do.call(rbind.data.frame, modis_list)

# Use lubridate to generate a decimal version of the date
modis_df$dec_date <- decimal_date(modis_df$time_start)

modis_df$year <- year(modis_df$time_mean)

# Check the result
str(modis_df)

# Join the fire growth rate (km2/hr) by New_ID
modis_df <- modis_df %>% right_join(modis_fgr_max, by = "New_ID")

# Create a field with a shorter name for the following category so that it fits
# in the figure title
idx <- which(modis_df$ecoregion == "California Coastal Sage, Chaparral, and Oak Woodlands")

modis_df$ecorenamed <- modis_df$ecoregion
modis_df$ecorenamed[idx] <- "CA Coastal Sage, Chaparral, and Oak Woodlands"

# ggplot() + geom_point(modis_df, mapping = aes(x = dec_date, y = v_xy)) + facet_wrap(~ecorenamed, ncol = 3) + xlab("Date") + ylab("Maximum speed for each fire (km h\u207B\u00B9)")
# ggsave("trends_by_ecoregion.png", width = 8, height = 10, units = "in")
# 
# write.csv(modis_df, "modis_speed_by_ecoregion.csv")
# Investigate trends for each ecoregion in a table

# Coefficients, standard error, and probabilities

#unique_ecoregions <- unique(modis_df$ecoregion)

# Determine which ecoregions intersect the Western United States
idx <- st_intersects(eco, wus, sparse = FALSE)

# Select the ecoregions within the Western United States
wus_eco <- eco[idx,]

# Collect a version of these ecoregion perimeters that are clipped to the 
# Western United States
wus_eco2 <- st_intersection(eco, wus, sparse = FALSE)

# Plot these ecoregions to test the result
#ggplot() + geom_sf(data = wus_eco2, fill = NA)
#ggsave('test.png')

unique_ecoregions <- unique(wus_eco2$US_L3NAME)

modis_ecoregion_list <- list()

for (i in 1:length(unique_ecoregions)){
  # Create a temporary data frame for the ith ecoregion
  temp_df <- dplyr::filter(modis_df, ecoregion == unique_ecoregions[i])
  
  # Determine how many fire events occurred within this ecoregion
  count <- sum(!is.na(temp_df$v_xy))
  
  # If there are at least 20 fire events in the ecoregion...
  if (count>=20){
    # Generate a linear model relating the fire speed to the date
    m <- lm(v_xy ~ year, data = temp_df)
    a1 <- coef(m)[2]
    if (!is.na(a1)){
      pval <- coef(summary(m))["year", "Pr(>|t|)"]
    } else{
      pval <- NA
    }
    
    # Generate a linear model relating the fire growth rate to the date
    m_fgr <- lm(max_dAdt ~ year, data = temp_df)
    a1_fgr <- coef(m_fgr)[2]
    if (!is.na(a1)){
      pval_fgr <- coef(summary(m_fgr))["year", "Pr(>|t|)"]
    } else{
      pval_fgr <- NA
    }
    
    # Store the parameters and attributes of this model in a data frame row corresponding to this ecoregion
    new_df <- data.frame(ecoregion = unique_ecoregions[i], N = count, slope = a1, p = pval, mean_speed = mean(temp_df$v_xy, na.rm = TRUE), max_speed = max(temp_df$v_xy, na.rm = TRUE), q95 = quantile(temp_df$v_xy, 0.95, na.rm = TRUE), q50 = quantile(temp_df$v_xy, 0.50, na.rm = TRUE), q05 = quantile(temp_df$v_xy, 0.05, na.rm = TRUE),
                         slope_fgr = a1_fgr, p_fgr = pval_fgr, mean_fgr = mean(temp_df$max_dAdt, na.rm = TRUE), max_fgr = max(temp_df$max_dAdt, na.rm = TRUE), q95_fgr = quantile(temp_df$max_dAdt, 0.95, na.rm = TRUE), q50_fgr = quantile(temp_df$max_dAdt, 0.50, na.rm = TRUE), q05_fgr = quantile(temp_df$max_dAdt, 0.05, na.rm = TRUE))
  } else{
    # Otherwise (if we have fewer than 20 examples), then do not list a slope, p-value, nor other attributes for the ecoregion
    new_df <- data.frame(ecoregion = unique_ecoregions[i], N = count, slope = NA, p = NA, mean_speed = NA, max_speed = NA, q95 = NA, q50 = NA, q05 = NA, 
                         slope_fgr = NA, p_fgr = NA, mean_fgr = NA, max_fgr = NA, q95_fgr = NA, q50_fgr = NA, q05_fgr = NA)
  }
  # Store the row for the ith ecoregion in a list
  modis_ecoregion_list[[i]] <- new_df
}

# Collect the results for each ecoregion into a single data frame
modis_ecoregion_summary <- do.call(rbind.data.frame, modis_ecoregion_list)        

# Take a look
modis_ecoregion_summary

# Use the slope to identify the mean 20-year change in fire speed
modis_ecoregion_summary$mean_20yr <- modis_ecoregion_summary$slope*20

# Use the slope to identify the mean 20-year change in fire growth rate
modis_ecoregion_summary$mean_20yr_fgr <- modis_ecoregion_summary$slope_fgr*20

# Determine the fraction of this change relative to the mean speed in that ecoregion
modis_ecoregion_summary$frac_change_20yr <- modis_ecoregion_summary$mean_20yr/modis_ecoregion_summary$mean_speed

# View speed results that are significant to a 95% confidence level
dplyr::filter(modis_ecoregion_summary, p <= 0.05) %>% arrange(desc(slope))

# View FGR results that are significant to a 95% confidence level
dplyr::filter(modis_ecoregion_summary, p_fgr <= 0.05) %>% arrange(desc(slope_fgr))

# Create a field for the L3 ecoregion that matches the ecoregion data frame
modis_ecoregion_summary$US_L3NAME <- modis_ecoregion_summary$ecoregion

# Join the clipped ecoregions to the fire speed model summary for each ecoregion
wus_ecoregion_summary <- right_join(wus_eco2, modis_ecoregion_summary, by = "US_L3NAME")

# Determine which ecoregions do not have a mean speed value (because they did not have
# at least 20 fire event examples in the ecoregion)
ecoregions_na <- wus_ecoregion_summary %>% dplyr::filter(is.na(mean_speed)) 

#ggplot() + geom_sf(data = wus_eco2, fill = 'blue') + geom_sf(data = ecoregions_na, fill = 'red')

p1 <- ggplot() + #geom_sf(data = wus_eco2, fill = 'grey50') +
  geom_sf(data = wus_ecoregion_summary, 
          mapping = aes(fill = mean_speed)) + 
  geom_sf_pattern(data = ecoregions_na, fill = 'gray90', pattern = "stripe", pattern_density = 0.50, pattern_fill = "gray90", pattern_color = "gray50") +
  #geom_sf(data = wus, color = 'black', lwd = 1.2, fill = 'NA') +
  scale_fill_viridis_c(option = "A", direction = 1, name = "Mean Speed\n(km h\u207B\u00B9)", na.value = "gray90") +
  
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        pad_x = unit(0.15, "in"),
  #                        pad_y = unit(0.20, "in"),
  #                        style = north_arrow_fancy_orienteering,
  #                        height = unit(0.8, "cm"),   # adjust arrow size
  #                        width  = unit(0.8, "cm")) +
  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10, hjust = 0),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    fill = guide_colorbar(
      title.hjust = 0,
      title.vjust = 1,
      barwidth = unit(1.8, "in"),  # make the bar wider
      barheight = unit(0.20, "in") # control thickness
    )
  )

#p1

c_lim <- max(abs(wus_ecoregion_summary$mean_20yr), na.rm = TRUE)

p2 <- ggplot() + geom_sf(data = wus_eco2, fill = 'grey50') +
  geom_sf_pattern(data = ecoregions_na, fill = 'gray90', pattern = "stripe", pattern_density = 0.50, pattern_fill = "gray90", pattern_color = "gray50") +
  geom_sf(data = dplyr::filter(wus_ecoregion_summary, p <= 0.05), 
          mapping = aes(fill = mean_20yr)) + 
  #geom_sf(data = wus, color = 'black', lwd = 1.2, fill = 'NA') +
  scale_fill_distiller(palette = "RdBu", limits = c(-1*c_lim, c_lim), name = "Approx. 20-year \nSpeed Change (km h\u207B\u00B9)") +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10, hjust = 0),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    fill = guide_colorbar(
      title.hjust = 0,
      title.vjust = 1,
      barwidth = unit(1.8, "in"),  # make the bar wider
      barheight = unit(0.20, "in") # control thickness
    )
  )

# Determine which ecoregions do not have a mean speed value (because they did not have
# at least 20 fire event examples in the ecoregion)
ecoregions_na_fgr <- wus_ecoregion_summary %>% dplyr::filter(is.na(mean_fgr)) 

#ggplot() + geom_sf(data = wus_eco2, fill = 'blue') + geom_sf(data = ecoregions_na, fill = 'red')

p3 <- ggplot() + #geom_sf(data = wus_eco2, fill = 'grey50') +
  geom_sf(data = wus_ecoregion_summary, 
          mapping = aes(fill = mean_fgr)) + 
  geom_sf_pattern(data = ecoregions_na_fgr, fill = 'gray90', pattern = "stripe", pattern_density = 0.50, pattern_fill = "gray90", pattern_color = "gray50") +
  #geom_sf(data = wus, color = 'black', lwd = 1.2, fill = 'NA') +
  scale_fill_viridis_c(option = "A", direction = 1, "Mean Max. Fire \n Growth Rate (km\u00B2 h\u207B\u00B9)", na.value = "gray90") +
  
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +

  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10, hjust = 0),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.title.align = 0
  ) +
  guides(
    fill = guide_colorbar(
      title.hjust = 0,
      title.vjust = 1,
      barwidth = unit(1.8, "in"),  # make the bar wider
      barheight = unit(0.20, "in") # control thickness
    )
  )

p3

c_lim_fgr <- max(abs(wus_ecoregion_summary$mean_20yr_fgr), na.rm = TRUE)

p4 <- ggplot() + geom_sf(data = wus_eco2, fill = 'grey50') +
  geom_sf_pattern(data = ecoregions_na_fgr, fill = 'gray90', pattern = "stripe", pattern_density = 0.50, pattern_fill = "gray90", pattern_color = "gray50") +
  geom_sf(data = dplyr::filter(wus_ecoregion_summary, p_fgr <= 0.05), 
          mapping = aes(fill = mean_20yr_fgr)) + 
  #geom_sf(data = wus, color = 'black', lwd = 1.2, fill = 'NA') +
  scale_fill_distiller(palette = "RdBu", limits = c(-1*c_lim_fgr, c_lim_fgr), name = "Approx. 20-year\nFire Growth Rate\nChange (km\u00B2 h\u207B\u00B9)") +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10, hjust = 0),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.title.align = 0
  ) +
  guides(
    fill = guide_colorbar(
      title.hjust = 0,
      title.vjust = 1,
      barwidth = unit(1.8, "in"),  # make the bar wider
      barheight = unit(0.20, "in") # control thickness
    )
  )

#maps <- plot_grid(p1, p2, p3, p4, nrow = 2, labels = c("a.", "b.", "c.", "d."), label_fontface = "plain")

#ggsave("ecoregion_maps.png", plot = maps, width = 183, height = 240, units = "mm")

# Okay, let's make an alternative figure display where we have the fire speed
# trends separate from the fire growth rate trends

maps_speed <- plot_grid(p1, p2, nrow = 1, labels = c("a.", "b."), label_fontface = "plain")
ggsave("ecoregion_maps_speed.png", plot = maps_speed, width = 183, height = 120, units = "mm")

maps_fgr <- plot_grid(p3, p4, nrow = 1, labels = c("a.", "b."), label_fontface = "plain")
ggsave("ecoregion_maps_fgr.png", plot = maps_fgr, width = 183, height = 120, units = "mm")


# Use contained data frame instead
fastest_speed <- modis_df %>% dplyr::arrange(desc(v_xy))
fastest_speed <- fastest_speed[1:10,]
mtbs_fastest <- dplyr::filter(mtbs, New_ID %in% fastest_speed$New_ID)
mtbs_fastest <- right_join(mtbs_fastest, dplyr::select(fastest_speed, v_xy, New_ID), by = "New_ID")

modis_perims_fastest_speed <- modis_perims %>% dplyr::filter(New_ID %in% fastest_speed$New_ID)
modis_vectors <- modis %>% dplyr::filter(New_ID %in% fastest_speed$New_ID)

#write.csv(modis_vectors, "modis_vectors_fastest_speed.csv")

# Find XXth percentile 
v_xy_thresh <- quantile(modis_df$v_xy, 0.95)
v_xy_thresh

id_thresh <- modis_df$New_ID[modis_df$v_xy >= v_xy_thresh]
mtbs_thresh <- dplyr::filter(mtbs, New_ID %in% id_thresh)


# Which ecoregions have the fastest fire speeds? Fire growth rates?
speed_summary <- wus_ecoregion_summary %>% group_by(ecoregion) %>%
  summarize(N = N[1], max_speed = max_speed[1], max_fgr = max_fgr[1]) %>% drop_na()

speed_summary %>% arrange(desc(max_speed))

speed_summary %>% arrange(desc(max_fgr))

c_max <- max(fastest_speed$v_xy)

p_max <- ggplot() + #geom_sf(data = wus_eco2, fill = 'grey50') +
  geom_sf(data = wus_ecoregion_summary, 
          mapping = aes(fill = max_speed)) + 
  geom_sf_pattern(data = ecoregions_na, fill = 'gray90', pattern = "stripe", pattern_density = 0.50, pattern_fill = "gray90", pattern_color = "gray50") +
  scale_fill_viridis_c(option = "F", direction = 1, limits = c(0, c_max), end = 0.7, name = "Maximum Fire\nSpeed\n(km h\u207B\u00B9)", na.value = "gray90", 
                       guide = guide_colorbar(barwidth  = unit(60, "mm"))
  ) +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) 

p_max



p_fastest <- ggplot() + geom_sf(data = wus_eco2, fill = 'white', color = 'gray10') +
  geom_sf(data = mtbs_thresh, fill = 'gray30', color = 'gray30', alpha = 0.40, linewidth = 0.40)+
  geom_sf(data = mtbs_fastest, mapping = aes(color = v_xy, 
                                             fill = v_xy),
          alpha = 0.80,
          linewidth = 0.80) +
  #geom_sf(data = wus, color = 'black', fill = 'NA', lwd = 0.8) +
  scale_color_viridis_c(name = "Max. Speed (km h\u207B\u00B9)", option = "F", direction = 1, limits = c(0, c_max), end = 0.7) +
  scale_fill_viridis_c(name = "Max. Speed (km h\u207B\u00B9)", option = "F", direction = 1, limits = c(0, c_max), end = 0.7) +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +

  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_fastest



## Adding fastest FGR here

fastest_fgr <- modis_df %>% dplyr::arrange(desc(max_dAdt))
fastest_fgr <- fastest_fgr[1:10,]

# Find XXth percentile 
dAdt_thresh <- quantile(modis_df$max_dAdt, 0.95)
dAdt_thresh

id_thresh_dAdt <- modis_df$New_ID[modis_df$max_dAdt >= dAdt_thresh]
mtbs_thresh_dAdt <- dplyr::filter(mtbs, New_ID %in% id_thresh_dAdt)

c_max_fgr <- max(fastest_fgr$max_dAdt)

p_max_fgr <- ggplot() + #geom_sf(data = wus_eco2, fill = 'grey50') +
  geom_sf(data = wus_ecoregion_summary, 
          mapping = aes(fill = max_fgr)) + 
  geom_sf_pattern(data = ecoregions_na_fgr, fill = 'gray90', pattern = "stripe", pattern_density = 0.50, pattern_fill = "gray90", pattern_color = "gray50") +
  scale_fill_viridis_c(option = "F", direction = 1, limits = c(0, c_max_fgr), end = 0.7, name = "Maximum Fire\nGrowth Rate\n(km\u00B2 h\u207B\u00B9)", na.value = "gray90", 
                       guide = guide_colorbar(barwidth  = unit(60, "mm"))
  ) +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) 

p_max_fgr

mtbs_fastest_fgr <- dplyr::filter(mtbs, New_ID %in% fastest_fgr$New_ID)
mtbs_fastest_fgr <- right_join(mtbs_fastest_fgr, dplyr::select(fastest_fgr, max_dAdt, New_ID), by = "New_ID")


p_fastest_fgr <- ggplot() + geom_sf(data = wus_eco2, fill = 'white', color = 'gray10') +
  geom_sf(data = mtbs_thresh_dAdt, fill = 'gray30', color = 'gray30', alpha = 0.40, linewidth = 0.40)+
  geom_sf(data = mtbs_fastest_fgr, mapping = aes(color = max_dAdt, 
                                             fill = max_dAdt),
          alpha = 0.80,
          linewidth = 0.80) +
  #geom_sf(data = wus, color = 'black', fill = 'NA', lwd = 0.8) +
  scale_color_viridis_c(name = "Max. Speed (km\u00B2 h\u207B\u00B9)", option = "F", direction = 1, limits = c(0, c_max_fgr), end = 0.7) +
  scale_fill_viridis_c(name = "Max. Speed (km\u00B2 h\u207B\u00B9)", option = "F", direction = 1, limits = c(0, c_max_fgr), end = 0.7) +
  annotation_scale(location = "bl", width_hint = 0.2, text_cex = 0.8) +
  
  theme(
    legend.position = "bottom",              # move legend to bottom
    legend.direction = "horizontal",         # arrange items horizontally
    legend.title = element_text(size = 10),  # match base font
    legend.text  = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

p_fastest_fgr



bb <- sf::st_bbox(wus)

p_max2 <- p_max +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE)

p_fastest2 <- p_fastest +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE)

p_max_fgr2 <- p_max_fgr +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE)

p_fastest_fgr2 <- p_fastest_fgr +
  coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
           ylim = c(bb["ymin"], bb["ymax"]),
           expand = FALSE)


leg_max   <- cowplot::get_legend(p_max2 + theme(legend.position = "right"))
leg_fast  <- cowplot::get_legend(p_fastest2 + theme(legend.position = "right"))
leg_max_fgr  <- cowplot::get_legend(p_max_fgr2 + theme(legend.position = "right"))
leg_fast_fgr  <- cowplot::get_legend(p_fastest_fgr2 + theme(legend.position = "right"))

p_max_noleg  <- p_max2 + theme(legend.position = "none")
p_fast_noleg <- p_fastest2 + theme(legend.position = "none")
p_max_fgr_noleg <- p_max_fgr2 + theme(legend.position = "none")
p_fast_fgr_noleg <- p_fastest_fgr2 + theme(legend.position = "none")

top_row <- plot_grid(
  p_max_noleg, p_fast_noleg,
  nrow = 1,
  labels = c("a.", "b."),
  label_fontface = "plain",
  align = "hv", axis = "tblr"
)

top_leg <- plot_grid(
  leg_max, 
  nrow = 1,
  align = "hv", axis = "tblr"
)

bottom_row <- plot_grid(
  p_max_fgr_noleg, p_fast_fgr_noleg,
  nrow = 1,
  labels = c("c.", "d."),
  label_fontface = "plain",
  align = "hv", axis = "tblr"
)

bottom_leg <- plot_grid(
  leg_max_fgr,
  nrow = 1,
  align = "hv", axis = "tblr"
)


# maps_fastfires <- plot_grid(
#   top_row, top_leg, bottom_row, bottom_leg, 
#   ncol = 1,
#   rel_heights = c(1, 0.15, 1, 0.15)  
# )
# 
# maps_fastfires
# 
# ggsave("ecoregion_maps_fastfires.png", plot = maps_fastfires, width = 183, height = 240, units = "mm", dpi = 600)


# Alternatively, split the fire speed from FGR

maps_fastfires_speed <- plot_grid(
  top_row, top_leg,
  ncol = 1,
  rel_heights = c(1, 0.15)  
)

maps_fastfires_speed

ggsave("ecoregion_maps_fastfires_speed.png", plot = maps_fastfires_speed, width = 183, height = 120, units = "mm", dpi = 600)


# Alternatively, split the fire speed from FGR

bottom_row <- plot_grid(
  p_max_fgr_noleg, p_fast_fgr_noleg,
  nrow = 1,
  labels = c("a.", "b."),
  label_fontface = "plain",
  align = "hv", axis = "tblr"
)

maps_fastfires_fgr <- plot_grid(
  bottom_row, bottom_leg,
  ncol = 1,
  rel_heights = c(1, 0.15)  
)

maps_fastfires_fgr

ggsave("ecoregion_maps_fastfires_fgr.png", plot = maps_fastfires_fgr, width = 183, height = 120, units = "mm", dpi = 600)
