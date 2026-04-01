library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
library(tidyr)
library(car)
library(cowplot)
library(ggh4x)

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


# Read in VIIRS- and MODIS-derived burn perimeters

viirs_perims <- st_read("data/perim_polygons_viirs.geojson") %>% dplyr::filter(perim_avail == 1)
str(viirs_perims)

modis_perims <- st_read("data/perim_polygons_modis.geojson") %>% dplyr::filter(perim_avail == 1)
str(modis_perims)

# And speed vectors

viirs <- read.csv("data/burn_direction_viirs.csv") %>% dplyr::filter(burn_avail == TRUE)
id_viirs <- viirs$New_ID
modis <- read.csv("data/burn_direction_modis.csv") %>% dplyr::filter(burn_avail == TRUE)
id_modis <- modis$New_ID


test_dt <- function(temp_df){
  N <- nrow(temp_df)
  temp_df$dec_date <- decimal_date(temp_df$summary_date) %>% as.numeric()
  temp_df <- temp_df %>% arrange(dec_date)
  dt <- difftime(temp_df[2:N,]$summary_date, temp_df[1:(N-1),]$summary_date, units = "hours")
  new_df <- data.frame(Incid_Name = temp_df$Incid_Name[1], INSTRUMENT = temp_df$INSTRUMENT[1], New_ID = temp_df$New_ID[1], SATELLITE = temp_df$SATELLITE[1], summary_date = temp_df$summary_date[1], min_dt = min(dt))
  return(new_df)
}

dt_check <- viirs_perims %>% st_drop_geometry() %>% 
  group_by(SATELLITE, INSTRUMENT, New_ID) %>% 
  dplyr::group_map(~test_dt(.x), .keep = TRUE)

dt_values <- do.call(rbind.data.frame, dt_check)

str(dt_values)

# Check for values within 6 minutes of each other
idx <- dt_values$min_dt<(1/10)
idx_values <- which(idx)

dt_values[idx_values,]

# Checking for the least increment between successive returns

dt_check_modis <- modis_perims %>% st_drop_geometry() %>% 
  group_by(SATELLITE, INSTRUMENT, New_ID) %>%
  dplyr::group_map(~test_dt(.x), .keep = TRUE)

dt_values_modis <- do.call(rbind.data.frame, dt_check_modis)

idx <- which(dt_values_modis$min_dt<(1/10))

dt_values_modis_check <- dt_values_modis[idx,]

dt_values_modis_check

check <- dt_values$Incid_Name %in% dt_values_modis$Incid_Name
sum(check)

# Compute the area
viirs_area_m2 <- st_area(viirs_perims) %>% as.numeric()
modis_area_m2 <- st_area(modis_perims) %>% as.numeric()

viirs_perims <- mutate(viirs_perims, area_m2 = viirs_area_m2)
modis_perims <- mutate(modis_perims, area_m2 = modis_area_m2)

# Next, we want a function that tracks the total change in area each day 

test <- dplyr::filter(modis_perims, Incid_Name == "EAST TROUBLESOME")

ggplot() + geom_point(data = test, 
                      mapping = aes(x = summary_date, y = area_m2))

# So, we want to track how much change occurred between each interval.
# Let the first one be NA

burn_area_per_time <- function(fire_df){
  # How many entries do we have for this fire?
  N <- nrow(fire_df)
  
  # If there is more than one entry:
  if (N > 1){
    # Arrange in ascending order by time
    fire_df <- arrange(fire_df, summary_date)
    
    # Compute the change in area
    dA <- fire_df$area_m2[2:N] - fire_df$area_m2[1:(N-1)]
    
    # Compute the change in start time
    dt <- difftime(fire_df$summary_date[2:N], fire_df$summary_date[1:(N-1)], units = "hours") %>% as.numeric()
    
    # Add the change in area, the change in time, and the adjusted change in area per 12 hours
    fire_df <- mutate(fire_df, dA = c(NA, dA), dt = c(NA, dt))
  } else{
    # If there is only one entry, fill these new attributes with NA
    fire_df <- mutate(fire_df, dA = NA, dt = NA)
  }
  return(fire_df)
}

test <- test %>% dplyr::filter(SATELLITE == "Aqua") %>% burn_area_per_time()

# Note the red is the growth since the previous reading which can be variable 
# time intervals ranging from 9 hours to 24 hours; and purple is the adjusted 
# growth per 12 hours
ggplot(data = test) + geom_point(mapping = aes(x = summary_date, y = area_m2)) +
  geom_point(mapping = aes(summary_date, y = dA), color = 'red') +
  xlim(as.POSIXct("2020-10-14 00:00:00", tz = "UTC"), as.POSIXct("2020-10-27 00:00:00", tz = "UTC"))
  xlab("Date")

# Does the maximum period of growth align with newspaper records?
check <- which.max(test$dA)

# So, I see a period of maximum fire growth rate between 12:00 UTC and 23:00 UTC
# on 2020-10-21. This corresponds to 6:00AM MT and 5:00PM MT on 2020-10-21.
# While it tapers after this, there is one more period of large growth 
# (second largest amount) right after. This seems to check out.
test[(check-1),]
test[check,]

# Check VIIRS as well
test2 <- dplyr::filter(viirs_perims, Incid_Name == "EAST TROUBLESOME")

ggplot() + geom_point(data = test2, 
                      mapping = aes(x = summary_date, y = area_m2))

# So, we want to track how much change occurred between each interval.
# Let the first one be NA

test2 <- test2 %>% dplyr::filter(SATELLITE == "N") %>% burn_area_per_time()

# Note the red is the growth since the previous reading which can be variable 
# time intervals ranging from 9 hours to 24 hours
ggplot(data = test2) + geom_point(mapping = aes(x = summary_date, y = area_m2)) +
  geom_point(mapping = aes(summary_date, y = dA), color = 'red') +
  xlab("Date")

# So, I see a period of maximum fire growth rate between 12:00 UTC and 23:00 UTC
# on 2020-10-21. This corresponds to 6:00AM MT and 5:00PM MT on 2020-10-21.
# While it tapers after this, there is one more period of large growth 
# (second largest amount) right after. This seems to check out.
test2[(check-1),]
test2[check,]

# So, now we want to group by Incid_Name and apply this to each fire
fire_groups_modis <- modis_perims %>% dplyr::filter(SATELLITE == "Aqua") %>% dplyr::filter(New_ID %in% id_modis) %>% group_by(New_ID) %>% group_map(~burn_area_per_time(.x), .keep = TRUE)
fire_modis_df <- do.call(rbind.data.frame, fire_groups_modis)
#unlink("data/modis_fire_speed_and_growth_rate.geojson")
#st_write(fire_modis_df, "data/modis_fire_speed_and_areal_growth.geojson")

fire_groups_viirs <- viirs_perims %>% dplyr::filter(SATELLITE == "N") %>% dplyr::filter(New_ID %in% id_viirs) %>% group_by(New_ID) %>% group_map(~burn_area_per_time(.x), .keep = TRUE)
fire_viirs_df <- do.call(rbind.data.frame, fire_groups_viirs)
#unlink("data/viirs_fire_speed_and_areal_growth.geojson")
#st_write(fire_viirs_df, "data/viirs_fire_speed_and_areal_growth.geojson")

# Compare final burn area to each other (both are constrained by MTBS)
modis_final <- fire_modis_df %>% group_by(New_ID) %>% summarize(modis_area_m2 = max(area_m2, na.rm = TRUE), modis_max_dA = max(dA, na.rm = TRUE), modis_summary_date = summary_date[1])
viirs_final <- fire_viirs_df %>% group_by(New_ID) %>% summarize(viirs_area_m2 = max(area_m2, na.rm = TRUE), viirs_max_dA = max(dA, na.rm = TRUE), viirs_summary_date = summary_date[1])

# To pull the maximum speed, we need the burn direction data frames
modis_speed <- modis %>% dplyr::filter(New_ID %in% modis_final$New_ID) %>%
  group_by(New_ID) %>% 
  summarize(modis_max_v_elev = max(v_elev), modis_max_v_xy = max(v_xy),
            N_modis = n(), modis_summary_date = time_mean[1])

viirs_speed <- viirs %>% dplyr::filter(New_ID %in% viirs_final$New_ID) %>%
  group_by(New_ID) %>% 
  summarize(viirs_max_v_elev = max(v_elev), viirs_max_v_xy = max(v_xy),
            N_viirs = n(), viirs_summary_date = time_mean[1])

summary_df <- right_join(st_drop_geometry(modis_final), 
                         st_drop_geometry(viirs_final), by = "New_ID") %>%
  right_join(st_drop_geometry(modis_speed), by = "New_ID") %>%
  right_join(st_drop_geometry(viirs_speed), by = "New_ID")

str(summary_df)

# Look for trends in linear speed and areal spread rate here
modis_final$modis_year <- year(modis_final$modis_summary_date)
modis_speed$modis_year <- year(modis_speed$modis_summary_date)
viirs_final$viirs_year <- year(viirs_final$viirs_summary_date)
viirs_speed$viirs_year <- year(viirs_speed$viirs_summary_date)

fgr_trend_modis <- lm(modis_max_dA ~ modis_year, data = modis_final)
summary(fgr_trend_modis)

# This is a rate per year, and it is very, very small but significant
a0 <- coef(fgr_trend_modis)[1]
a1 <- coef(fgr_trend_modis)[2]

a1_20yrs <- a1*20
a1_20yrs

lin_trend_modis <- lm(modis_max_v_xy ~ modis_year, data = modis_speed)
summary(lin_trend_modis)

ggplot() + geom_point(data = modis_final, mapping = aes(x = modis_summary_date, y = modis_max_dA))

fgr_trend_viirs <- lm(viirs_max_dA ~ viirs_year, data = viirs_final)
summary(fgr_trend_viirs)

summary_df$viirs_year <- lubridate::year(summary_df$viirs_summary_date.x)

lin_trend_viirs <- lm(viirs_max_v_xy ~ viirs_year, data = summary_df)
summary(lin_trend_viirs)

str(summary_df)

ggplot() + geom_point(data = summary_df, mapping = aes(x = viirs_summary_date.x, y = viirs_max_dA))

max_area_m2 <- max(summary_df$modis_area_m2, na.rm = TRUE)

area_compare <- lm(viirs_area_m2 ~ modis_area_m2 + 0, data = summary_df)
summary(area_compare)
R2 <- summary(area_compare)$r.squared
linearHypothesis(area_compare, "modis_area_m2 = 1")

p_compare_final_area <- ggplot() + geom_point(data = summary_df, mapping = aes(x = modis_area_m2*1e-6, y = viirs_area_m2*1e-6), alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  xlim(c(0, 6200)) +
  ylim(c(0, 6200)) +
  xlab(expression("MODIS Final Area (km"^2*")")) +
  ylab(expression("VIIRS Final Area (km"^2*")")) +
  annotate("text", x = 0, y = 6076, 
           label = sprintf("R^2 == %.2f", R2),
           parse = TRUE,
           hjust = 0, size = 5) +
  coord_equal() +
  theme(aspect.ratio = 1) +
  ggh4x::force_panelsizes(rows = unit(2.2, "in"), cols = unit(2.5, "in"))

#p_compare_final_area

#ggsave("modis_viirs_final_area_comparison.png", plot = p_compare_final_area, dpi = 600, width = 3.5, height = 3.5)

# Additional 1:1 plots: maximum speed (kph) from both; maximum rate of spread (m2 per 12 hours)

# Need to add these variables to summary_df or another data frame

lm_v_elev <- lm(modis_max_v_elev ~ viirs_max_v_elev + 0, data = summary_df)
summary(lm_v_elev)

R2_2 <- summary(lm_v_elev)$r.squared

linearHypothesis(lm_v_elev, "viirs_max_v_elev = 1")

lm_v_xy <- lm(modis_max_v_xy ~ viirs_max_v_xy + 0, data = summary_df)
summary(lm_v_xy)

linearHypothesis(lm_v_xy, "viirs_max_v_xy = 1")

p_compare_max_speed <- ggplot(data = summary_df) + geom_point(mapping = aes(x = modis_max_v_xy, y = viirs_max_v_xy), alpha = 0.5) + geom_abline(slope = 1, intercept = 0, color = 'red') +
  xlim(c(0, 3.5)) +
  ylim(c(0, 3.5)) +
  xlab(expression("MODIS Max Speed (kph)")) +
  ylab(expression("VIIRS Max Speed (kph)")) +
  annotate("text", x = 0, y = 3.43, 
           label = sprintf("R^2 == %.2f", R2_2),
           parse = TRUE,
           hjust = 0, size = 5) +
  coord_equal() +
  theme(aspect.ratio = 1) +
  ggh4x::force_panelsizes(rows = unit(2.2, "in"), cols = unit(2.5, "in"))
#p_compare_max_speed  
#ggsave("viirs_modis_max_speed_comparison.png", dpi = 600, height = 3, width = 3, units = "in")


lm_dA <- lm(modis_max_dA ~ viirs_max_dA + 0, data = summary_df)
summary(lm_dA)
R2_dA <- summary(lm_dA)$r.squared

p_compare_max_fgr <- ggplot(data = summary_df) + 
  geom_point(mapping = aes(x = modis_max_dA*1e-6, 
                           y = viirs_max_dA*1e-6), 
             alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  xlim(0, 50) +
  ylim(0, 50) + 
  labs(x = expression("MODIS Max FGR (" * km^2 * "/hr)"),
       y = expression("VIIRS Max FGR (" * km^2 * "/hr)")) +
  annotate("text", x = 0, y = 49, 
           label = sprintf("R^2 == %.2f", R2_dA),
           parse = TRUE,
           hjust = 0, size = 5) +
  coord_equal() +
  theme(aspect.ratio = 1) +
  ggh4x::force_panelsizes(rows = unit(2.2, "in"), cols = unit(2.5, "in"))
#p_compare_max_fgr

p_compare <- plot_grid(p_compare_final_area, p_compare_max_speed, p_compare_max_fgr, nrow = 1, align = "hv", labels = c("a.", "b.", "c."), label_fontface = "plain")
ggsave("modis_vs_viirs_comparison_panel.png", plot = p_compare, width = 9.5, height = 3, units = "in", bg = "white")


