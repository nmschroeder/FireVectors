library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)
library(tidyr)
library(ggh4x)
library(scales)

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


viirs <- read.csv("data/burn_direction_viirs.csv")

str(viirs)

New_ID <- unique(viirs$New_ID)

idx <- which(viirs$Incid_Name == "EAST TROUBLESOME")

test <- viirs[idx,]

# Plot burn direction
#ggplot() + geom_sf(data = test)

viirs$v_elev <- as.numeric(viirs$v_elev)

viirs$time_start <- as.POSIXct(viirs$time_end, tz = "UTC") - as.difftime(viirs$dt, units = "hours")

str(viirs)

fire_speed_viirs <- viirs %>% st_drop_geometry() %>% group_by(New_ID) %>% summarize(v_max = max(v_elev), date = time_start[1])

yr <- year(fire_speed_viirs$date)
fire_speed_by_year_viirs <- mutate(fire_speed_viirs, year = as.factor(yr), dec_date = decimal_date(as.Date(date))) 

ggplot() + geom_point(data = fire_speed_by_year_viirs, mapping = aes(x = dec_date, y = v_max))

p_viirs <- ggplot() + geom_boxplot(data = fire_speed_by_year_viirs, mapping = aes(x = year, y = v_max)) +
  labs(x = "Year", y = "Maximum fire speed (km h\u207B\u00B9)") + ggtitle("VIIRS-Derived Fire Speed Estimates") +
  ylim(c(-0.1, 7)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_viirs

#ggsave("VIIRS-derived_fire_speed.png", plot = p_viirs, height = 6, width = 7.5, units = "in")

m1 <- lm(v_max ~ dec_date, data = fire_speed_by_year_viirs)
m1
summary(m1)

modis <- read.csv("data/burn_direction_modis.csv")
idx_valid <- !is.na(modis$time_end)
modis <- modis[idx_valid,]
modis$time_end <- as.POSIXct(modis$time_end, tz = "UTC")
modis$time_start <- modis$time_end - dhours(modis$dt)

modis_perims <- st_read("data/perim_polygons_modis.geojson")

str(modis)

New_ID <- unique(modis$New_ID)

idx <- which(modis$Incid_Name == "EAST TROUBLESOME")

test <- modis[idx,]
test_perims <- dplyr::filter(modis_perims, Incid_Name == "EAST TROUBLESOME")
# Plot burn direction
ggplot() + geom_segment(data = test, mapping = aes(x = x1, xend = x2, y = y1, yend = y2), arrow = arrow(length = unit(0.2,"cm"))) +
  geom_sf(data = test_perims, fill = NA)

fire_speed_modis <- modis %>% group_by(New_ID) %>% arrange(time_start) %>% summarize(v_max = max(v_elev), date = Ig_Date[1])
idx <- !is.na(fire_speed_modis$v_max)
fire_speed_modis <- fire_speed_modis[idx,]

yr <- year(fire_speed_modis$date)
fire_speed_by_year_modis <- mutate(fire_speed_modis, year = as.factor(yr)) %>% drop_na()

fire_speed_by_year_modis$dec_date <- decimal_date(as.Date(fire_speed_by_year_modis$date))

p_scatter <- ggplot() + geom_point(data = fire_speed_by_year_modis, mapping = aes(x = dec_date, y = v_max))
p_scatter

p_modis <- ggplot() + geom_boxplot(data = fire_speed_by_year_modis, mapping = aes(x = year, y = v_max)) +
  labs(x = "Year", y = "Maximum fire speed (km h\u207B\u00B9)") + ggtitle("MODIS-Derived Fire Speed Estimates") +
  ylim(c(0, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p_modis

#ggsave("MODIS-derived_fire_speed.png", plot = p_modis, height = 6, width = 7.5, units = "in")


m2 <- lm(v_max ~ dec_date, data = fire_speed_by_year_modis)
m2
summary(m2)

# This is a rate per year
a0 <- coef(m2)[1]
a1 <- coef(m2)[2]

a1_20yrs <- a1*20
a1_20yrs

idx <- is.na(fire_speed_by_year_modis$v_max)
sum(idx)

p_trend <- p_scatter + geom_abline(intercept = unname(a0), 
                                   slope = unname(a1), 
                                   color = 'red') + 
  xlim(2000, 2024) + 
  ylim(0, 10)
p_trend

# What is happening to the fastest fires? Find the 95th percentile for each year

yrs_modis <- seq(floor(min(fire_speed_by_year_modis$dec_date, na.rm = TRUE)), floor(max(fire_speed_by_year_modis$dec_date, na.rm = TRUE)))

speed_quantile <- vector(length = length(yrs_modis))

quantile_df <- data.frame(year = yrs_modis, speed = speed_quantile)

for (i in 1:nrow(quantile_df)){
  temp_df <- dplyr::filter(fire_speed_by_year_modis, year == floor(quantile_df$year[i]))
  quantile_df$speed[i] <- quantile(temp_df$v_max, probs = 0.95, na.rm = TRUE)
}

# No trend in the cut-off values themselves (this is a proxy for maximum speed)
p1 <- ggplot() + geom_point(data = quantile_df, mapping = aes(x = year, y = speed)) +
  xlab("Year") + ylab("95th-Percentile Speed (km h\u207B\u00B9)") +
  ggh4x::force_panelsizes(rows = unit(2.2, "in"), cols = unit(2.5, "in"))

#ggsave("fire_speed_by_time_95th_percentile.png", plot = p1, dpi = 600, width = 3.5, height = 3)

# Here, we see that there is no trend in the 95th percentile thresholds from
# year to year through the record. 
test <- lm(speed ~ year, data = quantile_df)
summary(test)

fire_fgr_modis <- modis %>% group_by(New_ID) %>% arrange(time_start) %>% summarize(fgr_max = max(dAdt, na.rm = TRUE), date = as.Date(Ig_Date[1]))

fire_fgr_modis$year <- decimal_date(fire_fgr_modis$date)

fgr_check <- lm(fgr_max ~ year, data = fire_fgr_modis)
summary(fgr_check)

fgr_quantile <- vector(length = length(yrs_modis))

fgr_df <- data.frame(year = yrs_modis, fgr_km2 = fgr_quantile)

for (i in 1:nrow(fgr_df)){
  temp_df <- dplyr::filter(fire_fgr_modis, floor(year) == fgr_df$year[i])
  fgr_df$fgr_km2[i] <- quantile(temp_df$fgr_max, probs = 0.95, na.rm = TRUE)
}

fgr_df

# No significant trends here, either
p2 <- ggplot() + geom_point(data = fgr_df, mapping = aes(x = year, y = fgr_km2)) +
  xlab("Year") + ylab("95th-Percentile Growth Rate (km\u00B2 h\u207B\u00B9)") +
  ggh4x::force_panelsizes(rows = unit(2.2, "in"), cols = unit(2.5, "in"))

#ggsave("fire_fgr_by_time_95th_percentile.png", plot = p2, dpi = 600, width = 3.5, height = 3)

fire_trends_panel <- plot_grid(p1, NULL, p2, ncol = 3, rel_widths = c(1, -0.05, 1), align = "hv", labels = c("a.", " ", "b."), axis = "tblr", label_fontface = "plain")
fire_trends_panel

test_fgr <- lm(fgr_km2 ~ year, data = fgr_df)
summary(test_fgr)

ggsave("fire_trends_panel.png", plot = fire_trends_panel, dpi = 600, width = 7.5, height = 3, bg = "white")




