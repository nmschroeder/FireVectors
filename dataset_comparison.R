library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Script to compare fire progressions from multiple sources

# Read in data from NIROPS
nirops <- st_read("NIROPS/nirops_mtbs_match.shp") 

nirops_speed <- read.csv("NIROPS/burn_direction_nirops.csv")
str(nirops_speed)

california <- st_read("TIGER/tl_2012_us_state.shp") %>% dplyr::filter(NAME == "California") %>% st_transform(crs = 5070)

nirops_sf <- st_as_sf(dplyr::filter(nirops_speed, !is.na(x1), !is.na(y1)), coords = c("x1", "y1"), crs = 5070)

idx <- st_intersects(nirops_sf, california, sparse = FALSE)

nirops_ca <- nirops_sf[idx,]

# Check that we have California fires
ggplot() + geom_sf(data = california, fill=NA) + geom_sf(data = nirops_ca)

nirops_ca$year <- year(nirops_ca$time_start)

# Okay, pull the fastest one(s)
nirops_ca <- nirops_ca %>% dplyr::filter(year == 2020) %>% arrange(desc(v_xy))

nirops_fastest <- nirops_ca[1,]

nirops_perims <- nirops %>% dplyr::filter(New_ID == nirops_fastest$New_ID)

# Read in data from FIRED
fired <- st_read("~/Data/fired/fired_uscan_to2021121_daily_fixed.gpkg")

# Read in data from MTBS
mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp")

# Read in data from FEDS
feds_dir <- "FEDS/Largefire"
feds_2020 <- paste0(feds_dir, "/LargeFires_2020.gpkg")

st_layers(feds_2020)

# Read in FireVectors data
firevectors <- st_read("data/perim_polygons_viirs.geojson")
str(firevectors)

fv_modis <- st_read("data/perim_polygons_modis.geojson")

fv_sample <- dplyr::filter(firevectors, New_ID == nirops_fastest$New_ID)

mtbs_fire <- dplyr::filter(mtbs, New_ID == nirops_fastest$New_ID)

feds_perims_2020 <- st_read(feds_2020, layer = "perimeter") %>% 
  st_make_valid() %>%
  st_transform(crs = st_crs(mtbs_fire))
str(feds_perims_2020)


# Find fired perimeters with a similar ignition date
str(fired)

fired$ig_date <- as.POSIXct(fired$ig_date)

fired_filter <- dplyr::filter(fired, ig_date > (mtbs_fire$Ig_Date - days(30)) & 
                                ig_date < (mtbs_fire$Ig_Date + days(120))) %>%
  st_transform(crs = st_crs(mtbs_fire))
str(fired_filter)

idx <- st_intersects(fired_filter, st_buffer(mtbs_fire, dist = 5000), sparse = FALSE)

fired_mtbs_match <- fired_filter[idx,]

ggplot() + geom_sf(data = fired_mtbs_match, fill = NA, mapping = aes(color = as.factor(id)))

ggplot() + geom_sf(data = fired_mtbs_match, fill = NA, mapping = aes(color = date))

idx <- st_intersects(feds_perims_2020, st_buffer(mtbs_fire, dist = 5000), sparse = FALSE)
sum(idx)

feds_sample <- feds_perims_2020[idx,]
feds_sample

if (length(unique(feds_sample$fireID))){
  id <- unique(feds_sample$fireID)
}

feds_fire <- feds_perims_2020 %>% dplyr::filter(fireID == id)
feds_fire$date <- as.POSIXct(feds_fire$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

fv_fire <- firevectors %>% dplyr::filter(New_ID == nirops_fastest$New_ID) 
fvm_fire <- fv_modis %>% dplyr::filter(New_ID == nirops_fastest$New_ID)

# Convert to a consistent CRS
feds_fire <- feds_fire %>% st_transform(crs = 5070)
nirops_fire <- nirops_perims %>% st_transform(crs = 5070)
fv_fire   <- fv_fire %>% st_transform(crs = 5070)
fired_fire <- fired_mtbs_match

fired_df <- dplyr::select(fired_fire, geom, date) %>% rename(geometry = geom) %>% mutate(source = "a. FIRED")
feds_df <- dplyr::select(feds_fire, geom, date) %>% rename(geometry = geom) %>% mutate(source = "b. FEDS")
nirops_df <- dplyr::select(nirops_fire, geometry, DateUTC) %>% rename(date = DateUTC) %>% mutate(source = "c. NIROPS")
fvm_df <- dplyr::select(fvm_fire, geometry, summary_date) %>% rename(date = summary_date) %>% mutate(source = "d. FireVectors - MODIS")
fv_df <- dplyr::select(fv_fire, geometry, summary_date) %>% rename(date = summary_date) %>% mutate(source = "e. FireVectors - VIIRS")

fires_df <- rbind.data.frame(fired_df, feds_df, nirops_df, fvm_df, fv_df)


# Using facet grid

min_date <- mtbs_fire$Ig_Date %>% as.POSIXct()
max_date <- max(fv_sample$summary_date)

bb <- st_bbox(st_transform(fires_df, crs = 4326))
x_breaks <- pretty(c(bb["xmin"], bb["xmax"]), n = 2) 

fires_df$date <- as.POSIXct(fires_df$date, tz = "UTC")

ggplot() + geom_sf(data = arrange(fires_df, desc(date)), fill = NA, mapping = aes(color = date), lwd = 0.6) +
  scale_color_viridis_c(
    name = "Date",
    option = "inferno",
    trans = "time",
    begin = 0, 
    end = 1.0,
    labels = label_date("%b-%d"),
    limits = c(min_date, max_date),
    guide = guide_colorbar()
  ) +
  scale_x_continuous(breaks = x_breaks) +
  theme_bw() +
  theme(
    legend.position = c(0.76, 0.45),     # x,y in [0,1]; aims at bottom-right cell
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    panel.spacing.x = unit(0.8, "lines")
  ) +
  facet_wrap(~source)

ggsave("dataset_comparison_panel.png", dpi = 600, width = 183, height = 130, units = "mm")

