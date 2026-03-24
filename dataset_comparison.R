library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Script to compare fire progressions from multiple sources

# Read in reference data from NIFC
nifc <- st_read("~/Data/InterAgencyFirePerimeterHistory_All_Years_View/InterAgencyFirePerimeterHistory_All_Years_View.shp")

# Read in data from FIRED
fired <- st_read("~/Data/fired/fired_uscan_to2021121_daily_fixed.gpkg")

# Read in data from MTBS
mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp")

# Read in data from FEDS
feds_dir <- "/Volumes/SSD/FEDS/Largefire"
feds_2018 <- paste0(feds_dir, "/LargeFires_2018.gpkg")
st_layers(feds_2018)

# Read in FireVectors data
firevectors <- st_read("data/perim_polygons_viirs.geojson")
str(firevectors)

fv_modis <- st_read("data/perim_polygons_modis.geojson")

fv_sample <- dplyr::filter(firevectors, Incid_Name == "CAMP", year(summary_date) == 2018)

nifc_fire <- dplyr::filter(nifc, INCIDENT == "Camp", FIRE_YEAR == 2018) %>% st_make_valid()

mtbs_fire <- dplyr::filter(mtbs, Incid_Name == "CAMP")

st_crs(mtbs_fire)

ggplot() + geom_sf(data = nifc_fire, fill = NA)

feds_perims_2018 <- st_read(feds_2018, layer = "perimeter") %>% 
  st_make_valid() %>%
  st_transform(crs = st_crs(mtbs_fire))
str(feds_perims_2018)


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

# What ignition date does NIFC give? Find fires within the dates and region and
# select the unique FIRED ID(s) for that


idx <- st_intersects(feds_perims_2018, st_buffer(mtbs_fire, dist = 5000), sparse = FALSE)
sum(idx)

feds_sample <- feds_perims_2018[idx,]
feds_sample

if (length(unique(feds_sample$fireID))){
  id <- unique(feds_sample$fireID)
}

feds_fire <- feds_perims_2018 %>% dplyr::filter(fireID == id)
feds_fire$date <- as.POSIXct(feds_fire$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")


fv_fire <- firevectors %>% dplyr::filter(Incid_Name == "CAMP") 
fvm_fire <- fv_modis %>% dplyr::filter(Incid_Name == "CAMP")

# Convert to a consistent CRS
nifc_fire <- nifc_fire %>% st_transform(crs = 5070)
feds_fire <- feds_fire %>% st_transform(crs = 5070)
fv_fire   <- fv_fire %>% st_transform(crs = 5070)
fired_fire <- fired_mtbs_match

nifc_df <- dplyr::select(nifc_fire, geometry) %>% mutate(date = as.POSIXct(NA), source = "a. NIFC")
mtbs_df <- dplyr::select(mtbs_fire, geometry) %>% mutate(date = as.POSIXct(NA), source = "a. MTBS")

fired_df <- dplyr::select(fired_fire, geom, date) %>% rename(geometry = geom) %>% mutate(source = "b. FIRED")
feds_df <- dplyr::select(feds_fire, geom, date) %>% rename(geometry = geom) %>% mutate(source = "c. FEDS")
fv_df <- dplyr::select(fv_fire, geometry, summary_date) %>% rename(date = summary_date) %>% mutate(source = "d. FireVectors - VIIRS")
fvm_df <- dplyr::select(fvm_fire, geometry, summary_date) %>% rename(date = summary_date) %>% mutate(source = "e. FireVectors - MODIS")

fires_df <- rbind.data.frame(mtbs_df, fired_df, feds_df, fv_df, fvm_df)


# Using facet grid

min_date <- mtbs_fire$Ig_Date %>% as.POSIXct()
max_date <- as.POSIXct("2018-11-25") 

bb <- st_bbox(st_transform(fires_df, crs = 4326))
x_breaks <- pretty(c(bb["xmin"], bb["xmax"]), n = 2) 

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

# Use fill to show differences in burned area detection

# ggplot() + geom_sf(data = arrange(fires_df, desc(date)), mapping = aes(color = date, fill = date), lwd = 0.6) +
#   scale_fill_viridis_c(
#     name = "Date",
#     option = "inferno",
#     trans = "time",
#     begin = 0, 
#     end = 1.0,
#     labels = label_date("%b-%d"),
#     limits = c(min_date, max_date),
#     guide = guide_colorbar()
#   ) +
#   scale_color_viridis_c(
#     name = "Date",
#     option = "inferno",
#     trans = "time",
#     begin = 0, 
#     end = 1.0,
#     labels = label_date("%b-%d"),
#     limits = c(min_date, max_date),
#     guide = guide_colorbar()
#   ) +
#   scale_x_continuous(breaks = x_breaks) +
#   theme_bw() +
#   theme(
#     legend.position = c(0.76, 0.45),     # x,y in [0,1]; aims at bottom-right cell
#     legend.justification = c(0, 1),
#     legend.background = element_rect(fill = "white", color = NA),
#     panel.spacing.x = unit(0.8, "lines")
#   ) +
#   facet_wrap(~source)
# 
# ggsave("dataset_comparison_panel_v2.png", dpi = 600, width = 183, height = 130, units = "mm")
# 
