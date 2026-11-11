library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Script to compare fire progressions from multiple sources

days_from_ignition <- 14

# Read in data from NIROPS
nirops <- st_read("NIROPS/nirops_mtbs_match.shp") 

nirops_speed <- read.csv("NIROPS/burn_direction_nirops.csv")
str(nirops_speed)

california <- st_read("~/Data/TIGER/tl_2012_us_state.shp") %>% dplyr::filter(NAME == "California") %>% st_transform(crs = 5070)

# Read in data from MTBS
mtbs <- st_read("MTBS/mtbs_wus_2000_2024.shp")

# Read in reference data from NIFC
nifc <- st_read("~/Data/InterAgencyFirePerimeterHistory_All_Years_View/InterAgencyFirePerimeterHistory_All_Years_View.shp")

# Read in data from FIRED
fired <- st_read("~/Data/fired/fired_uscan_to2021121_daily_fixed.gpkg")

# Read in data from FEDS
feds_dir <- "/Volumes/SSD/FEDS/Largefire"
feds_2020 <- paste0(feds_dir, "/LargeFires_2020.gpkg")

st_layers(feds_2020)

feds_perims_2020 <- st_read(feds_2020, layer = "perimeter") %>% 
  st_make_valid() %>%
  st_transform(crs = st_crs(mtbs))
str(feds_perims_2020)

unique(feds_perims_2020$fireID)%>%length()

# Read in FireVectors data
firevectors <- st_read("data/perim_polygons_viirs.geojson")
str(firevectors)

# Fire Vectors from MODIS
fv_modis <- st_read("data/perim_polygons_modis.geojson")


## The NIROPS and FEDS data sets we are using overlap for the 2020 in California; let's identify
## just those fires in NIROPS

nirops_sf <- st_as_sf(dplyr::filter(nirops_speed, !is.na(x1), !is.na(y1)), coords = c("x1", "y1"), crs = 5070)

idx <- st_intersects(nirops_sf, california, sparse = FALSE)

nirops_ca <- nirops_sf[idx,]

check <- unique(nirops_ca$New_ID) %>% length()

# Check that we have California fires
ggplot() + geom_sf(data = california, fill=NA) + geom_sf(data = nirops_ca)

nirops_ca$year <- year(nirops_ca$time_start)

# Okay, pull the fastest one(s)
nirops_ca <- nirops_ca %>% dplyr::filter(year == 2020) %>% arrange(desc(v_xy))

# Fastest 2020 NIROPS CA fire
nirops_fastest <- nirops_ca[1,]

nirops_ids <- nirops_ca$New_ID %>% unique()

# Match to the California NIROPS
mtbs_matches <- dplyr::filter(mtbs, New_ID %in% nirops_ids)
fv_viirs_matches <- dplyr::filter(firevectors, New_ID %in% nirops_ids)
fv_modis_matches <- dplyr::filter(fv_modis, New_ID %in% nirops_ids)

nifc$irwin <- gsub(pattern = "[{}]", replacement = "", nifc$IRWINID)

nifc_matches <- dplyr::filter(nifc, irwin %in% nirops$IrwinID)

str(nifc_matches)

# Okay, so we still need to locate these fires within the FIRED and FEDS data sets
# which do not have names/labels for matching. We will use the ignition date and
# spatial information to find closest matches.

# Find fired perimeters with a similar ignition date
str(fired)

# Convert ignition date to POSIX
fired$ig_date <- as.POSIXct(fired$ig_date)

# For each fire in the MTBS final fire perimeters....
fired_mtbs_match_list <- list()

for (i in 1:nrow(mtbs_matches)){
  mtbs_fire <- mtbs_matches[i,]
  # fired_filter <- dplyr::filter(fired, ig_date > (mtbs_fire$Ig_Date - days(30)) & 
  #                                 ig_date < (mtbs_fire$Ig_Date + days(120))) %>%
  #   st_transform(crs = st_crs(mtbs_fire))
  # Troubleshooting further without filtering through the ENTIRE FIRED data set again
  fired_filter <- dplyr::filter(fired, ig_date > (mtbs_fire$Ig_Date - days(days_from_ignition)) & 
                                  ig_date < (mtbs_fire$Ig_Date + days(days_from_ignition))) %>%
    st_transform(crs = st_crs(mtbs_fire))
  
  idx <- st_intersects(fired_filter, st_buffer(mtbs_fire, dist = 5000), sparse = FALSE)
  
  
  
  
  
  # Proposed match
  fired_mtbs_match_list[[i]] <- fired_filter[idx,] %>% mutate(New_ID = mtbs_fire$New_ID)

}

fired_matches <- do.call(rbind.data.frame, fired_mtbs_match_list)
st_write(fired_matches, "fired_matches.geojson", delete_dsn = TRUE)
# Visually inspect.
ggplot() + geom_sf(data = california, fill = NA) + geom_sf(data = fired_matches, fill = NA)



feds_mtbs_match_list <- list()

for (i in 1:nrow(mtbs_matches)){
  mtbs_fire <- mtbs_matches[i,]
  ig_date <- mtbs_fire$Ig_Date
  ig_date_posix <- as.POSIXct(ig_date, tz = "UTC")
  # We need a constraint on the ignition date here as with the others
  feds_filter <- feds_perims_2020 %>% arrange(time) %>% dplyr::filter((time >= ig_date_posix - days(30)))
  
  idx <- st_intersects(feds_filter, st_buffer(mtbs_fire, dist = 5000), sparse = FALSE)
  
  feds_proposed <- feds_filter[idx,]
  
  feds_ids <- unique(feds_proposed$fireID)
  
  if (length(feds_ids)>1){
    
    # If there is more than one select the one with the closer start date
    feds_temp <- dplyr::filter(feds_perims_2020, fireID %in% feds_ids)
    feds_groups <- feds_temp %>% arrange(time) %>% group_by(fireID) %>% summarize(tstart = time[1])
    dt <- difftime(ig_date, as.Date(feds_groups$tstart), unit = "days") %>% as.numeric() %>% abs()
    idx <- abs(dt - min(dt))<=days_from_ignition
    feds_ids <- feds_groups$fireID[idx]
  }
  
  if (length(feds_ids)==0){
    print(paste0("no match found for ", as.character(mtbs_fire$New_ID)))
  
  } else{
    
    feds_mtbs_match_list[[i]] <- feds_perims_2020 %>% dplyr::filter(fireID %in% feds_ids) %>% mutate(New_ID = mtbs_fire$New_ID)
    
  }
  

}


feds_matches <- do.call(rbind.data.frame, feds_mtbs_match_list)
st_write(feds_matches, "feds_matches.geojson", delete_dsn = TRUE)

feds_matches$date <- as.POSIXct(feds_matches$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")


# Create plot for fastest fire (according to NIROPS) within the intersection of these data sets
nifc_fire <- nifc %>% dplyr::filter(irwin == nirops_fastest$irwinID) %>% st_transform(crs = 5070)
nirops_fire <- nirops %>% dplyr::filter(New_ID == nirops_fastest$New_ID) %>% st_transform(crs = 5070)
fired_fire <- fired_matches %>% dplyr::filter(New_ID == nirops_fastest$New_ID) %>% st_transform(crs = 5070)
feds_fire <- feds_matches %>% dplyr::filter(New_ID == nirops_fastest$New_ID) %>% st_transform(crs = 5070)
fv_fire <- firevectors %>% dplyr::filter(New_ID == nirops_fastest$New_ID) %>% st_transform(crs = 5070)
fvm_fire <- fv_modis %>% dplyr::filter(New_ID == nirops_fastest$New_ID) %>% st_transform(crs = 5070)
mtbs_fire <- mtbs %>% dplyr::filter(New_ID == nirops_fastest$New_ID) %>% st_transform(crs = 5070)

nifc_df <- dplyr::select(nifc_fire, geometry) %>% mutate(date = as.POSIXct(NA), source = "a. NIFC")
mtbs_df <- dplyr::select(mtbs_fire, geometry) %>% mutate(date = as.POSIXct(NA), source = "a. MTBS")
fired_df <- dplyr::select(fired_fire, geom, date) %>% rename(geometry = geom) %>% mutate(source = "a. FIRED")
feds_df <- dplyr::select(feds_fire, geom, date) %>% rename(geometry = geom) %>% mutate(source = "b. FEDS")
nirops_df <- dplyr::select(nirops_fire, geometry, DateUTC) %>% rename(date = DateUTC) %>% mutate(source = "c. NIROPS")
fvm_df <- dplyr::select(fvm_fire, geometry, summary_date) %>% rename(date = summary_date) %>% mutate(source = "d. FireVectors - MODIS")
fv_df <- dplyr::select(fv_fire, geometry, summary_date) %>% rename(date = summary_date) %>% mutate(source = "e. FireVectors - VIIRS")

fires_df <- rbind.data.frame(fired_df, feds_df, nirops_df, fvm_df, fv_df)

# Using facet grid

min_date <- mtbs_fire$Ig_Date %>% as.POSIXct()
max_date <- max(fv_fire$summary_date)

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


feds_matches$New_ID %>% unique() %>% length()

# Okay, now we have matched FIRED and FEDS to NIROPS and FireVectors

# Let's take a look at the Jaccard index for the daily fire progressions

# For each day, take the latest fire progression of that day...

# Compute the Jaccard index on each pair (intersection over union)

# We may need to compute the cumulative geometry for FIRED within each event;
# it's original format is the additional area each day

fired_matches

fired_daily_list <- list()
count <- 1
for (i in 1:length(nirops_ids)){
  temp_df <- dplyr::filter(fired_matches, New_ID == nirops_ids[i]) %>% arrange(date)
  d <- temp_df$date %>% unique()
  
  for (j in 1:length(d)){
    # Filter for the specific day
    temp_df_by_date <- dplyr::filter(temp_df, date == d[j])
    
    # Union the pixels from a given day
    geo <- st_union(temp_df_by_date$geom)
    
    # If it is the first day...
    if (j==1){
      cumulative_geo <- geo
      
    } else{
      cumulative_geo <- st_union(geo, cumulative_geo)
    }
    fired_daily_list[[count]] <- data.frame(date = d[j], geometry = cumulative_geo, source = "FIRED", New_ID = nirops_ids[i])
    count <- count + 1
  }
  
}

fired_daily_df <- do.call(rbind.data.frame, fired_daily_list)

fired_daily_df

st_geometry(fired_daily_df) <- fired_daily_df$geometry





feds_daily_list <- list()
count <- 1
for (i in 1:length(nirops_ids)){
  temp_df <- dplyr::filter(feds_matches, New_ID == nirops_ids[i],ted_ampm == "AM") %>% arrange(date)
  d <- unique(as.Date(temp_df$date))
  
  if (nrow(temp_df)>0){
    for (j in 1:length(d)){
      temp_df_by_date <- dplyr::filter(temp_df, as.Date(date) == d[j]) 
      day_adj <- d[j]-day(1)
      feds_daily_list[[count]] <- data.frame(date = day_adj, geometry = temp_df_by_date$geom, source = "FEDS", New_ID = nirops_ids[i])
      count <- count + 1
    }
  }
}


feds_daily_df <- do.call(rbind.data.frame, feds_daily_list)


# Need to kick out cases where the start date is not within days_from_ignition days of the ig date to be consistent with the previous two data sets
fvv_daily_list <- list()
count <- 1
for (i in 1:length(nirops_ids)){
  temp_df <- dplyr::filter(firevectors, New_ID == nirops_ids[i]) %>% arrange(summary_date)
  
  mtbs_fire <- dplyr::filter(mtbs, New_ID == nirops_ids[i])
  
  if (difftime(as.Date(temp_df$summary_date[1]), mtbs_fire$Ig_Date, units = "days") <= days_from_ignition){
  
    d <- unique(as.Date(temp_df$summary_date))
    for (j in 1:length(d)){
      temp_df_by_date <- dplyr::filter(temp_df, as.Date(summary_date) == d[j]) %>% arrange(summary_date)
      temp_date <- temp_df_by_date$summary_date[nrow(temp_df_by_date)] %>% as.POSIXct(tz = "UTC")
      check_date <- as.POSIXct(paste0(d[j], " 12:00:00"), tz = "UTC")
      if (check_date > temp_date){
        temp_date <- temp_date - day(1)
      }
      
      fvv_daily_list[[count]] <- data.frame(date = as.Date(temp_date), geometry = temp_df_by_date$geometry[nrow(temp_df_by_date)], source = "FireVectors-VIIRS", New_ID = nirops_ids[i])
      count <- count + 1
    }
  }
}

fvv_daily_df <- do.call(rbind.data.frame, fvv_daily_list)

fvm_daily_list <- list()
count <- 1
for (i in 1:length(nirops_ids)){
  temp_df <- dplyr::filter(fv_modis, New_ID == nirops_ids[i]) %>% arrange(summary_date)
  
  mtbs_fire <- dplyr::filter(mtbs, New_ID == nirops_ids[i])
  
  if (difftime(as.Date(temp_df$summary_date[1]), mtbs_fire$Ig_Date, units = "days") <= days_from_ignition){
    d <- unique(as.Date(temp_df$summary_date))
    for (j in 1:length(d)){
      temp_df_by_date <- dplyr::filter(temp_df, as.Date(summary_date) == d[j]) %>% arrange(summary_date)
      temp_date <- temp_df_by_date$summary_date[nrow(temp_df_by_date)] %>% as.POSIXct(tz = "UTC")
      check_date <- as.POSIXct(paste0(d[j], " 12:00:00"), tz = "UTC")
      if (check_date > temp_date){
        temp_date <- temp_date - day(1)
      }
      
      fvm_daily_list[[count]] <- data.frame(date = as.Date(temp_date), geometry = temp_df_by_date$geometry[nrow(temp_df_by_date)], source = "FireVectors-MODIS", New_ID = nirops_ids[i])
      count <- count + 1
    }
  }
}

fvm_daily_df <- do.call(rbind.data.frame, fvm_daily_list)


# Do NIROPS have more than one? Hmmmmmm we probably want nighttime values for end of day... a lot of these UTC times are around 05:00:00 UTC which is 22:00:00 PT the previous day... so lets count that as end of day on the 
# previous day for an evening local time return... 
nirops

nirops_daily_list <- list()
count <- 1
for (i in 1:length(nirops_ids)){
  temp_df <- dplyr::filter(nirops, New_ID == nirops_ids[i]) %>% arrange(UTC)
  d <- unique(temp_df$DateUTC)
  for (j in 1:length(d)){
    temp_df_by_date <- dplyr::filter(temp_df, DateUTC == d[j]) %>% arrange(UTC)
    temp_date <- temp_df_by_date$UTC[nrow(temp_df_by_date)] %>% as.POSIXct(tz = "UTC")
    check_date <- as.POSIXct(paste0(d[j], " 12:00:00"), tz = "UTC")
    if (check_date > temp_date){
      temp_date <- temp_date - day(1)
    }
    
    final_df <- data.frame(date = as.Date(temp_date), geometry = temp_df_by_date$geometry[nrow(temp_df_by_date)], source = "NIROPS", New_ID = nirops_ids[i])
    final_df <- st_as_sf(final_df, crs = st_crs(temp_df)) %>% st_transform(crs = 5070)
    #ggplot() + geom_sf(data = final_df)
    nirops_daily_list[[count]] <- final_df
    count <- count + 1
  }
}

nirops_daily_df <- do.call(rbind.data.frame, nirops_daily_list) %>% st_transform(crs = 5070)

# Troubleshooting NIROPS CRS
check_id <- nirops_ids[1]

temp_df <- dplyr::filter(nirops_daily_df, New_ID == check_id)

# Still correct here
ggplot() + geom_sf(data = temp_df)

st_crs(nirops_daily_df)

# These are all missing...
st_crs(fired_daily_df)

st_crs(feds_daily_df)

st_crs(fvv_daily_df)

st_crs(fvm_daily_df)

daily_df <- rbind.data.frame(fired_daily_df, feds_daily_df, fvv_daily_df, fvm_daily_df, nirops_daily_df)

daily_df

unique_sources <- unique(daily_df$source)

jaccard_list <- list()

daily_df <- st_as_sf(daily_df)
daily_df$area <- st_area(daily_df) %>% as.numeric()

ggplot() + 
  geom_line(data = daily_df, mapping = aes(x = date, y = area*1e-6, color = source)) + 
  facet_wrap(~New_ID, scales = "free") +
  ylab(expression("Area "(km^2)))



# If we're going to look at cumulative area, let's look at all time points 
fired_matches_short <- fired_daily_df # needed to make cumulative geometries for these
feds_matches_short <- dplyr::select(feds_matches, time, geom, New_ID)%>% dplyr::filter(New_ID %in% nirops_ids) %>% st_transform(crs = 5070) %>% rename(geometry = geom, date = time) %>% mutate(source = "FEDS")
fvv_matches_short <- dplyr::select(firevectors, summary_date, geometry, New_ID)%>% dplyr::filter(New_ID %in% nirops_ids) %>% st_transform(crs = 5070) %>% rename(date = summary_date) %>% mutate(source = "FireVectors-VIIRS")
fvm_matches_short <- dplyr::select(fv_modis, summary_date, geometry, New_ID)%>% dplyr::filter(New_ID %in% nirops_ids) %>% st_transform(crs = 5070) %>% rename(date = summary_date) %>% mutate(source = "FireVectors-MODIS")
nirops_matches_short <- dplyr::select(nirops, UTC, geometry, New_ID) %>% dplyr::filter(New_ID %in% nirops_ids) %>% st_transform(crs = 5070) %>% rename(date = UTC) %>% mutate(source = "NIROPS")

matches_df <- rbind.data.frame(fired_matches_short, feds_matches_short, fvv_matches_short, fvm_matches_short, nirops_matches_short)

matches_df$area_km2 <- (1e-6)*as.numeric(st_area(matches_df))
matches_df$date <- as.POSIXct(matches_df$date, tz = "UTC")

# Panel draft
ggplot() + geom_line(data = matches_df, mapping = aes(x = date, y = area_km2, color = source)) + facet_wrap(~New_ID, scale = 'free')

# Need the MTBS names joined to matches_df; join by New_ID
mtbs_key <- mtbs %>% dplyr::filter(New_ID %in% matches_df$New_ID, Incid_Type == "Wildfire") %>% dplyr::select(New_ID, Incid_Name) %>% st_drop_geometry()

named_matches_df <- right_join(matches_df, mtbs_key, by = "New_ID")

ggplot() + 
  geom_line(data = named_matches_df, mapping = aes(x = date, y = area_km2, color = source)) +
  scale_color_manual(
    name = "",
    values = c(
      "FEDS" = "#56B4E9",
      "FIRED" = "#009E73",
      "FireVectors-MODIS" = "#E69F00",
      "FireVectors-VIIRS" = "#D55E00",
      "NIROPS" = "black"
    ),
    breaks = c(
      "NIROPS",
      "FEDS",
      "FIRED",
      "FireVectors-MODIS",
      "FireVectors-VIIRS"
    )
  ) +
  scale_x_date(
    breaks = function(x) {
      seq(
        from = x[1] + 0.25 * diff(x),
        to = x[2] - 0.25 * diff(x),
        length.out = 2
      )
    },
    date_labels = "%b %d"
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 3)
  ) +
  facet_wrap(~Incid_Name, scale = 'free', ncol = 5) +
  xlab("Date (All Fires Shown Begin in 2020)") +
  ylab(expression("Area "(km^2))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    #axis.text.x = element_text(angle = 45,hjust = 1)
  )

ggsave("data_comparison_area_by_date.png", dpi = 600, width = 183, height = 220, units = "mm")

