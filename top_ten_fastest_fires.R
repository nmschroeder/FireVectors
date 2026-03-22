library(dplyr)
library(sf)
library(ggplot2)
library(cowplot)
library(grid)
library(stringr)
library(lubridate)
library(ggspatial)

# Read in VIIRS- and MODIS-derived burn perimeters
viirs_perims <- st_read("data/perim_polygons_viirs.geojson") %>% dplyr::filter(perim_avail == 1)
str(viirs_perims)

modis_perims <- st_read("data/perim_polygons_modis.geojson") %>% dplyr::filter(perim_avail == 1)
str(modis_perims)

# Burn direction
viirs <- read.csv("data/burn_direction_viirs.csv") %>% dplyr::filter(burn_avail == TRUE)
modis <- read.csv("data/burn_direction_modis.csv") %>% dplyr::filter(burn_avail == TRUE)

# Active fire pixels
viirs_pixels <- st_read("data/active_fire_pixel_data_viirs.geojson")
modis_pixels <- st_read("data/active_fire_pixel_data_modis.geojson")

# Table 1
ids_pixel_events_modis <- unique(modis_pixels$New_ID)
N_pixel_events_modis <- length(ids_pixel_events_modis)

ids_pixel_events_viirs <- unique(viirs_pixels$New_ID)
N_pixel_events_viirs <- length(ids_pixel_events_viirs)

N_overlap <- sum(ids_pixel_events_viirs %in% ids_pixel_events_modis)

modis_test <- modis_pixels %>% dplyr::filter(SATELLITE == "Aqua") %>% group_by(New_ID) %>% summarize(N_dates = length(unique(ACQ_DATETIME)))
str(modis_test)

N_speed_verify <- modis_test %>% dplyr::filter(N_dates > 1) %>% nrow()

viirs_test <- viirs_pixels %>% dplyr::filter(SATELLITE == "N") %>% group_by(New_ID) %>% summarize(N_dates = length(unique(ACQ_DATETIME)))
str(viirs_test)

N_overlap_by_sat <- sum(viirs_test$New_ID %in% modis_test$New_ID)

N_speed_verify_viirs <- viirs_test %>% dplyr::filter(N_dates > 1) %>% nrow()

# Which event IDs are missing from the burn direction data set despite having multiple returns from our satellites of interest?



theme_set(
  theme_void(base_size = 10, base_family = "Helvetica") +
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


modis <- arrange(modis, desc(v_xy))
str(modis)

N <- 10

fire_sample <- modis[1:N,]

#write.csv(fire_sample, "fastest_ten_fires.csv")

unique_ids <- unique(fire_sample$New_ID)

perim_sample <- dplyr::filter(modis_perims, New_ID %in% unique_ids)
pixel_sample <- dplyr::filter(modis_pixels, New_ID %in% unique_ids)
burn_sample <- dplyr::filter(modis, New_ID %in% unique_ids)

ggplot() + geom_sf(data = perim_sample, fill = NA) + facet_wrap(~New_ID)

ext_list <- list()

for (i in 1:N){
  perim_temp <- dplyr::filter(perim_sample, New_ID == unique_ids[i]) %>% st_buffer(dist = 2000)
  
  ext <- st_bbox(perim_temp) 
  ext_temp <- data.frame(xmin = ext$xmin, xmax = ext$xmax, ymin = ext$ymin, ymax = ext$ymax) %>%
    mutate(dx = xmax - xmin, dy = ymax - ymin, cx = mean(c(xmin, xmax)), cy = mean(c(ymin, ymax)))
  ext_list[[i]] <- ext_temp
}

ext_df <- do.call(rbind.data.frame, ext_list)


make_two_line_title <- function(letter, year, name, max_chars_line1_total,indent_line2) {
  
  prefix1 <- paste0(letter, " ", year, " ")  # "f. 2007 "
  prefix2 <- paste0(letter, " ")          # "f. " 
  indent <- strrep(" ", nchar(prefix2))
  
  words <- strsplit(name, "\\s+")[[1]]
  words <- words[nzchar(words)]
  
  # Pack as many words as fit on line 1, counting TOTAL length (prefix + name)
  line1_words <- character(0)
  n_used <- 0
  
  for (k in seq_along(words)) {
    candidate_name <- paste(c(line1_words, words[k]), collapse = " ")
    if (nchar(paste0(prefix1, candidate_name)) <= max_chars_line1_total) {
      line1_words <- c(line1_words, words[k])
      n_used <- k
    } else {
      break
    }
  }
  
  # Fallback if even the first word doesn't fit: force it on line 1
  if (n_used == 0) {
    line1_words <- words[1]
    n_used <- 1
  }
  
  line1 <- paste0(prefix1, paste(line1_words, collapse = " "))
  
  line2 <- if (n_used < length(words)) {
    paste(words[(n_used + 1):length(words)], collapse = " ")
  } else {
    ""
  }
  
  # Optional: indent line 2 so it starts under the year (by prefixing "f. ")
  if (indent_line2 && line2 != "") {
    line2 <- paste0(indent, line2)
  }
  
  paste0(line1, "\n", line2)
}


max_dx <- max(ext_df$dx)/2
max_dy <- max(ext_df$dy)/2

letter_labels <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j") %>% paste0(".")

plot_list <- list()

for (i in 1:N){
  perim_temp <- dplyr::filter(perim_sample, New_ID == unique_ids[i])
  perim_temp$Date <- as.Date(perim_temp$summary_date)
  pixel_temp <- dplyr::filter(pixel_sample, New_ID == unique_ids[i], ACQ_DATETIME < max(perim_temp$summary_date))
  pixel_temp$Date <- as.Date(pixel_temp$ACQ_DATETIME)
  pixel_temp <- arrange(pixel_temp, ACQ_DATETIME)
  pixel_temp$days <- pixel_temp$Date - pixel_temp$Date[1]
  pixel_temp <- st_buffer(pixel_temp, dist = 500)
  
  yr <- year(perim_temp$Date[1])
  
  cx <- ext_df$cx[i]
  cy <- ext_df$cy[i]
  
  perim_name <- perim_temp$Incid_Name[1] %>% str_to_title() #%>% str_remove("\\s+Complex$")
  

  title_txt <- make_two_line_title(letter_labels[i], yr, perim_name, 20, FALSE)

  
  p_temp <- ggplot() + 
    geom_sf(data = pixel_temp, color = NA, mapping = aes(fill = days)) + 
    geom_sf(data = perim_temp, 
            fill = NA, color = 'black') + 

    geom_segment(dplyr::filter(burn_sample, New_ID == unique_ids[i], d_xy >= 4), 
                 mapping = aes(x = x1, xend = x2, y = y1, yend = y2, color = v_xy), 
                 arrow = arrow(type = "closed", length = unit(0.2,"cm")),
                 linewidth = 1.2) + 
    scale_color_viridis_c(option = "F", direction = 1, begin = 0, end = 0.7, name = "Fire\nSpeed\n(km h\u207B\u00B9)", limits = c(0, 3.4), guide = guide_colorbar(barwidth = 0.5, barheight = 5)) +
    scale_fill_gradient(name = "Days\nSince\nIgnition", low = 'grey90', high = 'grey10', limits = c(0, 20), guide = guide_colorbar(barwidth = 0.5, barheight = 5)) +
    #labs(title = paste0(letter_labels[i], " ", yr, " ", perim_name)) +
    labs(title = title_txt) +
    guides(color = guide_colorbar(barwidth = unit(0.4, "cm"),
                                  barheight = unit(2, "cm")),
           fill = guide_colorbar(barwidth = unit(0.4, "cm"),
                                 barheight = unit(2, "cm"))) +
    coord_sf(
      xlim = c(cx - max_dx, cx + max_dx),
      ylim = c(cy - max_dy, cy + max_dy),
      expand = FALSE,
      datum  = NA            # removes graticule reference lines
    ) +
    # xlim(c((cx - max_dx), (cx + max_dx))) +
    # ylim(c((cy - max_dy), (cy + max_dy))) +
    theme(
      legend.position.inside = c(0.98, 0.02),   # x,y in [0,1] of the panel
      legend.justification   = c(1, 0),         # anchor legend’s top-left/bottom-right
      legend.background      = element_rect(fill = "white", color = NA),
      legend.margin          = margin(2, 2, 2, 2),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0),
      plot.title.position = "plot"
    ) + 
    xlab("") + 
    ylab("") +
    annotation_north_arrow(location="br", which_north="true",
                           pad_x = unit(0.05, "in"),
                           pad_y = unit(0.05, "in"),
                           style = north_arrow_fancy_orienteering(
                             text_col  = NA      # or: text_size = 0
                           ),
                           height = unit(0.5,"cm"), width = unit(0.5,"cm"))
  
  if (i == 1){
    p_legend <- cowplot::get_legend(p_temp)
  }
  
  p_temp <- p_temp + theme(legend.position = "none")
  
  if (i == 10){ # Note: checked on Oct. 8, 2025, to make sure this doesn't cause spatial distortion;
    # It does not.
    p_temp <- p_temp + annotation_scale(location="bl", width_hint=0.30, text_cex=1)
  }
  
  
  
  plot_list[[i]] <- p_temp
}



legend1 <- p_legend$grobs[[1]]
legend2 <- p_legend$grobs[[2]]

plot_legend <- plot_grid(legend1, NULL, legend2, rel_heights = c(1, -0.3, 1), align = "hv", ncol = 1)

plot_legend

p <- plot_grid(plotlist = plot_list, align = "hv", axis = "tblr", ncol = 5)

p

plot_complete <- plot_grid(p, plot_legend, rel_widths = c(0.92, 0.08), ncol = 2)

plot_complete

ggsave("top_ten_fastest_fires.png", dpi = 600, bg = "white", plot = plot_complete, width = 183, height = 100, units = "mm")


