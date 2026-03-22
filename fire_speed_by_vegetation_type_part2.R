# Script to identify the primary vegetation type for each MTBS perimeter

library(tidyverse)
library(sf)
library(ggplot2)
library(raster)
library(exactextractr)
library(lme4)
library(viridis)
library(cowplot)
library(dunn.test)
library(rstatix)

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


mtbs_class <- st_read("mtbs_veg_class.gpkg")
str(mtbs_class)

class_names <- c("Forest", "Shrubland", "Herbaceous")

nrow(mtbs_class)

modis_speed <- read.csv("data/burn_direction_modis.csv")

modis_valid <- modis_speed %>% dplyr::filter(!is.na(v_xy))
modis_speed_unique <- modis_valid %>% pull(New_ID) %>% unique()


mtbs_valid <- dplyr::filter(mtbs_class, New_ID %in% modis_speed_unique)
mtbs_valid

modis_valid_unique <- modis_valid %>% group_by(New_ID) %>% summarize(max_v_xy = max(v_xy, na.rm = TRUE))

mtbs_speed <- left_join(mtbs_valid, modis_valid_unique, by = "New_ID")

mtbs_long <- pivot_longer(mtbs_speed, cols = c("Forest", "Shrubland", "Herbaceous"), names_to = "veg_type")
str(mtbs_long)

mtbs_data = dplyr::filter(mtbs_long, nlcd_class %in% class_names)

# Create colors for the NLCD classes using Viridis to use throughout
cols <- viridis(3, option = "viridis")

p_boxplot <- ggplot() + geom_boxplot(data = mtbs_data, mapping = aes(x = nlcd_class, y = max_v_xy, fill = nlcd_class)) +
  scale_x_discrete(
    limits = c("Forest", "Shrubland", "Herbaceous"),
    labels = c(
      "Forest" = "Forest",
      "Shrubland" = "Shrubland",
      "Herbaceous" = "Grassland"
    )
  ) +
  scale_fill_manual(  values = c(
    "Forest" = cols[1],
    "Shrubland" = cols[2],
    "Herbaceous" = cols[3]
  ), 
  labels = c(
    "Forest" = "Forest",
    "Shrubland" = "Shrubland",
    "Herbaceous" = "Grassland"
  ),
  name = ""
  ) +
  xlab("Primary NLCD Class") + ylab("Speed (km h\u207B\u00B9)") +
  guides(fill = "none")

p_boxplot

ggsave("speed_by_vegetation_class_boxplots.png", plot = p_boxplot, dpi = 600, width = 91, height = 80, units = "mm")


# Perform Kruskal-Wallis test (non-parametric)
kruskal_result <- kruskal.test(max_v_xy ~ nlcd_class, data = mtbs_data)
print(kruskal_result)

# Only tells you if there is a significant difference between at least one pair among
# three or more groups

# Perform Dunn's test to determine which groups are significantly different
dunn_test_result <- dunn.test(mtbs_data$max_v_xy, mtbs_data$nlcd_class, method = "bonferroni") # Can adjust p-value method
print(dunn_test_result)

# Alternatively, use Wilcox Test
mtbs_data %>% st_drop_geometry() %>% wilcox_test(max_v_xy ~ nlcd_class, p.adjust.method = "bonferroni")

mtbs_summary <- mtbs_data %>% st_drop_geometry() %>% group_by(nlcd_class) %>% summarize(mean = mean(max_v_xy, na.rm = TRUE),
                                                                                        median = median(max_v_xy, na.rm = TRUE),
                                                                                        q75 = quantile(max_v_xy, 0.75, na.rm = TRUE),                                                                                  q95 = quantile(max_v_xy, 0.95, na.rm = TRUE))

mtbs_summary
