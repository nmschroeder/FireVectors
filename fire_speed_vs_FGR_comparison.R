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

viirs <- read.csv("viirs_speed_veg_class.csv") %>% dplyr::filter(burn_avail == TRUE, nlcd_class %in% c("Forest", "Shrubland", "Herbaceous"))
modis <- read.csv("modis_speed_veg_class.csv") %>% dplyr::filter(burn_avail == TRUE, nlcd_class %in% c("Forest", "Shrubland", "Herbaceous"))

str(viirs)

unique_viirs <- viirs %>% dplyr::pull(New_ID) %>% unique() %>% sort()
unique_modis <- modis %>% dplyr::pull(New_ID) %>% unique() %>% sort()

viirs_list <- list()

for (i in 1:length(unique_viirs)){
  temp_df <- dplyr::filter(viirs, New_ID == unique_viirs[i])
  idx <- which.max(temp_df$v_xy)
  viirs_list[[i]] <- temp_df[idx,]
  
}

viirs_df <- do.call(rbind.data.frame, viirs_list)

modis_list <- list()

for (i in 1:length(unique_modis)){
  temp_df <- dplyr::filter(modis, New_ID == unique_modis[i])
  idx <- which.max(temp_df$v_xy)
  modis_list[[i]] <- temp_df[idx,]
  
}
modis_df <- do.call(rbind.data.frame, modis_list)

viirs_df$nlcd_class <- factor(viirs_df$nlcd_class,levels = c("Forest", "Shrubland", "Herbaceous"))
modis_df$nlcd_class <- factor(modis_df$nlcd_class,levels = c("Forest", "Shrubland", "Herbaceous"))

p1 <- ggplot() + geom_point(data = viirs_df, mapping = aes(y = dAdt, x = v_xy), alpha = 0.2) + ylim(c(0, 30)) + xlim(c(0, 2.2))
p2 <- ggplot() + geom_point(data = modis_df, mapping = aes(y = dAdt, x = v_xy), alpha = 0.2) + ylim(c(0, 30)) + xlim(c(0, 2.2))

p_panel <- plot_grid(p1, p2)
p_panel



veg_labs <- c(
  Forest = "a. Forest",
  Shrubland = "b. Shrubland",
  Herbaceous = "c. Grassland"
)

# VIIRS
p_facet1 <- p1 + facet_wrap(~nlcd_class, labeller = labeller(nlcd_class = veg_labs)) + xlab("Fire Speed (km h\u207B\u00B9)") + ylab("Fire Growth Rate (km\u00B2 h\u207B\u00B9)")
p_facet1

# MODIS
p_facet2 <- p2 + facet_wrap(~nlcd_class, labeller = labeller(nlcd_class = veg_labs)) + xlab("Fire Speed (km h\u207B\u00B9)") + ylab("Fire Growth Rate (km\u00B2 h\u207B\u00B9)")
p_facet2

# Try log-scaling

q_threshold <- quantile(modis_df$dAdt, 0.05)

p_facet_log_modis <- ggplot() + geom_point(data = dplyr::filter(modis_df, dAdt > q_threshold), mapping = aes(y = log(dAdt), x = log(v_xy)), alpha = 0.2) + facet_wrap(~nlcd_class, labeller = labeller(nlcd_class = veg_labs)) + xlab("Log speed (km h\u207B\u00B9)") + ylab("Log growth rate (km\u00B2 h\u207B\u00B9)") #+ ylim(c(0, 30)) + xlim(c(0, 2.2)) 
p_facet_log_modis

#ggsave("log-log_plots_ros_by_speed_modis.png", plot = p_facet_log_modis, width = 183, height = 150, units = "mm")

# Create log log models of these data

m <- lm(log(dAdt) ~ log(v_xy) + 1, data = dplyr::filter(modis_df, dAdt > q_threshold))
summary(m)


m_forest <- lm(log(dAdt) ~ log(v_xy) + 1, data = dplyr::filter(modis_df, dAdt > q_threshold, nlcd_class == "Forest"))
summary(m_forest)

m_shrub <- lm(log(dAdt) ~ log(v_xy) + 1, data = dplyr::filter(modis_df, dAdt > q_threshold, nlcd_class == "Shrubland"))
summary(m_shrub)

m_grass <- lm(log(dAdt) ~ log(v_xy) + 1, data = dplyr::filter(modis_df, dAdt > q_threshold, nlcd_class == "Herbaceous"))
summary(m_grass)

m_df <- data.frame(yintercept = c(m_forest$coefficients[1], m_shrub$coefficients[1], m_grass$coefficients[1]), slope = c(m_forest$coefficients[2], m_shrub$coefficients[2], m_grass$coefficients[2]), R2 = c(summary(m_forest)$adj.r.squared, summary(m_shrub)$adj.r.squared, summary(m_grass)$adj.r.squared), nlcd_class = as.factor(c("Forest", "Shrubland", "Herbaceous")))
m_df$label <- paste0("R^2 == ", round(m_df$R2, 2))
m_df$x <- -5
m_df$y <- 2.5

p_facet_log_modis <- ggplot() + geom_point(data = dplyr::filter(modis_df, dAdt > q_threshold), mapping = aes(y = log(dAdt), x = log(v_xy)), alpha = 0.5) + 
  geom_abline(data = m_df, mapping = aes(slope = slope, intercept = yintercept), color = 'red') +
  geom_text(data = m_df, mapping = aes(x = x, y = y, label = label), parse = TRUE) +
  facet_wrap(~nlcd_class, labeller = labeller(nlcd_class = veg_labs)) + xlab("Log fire speed (km h\u207B\u00B9)") + ylab("Log fire growth rate (km\u00B2 h\u207B\u00B9)") #+ ylim(c(0, 30)) + xlim(c(0, 2.2)) 
p_facet_log_modis

ggsave("log-log_plots_fgr_by_speed_modis.png", plot = p_facet_log_modis, width = 183, height = 80, units = "mm")




