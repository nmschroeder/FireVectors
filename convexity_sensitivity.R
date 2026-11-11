library(sf)
library(stringr)
library(dplyr)
library(ggplot2)
library(cowplot)

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



nirops <- st_read("NIROPS/nirops_mtbs_match.shp")
str(nirops)

nirops_list <- list()
nirops_id <- unique(nirops$New_ID)
for (i in 1:length(nirops_id)){
  
  temp_df <- dplyr::filter(nirops, New_ID == nirops_id[i]) %>% arrange(UTC)
  nirops_list[[i]] <- temp_df[nrow(temp_df),]
}

nirops_df <- do.call(rbind.data.frame, nirops_list) %>% st_transform(crs = 5070)

fnames <- list.files(path = "sensitivity", pattern = ".csv$", full.names = TRUE)
fnames

burn_direction_data <- lapply(fnames, FUN = read.csv)

burn_direction_df <- do.call(rbind.data.frame, burn_direction_data)
str(burn_direction_df)

ggplot() + geom_boxplot(data = burn_direction_df, mapping = aes(x = as.factor(ratio_1), y = v_xy))

# Let's group by the event and select the fastest speed to look just as those...

burn_groups <- burn_direction_df %>% group_by(New_ID, ratio_1) %>% summarize(max_v = max(v_xy, na.rm = TRUE))

burn_groups

p_speed_by_convexity <- ggplot() + geom_boxplot(data = burn_groups, mapping = aes(x = as.factor(ratio_1), y = max_v)) +
  xlab("Convexity Fraction") + ylab(expression("Maximum speed (" * km ~ h^{-1} *")")) + theme_bw()


burn_direction_df$r <- as.factor(burn_direction_df$ratio_1)

anova_model <- aov(v_xy ~ r, burn_direction_df)
anova_model
summary(anova_model)

# Run Tukey's Honestly Significant Difference test
tukey_results <- TukeyHSD(anova_model)

# View the pairwise comparisons
print(tukey_results)

# Plot the differences to see which pairs' mean difference confidence intervals
# don't include zero
plot(tukey_results, las = 1)

tukey_df <- as.data.frame(tukey_results$r) %>% tibble::rownames_to_column("comparison")

levels <- sprintf("%.1f", seq(0.1, 1.0, by = 0.1))

comparison_order <- combn(
  levels, 2,
  FUN = function(x) paste(rev(x), collapse = "-")
)

# Match TukeyHSD formatting (1 instead of 1.0)
comparison_order <- gsub("1\\.0", "1", comparison_order)

tukey_df$ordered_comparison <- factor(
  tukey_df$comparison,
  levels = comparison_order
)

# Create a forest plot
p_speed_tukey_hsd <- ggplot(tukey_df, aes(x = diff, y = ordered_comparison)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(limits = rev(levels(tukey_df$ordered_comparison))) +
  labs(
    x = "Difference in Mean (95% Family-Wise Confidence Interval)",
    y = "Pairwise Comparison"
  ) +
  theme_bw()


speed_panel <- plot_grid(p_speed_by_convexity, p_speed_tukey_hsd, ncol = 1, rel_heights = c(1,2), labels = c("a.", "b."), label_fontface = "plain", hjust = 0)

ggsave("speed_by_convexity_boxplots_tukey_hsd.png", plot = speed_panel, dpi = 600, width = 183, height = 220, units = "mm")

# So, it seems that 0.1 is significantly different than all the others; 0.2 is sig. different than many of the others
# The differences are not significant among the other parameter values


## Next: moving on to checking sensitivity of final fire perimeter to convexity parameter

# Now that we've done some analysis for how the convexity impacts speed... let's look at how
# it impacts the final fire perimeter

# Let's look at the Jaccard index by ratio value

# We need to compute the Jaccard index for every polygon and its match in the NIROPS data
# (at least for our subset of NIROPS perimeters we want to use to fit the convexity parameter)


fnames <- list.files(path = "sensitivity", pattern = ".geojson", full.names = TRUE)
fnames

perim_data <- lapply(fnames, FUN = st_read)

temp_df <- perim_data[[1]]

unique_id <- unique(temp_df$New_ID)
temp_list <- list()
count <- 1
for (i in 1:length(fnames)){
  temp_df <- perim_data[[i]]
  for (j in 1:length(unique_id)){
    
    event_id <- unique_id[j]
    final_df <- dplyr::filter(temp_df, New_ID == event_id) %>% arrange(summary_date)
    temp_list[[count]] <- final_df[nrow(final_df),]
    count <- count + 1
    
  }
  
  
  
}

# Row bind these final perimeters together
perim_df <- do.call(rbind.data.frame, temp_list)
str(perim_df)

# Next, let's compute the Jaccard index on each of these using the corresponding NIROPS perimeter

r_val <- unique(perim_df$ratio_1)
# So we are going to have for each event... and for each convexity parameter ... calculate the jaccard index
jaccard_list <- list()
count <- 1
for (i in 1:length(unique_id)){
  
  temp_df <- dplyr::filter(perim_df, New_ID == unique_id[i])
  A_df <- dplyr::filter(nirops_df, New_ID == unique_id[i])
  A <- A_df$geometry
  nirops_area <- st_area(A) %>% as.numeric()
  
  for (j in 1:length(r_val)){
    r_df <- dplyr::filter(temp_df, ratio_1 == r_val[j])
    B <- r_df$geometry
    fv_area <- st_area(B) %>% as.numeric()
    jaccard_index <- st_area(st_intersection(A, B))/st_area(st_union(A, B))%>%as.numeric()
    if (length(jaccard_index)==0){
      jaccard_index = 0
    }
    jaccard_list[[count]] <- data.frame(New_ID = unique_id[i], r = r_val[j], jaccard = jaccard_index, N_points = r_df$N_pts, nirops_area, fv_area)
    count <- count + 1
  }
  
}

jaccard_df <- do.call(rbind.data.frame, jaccard_list)

str(jaccard_df)

jaccard_df$jaccard_numeric <- as.numeric(jaccard_df$jaccard)

jaccard_boxplots <- ggplot() + geom_boxplot(data = jaccard_df, mapping = aes(x = as.factor(r), y = jaccard_numeric)) + 
  xlab("Convexity Fraction") +
  ylab("Jaccard Index") +
  theme_bw()


ggplot() + geom_point(data = jaccard_df, mapping = aes(x = N_points, y = jaccard_numeric, color = as.factor(r))) + scale_color_viridis_d()

jaccard_df$r <- jaccard_df$r %>% as.factor()

jaccard_anova <- aov(jaccard_numeric ~ r, data = jaccard_df)

summary(jaccard_anova)

# Run Tukey's Honestly Significant Difference test
jaccard_tukey <- TukeyHSD(jaccard_anova)

# View the pairwise comparisons
print(jaccard_tukey)

# Plot the differences to easily see which pairs don't cross 0
plot(jaccard_tukey, las = 1)

# Clean up Tukey's HSD CI plot
jaccard_tukey_df <- as.data.frame(jaccard_tukey$r) %>% tibble::rownames_to_column("comparison")
jaccard_tukey_df

levels <- sprintf("%.1f", seq(0.1, 1.0, by = 0.1))

comparison_order <- combn(
  levels, 2,
  FUN = function(x) paste(rev(x), collapse = "-")
)

# Match TukeyHSD formatting (1 instead of 1.0)
comparison_order <- gsub("1\\.0", "1", comparison_order)

jaccard_tukey_df$ordered_comparison <- factor(
  jaccard_tukey_df$comparison,
  levels = comparison_order
)



# Create a forest plot
jaccard_tukey_hsd <- ggplot(jaccard_tukey_df, aes(x = diff, y = ordered_comparison)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lwr, xmax = upr), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(limits = rev(levels(jaccard_tukey_df$ordered_comparison))) +
  labs(
    x = "Difference in Mean (95% Family-Wise Confidence Interval)",
    y = "Pairwise Comparison"
  ) +
  theme_bw()


jaccard_tukey_hsd

jaccard_panel <- plot_grid(jaccard_boxplots, jaccard_tukey_hsd, ncol = 1, rel_heights = c(1,2), labels = c("a.", "b."), label_fontface = "plain", hjust = 0)

ggsave("jaccard_by_convexity_boxplots_tukey_hsd.png", plot = jaccard_panel, dpi = 600, width = 183, height = 220, units = "mm")


# Note that here 0.1 is the only one with significantly different results and it does not perform very well
# for low numbers of active fire pixel data. This supports capping the minimum value at 0.2

ggplot() + geom_point(data = jaccard_df, mapping = aes(x = N_points, y = jaccard_numeric, color = as.factor(r))) + scale_color_viridis_d() + xlim(c(0,200))

ggplot() + geom_point(data = jaccard_df, mapping = aes(x = N_points, y = jaccard_numeric, color = as.factor(r))) + scale_color_viridis_d()

# Troubleshooting - where did r = 0.3 and r = 0.7 go in later plots??
# Are they still visible here?

test_df <- dplyr::filter(jaccard_df, r == 0.3 | r == 0.7)

# Still totally there in jaccard_df
ggplot() + geom_point(data = test_df, mapping = aes(x = N_points, y = jaccard_numeric, color = as.factor(r)))

# Collect the maximum jaccard index seen
max_jaccard <- jaccard_df$jaccard_numeric %>% max()

# Collect the maximum number of points
max_n <- jaccard_df$N_points %>% max()

# Let's quickly bin these jaccard indices into 200's [0-200), [200-400), etc.
lb <- seq(0, max_n, by = 200)
ub <- lb + 200
r_vals <- seq(0.1, 1, by = 0.1) %>% round(digits = 1)
#jaccard_df$r <- round(jaccard_df$r, digits = 1)
summary_list <- list()
count <- 1

for (i in 1:length(lb)){
  temp_df <- dplyr::filter(jaccard_df, N_points >= lb[i], N_points < ub[i])
  for (j in 1:length(r_vals)){
    temp_df_by_r <- dplyr::filter(temp_df, r == r_vals[j])
    
    N_fires <- nrow(temp_df_by_r)
    mean_jaccard <- temp_df_by_r$jaccard_numeric %>% mean()
    sd_jaccard <- temp_df_by_r$jaccard_numeric %>% sd()
    r <- r_vals[j]
    
    summary_list[[count]] <- data.frame(N_fires, mean_jaccard, sd_jaccard, r, lb = lb[i], ub = ub[i])
    count <- count + 1
    
    
    
  }
}

summary_df <- do.call(rbind.data.frame, summary_list)

summary_df$x <- as.factor((summary_df$lb + summary_df$ub)/2)

interval_labels <- paste0(
  "[", lb, ", ", ub, ")"
)

names(interval_labels) <- as.character((lb+ub)/2)

interval_labels

npoints_jaccard <- ggplot(data = summary_df) + 
  geom_col(mapping = aes(x = x, y = mean_jaccard, fill = as.factor(r)), position = "dodge", color = 'black', lwd = 0.1) +
  scale_fill_viridis_d(name = "Convexity\nFraction") +
  xlab("Number of Active Fire Detections") +
  ylab("Jaccard Index") +
  scale_x_discrete(labels = interval_labels) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

npoints_jaccard
ggsave("jaccard_by_number_of_points.png", plot = npoints_jaccard, dpi = 600, width = 183, height = 120, units = "mm")

xx <- seq(0, 4700, by = 100)

yy <- 0.6*(1/6)^(xx/1000)

yy2 <- 0.8*(1/8)^(xx/1000)


fx <- vector(length = length(yy))
fx2 <- vector(length = length(yy2))
for (i in 1:length(fx)){
  fx[i] <- max(0.1, yy[i])  
  fx2[i] <- max(0.1, yy2[i])
}

ggplot() + geom_point(data = data.frame(xx, yy, fx), mapping = aes(xx, fx)) + geom_point(data = data.frame(xx, yy2, fx2), mapping = aes(xx, fx2), color = 'red')

# Pull summary data that is the same for all convexity parameters
summary_filter <- dplyr::filter(summary_df, r == 0.1) %>% arrange(x)
summary_filter

summary_filter$cumulative_fires <- cumsum(summary_filter$N_fires)
summary_filter$percent_total <- 100*summary_filter$cumulative_fires/sum(summary_filter$N_fires)

ggplot(data = summary_filter) +
  geom_col(mapping = aes(x = x, y = N_fires)) +
  xlab("Number of Active Fire Detections") +
  ylab("Number of Fire Events") +
  theme_bw()

ggplot(data = summary_filter) +
  geom_col(mapping = aes(x = x, y = percent_total)) +
  ylim(c(0, 101)) +
  xlab("Number of Active Fire Detections") +
  ylab("Cumulative Percent") +
  theme_bw()

# Calculate the Jaccard index for the original data set fires (both MODIS and VIIRS)

modis_df <- st_read("data/perim_polygons_modis.geojson")
viirs_df <- st_read("data/perim_polygons_viirs.geojson")

modis_df <- dplyr::filter(modis_df, New_ID %in% unique_id)
viirs_df <- dplyr::filter(viirs_df, New_ID %in% unique_id)

modis_list <- list()
viirs_list <- list()


for (i in 1:length(unique_id)){
  temp_modis <- dplyr::filter(modis_df, New_ID == unique_id[i]) %>% arrange(desc(summary_date))
  modis_list[[i]] <- temp_modis[1,]
  temp_viirs <- dplyr::filter(viirs_df, New_ID == unique_id[i]) %>% arrange(desc(summary_date))
  viirs_list[[i]] <- temp_viirs[1,]

}

modis_final <- do.call(rbind.data.frame, modis_list)
viirs_final <- do.call(rbind.data.frame, viirs_list)

# Compute Jaccard index for each one

perim_df <- modis_final
jaccard_list <- list()

for (i in 1:length(unique_id)){
  
  temp_df <- dplyr::filter(perim_df, New_ID == unique_id[i])
  A_df <- dplyr::filter(nirops_df, New_ID == unique_id[i])
  A <- A_df$geometry
  nirops_area <- st_area(A) %>% as.numeric()
  
  B <- temp_df$geometry
  fv_area <- st_area(B) %>% as.numeric()
  jaccard_index <- st_area(st_intersection(A, B))/st_area(st_union(A, B))%>%as.numeric()
  if (length(jaccard_index)==0){
    jaccard_index = 0
  }
  jaccard_list[[i]] <- data.frame(New_ID = unique_id[i], jaccard = as.numeric(jaccard_index), nirops_area, fv_area)


  
}

jaccard_modis <- do.call(rbind.data.frame, jaccard_list)



perim_df <- viirs_final
jaccard_list <- list()

for (i in 1:length(unique_id)){
  
  temp_df <- dplyr::filter(perim_df, New_ID == unique_id[i])
  A_df <- dplyr::filter(nirops_df, New_ID == unique_id[i])
  A <- A_df$geometry
  nirops_area <- st_area(A) %>% as.numeric()
  
  B <- temp_df$geometry
  fv_area <- st_area(B) %>% as.numeric()
  jaccard_index <- st_area(st_intersection(A, B))/st_area(st_union(A, B))%>%as.numeric()
  if (length(jaccard_index)==0){
    jaccard_index = 0
  }
  jaccard_list[[i]] <- data.frame(New_ID = unique_id[i], jaccard = as.numeric(jaccard_index), nirops_area, fv_area)
  
  
  
}

jaccard_viirs <- do.call(rbind.data.frame, jaccard_list)

ggplot() + geom_histogram(data = jaccard_viirs, mapping = aes(x = as.numeric(jaccard)))


ggplot() + geom_histogram(data = jaccard_modis, mapping = aes(x = as.numeric(jaccard)))


jaccard_summary <- rbind.data.frame(mutate(jaccard_viirs, instrument = "VIIRS"), mutate(jaccard_modis, instrument = "MODIS"))

ggplot() + geom_histogram(data = jaccard_summary, mapping = aes(x = as.numeric(jaccard), fill = instrument), position = "identity", alpha = 0.5, color = 'black') +
  scale_fill_viridis_d(name = "Instrument")

# Where does the example we chose fall?

example_fire <- dplyr::filter(modis_df, Incid_Name == "RIVER")

ggplot() + geom_sf(data = example_fire, fill = NA, mapping = aes(color = as.factor(New_ID)))

example_id <- 6199 # following the plot cross-referenced with the manuscript

j_vals <- dplyr::filter(jaccard_summary, New_ID == example_id)


ecdf(jaccard_viirs$jaccard)(dplyr::filter(j_vals, instrument == "VIIRS")$jaccard) * 100

ecdf(jaccard_modis$jaccard)(dplyr::filter(j_vals, instrument == "MODIS")$jaccard) * 100

