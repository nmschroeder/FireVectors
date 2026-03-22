# FireVectors
Repository for the FireVectors algorithm, data sets, and analyses

# Fire progressions and speed data sets
The primary products within FireVectors are the fire progression perimeters and associated speed vectors.

* Fire progression data locations
  * data/perim_polygons_viirs.geojson for VIIRS
  * data/perim_polygons_modis.geojson for MODIS

* Fire vector data locations
  * data/burn_direction_viirs.csv for VIIRS
  * data/burn_direction_modis.csv for MODIS

# Scripts for manuscript figures, tables, and analyses
The following is a guide of where to find the script to generate each figure, table, and analysis completed in R.

* Figure 1. Speed algorithm for fire progression perimeters
  * Created by Nathaniel Hofford using Canva
* Figure 2. Top ten fastest fires across the Western United States
  * Script: top_ten_fastest_fires.R
  * Figure: top_ten_fastest_fires.png
* Figure 3. Maximum fire speed by EPA Level III ecoregion and map of fastest fires
  * Script: fire_behavior_by_ecoregion
  * Figure: ecoregion_maps_fastfires_speed.png
* Figure 4. Time series of 95th-percentile fire speed and fire growth rate (2002-2024)
  * Script: fire_behavior_by_year.R
  * Figure: fire_trends_panel.png 
* Figure 5. Mean and trends in maximum fire speed per fire event by ecoregion
  * Script: fire_behavior_by_ecoregion.R
  * Figure: ecoregion_maps_speed.png 
* Figure 6. Boxplots of fire speed by primary vegetation type
  * Script: fire_speed_by_vegetation_type_part2.R
  * Figure: speed_by_vegetation_class_boxplots.png
  * Note: The associated script also contains the Kruskal-Wallis and Wilcox tests
* Figure 7. Relationship between log fire speed and log fire growth rate by primary vegetation type
  * Script: fire_speed_vs_FGR_comparison.R
  * Figure: log-log_plots_fgr_by_speed_modis.png
* Figure 8. Comparison of fire event metrics derived from MODIS and VIIRS
  * Script: modis_vs_viirs_comparison.R
  * Figure: modis_vs_viirs_comparison_panel.png
* Figure 9. Comparison of selected published geospatial data sets for the 2018 Camp Fire
  * Script: dataset_comparison.R
  * Figure: dataset_comparison_panel.png
* Table 1. Number of fires searched and analyzed from MTBS with respect to Aqua MODIS and Suomi NPP VIIRS
  * Script: top_ten_fastest_fires.R
* Table 2. Number of fires in grasslands, shrublands, and forests for MTBS fires searched, MTBS fires with active fire pixel-derived perimeters, and MTBS fires with speed estimates from Aqua MODIS and Suomi NPP VIIRS
  * Script: fire_speed_by_vegetation_type_part1.R 
* Table 3. Mean, median, 75th- and 95th-percentile fire speed (km h-1) by primary vegetation type along each fire’s maximum speed vector
  * Script: fire_speed_by_vegetation_type_part2.R
* Table 4. Ordinary least squares parameters for the log-log relationships shown in Figure 7
  * Script: fire_speed_vs_FGR_comparison.R 
* Figure S1. FireVectors algorithm
  * Created by Nicole Hemming-Schroeder using draw.io
* Figure S2. Maximum fire growth rate by EPA Level III ecoregion and map of fastest fires
  * Script: fire_behavior_by_ecoregion.R
  * Figure: ecoregion_maps_fastfires_fgr.png
* Figure S3. Mean and trends in maximum fire growth rate per fire event by ecoregion
  * Script: fire_behavior_by_ecoregion.R
  * Figure: ecoregion_maps_fgr.png 
* Table S1. Number of fires and 95th-percentile speed by ecoregion and NLCD vegetation type
  * fire_speed_by_vegetation_type_part1.R

# Initial set up and FireVectors algorithm files

* Initial set up: prepare_mtbs_western_us.R
  * Inputs
    * MTBS fire boundaries
    * United States Tiger Line state shapefile
    * FIRMS archived active fire detection data for VIIRS and MODIS
  * Outputs
    * MTBS fire boundaries for time period of overlap with FIRMS data 
    * Western United States boundary file
    * FIRMS data clipped to Western United States
 * FireVectors, part 1: HPC/fire_vectors_01.R
    * Create fire progressions and speed vectors
    * Designed to run as a task array with one fire event per task
 * FireVectors, part 2: HPC/fire_vectors_02.R
    * Collates perimeter and speed vector data from FireVectors, part 1 
 * FireVectors, part 3: HPC/fire_vectors_03.R
    * Collates active fire detection data labeled by fire event from FireVectors, part 1

