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
  * Script: fire_behavior_by_ecoregion.R
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
* Figure 9. Comparison of fire speed derived from MODIS to fire speed derived from NIROPS
  * Script: nirops_comparison.R
  * Figure: modis_nirops_comparison.png
* Figure 10. Comparison of selected published geospatial data sets for the 2018 Camp Fire
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

# Initial set up and running FireVectors algorithm

* Initial set up: prepare_mtbs_western_us.R
  * Inputs
    * MTBS burned area boundaries data set (https://www.mtbs.gov/direct-download)
    * United States Tiger Line state shapefile (https://www2.census.gov/geo/tiger/TIGER2025/STATE/)
    * FIRMS archived active fire detection data for VIIRS and MODIS (https://firms.modaps.eosdis.nasa.gov/download/)
  * Outputs
    * MTBS fire boundaries for time period of overlap with FIRMS data
      * mtbs_wus_2002_2024.shp and ancillary files 
    * Western United States boundary file
      * western_us.shp and ancillary files 
    * FIRMS data clipped to Western United States
      * shapefiles beginning in western_us_fire_archive
 * FireVectors, part 1: HPC/fire_vectors_01.R
    * Create fire progressions and speed vectors
    * Designed to run as a task array with one fire event per task
 * FireVectors, part 2: HPC/fire_vectors_02.R
    * Collates perimeter and speed vector data from FireVectors, part 1 
 * FireVectors, part 3: HPC/fire_vectors_03.R
    * Collates active fire detection data labeled by fire event from FireVectors, part 1
 * NIROPS speed comparison, part 1: HPC/nirops_mtbs_match.R
    * Matches NIROPS-based fire progressions from Magstadt et al. (In Review) to MTBS perimeters by IrwinID 
 * NIROPS speed comparison, part 2: HPC/fire_vectors_01_daily.R
    * Computes fire speed between subsequent nighttime Terra MODIS fire progressions to get closest temporal alignment with NIROPS progressions
 * NIROPS speed comparison, part 3: HPC/fire_vectors_02_daily.R
    * Collates results of fire_vectors_01_daily.R into one data frame (burn_direction_daily.csv)
 * NIROPS speed comparison, part 4: HPC/fire_vectors_01_nirops.R
    * Computes fire speed between subsequent NIROPS fire progressions occurring between 0100 and 0900 UTC, inclusive
 * NIROPS speed comparison, part 5: HPC/fire_vectors_02_nirops.R
    * Collates results of fire_vectors_01_nirops.R into one data frame (burn_direction_nirops.csv)

# Downloading ancillary files for figures, tables, and analyses
 * Please download LC20_Elev_220.tif elevation file from LANDFIRE and place in FireVectors directory
 * Create MTBS directory inside FireVectors directory
   * Place mtbs_wus_2002_2024.shp (and supporting files ending in .prj, .shx, and .dbf) from prepare_mtbs_western_us.R in the MTBS directory (MTBS data clipped to the Western US and time frame of active fire detection data)
 * Create FIRMS directory inside FireVectors
   * Place files beginning with western_us_fire_archive created with prepare_mtbs_western_us.R in the FIRMS directory (FIRMS data clipped to the Western US)
 * Create ecoregions directory inside the FireVectors directory
   * Download Environmental Protection Agency Ecoregion Level III files beginning in us_eco_l3.* and place them there
 * Create NLCD directory inside the FireVectors directory
   * Download annual NLCD data for 2001 through 2024 and place inside (example file for 2001: Annual_NLCD_LndCov_2001_CU_C1V1.tif)
 * Create TIGER directory inside the FireVectors directory
   * Place shapefile files beginning in western_us.* generated from prepare_mtbs_western_us.R in the TIGER directory (Western US shapefile)
 * Create NIROPS directory inside the FireVectors directory
   * Download NIROPS shapefiles from Magstadt et al. (In Review) using the reserved DOI when available
   * doi:10.17632/95rj5d379g.1 
