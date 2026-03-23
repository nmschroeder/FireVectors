# FireVectors Data Sets

Please obtain the following files from their temporary locations below

* Fire progression data locations
  * [perim_polygons_viirs.geojson for VIIRS](https://o365coloradoedu-my.sharepoint.com/:u:/g/personal/nihe1301_colorado_edu/IQDK0-1RTt3SR7ytZoAViiVGAXXZXOtV820F3xUBMXFt3EM?e=crfblY)
  * [perim_polygons_modis.geojson for MODIS](https://o365coloradoedu-my.sharepoint.com/:u:/g/personal/nihe1301_colorado_edu/IQBtTiBAQc_6R6f98cWC8V0jAb3Fm3nSA85ve3IxXa4F268?e=ynPm0n)
  * Description of fields
  * summary_date
    * The first date and time (UTC) of VIIRS or MODIS detections for a given overpass
    * Swaths can take several minutes to complete, so this is the initial time stamp
    * The remaining active fire detections for a given overpass are assigned to this time stamp
   * SATELLITE, INSTRUMENT, VERSION, DAYNIGHT
    * These are fields from FIRMS indicating the satellite, instrument, version, and day (D) vs. night (N) indicator
   *  N_pts
    * The number of active fire pixel detections used to create the current perimeter
   * perim_avail
    * A TRUE/FALSE flag to indicate whether or not a perimeter is available at this time step
   * event_perim
    * An integer to number each fire progression for a given event from the first (1) to the last (N)
   * Incid_Name, Ig_Date
    * Fields from MTBS indicating the incident name and ignition date
   * New_ID
    * An ID unique to this study assigned to each event within the filtered MTBS data set for the Western United States (2002-2024)
   * geometry
    * The geometry corresponding to the current fire progression        
     
* Fire vector data locations
  * [burn_direction_viirs.csv for VIIRS](https://o365coloradoedu-my.sharepoint.com/:x:/g/personal/nihe1301_colorado_edu/IQAknOKte3T7TLfELx1ys4zuAeP1xaP7I6Hm2GlKbhNAAPY?e=ZdYRCD)
  * [burn_direction_modis.csv for MODIS](https://o365coloradoedu-my.sharepoint.com/:x:/g/personal/nihe1301_colorado_edu/IQDUPZ66g6DxTZBxYILYCKT0Afv_kEyppv6nP-qn25Sf_U0?e=efYQEq)
  * Description of fields
  * x1, y1, x2, y2
    * The starting (x1, y1) and ending (x2, y2) points of the speed vector for a given time step and polygon within an event's fire progression
    * The coordinate reference system for the end points is EPSG: 5070
    * Note: a fire progression at a given time step may be represented as a multipolygon object if there are multiple areas burning at a time. In this case, there may be multiple speed vectors.
   * time_start, time_end, time_mean
    * time_start is the starting time, which is the at (x1, y1)
    * time_end is the ending time, which is the time at (x2, y2)
    * time_mean is the average of time_start and time_end
    * These times are in UTC
   * dt
    * The time difference between the starting and ending points in hours
   * d_xy
    * The 2D Euclidean distance traveled between (x1, y1) and (x2, y2) in km
   * v_xy
    * The fire speed estimate from dividing d_xy by dt (km h$^{-1}$)
   * d_elev
    * An estimate of the distance traveled along the terrain below the speed vector
    * This uses a numerical approximation for the arc length formula
    * We use the LANDFIRE 30-m digital elevation model for the terrain
   * v_elev
    * The corresponding estimate for fire speed that incorporates the terrain below the speed vector (km h$^{-1}$)
   * dA
    * The change in area for a given polygon within the current fire progression in km$^{2}$
    * Some progressions may have multiple polygons associated with distinct fires within the progression
    * In this case, there is a row for each polygon within the progression with its own fire speed and fire growth rate
   * dAdt
    * The fire growth rate (km$^{2}$ h$^{-1}$) estimated by dividing dA by dt
   * Incid_Name, Incid_Type, and Ig_Date
    * Fields from MTBS indicating the incident name, the incident type (wildfire or prescribed), and the ignition date
   * New_ID
    * An ID unique to this study assigned to each event within the filtered MTBS data set for the Western United States (2002-2024)

