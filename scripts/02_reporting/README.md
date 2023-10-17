Workflows for putting the data in one place to build the report and figures (ex. hydrometric graphs).  Additional processing for datasets to get ready for reporting include:


# 0160-load-bcfishpass-data.R
  *  Grab the modeling outputs for our watershed group of interest from postgres built from bcfishpass and burn to a local sqlite to provide reproducible snapshot in time. Things change alot as modelling is based on certain data and assumptions.  Also, grab the bcfishpass [spawning and rearing parameters](https://github.com/smnorris/bcfishpass/tree/main/02_model) table and put in local sqlite database so it can be used to populate the methods and tie to the references table.

  
# 0170-load-wshd_stats.R

  * Derive watershed areas upstream of Phase 2 sites using fwapg.
  * Derive watershed statistics for watershed areas using `elevatr` to download rasters and `raster` to process. 
  * Use poisspatial to derive elevation for stream crossing site locations and joint to watershed stats. Save to sqlite db.
  

# analyze_fish.R  
  * Build graph that shows the breakdown of fish observations.



# extract-fiss-species-table.R
  * Build the fiss species table and join to species at risk data.


# 0180-photos-extract-metadata.R
  * Extract photo metadata so we can display the photos from the reporting on the interactive map.
