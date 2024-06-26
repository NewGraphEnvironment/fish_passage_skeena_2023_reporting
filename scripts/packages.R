# if required install pak
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}


pkgs_cran <- c(
  #'plotKML', #this takes forever to load - and doesn't work due to rgdal depends so going to leave it out for now
  'rmarkdown',
  'raster', #load this dog before dplyr and bcdata to avoid conflicts (filter and select)
  'bcdata',
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'httr',
  'RPostgres',
  'DBI',
  'magick',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'lubridate',
  'bookdown',
  'fasstr',
  'tidyhydat',
  'elevatr',
  'english',
  'leaflet.extras',
  'ggdark',
  'pdftools',
  'chron',
  'leafpop',
  'exifr',
  'pagedown',
  'devtools',
  "geojsonio",
  "fs",
  "fishbc"
)

pkgs_gh <- c(
  # specific version we know gives white captions when rendered in dark mode....
  "haozhu233/kableExtra@a9c509a",
  "poissonconsulting/fwapgr",
  "newgraphenvironment/fpr",
  "newgraphenvironment/rfp",
  'poissonconsulting/poisspatial'
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)

# install or upgrade all the packages with pak
if(params$update_packages){
  lapply(pkgs_all,
         pak::pkg_install,
         ask = FALSE)
}

# load all the packages
pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

