# install.packages('pak')

pkgs_cran <- c(
  #'plotKML', #this takes forever to load - and doesn't work due to rgdal depends so going to leave it out for now
  'raster', #load this dog before dplyr
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
  'bcdata',
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
  'devtools'
)

pkgs_gh <- c(
  "newgraphenvironment/fpr",
  "poissonconsulting/fwapgr",
  'poissonconsulting/poisspatial',
  "poissonconsulting/fishbc",
  "haozhu233/kableExtra"
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)

# install or upgrade all the packages with pak
lapply(pkgs_all,
       pak::pkg_install, ask = FALSE)

# load all the packages
pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)

