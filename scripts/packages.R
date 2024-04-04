# install.packages('pak')

pkgs_cran <- c(
  #'plotKML', #this takes forever to load so going to leave it out for now
  'raster', #load this dog before dplyr yo
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'kableExtra',
  'httr',
  # 'RPostgres',
  # 'RPostgreSQL',
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
  'rayshader',
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
  "poissonconsulting/fishbc"
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

