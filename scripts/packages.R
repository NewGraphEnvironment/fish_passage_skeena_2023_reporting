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
  'RPostgres',
  'RPostgreSQL',
  'DBI',
  'magick',
  'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'lubridate',
  'forcats',
  'bookdown',
  'fasstr',
  'tidyhydat',
  'elevatr',
  'rayshader',
  'geojsonio',
  'english',
  'leaflet.extras',
  'ggdark',
  'pdftools',
  'chron',
  'leafpop',
  'exifr',
  'chron',
  'pagedown',
  'devtools'
  # geojsonsf,
  # bit64 ##to make integer column type for pg
  # gert  ##to track git moves
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

# for a fresh install of R
# lapply(package_list,
#        install.packages,
#        character.only = TRUE)

# we need the development version of pagedown as of 20200303 https://github.com/rstudio/pagedown/issues/265
# remotes::install_github('rstudio/pagedown')


# custom package
# devtools::install_github("NewGraphEnvironment/fpr"
#                          ,ref="main"
#                          ,auth_token = git_token
# )
