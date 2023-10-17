# install.packages('pacman')

pacman::p_load(
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
  'fwapgr',
  'citr',
  'pdftools',
  'pacman',
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

# for a fresh install of R
# lapply(package_list,
#        install.packages,
#        character.only = TRUE)

# we need the development version of pagedown as of 20200303 https://github.com/rstudio/pagedown/issues/265
# remotes::install_github('rstudio/pagedown')


pacman::p_load_gh("poissonconsulting/fwapgr",
                  'poissonconsulting/poisspatial',
                  "crsh/citr",
                  "poissonconsulting/fishbc",
                  "newgraphenvironment/fpr",
                  "paleolimbot/rbbt")
# "poissonconsulting/subfoldr2")


# custom package
# devtools::install_github("NewGraphEnvironment/fpr"
#                          ,ref="main"
#                          ,auth_token = git_token
# )
