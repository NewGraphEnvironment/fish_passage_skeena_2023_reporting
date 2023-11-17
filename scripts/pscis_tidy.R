source('scripts/packages.R')

#--------------------process forms raw from field---------------------------

# name the project directory we are pulling from
dir_project <- 'sern_skeena_2023'

# pull out utm coordinates, set utm zone but check to make sure all data falls in one zone
utm <- 9

# import form_pscis.gpkg direct from mergin and then import form_pscis_2023 and bind rows
# NOTE : this step only needs to be done when new crossing data is added to the project,
# in this case, Andy and Alicia from GWA added new data

# form_pscis_field <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis.gpkg')) %>%
#   filter(mergin_user == "andyr")
#
# form_pscis_working <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg'))
#
# # check to see that column names are equiv (must be if number is the same but still)
# identical(names(form_pscis_field), names(form_pscis_working))
#
# # bind rows
# form_prep <- bind_rows(
#   form_pscis_field,
#   form_pscis_working
# ) %>%
#   st_transform(crs = 26900 + utm) %>%
#   poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') %>%
#   # add in utm zone of study area
#   mutate(utm_zone = utm)
#
# # burn to backup folder as csv
# form_prep %>%
#   readr::write_csv(paste0('data/inputs_extracted/mergin_backups/form_pscis_raw_',
#         format(lubridate::now(), "%Y%m%d"), '.csv'))
#
# # re burn to working geopackage in Q project
# form_prep %>%
#   st_as_sf(coords = c('easting', 'northing'), crs = 26900 + utm, remove = F) %>%
#   # convert back to project crs
#   st_transform(crs = 3005) %>%
#   sf::st_write(paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg'), append=F, delete_dsn=T)

#---------------------pscis clean and QA only--------------------------

form_pscis <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg')) %>%
  # assumes in albers
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]) %>%
  # then grab the utms. fragile since relies on having only 1 utm zone. there
  # are functions somewhere to deal with this (can't remember which repo though)
  st_transform(crs = 26900 + utm) %>%
  mutate(
    easting = sf::st_coordinates(.)[,1],
    northing = sf::st_coordinates(.)[,2]) %>%
  # add in utm zone of study area
  mutate(utm_zone = utm) %>%
  # not sure we need to but turn non-spatial
  sf::st_drop_geometry()

# NOTE: once the data clean is run once it does not need to be repeated, if changes are made in Q just read in the file above,
# and then skip to the re burn to geopackage and csv step

# check for duplicates
form_pscis %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  filter(n()>1)

form_prep1 <- form_pscis %>%
  #split date time column into date and time
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
                date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # filter out to get only the records newly created
  filter(!is.na(date_time_start)) %>%
  mutate(
    site_id = case_when(is.na(pscis_crossing_id) ~ my_crossing_reference,
                        T ~ pscis_crossing_id)
  ) %>%
  # remove the form making site
  filter(site_id != '12345') %>%
  arrange(site_id)

# clean up data fields to make copy and paste to prov template easier
form_prep2 <- form_prep1 %>%
  # some columns that have yes/no answers have NA values in mergin, need to change to No
  # need to add 'No' as default values to mergin
  mutate(across(contains('yes_no'), ~replace_na(.,'No'))) %>%
  # some numeric fields for CBS have NA values when a user input 0
  mutate(across(c(outlet_drop_meters, outlet_pool_depth_0_01m, culvert_slope_percent, stream_slope),
                ~case_when(crossing_type == 'Closed Bottom Structure' ~replace_na(.,0),
                           TRUE ~ .
                ))) %>%
  # change 'trib' to long version 'Tributary'
  mutate(stream_name = str_replace_all(stream_name, 'Trib ', 'Tributary ')) %>%
  # change 'Hwy' to 'Highway'
  mutate(road_name = str_replace_all(road_name, 'Hwy ', 'Highway '))

form_pscis_cleaned <- form_prep2 %>%
  # add new assessment comment column to preserve reproducability
  mutate(assessment_comment_og = assessment_comment) %>%
  # append moti ids to comments, differentiate between highway major structure, and add time to end
  mutate(assessment_comment = case_when(
    moti_chris_culvert_id > 1000000 ~ paste0(assessment_comment, ' Ministry of Transportation chris_culvert_id: ', moti_chris_culvert_id, '.'),
    T ~ assessment_comment),
    assessment_comment = case_when(
      moti_chris_culvert_id < 1000000 ~ paste0(assessment_comment, ' Ministry of Transportation chris_hwy_structure_road_id: ', moti_chris_culvert_id, '.'),
      T ~ assessment_comment),
    assessment_comment = paste0(assessment_comment, ' ', time)
  ) %>%
  # ditch time column
  select(-time)

# burn cleaned copy to QGIS project gpkg, the pscis clean section can be repeated again when changes are made in Q

form_pscis %>%
  st_as_sf(coords = c('easting', 'northing'), crs = 26900 + utm, remove = F) %>%
  # convert back to project crs
  st_transform(crs = 3005) %>%
  sf::st_write(paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg'), append=F, delete_dsn=T)

# burn to version controlled csv, so changes can be viewed on git

form_pscis %>%
  readr::write_csv(paste0('data/dff/form_pscis_2023.csv'), na='')

#---------------------pscis export only--------------------------

# in this section we will read in cleaned form from Q after review and finalization,
# and then get the names of the input template so we can copy and past special directly into the spreadsheet

# read in form from Q
form_pscis <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg')) %>%
  st_drop_geometry()

# this is a table that cross references column names for pscis table and has the columns in the same order as the spreadsheet
xref_names_pscis <- fpr::fpr_xref_pscis

# get order of columns as per the excel template spreadsheet
# this can be used as a select(all_of(name_pscis_sprd_ordered)) later
# to order columns for the field form and/or put the field entered table in order
name_pscis_sprd_ordered <- fpr::fpr_xref_pscis %>%
  filter(!is.na(spdsht)) %>%
  select(spdsht) %>%
  pull(spdsht)

# see names that coincide between the xref table and what we have
intersect(name_pscis_sprd_ordered, names(form_pscis))

# see which are different
setdiff(name_pscis_sprd_ordered, names(form_pscis))
# order matters
setdiff(names(form_pscis), name_pscis_sprd_ordered)

# to use all the columns from the template first we make an empty dataframe from a template
template <- fpr::fpr_import_pscis() %>%
  slice(0)

# then we join it to our populated spreadsheet
# we may as well keep all the columns that are not in the spreadsheet and append to the end
form <- bind_rows(
  template,
  form_pscis
) %>%
  # only select columns from template object
  select(any_of(names(template))) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  select(-stream_width_ratio:-barrier_result) %>%
  # then arrange it by pscis id to separate phase 1s from reassessments
  arrange(pscis_crossing_id, date)

# burn to a csv ready for copy and paste to template
form %>% readr::write_csv(paste0(
  'data/dff/pscis_export.csv'), na='')

# --------------------moti climate change ---------------------------
#
# moti_names <- setdiff(names(form_pscis), name_pscis_sprd_ordered) %>%
#   enframe(name = NULL, value = 'spdsht') %>%
#   mutate(report = str_to_title(spdsht),
#          report = stringr::str_replace_all(report, '_', ' '),
#          report = stringr::str_replace_all(report, 'event affecting culvert', ''),
#          report = stringr::str_replace_all(report, 'id', 'ID'),
#          report = stringr::str_replace_all(report, 'Gps', 'GPS'),
#          report_include = case_when(
#            str_detect(spdsht,
#                       'photo|long|lat|mergin|surveyor|gps|width|utm|time|source|camera|aggregated|rowid|geometry') ~ F,
#            T ~ T
#          ),
#          id_join = NA_integer_,
#          id_side = NA_integer_) %>%
#   filter(spdsht != 'stream_width_ratio_score')
# # filter(report_include == T)
#
# # burn out to csv so we can manually do the descriptions
# moti_names %>%
#   write_csv('data/inputs_raw/moti_climate.csv')
#
# # we want to get our climate change risk information summarized for each site.
# # if we were to add a tag to the names or a xref tie that tells us if each column is part of this or not we would get ahead...
#
# ##-------------------- moti correct chris_culvert_id-----------------------
# # one thing we definitely need to do it get the chris_culvert_id for each site as we used the wrong one on our forms. We should be able to cross ref the ids from bcdata
# # so let's try that first
#
#
# # get_this <- bcdata::bcdc_tidy_resources('ministry-of-transportation-mot-culverts') %>%
# #   filter(bcdata_available == T)  %>%
# #   pull(package_id)
# #
# # dat <- bcdata::bcdc_get_data(get_this)
# #
# # moti_raw <- dat %>%
# #   purrr::set_names(nm = janitor::make_clean_names(names(dat)))
# #
# # # match our sites to ids
# # moti <- left_join(
# #   form_prep,
# #
# #   moti_raw %>% select(culvert_id, chris_culvert_id) %>% sf::st_drop_geometry(),
# #
# #   by = c('mot_culvert_id' = 'culvert_id')
# # )
#
# # the names must have changed so lets use the file in the mergin project as we know that one is the same
# moti_raw <- sf::st_read('../../gis/mergin/bcfishpass_skeena_20220823/clipped_moti_culverts_sp.gpkg') %>%
#   sf::st_drop_geometry()
#
# moti <- left_join(
#   form_prep,
#
#   moti_raw %>% select(culvert_id, chris_culvert_id),
#
#   by = c('mot_culvert_id' = 'culvert_id')
# )
#
# # burn to a csv
# moti %>%
#
#
#   #sort data by date
#   arrange(date) %>%
#
#   readr::write_csv(paste0(
#     'data/dff/form_pscis_moti_',
#     format(lubridate::now(), '%Y%m%d'),
#     '.csv'))

