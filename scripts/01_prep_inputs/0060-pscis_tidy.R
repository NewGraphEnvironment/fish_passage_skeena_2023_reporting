# Scripts to prepare data for QA in QGIS

source('scripts/packages.R')

#--------------------process forms raw from field---------------------------

# name the project directory we are pulling from
dir_project <- 'sern_skeena_2023'

# pull out utm coordinates, set utm zone but check to make sure all data falls in one zone
utm <- 9

# Read in the form_pscis.gpkg from mergin, this step can probably be simplied with fpr_sp_assign_utm()
form_pscis <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis.gpkg')) %>%
  # this is no longer necessary
  mutate(
    x = sf::st_coordinates(.)[,1],
    y = sf::st_coordinates(.)[,2]) %>%
  # then grab the utms. fragile since relies on having only 1 utm zone. there
  # are functions somewhere to deal with this (can't remember which repo though)
  st_transform(crs = 26900 + utm) %>%
  # this is no longer necessary
  mutate(
    easting = sf::st_coordinates(.)[,1],
    northing = sf::st_coordinates(.)[,2]) %>%
  # add in utm zone of study area
  # this is no longer necessary
  mutate(utm_zone = utm) %>%
  # not sure we need to but turn non-spatial
  sf::st_drop_geometry()

#---------------------pscis clean and QA only--------------------------


# check for duplicates
form_pscis %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  filter(n()>1)

# check for sites that have a culvert length over 99.9 or a fill depth over 9.9,
# anything over this will cause error in submission sheet
# we can code in these changes in the future
form_pscis %>%
  filter(length_or_width_meters>99.9|fill_depth_meters>9.9)

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
  # IN FUTURE WILL BE ADDRESSED HERE HOPEFULLY https://github.com/NewGraphEnvironment/dff-2022/issues/119
  mutate(across(contains('yes_no'), ~replace_na(.,'No')),
         across(c(outlet_drop_meters, outlet_pool_depth_0_01m, culvert_slope_percent, stream_slope),
                ~case_when(crossing_type == 'Closed Bottom Structure' ~replace_na(.,0),
                           TRUE ~ .
                )),
         stream_name = str_replace_all(stream_name, 'Trib ', 'Tributary '),
         road_name = str_replace_all(road_name, 'Hwy ', 'Highway '))

# add in which phase of assessment the site is in, did the reassessment sites by hand (because there was only 3-4) so thats not in here.
# This only works when only phase 1 sites have a priority ranking, could use some updating but works for now.
# THIS IS A TEMPORARY FIX, THE PRIORITY RANKING SHOULD BE IN THE FORM and FOR THIS USE CASE WE SHOULD BE PASSING A LIST
# OF SITES TO THE FUNCTION TO DETERMINE phase1, reassessment, phase2
form_pscis_cleaned <-  form_prep2 %>%
  mutate(source = case_when(
    my_priority == 'phase 2' ~ 'phase2',
    my_priority == 'high' ~ 'phase1',
    my_priority == 'medium' ~ 'phase1',
    my_priority == 'low' ~ 'phase1',
    is.na(my_priority) ~ 'phase1',
    T ~ source))

# THIS STEP SEEMS  ODD SINCE THIS FUNCTION GRABS OUR RAW FORM FROM Q AND BACKS IT UP.....
# I THINK THIS SHOULD BE A write_csv call to BURN THE FORM FROM 'QGIS/form_pscis.gpkg' TO
# QGIS/data_field/2023/form_pscis_2023.gpkg
# burn cleaned copy to QGIS project gpkg, and csv and Rdata to data/backup for version control using git
form_pscis_cleaned <- fpr_sp_gpkg_backup(
  path_gpkg = paste0("~/Projects/gis/", dir_project, '/data_field/2023/form_pscis_2023.gpkg'),
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = TRUE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = TRUE)
