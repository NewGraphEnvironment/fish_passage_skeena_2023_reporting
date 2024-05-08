source('scripts/packages.R')

# import a geopackage and rearrange then burn to csv
# relative path to the q project form

# name the project directory we are pulling from
dir_gis <- 'sern_skeena_2023'
# define utm zone of study area
utm <- 9


# ------------1. process forms raw from the field-----------------------------------
# # list all the fiss form names in the file
# form_names_l <- list.files(path = paste0('../../gis/',
#                                          dir_gis),
#                            # ?glob2rx is a funky little unit
#                            pattern = glob2rx('*fiss_site*.gpkg'),
#                            full.names = T
# ) %>%
#   purrr::map(sf::st_read) %>%
#   bind_rows() %>%
#   # remove the test
#   filter(gazetted_names != 'Robert Hatch')

# # burn raw csv file to backup folder
# form_names_l %>%
#   readr::write_csv(paste0('data/inputs_extracted/mergin_backups/form_fiss_site_raw_',
#                           format(lubridate::now(), "%Y%m%d"), '.csv'), na = '')
#
## ------burn amalgamated forms to geopackage- SHOULD BE IN QGIS I THINK-------------
# so that work can be done in Q to move sites, remove duplicates, get stream names correct, etc
# form_names_l %>%
#   sf::st_write(paste0('data/inputs_extracted/mergin_backups/form_fiss_site_',
#                       format(lubridate::now(), "%Y%m%d"),'.gpkg'), append=FALSE)


#---------TO DEAL with ISSUE #16 we needed and extra backup version --------------------
# read in the cleaned fiss form and archive
dir.create(paste0('../../gis/', dir_gis, '/data_field/2023/archive'))
form_fiss_site_raw_prep <- sf::st_read(dsn= paste0('../../gis/', dir_gis, '/data_field/2023/form_fiss_site_2023.gpkg')) %>%
  # this should hve bben done but wasn't yet so is put here
  filter(gazetted_names != 'Robert Hatch') %>%
  sf::st_write(paste0('../../gis/', dir_gis, '/data_field/2023/archive/form_fiss_site_',
                      format(lubridate::now(), "%Y%m%d"),'.gpkg'), append=FALSE, delete_dsn = TRUE)



# -------------------read in amalgamated form and clean in---------------------------------
# read in again (from reburned version for reproducability)
form_fiss_site_raw <- sf::st_read(dsn= paste0('../../gis/', dir_gis, '/data_field/2023/archive/form_fiss_site_20231106.gpkg'))

# identify duplicate sites (that are not NAs) as we don't want to input two of the same
dups <- form_fiss_site_raw %>%
  filter(!is.na(local_name)) %>%
  group_by(local_name) %>%
  filter(n()>1)

# if there are dups go back and clean up in gpkg

# clean up the form
form_fiss_site_cleaned <- form_fiss_site_raw %>%
  st_transform(crs = 26900 + utm) %>%
  mutate(
    utm_easting = round(sf::st_coordinates(.)[,1]),
    utm_northing = round(sf::st_coordinates(.)[,2]),
    utm_zone = utm) %>%
  # get a site_id and a location that we can use to make photo directories and tag photos respectively
  tidyr::separate(local_name, into = c('site_id', 'location'), remove = F, extra = "merge") %>%
  #need to rename the photo columns
  dplyr::rename(photo_extra1 = photo_extra_1,
                photo_extra2 = photo_extra_2,
                photo_extra1_tag = photo_extra_1_tag,
                photo_extra2_tag = photo_extra_2_tag,
                photo_typical1 = photo_typical_1,
                photo_typical2 = photo_typical_2) %>%
  # is filter by no date_time necessary?
  # dplyr::filter(!is.na(date_time_start)) %>%
  # split out the date and the time - change type of column first
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
                survey_date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # change "trib" to long version "Tributary"
  mutate(gazetted_names = str_replace_all(gazetted_names, 'Trib ', 'Tributary ')) %>%
  # fill in text columns from spreadsheet that will likely never change
  mutate(waterbody_type = 'stream',
         method_for_channel_width = 'metre tape',
         method_for_wetted_width = 'metre tape',
         method_for_residual_pool_depth = 'metre stick',
         method_for_bankfull_depth = 'metre stick',
         method_for_gradient = 'clinometer',
         method_for_temperature = 'recording meter',
         method_for_conductivity = 'recording meter',
         method_for_p_h = 'pH meter (general)') %>%
  # arrange by surveyor and date/time
  dplyr::arrange(mergin_user, date_time_start) %>%
  #
  dplyr::mutate(comments = case_when(
    !is.na(comments_2) ~ paste0(comments, comments_2, time),
    T ~ paste0(comments, time))) %>%
  # ditch the commments2 and time since we don't need anymore. Time was dropped on gpkg creation due to type conflict
  select(-comments_2, -time) %>%
  #give each row and index so we can filter - note: Nov 2023 - don't see this used currently
  tibble::rowid_to_column()


# Burn cleaned copy to the QGIS project - utm crs with coords -
form_fiss_site_cleaned %>%
  sf::st_write(paste0('../../gis/', dir_gis, '/data_field/2023/form_fiss_site_2023.gpkg'), append=FALSE, delete_dsn = T)


# -----------------read in form after review and finalization in QGIS------------------
# read in the form
form_fiss_site_raw <- sf::st_read(dsn= paste0('../../gis/', dir_gis, '/data_field/2023/form_fiss_site_2023.gpkg')) %>%
  # need to convert date type b/c gpkg and excel import differently
  mutate(survey_date = lubridate::as_date(survey_date))

# Time issue-----------------------------------------------------------------------------------------------------
# Skeena 2023 fiss form has the incorrect time and needs to have 7hrs added to be in PDT. This is a temporary fix.
# First double check the repaired time is correct
fiss_correct_time_check <- form_fiss_site_raw |>
  # there is perhaps a more reliable method for this issue in
  # Seems like  more reliable method way to do would be to use force_tz and with_tz since it takes into account daylight savings time
  # https://github.com/NewGraphEnvironment/fish_passage_skeena_2023_reporting/issues/49
  # I still can't get this method to work, so sticking with what's below for now
  dplyr::mutate(date_time_start_repaired = date_time_start + lubridate::hours(7)) |>
  dplyr::relocate(date_time_start_repaired, .after = date_time_start)

# If correct -  replace - we will replace object to avoid confusion in future when we hopefully don't need this step
form_fiss_site_raw <- fiss_correct_time_check |>
  dplyr::mutate(date_time_start = date_time_start_repaired) |>
  dplyr::select(-date_time_start_repaired)

# see the names of our form
names(form_fiss_site_raw)

# let's get the names of the input template
# there is lots of work to do to pull out all the information we can use so we will start with one small step at a time
# lets just populate the location and site info pages.
form_raw_names_site <- fpr::fpr_import_hab_con(backup = F,
  row_empty_remove = T) %>%
  # pull out just the site info page for now
  pluck(4) %>%
  # only keep the names of the columns
  names()

# location names
form_raw_names_location <- fpr::fpr_import_hab_con(backup = F,
  row_empty_remove = T) %>%
  # pull out just the site info page for now
  pluck(1) %>%
  # only keep the names of the columns
  names()



# we don't want duplicate column names because it messes with them (renames them both) so we need to get rid of dupes
names_dup <- intersect(form_raw_names_site, form_raw_names_location)

# join the names of our two target tables together without (!) the dupes
form_raw_names_sl <- c(form_raw_names_location,
                       form_raw_names_site[!form_raw_names_site %in% names_dup])


# tidy our populated table to PASTE SPECIAL value only!!! to our template. Might need to be in chunks but so be it
form_site_info_prep <- form_fiss_site_raw %>%
  dplyr::select(rowid,
                mergin_user,
                date_time_start,
                dplyr::contains('surveyor'),
                dplyr::any_of(form_raw_names_sl))

# make the loc form
form_fiss_loc <- bind_rows(

  # we need the raw form or we don't have all the right columns
  fpr::fpr_import_hab_con(backup = F,
                          row_empty_remove = T) %>%
    # pull out just the site info page for now
    pluck("step_1_ref_and_loc_info") %>%
    mutate(survey_date = lubridate::as_date(survey_date)) %>%
    slice(0) %>%
    mutate(dewatered_dry_int_channel = as.character(dewatered_dry_int_channel)),

  form_site_info_prep %>%
    sf::st_drop_geometry() %>%
    # alias local name and gazetted_name is not called the same in both sheets so rename
    rename(alias_local_name = local_name,
           gazetted_name = gazetted_names) %>%
    mutate(utm_method = as.character(utm_method)) %>%
    select(rowid, dplyr::any_of(form_raw_names_location))) %>%
  mutate(site_number = dplyr::row_number(),
         reference_number = dplyr::row_number())



# make the site form
form_fiss_site <- bind_rows(

  # we need the raw form or we don't have all the right columns
  a <- fpr::fpr_import_hab_con(backup = F,
    row_empty_remove = T) %>%
    # pull out just the site info page for now
    pluck("step_4_stream_site_data") %>%
    slice(0) %>%
    mutate(feature_height_length_method = as.character(feature_height_length_method),
           utm_method = as.character(utm_method)),

  form_site_info_prep %>%
    sf::st_drop_geometry() %>%
    mutate(morphology = as.character(morphology),
           utm_method = as.character(utm_method)) %>%
    select(rowid,
           dplyr::any_of(form_raw_names_site),
           # add the time to help put the puzzle together after)
           survey_date)
) %>%
  select(rowid, everything())

# burn to file
# The following fields need to be added by hand in the spreadsheet:
# UTM method, No Visible Channel, Waterbody (ID) Identifier (this should be scripted eventually)
form_fiss_loc %>%
  readr::write_csv(paste0(
    'data/inputs_extracted/form_fiss_loc_tidy',
    '.csv'),
    na = '')


form_fiss_site %>%
  readr::write_csv(paste0(
    'data/inputs_extracted/form_fiss_site_tidy',
    '.csv'),
    na = '')

## Next populate step 3 in fish_data_tidy.R then populate step 2 in 0100-extract-inputs.R.
