source('scripts/packages.R')

# this script is going to build a table of features from all phase 2 sites
# there are 3 columns for features in mergin form but only one in the hab con sheet
# easier to do it this way so we can get only the features and utms, required for hab_features object in `outgoing-mapping.R` script

# import geopackages from mergin and rearrange then burn to csv

# name the project directory we are pulling from
dir_project <- 'bcfishpass_skeena_20220823'

# there are tables with no new events and the types are mangels so remove dataframes with only 1 feature
form_names_to_remove <- c('form_fiss_site_202209070609', 'form_fiss_site_202209100809')

# list all the fiss form names in the file
form_names_l <- list.files(path = paste0('../../gis/mergin/',
                                         dir_project),
                           pattern = glob2rx('*site_2022*.gpkg'),
                           full.names = T
) %>%
  stringr::str_subset('form_fiss_site_202209070609', negate = T) %>%
  stringr::str_subset('form_fiss_site_202209100809', negate = T)



# read all the forms into a list of dataframes using colwise to guess the column types
# if we don't try to guess the col types we have issues later with the bind_rows join
form <- form_names_l %>%
  purrr::map(sf::st_read) %>%
  purrr::map(plyr::colwise(type.convert)) %>%
  # name the data.frames so we can add it later as a "source" column - we use basename to leave the filepath behind
  purrr::set_names(nm = basename(form_names_l)) %>%
  bind_rows(.id = 'source')


# see the names of our form
names(form)

# select the columns we want

features <- form %>%
  select(local_name, feature_type, feature_height_m, feature_length_m, feature_height_length_method, feature_time) %>%
  filter(!is.na(feature_type))

features2 <- form %>%
  select(local_name, feature_type_2, feature_height_2_m, feature_length_2_m, feature_height_length_method, feature_time_2) %>%
  filter(!is.na(feature_type_2)) %>%
  rename(feature_type = feature_type_2,
         feature_height_m = feature_height_2_m,
         feature_length_m = feature_length_2_m,
         feature_time = feature_time_2) %>%
  # there was bug in the mergin form where debris jam couldn't be added as feature type so it was included in length column
  mutate(feature_type = case_when(feature_length_m == '2m debris jam' ~ 'LWD Jam', T ~ feature_type)) %>%
  mutate(feature_length_m = case_when(feature_length_m == '2m debris jam' ~ '2', T ~ feature_length_m)) %>%
  mutate(feature_length_m = as.numeric(feature_length_m))

features3 <- form %>%
  select(local_name, feature_type_3, feature_height_3_m, feature_length_3_m, feature_height_length_method, feature_time_3) %>%
  filter(!is.na(feature_type_3)) %>%
  rename(feature_type = feature_type_3,
         feature_height_m = feature_height_3_m,
         feature_length_m = feature_length_3_m,
         feature_time = feature_time_3) %>%
  # there was bug in the mergin form where debris jam couldn't be added as feature type so it was included in height column
  mutate(feature_type = case_when(feature_height_m == '2 log jam' ~ 'LWD Jam', T ~ feature_type)) %>%
  mutate(feature_height_m = case_when(feature_height_m == '2 log jam' ~ '2', T ~ feature_height_m)) %>%
  mutate(feature_height_m = as.numeric(feature_height_m))

features_final <- bind_rows(features, features2, features3) %>%
  mutate(feature_time = lubridate::as_datetime(feature_time, tz="America/Vancouver"))

# get UTMS of features that have times by joining to tracks
# read in tracks
track_points_prep = read_sf('data/habitat_confirmation_tracks.gpx', layer = "track_points")

track_points <- track_points_prep %>%
  st_coordinates() %>%
  as_tibble() %>%
  setNames(c("gps_latitude","gps_longitude")) %>%
  rowid_to_column()

#find the track point that is closest in time to the CreateDate stamp of the photo..
#https://stackoverflow.com/questions/21607247/searching-for-nearest-date-in-data-frame
get_closest_line_in_history <- function(x, history){
  time_diffs <- difftime(x, history)
  time_diffs[time_diffs<0] <- NA

  res <- which.min(time_diffs)
  if (length(res) != 1){
    return(NA)  ##return a NA as in the post
  }else{
    return(res)
  }
}

indx_closest_point <- sapply(features_final$feature_time,
                             get_closest_line_in_history,
                             track_points_prep$time) %>%
  as_tibble()

# closest point corresponds to row id in track points so join dataframes
joined_tracks <- left_join(indx_closest_point, track_points, by = c('value' = 'rowid'))

# tracks are now matched up to photos indexes so bind columns and drop value column
# clean up data frame, waterfall creek shouldn't be in here so filter out, change sterritt creek local names
# change all local names to new pscis ids

features_joined <- bind_cols(features_final, joined_tracks) %>%
  select(-value, -feature_time) %>%
  filter(local_name != 'Waterfall creek') %>%
  mutate(local_name = case_when(local_name == 'Sterritt creek ds' ~ '198225', T ~ local_name)) %>%
  filter(!is.na(gps_latitude))


#add Owen Creek features because they were from last year and not in mergin forms


# burn to file

form_features %>%
  readr::write_csv(paste0(
    'data/dff/features_tidy_',
    format(lubridate::now(), "%Y%m%d"),
    '.csv'),
    na = '')
