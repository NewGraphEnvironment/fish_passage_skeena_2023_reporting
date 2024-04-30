##here we need to pull all the metadata from all the marked photos so we can use it to have our photos show on the leaflet map
## NOTE: this script needs to be re run if photos are deleted or new ones added
# source('R/packages.R')

# define your project repo name b/c this is not
repo_name <- 'fish_passage_skeena_2023_reporting'

photo_metadata_prep <- exifr::read_exif('data/photos',recursive=T)  %>%
  janitor::clean_names() %>%
  select(file_name, source_file, create_date, gps_latitude, gps_longitude) %>%
  mutate(url  = paste0('https://github.com/NewGraphEnvironment/', repo_name, '/raw/main/',
                       source_file)) %>%
  # filter photos used in hab con site memos, but do not include photos used for pscis phase 2 submission portal as we don't want to clutter map
  # portal photos have been labelled '_k_nm' to distinguish them, they are still committed to repo
  filter(
    file_name %like% '_k_'& !file_name %like% '_nm_'
  ) %>%
  mutate(create_date = lubridate::as_datetime(create_date, tz="America/Vancouver"))

# MW photos aren't georeferenced so link them to tracks
# read in tracks
track_points_prep = read_sf('data/habitat_confirmation_tracks.gpx', layer = "track_points")

track_points <- track_points_prep %>%
  st_coordinates() %>%
  as_tibble() %>%
  setNames(c("gps_longitude","gps_latitude")) %>%
  rowid_to_column()

mw_photos <- photo_metadata_prep %>%
  filter(source_file %like% 'TimePhoto') %>%
  select(file_name, source_file, create_date) %>%
  mutate(create_date = lubridate::as_datetime(create_date, tz="America/Vancouver"))

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

indx_closest_point <- sapply(mw_photos$create_date,
                             get_closest_line_in_history,
                             track_points_prep$time) %>%
  as_tibble()

# closest point corresponds to row id in track points so join dataframes
joined_tracks <- left_join(indx_closest_point, track_points, by = c('value' = 'rowid')) %>%
  mutate(gps_latitude = as.character(gps_latitude)) %>%
  mutate(gps_longitude = as.character(gps_longitude))

# tracks are now matched up to photos indexes so bind columns and drop value column
photo_metadata_processed <- bind_cols(mw_photos, joined_tracks) %>%
  select(-value) %>%
  mutate(url  = paste0('https://github.com/NewGraphEnvironment/', repo_name, '/raw/main/',
                       source_file))

photo_metadata <- filter(photo_metadata_prep, !source_file %like% 'TimePhoto') %>%
  bind_rows(photo_metadata_processed) %>%
  select(-create_date)




conn <- rws_connect("data/bcfishpass.sqlite")
rws_list_tables(conn)
rws_drop_table("photo_metadata", conn = conn) ##now drop the table so you can replace it
rws_write(photo_metadata, exists = F, delete = TRUE,
          conn = conn, x_name = "photo_metadata")
rws_list_tables(conn)
rws_disconnect(conn)

