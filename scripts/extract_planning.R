# read in the data, add the columns we want and burn to mergin for processing
source('scripts/packages.R')


dir_project <- 'sern_skeena_2023'

conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
planning_raw <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)
pscis_raw <- readwritesqlite::rws_read_table("pscis", conn = conn) %>%
  sf::st_drop_geometry()

unique(planning_raw$utm_zone)



planning <- left_join(

  planning_raw %>%
    st_as_sf(coords = c('utm_easting', 'utm_northing'), crs = 26909, remove = F) %>%
    st_transform(crs = 3005),


  planning_raw2 <- left_join(
    planning_raw %>%
      arrange(aggregated_crossings_id) ,

    pscis_raw %>%
      mutate(stream_crossing_id = as.character(stream_crossing_id)) %>%
      dplyr::select(
        stream_crossing_id,
        stream_name,
        road_name,
        outlet_drop,
        downstream_channel_width,
        habitat_value_code,
        image_view_url),

    by = c('aggregated_crossings_id' = 'stream_crossing_id')) %>%
    mutate(map_link = paste0('https://hillcrestgeo.ca/outgoing/fishpassage/projects/parsnip/archive/2022-05-27/FishPassage_', dbm_mof_50k_grid, '.pdf')) %>%
    mutate(my_review = TRUE) %>%
    dplyr::select(aggregated_crossings_id,
           my_review,
           stream_name,
           road_name,
           outlet_drop,
           downstream_channel_width,
           habitat_value_code,
           image_view_url),

  by = 'aggregated_crossings_id'

) %>%
  filter(watershed_group_code == 'KLUM') %>%
  filter(st_rearing_km > 0.1) %>%
  filter(crossing_type_code != 'OBS') %>%
  filter(barrier_status == 'POTENTIAL'|barrier_status == 'BARRIER') %>%
  filter(is.na(pscis_status) | (pscis_status != 'HABITAT CONFIRMATION' &
                                  barrier_status != 'PASSABLE' &
                                  barrier_status != 'UNKNOWN')) %>%
  filter(is.na(barriers_anthropogenic_dnstr)) %>%
  mutate(
         my_priority = NA_character_,
         my_priority_comments = NA_character_,
         my_citation_key1 = NA_character_,
         my_citation_key2 = NA_character_,
         my_citation_key3 = NA_character_
         )

planning %>%

  sf::st_write(paste0('../../gis/',
                    dir_project,
                    '/',
                    paste0('planning_', format(lubridate::now(), "%Y%m%d")),
                    '.gpkg'),
             # turned this T now that we have time in name
             delete_layer = T)


# no salmon modelling data in dev server for klum watershed
# no fish modelling data at all in shared db bcfishpass





























