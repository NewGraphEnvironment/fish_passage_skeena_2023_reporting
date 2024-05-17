source('scripts/packages.R')
source('scripts/tables.R')

fpr_make_geopackage(dat = hab_fish_collect)
fpr_make_geopackage(dat = hab_features)
fpr_make_geopackage(dat = hab_site_priorities)
fpr_make_geopackage(dat = phase1_priorities)

##we do this manually since the
# phase1_priorities %>%
#   st_transform(crs = 3005) %>%
#   sf::st_write(paste0("./data/", 'fishpass_mapping', ".gpkg"), 'phase1_priorities', delete_layer = TRUE)



##add the tracks
sf::read_sf("data/habitat_confirmation_tracks.gpx", layer = "tracks") %>%
  sf::st_write("data/fishpass_mapping/fishpass_mapping.gpkg", 'hab_tracks', append = TRUE)

wshd_study_areas <- fpr_db_query(
  query = "SELECT * FROM whse_basemapping.fwa_watershed_groups_poly a
                               WHERE a.watershed_group_code IN ('ZYMO','MORR','KISP', 'BULK', 'KLUM')"
)

wshd_study_areas %>%
  # select(-wscode_ltree) %>%
  st_cast("POLYGON") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_write("data/fishpass_mapping/fishpass_mapping.gpkg", 'wshd_study_areas', append = F)


####------------add the watersheds-------------------------

##we need to remove crossings that are first order - b/capi kicks us off...
# we don't need to do that in 2023 bc all are 2nd order or higher
# !if we have 1st order watersheds see procedures in skeena 2022 script
bcfishpass_phase2_clean <- bcfishpass_phase2 %>%
  filter(stream_order != 1)

wshds_raw <- fpr_sp_watershed(bcfishpass_phase2_clean)

# calculate stats for each watershed
wshds <- fpr::fpr_sp_wshd_stats(dat = wshds_raw) |>
  mutate(area_km = round(area_ha/100, 1)) %>%
  # below used to be mutate(across(contains('elev'), round, 0)) but throws error... complicated new format...
  mutate(across(contains('elev'), \(x) round(x, 0))) %>%
  arrange(stream_crossing_id)

# ##add to the geopackage
wshds %>%
  sf::st_write("data/fishpass_mapping/fishpass_mapping.gpkg", 'hab_wshds', append = F) ##might want to f the append....

#burn to kml as well so we can see elevations
st_write(wshds, append = F, driver = 'kml', dsn = "data/inputs_extracted/wshds.kml")

# put in the sqlite (really redundant but going to leave for now
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_write(wshds, exists = F, delete = TRUE,
                           conn = conn, x_name = "wshds")
readwritesqlite::rws_disconnect(conn)

####--------------------burn geojsons from geopackage-----------------------------------------------------
##we need geojsons to make the mapping convenient so lets pull everything out of the geopackage and write to geojson files
read_gpkg <- function(layers = layer_name){
  sf::st_read(dsn = "data/fishpass_mapping/fishpass_mapping.gpkg", layer = layers) %>%
    mutate(name = layers)
}

##get the names of the layers you want
layer_names <- sf::st_layers("data/fishpass_mapping/fishpass_mapping.gpkg") %>%
  pluck('name')

##grab the layers and give them a name
layers_to_burn <- layer_names %>%
  purrr::map(read_gpkg) %>%
  purrr::set_names(nm = layer_names)


write_geojson <- function(layers){
  layers %>%
  geojsonio::geojson_write(file = paste0("./data/fishpass_mapping/", unique(layers$name), ".geojson"))
}

layers_to_burn %>%
  map(write_geojson)
