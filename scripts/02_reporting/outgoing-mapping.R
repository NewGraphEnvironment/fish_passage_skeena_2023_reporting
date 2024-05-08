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
sf::read_sf("./data/habitat_confirmation_tracks.gpx", layer = "tracks") %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'hab_tracks', append = TRUE)

##study area watersheds
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv('PG_DB_DO'),
  host = Sys.getenv('PG_HOST_DO'),
  port = Sys.getenv('PG_PORT'),
  user = Sys.getenv('PG_USER_DO'),
  password = Sys.getenv('PG_PASS_DO')
)

# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='fwa_watershed_groups_poly'")

##here is the study area watersheds
wshd_study_areas <- st_read(conn,
                           query = "SELECT * FROM whse_basemapping.fwa_watershed_groups_poly a
                               WHERE a.watershed_group_code IN ('ZYMO','MORR','KISP')"
)

wshd_study_areas %>%
  # select(-wscode_ltree) %>%
  st_cast("POLYGON") %>%
  sf::st_transform(crs = 4326) %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'wshd_study_areas', append = F)

dbDisconnect(conn = conn)

####------------add the watersheds-------------------------

##we needed to remove crossings that are first order - this used to run but doesn't want to anymore
##i wonder if it is because the 1st order watershed is the first one on the list so the api kicks us off...
bcfishpass_phase2_clean <- bcfishpass_phase2 %>%
  filter(stream_order != 1)

wshds <- fpr_sp_watershed(bcfishpass_phase2_clean)

# ##add to the geopackage
wshds %>%
  sf::st_write(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg"), 'hab_wshds', append = F) ##might want to f the append....

#burn to kml as well so we can see elevations
st_write(wshds, append = F, driver = 'kml', dsn = "data/inputs_extracted/wshds.kml")

####--------------------burn geojsons from geopackage-----------------------------------------------------
##we need geojsons to make the mapping convenient so lets pull everything out of the geopackage and write to geojson files
read_gpkg <- function(layers = layer_name){
  sf::st_read(dsn = "./data/fishpass_mapping/fishpass_mapping.gpkg", layer = layers) %>%
    mutate(name = layers)
    # sf::st_transform(crs = 4326)
}

##get the names of the layers you want
layer_names <- sf::st_layers(paste0("./data/fishpass_mapping/", 'fishpass_mapping', ".gpkg")) %>%
  pluck('name')

##grab the layers and give them a name
layers_to_burn <- layer_names %>%
  purrr::map(read_gpkg) %>%
  purrr::set_names(nm = layer_names)

# ##now burn them to a folder called fishpass_mapping
# dir.create('data/fishpass_mapping')

write_geojson <- function(layers){
  layers %>%
  geojsonio::geojson_write(file = paste0("./data/fishpass_mapping/", unique(layers$name), ".geojson"))
}

layers_to_burn %>%
  map(write_geojson)
