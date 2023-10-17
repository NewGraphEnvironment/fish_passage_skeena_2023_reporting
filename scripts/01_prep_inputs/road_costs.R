source('scripts/packages.R')
source('scripts/tables.R')

##get the road info from the database
conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv('PG_DB_BCBARRIERS'),
  host = Sys.getenv('PG_HOST_BCBARRIERS'),
  port = 5432,
  user = Sys.getenv('PG_USER_BCBARRIERS'),
  password = Sys.getenv('PG_PASS_BCBARRIERS')
)
#
# # ##listthe schemas in the database
# dbGetQuery(conn,
#            "SELECT schema_name
#            FROM information_schema.schemata")
# # #
# # #
# # # # ##list tables in a schema
# dbGetQuery(conn,
#            "SELECT table_name
#            FROM information_schema.tables
#            WHERE table_schema='whse_basemapping'")
# # # # # #
# # # # # # ##list column names in a table
# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='transport_line_type_code'")
#
# dbGetQuery(conn,
#            "SELECT column_name,data_type
#            FROM information_schema.columns
#            WHERE table_name='crossings'")


# test <- dbGetQuery(conn, "SELECT * FROM bcfishpass.waterfalls")

# build pscis_all_sf with the first 100 lines of tables.R - # HACK!!!!!!!!!!! - replace pscis_all_prep with pscis_all there
# add a unique id - we could just use the reference number
pscis_all_sf$misc_point_id <- seq.int(nrow(pscis_all_sf))


# I'm going to create the ali schema locally so this looks the same on the remote db
# dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", "ali",";"))


# load to database
sf::st_write(obj = pscis_all_sf, dsn = conn, Id(schema= "ali", table = "misc"))



# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON ali.misc USING GIST (geometry)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE ali.misc ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

dat_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.*,
  ST_Distance(ST_Transform(a.geometry,3005), b.geom) AS distance
FROM
  ali.misc AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM bcfishpass.crossings
   ORDER BY
     a.geometry <-> geom
   LIMIT 1) AS b")



##swapped out fish_passage.modelled_crossings_closed_bottom for bcfishpass.barriers_anthropogenic

##join the modelling data to our pscis submission info
dat_joined <- left_join(
  select(pscis_all_sf, misc_point_id, pscis_crossing_id, my_crossing_reference, source), ##traded pscis_crossing_id for my_crossing_reference
  dat_info,
  by = "misc_point_id"
) %>%
  mutate(downstream_route_measure = as.integer(downstream_route_measure))


# dbDisconnect(conn = conn)
#
#
# ##we also need to know if the culverts are within a municipality so we should check
# ##get the road info from our database
# conn <- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname = dbname_wsl,
#   host = host_wsl,
#   port = port_wsl,
#   user = user_wsl,
#   password = password_wsl
# )

# load to database
sf::st_write(obj = pscis_all_sf, dsn = conn, Id(schema= "ali", table = "misc"))

dat_info <- dbGetQuery(conn,
                       "

                                  SELECT a.misc_point_id, b.admin_area_abbreviation, c.map_tile_display_name
                                  FROM ali.misc a
                                  INNER JOIN
                                  whse_basemapping.dbm_mof_50k_grid c
                                  ON ST_Intersects(c.geom, ST_Transform(a.geometry,3005))
                                  LEFT OUTER JOIN
                                  whse_legal_admin_boundaries.abms_municipalities_sp b
                                  ON ST_Intersects(b.geom, ST_Transform(a.geometry,3005))
                       ")

dbDisconnect(conn = conn)

##add the municipality info
dat_joined2 <- left_join(
  dat_joined,
  dat_info,
  by = "misc_point_id"
)

# ##clean up the workspace
rm(dat_info, dat_joined, res)
#

##make a tibble of the client names so you can summarize in the report
##we do not need to repeat this step but this is how we make a dat to paste into a kable in rmarkdown then paste tibble as a rstudio addin so we can
##populate the client_name_abb...

##we already did this but can do it again I guess.  you cut and paste the result into kable then back
##into here using addin for datapasta
tab_rd_tenure_xref <- unique(dat_joined2$ften_client_name) %>%
  as_tibble() %>%
  purrr::set_names(nm = 'ften_client_name') %>%
  mutate(client_name_abb = stringr::str_to_title(ften_client_name))

tab_rd_tenure_xref <- tibble::tribble(
                                          ~ften_client_name,                    ~client_name_abb,
                                                        "–",                                 "–",
                              "TIMBER SALES MANAGER SKEENA",                              "BCTS",
                               "GITXSAN FOREST LICENCE INC",                    "Gitxsan Forest",
                        "DISTRICT MGR SKEENA STIKINE (DSS)",                               "MoF",
                                "KISPIOX RIVER TIMBER LTD.",              "Kispiox River Timber"
                        )


##add that to your dat file for later
dat_joined3 <- left_join(
  dat_joined2,
  tab_rd_tenure_xref,
  by = 'ften_client_name'
)


# see https://github.com/NewGraphEnvironment/fish_passage_bulkley_2022_reporting/issues/16 for this section.  putting aside for now
# we need to get the road class from the DRA layer whse_basemapping.transport_line_type_code



# look at any sites more than 100m away from their match
#  the reason there is more than one hit is because we
more_100 <- dat_joined3 %>%
  filter(distance > 100) %>%
  select(aggregated_crossings_id, modelled_crossing_id, distance, source) %>%
  distinct(aggregated_crossings_id, distance, .keep_all = T)

# these are the columms we need
# ften_forest_file_id
# transport_line_type_description
# ften_forest_file_id
# ften_client_name
# rail_owner_name

unique(dat_joined3$transport_line_type_description)
##make a dat to make it easier to see so we can summarize the road info we might want to use
dat_joined4 <- dat_joined3 %>%
  mutate(
    # rewriting since we don't have road_class
    # admin_area_abbreviation = case_when(
    #   is.na(admin_area_abbreviation) & (road_class %ilike% 'arterial' | road_class %ilike% 'local') ~ 'MoTi',
    #   T ~ admin_area_abbreviation),
    admin_area_abbreviation = case_when(
      is.na(admin_area_abbreviation) & (transport_line_type_description %ilike% 'arterial' |
                                          transport_line_type_description %ilike% 'local' |
                                          transport_line_type_description %ilike% 'highway') ~ 'MoTi',
      T ~ admin_area_abbreviation),
    admin_area_abbreviation = replace_na(admin_area_abbreviation, ''),
    my_road_tenure =
      case_when(!is.na(client_name_abb) ~ paste0(client_name_abb, ' ', ften_forest_file_id),
                !is.na(transport_line_type_description) ~ paste0(admin_area_abbreviation, ' ', stringr::str_to_title(transport_line_type_description)),
                !is.na(rail_owner_name) ~ rail_owner_name)) %>%
  # mutate(my_road_tenure =
  #          case_when(distance > 100 ~ 'Unknown',  ##we need to get rid of the info for the ones that are far away
  #                    T ~ my_road_tenure)) %>%
  rename(geom_modelled_crossing = geom) %>%
  mutate(
    my_road_tenure =stringr::str_trim(my_road_tenure),
    aggregated_crossings_id = case_when(!is.na(pscis_crossing_id) ~ pscis_crossing_id,
                                        my_crossing_reference > 200000000 ~ my_crossing_reference,
                                        T ~ my_crossing_reference + 1000000000)) %>%
  sf::st_drop_geometry() %>%
  # replace some words to shorten
  mutate(my_road_tenure = str_replace_all(my_road_tenure, 'Road', '')) %>%
  mutate(my_road_tenure = str_replace_all(my_road_tenure, 'Minor', '')) %>%
  mutate(my_road_tenure = str_replace_all(my_road_tenure, 'Major', ''))

##we cannot use base R to add a column named 'source' so we choose a different name
col_new <- pscis_all_sf$source
dat_joined4$source_wkb <- col_new


##build tables to populate the pscis spreadsheets
pscis1_rd_tenure <- left_join(
  select(pscis_phase1, rowid, my_crossing_reference, road_tenure),
  dat_joined4 %>% filter(source_wkb %ilike% 'phase1') %>% select(my_crossing_reference, transport_line_structured_name_1, my_road_tenure),
  by = 'my_crossing_reference'
) %>%
  # for some reason there is a duplicate. not messing withi it
  distinct(rowid, .keep_all = T)




##burn it all to a file we can input to pscis submission spreadsheet
pscis1_rd_tenure %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis1.csv'),
                   na = '')



pscis_reassessments_rd_tenure <- left_join(
  select(pscis_reassessments, rowid, pscis_crossing_id, road_tenure),
  dat_joined4 %>% filter(source_wkb %ilike% 'reassess') %>% select(pscis_crossing_id, transport_line_structured_name_1, my_road_tenure),
  by = 'pscis_crossing_id'
)

##burn it all to a file we can input to pscis submission spreadsheet
pscis_reassessments_rd_tenure %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis_reassess.csv'),
                   na = '')

pscis2_rd_tenure <- left_join(
  select(pscis_phase2, rowid, pscis_crossing_id, my_crossing_reference, road_tenure),
  dat_joined4 %>% filter(source_wkb %ilike% 'phase2') %>% select(my_crossing_reference, transport_line_structured_name_1, my_road_tenure),
  by = 'my_crossing_reference'
)

pscis2_rd_tenure %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/rd_tenure_pscis2.csv'),
                   na = '')

