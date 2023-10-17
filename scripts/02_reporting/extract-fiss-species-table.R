source('scripts/packages.R')

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv('PG_DB_DEV'),
  host = Sys.getenv('PG_HOST_DEV'),
  port = Sys.getenv('PG_PORT_DEV'),
  user = Sys.getenv('PG_USER_DEV'),
  password = Sys.getenv('PG_PASS_DEV')
)


##get the observations from the fiss layer
fish_species_watershed <- sf::st_read(conn,
                                      query = "SELECT DISTINCT ws.watershed_group_code, x.species_code,x.species_name
                   FROM whse_fish.fiss_fish_obsrvtn_pnt_sp x
                   INNER JOIN
                   whse_basemapping.fwa_watershed_groups_poly ws
                   ON ST_intersects(x.geom, ws.geom)
                   WHERE ws.watershed_group_code IN
                             ('MORR', 'ZYMO', 'KISP')")


##lets bust it up and join it back together
fish_spp <- merge(merge(fish_species_watershed %>% filter(watershed_group_code == 'MORR') %>% rename(Morice = watershed_group_code),
                        fish_species_watershed %>% filter(watershed_group_code == 'ZYMO')%>% rename(Zymoetz = watershed_group_code),
                        by = c('species_code', 'species_name'), all = TRUE),
                        fish_species_watershed %>% filter(watershed_group_code == 'KISP')%>% rename(Kispiox = watershed_group_code),
                        by = c('species_code', 'species_name'), all = TRUE
                        )

fish_all <- fishbc::freshwaterfish
fish_cdc <- fishbc::cdc

fish_spp2 <- left_join(fish_spp,
                       fish_all,
                       by = c("species_code" = "Code")) %>%
  filter(!is.na(Class) & !species_code == 'TR') %>% ##mottled sculpin has some sort of error going on
  # mutate(CDCode = case_when(species_code == 'BT' ~ 'F-SACO-11', ##pacific population yo
  #                           T ~ CDCode)) %>%
  select(species_code, species_name, Morice, Zymoetz, Kispiox, CDCode)

fish_spp3 <- left_join(
  fish_spp2,
  fish_cdc,
  by = c('CDCode' = 'Species Code')
) %>%
  select(`Scientific Name`,
         'Species Name' = species_name,
         'Species Code' = species_code,
         `BC List`,
         `Provincial FRPA`,
         COSEWIC,
         SARA,
         Morice,
         Zymoetz,
         Kispiox) %>%
  mutate(Morice = case_when(!is.na(Morice) ~ 'Yes',
                                 T ~ Morice),
         Zymoetz = case_when(!is.na(Zymoetz) ~ 'Yes',
                              T ~ Zymoetz),
         Kispiox = case_when(!is.na(Kispiox) ~ 'Yes',
                          T ~ Kispiox)) %>%
  arrange(`Scientific Name`, `Species Name`)

##print your table to input_raw for use in the report
fish_spp3 %>% readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/fiss_species_table.csv'))

dbDisconnect(conn = conn)
