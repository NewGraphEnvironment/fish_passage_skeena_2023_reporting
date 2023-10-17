# this file will
# find watershed codes (need to qa these results)
# assign waterbody identifier manually since it is easy when we are just in one wsgroup
# get a csv ready to copy the "alias local name" over to the start of the comments for submission to meet requirements of the spreadsheet.
# I dont think the alias local name matter really but can't hurt to follow their instructions plus it ensures the reader knows which PSCIS id to match the site to.
# after the info is hand bombed in we need to copy the file into the permit directory with the proper name
# copy report and attach

source('scripts/functions.R')
source('scripts/packages.R')
source('scripts/tables.R')

conn <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv('PG_DB_DEV'),
  host = Sys.getenv('PG_HOST_DEV'),
  port = Sys.getenv('PG_PORT_DEV'),
  user = Sys.getenv('PG_USER_DEV'),
  password = Sys.getenv('PG_PASS_DEV')
)

dbGetQuery(conn,
           "SELECT column_name,data_type
           FROM information_schema.columns
           WHERE table_name='wdic_waterbody_route_line_svw'")

wscodes <- dbGetQuery(conn,
                      "SELECT DISTINCT ON (stream_crossing_id)
a.stream_crossing_id,
a.linear_feature_id,
a.watershed_group_code,
b.watershed_code_50k,
substring(b.watershed_code_50k from 1 for 3)
||'-'||substring(b.watershed_code_50k from 4 for 6)
||'-'||substring(b.watershed_code_50k from 10 for 5)
||'-'||substring(b.watershed_code_50k from 15 for 5)
||'-'||substring(b.watershed_code_50k from 20 for 4)
||'-'||substring(b.watershed_code_50k from 24 for 4)
||'-'||substring(b.watershed_code_50k from 28 for 3)
||'-'||substring(b.watershed_code_50k from 31 for 3)
||'-'||substring(b.watershed_code_50k from 34 for 3)
||'-'||substring(b.watershed_code_50k from 37 for 3)
||'-'||substring(b.watershed_code_50k from 40 for 3)
||'-'||substring(b.watershed_code_50k from 43 for 3) as watershed_code_50k_parsed,
b.blue_line_key_20k,
b.watershed_key_20k,
b.blue_line_key_50k,
b.watershed_key_50k,
b.match_type
FROM bcfishpass.crossings a
LEFT OUTER JOIN whse_basemapping.fwa_streams_20k_50k b
ON a.linear_feature_id = b.linear_feature_id_20k
WHERE a.watershed_group_code IN ('MORR', 'BULK', 'ZYMO', 'KISP')
ORDER BY a.stream_crossing_id, b.match_type;"
)  %>%
  filter(stream_crossing_id %in% (pscis_phase2 %>% pull(pscis_crossing_id)))

# define our crs
##now we need to join to our habitat_confirmations sheet in the right order so we can copy and past into the spreadsheet
wsc_join <- left_join(
  hab_loc %>%
    tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
    mutate(site = as.integer(site)),
  select(wscodes, stream_crossing_id, watershed_code_50k = watershed_code_50k_parsed, watershed_group_code),
  by = c('site' = 'stream_crossing_id')) %>%
  select(reference_number, gazetted_name, alias_local_name, site, location, watershed_code_50k, watershed_group_code) %>%
  mutate(alias_corrected = NA_character_,
         waterbody_id = paste0('00000', watershed_group_code),
         waterbody_type = 'stream') %>%
  # QA in QGIS using the wdic_waterbody_route_line_svw layer in postgres db, this layer comes from:
  # https://catalogue.data.gov.bc.ca/dataset/wsa-stream-routes-50-000/resource/d694884b-97a6-4d34-9ece-263aa46974f5
  # here are some hand bomb corrections
  # mutate(watershed_code_50k = case_when(
  #   site == 197974 ~ '460-829700-20600-00000-0000-0000-000-000-000-000-000-000',
  #   site == 198066 ~ '460-517700-00000-00000-0000-0000-000-000-000-000-000-000',
  #   site == 198116 ~ '460-007300-39470-00000-0000-0000-000-000-000-000-000-000',
  #   T ~ watershed_code_50k)) %>%
  select(reference_number, alias_local_name, site, location, gazetted_name, alias_corrected, waterbody_type, waterbody_id, watershed_code_50k)


# order to location spreadsheet and burn to file
wsc_join %>%
  readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/hab_con_wsc_codes.csv'), na = "")


dbDisconnect(conn = conn)

# now make a csv with the alias_local_name pasted to the comments so people can see which comments are linked to which site
target_dir = paste0(getwd(), '/data/inputs_extracted/alias_local_name_relocate.csv')

fpr_hab_alias_to_comments()

# WATCH OUT HERE BECAUSE we might have 'feature record only' events that are not in the hab_site dataframe.  We need to account for that
# burn to the file so you can copy and paste to comments and erase alias_local_name AFTER after moving file to permit submission file .

# copy the hab con file over to the permit folder on OneDrive
permit_id = 'SM22-757463'

file.copy(from = 'data/habitat_confirmations.xls',
          to = paste0('C:/Users/matwi/OneDrive/Projects/repo/fish_passage_skeena_2022_reporting/permit_submission/', permit_id, '_data.xls'))
