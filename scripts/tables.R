# Import our data and builds the tables we need for our reporting


source('scripts/functions.R')
# define the repo name since it is used in tab_map in this script within this environment (vs the bookdown rendering environment)
# not sure those links in those tables work anyway though... need to confirm
repo_name <- 'fish_passage_skeena_2023_reporting'


# script over the photos from onedrive to our local repo



pscis_list <- fpr::fpr_import_pscis_all()
pscis_phase1 <- pscis_list |> pluck('pscis_phase1')
pscis_phase2 <- pscis_list |> pluck('pscis_phase2') |>
  arrange(pscis_crossing_id)
pscis_reassessments <- pscis_list |> pluck('pscis_reassessments')
pscis_all_prep <- pscis_list |>
  bind_rows()


# import data from sqlite -------------------------------------------------
#this is our new db made from load-bcfishpass-data.R and 0290
conn <- readwritesqlite::rws_connect("data/bcfishpass.sqlite")
readwritesqlite::rws_list_tables(conn)
bcfishpass <- readwritesqlite::rws_read_table("bcfishpass", conn = conn)
  # sf::st_drop_geometry()
  # mutate(ch_cm_co_pk_sk_network_km = round(ch_cm_co_pk_sk_network_km,2))
# pscis_historic_phase1 <- readwritesqlite::rws_read_table("pscis_historic_phase1", conn = conn)
bcfishpass_spawn_rear_model <- readwritesqlite::rws_read_table("bcfishpass_spawn_rear_model", conn = conn)
# tab_cost_rd_mult <- readwritesqlite::rws_read_table("rd_cost_mult", conn = conn)
rd_class_surface_prep <- readwritesqlite::rws_read_table("rd_class_surface", conn = conn)
pscis_assessment_svw <- readwritesqlite::rws_read_table("pscis_assessment_svw", conn = conn)
xref_pscis_my_crossing_modelled <- readwritesqlite::rws_read_table("xref_pscis_my_crossing_modelled", conn = conn)
wshds <- readwritesqlite::rws_read_table("wshds", conn = conn) |>
   mutate(aspect = as.character(aspect))

photo_metadata <- readwritesqlite::rws_read_table("photo_metadata", conn = conn)
# # fiss_sum <- readwritesqlite::rws_read_table("fiss_sum", conn = conn)
readwritesqlite::rws_disconnect(conn)


bcfishpass_phase2 <- bcfishpass |>
  dplyr::filter(stream_crossing_id %in%
                  (pscis_phase2 |>
                     pull(pscis_crossing_id))) |>
  dplyr::filter(!is.na(stream_crossing_id))

# due to this https://github.com/smnorris/bcfishpass/issues/492 we are doing it this way for now
# methods to update are in fpr I believe
# not sure why this is the same as bcfishpass_column_comments
# investigate then delete--------------------------
# bcfishpass_column_comments <- fpr::fpr_xref_crossings

tab_cost_rd_mult <- readr::read_csv('data/inputs_raw/tab_cost_rd_mult.csv')

# bcfishpass modelling table setup for reporting
xref_bcfishpass_names <- fpr::fpr_xref_crossings

# this doesn't work till our data loads to pscis
pscis_all <- left_join(
  pscis_all_prep,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) |>
  mutate(pscis_crossing_id = case_when(
    is.na(pscis_crossing_id) ~ as.numeric(stream_crossing_id),
    T ~ pscis_crossing_id
  )) |>
  # mutate(amalgamated_crossing_id = case_when(
  #   !is.na(my_crossing_reference) ~ my_crossing_reference,
  #   T ~ pscis_crossing_id
  # )) |>
  # select(-stream_crossing_id) |>
  dplyr::arrange(pscis_crossing_id)

# make spatial object of pscis data
pscis_all_sf <- pscis_all |>
  fpr::fpr_sp_assign_sf_from_utm()


# here is a spot to burn this to a file to try to figure out wtf is going on with these IDs
# pscis_all_sf |> sf::write_sf('data/inputs_extracted/pscis_all_2023.gpkg')


# get_elev is in the `scripts/functions.R` file
# we should prob just do this once and store values in the sqlite to avoid outside api calls (no need then for internet to build)
pscis_all_sf <- pscis_all_sf |>
  # we only split it b/c the api can only handle so many requests at once
  dplyr::group_split(source) |>
  # this funciton is in the scripts/functions.R file
  purrr::map(get_elev) |>
  dplyr::bind_rows()


##this is not working or needed yet
# bcfishpass_rd <- bcfishpass |>
#   select(pscis_crossing_id = stream_crossing_id, my_crossing_reference, crossing_id, distance, road_name_full,
#          road_class, road_name_full, road_surface, file_type_description, forest_file_id,
#          client_name, client_name_abb, map_label, owner_name, admin_area_abbreviation,
#          steelhead_network_km, steelhead_belowupstrbarriers_network_km, distance) |>
#   # dplyr::filter(distance < 100) |> ## we need to screen out the crossings that are not matched well
#   select(pscis_crossing_id, my_crossing_reference:admin_area_abbreviation, steelhead_network_km, steelhead_belowupstrbarriers_network_km)


# prob fine to keep costs locally since they change all the time
tab_cost_rd_mult_report <- tab_cost_rd_mult |>
  mutate(cost_m_1000s_bridge = cost_m_1000s_bridge * 10) |>
  rename(
    Class = my_road_class,
    Surface = my_road_surface,
    `Class Multiplier` = road_class_mult,
    `Surface Multiplier` = road_surface_mult,
    `Bridge $K/10m` = cost_m_1000s_bridge,
    `Streambed Simulation $K` = cost_embed_cv
  ) |>
  dplyr::filter(!is.na(Class)) |>
  mutate(Class =case_when(
    Class == 'fsr' ~ str_to_upper(Class),
    T ~ stringr::str_to_title(Class)),
    Surface = stringr::str_to_title(Surface)
  )


# we are not doing this right now because we have PSCIS Ids for everything
# this is confusing because of the `name` of xref_pscis_my_crossing_modelled.  It should be named differently
# b/c in this instance it is a match based on a join of an sf of our crossings and bcfishpass.crossings_vw. Different
# than the actual new PSCIS IDs and my_crossing_reference - sigh
# pscis_rd <- left_join(
#   rd_class_surface,
#   xref_pscis_my_crossing_modelled,
#   by = c('my_crossing_reference' = 'external_crossing_reference')
# ) |>
#   mutate(stream_crossing_id = as.numeric(stream_crossing_id)) |> #should be able to remove this after we have the data in?
#   mutate(pscis_crossing_id = case_when(!is.na(stream_crossing_id) ~ stream_crossing_id,
#                                        T ~ pscis_crossing_id)) |>
#   select(-stream_crossing_id)
#   # dplyr::filter(distance < 100)


# priorities phase 1 ------------------------------------------------------
##uses habitat value to initially screen but then refines based on what are likely not barriers to most  the time
# this includes all of the reassessments too....
phase1_priorities <- pscis_all |>
  dplyr::filter(!str_detect(source, 'phase2')) |>
  dplyr::select(aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, utm_zone:northing, habitat_value, barrier_result, source) |>
  dplyr::mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result != 'Passable' ~ 'high',
                                     habitat_value == 'Medium' & barrier_result != 'Passable' ~ 'mod',
                                     habitat_value == 'Low' & barrier_result != 'Passable' ~ 'low',
                                     T ~ NA_character_)) |>
  dplyr::mutate(priority_phase1 = case_when(habitat_value == 'High' & barrier_result == 'Potential' ~ 'mod',
                                     T ~ priority_phase1)) |>
  dplyr::mutate(priority_phase1 = case_when(habitat_value == 'Medium' & barrier_result == 'Potential' ~ 'low',
                                     T ~ priority_phase1)) |>
  # mutate(priority_phase1 = case_when(my_crossing_reference == 99999999999 ~ 'high', ##this is where we can make changes to the defaults
  #                                    T ~ priority_phase1)) |>
  dplyr::rename(utm_easting = easting, utm_northing = northing)


##turn spreadsheet into list of data frames
pscis_phase1_for_tables <- pscis_all |>
  dplyr::filter(str_detect(source, 'phase1'))|>
  arrange(pscis_crossing_id)


pscis_split <- pscis_phase1_for_tables  |> #pscis_phase1_reassessments
  # sf::st_drop_geometry() |>
  # mutate_if(is.numeric, as.character) |> ##added this to try to get the outlet drop to not disapear
  # tibble::rownames_to_column() |>
  dplyr::group_split(pscis_crossing_id) |>
  purrr::set_names(pscis_phase1_for_tables$pscis_crossing_id)

##make result summary tables for each of the crossings
tab_summary <- pscis_split |>
  purrr::map(fpr::fpr_table_cv_detailed)

tab_summary_comments <- pscis_split |>
  purrr::map(fpr::fpr_table_cv_detailed_comments)

##had a hickup where R cannot handle the default size of the integers we used for numbers so we had to change site names!!
tab_photo_url <- left_join(

  list.files(path = 'data/photos/', full.names = T) |>
  basename() |>
  as_tibble() |>
  mutate(value = as.integer(value)) |>  ##need this to sort
  dplyr::arrange(value)  |>
  mutate(photo = paste0('![](data/photos/', value, '/crossing_all.JPG)')) |>
  dplyr::filter(value %in% pscis_phase1_for_tables$my_crossing_reference),
  ##we don't want all the photos - just the phase 1 photos for this use case!!!

  xref_pscis_my_crossing_modelled,

  by = c('value' = 'external_crossing_reference'))  |> ##we need to add the pscis id so that we can sort the same
  arrange(stream_crossing_id) |>
  select(-value) |>
  # pull(photo)
  dplyr::group_split(stream_crossing_id)
  # purrr::set_names(nm = . |> bind_rows() |> arrange(value) |> pull(stream_crossing_id)) |>
  # bind_rows()
  # arrange(stream_crossing_id) |>
  # dplyr::group_split(value)


# html tables
tabs_phase1 <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url,
  gitbook_switch = TRUE)


tabs_phase1_pdf <- mapply(
  fpr::fpr_table_cv_detailed_print,
  tab_sum = tab_summary,
  comments = tab_summary_comments,
  photos = tab_photo_url,
  gitbook_switch = FALSE)

# tabs_phase1_pdf <- mapply(fpr_print_tab_summary_all_pdf, tab_sum = tab_summary, comments = tab_summary_comments, photos = tab_photo_url)

#-------------- habitat and fish data------------------
habitat_confirmations <- fpr::fpr_import_hab_con(col_filter_na = T, row_empty_remove = T)


hab_site_prep <-  habitat_confirmations |>
  purrr::pluck("step_4_stream_site_data") |>
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) |>
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) |>
  mutate_if(is.numeric, round, 1) |>
  select(-gazetted_names:-site_number, -feature_type:-utm_method) |>   ##remove the feature utms so they don't conflict with the site utms
  distinct(reference_number, .keep_all = T) ##since we have features we need to dplyr::filter them out


hab_loc <- habitat_confirmations |>
  purrr::pluck("step_1_ref_and_loc_info") |>
  dplyr::filter(!is.na(alias_local_name))|>
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) |>
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) |>
  mutate(site = as.numeric(site)) |>
  dplyr::filter(!stringr::str_detect(alias_local_name, "_ef"))

hab_fish_collect_map_prep <- habitat_confirmations |>
  purrr::pluck("step_2_fish_coll_data") |>
  dplyr::filter(!is.na(site_number)) |>
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
  mutate(site_id = paste0(site, location)) |>
  distinct(local_name, species, .keep_all = T) |> ##changed this to make it work as a feed for the extract-fish.R file
  mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) |>
  mutate(across(c(time_in,time_out), chron::times))


##prep the location info so it is ready to join to the fish data

################################################################################################################
#------------------------------hack to deal with fish utm and demo waypoints - start----------------------------
################################################################################################################
# name sites that don't have `ef` or `mt` in their name but have fish info b/c we still need
# those UTMs
sites_no_label <- c("8478_us", "58067_ds2")

# now we are getting really custom
# read in the UTM of the canyon at Gramaphone so we get accurate point for the coho for our own mapping
# waypoints are only in the "waypoints" file because they were scripted out
# to avoid the mess that basecamp makes of duplicate names and to give unique IDs based on the surveyor. not sure why
# they are not also in skeena_2023_field_al.gpx - seems odd but not dealing with it now
gpx <- "~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/gps/skeena_2023_field_waypoints_al.gpx"

# see the layers
sf::st_layers(gpx)

wp_al <- sf::st_read(gpx,
                     layer = 'waypoints',
                     quiet = T) |>
  fpr::fpr_sp_assign_utm() |>
  # add a column to join on
  dplyr::mutate(alias_local_name =
                  dplyr::case_when(
                    name == 'ai_314' ~ "58067_ds2",
                    T ~ NA_character_))


hab_loc2_prep <- hab_loc |>
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) |>
  mutate(site_id = paste0(site, location)) |>
  dplyr::filter(str_detect(alias_local_name, paste(c('ef', 'mt', sites_no_label), collapse = '|'))) ##filter ef and mt sites

# now join the waypoint table and hab_loc2 and sub in this utm in place of the existing "58067_ds2"
hab_loc2 <- dplyr::left_join(
  hab_loc2_prep,

  wp_al |>
    dplyr::select(alias_local_name, easting, northing) |>
    sf::st_drop_geometry(),

  by = "alias_local_name"
  ) |>
  dplyr::mutate(utm_easting =
                  dplyr::case_when(
                    !is.na(easting) ~ easting,
                    T ~ utm_easting),
                utm_northing =
                  dplyr::case_when(
                    !is.na(northing) ~ northing,
                    T ~ utm_northing)) |>
  dplyr::select(-all_of(c("easting", "northing")))


################################################################################################################
#--------------------------------------------------end hack for utm fish gramaphone-----------------------------
################################################################################################################

# test to see what we get at each site
test <- hab_fish_collect_map_prep |>
  distinct(local_name, species)



##join the tables together and keep a rational column order and `local_name` (vs `alias_local_name`) with the right join
hab_fish_collect_map_prep2 <- dplyr::right_join(
  # distinct to get rid of lots of sites

  hab_loc2 |>
    select(reference_number, alias_local_name, utm_zone:utm_northing) |>
    distinct(alias_local_name, .keep_all = T),

  hab_fish_collect_map_prep |>
    select(local_name, species) |>
    distinct(local_name, species, .keep_all = T),

  by = c('alias_local_name' = 'local_name')
)



##add the species code
hab_fish_codes <- fishbc::freshwaterfish |>
  select(species_code = Code, common_name = CommonName) |>
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') |>
  mutate(common_name =
           case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)',
                     T ~ common_name))


# this is the table to burn to geojson for mapping
# we are just going to keep 1 site for upstream and downstream because more detail won't show well on the map anyway
# purpose is to show which fish are there vs. show all the sites and what was caught at each. TMI

# view what species we have
# hab_fish_collect_map_prep2 |>
# dplyr::distinct(species) |>
#   dplyr::pull(species)

hab_fish_collect_map_prep3 <- dplyr::left_join(
  hab_fish_collect_map_prep2 |>
    mutate(species = as.factor(species)),  ##just needed to do this b/c there actually are no fish.

  hab_fish_codes |>
    select(common_name, species_code),

  by = c('species' = 'common_name')
)
  # when we did muli-pass sites we ditched the nfc because we didn't want it to look like sites are non-fish bearing.
 # Its a multipass thing from 2022 and not something we ditch this time
  # dplyr::filter(species_code != 'NFC')

# need to make an array for mapping the hab_fish_collect files
# this gives a list column vs an array.  prob easier to use postgres and postgis to make the array
hab_fish_collect <- dplyr::left_join(
  hab_fish_collect_map_prep3 |>
    dplyr::select(alias_local_name:utm_northing) |>
    dplyr::distinct(),

  hab_fish_collect_map_prep3 |>
    dplyr::select(-species, -reference_number, -utm_zone:-utm_northing) |>
    tidyr::pivot_wider(names_from = 'alias_local_name', values_from = "species_code") %>% #we need the magrittr pipe to keep syntax simple
    # get first and last column name
    tidyr::pivot_longer(cols = names(.)[1]:names(.)[ncol(.)]) |>
    dplyr::rename(alias_local_name = name,
             species_code = value),

    by = 'alias_local_name'
) |>
  rowwise() |>
  mutate(species_code = toString(species_code),
         species_code = stringr::str_replace_all(species_code, ',', ''))


rm(hab_fish_collect_map_prep, hab_fish_collect_map_prep2)

hab_fish_collect_prep1 <- habitat_confirmations |>
  purrr::pluck("step_2_fish_coll_data") |>
  dplyr::filter(!is.na(site_number)) |>
  select(-gazetted_name:-site_number)

hab_features <- dplyr::left_join(
  habitat_confirmations |>
    purrr::pluck("step_4_stream_site_data") |>
    dplyr::select(reference_number,local_name, feature_type:utm_northing) |>
    dplyr::filter(!is.na(feature_type)),

  fpr::fpr_xref_obstacles,

  by = c('feature_type' = 'spreadsheet_feature_type')
)


## fish densities ----------------------------------------------------------
hab_fish_indiv_prep <- habitat_confirmations |>
  purrr::pluck("step_3_individual_fish_data") |>
  dplyr::filter(!is.na(site_number)) |>
  select(-gazetted_names:-site_number)

hab_fish_indiv_prep2 <- left_join(
  hab_fish_indiv_prep,
  hab_loc,
  by = 'reference_number'
)

hab_fish_indiv_prep3 <- left_join(
  hab_fish_indiv_prep2,

  hab_fish_codes |>
    select(common_name:species_code),

  by = c('species' = 'common_name')
) |>
  dplyr::select(reference_number,
                alias_local_name,
                site_number,
                sampling_method,
                method_number,
                haul_number_pass_number,
                species_code,
                length_mm,
                weight_g) ##added method #

hab_fish_collect_info <- habitat_confirmations |>
  purrr::pluck("step_2_fish_coll_data") |>
  dplyr::filter(!is.na(site_number)) |>
  # select(-gazetted_name:-site_number) |>
  dplyr::distinct(reference_number, sampling_method, method_number, haul_number_pass_number, .keep_all = T)

# join the indiv fish data to existing site info

# THIS SEEMS WEIRD SINCE WE ARE SELECTING SIZES THAT SHOULD BE INFORMED FROM `scripts/01_prep_inputs/0100-extract-inputs`
hab_fish_indiv <- dplyr::full_join(

  hab_fish_indiv_prep3 |>
    dplyr::select(
      reference_number,
      sampling_method,
      method_number,
      haul_number_pass_number,
      species_code,
      length_mm,
      weight_g),

  hab_fish_collect_info |>
    dplyr::select(
      reference_number,
      local_name,
      temperature_c:model, ##added date_in:time_out
      comments
    ),
  by = c(
    "reference_number",
    # 'alias_local_name' = 'local_name',
    "sampling_method",
    "method_number",
    "haul_number_pass_number")
) |>
  mutate(species_code = as.character(species_code)) |>
  # something is goign wrong here
  mutate(species_code = case_when(
    is.na(species_code) ~ 'NFC',
    T ~ species_code)
  ) |>
  mutate(species_code = as.factor(species_code)) |>
  mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
    length_mm <= 65 ~ 'fry',
    length_mm > 65 & length_mm <= 110 ~ 'parr',
    length_mm > 110 & length_mm <= 140 ~ 'juvenile',
    length_mm > 140 ~ 'adult',
    T ~ NA_character_
  ),
  life_stage = case_when(
    species_code %in% c('L', 'SU', 'LSU') ~ NA_character_,
    T ~ life_stage
  ))|>
  mutate(life_stage = fct_relevel(life_stage,
                                  'fry',
                                  'parr',
                                  'juvenile',
                                  'adult')) |>
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
  mutate(site_id = paste0(site, '_', location))

# make a summary table for fish sampling data

tab_fish_summary <- hab_fish_indiv |>
  group_by(site_id,
           ef,
           sampling_method,
           species_code) |> ##added sampling method!
  summarise(count_fish = n()) |>
  arrange(site_id, species_code, ef)

# this will be joined to the abundance estimates and the confidence intervals
fish_abund_prep <- hab_fish_indiv |>
  group_by(local_name,
           site_id,
           ef,
           sampling_method,
           haul_number_pass_number,
           species_code,
           life_stage,
           ef_seconds) |> ##added sampling method!
  dplyr::filter(sampling_method == 'electrofishing') |>
  summarise(catch = n()) |>
  arrange(site_id, species_code, ef) |>
  # ungroup() |>
  mutate(catch = case_when(
    species_code == 'NFC' ~ 0L,
    T ~ catch),
    # effort = catch/ef_seconds,
    id = paste0(local_name, '_', species_code, '_', life_stage)) |>
  ungroup() |>
  arrange(id)

# join the total number of passes to each event so that we know if it is a higher number than the pass of the catch
fish_abund_prep2 <- left_join(
  fish_abund_prep,

  fish_abund_prep |>
    group_by(local_name) |>
    summarise(pass_total = max(haul_number_pass_number)),
  by = 'local_name'
)

# make a dat to indicate if the nfc in the set for each species
fish_nfc_tag<- fish_abund_prep2 |>
  mutate(nfc_pass = case_when(
    # species_code != 'NFC' &
      haul_number_pass_number == pass_total ~ F,
    T ~ T),
    nfc_pass = case_when(
      species_code == 'NFC' ~ T,
      T ~ nfc_pass)
  ) |>
  select(
    local_name,
    species_code,
    life_stage,
    haul_number_pass_number,
    pass_total,
    # catch, #update 2024 - added this temporarily to make easier to understand that no fish of particular species captured on pass
    nfc_pass) |>
  arrange(desc(haul_number_pass_number)) |>
  # dplyr::filter(nfc_pass == T) |>
  distinct(local_name, species_code, life_stage, .keep_all = T) |>
  select(-haul_number_pass_number, -pass_total)

# dat to show sites  for those that have a pass where no fish of those species were captured
# nfc_pass tag used to indicate that this is an abundance estimate
# fish_nfc_tag <- left_join(
#   fish_abund_prep2,
#
#   fish_nfc_prep,
#   by = c('local_name','species_code', 'life_stage', 'haul_number_pass_number', 'pass_total')
# ) |>
#   tidyr::fill(nfc_pass, .direction = 'up')

  # dplyr::filter(!is.na(nfc_pass)) |>

  # mutate(nfc_pass = case_when(
  #   species_code != 'NFC' ~ 'TRUE',
  #   T ~ NA_character_))


# calculate abundance for each site regardless of whether a nfc_pass occurred.
fish_abund_prep3 <- left_join(
  fish_abund_prep2 |>
  group_by(local_name, species_code, life_stage) |>
  summarise(catch = sum(catch)),

  fish_nfc_tag,

  by = c('local_name', 'species_code', 'life_stage')
)


# add back the size of the sites so we can do a density
fish_abund <- left_join(
  fish_abund_prep3,

  hab_fish_collect_info |>
    select(local_name,
           # sampling_method,
           # haul_number_pass_number,
           ef_seconds:enclosure) |>
    distinct(local_name, ef_length_m, .keep_all = T),

  by = c('local_name')
) |>
  mutate(area_m2 = round(ef_length_m * ef_width_m,1),
         density_100m2 = round(catch/area_m2 * 100,1)) |>
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)


### depletion estimates -----------------------------------------------------

# only run depletion estimates when there are site/species events with more than 1 pass and all passes have fish.
# otherwise just add the counts together for the abundance
# fish_deplet_prep <- fish_abund_prep3 |>
#   dplyr::filter(is.na(abundance)) |>
#   # at least three passes
#   group_by(id) |>
#   dplyr::filter( n() > 2 )
#
#
# fish_abund_prep_ls <-  fish_deplet_prep |>
#   ungroup() |>
#   dplyr::group_split(id) |>
#   purrr::set_names(nm = unique(fish_deplet_prep$id))
#
# fpr_fish_depletion <- function(dat, ...){
#   ls <- FSA::depletion(dat$catch, dat$ef_seconds, Ricker.mod = F)
#   out <-  summary(ls) |>
#       as_tibble() |>
#       slice(1)
#   out
#   }
#
#
# fish_abund_prep_ls |>
#   purrr::map(fpr_fish_depletion) |>
#   map(plot)
#
# fish_abund_calc <- fish_abund_prep_ls |>
#   purrr::map(fpr_fish_depletion) |>
#   bind_rows(.id = 'id') |>
#   janitor::clean_names()

# well 2 events did not have a negative slope and 12 more were suspect as model
#  did not show a significant slope so this is crap.


### density results -----------------------------------------------------------
# need to summarize just the sites
tab_fish_sites_sum <- left_join(
  fish_abund_prep2 |>
    select(local_name, pass_total) |>
    distinct(),


  hab_fish_collect_info |>
    select(local_name,
           ef_length_m:enclosure) |>
    distinct(),

  by = 'local_name'
) |>
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) |>
  select(site = local_name, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)

rm(
  fish_abund_prep,
  fish_abund_prep2,
  fish_abund_prep3,
  fish_nfc_tag
)


# # table to summarize ef passes done in a site
tab_fish_sites <- hab_fish_collect_info |>
  select(local_name, haul_number_pass_number, ef_seconds:enclosure) |>
  distinct() |>
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) |>
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)



hab_fish_dens <- hab_fish_indiv |>
  dplyr::filter(sampling_method == 'electrofishing') |> ##added this since we now have mt data as well!!
  mutate(area = round(ef_length_m * ef_width_m),0) |>
  group_by(local_name, method_number, haul_number_pass_number, ef_length_m, ef_width_m, ef_seconds, area, species_code, life_stage) |>
  summarise(fish_total = length(life_stage)) |>
  ungroup() |>
  mutate(density_100m2 = round(fish_total/area * 100, 1)) |>
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) |>
  mutate(site_id = paste0(site, location),
         location = case_when(location == 'us' ~ 'Upstream',
                              T ~ 'Downstream'),
         life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult')))

# priorities phase 2--------------------------------------------------------------
#load priorities
habitat_confirmations_priorities <- readr::read_csv(
  file = "data/habitat_confirmations_priorities.csv",
  #this is not necessary but we will leave.
  locale = readr::locale(encoding = "UTF-8")) |>
  dplyr::filter(!stringr::str_detect(local_name, "_ef")) |>
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) |>
  dplyr::mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1),
                hab_value = stringr::str_to_title(hab_value))



hab_site_priorities_prep <- dplyr::left_join(

  habitat_confirmations_priorities |>
    dplyr::select(local_name, priority),

  hab_site |>
    dplyr::select(reference_number, alias_local_name, site, utm_zone:utm_northing),

  by = c('local_name' = "alias_local_name")

) |>
  dplyr::filter(!stringr::str_detect(local_name, '_ds') &
           # ends in a number
             !stringr::str_detect(local_name,'\\d$')) |>
  dplyr::filter(!is.na(priority)) |>  ##this is how we did it before.  changed it to get a start on it
  # !!!!edit 2024 - due to changes in hab priorities spreadsheet creation we now need to rename our alias_local_name to keep the resto of the scripts the dame
  dplyr::rename(alias_local_name = local_name)

hab_site_priorities <- left_join(
  hab_site_priorities_prep |>
    tidyr::separate(alias_local_name, into = c('alias_local_name', 'location'), remove = T),

  pscis_phase2 |> select(pscis_crossing_id, barrier_result) |> mutate(pscis_crossing_id = as.character(pscis_crossing_id)),

  by = c('alias_local_name' = 'pscis_crossing_id')
) |>
  select(-location)




#-----------overview table------------

tab_overview_prep1 <- pscis_phase2 |>
  select(pscis_crossing_id,
         stream_name,
         road_name,
         road_tenure,
         easting,
         northing
         # habitat_value #updzte 2024 - now grabbing from priorities
         )

tab_overview_prep2 <- habitat_confirmations_priorities |>
  dplyr::filter(location == 'us') |>
  select(site, species_codes, upstream_habitat_length_m, hab_value, priority, comments) |>
  mutate(upstream_habitat_length_km = round(upstream_habitat_length_m/1000,1))

tab_overview <- left_join(
  tab_overview_prep1,
  tab_overview_prep2,
  by = c('pscis_crossing_id' = 'site')
) |>
  mutate(utm = paste0(round(easting,0), ' ', round(northing,0))) |>
  select(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Tenure = road_tenure,
         #!!!!! watch out for this column
         `UTM (9U)` = utm,
         `Fish Species` = species_codes,
         `Habitat Gain (km)` = upstream_habitat_length_km,
         `Habitat Value` = hab_value,
         Priority = priority,
         Comments = comments )


rm(tab_overview_prep1, tab_overview_prep2)

####---------habitat summary--------------------------------

tab_hab_summary <- left_join(
  hab_site |>
    #dplyr::filter out minnow trap sites because we did not do habitat surveys on these
    dplyr::filter(!stringr::str_detect(alias_local_name, 'mt')) |>
    select(alias_local_name,
           site,
           location,
           avg_channel_width_m,
           avg_wetted_width_m,
           average_residual_pool_depth_m,
           average_gradient_percent,
           total_cover),

  habitat_confirmations_priorities |>
    select(local_name,
           # site,
           # location,
           length_surveyed,
           hab_value),

  by = c('alias_local_name' = 'local_name') #c('site', 'location')
) |>
  mutate(location = case_when(
    stringr::str_detect(location, 'us') ~ stringr::str_replace_all(location, 'us', 'Upstream'),
    T ~ stringr::str_replace_all(location, 'ds', 'Downstream')
    )) |>
  arrange(site, location) |>
  select(Site = site,
         Location = location,
         `Length Surveyed (m)` = length_surveyed,
         `Channel Width (m)` = avg_channel_width_m,
         `Wetted Width (m)` = avg_wetted_width_m,
         `Pool Depth (m)` = average_residual_pool_depth_m,
         `Gradient (%)` = average_gradient_percent,
         `Total Cover` = total_cover,
         `Habitat Value` = hab_value)


# cost estimates ----------------------------------------------------------

## phase1 --------------------
#make the cost estimates
# HACK !!!!!!!!!!!!!!!!!!!!!!! turned off all cost estimate data for now


# # need to add the crossing fix, filter for our sites and customize for the waterfall sites
rd_class_surface <-  left_join(

  pscis_all |>
    select(
      pscis_crossing_id,
      my_crossing_reference,
      aggregated_crossings_id,
      stream_name,
      road_name,
      downstream_channel_width_meters,
      barrier_result,
      fill_depth_meters,
      crossing_fix,
      habitat_value,
      recommended_diameter_or_span_meters,
      source),

  rd_class_surface_prep,

  by = c('pscis_crossing_id' = 'stream_crossing_id')
)
#   # here are the custom changes
#

tab_cost_est_prep <- left_join(
  rd_class_surface,
  select(tab_cost_rd_mult, my_road_class, my_road_surface, cost_m_1000s_bridge, cost_embed_cv),
  by = c('my_road_class','my_road_surface')
)

tab_cost_est_prep2 <- left_join(
  tab_cost_est_prep,
  select(fpr_xref_fix, crossing_fix, crossing_fix_code),
  by = c('crossing_fix')
) |>
  mutate(cost_est_1000s = case_when(
    crossing_fix_code == 'SS-CBS' ~ cost_embed_cv,
    crossing_fix_code == 'OBS' ~ cost_m_1000s_bridge * recommended_diameter_or_span_meters)
  ) |>
  mutate(cost_est_1000s = round(cost_est_1000s, 0))



##add in the model data.  This is a good reason for the data to be input first so that we can use the net distance!!
tab_cost_est_prep3 <- left_join(
  tab_cost_est_prep2,
  bcfishpass |>
    select(stream_crossing_id,
           st_network_km,
           st_belowupstrbarriers_network_km),
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) |>
  mutate(cost_net = round(st_belowupstrbarriers_network_km * 1000/cost_est_1000s, 1),
         cost_gross = round(st_network_km * 1000/cost_est_1000s, 1),
         cost_area_net = round((st_belowupstrbarriers_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1), ##this is a triangle area!
         cost_area_gross = round((st_network_km * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1)) ##this is a triangle area!


# # ##add the xref stream_crossing_id
tab_cost_est_prep4 <- left_join(
  tab_cost_est_prep3,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) |>
  mutate(stream_crossing_id = case_when(
    is.na(stream_crossing_id) ~ as.integer(pscis_crossing_id),
    T ~ stream_crossing_id
  ))

##add the priority info
tab_cost_est_phase1_prep <- left_join(
  phase1_priorities |> select(pscis_crossing_id,
                               priority_phase1),
  tab_cost_est_prep4,
  by = 'pscis_crossing_id'
) |>
  arrange(pscis_crossing_id) |>
  select(pscis_crossing_id,
         my_crossing_reference,
         my_crossing_reference,
         stream_name,
         road_name,
         barrier_result,
         habitat_value,
         downstream_channel_width_meters,
         priority_phase1,
         crossing_fix_code,
         cost_est_1000s,
         st_network_km,
         cost_gross, cost_area_gross, source) |>
  dplyr::filter(barrier_result != 'Unknown' & barrier_result != 'Passable')

# too_far_away <- tab_cost_est |> dplyr::filter(distance > 100) |> ##after review all crossing match!!!!! Baren rail is the hwy but that is fine. added source, distance, crossing_id above
#   dplyr::filter(source %like% 'phase2')

tab_cost_est_phase1 <- tab_cost_est_phase1_prep |>
  rename(
    `PSCIS ID` = pscis_crossing_id,
    `External ID` = my_crossing_reference,
    Priority = priority_phase1,
    Stream = stream_name,
    Road = road_name,
    Result = barrier_result,
    `Habitat value` = habitat_value,
    `Stream Width (m)` = downstream_channel_width_meters,
    Fix = crossing_fix_code,
    `Cost Est ( $K)` =  cost_est_1000s,
    `Habitat Upstream (km)` = st_network_km,
    `Cost Benefit (m / $K)` = cost_gross,
    `Cost Benefit (m2 / $K)` = cost_area_gross) |>
  dplyr::filter(!stringr::str_detect(source, 'phase2')) |>
  select(-source)

## phase2 --------------------
tab_cost_est_prep4 <- left_join(
  tab_cost_est_prep3,
  habitat_confirmations_priorities |>
    dplyr::filter(location == 'us') |>
    select(
    site,
    hab_value,
    upstream_habitat_length_m),
  by = c('pscis_crossing_id' = 'site')
) |>
  mutate(cost_net = round(upstream_habitat_length_m * 1000/cost_est_1000s, 1),
         cost_area_net = round((upstream_habitat_length_m * 1000 * downstream_channel_width_meters * 0.5)/cost_est_1000s, 1)) ##this is a triangle area!

tab_cost_est_prep5 <- left_join(
  tab_cost_est_prep4,

  hab_site |>
    dplyr::filter(
      !stringr::str_detect(alias_local_name,'ds') &
        !stringr::str_detect(alias_local_name, 'ef') &
        !stringr::str_detect(alias_local_name, '\\d$')) |>
    select(site,
           avg_channel_width_m),
  by = c('pscis_crossing_id' = 'site')
) |>
  # update 2024 - swapped priorities hab value
  select(-habitat_value)

##add the priority info
tab_cost_est_phase2 <- tab_cost_est_prep5 |>
  dplyr::filter(stringr::str_detect(source, 'phase2')) |>
  dplyr::filter(barrier_result != 'Unknown' & barrier_result != 'Passable') |>
  select(pscis_crossing_id,
         stream_name,
         road_name,
         barrier_result,
         # update 2024 - swapped priorities value
         habitat_value = hab_value,
         avg_channel_width_m,
         crossing_fix_code,
         cost_est_1000s,
         upstream_habitat_length_m,
         cost_net,
         cost_area_net,
         source) |>
  mutate(upstream_habitat_length_m = round(upstream_habitat_length_m,0))

tab_cost_est_phase2_report <- tab_cost_est_phase2 |>
  dplyr::arrange(pscis_crossing_id) |>
  # dplyr::filter(source %like% 'phase2') |>
  rename(`PSCIS ID` = pscis_crossing_id,
         Stream = stream_name,
         Road = road_name,
         Result = barrier_result,
         `Habitat value` = habitat_value,
         `Stream Width (m)` = avg_channel_width_m,
         Fix = crossing_fix_code,
         `Cost Est (in $K)` =  cost_est_1000s,
         `Habitat Upstream (m)` = upstream_habitat_length_m,
         `Cost Benefit (m / $K)` = cost_net,
         `Cost Benefit (m2 / $K)` = cost_area_net) |>
  select(-source)


rm(tab_cost_est_prep, tab_cost_est_prep2,
   tab_cost_est_prep3, tab_cost_est_prep4, tab_cost_est_prep5)


# map tables --------------------------------------------------------------
hab_loc_prep <- left_join(
  hab_loc |>
    tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) |>
    dplyr::filter(!stringr::str_detect(alias_local_name,'ef') &
             location == 'us') |>
    mutate(site = as.integer(site)),

  habitat_confirmations_priorities |>
    dplyr::filter(location == 'us'),
  select(site,
         priority,
         comments),
  by = 'site'
)


#need to populate the coordinates before this will work
###please note that the photos are only in those files ecause they are referenced in other parts
#of the document
tab_hab_map <- left_join(
  tab_cost_est_phase2 |>
    dplyr::filter(stringr::str_detect(source,'phase2')),

  hab_loc_prep |>
    select(site, priority, utm_easting, utm_northing, comments),
  by = c('pscis_crossing_id' = 'site')
)  |>
  sf::st_as_sf(coords = c("utm_easting", "utm_northing"),
               crs = 26909, remove = F) |>
  sf::st_transform(crs = 4326) |>
  ##changed this to docs .html from fig .png
  # mutate(data_link = paste0('<a href =',
  #                           'https://github.com/NewGraphEnvironment/fish_passage_bulkley_2020_reporting/tree/master/docs/sum/', pscis_crossing_id,
  #                           '.html', '>', 'data link', '</a>')) |>
  mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) |>
  # mutate(photo_link = paste0('<a href =', 'data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
  #                            'target="_blank">Culvert Photos</a>')) |>
  mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>')) |>
  # mutate(photo_link = paste0('<a href =',
  #                            'https://github.com/NewGraphEnvironment/fish_passage_skeena_2021_reporting/tree/master/data/photos/', pscis_crossing_id,
  #                            '/crossing_all.JPG', '>', 'photo link', '</a>')) |>
  mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/fish_passage_skeena_2022_reporting/master/data/photos/', pscis_crossing_id, '/crossing_all.JPG ',
                             'target="_blank">Culvert Photos</a>'))


# #--------------need to review if this is necessary
tab_map_prep <- left_join(
  pscis_all |>
    fpr::fpr_sp_assign_sf_from_utm() |>
    sf::st_transform(crs = 4326), ##convert to match the mapping format

  phase1_priorities |>
    select(-utm_zone:utm_northing, -my_crossing_reference, priority_phase1, -habitat_value, -barrier_result), # |> st_drop_geometry()
  by = 'pscis_crossing_id'
)


tab_map <- tab_map_prep |>
  mutate(priority_phase1 = case_when(priority_phase1 == 'mod' ~ 'moderate',
                                     T ~ priority_phase1)) |>
  mutate(data_link = paste0('<a href =', 'sum/cv/', pscis_crossing_id, '.html ', 'target="_blank">Culvert Data</a>')) |>
  mutate(photo_link = paste0('<a href =', 'https://raw.githubusercontent.com/NewGraphEnvironment/', repo_name, '/master/data/photos/', my_crossing_reference, '/crossing_all.JPG ',
                             'target="_blank">Culvert Photos</a>')) |>
  mutate(model_link = paste0('<a href =', 'sum/bcfp/', pscis_crossing_id, '.html ', 'target="_blank">Model Data</a>')) |>
  dplyr::distinct(site_id, .keep_all = T) #just for now






