# extract moti climate change data from mergin field form and make table to insert into reporting

xref_moti_climate_names <- tibble::tribble(
                                                                 ~spdsht,                                                                                                               ~report, ~description, ~id_join, ~id_side,
                                                     "pscis_crossing_id",                                                                                                   "pscis_crossing_id",          "–",      "–",      "–",
                                                 "my_crossing_reference",                                                                                               "my_crossing_reference",          "–",      "–",      "–",
                                                          "crew_members",                                                                                   "Crew Members Seperate with Spaces",          "–",      "–",      "–",
                                                 "moti_chris_culvert_id",                                                                                               "moti_chris_culvert_id",          "–",      "–",      "–",
                                                           "stream_name",                                                                                                         "stream_name",          "–",      "–",      "–",
                                                             "road_name",                                                                                                           "road_name",          "–",      "–",      "–",
                                                        "erosion_issues",                                                                                      "Erosion (scale 1 low - 5 high)",          "–",      "9",      "1",
                                                "embankment_fill_issues",                                                                  "Embankment fill issues 1 (low) 2 (medium) 3 (high)",          "–",      "2",      "1",
                                                       "blockage_issues",                                                                      "Blockage Issues 1 (0-30%) 2 (>30-75%) 3 (>75%)",          "–",      "3",      "1",
                                                        "condition_rank",                                                                    "Condition Rank = embankment + blockage + erosion",          "–",      "4",      "1",
                                                       "condition_notes",                                                                "Describe details and rational for condition rankings",          "–",      "–",      "–",
                              "likelihood_flood_event_affecting_culvert",                                                     "Likelihood Flood Event Affecting Culvert (scale 1 low - 5 high)",          "–",      "8",      "1",
                             "consequence_flood_event_affecting_culvert",                                                    "Consequence Flood Event Affecting Culvert (scale 1 low - 5 high)",          "–",      "5",      "1",
                                             "climate_change_flood_risk",                           "Climate Change Flood Risk (likelihood x consequence) 1-6 (low) 6-12 (medium) 10-25 (high)",          "–",      "6",      "1",
                                                    "vulnerability_rank",                                                                  "Vulnerability Rank = Condition Rank + Climate Rank",          "–",      "7",      "1",
                                                         "climate_notes",                                                             "Describe details and rational for climate risk rankings",          "–",      "–",      "–",
                                                        "traffic_volume",                                                                         "Traffic Volume 1 (low) 5 (medium) 10 (high)",          "–",      "9",      "2",
                                                      "community_access", "Community Access - Scale - 1 (high - multiple road access) 5 (medium - some road access) 10 (low - one road access)",          "–",      "2",      "2",
                                                                  "cost",                                                                                       "Cost (scale: 1 high - 10 low)",          "–",      "3",      "2",
                                                      "constructability",                                                                      "Constructibility (scale: 1 difficult -10 easy)",          "–",      "4",      "2",
                                                          "fish_bearing",                                                             "Fish Bearing 10 (Yes) 0 (No) - see maps for fish points",          "–",      "5",      "2",
                                                 "environmental_impacts",                                                                       "Environmental Impacts (scale: 1 high -10 low)",          "–",      "8",      "2",
                                                         "priority_rank",  "Priority Rank = traffic volume + community access + cost + constructability + fish bearing + environmental impacts",          "–",      "6",      "2",
                                                          "overall_rank",                                                                   "Overall Rank = Vulnerability Rank + Priority Rank",          "–",      "7",      "2",
                                                        "priority_notes",                                                                 "Describe details and rational for priority rankings",          "–",      "–",      "–"
                             )



form_pscis_moti <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = paste0("~/Projects/gis/sern_skeena_2023/data_field/2023/form_pscis_2023.gpkg"),
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE
) |>
  sf::st_drop_geometry()


# read in data from mergin form, contains all skeena data as well so need to filter out bulk sites
moti_site_data <- form_pscis_moti %>%
  select(any_of(xref_moti_climate_names %>% pull(spdsht)), contains('erosion'), contains('embankment')) %>%
  # pull out sites that match pscis ids or my crossing references from skeena repo object
  filter(pscis_crossing_id %in% (pscis_all %>% pull(pscis_crossing_id))|
        !is.na(my_crossing_reference) &
        my_crossing_reference %in% (pscis_all %>% pull(my_crossing_reference))) %>%
  select(-contains('photo')) %>%
  arrange(my_crossing_reference)


# little tests
# test <- moti_site_data %>%
#   select(date, source, stream_name, contains('erosion'), contains('embankment'))

# some pscis ids and crossing references were input wrong in mergin form, see which sites are missing by comparing to skeena repo pscis object

# see_diff <- pscis_all %>%
#   filter(!pscis_crossing_id %in% (moti_site_data %>% pull(pscis_crossing_id))&
#            !my_crossing_reference %in% (moti_site_data %>% pull(my_crossing_reference)))
# none of them are culverts, so they don't have climate data

# filter out sites that don't have overall climate ranks, filter out duplicate sites that were not done by Mateo or AI (keep Tieasha's Perow site)
moti_data_cleaned <- moti_site_data %>%
  # some have climate data but no rank....
  filter(!is.na(priority_rank)) #%>%
  #filter(str_detect(crew_members, "newgraph_airvine|MateoW")) #%>%


# add the xref pscis id
tab_moti_prep <- left_join(
  moti_data_cleaned,
  xref_pscis_my_crossing_modelled,
  by = c('my_crossing_reference' = 'external_crossing_reference')
) %>%
  mutate(stream_crossing_id = case_when(
    is.na(stream_crossing_id) ~ as.integer(pscis_crossing_id),
    T ~ stream_crossing_id)) %>%
  select(-pscis_crossing_id, pscis_crossing_id = stream_crossing_id) %>%
  relocate(pscis_crossing_id) %>%
  # have to add erosion to the condition rank, have to update every added rank field unfortunately (except priority rank)
  # can remove this chunk in future when math is updated in mergin form template
  mutate(condition_rank = erosion_issues + embankment_fill_issues + blockage_issues) %>%
  mutate(vulnerability_rank = condition_rank + climate_change_flood_risk) %>%
  mutate(overall_rank = vulnerability_rank + priority_rank)

# # burn to a csv to make changes in a reasonably quick manner.  Copy file and use it to read in
# tab_moti_prep %>%
# write_csv('data/inputs_extracted/tab_moti_prep_20230605.csv')
#
# # read it back in cleaned up
# tab_moti_prep <- read_csv(file = 'data/inputs_raw/moti_climate_tidied_hand.csv')

# make table for phase 1 sites to insert into report
tab_moti_phase1 <- tab_moti_prep %>%
  filter(my_crossing_reference %in% (pscis_phase1 %>% pull(my_crossing_reference))) %>%
  purrr::set_names(nm = xref_moti_climate_names %>% pull(report))
  # filter out sites that aren't from moti - NOPE lets leave them all
  # filter(!is.na(moti_chris_culvert_id))


# make table for phase 2 sites to insert into report
tab_moti_phase2 <- tab_moti_prep %>%
  filter(pscis_crossing_id %in% (pscis_phase2 %>% pull(pscis_crossing_id)))
  # filter out sites that aren't from moti - lets leave in and note in results!
  # filter(!is.na(moti_chris_culvert_id))

