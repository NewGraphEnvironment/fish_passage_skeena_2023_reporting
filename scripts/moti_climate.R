# extract moti climate change data from mergin field form and make table to insert into reporting

source('scripts/packages.R')
source('scripts/tables.R')

# read csv that has column names we want
xref_moti_climate <- read_csv(file = 'data/inputs_raw/xref_moti_climate.csv')

# read in data from mergin form, contains all skeena data as well so need to filter out bulk sites
moti_site_data <- read_csv(file = 'data/dff/form_pscis_moti_20230511.csv') %>%
  rename(moti_chris_culvert_id = chris_culvert_id) %>%
  # add the condition and erosion columns
  select(any_of(xref_moti_climate %>% pull(spdsht)), contains('erosion'), contains('embankment')) %>%
  # two phase 2 sites (pinenut and sterritt) had my crossing ref ids input incorrectly into the pscis id column in the form csv
  mutate(my_crossing_reference = case_when(pscis_crossing_id == 8302257 ~ 8302257, T ~ my_crossing_reference),
         my_crossing_reference = case_when(pscis_crossing_id == 8300128 ~ 8300128, T ~ my_crossing_reference)) %>%
  # sandstone creek site had incorrect my crossing ref
  mutate(pscis_crossing_id = case_when(my_crossing_reference == 4600913 ~ 8530, T ~ pscis_crossing_id)) %>%
  # pull out sites that match pscis ids or my crossing references from skeena repo object
  # this seems a bit strange.. first section said keep if the pscis id is there or there is not a my_crossing_ref... why? works to remove Cullen but maybe coudl just remove cullen explicitly?
  filter(pscis_crossing_id %in% (pscis_all %>% pull(pscis_crossing_id))|
        !is.na(my_crossing_reference) &
        my_crossing_reference %in% (pscis_all %>% pull(my_crossing_reference))) %>%
  # condition and erosion columns were renamed condition_issues and erosion_issues
  mutate(erosion_issues = case_when(!is.na(erosion) ~ erosion, T ~ erosion_issues),
         embankment_fill_issues = case_when(!is.na(embankment_issues) ~ embankment_issues, T ~ embankment_fill_issues)) %>%
  select(-erosion, -embankment_issues, -contains('photo')) %>%
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

names(moti_data_cleaned)
xref_moti_climate %>% pull(report)

# test to see the order is right - set_names seems risky perhaps... maybe its fine
try <- tibble(
  spdsht = names(moti_data_cleaned),
  report = xref_moti_climate %>% pull(report)
)

# all good

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

# burn to a csv to make changes in a reasonably quick manner.  Copy file and use it to read in
tab_moti_prep %>%
write_csv('data/inputs_extracted/tab_moti_prep_20230605.csv')

# read it back in cleaned up
tab_moti_prep <- read_csv(file = 'data/inputs_raw/moti_climate_tidied_hand.csv')

# make table for phase 1 sites to insert into report
tab_moti_phase1 <- tab_moti_prep %>%
  filter(my_crossing_reference %in% (pscis_phase1 %>% pull(my_crossing_reference))) %>%
  purrr::set_names(nm = xref_moti_climate %>% pull(report))%>%
  # filter out sites that aren't from moti
  filter(!is.na(moti_chris_culvert_id)) %>%
  mutate(stream_name = str_replace_all(stream_name, "Trib ", "Tributary "),
         stream_name = case_when(pscis_crossing_id == 198186 ~ "Tributary to Kispiox River", T ~ stream_name),
         stream_name = case_when(pscis_crossing_id == 198201 ~ "Tributary to Tea Creek", T ~ stream_name),
         stream_name = case_when(pscis_crossing_id == 198215 ~ "Dale Creek", T ~ stream_name)
         )


# make table for phase 2 sites to insert into report
tab_moti_phase2 <- tab_moti_prep %>%
  filter(pscis_crossing_id %in% (pscis_phase2 %>% pull(pscis_crossing_id))) %>%
         #|pscis_crossing_id == 198200) %>% # this is a phase 2 site on tea creek, but we used the pscis id of culvert on the highway in phase 2 spreadsheet
  mutate(stream_name = case_when(str_detect(stream_name, "Trib to Kitwanga") ~ "Tea Creek", T ~ stream_name)) %>%
  # Tea Creek highway structure has a different moti id
  mutate(moti_chris_culvert_id = case_when(pscis_crossing_id == 198220 ~ 4092, T ~ moti_chris_culvert_id)) %>%
  mutate(stream_name = case_when(pscis_crossing_id == 198220 ~ "Tea Creek", T ~ stream_name)) %>%
  # filter out sites that aren't from moti
  filter(!is.na(moti_chris_culvert_id))

