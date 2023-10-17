# this script gets a total count of fish passage field assessments done

# xref my_crossings pscis, pull records from bcdata using record ID


# the purpose of this move is to get the "package_id" using the name of the layer from the BC data catalougue url. you can see the catalouge with bcdc_browse()
get_this <- bcdata::bcdc_tidy_resources('7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)

# see which consultants are options
unique(dat$CONSULTANT_NAME)

## xref_pscis_my_crossing_modelled
xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(consultant_name == "IRVINE") %>%
  sf::st_drop_geometry() %>%
  tidyr::separate(assessment_date, into = c('year', 'month', 'day'), remove = F)

# total count by year for all pscis assessments and habitat confirmations (before unsubmitted 2022 sites added)
xref_total_count <- xref_pscis_my_crossing_modelled %>%
  group_by(year) %>%
  summarise(total = n())


# what are the names of the projects and which ones match the search?
unique(xref_pscis_my_crossing_modelled$funding_project)

xref_pscis_my_crossing_modelled %>%
  filter(str_detect(funding_project, 'Fish Habitat Confirmations|Phase 2')) %>%
  distinct(funding_project)

# total count by year for all phase 2 habitat confirmations
hab_con_count <- xref_pscis_my_crossing_modelled %>%
  filter(str_detect(funding_project, 'Fish Habitat Confirmations|Phase 2')) %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  # add missing phase 2s from 2022
  # skeena: 9
  # parsnip: 4
  # bulkley: 7
  add_row(year = '2022', total = 20)
  # mutate(year = case_when(is.na(year) ~ '2022', T ~ year)) %>%
  # mutate(total = case_when(year == '2022' ~ 20, T ~ total))

# another way to get the hab cons is to download that layer. Just remembering how this works. You can use the end of the url from bcdata catalougue to download the layer
get_this <- bcdata::bcdc_tidy_resources('pscis-habitat-confirmations') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat2 <- bcdata::bcdc_get_data(get_this)
hab_con_prep <- dat2 %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(assmt_consultant_name == "IRVINE") %>%
  sf::st_drop_geometry() %>%
  tidyr::separate(assmt_date, into = c('year', 'month', 'day'), remove = F)


hab_con <-hab_con_prep %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  add_row(year = '2022', total = 20) %>%
  rename(`Phase 2` = total)

# total count by year for all pscis assessments
pscis_count <- xref_pscis_my_crossing_modelled %>%
  # we can leave the "phase 2s in as they are still "phase 1"s as well
  # filter(!str_detect(funding_project, 'Fish Habitat Confirmations|Phase 2')) %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  # add missing reassessments from 2022 (all phase 1s have been submitted)
  # skeena: 7
  # parsnip: 5
  # bulkley: 5
  # elk: 2
  mutate(total = case_when(year == '2022' ~ total + 19, T ~ total)) %>%
  rename(`Phase 1` = total)

# filter out records that have been submitted recently to figure out which assessments have not been submitted yet
# and are therefore not included in the total count yet
# current_count <- xref_pscis_my_crossing_modelled %>%
#   filter(year == '2022')

# make some summaries of what was supposed to be submitted vs what actually has been so we can clean up the mess
sum <- left_join(
  xref_pscis_my_crossing_modelled %>%
  filter(str_detect(funding_project, 'Fish Habitat Confirmations|Phase 2')) %>%
  group_by(year, funding_project) %>%
  summarise(total = n()),

  hab_con_prep %>%
    group_by(year, assmt_funding_project) %>%
    summarise(total = n()),

  by = c('funding_project' = 'assmt_funding_project')
)

# aha - looks like some sort of double submission for Parsnip phase2s in 2019.  I can't remember what happened at all
test <- xref_pscis_my_crossing_modelled %>%
  filter(str_detect(funding_project, 'Parsnip River Watershed – Fish Habitat Confirmations'))

n_distinct(test$stream_crossing_id)
# hmm - coming back to me...
unique(xref_pscis_my_crossing_modelled$funding_project)

# right - so there was no submission named phase 1

# a couple descrepencies with 2019 parsnip and 2020 elk where there maybe should be one more site... weird as I am pretty sure those
# are both fully submitted and I am surprised it allowed be to finish. Maybe - I haveent actually fully finished the submsissions and
# things are dangling
# 2021 bulkley/morr makes sense because we still need to finish those submissions and finalize entry. Apparently phase 3s and 4s can't be submitted until
# that happens for sites even if they are already showing in the hab con.  CWF has been whining about that

#so overall count is

pscis_sum <- left_join(
  pscis_count,

  hab_con,

  by = 'year'

) %>%
  mutate(total = rowSums(across(c(`Phase 1`, `Phase 2`)))) %>%
  arrange(year)


