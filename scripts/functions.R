# build reporting table for minnow trap surveys
tab_fish_mt <- function(sit = my_site){
    hab_fish_indiv %>%
    filter(site == sit & sampling_method == 'minnow trapping' & !is.na(life_stage)) %>%
    group_by(site, location, species_code, life_stage) %>%
    summarise(Number = n()) %>%
    ungroup() %>%
    select(Location = location,
           Species = species_code,
           Stage = life_stage,
           Number) %>%
    pivot_wider(names_from = Stage,
                values_from = Number) %>%
    mutate(Location = case_when(Location == 'us' ~ 'Upstream',
                                T ~ 'Downstream')) %>%
    arrange(Species) %>%
    replace(is.na(.), 0)
}

# take habitat confirmation xls and make a csv with the alias_local_name pasted to the comments
# so people can see which comments are linked to which site
fpr_hab_alias_to_comments <- function(target = target_dir){
  fpr_import_hab_con(col_filter_na = T,
                     row_empty_remove = T,
                     backup = FALSE) %>%
    purrr::pluck("step_4_stream_site_data") %>%
    mutate(comments = paste0('Site ', local_name, '. ', comments)) %>%
    select(reference_number, gazetted_names, local_name, comments) %>%
    write_csv(target_dir)
}

# MOTI ----------------

# set up a table for the memos that contains the moti climate change data
# make a tribble of the xref_moti_climate_template to make 2 columns in table
# read in csv, then fpr_kable the data frame, run in rmd chunk, then copy and paste table using datapasta add in "paste as tribble"
#xref_moti_climate <- read_csv(file = paste0(getwd(), '/data/inputs_extracted/xref_moti_climate_template.csv'))

xref_moti_climate_names <- tibble::tribble(
                                      ~spdsht,                                                                                                               ~report, ~description, ~id_join, ~id_side,
                          "pscis_crossing_id",                                                                                                   "pscis_crossing_id",         NA,     NA,     NA,
                      "my_crossing_reference",                                                                                               "my_crossing_reference",         NA,     NA,     NA,
                               "crew_members",                                                                                   "Crew Members Seperate with Spaces",         NA,     NA,     NA,
                      "moti_chris_culvert_id",                                                                                               "moti_chris_culvert_id",         NA,     NA,     NA,
                                "stream_name",                                                                                                         "stream_name",         NA,     NA,     NA,
                                  "road_name",                                                                                                           "road_name",         NA,     NA,     NA,
                             "erosion_issues",                                                                                      "Erosion (scale 1 low - 5 high)",         NA,     9L,     1L,
                     "embankment_fill_issues",                                                                  "Embankment fill issues 1 (low) 2 (medium) 3 (high)",         NA,     2L,     1L,
                            "blockage_issues",                                                                      "Blockage Issues 1 (0-30%) 2 (>30-75%) 3 (>75%)",         NA,     3L,     1L,
                             "condition_rank",                                                                    "Condition Rank = embankment + blockage + erosion",         NA,     4L,     1L,
                            "condition_notes",                                                                "Describe details and rational for condition rankings",         NA,     NA,     NA,
   "likelihood_flood_event_affecting_culvert",                                                     "Likelihood Flood Event Affecting Culvert (scale 1 low - 5 high)",         NA,     8L,     1L,
  "consequence_flood_event_affecting_culvert",                                                    "Consequence Flood Event Affecting Culvert (scale 1 low - 5 high)",         NA,     5L,     1L,
                  "climate_change_flood_risk",                           "Climate Change Flood Risk (likelihood x consequence) 1-6 (low) 6-12 (medium) 10-25 (high)",         NA,     6L,     1L,
                         "vulnerability_rank",                                                                  "Vulnerability Rank = Condition Rank + Climate Rank",         NA,     7L,     1L,
                              "climate_notes",                                                             "Describe details and rational for climate risk rankings",         NA,     NA,     NA,
                             "traffic_volume",                                                                         "Traffic Volume 1 (low) 5 (medium) 10 (high)",         NA,     9L,     2L,
                           "community_access", "Community Access - Scale - 1 (high - multiple road access) 5 (medium - some road access) 10 (low - one road access)",         NA,     2L,     2L,
                                       "cost",                                                                                       "Cost (scale: 1 high - 10 low)",         NA,     3L,     2L,
                           "constructability",                                                                      "Constructibility (scale: 1 difficult -10 easy)",         NA,     4L,     2L,
                               "fish_bearing",                                                             "Fish Bearing 10 (Yes) 0 (No) - see maps for fish points",         NA,     5L,     2L,
                      "environmental_impacts",                                                                       "Environmental Impacts (scale: 1 high -10 low)",         NA,     8L,     2L,
                              "priority_rank",  "Priority Rank = traffic volume + community access + cost + constructability + fish bearing + environmental impacts",         NA,     6L,     2L,
                               "overall_rank",                                                                   "Overall Rank = Vulnerability Rank + Priority Rank",         NA,     7L,     2L,
                             "priority_notes",                                                                 "Describe details and rational for priority rankings",         NA,     NA,     NA
  )

fpr_table_moti <- function(dat = tab_moti_phase2,
                           xref_table = xref_moti_climate_names,
                           site = my_site,
                           ...){
  df <- dat %>% filter(pscis_crossing_id == site)
  # build left side of table
  tab_results_left <- xref_table %>%
    filter(id_side == 1)

  tab_pull_left <- df %>%
    select(pull(tab_results_left, spdsht)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'spdsht'))

  # build right side of table
  tab_results_right <- xref_table %>%
    filter(id_side == 2)

  tab_pull_right <- df %>%
    select(pull(tab_results_right, spdsht)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'spdsht'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Condition and Climate Risk', 'Rank', 'Priority', 'Rank'))

  tab_joined %>%
    fpr_kable(caption_text = paste0('Summary of climate change risk assessment for PSCIS crossing ', site, '.'), scroll = F)

}

fpr_table_moti_comments <- function(dat = tab_moti_phase2,
                           site = my_site,
                           ...){
  tab_comments <- dat %>%
    select(pscis_crossing_id, condition_notes, climate_notes, priority_notes) %>%
    rename('Condition' = condition_notes,
           'Climate' = climate_notes,
           'Priority' = priority_notes) %>%
    pivot_longer(cols = Condition:Priority, names_to = "Category", values_to = "Comments") %>%
    filter(pscis_crossing_id == site) %>%
    select(-pscis_crossing_id)

  tab_comments %>%
    fpr_kable(caption_text = paste0('Details and rational for climate risk rankings'), scroll = F)
}

# PSCIS Submissions -------------

fpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

fpr_photo_change_name <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = path, replacement = targetdir)
}

fpr_copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}



