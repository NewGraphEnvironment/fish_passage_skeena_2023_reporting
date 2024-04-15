# Export pscis data to csv for cut and paste into PSCIS submission spreadsheet


# assign path of where field data is
path <- "~/Projects/gis/sern_skeena_2023/data_field/2023/form_pscis_2023.gpkg"


# read in cleaned form from Q after review and finalization
# first we back up the gpkg in the repo and update the coordinate columns in the gpkg in QGIS
pscis_export_raw <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = path,
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
  # csv is versioned
  write_to_rdata = FALSE,
  return_object = TRUE)

################################################################################################################
#---------------------------------------------START HACK OUT OF STEP---------------------------------------------------
################################################################################################################
# # should be addressed in form through https://github.com/NewGraphEnvironment/dff-2022/issues/119#issuecomment-1781242709
# #Define Phase-----------------------------------------------------------------------------------------------------
# # something we should do in the field but was not done so we will case_when
# reassessment <- c( "197365", #Tributary to Owen Creek
#                    "197640", #trib to buck
#                    "198225", #sterritt
#                    "58067", #gramaphone
#                    "8518", #White Swan Creek
#                    "198217", # sik-e-dakh - glen vowell
#                    "198215", # Dale Creek
#                    "123377", # thompson
#                    "124500", # helps
#                    "197360" # reddick - no cv ass
# )
#
# # these are ones with my_crossing_reference so we need to delegate by hand
# p2 <- c(
#   #trib to houston Tommy
#   "14000571",
#   "14001106",
#
#   # trib to Zymoetz - little nasty
#   "24601280")
#
# pscis_export_raw <- pscis_export_raw |>
#   # because we are doing some out of step moves we will populate a rowid column in order to try to minimize our diffs
#   # in mergin for the geopackage when we burn it back to Q. It likely explodes it and it can't track the table changeing
#   # but at least the row events are in the same place.  Will be interesting to see rowid order when we read it back in
#   #  It was there already unpopulated but should not be as it is explicitly removed when we make the form (currently anyway)
#   # - https://github.com/NewGraphEnvironment/dff-2022/blob/master/scripts/pscis_build_form.R
#   dplyr::select(-rowid) |>
#   tibble::rowid_to_column() |>
#   dplyr::relocate(rowid, .after = length_or_width_meters) |>
#   dplyr::mutate(pscis_phase = dplyr::case_when(pscis_crossing_id %in% reassessment ~ "reassessment",
#                                                !is.na(pscis_crossing_id) & !pscis_crossing_id %in% reassessment ~ "phase2",
#                                                my_crossing_reference %in% p2 ~ "phase2",
#                                                TRUE ~ NA_character_)) |>
#   # reordering is just for viewing.  We will leave out in build to minimize diffs
#   dplyr::select(pscis_phase, stream_name, everything()) |>
#   # we are going to fix a couple things here to clean up the file - shouldn't be an issue in the future
#   dplyr::select(-X.rowid, -x, -y)
#
# # after testing to confirm accuracy - sort back using the rowid (to reduce diff in mergin) - remove that rowid and burn
# # back to the gpkg so we have the new info in Q
# pscis_export_raw |>
#   dplyr::arrange(rowid) |>
#   dplyr::select(-rowid) |>
#   # BURNING BACK TO GPKG
#   sf::st_write(dsn = path, append = FALSE, delete_dsn = TRUE)
#
# # track the changes with informative commit message and inform our newly updated form
# pscis_export_raw <- fpr::fpr_sp_gpkg_backup(
#   path_gpkg = path,
#   update_utm = TRUE,
#   update_site_id = TRUE,
#   write_back_to_path = TRUE,
#   write_to_csv = TRUE,
#   # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
#   # csv is versioned
#   write_to_rdata = TRUE,
#   return_object = TRUE)

################################################################################################################
#---------------------------------------------END HACK OUT OF STEP---------------------------------------------------
################################################################################################################

# Time issue-----------------------------------------------------------------------------------------------------
# Skeena 2023 PSCIS form has the incorrect time and needs to have 7hrs added to be in PDT. This is a temporary fix.
# First double check the repaired time is correct
pscis_correct_time_check <- pscis_export_raw %>%
  # there is perhaps a more reliable method for this issue in
  # Seems like  more reliable method way to do would be to use force_tz and with_tz since it takes into account daylight savings time
  # https://github.com/NewGraphEnvironment/fish_passage_skeena_2023_reporting/issues/49
  dplyr::mutate(date_time_start_repaired = date_time_start + hours(7)) %>%
  dplyr::relocate(date_time_start_repaired, .after = date_time_start)

# If correct -  replace - we will replace object to avoid confusino in future when we hopefully don't need this step
pscis_export_raw <- pscis_correct_time_check %>%
  dplyr::mutate(date_time_start = date_time_start_repaired) %>%
  dplyr::select(-date_time_start_repaired)


# DON'T run this more than once because you will end up with duplicate text appended on...
pscis_export <- pscis_export_raw %>%
  # Get time to append to comments
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
                date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # append moti ids to comments, differentiate between highway major structure, and add time to end
  mutate(assessment_comment = case_when(
    moti_chris_culvert_id > 1000000 ~ paste0(assessment_comment, ' MoTi chris_culvert_id: ', moti_chris_culvert_id),
    moti_chris_culvert_id < 1000000 ~ paste0(assessment_comment, ' MoTi chris_hwy_structure_road_id: ', moti_chris_culvert_id),
    TRUE ~ assessment_comment)) %>%
  mutate(assessment_comment = case_when(moti_chris_culvert_id2 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id2), TRUE ~ assessment_comment)) %>%
  mutate(assessment_comment = case_when(moti_chris_culvert_id3 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id3), TRUE ~ assessment_comment)) %>%
  # add time to end
  mutate(assessment_comment = paste0(assessment_comment, '. ', time)) %>%
  # ditch time column
  select(-time) |>
  dplyr::select(
    any_of(names(fpr_xref_template_pscis())),
    pscis_phase,
    date_time_start
    ) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result) %>%
  sf::st_drop_geometry() %>%
  # arrange by phase so easy to copy/paste into correct spreadsheet
  arrange(pscis_phase,
          crew_members,
          date_time_start)


# write to the imports_extracted dir. This is data we import to the project but they are extracted from other places.
dir.create("data/inputs_extracted")

pscis_export %>%
  readr::write_csv('data/inputs_extracted/pscis_export_submission.csv', na='')

