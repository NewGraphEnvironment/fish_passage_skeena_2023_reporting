# Export pscis data to csv for cut and paste into PSCIS submission spreadsheet

# read in cleaned form from Q after review and finalization
# first we back up the gpkg in the repo and update the coordinate columns in the gpkg in QGIS
pscis_export_raw <- fpr_sp_gpkg_backup(
  path_gpkg = "~/Projects/gis/sern_skeena_2023/data_field/2023/form_pscis_2023.gpkg",
  update_utm = TRUE,
  update_site_id = TRUE,
  write_back_to_path = TRUE,
  write_to_csv = TRUE,
  # this versions on git everytime due to metadata and can't be tracked visually. Should only be committed when
  # csv is versioned
  write_to_rdata = TRUE,
  return_object = TRUE)

# Skeena 2023 PSCIS form has the incorrect time and needs to have 7hrs added to be in PDT. This is a temporary fix.
# First double check the repaired time is correct
pscis_correct_time_check <- pscis_export_raw %>%
  dplyr::mutate(date_time_start_repaired = date_time_start + hours(7)) %>%
  dplyr::relocate(date_time_start_repaired, .after = date_time_start)

# Then replace
pscis_correct_time <- pscis_correct_time_check %>%
  dplyr::mutate(date_time_start = date_time_start_repaired) %>%
  dplyr::select(-date_time_start_repaired)


# DON'T run this more than once because you will end up with duplicate text appended on...
pscis_export <- pscis_correct_time %>%
  # Get time to append to comments
  dplyr::mutate(date_time_start = lubridate::ymd_hms(date_time_start),
                date = lubridate::date(date_time_start),
                time = hms::as_hms(date_time_start)) %>%
  # append moti ids to comments, differentiate between highway major structure, and add time to end
  mutate(assessment_comment = case_when(
    moti_chris_culvert_id > 1000000 ~ paste0(assessment_comment, ' Ministry of Transportation chris_culvert_id: ', moti_chris_culvert_id),
    moti_chris_culvert_id < 1000000 ~ paste0(assessment_comment, ' Ministry of Transportation chris_hwy_structure_road_id: ', moti_chris_culvert_id),
    TRUE ~ assessment_comment)) %>%
  mutate(assessment_comment = case_when(moti_chris_culvert_id2 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id2), TRUE ~ assessment_comment)) %>%
  mutate(assessment_comment = case_when(moti_chris_culvert_id3 > 1000000 ~ paste0(assessment_comment, ', ', moti_chris_culvert_id3), TRUE ~ assessment_comment)) %>%
  # add time to end
  mutate(assessment_comment = paste0(assessment_comment, '. ', time)) %>%
  # ditch time column
  select(-time)

# prep for csvs for cut and paste by subsetting columns to those in spreadsheet
pscis_export_final <- pscis_export %>%
  # only select columns from template object site_id and date_time_start
  dplyr::select(
    any_of(names(fpr_xref_template_pscis())),
    site_id,
    date_time_start,
    source
    ) %>%
  # remove scoring columns, as these can't be copied and pasted anyways because of macros
  dplyr::select(-stream_width_ratio:-barrier_result) %>%
  sf::st_drop_geometry() %>%
  # arrange by phase so easy to copy/paste into correct spreadsheet
  arrange(source)


# write to the imports_extracted dir. This is data we import to the project but they are extracted from other places.
dir.create("data/inputs_extracted")

pscis_export_final %>%
  readr::write_csv(paste0('data/inputs_extracted/pscis_export_submission.csv'), na='')

