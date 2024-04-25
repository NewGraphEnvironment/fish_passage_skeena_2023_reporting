# script to add data from habitat_confirmations.xlsm to form_fiss_2023.gpkg because edits were made in habitat_confirmations.xlsm
# Moving forward we should do a QA of form_fiss_2023.gpkg in Q then populate the habitat_confirmations.xlsm.

source('scripts/packages.R')

dir_project <- 'sern_skeena_2023'


## Import the raw form_fiss_2023.gpkg
form_fiss_site_raw <- fpr::fpr_sp_gpkg_backup(
  path_gpkg = paste0("~/Projects/gis/", dir_project, '/data_field/2023/form_fiss_site_2023.gpkg'),
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE)


## Import the habitat_confirmations.xlsm
habitat_confirmations <- fpr::fpr_import_hab_con(row_empty_remove = T)

## Weird workflow here because hand edits were done to temp, cond, and turb data in step 2 therefore it has the most up
## to date temp, cond, and turb data (this data is also in step 4 but its not up to date). Temp, cond, and turbidity
## needs to be extracted from step 2 and added to form_fiss_site_2023.gpkg.

## Extract temp, cond, turb data from step_2_fish_coll_data
fish_coll_data <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  select(local_name, temperature_c, conductivity_m_s_cm, turbidity)

## Add temp, cond, and turb data from fish_coll_data to form_fiss_site_2023.gpkg
form_fiss_prep1 <- dplyr::left_join(form_fiss_site_raw %>%
                               #remove target columns that are in form_fiss_site_raw so we can add them from fish_coll_data
                               select(-c(temperature_c, conductivity_m_s_cm, turbidity)),
                              fish_coll_data %>%
                               distinct(local_name, temperature_c, conductivity_m_s_cm, turbidity),
                              by = join_by("local_name")) %>%
  relocate(temperature_c:turbidity, .after = "location")


# Burn updated copy to QGIS project as '/data_field/2023/form_pscis_2023.gpkg'
form_fiss_prep1 %>%
  sf::st_write(paste0("~/Projects/gis/", dir_project, '/data_field/2023/form_fiss_site_2023.gpkg'), append=F, delete_dsn=T)


## Backup form_fiss_site_2023.gpkg so we can commit the Rdata file to git, update
## https://github.com/NewGraphEnvironment/fish_passage_skeena_2023_reporting/issues/60 with version number and explaination of changes
fpr::fpr_sp_gpkg_backup(
  path_gpkg = paste0("~/Projects/gis/", dir_project, '/data_field/2023/form_fiss_site_2023.gpkg'),
  update_utm = TRUE,
  update_site_id = FALSE,
  write_back_to_path = FALSE,
  write_to_csv = TRUE,
  write_to_rdata = TRUE,
  return_object = FALSE)
