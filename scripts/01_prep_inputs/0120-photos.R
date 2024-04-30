# #NOTE: there is a missing step here where the photos were transfered to onedrive and resized.  Not a manual
# step but documentation may have been lacking when it was done.


# define our shared photos location (this is where they live when they are all sorted and resized)
dir_photos_shared = "~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/"


# import form_pscis.gpkg direct from mergin
dir_project <- 'sern_skeena_2023'


# NOTE - this was originally run with `form_pscis.gpkg` and sf::st_read in main directory but altered after the fact for QA down
form_pscis <- fpr::fpr_sp_gpkg_backup(
  path = paste0('~/Projects/gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg'),
  update_site_id = TRUE,
  write_to_csv = FALSE,
  write_to_rdata = FALSE,
  return_object = TRUE
) |>
  arrange(site_id)

# make photo directories ------------------------------------------------------------

##create the data and photos folder (recursive param) ON ONEDRIVE yo
# replace url of onedrive below
dir.create('onedriveurl/data/photos', recursive = T)

# check for duplicate sites
form_pscis %>%
  dplyr::filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  dplyr::filter(n()>1) %>%
  nrow()

# check for empty sites
form_pscis %>%
  dplyr::filter(is.na(site_id)) %>%
  nrow()

# create folders ON ONEDRIVE by changing the "path" param yo!!!!! ?fpr::fpr_photo_folders
form_pscis %>%
  pull(site_id) %>%
  as.character() %>%
  purrr::map(fpr::fpr_photo_folders, path = 'onedriveurl/data/photos')

# ----------------mergin photos-----------------------
## ------------ resize mergin photos -------------------

# resize the photos and change the extension to JPG for consistency and unforseen issues
# sync to mergin after copying to new dir (resized) and removing originals
# record version number of mergin project in issue for now to track
file.remove('/Users/airvine/Projects/gis/sern_skeena_2023/ignore_mobile/photos/photos.txt')

# resize the photos and keep them on the server for now for collaboration
fpr_photo_resize_batch(
  dir_source = '/Users/airvine/Projects/gis/sern_skeena_2023/ignore_mobile/photos/',
  dir_target = '/Users/airvine/Projects/gis/sern_skeena_2023/ignore_mobile/photos_resized/')

# could erase photos here but will do by hand for safety
# recreate the photos.txt file so the form still works
file.create('/Users/airvine/Projects/gis/sern_skeena_2023/ignore_mobile/photos/photos.txt')

# back the photos up to onedrive (should remove all from mergin)
fpr::fpr_photo_resize_batch(
  dir_source = '/Users/airvine/Projects/gis/sern_skeena_2023/ignore_mobile/photos/photos_resized/',
  dir_target = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/mergin/')

# for now I just removed all the ignore_mobile/photos/ and added back the photos.txt so the fieldform is still functional - could script later

# rename the photos from the FISS cards
fpr::fpr_photo_rename(
  dat = form_fiss_site_raw,
  dir_from_stub = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/mergin/',
  dir_to_stub = '/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/',
  col_string_add = TRUE)

# used fpr_photo_remove_dupes to get rid of the first dupes
photos_dry_run <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/')
photos_dry_run3 <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/',
                                               min_replicates = 3)



# actually run the removal of the first un-renamed photo
# fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/',
#                             dry_run = F)

photos_dry_run_after <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/')
photos_dry_run3_after <- fpr::fpr_photo_remove_dupes('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/',
                                               min_replicates = 3)

save(photos_dry_run, photos_dry_run3, photos_dry_run_after, photos_dry_run3_after, file = "data/inputs_extracted/photos_dry_run.RData")

# need to resize the photo that was not resized as per https://github.com/NewGraphEnvironment/fish_passage_skeena_2023_reporting/issues/55
error_unrezised_perhaps <- magick::image_read("/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/8801379/TimePhoto_20230926_110112_downstream.jpg")
magick::image_info(error_unrezised_perhaps)

# resize in place
fpr::fpr_photo_resize_convert(photo = "/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/8801379/TimePhoto_20230926_110112_downstream.jpg",
                         path = "/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/8801379/")

resized_image <- magick::image_read("/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/8801379/TimePhoto_20230926_110112_downstream.jpg")
magick::image_info(resized_image)


# same error... lets look at an amalgamated photo for clues
fpr_photo_amalg_cv(site_id = 8801379,
                   dir_photos = "~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos/")

# what a nightmare.  needed to replace.  pretty sure this was the result of point and click windows crap to resize this
# photo that was added after the fact - again see https://github.com/NewGraphEnvironment/fish_passage_skeena_2023_reporting/issues/55

#QA renamed photos-----------------------------------------------------------------------------------------------------
# check for missing photos/duplicates with the spreadsheets as the input (default)
fpr::fpr_photo_qa_df(dir_photos = dir_photos_shared)

# check for missing photos/duplicates with our imported gpkg built dataframe
fpr::fpr_photo_qa_df(dat = form_pscis, dir_photos = dir_photos_shared)

# make sure the site_id's in the two locations are the same.  If there not use waldo::compare to sleuth it out
identical(
  sort(form_pscis$site_id),

  sort(fpr::fpr_import_pscis_all() |>
         dplyr::bind_rows() |>
         distinct(site_id) |>
         pull(site_id))
  )


# build photo amalgamation for each site ------------------------------------------------
# get a list of sites to burn
sites_l <- fpr::fpr_import_pscis_all() %>%
  bind_rows() %>%
  distinct(site_id) %>%
  arrange(site_id) %>%
  pull(site_id)

# burn the amal photos to onedrive
sites_l %>%
  purrr::map(fpr::fpr_photo_amalg_cv, dir_photos = dir_photos_shared)

# Find sites that have directories but do not have an entry in the PSCIS spreadsheets
setdiff(
  list.dirs(dir_photos_shared, full.names = F, recursive = F),

  pscis_all %>%
    distinct(site_id) %>%
    arrange(site_id) %>%
    # head() %>% #test
    pull(site_id)
)

# result is [1] "123379" "197360" "197378" "197379" "197912" "198060"


#make Phase 2 photo directories-----------------------------------------------------------------------------------------------------
# for this step we copy phase 1 directories that have phase 2 events and give then the PSCIS ID. We need to do this
# to complete the reporting
path <- "~/Library/CloudStorage/OneDrive-Personal/Projects/2023_data/skeena/photos"

# define our phase 1 project to speed up our call to the bc catalougue api
funding_project_number <- 'skeena_2023_Phase1'

##use the pscis spreadsheet to make the folders to copy the photos to (d is dat)
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')

# build our xref to get pscis ids - https://github.com/NewGraphEnvironment/fpr/issues/73
# note that we don't actually need to filter this at all since its a join but it is waaay faster and we don't need all 18k records so we will
xref_pscis_my_crossing_modelled <- rfp::rfp_bcd_get_data(
  bcdata_record_id = "WHSE_FISH.PSCIS_ASSESSMENT_SVW",
  col_filter = 'FUNDING_PROJECT_NUMBER',
  # this part is project specific!
  col_filter_value = funding_project_number,
  col_extract = c('EXTERNAL_CROSSING_REFERENCE', 'STREAM_CROSSING_ID'),
  drop_geom = TRUE
)

pscis_new_sites <- dplyr::left_join(
  d,
  xref_pscis_my_crossing_modelled,
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  dplyr::filter(!is.na(external_crossing_reference))

folderstocopy <- pscis_new_sites$external_crossing_reference |>
  as.character()

folders_new_names <- pscis_new_sites$pscis_crossing_id |>
  as.character()

path_to_photos <- paste0(path, folderstocopy)

folderstocreate <- paste0(path, folders_new_names)

##create the folders
lapply(folderstocreate, dir.create)


paths_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = T,
             include.dirs = T)
  # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}

photo_names_to_copy <- function(target){
  list.files(path = target,
             pattern = ".JPG$",
             recursive = TRUE,
             full.names = F,
             include.dirs = T)
  # stringr::str_subset(., 'barrel|outlet|upstream|downstream|road|inlet')
}


filestocopy_list <- path_to_photos %>%
  purrr::map(paths_to_copy)

change_file_names <- function(filestocopy, filename_before, filename_after){
  gsub(filestocopy, pattern = filename_before, replacement = filename_after)
}


filestopaste_list <- mapply(change_file_names, filestocopy_list, folderstocopy, folders_new_names)

copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}

mapply(copy_over_photos, filescopy =  filestocopy_list,
       filespaste = filestopaste_list)




#make amalgamated photos
fpr::fpr_photo_amalg_cv()
