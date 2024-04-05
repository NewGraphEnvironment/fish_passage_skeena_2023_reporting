# #NOTE: there is a missing step here where the photos were transfered to onedrive and resized.  Not a manual
# step but documentation may have been lacking when it was done.

# make photo directories ------------------------------------------------------------

# steps
# import form_pscis.gpkg direct from mergin
dir_project <- 'sern_skeena_2023'

form_pscis <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis.gpkg')) %>%
  mutate(
  site_id = case_when(is.na(pscis_crossing_id) ~ my_crossing_reference,
                      T ~ pscis_crossing_id)
) %>%
  # remove the form making site
  filter(site_id != '12345') %>%
  arrange(site_id)

# pscis_all <- fpr::fpr_import_pscis_all() %>%
#   bind_rows()


##create the data and photos folder (recursive param) ON ONEDRIVE yo
# replace url of onedrive below
dir.create('onedriveurl/data/photos', recursive = T)

# check for duplicate sites
form_pscis %>%
  filter(!is.na(site_id)) %>%
  group_by(site_id) %>%
  filter(n()>1) %>%
  nrow()

# check for empty sites
form_pscis %>%
  filter(is.na(site_id)) %>%
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
