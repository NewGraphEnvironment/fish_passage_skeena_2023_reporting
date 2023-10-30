# grab the files from mergin and move to project using linux cmd
# mv -v ~/Projects/gis/mergin/bcfishpass_elkr_20220904/photos/* ~/Projects/current/2022-056-nupqu-elk-cwf/data/photos/mergin/originals
# mv -v ~/Projects/gis/mergin/bcfishpass_skeena_20220823-v225/photos/* ~/Projects/current/2022-049-sern-skeena-fish-passage/data/photos/mergin/


# make folders ------------------------------------------------------------

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



# remove duplicate photos
photo_metadata_prep <- exifr::read_exif('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/8518',recursive=T)  %>%
  janitor::clean_names()  %>%
  # filter(stringr::str_detect(file_name, 'Time'))
  group_by(date_time_original) %>%
  filter(n()>1) %>%
  select(file_name, date_time_original) %>%
  arrange(date_time_original)

photo_metadata_prep2 <- exifr::read_exif('/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_skeena_2023_reporting/data/photos/sorted/8478',recursive=T)  %>%
  janitor::clean_names() %>%
  group_by() %>%
  filter(n()>1) %>%
  select(file_name, date_time_original) %>%
  arrange(date_time_original)


# sort photos to folders --------------------------------------------------
pscis_all <- fpr::fpr_import_pscis_all() %>%
  dplyr::bind_rows()

# ensure you have a surveyor for every crossing
test <- pscis_all %>% filter(is.na(crew_members))



# ensure you have a time for every crossing for all the people you are sorting for
# we only exported fpr_photo_time_prep for this test
test <- fpr::fpr_photo_time_prep() %>%
  filter(
    is.na(date_time_start) &
      (camera_id == 'AI' |
         camera_id == 'KP')
  )


##lets pass it a list of folders to do the sorting on
##we do not include nupqu becasue their photos are already sorted into folders
ls_folders <- c("C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/photos/AI",
                "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/photos/KP"
)


# this should of happened already but it didn't so we need to convert jpeg to JPG to avoid issues later
# find the jpeg files in kyle's folder

# path <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/photos/KP"
# jpegs_jpgs <- list.files(path, full.names = T) %>%
#   grep(pattern = '.*//.(jpg|jpeg)$',
#         value = T)
#
# jpegs_jpgs %>%
#   purrr::map(fpr::fpr_photo_resize_convert, path = path)
#
# basename(jpegs_jpgs)
#
# # remove the old files
# file.remove(jpegs_jpgs)

# get the photo metadata
photo_meta <- ls_folders %>%
  map(fpr::fpr_photo_sort_metadat) %>%
  purrr::set_names(nm = basename(ls_folders)) %>%
  bind_rows(.id = 'camera_id') %>%
  tibble::rowid_to_column()

# define surveyors
ls_surveyors = c('AI', 'KP')


##we have a few special cases no lets make some conditions.  These are shots of the cards.
# probably not worth doing this again as it is a bit time consuming and it doesn't really matter if the files move.
photo_ids_dont_copy01 <- c(
  paste0('KP_IMG_', 0262:0277, '.JPG'),
  paste0('KP_IMG_', 0492:0515, '.JPG'),
  paste0('KP_IMG_', 0678:0694, '.JPG'),
  paste0('KP_IMG_', 0931:0947, '.JPG'),
  paste0('KP_IMG_', 1171:1180, '.JPG'),
  paste0('KP_IMG_', 1491:1498, '.JPG'),
  paste0('KP_IMG_', 2148:2141, '.JPG'),
  paste0('KP_IMG_', 2703:2714, '.JPG'),
  paste0('AI_IMG_', 5300:5377, '.JPG'),
  paste0('AI_IMG_', 5588:5595, '.JPG'),
  paste0('AI_IMG_', 5680:5691, '.JPG'),
  paste0('AI_IMG_', 5786:5801, '.JPG'),
  paste0('AI_IMG_', 5908:5917, '.JPG'),
  paste0('AI_IMG_', 6134:6152, '.JPG'),
  paste0('AI_IMG_', 6329:6340, '.JPG'),
  paste0('AI_IMG_', 6499:6509, '.JPG'),
  paste0('AI_IMG_', 6539:6640, '.JPG')
)

photos_to_transfer <- ls_surveyors %>%
  purrr::map(fpr::fpr_photo_sort_plan) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    site = case_when(photo_fullname %in% photo_ids_dont_copy01 ~ NA_real_,
                     T ~ site),
    folder_to_path = paste0(getwd(), '/data/photos/', as.character(site), '/', camera_id, '_', photo_basename)  ##we could add the source to the name of the photo if we wanted....
  ) %>%
  filter(
    !sourcefile %ilike% 'not_used' & #not applied in this project
      !is.na(site) ##filter out some photos that shouldn't or don't move as per our ids_dont_copy files
  )


# burn a csv record of the photo assignment
photos_to_transfer %>%
  readr::write_csv(paste0('data/inputs_extracted/photo_transfer_record_', format(now(), "%Y%m%d_%H%M"), '.csv'),
                   na = '')


# # since we have a record we can delete the jpeg and jpgs that were copied over before!
# photos_to_delete <- readr::read_csv('data/inputs_extracted/photos_to_transfer_2022-03-24.csv') %>%
#   filter(folder_to_path %like% '.jpeg|.jpg' ) %>%
#   pull(folder_to_path)
#
# # # remove the old files
# file.remove(photos_to_delete)

# burn photos to files ----------------------------------------------------

# make sure you create the necessary directories
photos_to_transfer %>%
  dplyr::distinct(site) %>%
  pull(site) %>%
  purrr::map(fpr::fpr_photo_folders)

##test a bunch first!!!!!!!
test <- photos_to_transfer %>%
  filter(site == 197662)

# just a test of one folder
file.copy(from = test$sourcefile, to = test$folder_to_path,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)

# we could move them vs. copy but we need to be sure they are backed up first!!!!
# we should script the backup and resizing to an intermediary file then move vs. copy next time
# !!!!!!!!!!!!!this is the command to copy over!
file.copy(from=photos_to_transfer$sourcefile, to=photos_to_transfer$folder_to_path,
          overwrite = F, recursive = FALSE,
          copy.mode = TRUE)


##we also can erase the photos we said not to move since they are backed up and
##we want to see any left overs
# photo_folder_targets_delete <- photo_folder_targets %>%
#   filter(
#     is.na(folder_to_id)  ##filter out some photos that shouldn't or don't move
#   )
#
# file.remove(photo_folder_targets_delete$sourcefile)



# rename Lars photo directories -------------------------------------------

# need to rename the directories so they match the site name

path <- "C:/Users/allan/OneDrive/New_Graph/Current/2021-034-hctf-bulkley-fish-passage/data/photos/moe_flnr"

folders <- list.dirs(path,
                     recursive = F,
                     full.names = F)

folders_renamed <- folders %>%
  stringr::str_replace('101400022_', '1014000022_') %>% # 101400022 is actually 1014000022
  stringr::str_extract("[^FP_|^?]*$")

# turned off for safety
# file.rename(from = paste0(path, '/', folders),
#             to = paste0(path, '/', folders_renamed)
# )


# flip photos -------------------------------------------------------------

# needed to flip some photos.  couldn't figure out how to run over list.
my_site <- 2021090150
fpr::fpr_photo_flip(str_to_pull = 'KP_TC_00169')

fpr::fpr_photo_flip(site_id = 14000988, rotate = 270, str_to_pull = '5587')
fpr::fpr_photo_flip(site_id = 14000994, rotate = 270, str_to_pull = '00938')
fpr::fpr_photo_flip(site_id = 14000997, rotate = 270, str_to_pull = '5578')
fpr::fpr_photo_flip(site_id = 2021090399, rotate = 270, str_to_pull = '5756')

# QA photo files ----------------------------------------------------------

pscis_all <- fpr_import_pscis_all() %>%
  bind_rows


# here is a little test on how to see the folder that need all photos to be renamed
# if you have a folder that is not in your pscis sheets it will break things.  watch out.
# consider using list of folders in the file
test <- fpr::fpr_photo_qa() %>%
  data.table::rbindlist(fill = T)


do_these_bud <- fpr::fpr_photo_qa()[
  fpr::fpr_photo_qa() %>%
    map(fpr::fpr_dat_w_rows) %>%
    grep(pattern = F)
] %>%
  names(.) %>%
  unique(.)

# here is the test for missing individual photos
test <- fpr::fpr_photo_qa() %>%
  bind_rows() %>%
  dplyr::filter(if_any(everything(), is.na))

test

# build photo amalgamation for each site ------------------------------------------------
pscis_all <- fpr::fpr_import_pscis_all() %>%
  bind_rows()

pscis_all %>%
  distinct(site_id) %>%
  arrange(site_id) %>%
  # put this here to work around issue 64
  # filter(site_id != 198110) %>%
  # filter(site_id != 123750) %>%
  # filter(site_id != 198116) %>%
  # head() %>% #test
  pull(site_id)  %>%
  purrr::map(fpr_photo_amalg_cv)


# can't build the amalgamated photos
setdiff(  list.dirs('data/photos', full.names = F, recursive = F),

          pscis_all %>%
            distinct(site_id) %>%
            arrange(site_id) %>%
            # head() %>% #test
            pull(site_id)
)

# make phase2 photo files and copy in photos ------------------------------

# because we did not have the pscis ids our photos for phase 2 sites that had a my_crossing_reference
# need to be copied into new folders

##path to the photos
path <- paste0(getwd(), '/data/photos/')


##use the pscis spreadsheet to make the folders to copy the photos to
# d <- import_pscis(workbook_name = 'pscis_phase1.xlsm')
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')

conn <- rws_connect("data/bcfishpass.sqlite")
xref_pscis_my_crossing_modelled <- readwritesqlite::rws_read_table("xref_pscis_my_crossing_modelled", conn = conn)
rws_disconnect(conn)

pscis_new_sites <- left_join(
  d,
  xref_pscis_my_crossing_modelled,
  by = c('pscis_crossing_id' = 'stream_crossing_id')
) %>%
  filter(!is.na(external_crossing_reference))

folderstocopy<- pscis_new_sites$external_crossing_reference %>% as.character()

folders_new_names <- pscis_new_sites$pscis_crossing_id %>% as.character()

path_to_photos <- paste0(getwd(), '/data/photos/', folderstocopy)

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



