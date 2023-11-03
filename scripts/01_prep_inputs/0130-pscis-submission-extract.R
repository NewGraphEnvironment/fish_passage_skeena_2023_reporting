##QA to be sure that you have all 6 required photos for submission to PSCIS
##convert jpg (or other formats - need to code) to JPG for consistency and to avoid issues with submission/reporting
##move the photos and spreadsheet ready for submission to pscis
## we sort, store and modify all photos on onedrive so that all collaborators can see all photos

source('scripts/packages.R')
source('scripts/functions.R')

name_repo <- 'fish_passage_skeena_2023_reporting'
stub_from <- 'C:/Users/matwi/OneDrive'

# Phase 1 submission -----------

##path to the photos on onedrive
path <- paste0(stub_from, '/Projects/repo/', name_repo,
               '/data/photos/sorted/')

##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase1.xlsm')

folderstocopy<- d$my_crossing_reference %>% as.character()

path_to_photos <- paste0(path, folderstocopy)


##here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface
targetdir = paste0(stub_from, '/Projects/PSCIS/phase1_submissions/PSCIS_skeena_2023_phase1/')
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)

filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))

empty_files <- empty_idx %>% fpr_filter_list()


##rename long names if necessary

photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))


###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal becasue we did some sorting by hand without adding name of camera to the file name but a start on reproducability nonetheless

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking_phase1.csv'))

filestopaste_list <- filestocopy_list %>%
  map(fpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_phase1.xlsm',
          to = paste0(targetdir, 'pscis_phase1.xlsm'),
          overwrite = T)

#macros don't seem to work in one drive so copy the submission folder to a directory on my machine sing windows command line

#create directory:
#mkdir C:\Users\matwi\Projects\current\2023-066-sern-skeena-fish-passage\PSCIS_skeena_2023_phase1
#copy folder over:
#xcopy C:\Users\matwi\OneDrive\Projects\PSCIS\phase1_submissions\PSCIS_skeena_2023_phase1 /e C:\Users\matwi\Projects\current\2023-066-sern-skeena-fish-passage\PSCIS_skeena_2023_phase1

# Phase 2 submission ---------------

##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase2.xlsm')

folderstocopy<- d$pscis_crossing_id %>% as.character()

path <- paste0(getwd(), '/data/photos/')

path_to_photos <- paste0(path, folderstocopy)

# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface
targetdir = paste0(stub_from, '/Projects/PSCIS/phase2_submissions/PSCIS_skeena_2023_phase2/')
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)

filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))

##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))

empty_files <- empty_idx %>% fpr_filter_list()


##rename long names if necessary

photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))


###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal because we did some sorting by hand without adding name of camera to the file name but a start on reproducability nonetheless

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking_phase2.csv'))

filestopaste_list <- filestocopy_list %>%
  map(fpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_phase2.xlsm',
          to = paste0(targetdir, 'pscis_phase2.xlsm'),
          overwrite = T)

#macros don't seem to work in one drive so copy the submission folder to a directory on my machine using windows command line shown below

#create directory:
#mkdir C:\Users\matwi\Projects\current\2023-049-sern-skeena-fish-passage\PSCIS_skeena_2023_phase2
#copy folder over:
#xcopy C:\Users\matwi\OneDrive\Projects\PSCIS\phase2_submissions\PSCIS_skeena_2023_phase2 /e C:\Users\matwi\Projects\current\2023-049-sern-skeena-fish-passage\PSCIS_skeena_2023_phase2

# Reassessments submission------

##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_reassessments.xlsm')

folderstocopy<- d$pscis_crossing_id %>% as.character()

path <- paste0(getwd(), '/data/photos/')

path_to_photos <- paste0(path, folderstocopy)

# here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface
targetdir = paste0(stub_from, '/Projects/PSCIS/reassessments_submissions/PSCIS_skeena_2023_reassessments/')
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)

filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))

##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))

empty_files <- empty_idx %>% fpr_filter_list()


##rename long names if necessary

photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))

###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal because we did some sorting by hand without adding name of camera to the file name but a start on reproducibility nonetheless

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking_reassessments.csv'))

filestopaste_list <- filestocopy_list %>%
  map(fpr_photo_change_name)

##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_reassessments.xlsm',
          to = paste0(targetdir, 'pscis_reassessments.xlsm'),
          overwrite = T)

#macros don't seem to work in one drive so copy the submission folder to a directory on my machine using windows command line shown below

#create directory:
#mkdir C:\Users\matwi\Projects\current\2023-049-sern-skeena-fish-passage\PSCIS_skeena_2023_reassessments
#copy folder over:
#xcopy C:\Users\matwi\OneDrive\Projects\PSCIS\reassessments_submissions\PSCIS_skeena_2023_reassessments /e C:\Users\matwi\Projects\current\2023-049-sern-skeena-fish-passage\PSCIS_skeena_2023_reassessments



# Submission Notes ---------

# !!!!!!!READ!!!!!!! here I record the command to move everything into the repo via command line on linux.  Suggest moving to your repo using command line on windows (google copy all files and directories on command line with windows or something) as well because then it is easy to repeat when things change.
# not quite sure how best to deal with sharing the photos yet and might end up being easiest to just work in onedrive and copy things over via command line into the repo (hope not though)
# Mateo - write down the command to copy over with command line below the linux version
# cp -R ~/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_bulkley_2023_reporting/data/* ~/Projects/repo/fish_passage_bulkley_2023_reporting/data/photos/

## going to make a few notes here about the submission process
## we need to work in microsoft edge and put sites in "Internet Explorer mode pages" and set exceptions for uploading to soft and esf

# Make exceptions for these sites in setting to use "Internet Explorer mode pages"
# NOTE: these mode pages only stay active for 30 days so you might have to re add them
## https://www.env.gov.bc.ca/csd/imb/soft/soft.shtml
## https://logon7.gov.bc.ca/clp-cgi/capBceid/logon.cgi?flags=1111:1,8&TARGET=$SM$https%3a%2f%2fapps%2enrs%2egov%2ebc%2eca%2fext%2fesf%2fsubmissionWelcome%2edo
## https://apps.nrs.gov.bc.ca/ext/esf/submissionWelcome.do?


# fill in soft url here
# https://www.env.gov.bc.ca/perl/soft/dl.pl/20230111171949-07-gp-03ff7bb7-ae59-4857-8584-e8a48a75?simple=y


##fill in where to check on your submission here (copy and paste url once file uploads)
# https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=clear
# user reference: 049_sern_skeena_20230111
# submission id: 221983




## ---------------------------start on function ---------------

####################################


test_fpr_photos_pscis <- function(name_repo = 'fish_passage_skeena_2023_reporting',
                                  stub_to = '~/Dropbox',
                                  path_from = 'data/photos',
                                  workbook_name = 'pscis_phase2.xlsm'){

  ##use the pscis spreadsheet to make the folders to copy the photos to
  d <- fpr::fpr_import_pscis(workbook_name = workbook_name)

  folderstocopy<- d$my_crossing_reference %>% as.character()

  path_to_photos <- paste0(path_from, folderstocopy)

  #!!!!!!!!!!!!!!!!!!!!!!not done properly yet
  ##here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface
  targetdir = paste0(stub_to, '/Projects/PSCIS/phasex_submissions/PSCIS_skeena_2023_phase1/')
  dir.create(targetdir)

  folderstocreate<- paste0(targetdir, folderstocopy)

  ##create the folders
  lapply(folderstocreate, dir.create)

  filestocopy_list <- path_to_photos %>%
    purrr::map(fpr::fpr_photo_paths_to_copy) %>%
    purrr::set_names(basename(folderstocreate))

  ##rename long names if necessary
  photo_sort_tracking <- path_to_photos %>%
    purrr::map(fpr::fpr_photo_document_all) %>%
    purrr::set_names(folderstocopy) %>%
    bind_rows(.id = 'folder') %>%
    mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
           photo_name_length = stringr::str_length(photo_name))

  ###here we back up a csv that gives us the new location and name of the original JPG photos.
  ## Not ideal becasue we did some sorting by hand without adding name of camera to the file name but a start on reproducability nonetheless

  ##burn to csv
  photo_sort_tracking %>%
    readr::write_csv(file = paste0('data/photos/photo_sort_tracking_',  tools::file_path_sans_ext(workbook_name), '.csv'))

  filestopaste_list <- filestocopy_list %>% fpr_photo_change_name(pattern = path_from)

  ##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
  mapply(fpr_copy_over_photos,
         filescopy =  filestocopy_list,
         filespaste = filestopaste_list)

  ##also move over the pscis file
  file.copy(from = paste0('data/', workbook_name),
            to = paste0(targetdir, workbook_name),
            overwrite = T)
}


##################################
