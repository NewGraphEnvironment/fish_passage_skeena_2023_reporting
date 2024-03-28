## ***Updated version of this script can be found in Fraser 2023 repo***


# import form_pscis.gpkg direct from mergin and then import form_pscis_2023 and bind rows
# NOTE : this step only needs to be done when new crossing data is added to the project,
# in this case, Andy and Alicia from GWA added new data

# form_pscis_field <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/form_pscis.gpkg')) %>%
#   filter(mergin_user == "andyr")
#
# form_pscis_working <- sf::st_read(dsn= paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg'))
#
# # check to see that column names are equiv (must be if number is the same but still)
# identical(names(form_pscis_field), names(form_pscis_working))
#
# # bind rows
# form_prep <- bind_rows(
#   form_pscis_field,
#   form_pscis_working
# ) %>%
#   st_transform(crs = 26900 + utm) %>%
#   poisspatial::ps_sfc_to_coords(X = 'easting', Y = 'northing') %>%
#   # add in utm zone of study area
#   mutate(utm_zone = utm)
#
# # burn to backup folder as csv
# form_prep %>%
#   readr::write_csv(paste0('data/inputs_extracted/mergin_backups/form_pscis_raw_',
#         format(lubridate::now(), "%Y%m%d"), '.csv'))
#
# # re burn to working geopackage in Q project
# form_prep %>%
#   st_as_sf(coords = c('easting', 'northing'), crs = 26900 + utm, remove = F) %>%
#   # convert back to project crs
#   st_transform(crs = 3005) %>%
#   sf::st_write(paste0('../../gis/', dir_project, '/data_field/2023/form_pscis_2023.gpkg'), append=F, delete_dsn=T)
