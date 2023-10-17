# make a csv of climate changecolumn names and aliases ready for description to be input manually for climate change methods



library(readODS)

aliases <- readODS::read_ods('data/dff/form_pscis_alias.ods') %>%
  select(
    pscis_crossing_id,
    my_crossing_reference,
    `Crew Members Seperate with Spaces`,
    moti_chris_culvert_id,
    stream_name,
    road_name,
    `Erosion (scale 1 low - 5 high)`:`Describe details and rational for priority rankings`
  )


c_names <- readODS::read_ods('data/dff/form_pscis_colnames.ods') %>%
 select(
    pscis_crossing_id,
    my_crossing_reference,
    crew_members,
    moti_chris_culvert_id,
    stream_name,
    road_name,
    erosion_issues:priority_notes
  )

xref_moti_climate_template <- tibble::tibble(
  spdsht = names(c_names),
  report = names(aliases)
) %>%
  mutate(description = NA_character_,
         id_join = NA_integer_,
         id_side = NA_integer_)


# burn to csv
xref_moti_climate_template %>%
  write_csv('data/inputs_extracted/xref_moti_climate_template.csv')

# we work by hand on a copied version in `data/inputs_raw/xref_moti_climate.csv so we don't overwrite
