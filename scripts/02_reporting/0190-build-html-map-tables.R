source('scripts/packages.R')

# lets just make the html tables when we need to


unlink('docs/sum/bcfp', recursive = T) #erase the file and start over every time
dir.create('docs/sum/bcfp')



# bcfishpass html to link from map ----------------------------------------
bcfishpass %>%
  # need to determine which sites have no bcfishpass info and exclude those so the purrr::map does not bail on us
  filter(stream_crossing_id %in% (pscis_all %>% pull(pscis_crossing_id))) %>%
  pull(stream_crossing_id) %>%
  purrr::map(fpr::fpr_table_bcfp_html)



unlink('docs/sum/cv', recursive = T) #erase the file and start over every time
dir.create('docs/sum/cv')

# build all the cv tables for interactive map - be sure to run options(knitr.kable.XXX = '--') in the setup chunk of index.Rmd first
pscis_all %>%
  distinct(pscis_crossing_id) %>%
  # filter(source %ilike% 'phase2') %>%
  pull(pscis_crossing_id) %>%
  map(fpr::fpr_table_cv_html)


