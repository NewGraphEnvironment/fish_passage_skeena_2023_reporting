# build reporting table for minnow trap surveys
tab_fish_mt <- function(sit = my_site){
    hab_fish_indiv %>%
    filter(site == sit & sampling_method == 'minnow trapping' & !is.na(life_stage)) %>%
    group_by(site, location, species_code, life_stage) %>%
    summarise(Number = n()) %>%
    ungroup() %>%
    select(Location = location,
           Species = species_code,
           Stage = life_stage,
           Number) %>%
    pivot_wider(names_from = Stage,
                values_from = Number) %>%
    mutate(Location = case_when(Location == 'us' ~ 'Upstream',
                                T ~ 'Downstream')) %>%
    arrange(Species) %>%
    replace(is.na(.), 0)
}

# take habitat confirmation xls and make a csv with the alias_local_name pasted to the comments
# so people can see which comments are linked to which site
fpr_hab_alias_to_comments <- function(target = target_dir){
  fpr_import_hab_con(col_filter_na = T,
                     row_empty_remove = T,
                     backup = FALSE) %>%
    purrr::pluck("step_4_stream_site_data") %>%
    mutate(comments = paste0('Site ', local_name, '. ', comments)) %>%
    select(reference_number, gazetted_names, local_name, comments) %>%
    write_csv(target_dir)
}

# MOTI ----------------

# set up a table for the memos that contains the moti climate change data
# make a tribble of the xref_moti_climate_template to make 2 columns in table
# read in csv, then fpr_kable the data frame, run in rmd chunk, then copy and paste table using datapasta add in "paste as tribble"
#xref_moti_climate <- read_csv(file = paste0(getwd(), '/data/inputs_extracted/xref_moti_climate_template.csv'))

xref_moti_climate_names <- tibble::tribble(
                                      ~spdsht,                                                                                                               ~report, ~description, ~id_join, ~id_side,
                          "pscis_crossing_id",                                                                                                   "pscis_crossing_id",         NA,     NA,     NA,
                      "my_crossing_reference",                                                                                               "my_crossing_reference",         NA,     NA,     NA,
                               "crew_members",                                                                                   "Crew Members Seperate with Spaces",         NA,     NA,     NA,
                      "moti_chris_culvert_id",                                                                                               "moti_chris_culvert_id",         NA,     NA,     NA,
                                "stream_name",                                                                                                         "stream_name",         NA,     NA,     NA,
                                  "road_name",                                                                                                           "road_name",         NA,     NA,     NA,
                             "erosion_issues",                                                                                      "Erosion (scale 1 low - 5 high)",         NA,     9L,     1L,
                     "embankment_fill_issues",                                                                  "Embankment fill issues 1 (low) 2 (medium) 3 (high)",         NA,     2L,     1L,
                            "blockage_issues",                                                                      "Blockage Issues 1 (0-30%) 2 (>30-75%) 3 (>75%)",         NA,     3L,     1L,
                             "condition_rank",                                                                    "Condition Rank = embankment + blockage + erosion",         NA,     4L,     1L,
                            "condition_notes",                                                                "Describe details and rational for condition rankings",         NA,     NA,     NA,
   "likelihood_flood_event_affecting_culvert",                                                     "Likelihood Flood Event Affecting Culvert (scale 1 low - 5 high)",         NA,     8L,     1L,
  "consequence_flood_event_affecting_culvert",                                                    "Consequence Flood Event Affecting Culvert (scale 1 low - 5 high)",         NA,     5L,     1L,
                  "climate_change_flood_risk",                           "Climate Change Flood Risk (likelihood x consequence) 1-6 (low) 6-12 (medium) 10-25 (high)",         NA,     6L,     1L,
                         "vulnerability_rank",                                                                  "Vulnerability Rank = Condition Rank + Climate Rank",         NA,     7L,     1L,
                              "climate_notes",                                                             "Describe details and rational for climate risk rankings",         NA,     NA,     NA,
                             "traffic_volume",                                                                         "Traffic Volume 1 (low) 5 (medium) 10 (high)",         NA,     9L,     2L,
                           "community_access", "Community Access - Scale - 1 (high - multiple road access) 5 (medium - some road access) 10 (low - one road access)",         NA,     2L,     2L,
                                       "cost",                                                                                       "Cost (scale: 1 high - 10 low)",         NA,     3L,     2L,
                           "constructability",                                                                      "Constructibility (scale: 1 difficult -10 easy)",         NA,     4L,     2L,
                               "fish_bearing",                                                             "Fish Bearing 10 (Yes) 0 (No) - see maps for fish points",         NA,     5L,     2L,
                      "environmental_impacts",                                                                       "Environmental Impacts (scale: 1 high -10 low)",         NA,     8L,     2L,
                              "priority_rank",  "Priority Rank = traffic volume + community access + cost + constructability + fish bearing + environmental impacts",         NA,     6L,     2L,
                               "overall_rank",                                                                   "Overall Rank = Vulnerability Rank + Priority Rank",         NA,     7L,     2L,
                             "priority_notes",                                                                 "Describe details and rational for priority rankings",         NA,     NA,     NA
  )

fpr_table_moti <- function(dat = tab_moti_phase2,
                           xref_table = xref_moti_climate_names,
                           site = my_site,
                           ...){
  df <- dat %>% filter(pscis_crossing_id == site)
  # build left side of table
  tab_results_left <- xref_table %>%
    filter(id_side == 1)

  tab_pull_left <- df %>%
    select(pull(tab_results_left, spdsht)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  left <- left_join(tab_pull_left, xref_table, by = c('rowname' = 'spdsht'))

  # build right side of table
  tab_results_right <- xref_table %>%
    filter(id_side == 2)

  tab_pull_right <- df %>%
    select(pull(tab_results_right, spdsht)) %>%
    t() %>%
    as.data.frame() %>%
    tibble::rownames_to_column()

  right <- left_join(tab_pull_right, xref_table, by = c('rowname' = 'spdsht'))

  tab_joined <- left_join(
    select(left, report, V1, id_join),
    select(right, report, V1, id_join),
    by = 'id_join'
  ) %>%
    select(-id_join) %>%
    purrr::set_names(c('Condition and Climate Risk', 'Rank', 'Priority', 'Rank'))

  tab_joined %>%
    fpr_kable(caption_text = paste0('Summary of climate change risk assessment for PSCIS crossing ', site, '.'), scroll = F)

}

fpr_table_moti_comments <- function(dat = tab_moti_phase2,
                           site = my_site,
                           ...){
  tab_comments <- dat %>%
    select(pscis_crossing_id, condition_notes, climate_notes, priority_notes) %>%
    rename('Condition' = condition_notes,
           'Climate' = climate_notes,
           'Priority' = priority_notes) %>%
    pivot_longer(cols = Condition:Priority, names_to = "Category", values_to = "Comments") %>%
    filter(pscis_crossing_id == site) %>%
    select(-pscis_crossing_id)

  tab_comments %>%
    fpr_kable(caption_text = paste0('Details and rational for climate risk rankings'), scroll = F)
}

# PSCIS Submissions -------------

tfpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

tfpr_photo_change_name <- function(filenames_to_change = filestocopy_list){
  gsub(filenames_to_change, pattern = path, replacement = targetdir)
}

tfpr_copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}


#' Determines the road class for each site and burns it to sqlite database in data folder
#'
#' @param dat String (quoted) name of sqlite database in data folder, defaults to bcfishpass
#'
#' @importFrom glue glue
#' @importFrom dplyr mutate select filter case_when pull
#' @importFrom stringr str_replace_all str_detect word str_to_lower
#' @importFrom readwritesqlite rws_connect rws_list_tables rws_write
#'

# tab cost multipliers for road surface can be found in a csv located in 'data/inputs_raw/tab_cost_rd_mult.csv'

tfpr_road_class <- function(
    dat = 'bcfishpass'){

  # rebuild using bcfishpass object from the tables.R script.
  # see older repos if we need to go back to a system that can run these before we have pscis IDs simplifying for now on
  rd_class_surface <- dat %>%
    dplyr::select(stream_crossing_id, transport_line_structured_name_1:dam_operating_status) %>%
    dplyr::filter(stream_crossing_id %in% (
      pscis_all %>% dplyr::pull(pscis_crossing_id))
    ) %>%
    dplyr::mutate(my_road_class = ften_file_type_description) %>%
    dplyr::mutate(my_road_class = dplyr::case_when(is.na(my_road_class) & !is.na(transport_line_type_description) ~
                                                     transport_line_type_description,
                                                   T ~ my_road_class)) %>%

    dplyr::mutate(my_road_class = dplyr::case_when(is.na(my_road_class) & !is.na(rail_owner_name) ~
                                                     'rail',
                                                   T ~ my_road_class)) %>%
    dplyr::mutate(my_road_surface = dplyr::case_when(is.na(transport_line_surface_description) & !is.na(ften_file_type_description) ~
                                                       'loose',
                                                     T ~ transport_line_surface_description)) %>%
    dplyr::mutate(my_road_surface = dplyr::case_when(is.na(my_road_surface) & !is.na(rail_owner_name) ~
                                                       'rail',
                                                     T ~ my_road_surface)) %>%
    dplyr::mutate(my_road_class = stringr::str_replace_all(my_road_class, 'Forest Service Road', 'fsr'),
                  my_road_class = stringr::str_replace_all(my_road_class, 'Road ', ''),
                  my_road_class = stringr::str_replace_all(my_road_class, 'Special Use Permit, ', 'Permit-Special-'),
                  my_road_class = dplyr::case_when(
                    stringr::str_detect(my_road_class, 'driveway') ~ 'driveway',
                    T ~ my_road_class),
                  my_road_class = stringr::word(my_road_class, 1),
                  my_road_class = stringr::str_to_lower(my_road_class)) %>%
    dplyr::filter(stream_crossing_id %in% (
      pscis_all %>% dplyr::pull(pscis_crossing_id))
    )



  conn <- readwritesqlite::rws_connect(glue::glue("data/{dat}.sqlite"))
  readwritesqlite::rws_list_tables(conn)
  readwritesqlite::rws_write(rd_class_surface, exists = F, delete = T,
                             conn = conn, x_name = "rd_class_surface")
  readwritesqlite::rws_disconnect(conn)

}


#' Determine replacement structure type and size based on measured field metrics.
#' @param dat PSCIS data
#' @param fill_dpth standard fill depth, default is 3m.
#' @param brdg_wdth standard bridge width, default is 15m.
#' @param chn_wdth_max maximum channel width where the bridge should start to be more than brdg_wdth, default is brdg_wdth - 5m.
#' @param fill_dpth_mult for every 1 m deeper than 3m, we need a 1.5:1 slope so there is 3m more bridge required
#'
#' @importFrom dplyr mutate filter select case_when
#' @importFrom plyr round_any
#' @importFrom readr write_csv
#' @importFrom chk chk_numeric
#'
#' @export
#'
#' #' @examples \dontrun{
#' fpr_structure_size_type(dat)
#' }
#'

lfpr_structure_size_type <- function(
    dat = NULL,
    fill_dpth = 3,
    brdg_wdth = 15,
    chn_wdth_max = brdg_wdth - 5,
    fill_dpth_mult = 3) {

  if (is.null(dat))
    stop('please provide "dat" (dataframe) object')
  if (!is.data.frame(dat))
    stop('"dat" must inherit from a data.frame')

  chk::chk_numeric(fill_dpth)
  chk::chk_numeric(brdg_wdth)
  chk::chk_numeric(chn_wdth_max)
  chk::chk_numeric(fill_dpth_mult)

  # Unsure if this still needs to be included, but can't find pcsis2...
  # ##according to the moe specs in MoE 2011 - backwatering requires od<30 and slope <2, swr <1.2 see if there are options
  # tab_backwater <- dat %>%  ##changed this to pscis2!
  #   filter(barrier_result != 'Passable' &
  #            barrier_result != 'Unknown' &
  #            outlet_drop_meters < 0.3 &
  #            stream_width_ratio_score < 1.2 &
  #            culvert_slope_percent <= 2 )


  str_type <- dat %>%
    dplyr::select(rowid, aggregated_crossings_id, pscis_crossing_id, my_crossing_reference, source, barrier_result,
                  downstream_channel_width_meters, fill_depth_meters) %>%
    dplyr::mutate(fill_dpth_over = fill_depth_meters - fill_dpth_mult) %>%
    dplyr::mutate(crossing_fix = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                  & downstream_channel_width_meters >= 2 ~ 'Replace with New Open Bottom Structure',
                                                  barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_character_,
                                                  T ~ 'Replace Structure with Streambed Simulation CBS'))  %>%
    dplyr::mutate(span_input = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                & downstream_channel_width_meters >= 2 ~ brdg_wdth,
                                                barrier_result == 'Passable' | barrier_result == 'Unknown' ~ NA_real_,
                                                T ~ 3))  %>%
    dplyr::mutate(span_input = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                & fill_dpth_over > 0 & !crossing_fix %ilike% 'Simulation' ~
                                                  (brdg_wdth + fill_dpth_mult * fill_dpth_over),  ##1m more fill = 3 m more bridge
                                                T ~ span_input)) %>%
    dplyr::mutate(span_input = dplyr::case_when(span_input < (downstream_channel_width_meters + 4) & ##span not need be extended if already 4m bigger than channel width
                                                  downstream_channel_width_meters > chn_wdth_max ~
                                                  (downstream_channel_width_meters - chn_wdth_max) + span_input,  ##for every m bigger than a 5 m channel add that much to each side in terms of span
                                                T ~ span_input)) %>%
    ##let's add an option that if the stream is under 3.5m wide and under more than 5m of fill we do a streambed simulation with a 4.5m embedded multiplate like 4607464 on Flathead fsr
    dplyr::mutate(crossing_fix = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                  & downstream_channel_width_meters > 2 &
                                                    downstream_channel_width_meters <= 3.5 &
                                                    fill_depth_meters > 5 ~ 'Replace Structure with Streambed Simulation CBS',
                                                  T ~ crossing_fix),
                  span_input = dplyr::case_when((barrier_result == 'Barrier' | barrier_result == 'Potential')
                                                & downstream_channel_width_meters > 2 &
                                                  downstream_channel_width_meters <= 3.5 &
                                                  fill_depth_meters > 5 ~ 4.5,
                                                T ~ span_input)) %>%
    dplyr::mutate(span_input = plyr::round_any(span_input, 0.5))


  ##burn to a csvs so we can copy and paste into spreadsheet

  str_type %>%
    dplyr::filter(source %ilike% 'phase1') %>%
    readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis1.csv'),
                     na = '')
  str_type %>%
    dplyr::filter(source %ilike% 'phase2') %>%
    readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis2.csv'),
                     na = '')
  str_type %>%
    dplyr::filter(source %ilike% 'reasses') %>%
    readr::write_csv(file = paste0(getwd(), '/data/inputs_extracted/str_type_pscis_reassessments.csv'),
                     na = '')

}

# Reporting -------------

#' Creates hydrograph and hydrology stats figures for a given station.
#'
#' @param station String (quoted) number of station
#' @param pane_hydat Boolean TRUE if you want a pane layout of all hydrographs
#' @param single_hydat Boolean TRUE if you want a single hydrograph with mean flows
#' @param start_year Specific start year, if not specified, will use the first year of the data
#' @param end_year Specific end year, if not specified, will use the first year of the data
#' @param fig/hydrology_stats_ hydrology stats figure saved to the fig folder
#' @param fig/hydrograph_ hydrograph figure saved to the fig folder
#'
#' @importFrom lubridate year
#' @importFrom tidyhydat hy_daily_flows search_stn_number
#' @importFrom stringr str_to_title
#' @importFrom fasstr plot_data_screening
#' @importFrom ggdark dark_theme_bw
#' @importFrom dplyr mutate group_by summarise
#' @importFrom ggplot2 ggsave ggplot geom_ribbon scale_x_date labs geom_line scale_colour_manual
#' @importFrom cli cli_alert
#' @importFrom chk chk_string chk_flag chk_number
#' @importFrom poisutils ps_error
#'
#' @export
#'
#' @examples \dontrun{fpr_create_hydrograph('08EE004', pane_hydat = FALSE))}

lfpr_create_hydrograph <- function(
    station = NULL,
    pane_hydat = TRUE,
    single_hydat = TRUE,
    start_year = NULL,
    end_year = NULL){

  if(is.null(station)){
    poisutils::ps_error('Please provide a station number, for example "08EE004"')
  }

  chk::chk_string(station)
  chk::chk_flag(pane_hydat)
  chk::chk_flag(single_hydat)

  flow_raw <- tidyhydat::hy_daily_flows(station)

  if(is.null(start_year)){
    start_year <- flow_raw$Date %>% min() %>% lubridate::year()
  }

  if(is.null(end_year)){
    end_year <- flow_raw$Date %>% max() %>% lubridate::year()
  }

  chk::chk_number(start_year)
  chk::chk_number(end_year)

  tidyhat_info <- tidyhydat::search_stn_number(station)



  ##### Hydrograph Pane #####

  ##build caption for the pane figure
  caption_info <- dplyr::mutate(tidyhat_info, title_stats = paste0(stringr::str_to_title(STATION_NAME),
                                                                   " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                   " Lon ",round(LONGITUDE,6), "). Available daily discharge data from ", start_year,
                                                                   # FIRST_YEAR, ##removed the default here
                                                                   " to ",end_year, "."))

  hydrograph1_stats_caption <- caption_info$title_stats



  if (pane_hydat == TRUE){
    #Create pane of hydrographs with "Mean", "Minimum", "Maximum", and "Standard Deviation" flows
    hydrograph_stats_print <- fasstr::plot_data_screening(station_number = station, start_year = start_year,
                                                          include_stats = c("Mean", "Minimum", "Maximum", "Standard Deviation"),
                                                          plot_availability = FALSE)[["Data_Screening"]] + ggdark::dark_theme_bw() ##first version is not dark
    hydrograph_stats_print

    #Save hydrograph pane
    ggplot2::ggsave(plot = hydrograph_stats_print, file=paste0("fig/hydrology_stats_", station, ".png"),
                    h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption)
  }





  ##### Single Hydrograph  #####

  ##build caption for the single figure
  caption_info2 <- dplyr::mutate(tidyhat_info, title_stats2 = paste0(stringr::str_to_title(STATION_NAME),
                                                                     " (Station #",STATION_NUMBER," - Lat " ,round(LATITUDE,6),
                                                                     " Lon ",round(LONGITUDE,6), "). Available mean daily discharge data from ", start_year,
                                                                     # FIRST_YEAR, ##removed the default here
                                                                     " to ",end_year, "."))

  hydrograph1_stats_caption2 <- caption_info2$title_stats2

  if (single_hydat == TRUE){
    # Create single hydrograph with mean flows from date range
    flow <- flow_raw %>%
      dplyr::mutate(day_of_year = lubridate::yday(Date)) %>%
      dplyr::group_by(day_of_year) %>%
      dplyr::summarise(daily_ave = mean(Value, na.rm=TRUE),
                       daily_sd = sd(Value, na.rm = TRUE),
                       max = max(Value, na.rm = TRUE),
                       min = min(Value, na.rm = TRUE)) %>%
      dplyr::mutate(Date = as.Date(day_of_year))

    plot <- ggplot2::ggplot()+
      ggplot2::geom_ribbon(data = flow, aes(x = Date, ymax = max,
                                            ymin = min),
                           alpha = 0.3, linetype = 1)+
      ggplot2::scale_x_date(date_labels = "%b", date_breaks = "2 month") +
      ggplot2::labs(x = NULL, y = expression(paste("Mean Daily Discharge (", m^3, "/s)", sep="")))+
      ggdark::dark_theme_bw() +
      ggplot2::geom_line(data = flow, aes(x = Date, y = daily_ave),
                         linetype = 1, linewidth = 0.7) +
      ggplot2::scale_colour_manual(values = c("grey10", "red"))
    plot

    ggplot2::ggsave(plot = plot, file=paste0("fig/hydrograph_", station, ".png"),
                    h=3.4, w=5.11, units="in", dpi=300)

    cli::cli_alert(hydrograph1_stats_caption2)
  }
}


# write the contents of the NEWS.md file to a RMD file that will be included as an appendix
news_to_appendix <- function(
    md_name = "NEWS.md",
    rmd_name = "2090-report-change-log.Rmd",
    appendix_title = "# Report Change Log") {

  # Read and modify the contents of the markdown file
  news_md <- readLines(md_name)
  news_md <- stringr::str_replace(news_md, "^#", "###") |>
    stringr::str_replace_all("(^(### .*?$))", "\\1 {-}")

  # Write the title, a blank line, and the modified contents to the Rmd file
  writeLines(
    c(paste0(appendix_title, " {-}"), "", news_md),
    rmd_name
  )
}
