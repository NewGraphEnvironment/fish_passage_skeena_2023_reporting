## Scripts directory guide

*Note - there are often headers within the commenting of these scripts that can help the user navigate sections.  Use `shift-command-o` to see the outline in the source window of Rstudio.

1.  `01_prep_inputs/`

-   Directory with scripts used to extract data from database to input into pscis spreadsheets, process photos, and prep for pscis phase 1 submission.

2.  `02_reporting/`

-   Directory with scripts used to build materials used in reporting and bcfishpass data processing. After phase 1 submission is accepted, run `0160-load-bcfishpass-data.R` script found here to grab info from bcfishpass and burn to our local sqlite database.  

3.  `03_permit_submission/`

-   Directory with scripts used to extract information for permitting.

4.  `0100-setup.R`

-   Used to set up directories in repo.

5.  `extract-pg.R`

-   Used to extract data from the postgres database.

6.  `fiss_site_tidy.R`

-   Used to tidy up phase 2 habitat confirmation data from geopackage in mergin project and burn to csvs ready to paste into submission template.

7.  `outgoing-mapping.R`

-   Used to make mapping files. Only run when you have all data together. Inspect the following environmental variables found in the `tables.R` script to make sure there is no missing data: `hab_fish_collect`, `hab_features`, `hab_site_priorities`, `phase1_priorities`.

8.  `packages.R`

-   Used to load all packages required in this repo.

9.  `pit_tags.R`

-   Used to process pit tag data used in electrofishing and combine with fish data to copy and paste into submission template.

10. `pscis_tidy.R`

-   Used to tidy up phase 1 data from geopackage in mergin project and burn to csvs ready to paste into submission template.

11. `run.R`

-   Used to run report in html or pdf output.

12. `tables.R`

-   Used to import data and build tables for reporting. Script is split up into different sections, use outline to navigate.
