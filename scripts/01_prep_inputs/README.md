Workflows for pre-processing of field data so it aligns with requirements for PSCIS upload and bcfishpass outputs. These are often 2 way workflows where we use output csv spreadsheets to update our raw input datasheets. These are designed to be self sufficient scripts with everything in them necessary to do there specific task with a clean working environment.

<br>

If the line `source('R/private_info.R')` is at the top of the file then the file requires connecting to a local or remote postgresql database and a google api key for grabbing elevations cannot be completed without obtaining the required info first.

<br>

It may be a bit confusing to follow but we try to document it just the same. Tasks completed include:

-   Backup original photos on a remote drive or server.

-   Resize photos to be under 1mb as per PSCIS requirements.

-   Rename jpg and jpeg to JPG to simplify reporting and eliminate issues with PSCIS upload.

-   Get UTMs of PSCIS, modelled crossing and dam sites from bcfishpass generated database when field survey indicates correct location.

-   Find road tenure information from bcfishpass.

-   Determine replacement structure type and size based on field measured metrics.

-   Build directories of folders related to each site based on PSCIS input spreadsheet site ids.

-   Do an initial drop of photos into the generated site folders based on dates, times and surveyor.

-   QA renamed photos to determine that all 5 photos (upstream, downstream, inlet, outlet, barrel) required for PSCIS as well as a road photo are present.

-   Submit excel spreadsheets with associated crossing photos to PSCIS

  + all code is located in `0130-pscis-submission-extract.R`

  + script is separated into phase 1 submission, phase 2, and reassessments

  + submission notes to consider when uploading any file is located at the bottom of the script

-   Generate an amalgamated photo for each site containing all 6 of the previously mentioned photos.

-   Generate a csv file that contains the locations and names of all photos after they are sorted and renamed to facilitate reproducability.

-   Find the PSCIS reference ID for crossings that underwent Phase 1 assessments within the same program and cross-reference to Phase 2 data.

-   Grab the modeling outputs for our watershed group of interest from postgres built from bcfishpass and burn to a local sqlite to provide reproducible snapshot in time. Things change alot as modelling is based on certain data and assumptions. Also, grab the bcfishpass [spawning and rearing parameters](https://github.com/smnorris/bcfishpass/tree/main/02_model) table and put in the database so it can be used to populate the methods and tie to the references table.
