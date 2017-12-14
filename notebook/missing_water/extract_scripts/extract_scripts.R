#------------------------------------------------------------------------------
# Extract
knitr::purl(file.path(rprojroot::find_rstudio_root_file(), "notebook/prepare_lagk/prepare_lagk.Rmd"),
            file.path(rprojroot::find_rstudio_root_file(), "notebook/missing_water/extract_scripts/prepare_lagk.R"))

knitr::purl(file.path(rprojroot::find_rstudio_root_file(), "notebook/missing_water/missing_water_assessment.Rmd"),
            file.path(rprojroot::find_rstudio_root_file(), "notebook/missing_water/extract_scripts/missing_water_assessment.R"))
#------------------------------------------------------------------------------
# Run
evaluate <- TRUE
source(file.path(rprojroot::find_rstudio_root_file(), "notebook/missing_water/extract_scripts/prepare_lagk.R"))
source(file.path(rprojroot::find_rstudio_root_file(), "notebook/missing_water/extract_scripts/missing_water_assessment.R"))
#------------------------------------------------------------------------------

