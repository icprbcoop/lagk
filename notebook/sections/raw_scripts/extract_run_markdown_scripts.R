#------------------------------------------------------------------------------
# Extract
knitr::purl("notebook/sections/introduction.Rmd",
            "notebook/sections/raw_scripts/extracted/introduction.R")
knitr::purl("notebook/sections/import_data.Rmd",
            "notebook/sections/raw_scripts/extracted/import_data.R")
knitr::purl("notebook/prepare_lagk/prepare_lagk.Rmd",
            "notebook/sections/raw_scripts/extracted/prepare_lagk.R")
#------------------------------------------------------------------------------
# Run
#evaluate <- TRUE
#cache.me <- TRUE
#cache.dir <- "markdown/sections/cache"

source("notebook/sections/raw_scripts/extracted/introduction.R")
source("notebook/sections/raw_scripts/extracted/import_data.R")
source("notebook/sections/raw_scripts/extracted/prepare_lagk.R")
#------------------------------------------------------------------------------
