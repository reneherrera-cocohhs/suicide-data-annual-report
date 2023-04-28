# List of packages needed ####
packages_needed_list <- c(
  "here", # https://github.com/r-lib/here
  "tidyverse", # https://github.com/tidyverse/tidyverse
  "pins", # https://github.com/rstudio/pins
  "janitor", # https://github.com/sfirke/janitor
  "readxl", # https://github.com/tidyverse/readxl
  "curl", # https://github.com/jeroen/curl
  "purrr", # https://github.com/tidyverse/purrr
  "scales", # https://github.com/r-lib/scales
  "tidycensus", # https://github.com/walkerke/tidycensus
  "zipcodeR", # https://github.com/gavinrozzi/zipcodeR/
  "tigris", # https://github.com/walkerke/tigris
  "sf", # https://github.com/r-spatial/sf/
  "cowplot", # https://github.com/wilkelab/cowplot
  "tidygeocoder", # https://jessecambon.github.io/tidygeocoder/index.html
  "slider", # https://www.tidyverse.org/blog/2020/02/slider-0-1-0/
  "renv",
  "styler",
  "base64enc",
  "digest",
  "evaluate",
  "glue",
  "highr",
  "htmltools",
  "jquerylib",
  "jsonlite",
  "knitr",
  "magrittr",
  "markdown",
  "mime",
  "rmarkdown",
  "stringi",
  "stringr",
  "tinytex",
  "xfun",
  "yaml",
  "epitools",
  "miniUI",
  "gtsummary",
  "gt",
  "kableExtra",
  "pandoc",
  "lintr",
  "styler",
  "renv",
  "rlang",
  "basemaps"
)

# function #### source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not
ipak <- function(pkg) {
  # check to see if packages are installed
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg)) {
    install.packages(new_pkg, dependencies = TRUE)
  } # Install them if they are not
}

# call function ####
ipak(packages_needed_list)

# initiate R environment https://rstudio.github.io/renv/articles/renv.html
renv::init()

# R environment status
renv::status()

# add packages to lockfile
renv::snapshot()

# lint and style ####
library(here)
library(lintr)
library(styler)

lint_dir(path = "../suicide-data-annual-report/")
style_dir(path = "../suicide-data-annual-report/")
