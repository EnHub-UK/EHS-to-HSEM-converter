#' -----------------------------------------------------------------------------
#' EHS Converter                                                       {Setup}
#'
#' This loads required libraries, system variables,
#' and loads basic datasets, functions and global variables.
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'

# Environment set-up -----------------------------------------------------------

# (a) Required libraries ----

load_r_libraries <- function(){

  message("Loading libraries")
  packageStartupMessage("initializing ...", appendLF = FALSE)

  # ... for loading and parsing data
  library(haven)
  library(foreign)
  library(pbapply)
  library(tibble)
  library(plyr)
  library(dplyr)
  library(survey)
  library(Hmisc)
  library(reshape2)

  # ... for making summaries and charts
  library(ggplot2)
  library(scales)
  library(pyramid)
  library(wesanderson)
  library(treemap)

  packageStartupMessage(" done")

}

load_r_libraries()
rm(load_r_libraries)

# (b) Auxiliary Functions ----
source("scripts/modules/environment.R", verbose = FALSE)
source("scripts/modules/parser.R", verbose = FALSE)
source("scripts/modules/conversion.R", verbose = FALSE)


# Display project state --------------------------------------------------------

#.. obtains and stores global path
path_ehs <- path.expand(getwd())

#.. lists available datasets
d_sets <-
  data.frame(name=list.files(path=paste0(path_ehs,"/data"), pattern="^UKDA"))

#.. creates folders in needed
make_output_folders(d_sets$name)
d_sets <- display_datasets(d_sets$name)
