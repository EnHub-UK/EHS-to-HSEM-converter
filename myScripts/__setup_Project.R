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

# Environment SetUp ------------------------------------------------------------

# (a) Required libraries ----

fnStartLibraries <- function(){
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

fnStartLibraries()
rm(fnStartLibraries)

# (b) Auxiliary Functions ----
source('myScripts/_aux_Environment.R')
source('myScripts/_aux_Parsing.R')
source('myScripts/_aux_PlotFunctions.R')



# Display project state --------------------------------------------------------

#.. obtains and stores global path
path.EHS <- path.expand(getwd())

#.. lists available datasets
var.EHSdata <-
  data.frame(name=list.files(path=paste0(path.EHS,"/myData"), pattern="^UKDA"))

#.. creates folders in needed
fnPrepareEnHubOutputFolders(var.EHSdata$name)
fnDisplayActiveData(var.EHSdata$name)
