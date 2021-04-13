#' -----------------------------------------------------------------------------
#' EHS Converter                                        {Conversion Requester}
#'
#' This second stage converts EHS tables into the suitable HSEM format.
#' 
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'
#' @run   `Rscript myScripts/B__conversion-HSEMs__export.R 'extended'`
#'
#' @notes  + The conversion works with the 2011 version only.
#'         + see `<root>/README` file
#'


# [0] Collect arguments --------------------------------------------------------

args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

(setExp = as.character(args[1]))

#' @Test:
#' setExp='extended'

if(setExp!='basic' & setExp!='extended' & setExp!='matching'){
  stop("Please check again the mode!")
}else{
  cat("\014")
  .proj.Mode <- 'B_conversion'
  message(paste("R-Project: EHS/Converter -------------------- mode:", setExp))
}



# [1] Setup --------------------------------------------------------------------

#.. global environment ----
source('myScripts/__setup_Project.R', verbose = FALSE)

#.. project parameters ----
var.EHSversion <- "UKDA-7386-stata9"     # definition/selection of stata folder
source('myScripts/_aux_GetData.R')       # data collection (~ 10 sec)

#.. additional workflows ----
source('myScripts/_aux_Conversion.R', verbose = FALSE)
source('myScripts/B__conversion-HSEMs_allocation.R', verbose = FALSE)



# [2] Generate Datasets --------------------------------------------------------

lst.HSEMs <- pblapply(ls()[grep("lst\\.[A-G]$",ls())], get)
names(lst.HSEMs) <- lblHSEMs
dtaHSEMS <- join_all(by='V001_HousingCode', lst.HSEMs)

if(setExp=='basic'){

  saveRDS(lst.HSEMs, paste0(path.EHS.datamd, file="/lstHSEMs.rds"))
  write.csv(dtaHSEMS, paste0(path.EHS.datamd, "/tblHSEMs.csv"))

}else if(setExp=='extended'){

  lst.HSEMs[['complementary']] <- lst.H
  dtaHSEMS <- join(dtaHSEMS, lst.H, by='V001_HousingCode')
  
  lst.HSEMs[['summarised']] <- lst.I
  dtaHSEMS <- join(dtaHSEMS, lst.I, by='V001_HousingCode')

  saveRDS(lst.HSEMs, paste0(path.EHS.datamd, file="/lstHSEMsExt.rds"))
  write.csv(dtaHSEMS, paste0(path.EHS.datamd, "/tblHSEMsext.csv"))

}else{

  write.csv(lst.M, paste0(path.EHS.datamd, "/tblHSEMsMatch.csv"))

}

rm(list = ls(a = TRUE))
