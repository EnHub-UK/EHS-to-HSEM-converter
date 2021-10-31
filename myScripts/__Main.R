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
#' @run   `Rscript myScripts/__Main.R 'conversion'`
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
#' setExp='conversion'

if(setExp!='conversion' & setExp!='households' & setExp!='predictive'){
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




# [2] Generate Datasets --------------------------------------------------------

if(setExp=='conversion'){

  source('myScripts/B__conversion.R', verbose = FALSE)

  lst.HSEMs <- pblapply(ls()[grep("lst\\.[A-I]$",ls())], get)
  names(lst.HSEMs) <- lblHSEMs
  dtaHSEMS <- join_all(by='V001_HousingCode', lst.HSEMs)

  #.. basic conversion

  saveRDS(lst.HSEMs, paste0(path.EHS.datamd, file="/lstHSEMs.rds"))
  write.csv(dtaHSEMS, paste0(path.EHS.datamd, "/tblHSEMs.csv"))

  #.. additional conversion (incl. contextual data and household info)

  lst.HSEMs[['complementary']] <- lst.H
  dtaHSEMS <- join(dtaHSEMS, lst.H, by='V001_HousingCode')

  lst.HSEMs[['summarised']] <- lst.I
  dtaHSEMS <- join(dtaHSEMS, lst.I, by='V001_HousingCode')

  saveRDS(lst.HSEMs, paste0(path.EHS.datamd, file="/lstHSEMsExt.rds"))
  write.csv(dtaHSEMS, paste0(path.EHS.datamd, "/tblHSEMsext.csv"))

  #.. generate subset with matching variables

  write.csv(lst.M, paste0(path.EHS.datamd, "/tblHSEMsMatch.csv"))

  cat("\014")
  message(cat(paste("Exported data in:\n",path.EHS.datamd)))

}else if(setExp=='households'){

  source('myScripts/C__households.R', verbose = FALSE, echo = FALSE)

}else if(setExp=='predictive'){

  source('myScripts/D__predictive.R', verbose = FALSE, echo = FALSE)

}else{

  warning("nothing selected!")

}

rm(list = ls(a = TRUE))
