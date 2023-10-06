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
#' @run   `Rscript scripts/main.R 'conversion'`
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

(wkf = as.character(args[1]))

#' @Test:
#' wkf='conversion'

if (wkf != "conversion" & wkf != "households" & wkf != "predictive") {
  cat("\014")
  stop("Please check again the mode!")
} else {
  cat("\014")
  message(paste("R-Project: EHS/Converter -------------------- mode:", wkf))
}



# [1] Set-up -------------------------------------------------------------------

#.. global environment ----
source('scripts/setup.R', verbose = FALSE)

#.. project parameters ----
i_ehs_version <- "UKDA-7386-stata11"    # selection of STATA folder
source('scripts/modules/load_data.R')   # data collection (~ 10 sec)



# [2] Generate Datasets --------------------------------------------------------

if(wkf=='conversion'){

  source('scripts/workflows/B__conversion.R', verbose = FALSE)

  #.. full conversion (incl. contextual data and household info)

  l_hsem <- pblapply(ls()[grep("lst\\.[A-J]$",ls())], get)
  names(l_hsem) <- lbl_hsem
  df_hsem <- join_all(by='V001_HousingCode', l_hsem)

  saveRDS(l_hsem, file.path(path_datamd, file="lstHSEMsExt.rds"))
  write.csv(df_hsem, file.path(path_datamd, "tblHSEMsext.csv"))


  #.. basic conversion

  l_hsem_basic <-
    l_hsem[names(l_hsem) %in% c("common","complementary") == FALSE]
  df_hsem_basic <- join_all(by='V001_HousingCode', l_hsem_basic)

  saveRDS(l_hsem_basic, file.path(path_datamd, file="lstHSEMs.rds"))
  write.csv(df_hsem_basic, file.path(path_datamd, "tblHSEMs.csv"))


  #.. generate subset with matching variables

  write.csv(l_hsem$common, file.path(path_datamd, "tblHSEMsMatch.csv"))

  cat("\014")
  message(paste("Exported data in:\n", path_datamd))

}else if(wkf=='households'){

  source('scripts/workflows/C__households.R', verbose = FALSE, echo = FALSE)

}else if(wkf=='predictive'){

  source('scripts/workflows/D__predictive.R', verbose = FALSE, echo = FALSE)

}else{

  warning("nothing selected!")

}

rm(list = ls(a = TRUE))
