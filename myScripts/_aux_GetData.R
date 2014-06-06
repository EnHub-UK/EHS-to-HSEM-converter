#' -----------------------------------------------------------------------------
#' EHS Converter                                          {Auxiliary / Loader}
#'
#' @file `_aux_GetData.R` contains auxiliary functions to load raw data
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'
#' @notes
#'   + structure of `stata` folders
#'       .
#'       ├── derived
#'       │   └── detailed
#'       ├── fuel_poverty
#'       ├── interview
#'       └── physical
#'
#'       5 directories
#'


# Collection of Raw Data  ------------------------------------------------------
tblTUSRawSubsets <- fnLoadRawData(fnGetDataFolder(var.EHSversion))


# Extraction of Standard Parameters --------------------------------------------
tblEHSRawSubsets <- names(tblTUSRawSubsets)
tblVariableNames <- lapply(tblEHSRawSubsets, fnGetVarNames)
tblVariableUnique <- as.character(unique(unlist(tblVariableNames)))
tblDatasetNames <- as.character(unlist(lapply(1:length(tblVariableNames),
                          function(x) names(tblVariableNames[[x]]))))


# Homogenisation for analysis --------------------------------------------------

# (1) Process main datasets ----------------------------------------------------
lst.EHS.sets <- list()
if(any(grepl(var.reg <- "general(_|*plus|fs*)", ls()))){
  lst.EHS.sets[['general']] <- get(ls(pattern = var.reg))
  lst.EHS.sets[['general']]  <- fnGetControlVariables(lst.EHS.sets$general)
}
if(any(grepl(var.reg <- "physical(_|*plus|fs*)", ls()))){
  lst.EHS.sets[['physical']]  <- get(ls(pattern = var.reg))
  lst.EHS.sets[['physical']] <- fnGetControlVariables(lst.EHS.sets$physical)
}
if(any(grepl(var.reg <- "interview(_|*plus|fs*)", ls()))){
  lst.EHS.sets[['interview']] <- get(ls(pattern = var.reg))
  lst.EHS.sets[['interview']] <- fnGetControlVariables(lst.EHS.sets$interview)
}


# (2) Combine main datasets ----------------------------------------------------
dta.EHS.wide <- as_tibble(join_all(lst.EHS.sets, by=c("aacode")))

if(('weightdwell' %in% colnames(dta.EHS.wide)) &
   (!'weighthshld' %in% colnames(dta.EHS.wide))){
  dta.EHS.wide$weighthshld <- dta.EHS.wide$weightdwell}

if(('weighthshld' %in% colnames(dta.EHS.wide)) &
   (!'weightdwell' %in% colnames(dta.EHS.wide))){
  dta.EHS.wide$weightdwell <- dta.EHS.wide$weighthshld}

rm(var.reg, lst.EHS.sets)


# (3) Generate survey data (i.e., weighted values) -----------------------------
dta.EHS.survey <-
  svydesign(id=~aacode, weights=~weightdwell, data=dta.EHS.wide)
summary(dta.EHS.survey)


# (4) Additional re-formatting -------------------------------------------------

if(any(ls() %in% 'elevate')){
  if(!is.null(elevate$felorien)){
    elevate$orientation <-
      factor(as.integer(elevate$felorien), labels = c(seq(0, 359, 45), NA))
    elevate$orientation <-
      as.integer(as.character(elevate$orientation))
  }
}


# Prepare output folders and display info --------------------------------------

cat("\014")
message(getStudyName())
path.EHS.report <- paste0('public/outReport/', var.EHSversion)
path.EHS.datamd <- paste0('public/outForModel/', var.EHSversion)
