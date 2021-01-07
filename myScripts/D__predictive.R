#' -----------------------------------------------------------------------------
#' EHS Converter                              {Merger for predictive analysis}
#'
#' @file `D__predictive.R` combines multiple EHS version for developing
#'       a predictor of archetype circumstances
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EHS-to-HSEM-converter
#'
#' @notes  + The rationale for the selection of variables is further
#'           explained in both TUS-parser and EnHub projects.
#'


# Setup ------------------------------------------------------------------------

#.. global environment ----
rm(list = ls(a = TRUE))
.proj.Mode <- 'D_predictive'
source('myScripts/__setup_Project.R', verbose = FALSE)

file.merged <- paste0(path.EHS,"/public/outForProfiles/all_merged.rds")
file.common <- paste0(path.EHS,"/public/outForProfiles/all_merged_common.rds")


# Auxiliary Functions (internal) ----

fnLoadOneYear <- function(varStudy){

  print(getStudyName(varStudy))

  fnLoadEHsTables <- function(varVersion){
    tblTUSRawSubsets <- fnLoadRawData(fnGetDataFolder(varVersion))
    return(tblTUSRawSubsets)
  }

  # (0) Load Stata files -------------------------------------------------------

  lst.EHS.version <- lst.EHS.sets <- list()
  varEnvIni <- ls(pos = ".GlobalEnv")

  lst.EHS.version <- fnLoadEHsTables(varStudy)

  varEnvNew <- ls(pos = ".GlobalEnv")
  varEnvDel <- varEnvNew[!varEnvNew %in% varEnvIni]

  # (1) Process main datasets --------------------------------------------------

  if(any(grepl(var.reg <- "general(_|*plus|fs*)", ls(pos = ".GlobalEnv")))){
    lst.EHS.sets[['general']] <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
    lst.EHS.sets[['general']]  <- fnGetControlVariables(lst.EHS.sets[['general']])
  }
  if(any(grepl(var.reg <- "physical(_|*plus|fs*)", ls(pos = ".GlobalEnv")))){
    lst.EHS.sets[['physical']]  <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
    lst.EHS.sets[['physical']] <- fnGetControlVariables(lst.EHS.sets[['physical']])
  }
  if(any(grepl(var.reg <- "interview(_|*plus|fs*)", ls(pos = ".GlobalEnv")))){
    lst.EHS.sets[['interview']] <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
    lst.EHS.sets[['interview']] <- fnGetControlVariables(lst.EHS.sets[['interview']])
  }

  # (2) Combine main datasets --------------------------------------------------
  dta.EHS.wide <- as_tibble(join_all(lst.EHS.sets, by=c("aacode")))

  if(('weightdwell' %in% colnames(dta.EHS.wide)) &
     (!'weighthshld' %in% colnames(dta.EHS.wide))){
    dta.EHS.wide$weighthshld <- dta.EHS.wide$weightdwell}

  if(('weighthshld' %in% colnames(dta.EHS.wide)) &
     (!'weightdwell' %in% colnames(dta.EHS.wide))){
    dta.EHS.wide$weightdwell <- dta.EHS.wide$weighthshld}

  rm(var.reg, lst.EHS.sets)
  rm(list=varEnvDel, pos = ".GlobalEnv")

  return(dta.EHS.wide)
}

fnRestoreClasses <- function(varEval, lstRef, lstEnd){

  # .. get table of classes
  tblRef <- as.data.frame(t(as.data.frame(lapply(lstRef, class))))
  tblRef <- tblRef[rownames(tblRef) %in% colnames(lstEnd),]
  tblRef$var <- rownames(tblRef)
  rownames(tblRef) <- NULL

  # .. get class of variable
  varCheck <- tblRef$V3[tblRef$var==varEval]

  # .. process tables
  tblEnd <- as.data.frame(subset(lstEnd, select = varEval))
  colnames(tblEnd) <- 'variable'

  tblRef <- as.data.frame(subset(lstRef, select = varEval))
  colnames(tblRef) <- 'variable'

  # .. transform table
  if(varCheck=='double'){

    tblParsed <- capture.output(print_labels(tblRef$variable))
    tblParsed <- tblParsed[4:length(tblParsed)]
    tblParsed <- data.frame(
      labels=gsub("^ *([[:punct:]]|\\d)* *","", tblParsed, perl=T),
      levels=gsub("(^ *([[:punct:]]\\d*|\\d*) )(.*)","\\2", tblParsed, perl=T))
    tblParsed$levels <- as.numeric(tblParsed$levels)


    if(!any(grepl("*more*",tblParsed$labels)) & dim(tblParsed)[1]>2){
      tblEnd$variable <- factor(tblEnd$variable,
                                levels = tblParsed$levels,
                                labels = tblParsed$labels)
    }else{
      tblEnd$variable <- as.numeric(as.character(unlist(tblEnd$variable)))
      tblEnd$variable[tblEnd$variable<0] <- NA
    }

  }else if(varCheck=='numeric'){
    tblEnd$variable <- as.numeric(as.character(unlist(tblEnd$variable)))
  }else if(varCheck=='character'){
    tblEnd$variable <- as.character(unlist(tblEnd$variable))
  }else{
    stop("invalid type of variable...")
  }

  colnames(tblEnd) <- varEval

  return(tblEnd)
}



# Combination Project ---------------------------------------------------------

path.EHS <- path.expand(getwd())
var.EHSdata <- data.frame(name = list.files(
  path = paste0(path.EHS,"/myData"), pattern = "^UKDA"))

lst.EHS.all <- pblapply(var.EHSdata$name, fnLoadOneYear)
names(lst.EHS.all) <- gsub("-","_",var.EHSdata$name)

saveRDS(lst.EHS.all, file=file.merged)
# lst.EHS.all <- readRDS(file=file.merged)





# Common Project --------------------------------------------------------------

lst.vars <- pblapply(lst.EHS.all, function(x) colnames(x))
lst.EHS.common <- pblapply(lst.EHS.all, function(x)
  data.matrix(subset(x, select=Reduce(intersect, lst.vars))))
lst.EHS.common <- do.call("rbind", lst.EHS.common)
lst.EHS.common <- pblapply(colnames(lst.EHS.common), fnRestoreClasses,
                           lst.EHS.all$UKDA_8494_stata, lst.EHS.common)
lst.EHS.common <- as_tibble(do.call("cbind", lst.EHS.common))
lst.EHS.common$aacode <- NULL

saveRDS(lst.EHS.common,  file=file.common)
# lst.EHS.common <- readRDS(file=file.common)




# ~~~~ Python excerpt / Mathilde's project ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# general = pd.read_csv('general.csv',sep=';')
# interview = pd.read_csv('interview.csv',sep=';')
# physical = pd.read_csv('physical.csv',sep=';')
#
# ehstot = ehstot[['aacode','area3x','dwtypenx','NBedsX','gorEHCS','sexhrp',
#                  'emphrpx','agehrpx','tenure4x','hhincx', 'hhtype6',
#                  'agepartx','hhsizex','ndepchild','pyngbx','empprtx']]
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# End --------------------------------------------------------------------------
