#' -----------------------------------------------------------------------------
#' EHS Converter                                    {Auxiliary / Data Parsers}
#'
#' @file `_aux_Parsing.R` contains auxiliary functions for parsing tables
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'

fnLoadEHSRawFiles <- function(){

  files <- list.files(pattern="dta", recursive=TRUE)
  files <- files[!is.na(files)]

  fnLoadEHSRawFiles <- function(file){
    perpos <- which(strsplit(file, "")[[1]]==".")   # find the '.*' in file
    prepos <- which(strsplit(file, "")[[1]]=="/")   # find the '.*' in file
    prepos <- ifelse(is.integer(prepos) && length(prepos) == 0L, 0, prepos)

    lbl.object <- gsub(" ","", substr(file, prepos + 1, perpos - 1))
    lbl.object <- gsub("/","__", lbl.object)
    lbl.object <- gsub("\\d{2}plus\\d{2}", "plus", lbl.object)

    if(grepl("^D",.proj.Mode)){
      assign(lbl.object,
             as_tibble(read_dta(file)),
             envir = globalenv())
    }else{
      assign(lbl.object,
             as_tibble(read.dta(file, warn.missing.labels=F)),
             envir = globalenv())
    }

    tbl.loaded <- get(lbl.object)
    if(!is.na(match("serialanon", names(tbl.loaded)))){
      tbl.loaded$aacode <- tbl.loaded$serialanon
      tbl.loaded$serialanon <- NULL
      tbl.loaded <- tbl.loaded %>% select(aacode, everything())
    }

    lbl.return <- list(lbl.object, tbl.loaded)
    return(lbl.return)
  }

  varEHSRawSub <- pblapply(files, fnLoadEHSRawFiles)

  varEHSRawSub.names <- as.character(lapply(varEHSRawSub, `[[`, 1))
  varEHSRawSub <- lapply(varEHSRawSub, `[[`, 2)
  names(varEHSRawSub) <- varEHSRawSub.names

  return(varEHSRawSub)
}

fnLoadRawData <- function(path.RawData){
  setwd(path.RawData)
  tblEHSRawSubsets <- fnLoadEHSRawFiles()
  setwd(path.EHS)
  return(tblEHSRawSubsets)
}

fnGetDataFolder <- function(varEHS.Data){
  path.RawData <- paste(path.EHS,"myData",varEHS.Data,sep="/")
  valNameData <- list.files(path=path.RawData, pattern = "^sta")
  path.RawData <- paste(path.EHS,"myData",varEHS.Data,valNameData,sep="/")
  return(path.RawData)
}

fnGetVarNames <- function(dtaName){
  lstEnvTmp <- get(dtaName)
  names(lstEnvTmp) <- gsub("\xa3|\xc2","", names(lstEnvTmp)) # removes £ sign
  lstEnvTmp <- tolower(names(lstEnvTmp))
  lstEnvTmp <- list(lstEnvTmp)
  names(lstEnvTmp) <- c(dtaName)
  return(lstEnvTmp)
}

findDocStrings <- function(varToSearch){
  path.EHS_data <- paste(path.EHS, "myData/AuxiliaryData/allissue",
                         var.EHSversion,"/", sep="/")

  setwd(path.EHS_data)

  if (fnGetOS() == "mac" | fnGetOS() == "unix") {
    exename <- "grep -rl ";
  } else {
    exename <- "dir /B | findstr /R /C: msdos";
  }

  command <- paste(exename,"'",varToSearch,"' *", sep=""); rm(exename);
  searchResult <- try(system(command, wait = F, intern = T))
  searchResult <- gsub("_ukda_data_dictionary.rtf.txt", "", searchResult,
                       perl=TRUE, ignore.case = FALSE)

  setwd(path.EHS)

  return(searchResult)
}

getStudyName <- function(varVer=var.EHSversion){

  path.EHS_data <- paste(path.EHS, "myData", varVer,sep="/")

  setwd(path.EHS_data)

  if (fnGetOS() == "mac" | fnGetOS() == "unix") {
    command <- "grep -o 'Study Number .*\\<' *.htm";
  } else {
    command <- "dir /B | findstr /R -o 'Study Number .*\\<' *.htm";
  }
  searchResult <- try(system(command, wait = F, intern = T))

  setwd(path.EHS)

  return(searchResult)
}

fnGetControlVariables <- function(dtaTo){
  val.code.dwll <- grep("serialanon|aacode", colnames(dtaTo))
  val.weight.dwll <- grep("aag[[:alpha:]]?d", colnames(dtaTo))
  val.weight.hhld <- grep("aag[[:alpha:]]?h", colnames(dtaTo))
  colnames(dtaTo)[val.code.dwll] <- "aacode"
  colnames(dtaTo)[val.weight.dwll] <- "weightdwell"
  colnames(dtaTo)[val.weight.hhld] <- "weighthshld"
  colnames(dtaTo) <- tolower(colnames(dtaTo))
  dtaTo$aacode <- as.character(dtaTo$aacode)
  return(dtaTo)
}
