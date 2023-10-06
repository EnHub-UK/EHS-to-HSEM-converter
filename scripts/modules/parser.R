#' -----------------------------------------------------------------------------
#' EHS Converter                                    {Auxiliary / Data Parsers}
#'
#' This file contains auxiliary functions process tables and extract info.
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'

display_datasets <- function(k_ehs) {
  d_studies <- data.frame(
    info = unlist(lapply(k_ehs, get_study_name)), code = k_ehs
  ) %>%
    mutate(is_stock_data = ifelse(grepl("Housing Stock Data", info), T, F)) %>%
    mutate(period = gsub(".*(\\,\\s)(\\d+\\-?\\d+?)(\\:).*", "\\2", info))

  cat("\014")
  message("Avaiable EHS datasets --- note the use of symlinks \u21AF ")
  print(jsonlite::toJSON(d_studies, pretty = T))

  return(d_studies)
}

load_ehs_files <- function(){

  files <- list.files(pattern="dta", recursive=TRUE)
  files <- files[!is.na(files)]

  load_ehs_files <- function(file){
    perpos <- which(strsplit(file, "")[[1]]==".")   # find the '.*' in file
    prepos <- which(strsplit(file, "")[[1]]=="/")   # find the '.*' in file
    prepos <- ifelse(is.integer(prepos) && length(prepos) == 0L, 0, prepos)

    lbl.object <- gsub(" ","", substr(file, prepos + 1, perpos - 1))
    lbl.object <- gsub("/","__", lbl.object)
    lbl.object <- gsub("\\d{2}plus\\d{2}", "plus", lbl.object)

    # Read STATA files (using `haven` library)
    assign(lbl.object,
           as_tibble(as_factor(read_dta(file))),
           envir = globalenv())


    tbl.loaded <- get(lbl.object)
    if(!is.na(match("serialanon", names(tbl.loaded)))){
      tbl.loaded$aacode <- tbl.loaded$serialanon
      tbl.loaded$serialanon <- NULL
      tbl.loaded <- tbl.loaded %>% select(aacode, everything())
    }

    lbl.return <- list(lbl.object, tbl.loaded)
    return(lbl.return)
  }

  varEHSRawSub <- pblapply(files, load_ehs_files)

  varEHSRawSub.names <- as.character(lapply(varEHSRawSub, `[[`, 1))
  varEHSRawSub.names <- gsub("^stat./*\\d__","",varEHSRawSub.names)
  varEHSRawSub <- lapply(varEHSRawSub, `[[`, 2)
  names(varEHSRawSub) <- varEHSRawSub.names

  return(varEHSRawSub)
}

load_raw_datasets <- function(path.RawData){
  setwd(path.RawData)
  lbl_ehs_subsets <- load_ehs_files()
  setwd(path_ehs)
  return(lbl_ehs_subsets)
}

load_dataset <- function(varStudy, varTable=''){

  print(get_study_name(varStudy))

  fnLoadEHsTables <- function(varVersion){
    l_ehs_subsets <- load_raw_datasets(locate_data_dir(varVersion))
    return(l_ehs_subsets)
  }

  # (0) Load Stata files -------------------------------------------------------

  lst.EHS.version <- l_ehs_sets <- list()
  varEnvIni <- ls(pos = ".GlobalEnv")

  lst.EHS.version <- fnLoadEHsTables(varStudy)

  varEnvNew <- ls(pos = ".GlobalEnv")
  varEnvDel <- varEnvNew[!varEnvNew %in% varEnvIni]

  # (1) Process main datasets --------------------------------------------------

  if(varTable=='' | varTable=='default'){

    if(any(grepl(var.reg <- "general(_|*plus|fs*|\\d)", ls(pos = ".GlobalEnv")))){
      l_ehs_sets[['general']] <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
      l_ehs_sets[['general']]  <- obtain_control_variables(l_ehs_sets[['general']])
    }
    if(any(grepl(var.reg <- "physical(_|*plus|fs*|\\d)", ls(pos = ".GlobalEnv")))){
      l_ehs_sets[['physical']]  <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
      l_ehs_sets[['physical']] <- obtain_control_variables(l_ehs_sets[['physical']])
    }
    if(any(grepl(var.reg <- "interview(_|*plus|fs*|\\d)", ls(pos = ".GlobalEnv")))){
      l_ehs_sets[['interview']] <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
      l_ehs_sets[['interview']] <- obtain_control_variables(l_ehs_sets[['interview']])
    }

    d_ehs_wide <- as_tibble(join_all(l_ehs_sets, by=c("aacode")))

  }else{

    if(any(grepl(var.reg <- varTable, ls(pos = ".GlobalEnv")))){
      d_ehs_wide <- get(ls(pattern = var.reg, pos = ".GlobalEnv"))
      d_ehs_wide <- obtain_control_variables(d_ehs_wide)
    }else{
      d_ehs_wide <- ""
      warning("No data available!")
    }

  }


  # (2) Combine main datasets --------------------------------------------------

  if(('weightdwell' %in% colnames(d_ehs_wide)) &
     (!'weighthshld' %in% colnames(d_ehs_wide))){
    d_ehs_wide$weighthshld <- d_ehs_wide$weightdwell}

  if(('weighthshld' %in% colnames(d_ehs_wide)) &
     (!'weightdwell' %in% colnames(d_ehs_wide))){
    d_ehs_wide$weightdwell <- d_ehs_wide$weighthshld}

  rm(var.reg, l_ehs_sets)
  rm(list=varEnvDel, pos = ".GlobalEnv")

  return(d_ehs_wide)
}

homogenise_variable <- function(lst, varTo){

  lst[varTo] <- lst %>% select(all_of(varTo)) %>%
    mutate(across(where(is.factor), as.character)) %>%
    mutate(across(where(is.character),
                  function(x) gsub("[[:alpha:]]|\\£|\\,","",x))) %>%
    mutate(across(where(is.character), as.numeric))

  return(lst)
}

locate_data_dir <- function(path_dir){
  path_raw <- paste(path_ehs,"data",path_dir,sep="/")
  label_data <- list.files(path=path_raw, pattern = "^sta")
  path_raw <- paste(path_ehs,"data",path_dir,label_data,sep="/")
  return(path_raw)
}

obtain_variable_labels <- function(k_set){
  
  d_set <- get(k_set)
  
  # removes unusual symbols: £ sign
  names(d_set) <- gsub("\xa3|\xc2","", names(d_set), useBytes = T)
  d_set <- tolower(names(d_set))
  
  l_set <- list(d_set)
  names(l_set) <- c(k_set)
  
  return(l_set)
}

locate_variable_in_dataset <- function(k_lbl, k_set=i_ehs_version){
  
  path_ehs_data <- file.path(path_ehs, "data/data_dictionaries", k_set)

  setwd(path_ehs_data)

  if (get_os() == "mac" | get_os() == "unix") {
    exename <- "grep -rl ";
  } else {
    exename <- "dir /B | findstr /R /C: msdos";
  }

  command <- paste(exename,"'",k_lbl,"' *", sep=""); rm(exename);
  res <- try(system(command, wait = F, intern = T))
  res <- gsub("_ukda_data_dictionary.rtf.txt", "", res,
                       perl=TRUE, ignore.case = FALSE)

  setwd(path_ehs)

  return(res)
}

get_study_name <- function(i_ver = i_ehs_version){
  
  path_ehs_data <- paste(path_ehs, "data", i_ver, sep = "/")

  setwd(path_ehs_data)

  if (get_os() %in% c("mac", "unix")) {
    command <- "grep -o 'Study Number .*\\<' *.htm"
  } else {
    command <- "dir /B | findstr /R -o 'Study Number .*\\<' *.htm"
  }
  res <- try(system(command, wait = F, intern = T))

  setwd(path_ehs)

  res <- gsub("[[:punct:]]+$", "", res)

  return(res)
}

obtain_control_variables <- function(d){
  i_dwl_code <- grep("serialanon|aacode", colnames(d))
  i_dwl_weight <- grep("aagp?[[:alpha:]]?d", colnames(d))
  i_hhld_weight <- grep("aagp?[[:alpha:]]?h", colnames(d))
  colnames(d)[i_dwl_code] <- "aacode"
  colnames(d)[i_dwl_weight] <- "weightdwell"
  colnames(d)[i_hhld_weight] <- "weighthshld"
  colnames(d) <- tolower(colnames(d))
  d$aacode <- as.character(d$aacode)
  return(d)
}
