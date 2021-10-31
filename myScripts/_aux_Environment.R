#' -----------------------------------------------------------------------------
#' EHS Converter                                   {Auxiliary / R Environment}
#'
#' This contains auxiliary functions for the project environment.
#' 
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'

fnGetOS <- function(...) {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if ((.Platform$OS.type == "unix") &
             (Sys.info()["sysname"] == "linux-gnu")) {
    "linux"
  } else if ((.Platform$OS.type == "unix") &
             (Sys.info()["nodename"] == "hpclogin")) {
    "hpc"
  } else {
    stop("Mystery Machine")
  }
}

fnPrepareEnHubOutputFolders <- function(dirs){
  dirs <- as.character(dirs)
  type <- c('outForModel','outReport','outForProfiles')
  out.folder <- 'public'

  assignDir <- function(name, outtype, folder){
    var.paths <- paste(folder, outtype, name, sep="/")
    fnMakeDir(var.paths)
    return(var.paths)
  }

  lapply(dirs, assignDir, type[1], out.folder)
  lapply(dirs, assignDir, type[2], out.folder)
  fnMakeDir(paste(out.folder, type[3], sep="/"))
}

fnMakeDir <- function(pathNew, myPath=path.EHS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathNew <- paste(normalizePath(myPath), gsub("/","\\\\",pathNew), sep="")
    shell(paste("mkdir ", pathNew, sep=""))
  }else{
    pathNew <- paste(myPath,"/",pathNew, sep="")
    system(paste("mkdir -p ", pathNew, "/", sep=""))
  }
}

fnRemoveDir <- function(pathNew, myPath=path.EHS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathNew <- paste(normalizePath(myPath), gsub("/","\\\\",pathNew), sep="")
    shell(paste("del /S /Q ", pathNew, sep="")) # /S in dir, /Q quiet
  }else{
    system(paste("rm -rf ", pathNew, "/", sep=""))
  }
}

fnRemoveDirFiles <- function(pathNew, myPath=path.EHS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathNew <- paste(normalizePath(myPath), gsub("/","\\\\",pathNew), sep="")
    shell(paste("del /S /Q *", pathNew, sep="")) # /S in dir, /Q quiet
  }else{
    system(paste("rm -f ", pathNew, "/*", sep=""))
  }
}

fnZipFile <- function(pathNew, myPath=path.EHS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathZip <- paste(normalizePath(myPath), "gzip.exe", sep="")
    pathNew <- paste(normalizePath(myPath), gsub("/","\\\\",pathNew), sep="")
    shell(paste(pathZip, pathNew, sep=" "))
  }else{
    system(paste("gzip ", pathNew, sep=""))
  }
}

fnUnZipFile <- function(pathNew, myPath=path.EHS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathZip <- paste(normalizePath(myPath), "gzip.exe -d", sep="")
    pathNew <- paste(normalizePath(myPath), gsub("/","\\\\",pathNew), sep="")
    shell(paste(pathZip, pathNew, sep=" "))
  }else{
    system(paste("gunzip ", normalizePath(myPath), "/", pathNew, sep=""))
  }
}

fnMoveFile <- function(varFrom, varTo, myPath=path.EHS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathFrom <- paste(normalizePath(myPath), gsub("/","\\\\",varFrom), sep="")
    pathTo <- paste(normalizePath(myPath), gsub("/","\\\\",varTo), sep="")
    shell(paste("move", pathFrom, pathTo, sep=" "))
  }else{
    system(paste("mv", varFrom, varTo, sep=" "))
  }
}

fnDisplayActiveData <- function(lbl){
  tblStudies <- data.frame(info=unlist(lapply(lbl, getStudyName)), code=lbl)
  cat("\014")
  message("Avaiable EHS datasets --- note these are symlinks \u21AF ")
  message(paste0(capture.output(tblStudies), collapse = "\n"))
}

#..create a line-by-line list with archetype information -----
inc <- function(x){ln. <<- x + 1}
