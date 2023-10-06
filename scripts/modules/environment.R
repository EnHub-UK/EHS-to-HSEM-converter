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

get_os <- function(...) {
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

make_output_folders <- function(dirs){
  dirs <- as.character(dirs)
  type <- c('outForModel','outReport','outForProfiles','outForGraph')
  out.folder <- 'export'

  assignDir <- function(name, outtype, folder){
    var.paths <- paste(folder, outtype, name, sep="/")
    dir.create(var.paths, recursive = T, showWarnings = F)
    return(var.paths)
  }

  lapply(dirs, assignDir, type[1], out.folder)
  lapply(dirs, assignDir, type[2], out.folder)
  dir.create(paste(out.folder, type[3], sep="/"),
             recursive = T, showWarnings = F)
}

#..create a line-by-line list with archetype information -----
inc <- function(x){ln. <<- x + 1}
