#' -----------------------------------------------------------------------------
#' EHS Converter                                          {Auxiliary / Loader}
#'
#' This contains auxiliary functions to load raw data.
#'
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
l_ehs_subsets <- load_raw_datasets(locate_data_dir(i_ehs_version))


# Extraction of Standard Parameters --------------------------------------------
lbl_ehs_subsets <- names(l_ehs_subsets)
l_lbl <- lapply(lbl_ehs_subsets, obtain_variable_labels)
l_ehs_variables <- as.character(unique(unlist(l_lbl)))
rm(l_lbl, l_ehs_subsets)


# Homogenisation for analysis --------------------------------------------------

# (1) Process main datasets ----------------------------------------------------
l_ehs_sets <- list()
if (any(grepl(k_reg <- "general(_|*plus|fs*)", ls()))) {
  l_ehs_sets[["general"]] <- get(ls(pattern = k_reg))
  l_ehs_sets[["general"]] <- obtain_control_variables(l_ehs_sets$general)
}
if (any(grepl(k_reg <- "physical(_|*plus|fs*)", ls()))) {
  l_ehs_sets[["physical"]] <- get(ls(pattern = k_reg))
  l_ehs_sets[["physical"]] <- obtain_control_variables(l_ehs_sets$physical)
}
if (any(grepl(k_reg <- "interview(_|*plus|fs*)", ls()))) {
  l_ehs_sets[["interview"]] <- get(ls(pattern = k_reg))
  l_ehs_sets[["interview"]] <- obtain_control_variables(l_ehs_sets$interview)
}


# (2) Combine main datasets ----------------------------------------------------
d_ehs_wide <- as_tibble(join_all(l_ehs_sets, by=c("aacode")))

if (("weightdwell" %in% colnames(d_ehs_wide)) &
  (!"weighthshld" %in% colnames(d_ehs_wide))) {
  d_ehs_wide$weighthshld <- d_ehs_wide$weightdwell
}

if (("weighthshld" %in% colnames(d_ehs_wide)) &
  (!"weightdwell" %in% colnames(d_ehs_wide))) {
  d_ehs_wide$weightdwell <- d_ehs_wide$weighthshld
}

rm(k_reg, l_ehs_sets)


# (3) Generate survey data (i.e., weighted values) -----------------------------
svy_ehs <- svydesign(id=~aacode, weights=~weightdwell, data=d_ehs_wide)
summary(svy_ehs)


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
message(get_study_name())
path_report <- paste0('export/outReport/', i_ehs_version)
path_datamd <- paste0('export/outForModel/', i_ehs_version)
