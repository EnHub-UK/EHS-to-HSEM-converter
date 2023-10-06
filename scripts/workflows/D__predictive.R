#' -----------------------------------------------------------------------------
#' EHS Converter                             {Workflow for predictive analysis}
#'
#' This testing file combines multiple EHS version for later developing
#' a predictor of archetype circumstances.
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EHS-to-HSEM-converter
#'
#' @notes  + The rationale for the selection of variables is further
#'           explained in both TUS-parser and EnHub projects.
#'


# Set-up -----------------------------------------------------------------------

rm(list = ls(a = TRUE))
source('scripts/setup.R', verbose = FALSE)

path.profile <- "export/outForProfiles"
path.graph <- "export/outForGraph"
path_ehs.graph <- file.path(path_ehs, path.graph)
file.merged <- file.path(path_ehs, path.profile, "all_merged.rds")
file.common <- file.path(path_ehs, path.profile, "all_merged_common.rds")



# Generate `all_merged.rds` ----------------------------------------------------

d_sets <- data.frame(name = list.files(
  path = paste0(path_ehs,"/data"), pattern = "^UKDA"))
d_sets <- display_datasets(d_sets$name) %>% filter(is_stock_data==TRUE)

lst.EHS.all <- pblapply(d_sets$code, load_dataset)
names(lst.EHS.all) <- gsub("-", "_", d_sets$code)

saveRDS(lst.EHS.all, file=file.merged)



# Generate `all_merged_common.rds` ---------------------------------------------

lst.EHS.all$UKDA_8494_stata$gorehs <- '-'
lst.EHS.all$UKDA_8670_stata$gorehs <- '-'

# .. extract variables (columns) and subset datasets
lst.vars <- pblapply(lst.EHS.all, function(x) colnames(x))
lst.EHS.common <- pblapply(lst.EHS.all, function(x)
  tibble(subset(x, select=Reduce(intersect, lst.vars))))

# .. homogenise specific set/variable
levels(lst.EHS.common$UKDA_6612_stata11$lvnumx) <- 0:3
lst.EHS.common$UKDA_6612_stata11$lvnumx <-
  as.integer(as.character(lst.EHS.common$UKDA_6612_stata11$lvnumx))

# .. homogenise by variable
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'hhincx')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'ahcinceq')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'bhcinceq')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'floorx')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'cstactux')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'cstactbx')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'cstactcx')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'cststdux')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'cststdbx')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'cststdcx')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'sap12')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'weighthshld')

# .. combine rows
lst.EHS.common <- dplyr::bind_rows(lst.EHS.common, .id = "id_dataset")

# .. restore factors
lst.EHS.common$dwage6x <-
  factor(gsub(" to 19","-",as.character(lst.EHS.common$dwage6x)),
         levels = c('pre 1919','1919-44','1945-64',
                    '1965-80','1981-90','post 1990'), ordered = TRUE)
lst.EHS.common$watersys <-
  factor(tolower(as.character(lst.EHS.common$watersys)))
lst.EHS.common$attic <-
  ifelse(as.character(lst.EHS.common$attic) %in% c("y","yes"), TRUE, FALSE)
lst.EHS.common$lvanyx <-
  ifelse(as.character(lst.EHS.common$lvanyx) %in% c("y","yes"), TRUE, FALSE)
lst.EHS.common$alltypex <-
  factor(tolower(gsub('\\&','and', as.character(lst.EHS.common$alltypex))))

lst.EHS.common <- lst.EHS.common %>%
  mutate(tenex = as.character(tenex)) %>%
  mutate(tenex = gsub("HA$", "housing association", tenex)) %>%
  mutate(tenex = gsub("LA$", "local authority", tenex)) %>%
  mutate(tenure6x = as.factor(tenex)) %>%
  mutate(tenex = gsub("RSL$", "housing association", tenex)) %>%
  mutate(tenex = gsub("\\swith\\smortgage|\\soutright", "", tenex)) %>%
  mutate(tenure4x = as.factor(tenex)) %>%
  mutate(tenure2x = ifelse(grepl("rent", tenex), "renter", "owner")) %>%
  mutate(tenure2x = as.factor(tenure2x))

lst.EHS.common <- lst.EHS.common %>%
  mutate(gorehs = tolower(as.character(gorehs))) %>%
  mutate(gorehs = gsub("\\s+", " ", gorehs)) %>%
  mutate(gorehs = gsub("^-", NA, gorehs)) %>%
  mutate(gorehs = as.factor(gorehs))

saveRDS(lst.EHS.common,  file=file.common)
