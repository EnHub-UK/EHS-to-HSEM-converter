#' -----------------------------------------------------------------------------
#' EHS Converter                                        {Mining household data}
#'
#' This stage explores individual/household values and generates a new list.
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EHS-to-HSEM-converter
#'
#' @notes  + Most of the functions are based on functions developed for the
#'           TUS-parser project.
#'

# [0] Setup Environment --------------------------------------------------------

#.. global environment ----
rm(list = ls(a = TRUE))
source('scripts/setup.R', verbose = FALSE)

#.. project parameters ----
path.profile <- "export/outForProfiles"
path_report <- file.path(path_ehs, path.profile)
path.graph <- "export/outForGraph"
path_ehs.graph <- file.path(path_ehs, path.graph)


# [1] Parse data ---------------------------------------------------------------

var.selection <- 'people'
file.merged.var <- file.path(path_report,paste0("all_merged_",var.selection,".rds"))
file.combined.var <- file.path(path_report, paste0("all_sanitised_", var.selection, ".rds"))


#.. Make list of tables --------------------------------------------------------

d_sets <- data.frame(name = list.files(
  path = paste0(path_ehs,"/data"), pattern = "^UKDA"))
d_sets <- display_datasets(d_sets$name) %>% filter(is_stock_data==TRUE)

lst.EHS.all <- pblapply(d_sets$code, load_dataset, var.selection)
names(lst.EHS.all) <- gsub("-","_",d_sets$code)

#.. remove empty tables
lst.EHS.all <- lst.EHS.all[lengths(lst.EHS.all) > 1]

# use 1 to 5 (eg. d_sets$name[1:5]) to get education category or filter later
#.. include dummy variables (eg. highed1)
lst.EHS.all$UKDA_7802_stata11$highed1 <- "does not apply"

saveRDS(lst.EHS.all, file=file.merged.var)



#.. Process Variables ----------------------------------------------------------

#.. extract variables (columns) and subset datasets
lst.vars <- pblapply(lst.EHS.all, function(x) colnames(x))
lst.EHS.common <- pblapply(lst.EHS.all, function(x)
  tibble(subset(x, select=Reduce(intersect, lst.vars))))

#.. homogenise by variable
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'persno')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'dvhsize')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'hrp')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'dvbenu')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'afam')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'numnonr2')
lst.EHS.common <- lapply(lst.EHS.common, homogenise_variable, 'wholodno')

#.. combine rows
lst.EHS.common <- dplyr::bind_rows(lst.EHS.common, .id = "id_dataset")

#.. restore factors
dtaA <- lst.EHS.common %>%
  mutate(id_dataset = factor(id_dataset),
         age = as.character(age),
         highed1 = factor(tolower(as.character(highed1))),
         noofhrsr = factor(tolower(as.character(noofhrsr))),
         qualchk = factor(tolower(as.character(qualchk))),
         xmarsta = factor(tolower(as.character(xmarsta))),
         dvwork = as.character(dvwork),
         econfull = as.character(econfull),
         persno = as.integer(as.character(persno)),
         dvhsize = as.integer(as.character(dvhsize)),
         hrp = as.integer(as.character(hrp)),
         numnonr2 = as.integer(as.character(numnonr2)),
         wholodno = as.integer(as.character(wholodno))) %>%
  mutate(across(starts_with("r0"), as.character)) %>%
  mutate(across(where(is.character), ~na_if(., "Does not apply"))) %>%
  mutate(across(where(is.character), ~na_if(., "does not apply"))) %>%
  mutate(age=as.integer(gsub("[[:alpha:]]|[[:space:]]", "", age)),
         dvwork = as.factor(dvwork),
         highed1=as.factor(highed1),
         econfull = as.factor(econfull))

#.. correct var -> qualifications
lbl_fix <- levels(dtaA$highed1)
  lbl_fix <- gsub("([[:alnum:]])(/)","\\1 \\2", lbl_fix)
  lbl_fix <- gsub("(/)([[:alnum:]])","\\1 \\2", lbl_fix)
  lbl_fix <- gsub("GCSEeq","GCSE eq", lbl_fix, ignore.case = T)
  lbl_fix <- gsub(" Stand "," standard ", lbl_fix, ignore.case = T)
  lbl_fix <- gsub(" grd"," grade ", lbl_fix, ignore.case = T)
  lbl_fix <- gsub(" equiv "," equivalent ", lbl_fix, ignore.case = T)
  lbl_fix <- gsub(" qual "," qualification ", lbl_fix, ignore.case = T)
  lbl_fix <- gsub(" quals"," qualifications", lbl_fix, ignore.case = T)
  lbl_fix <- gsub(" incl "," incl. ", lbl_fix, ignore.case = T)
  lbl_fix <- gsub("\\s\\s"," ", lbl_fix, ignore.case = T)
levels(dtaA$highed1) <- lbl_fix

#.. correct var -> marital
lbl_fix <- levels(dtaA$xmarsta)
  lbl_fix <- gsub(", ie never married",
    " (never married)", lbl_fix)
  lbl_fix <- gsub("and living with husband/wife",
    "& living with spouse", lbl_fix)
  lbl_fix <- gsub("but separated from husband/wife",
    "& separated from spouse", lbl_fix)
  lbl_fix <- gsub("civil partner- separated",
    "legally-recognised civil partnership separated from partner", lbl_fix)
  lbl_fix <- gsub("ally rec",
    "ally-rec", lbl_fix)
levels(dtaA$xmarsta) <- lbl_fix

lbl_fix <- levels(dtaA$id_dataset)
  lbl_fix <- gsub("\\_", "-", lbl_fix)
levels(dtaA$id_dataset) <- lbl_fix

saveRDS(dtaA, file = file.combined.var)



# [2] Extract details ----------------------------------------------------------

# same interviewees? --> UKDA-6612-stata8 & UKDA-6804-stata11

lst_aacodes <- sort(unique(dtaA$aacode))
l_hhds <- pblapply(lst_aacodes, extract_household_info, dtaA)
names(l_hhds) <- lst_aacodes

#.. export list of households ----

saveRDS(l_hhds, file.path(path_report,"lstHhds.rds"))



#.. simplify info table ----

(dtaHhd <- dplyr::bind_rows(
  lapply(l_hhds, '[[', "Info"), .id = "id") %>%
    mutate(person_id = paste(id, sprintf("%03d", p_in), sep="-")) %>%
    group_by(id) %>%
    mutate(hrp = ifelse(hrp==p_in, TRUE, FALSE)) %>%
    ungroup() %>%
    mutate(aacode = id) %>%
    select(-id,-p_in) %>% relocate(person_id,aacode))

write.csv(dtaHhd, file.path(path_report, "tblHHds_info.csv"))


# .. display relationships ----

(dtaRel <- dplyr::bind_rows(
  lapply(l_hhds, '[[', "Relationship"), .id = "id") %>%
   filter(other!='-') %>% tibble() %>%
   mutate(person_id = paste(id, person, sep="-"),
          other_id = paste(id, other, sep="-"),
          relationship = as.factor(relationship)) %>%
   select(person_id, other_id, relationship) %>%
   rowid_to_column('id'))

write.csv(dtaRel, file.path(path_report, "tblHHds_relationships.csv"))
