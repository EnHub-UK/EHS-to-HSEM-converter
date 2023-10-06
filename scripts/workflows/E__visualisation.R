#' -----------------------------------------------------------------------------
#' EHS Converter                             {Workflow for data visualisation}
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EHS-to-HSEM-converter
#'


# Set-up -----------------------------------------------------------------------

rm(list = ls(a = TRUE))
source("scripts/setup.R", verbose = FALSE)

path.profile <- "export/outForProfiles"
path.graph <- "export/outForGraph"
path_ehs.graph <- file.path(path_ehs, path.graph)

file.merged <- file.path(path_ehs, path.profile, "all_merged.rds")
file.common <- file.path(path_ehs, path.profile, "all_merged_common.rds")
var.selection <- "people"
file.merged.var <- file.path(path_report, paste0("all_merged_", var.selection, ".rds"))
file.combined.var <- file.path(path_report, paste0("all_sanitised_", var.selection, ".rds"))


# Household data -----------------------------------------------------------

lst.EHS.all <- readRDS(file = file.merged.var)
dtaA <- readRDS(file = file.combined.var)


# .. compare by situation and age

ddply(dtaA, .(id_dataset, xmarsta, sex), summarise,
    mean = round(mean(age, na.rm = T), 2),
    sd = round(sd(age, na.rm = T), 2)
)

p1 <- ggplot(dtaA, aes(x = xmarsta, y = age, fill = sex)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~id_dataset, nrow = 1) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Spectral")

export_figure(
    p1,
    "/HHds_Marital", 12, 12
)

# .. compare by education and sex

ddply(dtaA, .(highed1, sex), summarise,
    mean = round(mean(age, na.rm = T), 2),
    sd = round(sd(age, na.rm = T), 2)
)

p2 <- ggplot(dtaA, aes(x = highed1, y = age, fill = sex)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~id_dataset, nrow = 1) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Spectral")
export_figure(
    p2,
    "/HHds_Qual", 12, 12
)

export_table(
    table(dtaA$persno, dtaA$sex),
    "/HHds_HRP"
)


# Predictive (common) data -------------------------------------------

lst.EHS.common <- readRDS(file=file.common)
lst.EHS.all <- readRDS(file=file.merged)
