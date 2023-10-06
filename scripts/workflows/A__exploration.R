#' -----------------------------------------------------------------------------
#' EHS Converter                                         {Descriptive Analysis}
#'
#' This first stage performs an initial descriptive analysis.
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'
#' @notes  + The searching of variables might be optimised by converting
#'           the *.rtf files to *.txt, and by accessing to them in a
#'           new location. The conversion may be performed in `vim`,
#'           requiring only one command.
#'
#'         + The script has not been optimised for stand-alone mode; however,
#'           once configured, it can be run from a CLI to generate summaries.
#'

# Setup ------------------------------------------------------------------------

#.. global environment ----
rm(list = ls(a = TRUE))
source('scripts/setup.R', verbose = FALSE)
source('scripts/modules/charts.R')

#.. project parameters ----
i_ehs_version <- "UKDA-7386-stata11"    # definition/selection of stata folder
source('scripts/modules/load_data.R')       # data collection (~ 10 sec)



# Check location of variables/tables in data sources ---------------------------

lbl_ehs_subsets

locate_variable_in_dataset("aacode")
locate_variable_in_dataset("type")
locate_variable_in_dataset("doors")
locate_variable_in_dataset("FloorArea")
locate_variable_in_dataset("Kitchen")
locate_variable_in_dataset("Orientation")
locate_variable_in_dataset("income")
locate_variable_in_dataset("Conservatories")
locate_variable_in_dataset("Felfenfw")
locate_variable_in_dataset("win")

head(shape)
describe(shape)
head(dwelling)
describe(dwelling)
summary(dwelling$PlanTen)
head(services)
head(dormers)
summary(dormers$type)
head(interior)
summary(interior$fincodor)
describe(elevate)
summary(elevate$felorien)




# [0] Quick Summaries  ---------------------------------------------------------
log. <- list(); ln. <- 0
log.[[inc(ln.)]] <- print(get_study_name())

# [1] Population sub-totals ----------------------------------------------------
log.[[inc(ln.)]] <- paste("Total England dwellings estimated:",
      format(sum(d_ehs_wide$weightdwell, na.rm = T),
             big.mark=",", scientific=F))
log.[[inc(ln.)]] <- paste("Total England households estimated:",
      format(sum(d_ehs_wide$weighthshld, na.rm = T),
             big.mark=",", scientific=F))
log.[[inc(ln.)]] <- paste("Total England population estimated:",
      format(sum(d_ehs_wide$weightdwell * as.integer(as.character(d_ehs_wide$hhsizex)), na.rm = T),
             big.mark=",", scientific=F), "(note the use of `hhsizex`)")
log.[[inc(ln.)]] <- lbl_ehs_subsets



# [2] Cross-tables by archetypes -----------------------------------------------
summary(svy_ehs)
as.data.frame(d_ehs_wide[1:7,])

svytotal(~ gorehs, svy_ehs)
svytotal(~ tenure8x, svy_ehs)
(tbl.summary <- svytable(~ gorehs + tenure8x, svy_ehs))
export_table(tbl.summary,
              '/region_tenure')

svytotal(~ dwtype8x, svy_ehs, na.rm=T, deff=TRUE)
(tbl.summary <- svytable(~ dwtype8x + dwage6x, svy_ehs))
export_table(tbl.summary,
              '/type_age')
export_table(svytable(~ dwtypenx + dwage5x, svy_ehs),
              '/type_age_b')
tbl.summary <- melt(tbl.summary)

export_figure(draw_bars(tbl.summary, 'value', 'dwage6x', 'dwtype8x'),
               "/dwage6x_dwtype8x", 8, 12)
export_figure(draw_bars(tbl.summary, 'value', 'dwtype8x',
                        'dwage6x', '', 'fill'),
               "/dwage6x_dwtype8x_100", 8, 12)

tbl.summary <- detailed__dimensions_plus
colnames(tbl.summary) <- tolower(colnames(tbl.summary))
tbl.summary <- join(d_ehs_wide, tbl.summary, by=c("aacode"))
tbl.summary <- ddply(tbl.summary, .(dwtypenx, dwage5x),
                   summarise,
                   acc = sum(weightdwell, na.rm = TRUE),
                   TFA = mean(floorarea, na.rm = TRUE))

export_figure(draw_bubble(tbl.summary,'acc','dwage5x','TFA','dwtypenx'),
               "/dwage5x_tfa_dwtypenx", 16, 6)
export_figure(draw_histogram(tbl.summary, 'TFA', 'dwtypenx', 'dwage5x',
                             bands=15, weight='acc'),
               "/dwage5x_tfa_dwtypenx_hist", 16, 6)


#.. for snapshot comparison
export_table(svytable(~ epceeb09e + dwtypenx, svy_ehs), "/snap_epceeb09e_dwtypenx")
export_table(svytable(~ epceeb09e + dwage9x, svy_ehs), "/snap_epceeb09e_dwage9x")
export_table(svytable(~ epceeb09e + dwtypenx + dwage9x, svy_ehs), "/snap_epceeb09e_dwtypenx_dwage9x")
export_table(svytable(~ tenure2x + region3x, svy_ehs), "/snap_tenure2x_region3x")
export_table(svytable(~ tenure8x + gorehs, svy_ehs), "/snap_tenure8x_gorehs")
export_table(svytable(~ rumorph + dwtypenx, svy_ehs), "/snap_rumorph_dwtypenx")
export_table(svytable(~ dwage9x + dwtypenx, svy_ehs), "/snap_dwage9x_dwtypenx")
export_table(svytable(~ typewstr2 + dwtypenx, svy_ehs), "/snap_typewstr2_dwtypenx")
export_table(svytable(~ arnatx + dwtypenx, svy_ehs), "/snap_arnatx_dwtypenx")
export_table(svytable(~ dblglaz2 + dwtypenx, svy_ehs), "/snap_dblglaz2_dwtypenx")
export_table(svytable(~ heat7x + dwtypenx, svy_ehs), "/snap_heat7x_dwtypenx")
export_table(svytable(~ typerstr + dwtypenx, svy_ehs), "/snap_typerstr_dwtypenx")
export_table(svytable(~ loftins6 + dwtypenx, svy_ehs), "/snap_loftins6_dwtypenx")
export_table(svytable(~ wallinsx + dwtypenx, svy_ehs), "/snap_wallinsx_dwtypenx")
export_table(svytable(~ constx + dwtypenx, svy_ehs), "/snap_constx_dwtypenx")
export_table(svytable(~ typerstr + loftins6 + dwtypenx, svy_ehs), "/snap_typerstr_loftins6_dwtypenx")
export_table(svytable(~ wallinsx + loftins6 + dwtypenx, svy_ehs), "/snap_wallinsx_loftins6_dwtypenx")
export_table(svytable(~ wallinsx + constx + dwtypenx, svy_ehs), "/snap_wallinsx_constx_dwtypenx")
export_table(svytable(~ dwage9x + heat7x + dwtypenx, svy_ehs), "/snap_dwage9x_heat7x_dwtypenx")
export_table(svytable(~ mainfuel + heat7x, svy_ehs), "/snap_mainfuel_heat7x")
export_table(svytable(~ mainfuel + heat7x + dwtypenx, svy_ehs), "/snap_mainfuel_heat7x_dwtypenx")
export_table(svytable(~ mainfuel + heat7x + dwage9x, svy_ehs), "/snap_mainfuel_heat7x_dwage9x")



# [3] Socio-demographics -------------------------------------------------------
locate_variable_in_dataset("Sex")

tbl.summary <- ddply(people, .(aacode, Sex), summarise, Age = mean(AGE))
tbl.summary$bands <- cut(tbl.summary$Age, 20, labels = seq(0,99,100/20))
tbl.summary$count <- 1
tbl.summary <- aggregate(count ~ Sex + bands, data = tbl.summary, length)
tbl.summary <- as.data.frame(dcast(tbl.summary, bands ~ Sex))

export_figure(draw_pyramid(tbl.summary, 'bands', 'male', 'female'),
               "/gender_age", 9, 9)



# [4] Dwelling characteristics -------------------------------------------------

tbl.dwell <- subset(d_ehs_wide,
         select=c(aacode, weightdwell, weighthshld, dwtype8x, dwage9x, storeyx))
tbl.dormers <- subset(dormers, Fexdb1pr=='Yes' | Fexdb2pr=='Yes',
         select=c(aacode, type, Fexdb1pr, Fexdb1no, Fexdb1ag,
                  Fexdb2pr, Fexdb2no, Fexdb2ag))
tbl.elevate <- subset(elevate,
         select=c(aacode, felorien, felroofp, Felexpff,
                  Felexplf, Felexprf, Felexpbf))
tbl.location <- subset(general_plus,
         select=c(aacode, GorEHS, Imd1010))
tbl.summary <- join_all(by='aacode',
                        list(tbl.dormers,tbl.dwell,tbl.elevate,tbl.location))

tbl.summary$Fexdb1no[tbl.summary$Fexdb1no==88] <- 0
tbl.summary$Fexdb2no[tbl.summary$Fexdb2no==88] <- 0
tbl.summary$Fexdb1ag[tbl.summary$Fexdb1ag==88] <- NA
tbl.summary$Fexdb2ag[tbl.summary$Fexdb2ag==88] <- NA
tbl.summary$facade <-
  ifelse(tbl.summary$Fexdb1pr=='Yes',
         ifelse(tbl.summary$Fexdb2pr=='Yes',"Back & Front","Front only"),
         ifelse(tbl.summary$Fexdb2pr=='Yes',"Back only","None"))
tbl.summary$no.elements <-
  (tbl.summary$Fexdb1no+tbl.summary$Fexdb2no)/tbl.summary$storeyx

tbl.extensions <- ddply(tbl.summary, .(type, dwtype8x, facade), summarise,
      dwellings = sum(weightdwell))

export_table(tbl.extensions,
              "/extensions")
export_figure(draw_bars(tbl.summary, 'weightdwell', 'dwtype8x', 'type', 'facade'),
               "/type_dwtype8x_facade", 12, 6)
export_figure(draw_notchs(tbl.summary, 'weightdwell', 'type', 'no.elements', 'facade', 'felorien','dwtype8x', FALSE),
               "/type_facade_extensions", 16, 16)
export_figure(draw_notchs(tbl.summary, 'weightdwell', 'type', 'no.elements', 'facade', 'felorien','dwtype8x', TRUE),
               "/type_facade_extensions_log", 16, 16)

tbl.conservatories <-
  ddply(subset(tbl.summary, type == "Conservatories" & felorien != "Unknown"),
        .(dwtype8x, dwage9x, felorien, facade), summarise, tot = sum(weightdwell))
export_figure(draw_polar(tbl.conservatories, 'tot', 'felorien', 'dwtype8x', 'facade'),
               "/felorien_dwtype8x", 9, 4)

tbl.facade <-
  ddply(subset(tbl.summary, felorien != "Unknown" & felorien != "Question Not Applicable" & !is.na(felorien)),
        .(dwtype8x, dwage9x, felorien, facade, type),
        summarise, tot = sum(weightdwell))
export_figure(draw_polar(tbl.facade, 'tot', 'felorien', 'dwtype8x', 'dwage9x', 'type'),
               "/type_orientation_dwage9x_facade", 19, 24)

tbl.region <-
  ddply(subset(tbl.summary, (felorien != "Unknown" & felorien != "Question Not Applicable" & !is.na(felorien)) &
                 GorEHS == "Yorkshire and the Humber"), # narrow to lSOA, mSOA
        .(dwtype8x, dwage9x, felorien, facade, type),
        summarise, tot = sum(weightdwell))
export_figure(draw_polar(tbl.region, 'tot', 'felorien', 'dwtype8x', 'dwage9x', 'facade'),
               "/type_orientation_subset_facade", 36, 14)
export_figure(draw_polar(tbl.region, 'tot', 'felorien', 'dwtype8x', 'type', 'facade'),
               "/type_orientation_subset_facade_type", 36, 14)



# [5] Conservatories -----------------------------------------------------------

tbl.summary <- subset(d_ehs_wide,
  select=c(aacode,weightdwell,weighthshld,dwtype8x,dwage9x,storeyx))
tbl.dormers <- subset(dormers, Fexdb1pr=='Yes' | Fexdb2pr=='Yes',
  select=c(aacode,type,Fexdb1pr,Fexdb1no,Fexdb1ag,Fexdb2pr,Fexdb2no,Fexdb2ag))
tbl.elevate <- subset(elevate,
  select=c(aacode, felorien, felroofp, Felexpff,
          Felexplf, Felexprf, Felexpbf, orientation))
tbl.location <- subset(general_plus,
  select=c(aacode, GorEHS, Imd1010))
tbl.interior <- subset(interior,
  select=c(aacode, fincodor, fincosiz, fincowin, fincorof, fincorad))
tbl.summary <- join_all(by='aacode', list(
  tbl.dormers, tbl.summary, tbl.elevate,
  tbl.location, tbl.interior))
tbl.summary <-
  subset(tbl.summary, type == "Conservatories" &
          (!is.na(orientation) | !is.nan(orientation)))

tbl.window <-
  ddply(tbl.summary,
        .(fincodor, fincosiz, fincowin, fincorof, fincorad, orientation),
        summarise, tot = sum(weightdwell))
dcast(tbl.window, fincodor ~ fincorad, value.var = "tot")
dcast(tbl.window, fincodor ~ fincorad, mean, value.var = "tot")
dcast(tbl.window, fincodor ~ fincorad, sum, value.var = "tot")
dcast(tbl.window, fincodor ~ orientation, sum, value.var = "tot")
dcast(tbl.window, fincorad ~ orientation, sum, value.var = "tot")
tbl.EHS.window <- dcast(tbl.window, orientation ~ fincodor + fincorad,
                       sum, value.var = "tot")
export_table(tbl.EHS.window,
              "/fincorad_fincorad")

tbl.fincorad <-
  ddply(tbl.summary,
      .(dwtype8x, dwage9x, fincorad, GorEHS),
      summarise, tot = sum(weightdwell))
export_figure(draw_polar(tbl.fincorad, 'tot', 'fincorad',
                         'dwtype8x', 'GorEHS', 'dwage9x'),
               "/fincorad", 12, 6)





# [6] Indoor characteristics ---------------------------------------------------
tbl.indoor <- subset(d_ehs_wide, select=c(aacode,weightdwell,weighthshld,
                                  dwtype8x,dwage9x,storeyx))
tbl.summary <- subset(interior,
                          select=c(aacode,Finlivex,Finlivfu,
                                   Finkitex,Finkitfu,Finbedex,Finbedfu))
tbl.summary <- join(tbl.summary, tbl.indoor, by='aacode')

tbl.summary$storeyx <-
  factor(ifelse(tbl.summary$storeyx>=10, 10, tbl.summary$storeyx),
         levels = 1:10, labels = c(1:9,"10+"))
tbl.summary <-
  as_tibble(ddply(tbl.summary,
        .(Finlivex, Finlivfu, Finkitex, Finkitfu, Finbedex, Finbedfu,
          dwtype8x, dwage9x, storeyx),
        summarise,
        acc = sum(weightdwell, na.rm = TRUE)))
export_table(tbl.summary,
              "/internal_elements")



# [7] Fuel Expenditure ---------------------------------------------------------

tbl.hhd <- subset(d_ehs_wide,
                  select=c(aacode,weightdwell,weighthshld,dwtype8x,
                           dwage9x,tenure8x,gorehs,heat7x,hhtype11))
tbl.energy <- subset(detailed__energy_performance_plus,
                     select=c(aacode, EPcalc09e, EPcuse09e, EPcco209e))
tbl.dimension <- subset(detailed__dimensions_plus,
                          select=c(aacode, FloorArea, nflorm, nflora, efwalar, ebwalar))
tbl.summary <- join_all(by='aacode', list(tbl.energy,tbl.hhd,tbl.dimension))

tbl.survey <-
  svydesign(id=~aacode, weights=~weightdwell, data=tbl.summary)
summary(tbl.survey)

var.typologies <- c('dwtype8x', 'dwage9x', 'tenure8x', 'gorehs',
                    'heat7x', 'hhtype11', 'FloorArea', 'nflorm')
tbl.typologies <- subset(tbl.summary, dwtype8x=="small terraced house" &
                           dwage9x=="1965 to 1974" &
                           hhtype11=="couple with no child(ren)",
                         select=var.typologies)
fml.typologies <- as.formula(paste("EPcuse09e",
                              paste(var.typologies, collapse=" + "), sep="~"))
lm.typologies <- svyglm(fml.typologies, tbl.survey)

tbl.typologies.outputs <- predict(lm.typologies, tbl.typologies)
cbind(tbl.typologies, tbl.typologies.outputs)

tbl.typologies.mean.typ <-
  svyby(~EPcuse09e+FloorArea, ~dwtype8x, svymean, design=tbl.survey)
tbl.typologies.mean.age <-
  svyby(~EPcuse09e+FloorArea, ~dwage9x, svymean, design=tbl.survey)
rownames(tbl.typologies.mean.typ) <- rownames(tbl.typologies.mean.age) <- NULL
log.[[inc(ln.)]] <- svymean(~EPcuse09e+FloorArea, tbl.survey)
log.[[inc(ln.)]] <- tbl.typologies.mean.typ
log.[[inc(ln.)]] <- tbl.typologies.mean.age

tbl.typologies.indices.avg <-
  svyby(~EPcuse09e+FloorArea, ~dwtype8x, svymean,design=tbl.survey)
tbl.typologies.indices.tot <-
  svyby(~EPcuse09e+FloorArea, ~dwtype8x, svytotal, design=tbl.survey)
tbl.typologies.indices <-
  svyby(~EPcuse09e+FloorArea, ~dwtype8x + dwage9x, svymean, design=tbl.survey)
rownames(tbl.typologies.indices) <- NULL

export_table(tbl.typologies.indices,
              "/svytfa")
export_figure(draw_scatter(tbl.typologies.indices, 'FloorArea','EPcuse09e','dwtype8x','dwage9x', F),
               "/svyscatter", 12, 12)



# [8] Fuel Poverty -------------------------------------------------------------

locate_variable_in_dataset("fuel")
locate_variable_in_dataset("ALLincx")
locate_variable_in_dataset("basic")
locate_variable_in_dataset("unoc")

if(!is.null(r <- get0('fuel_poverty_dataset_2011_for_ukda',
                       envir = .GlobalEnv))) {
  tbl.fp <-
    join(d_ehs_wide, fuel_poverty_dataset_2011_for_ukda, by=c("aacode"))
  colnames(tbl.fp) <- tolower(colnames(tbl.fp))
  tbl.fp <-
    subset(tbl.fp, (!is.na(fpbasinc) & !is.na(fuelexpn) & fpbasinc > 0 &
                          !is.na(allincx) & fuelexpn > 0 & !is.na(unoc) &
                          unoc == "Not under occupying"),
               select = c(aacode, weightdwell, weighthshld,
                          fpbasinc, allincx, fuelexpn, sap09))
  export_figure(draw_fuel_poverty(tbl.fp),
                 "/fuelpoverty", 12, 6)

  tbl.fp.ind <- tbl.fp
  tbl.fp.ind$.fuelPov <- tbl.fp.ind$fuelexpn / tbl.fp.ind$allincx
  tbl.fp.ind$.fuelPov <- ifelse(tbl.fp.ind$.fuelPov <= 0.1, F, T)
  tbl.fp.ind <-
    ddply(tbl.fp.ind, .(aacode, .fuelPov), summarise, sap = mean(sap09))
  tbl.fp.ind$bands <- cut(tbl.fp.ind$sap, 20, labels = seq(0,99,100/20))
  tbl.fp.ind$count <- 1
  tbl.fp.ind <- aggregate(count ~ .fuelPov + bands, data = tbl.fp.ind, length)
  tbl.fp.ind <- as.data.frame(dcast(tbl.fp.ind, bands ~ .fuelPov))
  colnames(tbl.fp.ind) <- c('sap','not_in_FP','in_FP')
  tbl.fp.ind$bands <- as.integer(as.character(tbl.fp.ind$sap))
  tbl.fp.ind[is.na(tbl.fp.ind)] <- 0
  export_figure(draw_pyramid(tbl.fp.ind, 'sap', 'not_in_FP', 'in_FP'),
                 "/fuelpoverty_sap", 9, 9)

}else{
  warning("This function was written exclusively for 2011 version.")
}




# [9] Glazing ---------------------------------------------------

tbl.wide <- d_ehs_wide %>% select(aacode, weightdwell, weighthshld, dwtype8x, dwage9x, storeyx)
tbl.wins <- windows %>% select(aacode, type, ends_with("no"))
tbl.dims <- dimensions %>% select(aacode, FloorArea, Intwalar, efwalar, ebwalar, efwinar, ebwinar, nflorm, nflora)

tbl.summary <- join_all(by='aacode', list(
  tbl.wide, tbl.wins, tbl.dims)) %>% tibble() %>%
  mutate(ext_area_fb = efwalar + ebwalar + efwinar + ebwinar,
         win_area_fb = efwinar + ebwinar) %>%
  mutate(win_ratio_front = efwinar / (efwalar + efwinar),
         win_ratio_backs = ebwinar / (ebwalar + ebwinar),
         win_ratio_fb = win_area_fb / ext_area_fb)


tbl.summary.w <- tbl.summary %>%
  select(aacode, weightdwell, win_ratio_fb, win_ratio_backs, win_ratio_front) %>%
  unique()

fivenum(tbl.summary$win_ratio_fb)

tbl.dims
tbl.dims %>% arrange(efwinar)
tbl.dims %>% arrange(desc(efwinar))
tbl.summary %>% arrange(desc(win_ratio_fb)) %>% select(ext_area_fb, win_area_fb)

df <- melt(tbl.summary.w, id=c("aacode", "weightdwell"))
ggplot(df, aes(value)) +
  geom_histogram(aes(weight = weightdwell, fill=variable), binwidth = 0.1) +
  facet_wrap(. ~ variable, ncol = 1) +
  theme_light()

tbl.summary.w <-
  svydesign(id=~aacode, weights=~weightdwell, data=tbl.summary.w)

svymean(~ win_ratio_front, tbl.summary.w, na.rm = T)
svymean(~ win_ratio_backs, tbl.summary.w, na.rm = T)
svymean(~ win_ratio_fb, tbl.summary.w, na.rm = T)



# [10] Aditional charts --------------------------------------------------------
summary(svy_ehs)

export_figure(draw_mosaic(svy_ehs, 'nbedsx','tenure4x','dwtype3x'),
               "/Mosaic_occupied", 12, 12)
export_figure(draw_mosaic(svy_ehs, 'hhcompx','tenure8x'),
               "/Mosaic_hhcomposition", 12, 12)
export_figure(draw_mosaic(svy_ehs, 'heat7x', 'dwtype8x', 'dwage9x'),
               "/Mosaic_heat", 12, 12)
export_figure(draw_mosaic(svy_ehs, 'gorehs', 'dwtype8x', 'dwage9x'),
               "/Mosaic", 12, 12)
export_figure(draw_mosaic(svy_ehs, 'gorehs', 'dwtype8x', var.style="dens"),
               "/Mosaic_dens", 12, 12)
export_figure(draw_four_fold(svy_ehs, 'tenure2x', 'agehrp2x'),
               "/FourFold", 6, 6)

capture.output(log., file=paste0(path_report,"/_general.txt"))



cat("\014")
display_datasets(i_ehs_version)

# End --------------------------------------------------------------------------
