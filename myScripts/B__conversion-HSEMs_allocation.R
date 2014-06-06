#' -----------------------------------------------------------------------------
#' EHS Converter                                             {Table Converter}
#'
#' @file `B__conversion-HSEMs_allocation.R` converts EHS tables
#'       following the method described in:
#'         > Hughes, M., Armitage, P., Palmer, J., & Stone, A. (2012).
#'           Converting English Housing Survey Data for Use in Energy Models.
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'
#' @notes
#'   + In the original methodology, when **apartments** are adapted for the
#'     CHM format, they lose their relative position in the building
#'     (ie. top floor, ground floor). Here the values are maintained,
#'     as in the EHS, and then their positions are later exported.
#'
#'   + Heat pumps' CoPs are expressed as percentages (>100%)
#'
#'   + Some of the coded DHW systems do not correspond to the values
#'     published for CHM; see the following examples:
#'      I0304210 / coded as Electric in the exported dataset, however,
#'        although it has indeed an electric space heating system, the
#'        survey specifies a separate instantaneous heater supplied with gas
#'      I0193407 / coded as Gas in the exported dataset, however the survey
#'        indicates solar panels supplying the DHW.
#'      I0064104 / coded as Oil in the exported dataset, however,
#'        although its central system is supplied with oil, the boiler
#'        uses wood and might be better represented as with biomass boiler
#'


# Setup / Environment ----------------------------------------------------------

lblHSEMs <- c('dwelling.and.household.information', 'geometry',
              'ventilation','other.heat.loss.elements', 'space.heating',
              'hot.water.system', 'low.energy.lighting')


# + Auxiliary Tables  ----------------------------------------------------------

tbl.AuxA <- fnReadAux("tbl_CHM_EHS_ageBand")
tbl.AuxB <- fnReadAux("tbl_SAPs16")
tbl.AuxC <- fnReadAux("tbl_CHM_SAPs3", row.lbl=TRUE)
tbl.AuxD <- fnReadAux("tbl_SAPs3", row.lbl=TRUE)
tbl.AuxE <- fnReadAux("tbl_EHCS_heating")
tbl.AuxF <- fnReadAux("tbl_HSEM_heating")
tbl.AuxG <- fnReadAux("tbl_SAPs4b")


# A. [dwelling.and.household.information]---------------------------------------
dtaAa <- subset(firstimp_physical, select=c(aacode, fodconst, fodconac))
dtaBa <- subset(general_plus, select=c(aacode, aagpd1011, gorEHCS, tenure8x))
dtaCa <- subset(physical_plus, select=c(aacode, dwtype7x))
dtaDa <- subset(interview_plus, select=c(aacode, hhsizex, NDEPCHILD))

dtaModA <- subset(dtaBa, select=c(aacode, aagpd1011, gorEHCS))
colnames(dtaModA) <-
  c('V001_HousingCode', 'V002_DwellingWeight', 'D003_Region')
dtaModA$D003_Region <- droplevels(dtaModA)$D003_Region

dtaModB <- join(dtaAa, dtaBa, by='aacode')
dtaModB <- join(dtaModB, dtaCa, by='aacode')
dtaModB$tenuretype <- "private owner occupied"
dtaModB$tenuretype[grep("private rented*",dtaModB$tenure8x)] <- "private rented"
dtaModB$tenuretype[grep("RSL*",dtaModB$tenure8x)] <- "housing association"
dtaModB$tenuretype[grep("local authority*",dtaModB$tenure8x)] <- "local authority"
dtaModB$tenuretype <- factor(dtaModB$tenuretype)
dtaModB$tenuretype <- factor(dtaModB$tenuretype,levels(dtaModB$tenuretype)[c(3,2,1,4)])
dtaModB$dwellingtype <- factor(dtaModB$dwtype7x,levels(dtaModB$dwtype7x)[c(4,3,2,1,5,6,7)])
dtaModB$ehs.no <- as.integer(dtaModB$fodconst)
dtaModB$sap.no <- fnGetAgeBands(dtaModB$fodconac)
dtaModB <- subset(dtaModB, select=c(aacode, tenuretype, dwellingtype, sap.no, ehs.no))
colnames(dtaModB) <-
  c('V001_HousingCode', 'D004_TenureType', 'D002_DwellingType',
  'D001_DwellingAge', 'V003_ConstructionDate')

dtaModC <- dtaDa
dtaModC$hhsizex <- dtaModC$hhsizex - dtaModC$NDEPCHILD
colnames(dtaModC) <-
  c('V001_HousingCode', 'D007_OccupantsAdult', 'D006_OccupantsChildren')

lst.A <- as_tibble(
  join_all(lapply(ls()[grep("dtaMod\\w",ls())], get), by='V001_HousingCode'))
rm(list=ls()[grep("dta\\w+",ls())])



# B. [geometry]-----------------------------------------------------------------
dtaAb <- subset(dimensions, select=c(aacode, FloorArea))
dtaBb <- subset(physical_plus, select=c(aacode, storeyx))
dtaBb <- dtaBb %>% na_if(-8)
dtaCb <- subset(flatdets, select=c(aacode, Fdffloor))
dtaCb <- dtaCb %>% na_if(77) %>% na_if(8) %>% na_if(9)
dtaDb <- subset(interior, select=c(aacode, Finlivcl, Finkitcl,  Finbedcl, Finbatcl))
dtaDb <- dtaDb %>% na_if(88.8) %>% na_if(99.9)
dtaEb <- subset(physical_plus, select=c(aacode, attic))
dtaFb <- subset(shape,  select=c(aacode, Fdhmlev1, Fdhmwid1, Fdhmdep1, Fdhalev1,  Fdhawid1, Fdhadep1, Fdhmlev2, Fdhmlev3, Fdhmwid2,  Fdhmdep2, Fdhmwid3, Fdhmdep3, Fdhalev2, Fdhawid2,  Fdhadep2, Fdhalev3, Fdhawid3, Fdhadep3))
dtaFb <- dtaFb %>% na_if(88.8) %>% na_if(99.9) %>% na_if(88) %>% na_if(99)
dtaGb <- subset(flatdets, select=c(aacode, Fdfsamed, Fdfmainl,  Fdfnextl, Fdfmainw, Fdfmaind,  Fdfnextw, Fdfnextd, Fdffloor))
dtaGb <- dtaGb %>% na_if(77) %>% na_if(8) %>% na_if(9) %>% na_if(88) %>%  na_if(99) %>% na_if(88.8) %>% na_if(99.9) %>% na_if(77.7)
dtaHb <- subset(services, select=c(aacode, Finlopos))
dtaIb <- subset(interior, select=c(aacode, Finrooms, Finlivwi, Finlivde))
dtaIb <- dtaIb %>% na_if(88.8) %>% na_if(99.9)
dtaJb <- subset(elevate, select=c(aacode, Felcavff, Felcavlf, Felcavrf, Felcavbf, Felextff, Felextlf, Felextrf, Felextbf))
dtaKb <- subset(physical_plus, select=c(aacode, typewstr2))
dtaLb <- subset(firstimp_physical, select=c(aacode, fodconst, fodconac))
dtaMb <- subset(general_plus, select=c(aacode, gorEHCS))
dtaNb <- subset(elevate, select=c(aacode, Fvwtenff, Fvwtenbf, Fvwtenlf, Fvwtenrf))
dtaNb <- dtaNb %>% na_if(77) %>% na_if(8) %>% na_if(9) %>% na_if(88) %>%  na_if(99)
dtaOb <- subset(shape, select=c(aacode, Fshaddit))
dtaPb <- subset(doors, select=c(aacode, type, Fexdf1no, Fexdf2no))
dtaQb <- subset(flatdets, select=c(aacode, Fdffrooa, Fdflftoa, Fdfrigoa, Fdfbckoa, Fdffroia, Fdfbckia, Fdflftia, Fdfrigia, Fdffroof, Fdfbckof, Fdflftof, Fdfrigof))
dtaQb <- dtaQb %>% na_if(77) %>% na_if(8) %>% na_if(9)
dtaRb <- subset(elevate, select=c(aacode, Felfenfw, Felfenbw, Felfenlw, Felfenrw))
dtaRb <- dtaRb %>% na_if(77) %>% na_if(88) %>% na_if(9)
dtaSb <- subset(physical_plus, select=c(aacode, dblglaz2, dblglaz4))

dtaModA <- dtaDb
dtaModA$AvHeight <- rowMeans(subset(dtaModA, select=c(-aacode)), na.rm = T)
dtaModA <- join(dtaModA, dtaCb, by='aacode')
dtaModA <- join(dtaModA, dtaBb, by='aacode')
dtaModA <- join(dtaModA, dtaEb, by='aacode')
dtaModA <- join(dtaModA, subset(dtaFb,select=c(aacode,Fdhmlev1)), by='aacode')
dtaModA$BsHeight <- ifelse(dtaModA$Fdhmlev1=='BB', dtaModA$AvHeight, NA)
dtaModA$GfHeight <- ifelse(dtaModA$Fdhmlev1=='BB',  dtaModA$AvHeight + 0.25, dtaModA$AvHeight)
dtaModA$FfHeight <- ifelse(dtaModA$Fdhmlev1=='GG' |  dtaModA$Fdhmlev1=='BB',  dtaModA$AvHeight + 0.25, dtaModA$AvHeight)
dtaModA$FfHeight <- ifelse(dtaModA$storeyx>1, dtaModA$FfHeight, NA)
dtaModA$SfHeight <- ifelse(dtaModA$Fdhmlev1=='01' |  dtaModA$Fdhmlev1=='GG' |  dtaModA$Fdhmlev1=='BB',  dtaModA$AvHeight + 0.25, dtaModA$AvHeight)
dtaModA$SfHeight <- ifelse(dtaModA$storeyx>2, dtaModA$SfHeight, NA)
dtaModA$TfHeight <- ifelse(dtaModA$Fdhmlev1=='02' |  dtaModA$Fdhmlev1=='01' |  dtaModA$Fdhmlev1=='GG' |  dtaModA$Fdhmlev1=='BB',  dtaModA$AvHeight + 0.25, dtaModA$AvHeight)
dtaModA$TfHeight <- ifelse(dtaModA$storeyx>3, dtaModA$TfHeight, NA)
dtaModA$AtHeight <- ifelse(dtaModA$attic=="yes", 2.45, NA)
tbl.Aux.Dim <- dtaModA
dtaModA <-
  dtaModA[,c('aacode', 'BsHeight', 'GfHeight', 'FfHeight',
             'SfHeight', 'TfHeight', 'AtHeight')]
colnames(dtaModA) <-
  c('V001_HousingCode', 'D041_BasementHeight', 'D042_GroundFloorHeight',
    'D043_1stFloorHeight',  'D044_2ndFloorHeight', 'D045_3rdFloorHeight',
    'D046_RoomInRoofHeight')

dtaModB <- join(dtaFb, dtaAb, by='aacode')
dtaModB <- join(dtaModB, dtaBb, by='aacode')
dtaModB <- join(dtaModB, dtaGb, by='aacode')
dtaModB <- join(dtaModB, dtaEb, by='aacode')
dtaModB <- join(dtaModB, dtaHb, by='aacode')
dtaModB$WDm1 <- dtaModB$Fdhmwid1 * dtaModB$Fdhmdep1
dtaModB$WDm2 <- dtaModB$Fdhmwid2 * dtaModB$Fdhmdep2
dtaModB$WDm3 <- dtaModB$Fdhmwid3 * dtaModB$Fdhmdep3
dtaModB$WDa1 <- dtaModB$Fdhawid1 * dtaModB$Fdhadep1
dtaModB$WDa2 <- dtaModB$Fdhawid2 * dtaModB$Fdhadep2
dtaModB$WDa3 <- dtaModB$Fdhawid3 * dtaModB$Fdhadep3
dtaModB$WDf1 <- dtaModB$Fdfmainw * dtaModB$Fdfmaind
dtaModB$WDf2 <- dtaModB$Fdfnextw * dtaModB$Fdfnextd
dtaModB$WDat <-  ifelse(dtaModB$attic=="yes", ifelse(!is.na(dtaModB$WDm3),  dtaModB$WDm3 / 2,  ifelse(!is.na(dtaModB$WDm2),  dtaModB$WDm2 / 2,  dtaModB$WDm1 / 2)), NA)
dtaModB$WDraw <-  ifelse(is.na(dtaModB$Fdffloor) | (!is.na(dtaModB$Fdffloor) & (is.na(dtaModB$Fdfmainw) | is.na(dtaModB$Fdfmaind))),  rowSums(subset(dtaModB, select=c(WDm1,WDm2,WDm3,WDa1,WDa2,WDa3,WDat)), na.rm = T),  rowSums(subset(dtaModB, select=c(WDf1,WDf2)), na.rm = T))
dtaModB$LSF <- sqrt(dtaModB$FloorArea / dtaModB$WDraw)
dtaModB$WD_1 <- dtaModB$Fdhmwid1 * dtaModB$Fdhmdep1 * dtaModB$LSF^2
dtaModB$WD_1x <- dtaModB$Fdhawid1 * dtaModB$Fdhadep1 * dtaModB$LSF^2
dtaModB$WD_2 <- dtaModB$Fdhmwid2 * dtaModB$Fdhmdep2 * dtaModB$LSF^2
dtaModB$WD_2x <- dtaModB$Fdhawid2 * dtaModB$Fdhadep2 * dtaModB$LSF^2
dtaModB$WD_3 <- dtaModB$Fdhmwid3 * dtaModB$Fdhmdep3 * dtaModB$LSF^2
dtaModB$WD_3x <- dtaModB$Fdhawid3 * dtaModB$Fdhadep3 * dtaModB$LSF^2
dtaModB[,grep("WD",colnames(dtaModB))][is.na(dtaModB[,grep("WD",colnames(dtaModB))])] <- 0
dtaModB$WD_A <-  ifelse(dtaModB$attic=="yes", ifelse(dtaModB$WD_3!=0,  (dtaModB$WD_3 + dtaModB$WD_3x) / 2,  ifelse(dtaModB$WD_2!=0,  (dtaModB$WD_2 + dtaModB$WD_2x) / 2,  (dtaModB$WD_1 + dtaModB$WD_1x) / 2)), 0)
dtaModB$WD_SF <- rowSums(dtaModB[,grep("WDf",colnames(dtaModB))], na.rm = T)
dtaModB$WD_S1 <- rowSums(dtaModB[,grep("WD_1",colnames(dtaModB))], na.rm = T)
dtaModB$WD_S2 <- rowSums(dtaModB[,grep("WD_2",colnames(dtaModB))], na.rm = T)
dtaModB$WD_S3 <- rowSums(dtaModB[,grep("WD_3",colnames(dtaModB))], na.rm = T)
dtaModB$WD_SA <- dtaModB$WD_A
dtaModB$WD_STFA <- rowSums(dtaModB[,grep("WD_S",colnames(dtaModB))], na.rm = T)
dtaModB$WD_STFA <-  ifelse(is.na(dtaModB$Fdffloor) | (!is.na(dtaModB$Fdffloor) & (is.na(dtaModB$Fdfmainw) | is.na(dtaModB$Fdfmaind))),  dtaModB$WD_STFA, dtaModB$WD_SF)
dtaModB$Dim_BB <- ifelse(dtaModB$Fdhmlev1=='BB', dtaModB$WD_S1, 0)
dtaModB$Dim_GG <- ifelse(dtaModB$Fdhmlev1=='BB', dtaModB$WD_S2, dtaModB$WD_S1)
dtaModB$Dim_FF <- ifelse(dtaModB$Fdhmlev1=='BB', dtaModB$WD_S3, ifelse(dtaModB$storeyx>1, dtaModB$WD_S2, 0))
dtaModB$Dim_SF <- ifelse(dtaModB$Fdhmlev1=='BB' & dtaModB$storeyx>2, dtaModB$Dim_FF, ifelse(dtaModB$storeyx>2, dtaModB$WD_S3, 0))
dtaModB$Dim_HF <- ifelse(dtaModB$Fdhmlev1=='BB' & dtaModB$storeyx>3, dtaModB$Dim_SF, ifelse(dtaModB$storeyx>3, dtaModB$WD_S3, 0))
dtaModB$Dim_AT <- ifelse(dtaModB$attic=='yes', dtaModB$WD_SA, 0)
dtaModB$Dim_BB <- ifelse(!is.na(dtaModB$Fdffloor) & dtaModB$Fdhmlev1=='BB', dtaModB$WD_STFA, ifelse(is.na(dtaModB$Fdffloor), dtaModB$Dim_BB, 0))
dtaModB$Dim_GG <- ifelse(!is.na(dtaModB$Fdffloor) & dtaModB$Fdhmlev1=='GG', dtaModB$WD_STFA, ifelse(is.na(dtaModB$Fdffloor), dtaModB$Dim_GG, 0))
dtaModB$Dim_FF <- ifelse(!is.na(dtaModB$Fdffloor) & dtaModB$Fdhmlev1=='01', dtaModB$WD_STFA, ifelse(is.na(dtaModB$Fdffloor), dtaModB$Dim_FF, 0))
dtaModB$Dim_SF <- ifelse(!is.na(dtaModB$Fdffloor) & dtaModB$Fdhmlev1=='02', dtaModB$WD_STFA, ifelse(is.na(dtaModB$Fdffloor), dtaModB$Dim_SF, 0))
dtaModB$Dim_HF <- ifelse(!is.na(dtaModB$Fdffloor) & (dtaModB$Fdhmlev1=='07' | dtaModB$Fdhmlev1=='08'), dtaModB$WD_STFA, ifelse(is.na(dtaModB$Fdffloor), dtaModB$Dim_HF, 0))
dtaModB$Dim_AT <- ifelse(!is.na(dtaModB$Fdffloor) & dtaModB$WD_SA>0, dtaModB$WD_SA, ifelse(is.na(dtaModB$Fdffloor), dtaModB$Dim_AT, 0))
dtaModB$Dim_Sum <- rowSums(dtaModB[,grep("Dim_",colnames(dtaModB))], na.rm = T)
dtaModB$Dim_BB <- (dtaModB$Dim_BB / dtaModB$Dim_Sum) * dtaModB$WD_STFA
dtaModB$Dim_GG <- (dtaModB$Dim_GG / dtaModB$Dim_Sum) * dtaModB$WD_STFA
dtaModB$Dim_FF <- (dtaModB$Dim_FF / dtaModB$Dim_Sum) * dtaModB$WD_STFA
dtaModB$Dim_SF <- (dtaModB$Dim_SF / dtaModB$Dim_Sum) * dtaModB$WD_STFA
dtaModB$Dim_HF <- (dtaModB$Dim_HF / dtaModB$Dim_Sum) * dtaModB$WD_STFA
dtaModB$Dim_AT <- (dtaModB$Dim_AT / dtaModB$Dim_Sum) * dtaModB$WD_STFA
dtaModB$Dim_BBHA <- dtaModB$Dim_BB
dtaModB$Dim_GGHA <- ifelse(dtaModB$Dim_GG<dtaModB$Dim_BB, 0, dtaModB$Dim_GG-dtaModB$Dim_BB)
dtaModB$Dim_RA <- apply(dtaModB[,grep("Dim_",colnames(dtaModB))], 1, function(x) max(x, na.rm = T))
dtaModB$Dim_RA <- ifelse(is.na(dtaModB$Fdffloor),  dtaModB$Dim_RA - dtaModB$Dim_AT, ifelse(dtaModB$Fdhmlev1==dtaModB$Fdfmainl, dtaModB$Dim_RA, 0))
dtaModB$Dim_PA <- ifelse(dtaModB$Finlopos=='Top Floor Flat' | dtaModB$Finlopos=='Mid Floor Flat',  ifelse(dtaModB$Dim_GG>0, dtaModB$Dim_GG, dtaModB$Dim_FF),0)
dtaModB$Dim_CA <- ifelse(dtaModB$Finlopos=='Ground floor flat' | dtaModB$Finlopos=='Mid Floor Flat', ifelse(dtaModB$Dim_AT>0, dtaModB$Dim_AT,  ifelse(dtaModB$Dim_HF>0, dtaModB$Dim_HF,  ifelse(dtaModB$Dim_SF>0, dtaModB$Dim_SF,  ifelse(dtaModB$Dim_FF>0, dtaModB$Dim_FF,  ifelse(dtaModB$Dim_GG>0, dtaModB$Dim_GG, dtaModB$Dim_BB))))),0)
tbl.Aux.Tenths <- dtaModB
dtaModB <-
  dtaModB[,c('aacode','Dim_BB','Dim_GG','Dim_FF','Dim_SF', 'Dim_HF',
             'Dim_AT','Dim_BBHA','Dim_GGHA', 'Dim_RA','Dim_PA','Dim_CA')]
colnames(dtaModB) <-
  c('V001_HousingCode', 'D035_BasementArea',  'D036_GroundFloorArea',
    'D037_1stFloorArea',  'D038_2ndFloorArea', 'D039_3rdFloorArea',
    'D040_RoomInRoofArea',  'D059_BasementFloorHeatLossArea',
    'D060_GroundFloorHeatLossArea', 'D061_RoofArea',
    'D062_PartyFloorArea', 'D063_PartyCeilingArea')

dtaModC <- join(dtaIb, dtaAb, by='aacode')
dtaModC$LivArea <- dtaModC$Finlivwi * dtaModC$Finlivde
dtaModC$LivAreaFractionTbl <- unlist(pblapply(dtaModC$Finrooms, function(x) ifelse(x<15, tbl.AuxB$Living.Are.Fraction[tbl.AuxB$No.Rooms==x], tbl.AuxB$Living.Are.Fraction[tbl.AuxB$No.Rooms==15])))
dtaModC$LivAreaFractionCal <- dtaModC$LivArea / dtaModC$FloorArea
dtaModC$LivAreaFraction <- ifelse(is.na(dtaModC$LivArea), dtaModC$LivAreaFractionTbl, dtaModC$LivAreaFractionCal)
dtaModC <-
  dtaModC[,c('aacode','LivAreaFraction')]
colnames(dtaModC) <-
  c('V001_HousingCode', 'D078_LivingAreaFraction')

dtaModD <- join(dtaJb, dtaKb, by='aacode')
dtaModD <- join(dtaModD, dtaLb, by='aacode')
dtaModD <- join(dtaModD, dtaMb, by='aacode')
dtaModD <- join(dtaModD, dtaFb, by='aacode')
dtaModD$main.wall <- 'Stone: granite or whin (as built)'
dtaModD$main.wall[grep('solid',dtaModD$typewstr2)] <- 'Solid brick (as built)'
dtaModD$main.wall[grep('concrete',dtaModD$typewstr2)] <- 'System build (as built)'
dtaModD$main.wall[grep('situ',dtaModD$typewstr2)] <- 'Cob (as built)'
dtaModD$main.wall[grep('timber',dtaModD$typewstr2)] <- 'Timber frame'
dtaModD$main.wall[grep('mixed',dtaModD$typewstr2)] <- 'Filled cavity / Cavity with insulation (internal/external)'
dtaModD$main.wall[grep('cavity',dtaModD$typewstr2)] <- 'Cavity (as built)'
dtaModD$wall.cav <- ifelse(dtaModD$Felcavff=='Yes', TRUE, ifelse(dtaModD$Felcavlf=='Yes', TRUE, ifelse(dtaModD$Felcavrf=='Yes', TRUE, ifelse(dtaModD$Felcavbf=='Yes', TRUE, FALSE))))
dtaModD$wall.ins <- ifelse(dtaModD$Felextff=='Yes', TRUE, ifelse(dtaModD$Felextlf=='Yes', TRUE, ifelse(dtaModD$Felextrf=='Yes', TRUE, ifelse(dtaModD$Felextbf=='Yes', TRUE, FALSE))))
dtaModD$wall.typ <-
  ifelse(dtaModD$main.wall=='Stone: granite or whin (as built)',
                           ifelse(dtaModD$wall.cav==T | dtaModD$wall.ins==T, 'Stone/solid brick (external insulation)', 'Stone: granite or whin (as built)'),
                           ifelse(dtaModD$main.wall=='Solid brick (as built)',
                                  ifelse(dtaModD$wall.cav==T | dtaModD$wall.ins==T, 'Stone/solid brick (internal insulation)','Solid brick (as built)'),
                                  ifelse(dtaModD$main.wall=='Timber frame',
                                         ifelse(dtaModD$wall.cav==T | dtaModD$wall.ins==T, 'Filled cavity / Cavity with insulation (internal/external)','Timber frame'),
                                         ifelse(dtaModD$main.wall=='Cob (as built)', ifelse(dtaModD$wall.cav==T | dtaModD$wall.ins==T, 'Cob (external insulation)','Cob (as built)'),
                                                ifelse(dtaModD$main.wall=='System build (as built)',
                                                       ifelse(dtaModD$wall.cav==T | dtaModD$wall.ins==T, 'Filled cavity / Cavity with insulation (internal/external)', 'System build (as built)'),'Cavity (as built)')))))
dtaModD$ehs.no <- as.integer(dtaModD$fodconst)
dtaModD$sap.no <- fnGetAgeBands(dtaModD$fodconac)
dtaModD$wall.th <- unlist(pblapply(dtaModD$aacode, fnGetThick, dtaModD))
dtaModD$Ext.wall <- factor(dtaModD$wall.typ, levels=rownames(tbl.AuxC), labels = rownames(tbl.AuxC))
dtaModD$Ext.wall <- as.integer(dtaModD$Ext.wall)
dtaModD$BBt.wall <- ifelse(dtaModD$Fdhmlev1=='BB', dtaModD$Ext.wall, 16)
dtaModD$Sem.wall <- dtaModD$Ext.wall
dtaModD <-
  dtaModD[,c('aacode','BBt.wall','Ext.wall','Sem.wall','wall.th')]
colnames(dtaModD) <-
  c('V001_HousingCode', 'D076_BasementWallConstruction',
    'D077_ExternalWallConstruction','D122_SemiExposedConstruction',
    'D030_WallThickness')

dtaModE <- dtaNb
dtaModE$sheltered.f <- ifelse(dtaModE$Fvwtenff>=5 | is.na(dtaModE$Fvwtenff), T, F)
dtaModE$sheltered.b <- ifelse(dtaModE$Fvwtenbf>=5 | is.na(dtaModE$Fvwtenbf), T, F)
dtaModE$sheltered.l <- ifelse(dtaModE$Fvwtenlf>=5 | is.na(dtaModE$Fvwtenlf), T, F)
dtaModE$sheltered.r <- ifelse(dtaModE$Fvwtenrf>=5 | is.na(dtaModE$Fvwtenrf), T, F)
dtaModE$sheltered <- rowSums(dtaModE[,grep("sheltered.",colnames(dtaModE))], na.rm = T)
tbl.Aux.Shelter <- dtaModE
dtaModE <-
  dtaModE[,c('aacode','sheltered')]
colnames(dtaModE) <-
  c('V001_HousingCode', 'D056_SidesSheltered')

dtaTb <- tbl.Aux.Tenths[,c(1,grep("Fdh[ma]wid", colnames(tbl.Aux.Tenths)), grep("Fdh[ma]dep", colnames(tbl.Aux.Tenths)))]
dtaTb$lot.w <- rowMeans(dtaTb[,grep("Fdh[ma]wid", colnames(dtaTb))], na.rm = T)
dtaTb$lot.d <- rowMeans(dtaTb[,grep("Fdh[ma]dep", colnames(dtaTb))], na.rm = T)
val.avg.w <- mean(colMeans(dtaTb[,grep("Fdh[ma]wid", colnames(dtaTb))], na.rm = T))
val.avg.d <- mean(colMeans(dtaTb[,grep("Fdh[ma]dep", colnames(dtaTb))], na.rm = T))
dtaTb$lot.w <- ifelse(is.na(dtaTb$lot.w), val.avg.w, dtaTb$lot.w)
dtaTb$lot.d <- ifelse(is.na(dtaTb$lot.d), val.avg.d, dtaTb$lot.d)
rm(val.avg.w, val.avg.d)
dtaTb$ratio.tfa <- dtaTb$lot.w / dtaTb$lot.d
dtaTb <- subset(dtaTb, select=c(aacode, ratio.tfa))

dtaUb <- tbl.Aux.Dim[,c(1,grep("\\w+Height", colnames(tbl.Aux.Dim)))]
dtaUb[is.na(dtaUb)] <- 0

dtaModF <- subset(dtaModB, select=c(V001_HousingCode, D035_BasementArea, D036_GroundFloorArea, D037_1stFloorArea, D038_2ndFloorArea, D039_3rdFloorArea, D040_RoomInRoofArea))
colnames(dtaModF) <- c('aacode', 'BFa', 'GFa', 'FFa', 'SFa', 'HFa', 'ATa')
dtaModF$TFA <- rowSums(dtaModF[,grep("^[A-Z]{2}",colnames(dtaModF))], na.rm = T)
dtaModF <- join(dtaModF, dtaOb, by='aacode')
dtaModF <- join(dtaModF, dtaTb, by='aacode')
dtaModF$AdditionalArea <- unlist(pblapply(dtaModF$aacode, fnGetAddition, dtaModF))
dtaModF$AdditionalArea <- ifelse(dtaModF$Fshaddit=='No additional part' | dtaModF$Fshaddit=='Unknown', 0, dtaModF$AdditionalArea)
dtaModF$lot.w.BB <- ifelse(dtaModF$BFa>0, sqrt(dtaModF$BFa*(1/dtaModF$ratio.tfa)), 0)
dtaModF$lot.d.BB <- ifelse(dtaModF$BFa>0, sqrt(dtaModF$BFa*(dtaModF$ratio.tfa)), 0)
dtaModF$lot.w.GG <- ifelse(dtaModF$GFa>0, sqrt(dtaModF$GFa*(1/dtaModF$ratio.tfa)), 0)
dtaModF$lot.d.GG <- ifelse(dtaModF$GFa>0, sqrt(dtaModF$GFa*(dtaModF$ratio.tfa)), 0)
dtaModF$lot.w.FF <- ifelse(dtaModF$FFa>0, sqrt(dtaModF$FFa*(1/dtaModF$ratio.tfa)), 0)
dtaModF$lot.d.FF <- ifelse(dtaModF$FFa>0, sqrt(dtaModF$FFa*(dtaModF$ratio.tfa)), 0)
dtaModF$lot.w.SF <- ifelse(dtaModF$SFa>0, sqrt(dtaModF$SFa*(1/dtaModF$ratio.tfa)), 0)
dtaModF$lot.d.SF <- ifelse(dtaModF$SFa>0, sqrt(dtaModF$SFa*(dtaModF$ratio.tfa)), 0)
dtaModF$lot.w.HF <- ifelse(dtaModF$HFa>0, sqrt(dtaModF$HFa*(1/dtaModF$ratio.tfa)), 0)
dtaModF$lot.d.HF <- ifelse(dtaModF$HFa>0, sqrt(dtaModF$HFa*(dtaModF$ratio.tfa)), 0)
dtaModF$lot.w.AT <- ifelse(dtaModF$ATa>0, sqrt(dtaModF$ATa*(1/dtaModF$ratio.tfa)), 0)
dtaModF$lot.d.AT <- ifelse(dtaModF$ATa>0, sqrt(dtaModF$ATa*(dtaModF$ratio.tfa)), 0)
dtaModF <- join(dtaModF, dtaUb, by='aacode')
dtaModF <- join(dtaModF, subset(tbl.Aux.Shelter, select=c(aacode, sheltered)), by='aacode')
dtaModF$SrfcWll_AreaBB <-  with(dtaModF,  (2*lot.w.BB + 2*lot.d.BB) * BsHeight)
dtaModF$SrfcWll_AreaGG <-  with(dtaModF,  (2*lot.w.GG + 2*lot.d.GG) * GfHeight)
dtaModF$SrfcWll_AreaAT <-  with(dtaModF,  (2*lot.w.AT + 2*lot.d.AT) * AtHeight)
dtaModF$SrfcWll_Area <-  with(dtaModF,  (2*lot.w.BB + 2*lot.d.BB) * BsHeight +  (2*lot.w.GG + 2*lot.d.GG) * GfHeight +  (2*lot.w.FF + 2*lot.d.FF) * FfHeight+  (2*lot.w.SF + 2*lot.d.SF) * SfHeight+  (2*lot.w.HF + 2*lot.d.HF) * TfHeight+ (2*lot.w.AT + 2*lot.d.AT) * AtHeight)
dtaModF$SrfcWll_Area_Attach <- dtaModF$SrfcWll_Area * (dtaModF$sheltered / 4)
dtaModF$SrfcWll_Area_unAttach <- dtaModF$SrfcWll_Area - dtaModF$SrfcWll_Area_Attach

dtaVb <- subset(dtaPb, select=c(aacode,type,Fexdf1no))
dtaVb <- suppressMessages(dcast(dtaVb, aacode ~ type))
dtaVb$no.doors.fro <- rowSums(dtaVb[,grep("aacode", colnames(dtaVb), invert = TRUE)], na.rm = TRUE)
dtaVb$typ.door.fro <- ifelse(dtaVb$Wood>0, 'wood', '-')
dtaVb$typ.door.fro <- ifelse(dtaVb$UPVC>0, 'upvc', dtaVb$typ.door.fro)
dtaVb$typ.door.fro <- ifelse(dtaVb$Metal>0, 'metal', dtaVb$typ.door.fro)
dtaVb <- subset(dtaVb, select=c(aacode,no.doors.fro,typ.door.fro))

dtaXb <- subset(dtaPb, select=c(aacode,type,Fexdf1no))
dtaXb <- suppressMessages(dcast(dtaXb, aacode ~ type))
dtaXb$no.doors.bck <- rowSums(dtaXb[,grep("aacode", colnames(dtaXb), invert = TRUE)], na.rm = TRUE)
dtaXb$typ.door.bck <- ifelse(dtaXb$Wood>0, 'wood', '-')
dtaXb$typ.door.bck <- ifelse(dtaXb$UPVC>0, 'upvc', dtaXb$typ.door.bck)
dtaXb$typ.door.bck <- ifelse(dtaXb$Metal>0, 'metal', dtaXb$typ.door.bck)
dtaXb <- subset(dtaXb, select=c(aacode,no.doors.bck,typ.door.bck))

dtaYb <- join(dtaVb, dtaXb, by='aacode')
dtaYb$no.doors <- with(dtaYb, no.doors.fro + no.doors.bck)
dtaYb$SrfcDoor <- 1.85 * dtaYb$no.doors

dtaWb <- dtaRb
dtaWb[is.na(dtaWb)] <- 0
dtaWb <- join(dtaWb, dtaSb, by='aacode')
dtaWb$SrfcWind <- with(dtaWb, Felfenfw/10 + Felfenbw/10 + Felfenlw/10 + Felfenrw/10) / 4
dtaWb$DblGlz <- ifelse(dtaWb$dblglaz4=='entire house', 1,  ifelse(dtaWb$dblglaz4=='more than half' & dtaWb$dblglaz2=='80% or more double glazed', 0.81,  ifelse(dtaWb$dblglaz4=='more than half' & dtaWb$dblglaz2=='less than 80% double glazed', 0.70,  ifelse(dtaWb$dblglaz4=='less than half', 0.45, 0))))
dtaWb <- subset(dtaWb, select=c(aacode, SrfcWind, DblGlz))

dtaModF <- join(dtaModF, dtaWb, by='aacode')
dtaModF <- join(dtaModF, dtaYb, by='aacode')
dtaModF$SrfcWind <- dtaModF$SrfcWind * dtaModF$SrfcWll_Area_unAttach
dtaModF$SrfcWll_Area_unAttach <- with(dtaModF, SrfcWll_Area_unAttach - SrfcWind - SrfcDoor)
dtaModF$SrfcWind_sin <- with(dtaModF, SrfcWind * (1-DblGlz))
dtaModF$SrfcWind_dbl <- with(dtaModF, SrfcWind * DblGlz)
dtaModF$HeatLossAT <- with(dtaModF, ifelse(ATa>0, ATa + SrfcWll_AreaAT, 0))
dtaModF$Exp_PerBB <-  with(dtaModF, ifelse(SrfcWll_AreaBB>0, SrfcWll_AreaBB / BsHeight, 0))
dtaModF$Exp_PerGG <-  with(dtaModF, SrfcWll_AreaGG / GfHeight)
tbl.Aux.TFA <- dtaModF

dtaZb <- dtaQb
dtaZb[is.na(dtaZb)] <- 0
dtaZb$semi.flts <- with(dtaZb, Fdffroia/10 + Fdfbckia/10 + Fdflftia/10 + Fdfrigia/10) / 4

dtaModF <- join(dtaModF, dtaZb, by='aacode')
dtaModF <-
  dtaModF[,c('aacode','SrfcWll_Area_Attach', 'SrfcDoor', 'SrfcWind_sin',
             'SrfcWind_dbl', 'SrfcWll_Area_unAttach', 'SrfcWll_AreaBB',
             'HeatLossAT', 'Exp_PerBB', 'Exp_PerGG', 'semi.flts')]
colnames(dtaModF) <-
  c('V001_HousingCode','D073_PartyWallArea','D057_DoorArea',
    'D067_Windows1Area', 'D069_Windows2Area', 'D065_ExternalWallArea',
    'D064_BasementWallArea', 'D066_RoomInRoofHeatLossArea',
    'D113_BasementFloorExposedPerimeter', 'D114_GroundFloorExposedPerimeter',
    'D121_SemiExposedArea')

lst.B <- as_tibble(
  join_all(lapply(ls()[grep("dtaMod[A-F]$",ls())], get), by='V001_HousingCode'))
rm(list=ls()[grep("dta\\w+",ls())])



# C. [ventilation]--------------------------------------------------------------
dtaX <- lst.A
dtaY <- lst.B
colnames(dtaX)[1] <- colnames(dtaY)[1] <- 'aacode'

dtaAc <- subset(chimney, select=c(aacode, Type, Fexcs1no, Fexcs2no))
dtaAc <- dtaAc %>% na_if(77) %>% na_if(8) %>% na_if(9) %>% na_if(88) %>%  na_if(99) %>% na_if(88.8) %>% na_if(99.9) %>% na_if(77.7)
dtaBc <- subset(services, select=c(aacode, Finohage, Finchbcd, Finmhboi, Finoheat, Finohtyp))
dtaBc <- dtaBc %>% na_if(77) %>% na_if(8) %>% na_if(9) %>% na_if(88) %>%  na_if(99) %>% na_if(888) %>% na_if(999) %>% na_if(777)
dtaCc <- subset(interior, select=c(aacode, Finrooms, Finlivle, Finkitle))
dtaCc <- dtaCc %>% na_if(77) %>% na_if(8) %>% na_if(9) %>% na_if(88) %>%  na_if(99) %>% na_if(88.8) %>% na_if(99.9) %>% na_if(77.7)
dtaDc <- subset(introoms, select=c(aacode, type, Finflrsf))

dtaModA <- join(suppressMessages(dcast(subset(dtaAc, select=c(-Fexcs2no)), aacode ~ Type)), suppressMessages(dcast(subset(dtaAc, select=c(-Fexcs1no)), aacode ~ Type)), by='aacode')
colnames(dtaModA) <- c('aacode','a.masonry','a.other','b.masonry','b.other')
dtaModA <- join(dtaModA, dtaBc, by='aacode')
dtaModA$no.chimneys <- rowSums(dtaModA[,grep("[ab]\\.", colnames(dtaModA))], na.rm = T)
dtaModA$no.chimneys <- ifelse(dtaModA$no.chimneys>0, 1, 0)
dtaModA$Finohage <- as.integer(ifelse(is.na(dtaModA$Finohage), 0, dtaModA$Finohage))
dtaModA$flue <- ifelse(dtaModA$Finoheat=='Yes' & as.integer(dtaModA$Finohtyp)<9 & dtaModA$Finohage<35, 1, 0)
dtaModA$flue <- dtaModA$flue * dtaModA$no.chimneys
dtaModA$no.chimneys <- dtaModA$flue <- dtaModA$no.chimneys
dtaModA$no.chimneys.sec <- dtaModA$no.chimneys.oth <- 0
dtaModA <-
  dtaModA[,c('aacode','no.chimneys','no.chimneys.sec','no.chimneys.oth')]
colnames(dtaModA) <-
  c('V001_HousingCode','D047_ChimneyNumber',
    'H022_Chimenys_SecondaryHeating','H023_Chimneys_Other')

dtaModB <- dtaBc
dtaModB$Finchbcd <- ifelse(dtaModB$Finchbcd==9999|dtaModB$Finchbcd==7777|dtaModB$Finchbcd==8888, 108, dtaModB$Finchbcd)
dtaModB$heat.typ <- unlist(pblapply(dtaModB$aacode, fnGetHeat, dtaModB))
dtaModB$heat.flue <- unlist(pblapply(dtaModB$aacode, fnGetHeat, dtaModB, 'Boiler.Flue'))
dtaModB$heat.flue <- ifelse(dtaModB$Finchbcd==108, 2, dtaModB$heat.flue)
dtaModB$heat.flue <- ifelse(dtaModB$Finchbcd==109, 1, dtaModB$heat.flue)
dtaModB$heat.flue <- ifelse(dtaModB$Finchbcd>=110&dtaModB$Finchbcd<=119, 2, dtaModB$heat.flue)
dtaModB$heat.flue <- ifelse(dtaModB$Finchbcd>=120&dtaModB$Finchbcd<=122, 2, dtaModB$heat.flue)
dtaModB$heat.flue <- ifelse(dtaModB$Finchbcd>=604&dtaModB$Finchbcd<=609, 2, dtaModB$heat.flue)
dtaModB$heat.flue.open <- ifelse(dtaModB$heat.flue==1, 1, 0)
dtaModB$heat.flue.open.sec <- ifelse(dtaModB$Finohtyp=='Mains gas - open flue', 1, 0)
dtaModB$heat.flue.open.oth <- 0
tbl.Aux.Heat <- dtaModB
dtaModB <-
  dtaModB[,c('aacode','heat.flue.open',
             'heat.flue.open.sec','heat.flue.open.oth')]
colnames(dtaModB) <-
  c('V001_HousingCode','D048_OpenFlues_MainHeating',
    'D049_OpenFlue_SecondaryHeating','H026_OpenFlues_Other')

dtaModC <- join(subset(dtaX, select=c(aacode, V003_ConstructionDate)), dtaCc, by='aacode')
dtaModC <- join(dtaModC, subset(tbl.Aux.Heat, select=c(aacode, heat.typ, Finohtyp)), by='aacode')
dtaModC <- join(dtaModC, subset(dtaY, select=c(aacode, D077_ExternalWallConstruction)), by='aacode')
dtaModC$Finrooms <- ifelse(is.na(dtaModC$Finrooms), 1, dtaModC$Finrooms)
dtaModC$Ext.fans <- ifelse(dtaModC$V003_ConstructionDate<6, 0,  ifelse(dtaModC$V003_ConstructionDate<8, 1, ifelse(dtaModC$Finrooms<=2, 1, ifelse(dtaModC$Finrooms<=5, 2, ifelse(dtaModC$Finrooms<=8, 3, 4)))))
dtaModC$Pass.vens <- 0
dtaModC$flueless <- ifelse(as.integer(dtaModC$Finohtyp)==8 | as.integer(dtaModC$Finohtyp)==11 | as.integer(dtaModC$Finohtyp)==12 | as.integer(dtaModC$Finohtyp)==13 |  dtaModC$heat.typ == 6 |  dtaModC$heat.typ == 7 |  dtaModC$heat.typ == 8 |  dtaModC$heat.typ == 10 |  dtaModC$heat.typ == 11  |  dtaModC$heat.typ == 12  |  dtaModC$heat.typ == 14 |  dtaModC$heat.typ == 15, 1, 0)
dtaModC$strc.inf <- ifelse(dtaModC$D077_ExternalWallConstruction==6|dtaModC$D077_ExternalWallConstruction==7|dtaModC$D077_ExternalWallConstruction==10, 0.25, 0.35)
dtaModC <-
  dtaModC[,c('aacode','Ext.fans','Pass.vens','flueless','strc.inf')]
colnames(dtaModC) <-
  c('V001_HousingCode','D050_ExtractorFansNumber','H028_PassiveVents',
    'D051_FluelessGasFire','D052_StructuralInfiltration')

dtaModD <- dtaDc
dtaModD$Finflrsf <- ifelse(dtaModD$Finflrsf=="Yes", TRUE, FALSE)
dtaModD <- suppressMessages(dcast(dtaModD, aacode ~ type))
colnames(dtaModD) <- tolower(gsub(" ",".",colnames(dtaModD)))
dtaModD <- join(dtaModD, dtaCc, by='aacode')
dtaModD <- join(dtaModD, subset(dtaX, select=c(aacode, D002_DwellingType, V003_ConstructionDate)), by='aacode')
dtaModD <- join(dtaModD, subset(dtaY, select=c(aacode, D067_Windows1Area, D069_Windows2Area)), by='aacode')
dtaModD$Finlivle <- ifelse(is.na(dtaModD$Finlivle), '01', dtaModD$Finlivle)
dtaModD$Finkitle <- ifelse(is.na(dtaModD$Finkitle), '01', dtaModD$Finkitle)
dtaModD$Finlivle <- as.factor(dtaModD$Finlivle)
dtaModD$Finkitle <- as.factor(dtaModD$Finkitle)
dtaModD$floor.reg <- ifelse(dtaModD$Finlivle=='GG' | dtaModD$Finlivle=='BB' | dtaModD$Finkitle=='GG' | dtaModD$Finkitle=='BB', 'known', 'unknown')
dtaModD$floor.type <- ifelse((dtaModD$living.room==TRUE | dtaModD$kitchen==TRUE) & dtaModD$floor.reg=='known', 'solid', 'timber')
dtaModD$floor.type <- ifelse(dtaModD$floor.reg=='unknown', 'solid', dtaModD$floor.type)
dtaModD$floor.seal <- ifelse(dtaModD$V003_ConstructionDate<=6 & dtaModD$floor.type=='timber', 'unsealed', ifelse(dtaModD$V003_ConstructionDate>6 & dtaModD$floor.type=='timber', 'sealed', '-'))
dtaModD$floor.inf <- ifelse(dtaModD$floor.seal=='unsealed', 0.2, ifelse(dtaModD$floor.seal=='sealed', 0.1, 0.0))
dtaModD$draught.lobby <- ifelse(as.integer(dtaModD$D002_DwellingType)<5, FALSE, TRUE)
dtaModD$draught.win.stripped <- round(100 * (dtaModD$D069_Windows2Area / (dtaModD$D067_Windows1Area + dtaModD$D069_Windows2Area)), 0)
dtaModD$vent <- 'natural'
tbl.Aux.Floor <- dtaModD
dtaModD <-
  dtaModD[,c('aacode','floor.inf','draught.lobby',
             'draught.win.stripped','vent')]
colnames(dtaModD) <-
  c('V001_HousingCode','D053_FloorInfiltration','D054_DraughtLobby',
    'D055_WindowsDoorsDraughtstripped','H035_VentilationSystem')

lst.C <- as_tibble(
  join_all(lapply(ls()[grep("dtaMod[A-D]$",ls())], get), by='V001_HousingCode'))
rm(list=ls()[grep("dta\\w+",ls())])



# D. [other.heat.loss.elements]-------------------------------------------------
dtaX <- lst.A
dtaY <- lst.B
dtaZ <- lst.C
colnames(dtaX)[1] <- colnames(dtaY)[1] <- colnames(dtaZ)[1] <- 'aacode'

dtaAd <- subset(physical_plus, select=c(aacode, typewin, typercov, typerstr))
dtaBd <- subset(services, select=c(aacode, Flithick))

dtaModA <- join(dtaAd, dtaBd, by='aacode')
dtaModA <- join(dtaModA, subset(dtaY, select=c(aacode, D067_Windows1Area, D069_Windows2Area, D059_BasementFloorHeatLossArea, D060_GroundFloorHeatLossArea, D040_RoomInRoofArea, D063_PartyCeilingArea)), by='aacode')
dtaModA <- join(dtaModA, subset(tbl.Aux.Floor, select=c(aacode, floor.type, floor.seal)), by='aacode')
dtaModA <- join(dtaModA, subset(tbl.Aux.TFA, select=c(aacode, SrfcWll_Area, SrfcWind, BFa, GFa, FFa, SFa, HFa, ATa, TFA)), by='aacode')
dtaModA <- join(dtaModA, subset(tbl.Aux.Tenths, select=c(aacode, storeyx, attic, Finlopos)), by='aacode')
dtaModA$door.u <- 3.0
dtaModA$winA.type <- ifelse(dtaModA$D067_Windows1Area>0, 'single glazing', '-')
dtaModA$winB.type <- ifelse(dtaModA$D069_Windows2Area>0, 'double glazing', '-')
dtaModA$winA.frame <- ifelse(dtaModA$winA.type=='-', '-', as.character(dtaModA$typewin))
dtaModA$winB.frame <- ifelse(dtaModA$winB.type=='-', '-', as.character(dtaModA$typewin))
dtaModA$winA.ovrshd <- ifelse(dtaModA$winA.type=='-', '-', 'average or unknown (20‐60% obstruction)')
dtaModA$winA.orintn <- ifelse(dtaModA$winA.type=='-', '-', 'east/west')
dtaModA$winB.ovrshd <- ifelse(dtaModA$winB.type=='-', '-', 'average or unknown (20‐60% obstruction)')
dtaModA$winB.orintn <- ifelse(dtaModA$winB.type=='-', '-', 'east/west')
dtaModA$winRf.type <- dtaModA$winRf.frame <- dtaModA$winRf.orintn <- "-"
dtaModA$winRf.area <- 0
dtaModA$floor.type.BB <- ifelse(dtaModA$D059_BasementFloorHeatLossArea>0, dtaModA$floor.type, '-')
dtaModA$floor.type.GG <- ifelse(dtaModA$D060_GroundFloorHeatLossArea>0, dtaModA$floor.type, '-')
dtaModA$floor.exp.type <- '-'
dtaModA$floor.exp.area <- 0
dtaModA$roof.type <- paste(dtaModA$typercov, dtaModA$typerstr, sep=" / ")
dtaModA$loft.ins <- dtaModA$Flithick
dtaModA$loft.ins <- ifelse(dtaModA$loft.ins=='No insulation' | dtaModA$loft.ins=='Question Not Applicable' | dtaModA$loft.ins=='Don t know thickness', '0mm', as.character(dtaModA$loft.ins))
dtaModA$loft.ins <- as.integer(gsub("\\D","", dtaModA$loft.ins))
dtaModA$attic.type <- ifelse(dtaModA$D040_RoomInRoofArea>0, dtaModA$roof.type, '-')
dtaModA$party.wall.type <- 'single plasterboard on dabs on both sides, dense blocks, cavity'
dtaModA$party.floor.type <- 'precast concrete planks floor, screed, carpeted'
dtaModA$party.ceil.type <- 'plasterboard ceiling, carpeted chipboard floor'
dtaModA$int.area <- dtaModA$SrfcWll_Area
dtaModA$int.wall.type <- "dense block, dense plaster"
dtaModA$int.floor.type <- "timber I‐joists, carpeted"
dtaModA$int.ceil.type <- "plasterboard ceiling, carpeted chipboard floor"
dtaModA$int.floor.area <- with(dtaModA, ifelse(ATa>0, TFA-ATa-BFa-GFa, TFA-BFa-GFa))
dtaModA$int.floor.area <- with(dtaModA, ifelse((Finlopos=='Top Floor Flat' | Finlopos=='Mid Floor Flat' | Finlopos=='Ground floor flat') & SFa<0, 0, int.floor.area))
dtaModA$int.ceil.area <- with(dtaModA, int.floor.area + D063_PartyCeilingArea)
dtaModA$int.ceil.area <- with(dtaModA, ifelse(int.ceil.area > int.floor.area, int.floor.area, int.ceil.area))
dtaModA <-
  dtaModA[,c('aacode', 'door.u', 'winA.type', 'winB.type', 'winA.frame',
             'winB.frame', 'winA.ovrshd', 'winA.orintn', 'winB.ovrshd',
             'winB.orintn', 'winRf.type', 'winRf.area', 'winRf.frame',
             'winRf.orintn', 'floor.type.BB', 'floor.type.GG',
             'floor.exp.type', 'floor.exp.area', 'roof.type', 'loft.ins',
             'attic.type', 'party.wall.type', 'party.floor.type',
             'party.ceil.type', 'int.area', 'int.floor.area',
             'int.ceil.area', 'int.wall.type', 'int.floor.type',
             'int.ceil.type')]
colnames(dtaModA) <-
  c('V001_HousingCode', 'D058_DoorUValue', 'D068_Windows1Type',
    'D070_Windows2Type',  'D071_Windows1FrameType',
    'D072_Windows2FrameType', 'H041_Windows1Overshading',
    'H042_Windows1Orientation', 'H046_Windows1Overshading',
    'H047_Windows1Orientation',  'H048_RoofWindowType',
    'H049_RoofWindowArea', 'H050_RoofWindowFrame',
    'H051_RoofWindowOrientation', 'D074_BasementFloorConstructionType',
    'D075_GroundFloorConstructionType',  'H058_ExposedFloorConstruction',
    'H059_ExposedFloorHeatLossArea',  'D079_RoofConstruction',
    'D080_LoftInsulationThickness',  'D081_RoomInRoofConstruction',
    'H072_PartyWallConstruction', 'H074_PartyFloorConstruction',
    'H076_PartyCeilingConstruction',  'H079_InternalWallArea',
    'H081_InternalFloorArea',  'H083_InternalCeilingArea',
    'H078_InternalWallConstruction', 'H080_InternalFloorConstruction',
    'H082_InternalCeilingConstruction')

lst.D <- dtaModA
rm(list=ls()[grep("dta\\w+",ls())])
lst.D <- as_tibble(lst.D)



# E. [space.heating]------------------------------------------------------------
dtaAe <- subset(services, select=c(aacode, Finmhfue, Finchtim, Finchrom, Finchtrv))
dtaBe <- tbl.Aux.Heat # generated in "C:ventilation"

dtaModA <- join(dtaAe, dtaBe, by='aacode')
dtaModA$heat.code <-  ifelse((dtaModA$Finchbcd==101 |  dtaModA$Finchbcd==102 |   dtaModA$Finchbcd==103 |  dtaModA$Finchbcd==104 |   dtaModA$Finchbcd==105 |   dtaModA$Finchbcd==107 |  dtaModA$Finchbcd==108) &   dtaModA$Finmhboi=='Back boiler',  3,  ifelse((dtaModA$Finchbcd==101 |  dtaModA$Finchbcd==102 |   dtaModA$Finchbcd==103 |  dtaModA$Finchbcd==104 |   dtaModA$Finchbcd==105 |   dtaModA$Finchbcd==107 |  dtaModA$Finchbcd==108) &   dtaModA$Finmhboi=='Combination', 2,  dtaModA$heat.typ))
dtaModA$tariff.ele <- as.factor(ifelse(dtaModA$heat.code==6 | dtaModA$heat.code==7 | dtaModA$heat.code==14 | dtaModA$heat.code==15, as.character(dtaModA$Finmhfue), "-"))
dtaModA$tariff.cmn <- as.factor(ifelse(dtaModA$heat.code==11 | dtaModA$heat.code==12, "Usage based charging", "-"))
dtaModA$fuel.cmn <- as.factor(ifelse(dtaModA$heat.code==11 | dtaModA$heat.code==12, "Gas", "-"))
dtaModA$fraction.cmn <- ifelse(dtaModA$heat.code==12, 0.35, NA)
dtaModA$fuel.chp <- as.factor(ifelse(dtaModA$heat.code==12, "Gas", "-"))
dtaModA$heat.emitter <- unlist(pblapply(dtaModA$aacode, fnGetHeat, dtaModA, 'Heat.emitter'))
dtaModA$heat.eff <- unlist(pblapply(dtaModA$aacode, fnGetHeat, dtaModA, 'System.efficiency'))
dtaModA$oil.pump.loc <- as.factor(ifelse(dtaModA$heat.code==4, "Outside dwelling", "-"))
dtaModA$mh.prog <- as.factor(ifelse(dtaModA$Finchtim=="Yes", TRUE, FALSE))
dtaModA$mh.thermostat <- as.factor(ifelse(dtaModA$Finchrom=="Yes", TRUE, FALSE))
dtaModA$mh.trvs <- as.factor(ifelse(dtaModA$Finchtrv=="Yes", TRUE, FALSE))
dtaModA$sec.heat <- "No secondary system"
dtaModA$sec.heat[grep("Electric",dtaModA$Finohtyp)] <- "Electric room heaters"
dtaModA$sec.heat[grep("LPG|Solid|Paraffin|Other",dtaModA$Finohtyp)] <- "Gas Coal effect fire"
dtaModA$sec.heat[grep("Mains gas",dtaModA$Finohtyp)] <- "Gas fire"
dtaModA$sec.heat[grep("open fire",dtaModA$Finohtyp)] <- "Open fire"
dtaModA$sec.heat <- factor(dtaModA$sec.heat)
dtaModA$sec.heat <- as.integer(factor(dtaModA$sec.heat,levels(dtaModA$sec.heat)[c(4,3,2,1,5)]))
tbl.Aux.MSH <- dtaModA
dtaModA <-
  dtaModA[,c('aacode','heat.code','tariff.ele','tariff.cmn','fuel.cmn',
             'fraction.cmn', 'fuel.chp','heat.flue','heat.emitter',
             'heat.eff','oil.pump.loc','mh.prog','mh.thermostat',
             'mh.trvs','sec.heat')]
colnames(dtaModA) <-
  c('V001_HousingCode','D082_MainHeatingSystemType',
    'D083_MainHeatingElectricTariff','D084_MainHeatingCommunityHeatingTariff',
    'D085_MainHeatingCommunityHeatingFuel',
    'D086_MainHeatingCommunityHeatingCHPFraction',
    'D087_MainHeatingCommunityHeatingCHPFuel','D088_MainHeatingHeaterFlue',
    'D090_MainHeatingHeatEmitter','D091_MainHeatingEfficiency',
    'D089_MainHeatingOilPumpLocation','D092_MainHeatingControl_Programmer',
    'D093_MainHeatingControl_RoomThermostat','D094_MainHeatingControl_TRVs',
    'D095_SecondaryHeatingSystem')

lst.E <- as_tibble(dtaModA)
rm(list=ls()[grep("dta\\w+",ls())])



# F. [water.heating]------------------------------------------------------------
dtaAf <- subset(services, select=c(aacode, Finwhcpr, Finwhopr, Finwhoty, Finwhxpr, Finwhxty, Finwsipr, Finwsity, Finwdipr, Finwdity, Finwsppr, Finwspty, Finwmppr, Finwmpty, Finwhlpr, Finwhlty, Finwhsiz, Finwhins, Finwhmms, Finwhthe, Finwotfu, Finwhthe))
dtaBf <- subset(firstimp_physical, select=c(aacode, fodconst, fodconac))

dtaModA <- join(subset(tbl.Aux.MSH, select=c(aacode, heat.code, tariff.ele, heat.eff)), dtaBf, by='aacode')
dtaModA <- join(dtaModA, dtaAf, by='aacode')
dtaModA$DHWwCH <- ifelse(dtaModA$Finwhcpr=='Yes', TRUE, FALSE)
dtaModA$DHW.boilero <- as.factor(ifelse(dtaModA$Finwhopr=='Yes', as.character(dtaModA$Finwhoty), "-"))
dtaModA$DHW.backboi <- as.factor(ifelse(dtaModA$Finwhxpr=='Yes', as.character(dtaModA$Finwhxty), "-"))
dtaModA$DHW.sin_imm <- as.factor(ifelse(dtaModA$Finwsipr=='Yes', as.character(dtaModA$Finwsity), "-"))
dtaModA$DHW.dbl_imm <- as.factor(ifelse(dtaModA$Finwdipr=='Yes', as.character(dtaModA$Finwdity), "-"))
dtaModA$DHW.sin_ins <- as.factor(ifelse(dtaModA$Finwsppr=='Yes', as.character(dtaModA$Finwspty), "-"))
dtaModA$DHW.dbl_ins <- as.factor(ifelse(dtaModA$Finwmppr=='Yes', as.character(dtaModA$Finwmpty), "-"))
dtaModA$DHW.cmmunal <- as.factor(ifelse(dtaModA$Finwhlpr=='Yes', as.character(dtaModA$Finwhlty), "-"))
dtaModA$DHW.unknown <- with(dtaModA, ifelse(dtaModA$heat.code<=3 & (DHW.boilero %in% c('-','Mains gas','Bulk LPG') | DHW.backboi %in% c('-','Mains gas') | (DHW.sin_imm=='-' & DHW.dbl_imm=='-') | (DHW.sin_ins=='-' & DHW.dbl_ins=='-')), 'Gas standard', 'Electric boiler'))
dtaModA$DHW.typ <- dtaModA$DHW.unknown
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.boilero %in% c('Mains gas','Bulk LPG') |  dtaModA$DHW.backboi %in% c('Mains gas') | dtaModA$DHW.sin_ins %in% c('Mains gas','Bulk LPG') | dtaModA$DHW.dbl_ins %in% c('Mains gas','Bottled gas') | dtaModA$heat.code==9, 'Gas standard', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse((dtaModA$DHW.sin_imm!='-' | dtaModA$DHW.dbl_imm!='-') & dtaModA$heat.code==10, 'Electric boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.unknown=='Electric boiler' & (dtaModA$DHW.sin_ins=='Standard' |  dtaModA$DHW.dbl_ins=='Standard'), 'Other electric', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$Finwotfu %in% c(15:19), 'Electric boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code==2, 'Gas - combi (storage)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(((dtaModA$DHW.sin_ins %in% c('Mains gas','Bulk LPG') | dtaModA$DHW.dbl_ins %in% c('Mains gas','Bottled gas')) & dtaModA$DHWwCH==T), 'Gas - combi (instantaneous)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code==3 |  dtaModA$DHW.backboi!='-', 'Gas back boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code==4 | dtaModA$DHW.boilero=='Oil' |  dtaModA$DHW.backboi=='Oil' |  dtaModA$DHW.dbl_ins=='Oil', 'Oil standard', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse((dtaModA$heat.code==5 & dtaModA$DHWwCH==T) | dtaModA$DHW.boilero %in% c('Coal','Smokeless','Anthracite') |  dtaModA$DHW.backboi %in% c('Coal','Smokeless','Anthracite'), 'Solid boiler (house coal/anthracite)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code==13 |  dtaModA$DHW.boilero %in% c('Wood') |  dtaModA$DHW.backboi %in% c('Wood'), 'Biomass boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(((dtaModA$heat.code>=6 & dtaModA$heat.code<=8) | dtaModA$heat.code==10 | dtaModA$heat.code>=14) & (dtaModA$DHW.sin_ins=='Standard' | dtaModA$DHW.dbl_ins=='Standard'), 'Other electric', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(((dtaModA$heat.code>=6 & dtaModA$heat.code<=8) | dtaModA$heat.code==10 | dtaModA$heat.code>=14) & (dtaModA$DHW.sin_imm!='-' | dtaModA$DHW.dbl_imm!='-'), 'Electric boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse((dtaModA$DHW.typ=='Gas standard' | dtaModA$DHW.typ=='Electric boiler') & dtaModA$DHWwCH==F & (dtaModA$DHW.sin_imm!='-' | dtaModA$DHW.dbl_imm!='-'), 'Electric boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code %in% c(7,9) & dtaModA$DHWwCH==F & (dtaModA$DHW.sin_ins %in% c('Mains gas') | dtaModA$DHW.dbl_ins %in% c('Mains gas')), 'Gas - combi (instantaneous)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.cmmunal=='CHP/Waste' | (dtaModA$heat.code==12 & dtaModA$DHWwCH==T), 'Community heating with CHP', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.cmmunal=='From boiler' | (dtaModA$heat.code==11 & dtaModA$DHWwCH==T), 'Community heating without CHP', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.typ=='Gas - combi (instantaneous)' & dtaModA$DHWwCH==T & (dtaModA$DHW.sin_imm!='-' | dtaModA$DHW.dbl_imm!='-') & (dtaModA$DHW.sin_ins!='-' | dtaModA$DHW.dbl_ins!='-'), 'Gas standard', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code==7 & dtaModA$DHW.typ=='Gas - combi (instantaneous)' & (dtaModA$DHW.sin_ins=='Standard' | dtaModA$DHW.dbl_ins=='Standard') & (dtaModA$DHW.sin_ins!='-' | dtaModA$DHW.dbl_ins!='-'), 'Gas standard', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.unknown=='Gas standard' & dtaModA$DHW.boilero %in% c('Mains gas','Bulk LPG'), 'Gas standard', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.typ=='Gas standard' & dtaModA$DHWwCH==F & (dtaModA$DHW.sin_ins %in% c('Standard') | dtaModA$DHW.dbl_ins %in% c('Standard')), 'Other electric', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.typ=='Gas standard' & dtaModA$heat.code>3 & (dtaModA$DHW.sin_ins %in% c('Mains gas','Bottled gas') | dtaModA$DHW.dbl_ins %in% c('Mains gas','Bottled gas')), 'Gas - combi (instantaneous)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHW.typ=='Gas standard' & dtaModA$heat.code==1 & dtaModA$DHWwCH==F & (dtaModA$DHW.sin_ins %in% c('Mains gas','Bottled gas') | dtaModA$DHW.dbl_ins %in% c('Mains gas','Bottled gas')), 'Gas - combi (instantaneous)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHWwCH==F & dtaModA$DHW.boilero=='Coal', 'Solid boiler (house coal/anthracite)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$heat.code==4 & dtaModA$DHWwCH==F & (dtaModA$DHW.sin_imm!='-' | dtaModA$DHW.dbl_imm!='-'), 'Electric boiler', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHWwCH==F & (dtaModA$DHW.boilero=='Oil' | dtaModA$DHW.backboi=='Oil'), 'Oil standard', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHWwCH==F & (dtaModA$DHW.boilero=='Coal' | dtaModA$DHW.backboi%in%c('Coal','Anthracite')), 'Solid boiler (house coal/anthracite)', dtaModA$DHW.typ)
dtaModA$DHW.typ <- ifelse(dtaModA$DHWwCH==T & dtaModA$heat.code==1 & dtaModA$DHW.typ=='Gas - combi (instantaneous)', 'Gas standard', dtaModA$DHW.typ)
dtaModA$DHW.typ.ele <- ifelse(dtaModA$DHW.typ %nin% c('Other electric','Electric boiler'), '-', 'other electric')
dtaModA$DHW.typ.ele <- ifelse(dtaModA$DHW.typ.ele=='other electric' & dtaModA$Finwsipr=='Yes', 'single immersion', dtaModA$DHW.typ.ele)
dtaModA$DHW.typ.ele <- ifelse(dtaModA$DHW.typ.ele=='other electric' & dtaModA$Finwdipr=='Yes', 'dual immersion', dtaModA$DHW.typ.ele)
dtaModA$DHW.tariff.ele <- ifelse(dtaModA$DHW.typ.ele=='-', '-', 'tariff')
dtaModA$DHW.tariff.ele <- ifelse(dtaModA$DHW.tariff.ele=='tariff' & dtaModA$tariff.ele!='-', as.character(dtaModA$tariff.ele), '-')
dtaModA$DHW.tariff.ele <- ifelse(dtaModA$DHW.typ.ele=='other electric', 'standard tariff', dtaModA$DHW.tariff.ele)
dtaModA$DHW.tariff.com <- ifelse(dtaModA$DHW.typ=='Community heating without CHP' | dtaModA$DHW.typ=='Community heating with CHP', 'Usage based charging', '-')
dtaModA$DHW.fuel.com <- ifelse(dtaModA$DHW.tariff.com!='-', 'Gas', '-')
dtaModA$DHW.fraction.com.chp <- ifelse(dtaModA$DHW.typ=='Community heating with CHP', 0.35, 0)
dtaModA$DHW.fuel.com.chp <- ifelse(dtaModA$DHW.typ=='Community heating with CHP', 'Gas', '-')
dtaModA$DHW.eff.est <- unlist(pblapply(dtaModA$aacode, fnGetEff, dtaModA))
dtaModA$DHW.eff <- ifelse(dtaModA$DHWwCH==TRUE, dtaModA$heat.eff, NA)
dtaModA$DHW.eff <- ifelse(is.na(dtaModA$DHW.eff), dtaModA$DHW.eff.est, dtaModA$DHW.eff)
dtaModA$DHW.cyl.vol <- ifelse((dtaModA$DHW.typ=='Other electric' | dtaModA$DHW.typ=='Electric boiler') & dtaModA$Finwhsiz=='Question Not Applicable', '450 x 900mm (110 l)', as.character(dtaModA$Finwhsiz))
dtaModA$DHW.cyl.vol <- ifelse(dtaModA$DHW.cyl.vol=="Question Not Applicable" | dtaModA$DHW.cyl.vol=="Unknown", "0", dtaModA$DHW.cyl.vol)
dtaModA$DHW.cyl.vol <- gsub("\\d.*\\(","", dtaModA$DHW.cyl.vol)
dtaModA$DHW.cyl.vol <- gsub("\\s*l\\)","", dtaModA$DHW.cyl.vol)
dtaModA$DHW.cyl.vol <- as.integer(dtaModA$DHW.cyl.vol)
dtaModA$DHW.cyl.ins.typ <- ifelse(dtaModA$Finwhins=='None' | dtaModA$Finwhins=='Question Not Applicable' | dtaModA$Finwhins=='Unknown', "-", as.character(dtaModA$Finwhins))
dtaModA$DHW.cyl.ins.thk <- ifelse(dtaModA$Finwhmms=='Question Not Applicable' | dtaModA$Finwhmms=='Unknown' | dtaModA$Finwhmms=='0', "0 mm", as.character(dtaModA$Finwhmms))
dtaModA$DHW.cyl.ins.thk <- as.numeric(gsub("\\s*mm","", dtaModA$DHW.cyl.ins.thk))
dtaModA$DHW.pip.ins <- ifelse(as.integer(dtaModA$fodconst)>9, TRUE, FALSE)
dtaModA$DHW.cyl.stt <- ifelse(dtaModA$Finwhthe=='Yes', TRUE, FALSE)
dtaModA$DHW.solar <- ifelse(dtaModA$Finwotfu==15, TRUE, FALSE)
dtaModA$DHW.solar.cyl <- ifelse(dtaModA$DHW.solar==TRUE & dtaModA$DHW.cyl.vol!="-", TRUE, FALSE)
dtaModA$DHW.solar.str <- ifelse(dtaModA$DHW.solar==FALSE & dtaModA$DHW.solar.cyl==FALSE, 0, ifelse(dtaModA$DHW.solar==TRUE & dtaModA$DHW.solar.cyl==FALSE, 75, 0))
dtaModA$DHW.solar.str <- ifelse(dtaModA$DHW.solar==TRUE & dtaModA$DHW.solar.cyl==TRUE,dtaModA$DHW.cyl.vol/2,dtaModA$DHW.solar.str)
tbl.DHW <- unique(as.character(tbl.AuxF$Domestic.Hot.Water))
tbl.DHW <- gsub("^ *","",tbl.DHW, perl = T)
dtaModA$DHW.typ <- as.integer(factor(dtaModA$DHW.typ, levels = tbl.DHW))
rm(tbl.DHW)
dtaModA <-
  dtaModA[,c('aacode','DHWwCH','DHW.typ','DHW.typ.ele','DHW.tariff.ele',
             'DHW.tariff.com', 'DHW.fuel.com', 'DHW.fraction.com.chp',
             'DHW.fuel.com.chp', 'DHW.eff', 'DHW.cyl.vol', 'DHW.cyl.ins.typ',
             'DHW.cyl.ins.thk', 'DHW.pip.ins', 'DHW.cyl.stt', 'DHW.solar',
             'DHW.solar.cyl', 'DHW.solar.str')]
colnames(dtaModA) <-
  c('V001_HousingCode', 'D096_DHWSystemWithCentralHeating',
    'D097_DHWSystemType', 'D098_DHWElectricSystemType',
    'D099_DHWElectricSystemTariff', 'D100_DHWCommunityHeatingTariff',
    'D101_DHWCommunityHeatingFuel', 'D102_DHWCommunityHeatingCHPFraction',
    'D103_DHWCommunityHeatingCHPFuel', 'D104_DHWSystemEfficiency',
    'D105_DHWCylinderVolume', 'D106_DHWCylinderInsulationType',
    'D107_DHWCylinderInsulationThickness', 'H096_DHWPrimaryPipeworkInsulation',
    'D108_DHWCylinderstat', 'D109_SolarDHWSystem', 'D110_SolarDHWInCylinder',
    'D111_SolarDHWStorage')

lst.F <- as_tibble(dtaModA)
rm(list=ls()[grep("dta\\w+",ls())])




# G. [low.energy.lighting]------------------------------------------------------
dtaAg <- subset(introoms, select=c(aacode, type, Finhtglg))

dtaModA <- dtaAg
dtaModA$Finhtglg <- ifelse(dtaModA$Finhtglg=="Yes", TRUE, FALSE)
dtaModA <- suppressMessages(dcast(dtaModA, aacode ~ type))
colnames(dtaModA) <- tolower(gsub(" ",".",colnames(dtaModA)))
dtaModA$weight.liv <- dtaModA$living.room * 2
dtaModA$weight.kit <- dtaModA$kitchen * 2
dtaModA$weight.bed <- dtaModA$bedroom * 1
dtaModA$weight.bat <- dtaModA$bathroom * 1
dtaModA$weight.cir <- dtaModA$circulation * 2
dtaModA$LEL <-
  rowSums(dtaModA[,grep("weight.", colnames(dtaModA))], na.rm = T) / 10
dtaModA <-
  dtaModA[,c('aacode','LEL')]
colnames(dtaModA) <-
  c('V001_HousingCode','D112_LowEnergyLightsFraction')

lst.G <- as_tibble(dtaModA)
rm(list=ls()[grep("dta\\w+",ls())])




# H. [additional]------------------------------------------------------
dtaX <- lst.A
dtaY <- lst.B
dtaZ <- lst.C
dtaW <- lst.D

dtaHa <- subset(dormers, select=c(aacode,type,Fexdb1pr,Fexdb1no,Fexdb1ag,Fexdb2pr,Fexdb2no,Fexdb2ag))
dtaHa <- dtaHa %>% na_if(8) %>% na_if(9) %>% na_if(88.8) %>% na_if(99.9) %>% na_if(88) %>% na_if(99) %>% na_if(77)
dtaHb <- subset(elevate, select=c(aacode, orientation, Felsolff, Felsollf, Felsolrf, Felsolbf, Felpvff, Felpvlf, Felpvrf, Felpvbf, Fvwpvff, Fvwpvlf, Fvwpvrf, Fvwpvbf, felwtur))
dtaHb <- dtaHb %>% na_if(8) %>% na_if(9)
dtaHc <- subset(general_plus, select=c(aacode, aagph1011, tenure8x, rumorph, rucontxt, Imd1010))
dtaHd <- subset(interior, select=c(aacode, fincodor, fincosiz, fincowin, fincorof, fincorad))
dtaHd <- dtaHd %>% na_if(8) %>% na_if(9) %>% na_if(888) %>% na_if(999)
dtaHe <- subset(interview_plus, select=c(aacode, hhtype11, agehrpx, agepartx, pyngx, ageoldx, emphrpx, empprtx, sexhrp, lenres, ALLincx))
dtaHf <- subset(fuel_poverty_dataset_2011_for_ukda, select=c(aacode, fpindb, fpindf, fuelexpn, spahcost, wathcost, litecost, cookcost))
dtaHg <- subset(dimensions, select=c(aacode, FloorArea, Intwalar, Floorstr, efevepe, ebevepe, emeveht, eaeveht))
dtaHh <- subset(around, select=c(aacode, Fexdstep, Fcuexpos, Fblblock, Fblsitua, Farnatur, fcuosdw, fcuosmr))
dtaHi <- subset(shape, select=c(aacode, Fshattic))
dtaHj <- subset(physical_plus, select=c(aacode, dwtypenx))
dtaHk <- subset(firstimp_physical, select=c(aacode, fodconst, fodconac))
dtaHl <- subset(services, select=c(aacode, Finhsco, Finohtyp, Finintyp, Finchbcd, Finwotfu))
dtaHl <- dtaHl %>% na_if(77) %>% na_if(8) %>% na_if(9) %>% na_if(88) %>%  na_if(99) %>% na_if(888) %>% na_if(999) %>% na_if(777)

dtaModA <- dtaHa
dtaModA$type <- as.factor(tolower(gsub("\\W","_",dtaModA$type)))
dtaModA$Fexdb1no <- ifelse(is.na(dtaModA$Fexdb1no), 0, dtaModA$Fexdb1no)
dtaModA$Fexdb2no <- ifelse(is.na(dtaModA$Fexdb2no), 0, dtaModA$Fexdb2no)

dtaModA.b <- suppressMessages(
  dcast(subset(dtaModA, select=c(aacode, type, Fexdb1no)), aacode ~ type))
dtaModA.b$front.extension <- paste(
  "Bal",dtaModA.b$balconies,
  "ByM",dtaModA.b$bays__multi_storey,
  "ByS",dtaModA.b$bays__single_storey,
  "Con",dtaModA.b$conservatories,
  "RfX",dtaModA.b$dormers__roof_extension,
  "Drm",dtaModA.b$dormers__standard,
  "Prh",dtaModA.b$porches, sep="-")
dtaModA.b$front.extension <- as.factor(ifelse(
  dtaModA.b$front.extension=='Bal-0-ByM-0-ByS-0-Con-0-RfX-0-Drm-0-Prh-0', '-', dtaModA.b$front.extension))
dtaModA.b$front.conservatory <- ifelse(dtaModA.b$conservatories>0, TRUE, FALSE)
dtaModA.c <- suppressMessages(
  dcast(subset(dtaModA, select=c(aacode, type, Fexdb1ag)), aacode ~ type))
dtaModA.c$dormer.year <-
  rowMeans(dtaModA.c[,grep("aacode", colnames(dtaModA.c), invert = TRUE)], na.rm = T)
dtaModA.d <- suppressMessages(
  dcast(subset(dtaModA, select=c(aacode, type, Fexdb2no)), aacode ~ type))
dtaModA.d$rear.extension <- paste(
  "Bal",dtaModA.d$balconies,
  "ByM",dtaModA.d$bays__multi_storey,
  "ByS",dtaModA.d$bays__single_storey,
  "Con",dtaModA.d$conservatories,
  "RfX",dtaModA.d$dormers__roof_extension,
  "Drm",dtaModA.d$dormers__standard,
  "Prh",dtaModA.d$porches, sep="-")
dtaModA.d$rear.extension <- as.factor(ifelse(
  dtaModA.d$rear.extension=='Bal-0-ByM-0-ByS-0-Con-0-RfX-0-Drm-0-Prh-0', '-', dtaModA.d$rear.extension))
dtaModA.d$rear.conservatory <- ifelse(dtaModA.d$conservatories>0, TRUE, FALSE)
dtaModA.e <- suppressMessages(
  dcast(subset(dtaModA, select=c(aacode, type, Fexdb2ag)), aacode ~ type))
dtaModA.e$dormer.year <-
  rowMeans(dtaModA.e[,grep("aacode", colnames(dtaModA.e), invert = TRUE)], na.rm = T)
dtaModA <- join(subset(dtaModA.b, select=c(aacode, front.extension,front.conservatory)), subset(dtaModA.c, select=c(aacode, dormer.year)), by='aacode')
dtaModA <- join(dtaModA, subset(dtaModA.d, select=c(aacode, rear.extension, rear.conservatory)), by='aacode')
dtaModA <- join(dtaModA, subset(dtaModA.e, select=c(aacode, dormer.year)), by='aacode')
dtaModA$conservatory <- paste(ifelse(dtaModA$front.conservatory==TRUE,'F','-'), ifelse(dtaModA$rear.conservatory==TRUE,'B','-'), sep="")
dtaModA <- join(dtaModA, subset(dtaHd, select=c(aacode, fincodor, fincosiz, fincorad)), by='aacode')
dtaModA <- join(dtaModA, subset(tbl.Aux.TFA, select=c(aacode, Fshaddit, AdditionalArea)), by='aacode')
dtaModA$fincodor <- ifelse(dtaModA$fincodor=='Yes' | dtaModA$fincodor=='No', as.character(dtaModA$fincodor), '-')
dtaModA$fincorad <- ifelse(dtaModA$fincorad=='Yes' | dtaModA$fincorad=='No', as.character(dtaModA$fincorad), '-')
dtaModA$fincosiz <- ifelse(dtaModA$fincodor!='-' & is.na(dtaModA$fincosiz), dtaModA$AdditionalArea, dtaModA$fincosiz)
dtaModA <-
  dtaModA[,c('aacode','front.extension','dormer.year',
             'rear.extension','dormer.year','conservatory',
             'fincodor','fincosiz','fincorad')]
colnames(dtaModA) <-
  c('V001_HousingCode','V501_FrontExtension','V502_FrontExtensionAge',
    'V503_RearExtension','V504_RearExtensionAge','V505_Conservatory',
    'V506_ConservatoryDoor','V507_ConservatoryArea','V508_ConservatoryHeater')

dtaModB <- dtaHb
dtaModB <- join(dtaModB, dtaHl, by='aacode')
dtaModB$solar <- as.factor(paste(ifelse(dtaModB$Felsolff=='Yes','F','-'),ifelse(dtaModB$Felsollf=='Yes','L','-'),ifelse(dtaModB$Felsolrf=='Yes','R','-'),ifelse(dtaModB$Felsolbf=='Yes','B','-'),sep=""))
dtaModB$pv <- as.factor(paste(ifelse(dtaModB$Felpvff=='Yes','F','-'),ifelse(dtaModB$Felpvlf=='Yes','L','-'),ifelse(dtaModB$Felpvrf=='Yes','R','-'),ifelse(dtaModB$Felpvbf=='Yes','B','-'),sep=""))
dtaModB$pot <- as.factor(paste(ifelse(!is.na(dtaModB$Fvwpvff) & dtaModB$Fvwpvff=='Yes','F','-'),ifelse(!is.na(dtaModB$Fvwpvlf) & dtaModB$Fvwpvlf=='Yes','L','-'),ifelse(!is.na(dtaModB$Fvwpvrf) & dtaModB$Fvwpvrf=='Yes','R','-'),ifelse(!is.na(dtaModB$Fvwpvbf) & dtaModB$Fvwpvbf=='Yes','B','-'),sep=""))
dtaModB$Finchbcd <- ifelse(dtaModB$Finchbcd==9999|dtaModB$Finchbcd==7777|dtaModB$Finchbcd==8888, 108, dtaModB$Finchbcd)
dtaModB$Finwotfu <- as.integer(ifelse(is.na(dtaModB$Finwotfu), 0, dtaModB$Finwotfu))
dtaModB <-
  dtaModB[,c('aacode','Finhsco','Finohtyp','Finintyp','Finchbcd',
             'Finwotfu','solar','pv','pot')]
colnames(dtaModB) <-
  c('V001_HousingCode','V587_CarbonRisk','V588_AdditionalHeaters',
    'V589_LoftInsulationMaterial','V590_BoilerCode','V591_DHWCode',
    'V509_SolarPanels','V510_SolarPhotovoltaics',
    'V511_SolarPhotovoltaicsPotential')

dtaModC <- join(dtaHc, dtaHe, by='aacode')
dtaModC <- join(dtaModC, dtaHf, by='aacode')
dtaModC$hhtype11 <- ifelse(is.na(dtaModC$hhtype11),'-', as.character(dtaModC$hhtype11))
dtaModC$emphrpx <- ifelse(is.na(dtaModC$emphrpx),'-', as.character(dtaModC$emphrpx))
dtaModC$empprtx <- ifelse(is.na(dtaModC$empprtx),'-', as.character(dtaModC$empprtx))
dtaModC$sexhrp <- ifelse(is.na(dtaModC$sexhrp),'-', as.character(dtaModC$sexhrp))
levels(dtaModC$Imd1010) <- 1:10
dtaModC <-
  dtaModC[,c('aacode','tenure8x','rumorph','rucontxt','Imd1010','hhtype11',
             'agehrpx','agepartx','pyngx','ageoldx','emphrpx','empprtx',
             'sexhrp','lenres','ALLincx','fpindb','fpindf','fuelexpn',
             'spahcost','wathcost','litecost','cookcost')]
colnames(dtaModC) <-
  c('V001_HousingCode','V512_Tenure_8x','V513_Rurality','V514_Sparsity',
    'V515_IMD_Decile', 'V516_HouseholdComposition','V517_HRPAge',
    'V518_HRPpAge','V519_Youngest','V520_Oldest', 'V521_HRPEmployment',
    'V522_HRPpEmployment','V523_HRPSex','V524_ResidenceTime',
    'V525_IncomeCombined', 'V526_FuelPovertyBasic','V527_FuelPovertyFull',
    'V528_FuelExpenditure','V529_SpaceHeatCost','V530_DHWCost',
    'V531_LightingCost','V532_CookingCost')

dtaModD <- subset(dtaW, select=c(V001_HousingCode, H081_InternalFloorArea))
colnames(dtaModD) <- c('aacode','intTFA')
dtaModD <- join(dtaModD, subset(tbl.Aux.TFA, select=c(aacode, TFA)), by='aacode')
dtaModD <- join(dtaModD, dtaHg, by='aacode')
dtaModD <-
  dtaModD[,c('aacode','TFA','FloorArea','intTFA','Intwalar',
             'Floorstr','efevepe','ebevepe','emeveht','eaeveht')]
colnames(dtaModD) <-
  c('V001_HousingCode','V533_TFAest','V534_TFAsurvey','V535_IntTFAest',
    'V536_IntTFAsurvey','V537_StairsArea','V538_FrontEavesLenght',
    'V539_RearEavesLenght','V538_FrontEavesHeight','V539_RearEavesHeight')

dtaModE <- subset(tbl.Aux.Tenths, select=c(aacode,storeyx,Fdffloor,Finlopos))
dtaModE <- join(dtaModE, tbl.Aux.TFA, by='aacode')
dtaModE <- join(dtaModE, dtaHh, by='aacode')
dtaModE$doortyp <- as.factor(ifelse(dtaModE$typ.door.fro==dtaModE$typ.door.bck, dtaModE$typ.door.fro, 'mixed'))
dtaModE$glaz.ratio <- dtaModE$SrfcWind / dtaModE$SrfcWll_Area
dtaModE$fcuosdw <- as.factor(ifelse(is.na(dtaModE$fcuosdw), "-", as.character(dtaModE$fcuosdw)))
dtaModE$fcuosmr <- as.factor(ifelse(is.na(dtaModE$fcuosmr), "-", as.character(dtaModE$fcuosmr)))
levels(dtaModE$fcuosdw) <- c('-','-','>80%','20%-60%','<20%','60%-80%')
levels(dtaModE$fcuosmr) <- c('-','-','>80%','20%-60%','<20%','60%-80%')
dtaModE <-
  dtaModE[, c('aacode', 'storeyx', 'Fdffloor', 'Finlopos', 'AvHeight',
              'lot.w.BB', 'lot.d.BB', 'BsHeight', 'lot.w.GG', 'lot.d.GG',
              'GfHeight', 'lot.w.FF', 'lot.d.FF', 'FfHeight', 'lot.w.SF',
              'lot.d.SF', 'SfHeight', 'lot.w.HF', 'lot.d.HF', 'TfHeight',
              'lot.w.AT', 'lot.d.AT', 'AtHeight', 'SrfcWll_Area', 'glaz.ratio',
              'DblGlz', 'no.doors', 'doortyp', 'fcuosdw', 'fcuosmr',
              'Fexdstep', 'Fcuexpos', 'Fblblock', 'Fblsitua', 'Farnatur')]
dtaModE.ltr <- dtaModE
colnames(dtaModE) <-
  c('V001_HousingCode', 'V540_Storeys', 'V541_FlatFloor', 'V542_FloorPosition',
    'V543_AverageHeight', 'V544_BasementWidth', 'V545_BasementDepth',
    'V546_BasementHeight', 'V547_GroundFloorWidth', 'V548_GroundFloorDepth',
    'V549_GroundFloorHeight', 'V550_FirstFloorWidth', 'V551_FirstFloorDepth',
    'V552_FirstFloorHeight', 'V553_SecondFloorWidth', 'V554_SecondFloorDepth',
    'V555_SecondFloorHeight', 'V556_HigherFloorWidth', 'V557_HigherFloorDepth',
    'V558_HigherFloorHeight', 'V559_AtticRoomWidth', 'V560_AtticRoomDepth',
    'V561_AtticRoomHeight', 'V562_SurfaceWallArea', 'V563_GlazingRatio',
    'V564_DoubleGlazingRatio', 'V565_Doors', 'V566_DoorType',
    'V567_WindowShading', 'V568_RoofObstruction', 'V569_AccessSteps',
    'V570_Exposition', 'V571_Rows', 'V572_Street', 'V573_Urbanity')

dtaModF <- join(subset(dtaModE.ltr, select=c(aacode, storeyx, Fdffloor, Finlopos)), dtaHi, by='aacode')
dtaModF <- join(dtaModF, tbl.Aux.Shelter, by='aacode')
dtaModF <- join(dtaModF, dtaHj, by='aacode')
dtaModF <- join(dtaModF, dtaHk, by='aacode')
dtaModF <- join(dtaModF, subset(dtaHb, select=c(aacode,orientation)), by='aacode')
dtaModF <- join(dtaModF, subset(dtaHc, select=c(aacode, aagph1011)), by='aacode')
dtaModF$.isattic <- ifelse(dtaModF$Fshattic=='Attic only' | dtaModF$Fshattic=='Both', TRUE, FALSE)
dtaModF$.iscellar <- ifelse(dtaModF$Fshattic=='Basement only' | dtaModF$Fshattic=='Both', TRUE, FALSE)
dtaModF$floors <- ifelse(dtaModF$storeyx>4, 4, dtaModF$storeyx)
dtaModF$flatlevel <- ifelse(dtaModF$Finlopos=='Top Floor Flat', dtaModF$floors,
                     ifelse(dtaModF$Finlopos=='Mid Floor Flat', dtaModF$floors - 1, 1))
dtaModF <-
  dtaModF[, c('aacode', 'floors', 'flatlevel', '.isattic', '.iscellar',
              'sheltered.f', 'sheltered.b', 'sheltered.l', 'sheltered.r',
              'sheltered','dwtypenx','fodconst','orientation','aagph1011')]
colnames(dtaModF) <-
  c('V001_HousingCode', 'V574_CuboidFloors', 'V575_CuboidLevel',
    'V576_CuboidAttic', 'V577_CuboidBasement', 'V578_CuboidAttachmentFront',
    'V579_CuboidAttachmentRear', 'V580_CuboidAttachmentLeft',
    'V581_CuboidAttachmentRight', 'V582_CuboidAttachments',
    'V583_CuboidType', 'V584_CuboidEpoch','V585_CuboidOrientation',
    'V586_HouseholdWeight')

lst.H <- as_tibble(
  join_all(lapply(ls()[grep("dtaMod[A-F]$",ls())], get), by='V001_HousingCode'))
rm(list=ls()[grep("dta\\w+",ls())])



# M. [subset]-------------------------------------------------------------------
dtaMa <- subset(general_plus, select=c(aacode, tenure4x, GorEHS))
dtaMb <- subset(physical_plus, select=c(aacode, dwtypenx, dwage6x, floor5x, attic, fuelx, dblglaz2, loftins4, heat4x, heat7x, watersys, boiler))
dtaMc <- subset(interview_plus, select=c(aacode, agehrp4x, sexhrp, hhinc5x, hhcompx, emphrpx))
levels(dtaMc$agehrp4x) <- gsub("^ *", "", levels(dtaMc$agehrp4x))

dtaM <- join(dtaMa, dtaMb, by='aacode')
dtaM <- join(dtaM, dtaMc, by='aacode')
colnames(dtaM) <- tolower(colnames(dtaM))
lst.M <- as_tibble(dtaM)
rm(list=ls()[grep("dta\\w+",ls())])

