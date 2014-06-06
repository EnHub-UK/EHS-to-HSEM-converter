#' -----------------------------------------------------------------------------
#' EHS Converter                               {Deeper Analysis of households}
#'
#' @file `C__households.R` explores individual/household values
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
.proj.Mode <- 'C_households'
source('myScripts/__setup_Project.R', verbose = FALSE)

#.. project parameters ----
var.EHSversion <- "UKDA-7386-stata9"     # definition/selection of stata folder
source('myScripts/_aux_GetData.R')       # data collection (~ 10 sec)



# [1] Load datasets ------------------------------------------------------------

#.. check/locate variables and datasets
tblVariableNames
findDocStrings("DV")
findDocStrings("AGE")
findDocStrings("Econ")

#.. load "people" dataset
dtaA <- people


# [2] Process datasets ---------------------------------------------------------

fnGetHhdData <- function(dtaId, dtaInfo){

  dtaInfoHhd <-
    subset(dtaInfo, aacode==dtaId,
           select=c(PERSNO,HRP,Sex,AGE,xMarSta,NumNonR2,
                    EconFull,EconPart,EconRtrd,EconStdt,HighEd1))
  colnames(dtaInfoHhd) <-
    c('no.','HRP','sex','age','marital','non_hhd_core',
      'WorkingFull','WorkingPart','Retired','Student','HighestEducation')

  dtaRelsHhd <- as.data.frame(t(
    subset(dtaInfo, aacode==dtaId,
           select=c(tr01, tr02, tr03, tr04, tr05, tr06, tr07, tr08,
                    tr09, tr010, tr011, tr012, tr013, tr014, tr015, tr016))))
  rownames(dtaRelsHhd) <- NULL

  if(max(dtaInfoHhd$no.)<2){
    colnames(dtaRelsHhd) <- c('A')
    dtaRelsHhd$A <-
      ifelse(dtaRelsHhd$A=='does not apply','-', as.character(dtaRelsHhd$A))
    dtaRelsHhd$check <- ifelse(dtaRelsHhd$A=='-', FALSE, TRUE)
    dtaRelsHhd$check[1] <- TRUE
    dtaRelsHhd <- subset(dtaRelsHhd, check==TRUE, select=c(A))
    colnames(dtaRelsHhd) <- c('no-rel')
  }else{
    colnames(dtaRelsHhd) <- c('A','B')
    dtaRelsHhd$A <-
      ifelse(dtaRelsHhd$A=='does not apply','-',as.character(dtaRelsHhd$A))
    dtaRelsHhd$B <-
      ifelse(dtaRelsHhd$B=='does not apply','-',as.character(dtaRelsHhd$B))
    dtaRelsHhd$check <-
      ifelse(dtaRelsHhd$A=='-' & dtaRelsHhd$B=='-', FALSE, TRUE)
    dtaRelsHhd <- subset(dtaRelsHhd, check==TRUE, select=c(A,B))
    colnames(dtaRelsHhd) <- c('rel-to-B','rel-to-A')
  }

  lstHhd <- list(dtaInfoHhd, dtaRelsHhd)
  names(lstHhd) <- c('Info','Relationship')
  return(lstHhd)
}

lstHouseholds <- pblapply(unique(dtaA$aacode), fnGetHhdData, dtaA)
names(lstHouseholds) <- unique(dtaA$aacode)


# [3] Quick Overview -----------------------------------------------------------

lstHouseholds[[99]]

#.. compare by situation and age
ddply(dtaA, .(xMarSta, Sex), summarise,
      mean = round(mean(AGE), 2),
      sd = round(sd(AGE), 2))

p1 <- ggplot(dtaA, aes(x=xMarSta, y=AGE, fill=Sex)) +
  geom_boxplot() +
  guides(fill=FALSE) + coord_flip() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Spectral")
fnExportFigure(p1,
               "/HHds_Marital", 12, 12)

#.. compare by education and sex
ddply(dtaA, .(HighEd1, Sex), summarise,
      mean = round(mean(AGE), 2),
      sd = round(sd(AGE), 2))

p2 <- ggplot(dtaA, aes(x=HighEd1, y=AGE, fill=Sex)) +
  geom_boxplot() +
  guides(fill=FALSE) + coord_flip() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Spectral")
fnExportFigure(p2,
               "/HHds_Qual", 12, 12)
fnExportTable(table(dtaA$PERSNO, dtaA$Sex),
              "/HHds_HRP")


# [4] Export list of households ------------------------------------------------

saveRDS(lstHouseholds, paste0(path.EHS.datamd, file="/lstHhds.rds"))
# lstHouseholds <- readRDS(paste0(path.EHS.datamd, file="/lstHhds.rds"))
