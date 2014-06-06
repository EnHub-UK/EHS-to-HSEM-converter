#' -----------------------------------------------------------------------------
#' EHS Converter                                 {Auxiliary / Data Conversion}
#'
#' @file `_aux_Conversion.R` contains auxiliary functions to process
#'       survey data
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'

fnGetThick <- function(id, dtaToMatch, dtaAux=tbl.AuxC){
  val.typ <- dtaToMatch$wall.typ[dtaToMatch$aacode==id]
  val.age <- dtaToMatch$sap.no[dtaToMatch$aacode==id]
  val.age <- ifelse(val.age >9,9,val.age)
  val.thk <- dtaAux[val.typ, val.age]
  return(val.thk)
}

fnGetAddition <- function(id, dtaToMatch){
  dtaMatch <-
    dtaToMatch[dtaToMatch$aacode==id,grep("\\wFa", colnames(dtaToMatch))]
  dtaMatch <-
    max(dtaMatch[dtaMatch>0]) - min(dtaMatch[dtaMatch>0])
  return(dtaMatch)
}

fnGetHeat <- function(id, dtaToMatch, var='Heating.System', dtaAux=tbl.AuxE){
  val.typ <- dtaToMatch$Finchbcd[dtaToMatch$aacode==id]
  val.thk <- dtaAux[dtaAux$Code==val.typ, var]
  val.thk <- as.integer(as.character(val.thk))
  return(val.thk)
}

fnGetEff <- function(id, dtaToMatch, dtaAux=tbl.AuxG){

  val.typ <- as.character(subset(dtaToMatch, aacode==id, select=c(DHW.typ)))

  val.eff <-
    switch(val.typ,
           "Gas standard"=dtaAux$Efficiency[3],
           "Gas - combi (storage)"=dtaAux$Efficiency[18],
           "Gas - combi (instantaneous)"=dtaAux$Efficiency[7],
           "Gas back boiler"=dtaAux$Efficiency[2],
           "Solid boiler (house coal/anthracite)"=dtaAux$Efficiency[12],
           "Oil standard"=dtaAux$Efficiency[20],
           "Electric boiler"=dtaAux$Efficiency[6],
           "Other electric"=dtaAux$Efficiency[9],
           "Community heating without CHP"=dtaAux$Efficiency[24],
           "Community heating with CHP"=dtaAux$Efficiency[25],
           "Biomass boiler"=dtaAux$Efficiency[23])
  return(val.eff)
}

fnGetAgeBands <- function(dtaTo, tblRef=tbl.AuxA){

  tblBand <- subset(tblRef, SAP.Band!="-", select=c(SAP.Band, SAP.Period))
  tblBand <- rbind(tblBand, c(11,"2007 - 2011"))
  tblBand$SAP.Period <- gsub("Before","0 -",tblBand$SAP.Period)
  tblBand$max <- as.integer(gsub("\\d* - ","", tblBand$SAP.Period))
  tblInterval <- sapply(dtaTo, function(x)
    tblBand$SAP.Band[which(tblBand$max>=x)[1]])
  tblInterval <- as.integer(tblInterval)

  # (old method) w/dtaTo <- dtaModB$ehs.no
  # tblInterval <- ceiling(dtaTo - dtaTo * tblRef$EHS2SAP[dtaTo])

  return(tblInterval)
}

fnReadAux <- function(tbl.name, pathAux="myData/AuxiliaryData/", row.lbl=F){
  if(row.lbl==FALSE){
    tblAux <- read.csv(paste0(pathAux,tbl.name,".csv"))
  }else{
    tblAux <- read.csv(paste0(pathAux,tbl.name,".csv"), row.names = 1)
  }
  return(tblAux)
}
