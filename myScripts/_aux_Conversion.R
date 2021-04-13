#' -----------------------------------------------------------------------------
#' EHS Converter                                 {Auxiliary / Data Conversion}
#'
#' This file contains auxiliary functions to process survey data.
#'
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

fnGetFactorHubTypo <- function(varToFactor, dtaCom){

  if(varToFactor=='V583_CuboidType'){
    varFact <- as.character(dtaCom$V583_CuboidType)
    dtaCom$Hub <-
      as.factor(sapply(varFact, switch,
                       'bungalow' = 'bungalow',
                       'converted flat' = 'apartments',
                       'detached' = 'detached',
                       'end terrace' = 'terraced',
                       'mid terrace' = 'terraced',
                       'purpose built flat, high rise' = 'apartments',
                       'purpose built flat, low rise' = 'apartments',
                       'semi detached' = 'semi-detached'))
  }else if(varToFactor=='V584_CuboidEpoch'){
    varFact <- as.character(dtaCom$V584_CuboidEpoch)
    dtaCom$Hub <- as.factor(sapply(varFact, switch,
                                       'Pre 1850' = 'a_pre-1919',
                                       '1850-1899' = 'a_pre-1919',
                                       '1900-1918' = 'a_pre-1919',
                                       '1919-1944' = 'b_interwar',
                                       '1945-1964' = 'c_postwar',
                                       '1965-1974' = 'd_industrial',
                                       '1975-1980' = 'd_industrial',
                                       '1981-1990' = 'e_modern',
                                       '1991-1995' = 'e_modern',
                                       '1996-2002' = 'e_modern',
                                       'Post 2002' = 'f_post-2002'))
    levels(dtaCom$Hub) <- gsub("\\w_","",levels(dtaCom$Hub))
  }else if(varToFactor=='D082_MainHeatingSystemType'){
    varFact <- as.integer(dtaCom$D082_MainHeatingSystemType)
    dtaCom$Hub <- as.factor(sapply(varFact, switch,
                                       '1' = "Gas",
                                       '2' = "Gas",
                                       '3' = "Gas",
                                       '4' = "Oil",
                                       '5' = "Solid",
                                       '6' = "Electric",
                                       '7' = "Electric",
                                       '8' = "Electric",
                                       '9' = "Air",
                                       '10' = "Air",
                                       '11' = "District",
                                       '12' = "District",
                                       '13' = "HeatPump and Bio",
                                       '14' = "HeatPump and Bio",
                                       '15' = "HeatPump and Bio"))
  }else if(varToFactor=='D097_DHWSystemType'){
    varFact <- as.integer(dtaCom$D097_DHWSystemType)
    dtaCom$Hub <- as.factor(sapply(varFact, switch,
                                       '1' = "boiler gas",
                                       '2' = "boiler gas",
                                       '3' = "boiler gas",
                                       '4' = "boiler gas",
                                       '5' = "boiler oil",
                                       '6' = "boiler solid",
                                       '7' = "boiler biomass",
                                       '8' = "boiler electric",
                                       '9' = "boiler electric",
                                       '10' = "boiler DH",
                                       '11' = "boiler DH"))
  }else{
    print("These aren't the droids you're looking for...")
  }

  dtaReFactored <- dtaCom$Hub
  return(dtaReFactored)
}

fnGetHeatingCode <- function(idCode, dtaSH, dtaDH){
  # Heating Configuration Code ---> MSHxDWHx

  dtaMSH <- subset(dtaSH, V001_HousingCode==idCode,
                   select = c(D082_MainHeatingSystemType))
  dtaDHW <- subset(dtaDH, V001_HousingCode==idCode,
                   select = c(D097_DHWSystemType))

  varMSH <- as.integer(dtaMSH$D082_MainHeatingSystemType)
  varDHW <- as.integer(dtaDHW$D097_DHWSystemType)
  varCode <- paste0("MSH", varMSH, "DHW", varDHW)

  return(varCode)
}
