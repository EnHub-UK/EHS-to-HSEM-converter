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

obtain_thickness <- function(id, df, d_aux=tbl.AuxC){
  i_typ <- df$wall.typ[df$aacode==id]
  i_age <- df$sap.no[df$aacode==id]
  i_age <- ifelse(i_age >9,9,i_age)
  i_thk <- d_aux[i_typ, i_age]
  return(i_thk)
}

obtain_addition <- function(id, df){
  df_res <-
    df[df$aacode==id,grep("\\wFa", colnames(df))]
  df_res <-
    max(df_res[df_res>0]) - min(df_res[df_res>0])
  return(df_res)
}

obtain_heat_par <- function(id, df, var='Heating.System', df_aux=tbl.AuxE){
  i_typ <- as.integer(as.character(df$Finchbcd[df$aacode==id]))
  i_thk <- df_aux[df_aux$Code==i_typ, var]
  i_thk <- as.integer(as.character(i_thk))
  return(i_thk)
}

obtain_heat_efficiency <- function(id, df, df_aux=tbl.AuxG){

  i_typ <- as.character(subset(df, aacode==id, select=c(DHW.typ)))

  i_eff <-
    switch(i_typ,
           "Gas standard"=df_aux$Efficiency[3],
           "Gas - combi (storage)"=df_aux$Efficiency[18],
           "Gas - combi (instantaneous)"=df_aux$Efficiency[7],
           "Gas back boiler"=df_aux$Efficiency[2],
           "Solid boiler (house coal/anthracite)"=df_aux$Efficiency[12],
           "Oil standard"=df_aux$Efficiency[20],
           "Electric boiler"=df_aux$Efficiency[6],
           "Other electric"=df_aux$Efficiency[9],
           "Community heating without CHP"=df_aux$Efficiency[24],
           "Community heating with CHP"=df_aux$Efficiency[25],
           "Biomass boiler"=df_aux$Efficiency[23])
  return(i_eff)
}

convert_fac_to_int <- function(s){
  rgx <- "Not Applicable|Question Not Applicable|Don't Know|unknown"
  t <- as.integer(gsub(rgx, NA, as.character(s), ignore.case = T))
  return(t)
}

obtain_age_bands <- function(df, df_ref=tbl.AuxA){

  rgx <- "Not Applicable|Question Not Applicable|Don't Know"
  df <- as.character(df)
  df <- gsub(rgx, NA, df, ignore.case = T)
  df <- as.integer(df)

  df_band <- subset(df_ref, SAP.Band!="-", select=c(SAP.Band, SAP.Period))
  df_band <- rbind(df_band, c(11,"2007 - 2022"))
  df_band$SAP.Period <- gsub("Before","0 -",df_band$SAP.Period)
  df_band$max <- as.integer(gsub("\\d* - ","", df_band$SAP.Period))
  df_res <- sapply(df, function(x)
    df_band$SAP.Band[which(df_band$max>=x)[1]])
  df_res <- as.integer(df_res)

  return(df_res)
}

load_auxiliary_table <- function(lbl_data, p_aux = "data/auxiliary_tables/", row._lbl = FALSE){
  if (row._lbl == FALSE) {
    tblAux <- read.csv(paste0(p_aux, lbl_data, ".csv"))
  } else {
    tblAux <- read.csv(paste0(p_aux, lbl_data, ".csv"), row.names = 1)
  }
  return(tblAux)
}

obtain_typology_fac <- function(i_factor, dfc){

  if(i_factor=='V583_CuboidType'){
    varFact <- as.character(dfc$V583_CuboidType)
    dfc$Hub <-
      as.factor(sapply(varFact, switch,
                       'bungalow' = 'bungalow',
                       'converted flat' = 'apartments',
                       'detached' = 'detached',
                       'end terrace' = 'terraced',
                       'mid terrace' = 'terraced',
                       'purpose built flat, high rise' = 'apartments',
                       'purpose built flat, low rise' = 'apartments',
                       'semi detached' = 'semi-detached'))
  }else if(i_factor=='V584_CuboidEpoch'){
    varFact <- as.character(dfc$V584_CuboidEpoch)
    dfc$Hub <- as.factor(sapply(varFact, switch,
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
    levels(dfc$Hub) <- gsub("\\w_","",levels(dfc$Hub))
  }else if(i_factor=='D082_MainHeatingSystemType'){
    varFact <- as.integer(dfc$D082_MainHeatingSystemType)
    dfc$Hub <- as.factor(sapply(varFact, switch,
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
  }else if(i_factor=='D097_DHWSystemType'){
    varFact <- as.integer(dfc$D097_DHWSystemType)
    dfc$Hub <- as.factor(sapply(varFact, switch,
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

  df_res <- dfc$Hub
  return(df_res)
}

obtain_heating_codeid <- function(idCode, dtaSH, dtaDH){
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

extract_household_info <- function(id, df){

  lbl_people <- sprintf("%03d", 1:16)

  df_hhd <- df %>%
    filter(aacode == id) %>%
    select(
      persno, hrp, sex, age, xmarsta, numnonr2, econfull,
      econpart, econrtrd, econstdt
    ) %>%
    unique() %>%
    arrange(persno)
  # highed1

  colnames(df_hhd) <-
    c(
      "p_in", "hrp", "sex", "age", "marital", "non_hhd_core",
      "working_full", "working_part", "retired", "student"
    )
  #'highest_education'

  df_rel <- df %>%
    filter(aacode == id) %>%
    select(matches("^persno|^r0", ignore.case = T)) %>%
    unique() %>%
    group_by(persno) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(matches("^r0", ignore.case = T)) %>%
    t() %>%
    data.frame()
  rownames(df_rel) <- NULL

  if(max(df_hhd$p_in)<2){

    df_rel <- data.frame(df_rel[,1])
    colnames(df_rel) <- c('value')
    df_rel$value <- as.character(df_rel$value)
    df_rel$value <- ifelse(is.na(df_rel$value), "-", df_rel$value)
    df_rel$value <- gsub("does not apply|^NA$","-",df_rel$value)
    df_rel$check <- ifelse(df_rel$value=='-', FALSE, TRUE)
    df_rel$value[1] <- "001"
    df_rel$check[1] <- TRUE
    df_rel <- df_rel %>% filter(check==TRUE) %>%
      mutate(other='-', relationship='(self)') %>%
      select(value,other,relationship)
    colnames(df_rel) <- c('person','other','relationship')

  }else{

    n_people <- length(colnames(df_rel))
    colnames(df_rel) <- lbl_people[0:n_people]
    df_rel <- df_rel[0:n_people, 0:n_people]
    df_rel$p_in <- lbl_people[0:n_people]
    df_rel <- melt(df_rel, id.vars = 'p_in')
    df_rel$value <- as.character(df_rel$value)
    df_rel$value <- ifelse(is.na(df_rel$value), "-", df_rel$value)
    df_rel$value <- gsub("does not apply","-",df_rel$value)
    df_rel$check <- ifelse(df_rel$value=='-', FALSE, TRUE)
    df_rel <- df_rel %>% filter(check==TRUE) %>%
      select(p_in,variable,value) %>% arrange(p_in)

    if(dim(df_rel)[1]>0){
      colnames(df_rel) <- c('person','other','relationship')

      tbl_unique <- data.frame(t(combn(c(unique(
        c(as.character(df_rel$person),as.character(df_rel$other)))), 2)))
      colnames(tbl_unique) <- c('person','other')

      df_rel <- inner_join(tbl_unique, df_rel, by=c("person","other"))
      # recast(df_rel, p ~ with_p, fill="-")

    }else{

      df_rel <- data.frame(t(combn(as.character(lbl_people[1:n_people]), 2)))
      colnames(df_rel) <- c('person','other')
      df_rel$relationship <- 'un-defined'

    }

  }

  l_rel <- list(df_hhd, df_rel)
  names(l_rel) <- c('Info','Relationship')

  return(l_rel)
}


# additional utilities to process missing values -------------------------------

na_if_num <- function(df, k){
  df <- df %>% mutate(across(where(is.numeric), ~na_if(., k)))
  return(df)
}

na_if_int <- function(df, k){
  df <- df %>% mutate(across(where(is.integer), ~na_if(., k)))
  return(df)
}

na_if_fac <- function(df){
  suppressWarnings(
    df <- df %>% 
      mutate(across(where(is.factor), ~as.integer(as.character(.)))))
  return(df)
}

log_fac <- function(df){
  dd <- data.frame(label=df)
  levels(dd$label) <- gsub('No','FALSE',levels(dd$label))
  levels(dd$label) <- gsub('Yes','TRUE',levels(dd$label))
  df <- as.logical(dd$label)
  return(df)
}

chr_fac <- function(x){
  x <- gsub('77|88|99|-','',x)
  x <- as.factor(x)
  levels(x)[levels(x)==''] <- NA
  return(x)
}
