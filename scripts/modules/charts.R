#' -----------------------------------------------------------------------------
#' EHS Converter                                {Auxiliary / Charts Functions}
#'
#' This contains auxiliary functions to plot EHS data.
#' (For the initial analysis stage.)
#'
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords stock, statistics, survey, parser
#' @repository github.com/EnHub-UK/EHS-to-HSEM-converter
#'

# Export Figures ---------------------------------------------------------------

export_figure <- function(obj_fig, lbl_fig, w_fig, h_fig, path_fig=path_report){
  path_fig <- paste0(path_fig, lbl_fig, ".pdf")
  pdf(path_fig, width = w_fig, height = h_fig)
  suppressMessages(print(obj_fig))
  invisible(dev.off())
}

export_table <- function(obj.tbl, lbl.tbl, path.tbl=path_report){
  path.tbl <- paste0(path.tbl, lbl.tbl, ".csv")
  write.csv(obj.tbl, path.tbl)
}


# Charts Generation ------------------------------------------------------------

draw_histogram <- function(df, varA, varB, varC, bands=10,
                          weight='V002_DwellingWeight'){

  df <- subset(df, select=c(weight, varA, varB, varC))
  colnames(df) <- c('weight', 'varA', 'varB', 'varC')

  g <- ggplot(df, aes(varA, weight=weight, fill=as.factor(varB))) +
    geom_vline(xintercept = mean(df$varA),
               linetype='dashed', colour='grey') +
    geom_histogram(color="black", fill="darkgrey", binwidth=bands) +
    facet_grid(varB ~ varC,scales = "free_y") +
    xlab(varA) +
    ylab(varB) +
    scale_y_continuous(labels=comma) +
    guides(fill=guide_legend(title=NULL,reverse=T,label.position = "right",
      keywidth = 0.5, keyheight = 2))
  g
}

draw_scatter <- function(df, varA, varB, varC="", varD="", split=TRUE){

  df <- subset(df, select=c(varA, varB, varC, varD))
  colnames(df) <- c('varA', 'varB','varC', 'varD')

  cbbPalette <- length(levels(df$varC))
  cbbPalette <- wes_palette(cbbPalette, name="FantasticFox1", type="continuous")

  p <- ggplot(df, aes(varA, varB, color= varC)) +
    geom_point() + geom_smooth(method="lm", se = F, alpha=0.4)

  p <- p + xlab("") + ylab("") +
    guides(fill=guide_legend(title=NULL, reverse=F,
                             label.position = "right",
                             keywidth = 0.5, keyheight = 2)) +
    scale_color_manual(values=cbbPalette) +
    theme_bw() + coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(fill = "variable")

  if(isTRUE(split)){
    g <- p + facet_wrap(~ varD)
  }else{
    g <- p
  }
  g
}

draw_bubble <- function(df, weight, varA, varB,
                       varC="", mode.plot="stack"){

  df <- subset(df, select=c(weight, varA, varB, varC))
  colnames(df) <- c('weight', 'varA', 'varB','varC')

  cbbPalette <- length(levels(df$varC))
  cbbPalette <- wes_palette(cbbPalette, name="FantasticFox1", type="continuous")

  p <- ggplot(df, aes(varA, varB, size=weight, color= varC)) +
    geom_point() + facet_wrap(~ varC, scales = "free")

  p <- p + xlab("") + ylab("") +
    guides(fill=guide_legend(title=NULL, reverse=F,
       label.position = "right", keywidth = 0.5, keyheight = 2)) +
    scale_color_manual(values=cbbPalette) +
    theme_bw() + coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(fill = "variable")

 p
}

draw_bars <- function(df, weight, varA, varB,
                     varC="", mode.plot="stack"){

  if(varC==""){
    df <- subset(df, select=c(weight, varA, varB))
    colnames(df) <- c('weight', 'varA', 'varB')
  }else{
    df <- subset(df, select=c(weight, varA, varB, varC))
    colnames(df) <- c('weight', 'varA', 'varB','varC')
  }

  cbbPalette <- length(levels(df$varB))
  cbbPalette <- wes_palette(cbbPalette, name="FantasticFox1", type="continuous")

  if(varC==""){
    p <- ggplot(df, aes(x=varA, fill=varB, weight=weight)) +
      geom_bar(position=mode.plot)
  }else{
    p <- ggplot(df, aes(x=varA, fill=varB, weight=weight)) +
      geom_bar(position=mode.plot) +
      facet_wrap(~ varC)
  }

  p <- p + xlab("") +
    ylab("") +
    guides(fill=guide_legend(title=NULL, reverse=F,
                label.position = "right", keywidth = 0.5, keyheight = 2)) +
    scale_fill_manual(values=cbbPalette) +
    theme_bw() +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(fill = "variable")

  p
}

draw_notchs <- function(df, weight, varA, varB,
                       varC="", varD="", varE="", axLog=FALSE){
  
  if(varE==""&varD==""&varC==""){
    df <- subset(df, select=c(weight,varA,varB))
    colnames(df) <- c('weight','varA', 'varB')
    cbbPalette <- length(levels(as.factor(df$varB)))
    loc <- "EDC"
  }else if(varE==""&varD==""){
    df <- subset(df, select=c(weight,varA,varB,varC))
    colnames(df) <- c('weight','varA', 'varB', 'varC')
    cbbPalette <- length(levels(as.factor(df$varB)))
    loc <- "DC"
  }else if(varE==""){
    df <- subset(df, select=c(weight,varA,varB,varC,varD))
    colnames(df) <- c('weight','varA', 'varB','varC', 'varD')
    cbbPalette <- length(levels(as.factor(df$varC)))
    loc <- "C"
  }else{
    df <- subset(df, select=c(weight,varA,varB,varC,varD,varE))
    colnames(df) <- c('weight','varA', 'varB','varC', 'varD', 'varE')
    cbbPalette <- length(levels(as.factor(df$varC)))
    loc <- "B"
  }

  df$varC <- df$varC %>% as.factor()
  df <- df %>% tibble()
  
  cbbPalette <- wes_palette(cbbPalette, name="FantasticFox1", type="continuous")

  if(varE==""&varD==""&varC==""){
    p <- ggplot(df, aes(x=varA, y=weight, fill=as.factor(varB)))
  }else if(varE==""&varD==""){
    p <- ggplot(df, aes(x=varA, y=weight, fill=as.factor(varB))) +
         facet_grid( ~ varC)
  }else if(varE==""){
    p <- ggplot(df, aes(x=varA, y=varB * weight,fill=as.factor(varC))) +
         facet_grid( ~ varD)
  }else{
    p <- ggplot(df, aes(x=varA, y=varB * weight, fill=as.factor(varC))) +
         facet_grid(varD ~ varE)
  }

  p <- p + geom_boxplot() + xlab("") + ylab("") +
    guides(fill=guide_legend(title=NULL, reverse=F,
                label.position = "right", keywidth = 0.5, keyheight = 2)) +
    scale_fill_manual(values=cbbPalette) + theme_bw() + coord_flip() +
    labs(fill = "variable")

  if(isTRUE(axLog)){
    p <- p + scale_y_log10()
  }

  suppressWarnings(suppressMessages(p))
}

draw_mosaic <- function(df, varA, varB, varC="", var.style="index"){

  if(varC==""){
    varFml <- paste("~",varA,"+",varB)
    df <- as.data.frame(svytable(as.formula(varFml), df))
    colnames(df) <- c('varA', 'varB', 'Freq')
  }else{
    varFml <- paste("~",varA,"+",varB,"+",varC)
    df <- as.data.frame(svytable(as.formula(varFml), df))
    colnames(df) <- c('varA', 'varB', 'varC', 'Freq')
  }

  df <- subset(df, Freq>0)

  if(varC==""){
    treemap(df,
            index=c("varA", "varB"),
            vSize="Freq",
            vColor="Freq",
            type=var.style,
            fontsize.labels=c(12, 8),
            align.labels=list(c("left", "center"),
                              c("left", "top")),
            lowerbound.cex.labels=1,
            format.legend = list(scientific = FALSE, big.mark = " "))
  }else{
    treemap(df,
            index=c("varA", "varC", "varB"),
            vSize="Freq",
            vColor="Freq",
            type=var.style,
            fontsize.labels=c(12, 8, 6),
            align.labels=list(c("left", "center"),
                              c("left", "top"),
                              c("right", "bottom")),
            lowerbound.cex.labels=1,
            format.legend = list(scientific = FALSE, big.mark = " "))
  }
}

draw_four_fold <- function(df, varA, varB){

  varFml <- paste("~",varA,"+",varB)
  df <- svytable(as.formula(varFml), df)
  df <- df[,c(3:4)]
  fourfoldplot(df)
}

draw_pyramid <- function(df, weight, varA, varB){

  varDensPlot=36
  df <- subset(df, select=c(varA, varB, weight))

  pyramid(df, Lcol="darkgreen", Rcol="darkred", Ldens=varDensPlot,
          Rdens=varDensPlot, GL=FALSE, Llab = varA, Rlab = varB, Clab = weight,
          Cgap = 0.2, Cstep = 1)
}

draw_polar <- function(df, weight, varA, varB, varC, varD=''){

  if(varD==""){
    df <- subset(df, select=c(weight, varA, varB, varC))
    colnames(df) <- c('weight', 'varA', 'varB', 'varC')
  }else{
    df <- subset(df, select=c(weight, varA, varB, varC, varD))
    colnames(df) <- c('weight', 'varA', 'varB','varC', 'varD')
  }

  cbbPalette <- length(levels(df$varB))
  cbbPalette <- wes_palette(cbbPalette, name="FantasticFox1", type="continuous")

  if(varD==""){
    p <- ggplot(df, aes(varA, weight, fill = varB)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ varC)
  }else{
    p <- ggplot(df, aes(varA, weight, fill = varB)) +
      geom_bar(stat = "identity") +
      facet_grid(varD ~ varC)
  }

  g <- p + scale_fill_manual(values=cbbPalette) +
    coord_polar(start = -0.4, clip = T) +
    theme_bw()
  g
}


# :: Draw Fuel Poverty region ---------------

draw_fuel_poverty <- function(df, varExp='fuelexpn', varInc='fpbasinc',
                           varCod='aacode', varWgt='weighthshld',
                           notesX='Annual basic income (£) excluding benefits'){

  df <- as_tibble(df[,c(varCod,varWgt,varExp,varInc)])
  colnames(df) <- c('code','weight','expenditure','income')

  df$.fuelPov <- df$expenditure / df$income
  df$.fuelPovInd <- ifelse(df$.fuelPov <= 0.1, F, T)

  extra_ax <- 100
  extra_ay <- 10000
  max_x <- max(df$income)
  max_y <- max(df$expenditure)
  d = data.frame(x=c(-extra_ax/0.1,max_x + extra_ay,
                     max_x + extra_ay, (8000+extra_ax)/0.1),
                 y=c(-extra_ax*0.1,0,max_y, max_y),
                 t=c('a', 'a', 'a', 'a'), r=c(1,2,3, 4))
  how <- as.data.frame(as.matrix(table(df$.fuelPovInd)))
  how <- round(100 * ( how[1,] / sum(how) ), 0)

  qplot(income, expenditure, data = df) +
    geom_polygon(data=d, mapping=aes(x=x, y=y), fill="gold", alpha =0.15) +
    geom_point(aes(colour = .fuelPovInd, alpha = .fuelPov),
               size = I(5), alpha = 1/20) +
    geom_smooth(aes(group=.fuelPovInd,fill = .fuelPovInd), method="lm") +
    geom_abline(intercept = 0, slope = 0.1, colour = "white", size = 2) +
    annotate("text", x = 155000, y = 7000,
             label = paste("not in fuel poverty : ",how,"%"),
             color = "#D55E00", size=7) +
    annotate("text", x = 190000, y = 6500,
             label = "( > 10% of income in fuel expenses)",
             color = "#D55E00", size=6) +
    coord_fixed(40) +
    xlab(notesX) +
    ylab("Total fuel cost (£)") +
    ggtitle("Considering occupied dwellings") +
    scale_y_continuous(labels=comma) +
    scale_x_continuous(labels=comma) +
    theme(legend.position="none")

}
