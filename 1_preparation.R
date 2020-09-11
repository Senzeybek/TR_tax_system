# Packages ------------------------------------------------------------------------------------
lapply(c("rio", "dplyr", "stringr",'readxl','readr',"WriteXLS","tidyr",'forecast','sp','grid','scales','directlabels','tools'), library, character.only = TRUE) # load packages

darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

