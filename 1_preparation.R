# Packages ------------------------------------------------------------------------------------
lapply(c("rio", "dplyr", "stringr",'readxl','readr',"WriteXLS","tidyr",'forecast','sp','grid','scales','directlabels','tools'), library, character.only = TRUE) # load packages



# data import ----
odd_2016 <- import("r_input/odd_kpmg_2016.xls",sheet="Sheet1",skip=2)  %>% mutate(year=2016)
odd_2017 <- import("r_input/odd_kpmg_2017.xls",sheet="Sheet1",skip=2) %>% mutate(year=2017) 
odd_2018 <- import("r_input/odd_kpmg_2018.xls",sheet="Sheet1",skip=2) %>% mutate(year=2018)
odd_2019 <- import("r_input/odd_kpmg_2019.xls",sheet="Sheet1",skip=2) %>% mutate(year=2019)
odd_2020 <- import("r_input/odd_kpmg_2020.xls",sheet="Sheet1",skip=2) %>% 
  mutate(year=2020,fiyat=as.numeric(fiyat),engine_displacement=as.numeric(engine_displacement),co2=as.numeric(co2))


#otv oranlari
eski_otv_oranlari <- import("r_input/otv_structure.xlsx",sheet="ice_eski")
mevcut_otv_oranlari <- import("r_input/otv_structure.xlsx",sheet="ice_mevcut")
yeni_otv_oranlari<- import("r_input/otv_structure.xlsx",sheet="yeni_otv")
sales_forecast<-  import("r_input/sales_forecast.xls")


#mtv oranlari
mtv_oranlari <- import("r_input/mtv_oranlari.xlsx",sheet="2020_mtv")
arac_omru <- import("r_input/mtv_oranlari.xlsx",sheet="degiskenler")[1,1]

# total sales until august
odd_agustos_2019<-  odd_2019 %>% select(-eylul:-aralik)
odd_agustos_2018<-  odd_2018 %>% select(-eylul:-aralik)
odd_agustos <- rbind(odd_2020,odd_agustos_2019)
odd_agustos <- rbind(odd_agustos,odd_agustos_2018)
colnames(odd_agustos)
odd_agustos$toplam <- rowSums(odd_agustos[,39:46])
odd_agustos %>% group_by(year) %>% summarise(sum(toplam))





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


milyar<-1000000000















