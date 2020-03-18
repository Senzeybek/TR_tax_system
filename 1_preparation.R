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


manual_emission_update_2017<- function(data=odd_2017){
  odd_2017[odd_2017$version=='passat 1.6 tdi bmt 120 ps impression dsg',]$co2_emission<<-104
  odd_2017[odd_2017$version=='520i sedan',]$co2_emission<<-149
  odd_2017[odd_2017$version=='mc 1.6 dci skypack x-tronic',]$co2_emission<<-119
  odd_2017[odd_2017$version=='mc x-trail 1.6 dci platinum x-tronic',]$co2_emission<<-139
  odd_2017[odd_2017$version=='1,0 tsi 95 ps highline dsg',]$co2_emission<<-107
  odd_2017[odd_2017$version=='diamond ecvt',]$co2_emission<<-87
  odd_2017[odd_2017$version=='mc 1.6 dci design pack x-tronic',]$co2_emission<<-122
  odd_2017[odd_2017$version=='yaris 1.5 fun special multidrive s',]$co2_emission<<-108
  odd_2017[odd_2017$version=='grandsport 1.6 excellence at',]$co2_emission<<-134
  odd_2017[odd_2017$version=='style 1.0 tsi 110 ps dsg',]$co2_emission<<-106
  odd_2017[odd_2017$version=='icon 1.5 dci edc 90bg faz2',]$co2_emission<<-103
  odd_2017[odd_2017$version=='1,0 tsi 95 ps comfortline dsg',]$co2_emission<<-107
  odd_2017[odd_2017$version=='dynamic ecvt',]$co2_emission<<-87
  odd_2017[odd_2017$version=='comfort 1.6 tdi 120 ps dsg',]$co2_emission<<-107
  odd_2017[odd_2017$version=='mc 1.5 dci skypack',]$co2_emission<<-99
  odd_2017[odd_2017$version=='grandsport 1.6 design at',]$co2_emission<<-134
  odd_2017[odd_2017$version=='grandsport 1.6 enjoy at',]$co2_emission<<- 134 
  odd_2017[odd_2017$version=='a3 sedan 1.0 turbo fsi 116 hp  s tronic pi',]$co2_emission<<-106
  odd_2017[odd_2017$version=='touchrome 1.2 edc 120bg (sl)',]$co2_emission<<-120
  odd_2017[odd_2017$version=='a6 2.0 turbo fsi quattro  252 hp  s tronic pi',]$co2_emission<<-153
  odd_2017[odd_2017$version=='1.0 ecotsi 95 hp style',]$co2_emission<<-106
  odd_2017[odd_2017$version=='mc 1.5 dci tekna',]$co2_emission<<-99
}

