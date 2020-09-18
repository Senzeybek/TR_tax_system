source("1_preparation.R")
source("OTV oranlarinin hesaplanmasi.R")


data <-  data %>% mutate(mtv_grubu=case_when(
  between(engine_displacement,0,1300) & between(net_fiyat,0,51800) ~ 1,
  between(engine_displacement,0,1300) & between(net_fiyat,51800,90800) ~ 2,
  between(engine_displacement,0,1300) & between(net_fiyat,90800,Inf) ~ 3,
  
  between(engine_displacement,1301,1600) & between(net_fiyat,0,51800) ~ 4,
  between(engine_displacement,1301,1600) & between(net_fiyat,51800,90800) ~ 5,
  between(engine_displacement,1301,1600) & between(net_fiyat,90800,Inf) ~ 6,
  
  between(engine_displacement,1601,1800) & between(net_fiyat,0,129800) ~ 7,
  between(engine_displacement,1601,1800) & between(net_fiyat,129800,Inf) ~ 8,
  
  between(engine_displacement,1801,2000) & between(net_fiyat,0,129800) ~ 9,
  between(engine_displacement,1801,2000) & between(net_fiyat,129800,Inf) ~ 10,
  
  between(engine_displacement,2001,2500) & between(net_fiyat,0,162100) ~ 11,
  between(engine_displacement,2001,2500) & between(net_fiyat,162100,Inf) ~ 12,
  
  between(engine_displacement,2501,3000) & between(net_fiyat,0,324400) ~ 13,
  between(engine_displacement,2501,3000) & between(net_fiyat,324400,Inf) ~ 14,
  
  between(engine_displacement,3001,3500) & between(net_fiyat,0,324400) ~ 15,
  between(engine_displacement,3001,3500) & between(net_fiyat,324400,Inf) ~ 16,
  
  between(engine_displacement,3501,4000) & between(net_fiyat,0,519200) ~ 17,
  between(engine_displacement,3501,4000) & between(net_fiyat,519200,Inf) ~ 18,
  
  between(engine_displacement,4001,99999) & between(net_fiyat,0,616500) ~ 19,
  between(engine_displacement,4001,99999) & between(net_fiyat,616500,Inf) ~ 20,
  
  TRUE ~ 0
))


# 2020de satilan araclarin omur boyunca toplam odeyecekleri MTVnin hesaplanmasi ----

mtv_oranlari$lifetime_mtv <-rowSums(mtv_oranlari %>% select(yil_1:(yil_1+arac_omru-1)))

data$lifetime_mtv <- mtv_oranlari$lifetime_mtv[match(data$mtv_grubu,mtv_oranlari$mtv_grubu)]

Mevcut_muhtemel_MTV_geliri <- sum(data$satis_2020*data$lifetime_mtv)/milyar

gereken_co2_mtv = (Mevcut_muhtemel_MTV_geliri+
                     (Mevcut_muhtemel_OTV_geliri-Yeni_toplam_OTV_geliri))*milyar/
                  (arac_omru*sum(data$satis_2020)*weighted.mean(data$co2,data$toplam,na.rm=T))


# sadece CO2 emisyonuna dayali vergi sistemi ----

data$yeni_mtv_sadece_co2 <- data$co2*mtv_per_co2

data$yeni_lifetime_mtv_sadece_co2 <- data$yeni_mtv*arac_omru

Yeni_muhtemel_MTV_geliri_sadece_co2 <- sum(data$satis_2020*data$yeni_lifetime_mtv,na.rm=T)/milyar

# OTV gruplarina dayali MTV olusturulmasi ----

data$yeni_mtv_co2_by_otv <- otv_grubuna_dayali_mtv$co2_tax[match(data$mevcut_otv_grubu,otv_grubuna_dayali_mtv$otv_grubu)]

data$yeni_mtv_by_otv <- data$co2*data$yeni_mtv_co2_by_otv

data$yeni_lifetime_mtv_otv_grubu_co2 <- data$yeni_mtv_by_otv*arac_omru

Yeni_muhtemel_MTV_geliri_otv_grubu_co2 <- sum(data$yeni_lifetime_mtv_otv_grubu_co2*data$satis_2020,na.rm = T)/milyar

