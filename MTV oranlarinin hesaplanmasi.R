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


# Yenis araclarin omur boyunca toplam odeyecekleri MTVnin hesaplanmasi
mtv_oranlari$lifetime_mtv <-rowSums(mtv_oranlari %>% select(yil_1:(yil_1+arac_omru-1)))
data$lifetime_mtv <- mtv_oranlari$lifetime_mtv[match(data$mtv_grubu,mtv_oranlari$mtv_grubu)]
Mevcut_muhtemel_MTV_geliri <- sum(data$sales_2020*data$lifetime_mtv)/milyar
co2_mtv = (Mevcut_muhtemel_MTV_geliri+(Mevcut_muhtemel_OTV_geliri-Yeni_toplam_OTV_geliri))*milyar/(arac_omru*sum(data$sales_2020)*weighted.mean(data$co2,data$toplam,na.rm=T))
