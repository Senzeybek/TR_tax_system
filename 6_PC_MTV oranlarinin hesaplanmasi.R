


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

mtv_oranlari$lifetime_mtv <-rowSums(mtv_oranlari %>% select(6:(6+arac_omru-1)))

data$lifetime_mtv <- mtv_oranlari$lifetime_mtv[match(data$mtv_grubu,mtv_oranlari$mtv_grubu)]


# sadece CO2 emisyonuna dayali vergi sistemi ----

data$yeni_mtv_sadece_co2 <- data$co2*mtv_per_co2

data$yeni_lifetime_mtv_sadece_co2 <- data$yeni_mtv_sadece_co2*arac_omru


# OTV gruplarina dayali MTV olusturulmasi ----

data$yeni_mtv_co2_by_otv <- otv_grubuna_dayali_mtv$co2_tax[match(data$mevcut_otv_grubu,otv_grubuna_dayali_mtv$otv_grubu)]

data$yeni_mtv_by_otv <- data$co2*data$yeni_mtv_co2_by_otv

data$yeni_lifetime_mtv_otv_grubu_co2 <- data$yeni_mtv_by_otv*arac_omru




# Opsiyon3: CO2 araliklarina gore vergilendirme ----

data <- data %>% mutate(co2_grubu= case_when(
   between(co2,co2_gruplari[1,1],co2_gruplari[1,2]) ~ co2_gruplari[1,3],
   between(co2,co2_gruplari[2,1],co2_gruplari[2,2]) ~ co2_gruplari[2,3],
   between(co2,co2_gruplari[3,1],co2_gruplari[3,2]) ~ co2_gruplari[3,3],
   between(co2,co2_gruplari[4,1],co2_gruplari[4,2]) ~ co2_gruplari[4,3],
   between(co2,co2_gruplari[5,1],co2_gruplari[5,2]) ~ co2_gruplari[5,3]
  ))



otv_grubu_co2_araliklari <- otv_grubu_co2_araliklari %>% 
  gather(key = "co2_grubu", value ="mtv_miktari", co2_grubu_1:co2_grubu_5) %>% 
  select(mevcut_otv_grubu,co2_grubu, yeni_mtv_co2_araliklari = mtv_miktari
)


data <- data %>% right_join(otv_grubu_co2_araliklari, by=c("mevcut_otv_grubu","co2_grubu"))

data$yeni_lifetime_mtv_co2_araliklari <- data$yeni_mtv_co2_araliklari*arac_omru


# clearing empty rows
data <- data %>% filter(!is.na(model))



# 15 yillik veriler
mtv_oranlari$lifetime_mtv_15_yil <-rowSums(mtv_oranlari %>% select(6:(6+15-1)))
data$lifetime_mtv_15_yil <- mtv_oranlari$lifetime_mtv_15_yil[match(data$mtv_grubu,mtv_oranlari$mtv_grubu)]
data$yeni_lifetime_mtv_sadece_co2_15_yil <- data$yeni_mtv_sadece_co2*15
data$yeni_lifetime_mtv_otv_grubu_co2_15_yil <- data$yeni_mtv_by_otv*15
data$yeni_lifetime_mtv_co2_araliklari_15_yil <- data$yeni_mtv_co2_araliklari*15

