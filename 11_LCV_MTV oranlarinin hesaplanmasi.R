


# 2020de satilan araclarin omur boyunca toplam odeyecekleri MTVnin hesaplanmasi ----

lcv_mtv_oranlari$lifetime_mtv <-rowSums(lcv_mtv_oranlari %>% select(7:(7+arac_omru-1)))

lcv_data$mtv_grubu<- case_when(
lcv_data$govde_tipi=="Van" & between(lcv_data$agirlik,0,Inf) & between(lcv_data$engine_displacement,0,1900) ~ 1,
lcv_data$govde_tipi=="Van" & between(lcv_data$agirlik,0,Inf) & between(lcv_data$engine_displacement,1901,Inf) ~ 2,

lcv_data$govde_tipi=="Minibus" ~ 3,

lcv_data$govde_tipi=="Kamyonet" & between(lcv_data$agirlik,0,1500) & between(lcv_data$engine_displacement,0,Inf) ~ 4,
lcv_data$govde_tipi=="Kamyonet"&between(lcv_data$agirlik,1501,3500)&between(lcv_data$engine_displacement,0,Inf) ~ 5,
lcv_data$govde_tipi=="Kamyonet"&between(lcv_data$agirlik,3501,5000)&between(lcv_data$engine_displacement,0,Inf) ~ 6,
lcv_data$govde_tipi=="Kamyonet"&between(lcv_data$agirlik,5001,10000)&between(lcv_data$engine_displacement,0,Inf) ~ 7,
lcv_data$govde_tipi=="Kamyonet"&between(lcv_data$agirlik,10001,20000)&between(lcv_data$engine_displacement,0,Inf) ~ 8,
lcv_data$govde_tipi=="Kamyonet"&between(lcv_data$agirlik,20001,Inf)&between(lcv_data$engine_displacement,0,Inf) ~ 9,

lcv_data$govde_tipi=="Pick-Up"&between(lcv_data$agirlik,0,1500)&between(lcv_data$engine_displacement,0,Inf) ~ 10,
lcv_data$govde_tipi=="Pick-Up"&between(lcv_data$agirlik,1501,3500)&between(lcv_data$engine_displacement,0,Inf) ~ 11,
lcv_data$govde_tipi=="Pick-Up"&between(lcv_data$agirlik,3501,5000)&between(lcv_data$engine_displacement,0,Inf) ~ 12,
lcv_data$govde_tipi=="Pick-Up"&between(lcv_data$agirlik,5001,10000)&between(lcv_data$engine_displacement,0,Inf) ~ 13,
lcv_data$govde_tipi=="Pick-Up"&between(lcv_data$agirlik,10001,20000)&between(lcv_data$engine_displacement,0,Inf) ~ 14,
lcv_data$govde_tipi=="Pick-Up"&between(lcv_data$agirlik,20001,Inf)&between(lcv_data$engine_displacement,0,Inf) ~ 15,

TRUE ~  0
)



lcv_data$lifetime_mtv <- lcv_mtv_oranlari$lifetime_mtv[match(lcv_data$mtv_grubu,lcv_mtv_oranlari$mtv_grubu)]



#CO2 verisinin olmadigi veriler CO2 ortalamasi ile degistirildi

lcv_data$co2<-ifelse(is.na(lcv_data$co2),weighted.mean(lcv_data$co2,lcv_data$sales, na.rm=T),lcv_data$co2)



# sadece CO2 emisyonuna dayali vergi sistemi ----

lcv_data$yeni_mtv_sadece_co2 <- lcv_data$co2*mtv_per_co2

lcv_data$yeni_lifetime_mtv_sadece_co2 <- lcv_data$yeni_mtv_sadece_co2*arac_omru



# Opsiyon3: CO2 araliklarina gore vergilendirme ----

lcv_data <- lcv_data %>% mutate(co2_grubu= case_when(
  between(co2,lcv_co2_gruplari[1,1],lcv_co2_gruplari[1,2]) ~ lcv_co2_gruplari[1,3],
  between(co2,lcv_co2_gruplari[2,1],lcv_co2_gruplari[2,2]) ~ lcv_co2_gruplari[2,3],
  between(co2,lcv_co2_gruplari[3,1],lcv_co2_gruplari[3,2]) ~ lcv_co2_gruplari[3,3],
  between(co2,lcv_co2_gruplari[4,1],lcv_co2_gruplari[4,2]) ~ lcv_co2_gruplari[4,3],
  between(co2,lcv_co2_gruplari[5,1],lcv_co2_gruplari[5,2]) ~ lcv_co2_gruplari[5,3]
))



lcv_mtv_grubu_co2_araliklari <- lcv_mtv_grubu_co2_araliklari %>% 
  gather(key = "co2_grubu", value ="mtv_miktari", co2_grubu_1:co2_grubu_5) %>% 
  select(mtv_grubu,co2_grubu, yeni_mtv_co2_araliklari = mtv_miktari
  )



lcv_data$yeni_mtv_co2_araliklari <- lcv_mtv_grubu_co2_araliklari$yeni_mtv_co2_araliklari[match
                                                  (paste(lcv_data$mtv_grubu,lcv_data$co2_grubu),
                                                   paste(lcv_mtv_grubu_co2_araliklari$mtv_grubu,lcv_mtv_grubu_co2_araliklari$co2_grubu))]

lcv_data$yeni_lifetime_mtv_co2_araliklari <- lcv_data$yeni_mtv_co2_araliklari*arac_omru


# # 15 yillik veriler
lcv_mtv_oranlari$lifetime_mtv_15_yil <-rowSums(lcv_mtv_oranlari %>% select(7:(7+15-1)))
lcv_data$lifetime_mtv_15_yil <- lcv_mtv_oranlari$lifetime_mtv_15_yil[match(lcv_data$mtv_grubu,lcv_mtv_oranlari$mtv_grubu)]
lcv_data$yeni_lifetime_mtv_sadece_co2_15_yil <- lcv_data$yeni_lifetime_mtv_sadece_co2*15
lcv_data$yeni_lifetime_mtv_co2_araliklari_15_yil <- lcv_data$yeni_mtv_co2_araliklari*15

