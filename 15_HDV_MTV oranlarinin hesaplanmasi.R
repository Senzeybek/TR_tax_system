


# 2020de satilan araclarin omur boyunca toplam odeyecekleri MTVnin hesaplanmasi ----

hdv_mtv_oranlari$lifetime_mtv <-rowSums(hdv_mtv_oranlari %>% select(7:(7+arac_omru-1)))

# hdv_data$mtv_grubu<- case_when(
#   hdv_data$govde_tipi=="kamyon" & between(hdv_data$agirlik,0,1500) & between(hdv_data$oturma_yeri,0,Inf) ~ 1,
#   hdv_data$govde_tipi=="kamyon"&between(hdv_data$agirlik,1501,3500)&between(hdv_data$oturma_yeri,0,Inf) ~ 2,
#   hdv_data$govde_tipi=="kamyon"&between(hdv_data$agirlik,3501,5000)&between(hdv_data$oturma_yeri,0,Inf) ~ 3,
#   hdv_data$govde_tipi=="kamyon"&between(hdv_data$agirlik,5001,10000)&between(hdv_data$oturma_yeri,0,Inf) ~ 4,
#   hdv_data$govde_tipi=="kamyon"&between(hdv_data$agirlik,10001,20000)&between(hdv_data$oturma_yeri,0,Inf) ~ 5,
#   hdv_data$govde_tipi=="kamyon"&between(hdv_data$agirlik,20001,Inf)&between(hdv_data$oturma_yeri,0,Inf) ~ 6,
#   
#   hdv_data$govde_tipi%in% c("otobus","midibus") &between(hdv_data$agirlik,0,1500)&between(hdv_data$oturma_yeri,0,25) ~ 7,
#   hdv_data$govde_tipi%in% c("otobus","midibus") &between(hdv_data$agirlik,1501,3500)&between(hdv_data$oturma_yeri,26,35) ~ 8,
#   hdv_data$govde_tipi%in% c("otobus","midibus") &between(hdv_data$agirlik,3501,5000)&between(hdv_data$oturma_yeri,36,45) ~ 9,
#   hdv_data$govde_tipi%in% c("otobus","midibus") &between(hdv_data$agirlik,5001,10000)&between(hdv_data$oturma_yeri,45,Inf) ~ 10,
#   
#   TRUE ~  0
# )



# 
# 
hdv_data$lifetime_mtv <- hdv_mtv_oranlari$lifetime_mtv[match(hdv_data$mtv_grubu,hdv_mtv_oranlari$mtv_grubu)]
# 
# 
# 
# #CO2 verisinin olmadigi veriler CO2 ortalamasi ile degistirildi
# 
# hdv_data$co2<-ifelse(is.na(hdv_data$co2),weighted.mean(hdv_data$co2,hdv_data$sales, na.rm=T),hdv_data$co2)
# 
# 
# 
# # sadece CO2 emisyonuna dayali vergi sistemi ----
# 
# hdv_data$yeni_mtv_sadece_co2 <- hdv_data$co2*mtv_per_co2
# 
# hdv_data$yeni_lifetime_mtv_sadece_co2 <- hdv_data$yeni_mtv_sadece_co2*arac_omru
# 
# 
# 
# # Opsiyon3: CO2 araliklarina gore vergilendirme ----
# 
# hdv_data <- hdv_data %>% mutate(co2_grubu= case_when(
#   between(co2,hdv_co2_gruplari[1,1],hdv_co2_gruplari[1,2]) ~ hdv_co2_gruplari[1,3],
#   between(co2,hdv_co2_gruplari[2,1],hdv_co2_gruplari[2,2]) ~ hdv_co2_gruplari[2,3],
#   between(co2,hdv_co2_gruplari[3,1],hdv_co2_gruplari[3,2]) ~ hdv_co2_gruplari[3,3],
#   between(co2,hdv_co2_gruplari[4,1],hdv_co2_gruplari[4,2]) ~ hdv_co2_gruplari[4,3],
#   between(co2,hdv_co2_gruplari[5,1],hdv_co2_gruplari[5,2]) ~ hdv_co2_gruplari[5,3]
# ))
# 
# 
# 
# hdv_mtv_grubu_co2_araliklari <- hdv_mtv_grubu_co2_araliklari %>% 
#   gather(key = "co2_grubu", value ="mtv_miktari", co2_grubu_1:co2_grubu_5) %>% 
#   select(mtv_grubu,co2_grubu, yeni_mtv_co2_araliklari = mtv_miktari
#   )
# 
# 
# 
# hdv_data$yeni_mtv_co2_araliklari <- hdv_mtv_grubu_co2_araliklari$yeni_mtv_co2_araliklari[match
#                                                                                          (paste(hdv_data$mtv_grubu,hdv_data$co2_grubu),
#                                                                                            paste(hdv_mtv_grubu_co2_araliklari$mtv_grubu,hdv_mtv_grubu_co2_araliklari$co2_grubu))]
# 
# hdv_data$yeni_lifetime_mtv_co2_araliklari <- hdv_data$yeni_mtv_co2_araliklari*arac_omru
# 
# 
# # # 15 yillik veriler
# mtv_oranlari$lifetime_mtv_15_yil <-rowSums(mtv_oranlari %>% select(6:(6+15-1)))
# hdv_data$lifetime_mtv_15_yil <- mtv_oranlari$lifetime_mtv_15_yil[match(hdv_data$mtv_grubu,mtv_oranlari$mtv_grubu)]
# hdv_data$yeni_lifetime_mtv_sadece_co2_15_yil <- hdv_data$yeni_lifetime_mtv_sadece_co2*15
# hdv_data$yeni_lifetime_mtv_co2_araliklari_15_yil <- hdv_data$yeni_mtv_co2_araliklari*15

