


data <-  data%>% separate(segment,     into = c("segment_text", "segment_num"), 
                          sep = "(?<=[A-Za-z])(?=[0-9])")

data$segment_text <- tolower(data$segment_text)



data$yuzde_satis_degisimi<- (data$yuzde_fiyat_degisimi*kendi_esnekligi)
data$yeni_satis<- round(data$satis_2020*(1+data$yuzde_satis_degisimi))



# Toplam yeni satis arasindaki fark ----
data$adet_degisimi <- data$yeni_satis-data$satis_2020
Yeni_toplam_OTV_geliri <- sum(data$eski_otv_miktari*data$toplam)/milyar + #agustos ayina kadar olan satislar
  sum(data$mevcut_otv_tutari * (data$satis_2020-data$toplam))/milyar

#ekstra yakit tuketimi ----
ekstra_yakit_tuketimi <- data %>% group_by(powertrain) %>% summarise(degisim=sum(adet_degisimi), 
                                                                     ortalama_yakit_tuketimi= weighted.mean(yakit_tuketimi,adet_degisimi,na.rm=T)) 


ekstra_yakit_tuketimi$gercek_tuketim <- 1.21*ekstra_yakit_tuketimi$ortalama_yakit_tuketimi

ekstra_yakit_tuketimi$toplam_yakit_tuketimi <- (arac_omru_km/100)*ekstra_yakit_tuketimi$gercek_tuketim
ekstra_yakit_tuketimi$yakit_otv<- ifelse(ekstra_yakit_tuketimi$powertrain=="Dizel",dizel_litre_otv,benzin_litre_otv)
Toplam_ekstra_yakit_geliri= sum(ekstra_yakit_tuketimi$toplam_yakit_tuketimi*ekstra_yakit_tuketimi$yakit_otv*ekstra_yakit_tuketimi$degisim)/milyar  


