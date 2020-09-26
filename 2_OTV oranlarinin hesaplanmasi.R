
#verinin sadelestirilmesi ve yillik forecast rakamlarinin uygulanmasi ----

data <- odd_2020 %>% filter(!is.na(fiyat),!is.na(engine_displacement)) %>% 
  select(-kampanya,-kampanya_fiyat,-turetilme_sebebi,-turetildigi_satir,-max_hiz,-hizlanma,
         -piyasadan_cikis,-piyasaya_giris,-ocak:-agustos,-turbo_motor,-start_stop,
         -ulke,-model_year,-silindir,-`electric_max_power (Kw)`,-`electric_max:_tork (Nm)`,
         -menzil_toplam,-baterry_capacity,-sarj,-max_guc,-max_guc_devri,-max_guc_devri2,-guc,
         -hybrid_toplam_guc,-emisyon_standarti,-sanziman,-garanti_km,-elektric_guc_tuketimi_100km,
         -donanim,-alt_model,-year)


data$"2020" <- round(data$toplam*(sales_forecast$`2020`[sales_forecast$arac_tipi=="binek_arac"]/sum(data$toplam)))
data$"2021" <- round(data$toplam*(sales_forecast$`2021`[sales_forecast$arac_tipi=="binek_arac"]/sum(data$toplam)))
data$"2022" <- round(data$toplam*(sales_forecast$`2022`[sales_forecast$arac_tipi=="binek_arac"]/sum(data$toplam)))
data$"2023" <- round(data$toplam*(sales_forecast$`2023`[sales_forecast$arac_tipi=="binek_arac"]/sum(data$toplam)))
data$"2024" <- round(data$toplam*(sales_forecast$`2024`[sales_forecast$arac_tipi=="binek_arac"]/sum(data$toplam)))
data$"2025" <- round(data$toplam*(sales_forecast$`2025`[sales_forecast$arac_tipi=="binek_arac"]/sum(data$toplam)))


data <- data %>% gather(key=year,value = sales, "2020":"2025")
data$year <- as.numeric(data$year)


#eski OTV oranlari ----

data <- data %>% mutate(eski_otv_grubu = case_when(
  between(engine_displacement,0,1600)& between(fiyat,0, 119770) ~ 1,
  between(engine_displacement,0,1600)& between(fiyat,119770,212400) ~ 2,
  between(engine_displacement,0,1600)& between(fiyat,212400,Inf) ~ 3,
  between(engine_displacement,1601,2000)& between(fiyat,0,401200) ~ 4,
  between(engine_displacement,1601,2000)& between(fiyat,401200,Inf) ~ 5,
  between(engine_displacement,2001,Inf)& between(fiyat,0,Inf) ~ 6,
  TRUE ~ 0
))

#hybrid incentives
data[data$powertrain=="Hybrid",] <- data%>% filter(powertrain=="Hybrid") %>% mutate(eski_otv_grubu=case_when(
  between(engine_displacement,0,1600) & between(fiyat,0, 119770) ~ 1,
  between(engine_displacement,0,1600) & between(fiyat,119770,212400) ~ 2,
  between(engine_displacement,0,1600) & between(fiyat,212400,Inf) ~ 3,
  between(engine_displacement,1601,1800) & between(fiyat,0,145435) ~ 1,
  between(engine_displacement,1601,1800) & between(fiyat,145435,238950) ~ 2,
  between(engine_displacement,1601,1800) & between(fiyat,238950,Inf) ~ 3,
  between(engine_displacement,2001,2500) & between(fiyat,0,401200) ~ 4,
  between(engine_displacement,2001,2500) & between(fiyat,401200,Inf) ~ 5,
  between(engine_displacement,1801,2000) & between(fiyat,0,401200) ~ 4,
  between(engine_displacement,1801,2000) & between(fiyat,401200,Inf) ~ 5,
  between(engine_displacement,2001,Inf)  & between(fiyat,0,Inf) ~ 6,
  TRUE ~ 0
))



#net fiyat bulunmasi ----
data$eski_otv_orani <- eski_otv_oranlari$eski_otv_orani[match(data$eski_otv_grubu,eski_otv_oranlari$otv_grup)]
data$kdv_orani <- 0.18
data <- data %>% mutate(net_fiyat= fiyat/((1+eski_otv_orani)*(1+kdv_orani)))
data$eski_otv_miktari <-  data$net_fiyat*data$eski_otv_orani
data$eski_kdv_miktari <- (data$net_fiyat+data$eski_otv_miktari)*data$kdv_orani

# mevcut OTV gruplari ----

data <- data %>% mutate(mevcut_otv_grubu = case_when(
  between(engine_displacement,0,1600)& between(net_fiyat,0, 85000) ~ 1,
  between(engine_displacement,0,1600)& between(net_fiyat,85000,130000) ~ 2,
  between(engine_displacement,0,1600)& between(net_fiyat,130000,Inf) ~ 3,
  between(engine_displacement,1601,2000)& between(net_fiyat,0,170000) ~ 4,
  between(engine_displacement,1601,2000)& between(net_fiyat,170000,Inf) ~ 5,
  between(engine_displacement,2001,Inf)& between(net_fiyat,0,Inf) ~ 6,
  TRUE ~ 0
))

#hybrid incentives
data[data$powertrain=="Hybrid",] <- data%>% filter(powertrain=="Hybrid") %>% 
  mutate(mevcut_otv_grubu=case_when(
    between(engine_displacement,0,1600)& between(net_fiyat,0, 85000) ~ 1,
    between(engine_displacement,0,1600)& between(net_fiyat,85000,130000) ~ 2,
    between(engine_displacement,0,1600)& between(net_fiyat,130000,Inf) ~ 3,
    between(engine_displacement,1601,1800)& between(net_fiyat,0,85000) ~ 1,
    between(engine_displacement,1601,1800)& between(net_fiyat,85000,135000) ~ 2,
    between(engine_displacement,1601,1800)& between(net_fiyat,135000,Inf) ~ 3,
    between(engine_displacement,2001,2500)& between(net_fiyat,0,170000) ~ 4,
    between(engine_displacement,2001,2500)& between(net_fiyat,170000,Inf) ~ 5,
    between(engine_displacement,1801,2000)& between(net_fiyat,0,170000) ~ 4,
    between(engine_displacement,1801,2000)& between(net_fiyat,170000,Inf) ~ 5,
    between(engine_displacement,2001,Inf)& between(net_fiyat,0,Inf) ~ 6,
    TRUE ~ 0
  ))


# mevcut OTV oranlari ----
data$mevcut_otv_orani <- mevcut_otv_oranlari$mevcut_otv_orani[match(data$mevcut_otv_grubu,mevcut_otv_oranlari$otv_grup)]
data <- data %>% mutate(mevcut_otv_tutari = net_fiyat*mevcut_otv_orani,
                        mevcut_kdv_tutari = (net_fiyat+mevcut_otv_tutari)*kdv_orani,
                        mevcut_fiyat =net_fiyat+mevcut_otv_tutari+mevcut_kdv_tutari)



# yeni OTV orani ----

data$yeni_otv_orani <- yeni_otv_oranlari$yeni_otv_orani[match(data$mevcut_otv_grubu,yeni_otv_oranlari$otv_grup)]
data$co2_vergisi <- data$co2*(yeni_otv_oranlari$co2_vergisi[match(data$mevcut_otv_grubu,yeni_otv_oranlari$otv_grup)])


data <- data %>% mutate(yeni_toplam_otv_tutari= (net_fiyat*yeni_otv_orani+co2_vergisi),
                        yeni_toplam_otv_orani = yeni_toplam_otv_tutari/net_fiyat,
                        yeni_kdv_tutari = (net_fiyat + yeni_toplam_otv_tutari)*kdv_orani,
                        yeni_fiyat = (net_fiyat+yeni_toplam_otv_tutari+yeni_kdv_tutari),
                        fark=yeni_fiyat-mevcut_fiyat,
                        yuzde_fiyat_degisimi=fark/mevcut_fiyat)



#toplam vergi gelirleri ----

Mevcut_muhtemel_vergi_gelirleri <- data %>% group_by(year) %>% summarise(Mevcut_muhtemel_OTV_geliri=sum(mevcut_otv_tutari * (sales))/milyar,
                                                                        Mevcut_muhtemel_KDV_geliri=sum(mevcut_kdv_tutari * (sales))/milyar)





























