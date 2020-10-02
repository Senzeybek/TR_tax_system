

#verinin sadelestirilmesi ve yillik forecast rakamlarinin uygulanmasi ----

lcv_data <- odd_lcv_2020 %>% filter(!is.na(fiyat))  

lcv_data$"2020" <- round(lcv_data$toplam*(sales_forecast$`2020`[sales_forecast$arac_tipi=="LCV"]/sum(lcv_data$toplam)))
lcv_data$"2021" <- round(lcv_data$toplam*(sales_forecast$`2021`[sales_forecast$arac_tipi=="LCV"]/sum(lcv_data$toplam)))
lcv_data$"2022" <- round(lcv_data$toplam*(sales_forecast$`2022`[sales_forecast$arac_tipi=="LCV"]/sum(lcv_data$toplam)))
lcv_data$"2023" <- round(lcv_data$toplam*(sales_forecast$`2023`[sales_forecast$arac_tipi=="LCV"]/sum(lcv_data$toplam)))
lcv_data$"2024" <- round(lcv_data$toplam*(sales_forecast$`2024`[sales_forecast$arac_tipi=="LCV"]/sum(lcv_data$toplam)))
lcv_data$"2025" <- round(lcv_data$toplam*(sales_forecast$`2025`[sales_forecast$arac_tipi=="LCV"]/sum(lcv_data$toplam)))


lcv_data <- lcv_data %>% gather(key=year,value = sales, "2020":"2025")
lcv_data$year <- as.numeric(lcv_data$year)
lcv_data <- lcv_data%>% filter(year !=2020)

#eski OTV oranlari ----

lcv_data <- lcv_data %>% mutate(eski_otv_grubu = case_when(
  govde_tipi=="Van" ~ 1,
  govde_tipi=="Minibüs" ~ 2,
  govde_tipi=="Kamyonet" ~ 3,
  govde_tipi=="Pick-Up" ~ 4,
))

#net fiyat bulunmasi ----
lcv_data <- lcv_data %>% mutate(eski_otv_orani= case_when(
  str_detect(segment,"Kombi Van") ~ 0.15, 
  govde_tipi=="Minibüs"  ~ 0.056,
  TRUE ~ 0.04
))

lcv_data$kdv_orani <- 0.18
lcv_data <- lcv_data %>% mutate(net_fiyat= fiyat/((1+eski_otv_orani)*(1+kdv_orani)))
lcv_data$eski_otv_miktari <-  lcv_data$net_fiyat*lcv_data$eski_otv_orani
lcv_data$eski_kdv_miktari <- (lcv_data$net_fiyat+lcv_data$eski_otv_miktari)*lcv_data$kdv_orani

# mevcut OTV gruplari ----

lcv_data <- lcv_data %>% mutate(mevcut_otv_grubu = eski_otv_grubu)

# mevcut OTV oranlari ----
lcv_data$mevcut_otv_orani <-lcv_data$eski_otv_orani
lcv_data <- lcv_data %>% mutate(mevcut_otv_tutari = net_fiyat*mevcut_otv_orani,
                        mevcut_kdv_tutari = (net_fiyat+mevcut_otv_tutari)*kdv_orani,
                        mevcut_fiyat =net_fiyat+mevcut_otv_tutari+mevcut_kdv_tutari)



# yeni OTV orani ----

lcv_data$yeni_otv_orani <- lcv_data$mevcut_otv_orani
lcv_data$co2_vergisi <- 0


lcv_data <- lcv_data %>% mutate(yeni_toplam_otv_tutari= (net_fiyat*yeni_otv_orani+co2_vergisi),
                        yeni_toplam_otv_orani = yeni_toplam_otv_tutari/net_fiyat,
                        yeni_kdv_tutari = (net_fiyat + yeni_toplam_otv_tutari)*kdv_orani,
                        yeni_fiyat = (net_fiyat+yeni_toplam_otv_tutari+yeni_kdv_tutari),
                        fark=yeni_fiyat-mevcut_fiyat,
                        yuzde_fiyat_degisimi=fark/mevcut_fiyat)







