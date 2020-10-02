


#verinin sadelestirilmesi ve yillik forecast rakamlarinin uygulanmasi ----

hdv_data <- odd_hdv_2020   

hdv_data$"2020" <- round(hdv_data$toplam*(sales_forecast$`2020`[sales_forecast$arac_tipi=="HDV"]/sum(hdv_data$toplam)))
hdv_data$"2021" <- round(hdv_data$toplam*(sales_forecast$`2021`[sales_forecast$arac_tipi=="HDV"]/sum(hdv_data$toplam)))
hdv_data$"2022" <- round(hdv_data$toplam*(sales_forecast$`2022`[sales_forecast$arac_tipi=="HDV"]/sum(hdv_data$toplam)))
hdv_data$"2023" <- round(hdv_data$toplam*(sales_forecast$`2023`[sales_forecast$arac_tipi=="HDV"]/sum(hdv_data$toplam)))
hdv_data$"2024" <- round(hdv_data$toplam*(sales_forecast$`2024`[sales_forecast$arac_tipi=="HDV"]/sum(hdv_data$toplam)))
hdv_data$"2025" <- round(hdv_data$toplam*(sales_forecast$`2025`[sales_forecast$arac_tipi=="HDV"]/sum(hdv_data$toplam)))



hdv_data <- hdv_data %>% gather(key=year,value = sales, "2020":"2025")
hdv_data$year <- as.numeric(hdv_data$year)
hdv_data <- hdv_data%>% filter(year !=2020)


#net fiyat bulunmasi ----

hdv_data$eski_otv_miktari <-  hdv_data$net_fiyat*hdv_data$otv_orani
hdv_data$eski_kdv_miktari <- (hdv_data$net_fiyat+hdv_data$eski_otv_miktari)*hdv_data$kdv_orani


# mevcut OTV oranlari ----
hdv_data$mevcut_otv_orani <-hdv_data$otv_orani
hdv_data <- hdv_data %>% mutate(mevcut_otv_tutari = net_fiyat*mevcut_otv_orani,
                                mevcut_kdv_tutari = (net_fiyat+mevcut_otv_tutari)*kdv_orani,
                                mevcut_fiyat =net_fiyat+mevcut_otv_tutari+mevcut_kdv_tutari)


# yeni OTV orani ----

hdv_data$yeni_otv_orani <- hdv_data$mevcut_otv_orani
hdv_data$co2_vergisi <- 0


hdv_data <- hdv_data %>% mutate(yeni_toplam_otv_tutari= (net_fiyat*yeni_otv_orani+co2_vergisi),
                                yeni_toplam_otv_orani = yeni_toplam_otv_tutari/net_fiyat,
                                yeni_kdv_tutari = (net_fiyat + yeni_toplam_otv_tutari)*kdv_orani,
                                yeni_fiyat = (net_fiyat+yeni_toplam_otv_tutari+yeni_kdv_tutari),
                                fark=yeni_fiyat-mevcut_fiyat,
                                yuzde_fiyat_degisimi=fark/mevcut_fiyat)


