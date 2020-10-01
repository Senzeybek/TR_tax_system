

# tesvik verilecek gruolar ve tesvik indirimi
hdv_data$yeni_satis <- hdv_data$sales

hdv_data <- hdv_data %>% mutate(hurda_tesvikli_otv_orani= yeni_toplam_otv_orani- HDV_hurda_tesvik_orani)



# yeni arac fiyati hesaplanmasi ----
hdv_data$hurda_tesvikli_OTV_tutari <- hdv_data$net_fiyat*hdv_data$hurda_tesvikli_otv_orani

hdv_data$hurda_tesvikli_KDV_tutari <- (hdv_data$net_fiyat)*hdv_data$kdv_orani # OTV negatif olacagi icin KDV hesaplamasina katilmiyor

hdv_data$hurda_tesvikli_fiyat      <- hdv_data$net_fiyat+hdv_data$hurda_tesvikli_OTV_tutari + hdv_data$hurda_tesvikli_KDV_tutari 

hdv_data$hurda_tesvikli_fiyat_degisimi <- hdv_data$hurda_tesvikli_fiyat - hdv_data$yeni_fiyat


# tesvik sayesinde gelecek indirimin talep etkisi
hdv_data$hurda_tesvikli_fiyat_degisim_orani <- hdv_data$hurda_tesvikli_fiyat_degisimi/ hdv_data$yeni_fiyat

hdv_data$hurda_tesviki_talep_degisim_orani  <- hdv_data$hurda_tesvikli_fiyat_degisim_orani*lcv_esneklik

hdv_data$hurda_tesvikli_satis_miktari       <- round(hdv_data$yeni_satis*(1+hdv_data$hurda_tesviki_talep_degisim_orani))

hdv_data$hurda_tesviki_ekstra_satis         <- hdv_data$hurda_tesvikli_satis_miktari - hdv_data$yeni_satis


