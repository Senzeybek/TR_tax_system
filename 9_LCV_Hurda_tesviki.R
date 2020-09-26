

# tesvik verilecek gruolar ve tesvik indirimi
lcv_data$yeni_satis <- lcv_data$sales

lcv_data <- lcv_data %>% mutate(hurda_tesvikli_otv_orani= yeni_toplam_otv_orani- LCV_hurda_tesvik_orani)



# yeni arac fiyati hesaplanmasi ----
lcv_data$hurda_tesvikli_OTV_tutari <- lcv_data$net_fiyat*lcv_data$hurda_tesvikli_otv_orani

lcv_data$hurda_tesvikli_KDV_tutari <- (lcv_data$net_fiyat)*lcv_data$kdv_orani # OTV negatif olacagi icin KDV hesaplamasina katilmiyor

lcv_data$hurda_tesvikli_fiyat <-      lcv_data$net_fiyat+lcv_data$hurda_tesvikli_OTV_tutari + lcv_data$hurda_tesvikli_KDV_tutari 

lcv_data$hurda_tesvikli_fiyat_degisimi <-      lcv_data$hurda_tesvikli_fiyat - lcv_data$yeni_fiyat


# tesvik sayesinde gelecek indirimin talep etkisi
lcv_data$hurda_tesvikli_fiyat_degisim_orani <- lcv_data$hurda_tesvikli_fiyat_degisimi/ lcv_data$yeni_fiyat

lcv_data$hurda_tesviki_talep_degisim_orani <- lcv_data$hurda_tesvikli_fiyat_degisim_orani*lcv_esneklik

lcv_data$hurda_tesvikli_satis_miktari <- round(lcv_data$yeni_satis*(1+lcv_data$hurda_tesviki_talep_degisim_orani))

lcv_data$hurda_tesviki_ekstra_satis <- lcv_data$hurda_tesvikli_satis_miktari - lcv_data$yeni_satis

# yaklasik %39 civarinda ekstra bir LCV satisi bekleniyor.

