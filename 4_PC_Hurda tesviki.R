# Hurda tesviki

# tesvik verilecek gruolar ve tesvik indirimi
data <- data %>% mutate(hurda_tesvikli_otv_orani= case_when(
  mevcut_otv_grubu==1 ~ yeni_toplam_otv_orani-yeni_arac_indirim_orani_grup1,
  mevcut_otv_grubu==2 ~ yeni_toplam_otv_orani-yeni_arac_indirim_orani_grup2,
  TRUE ~ yeni_toplam_otv_orani
  ))



# yeni arac fiyati hesaplanmasi
data$hurda_tesvikli_OTV_tutari <- data$net_fiyat*data$hurda_tesvikli_otv_orani

data$hurda_tesvikli_KDV_tutari <- (data$net_fiyat+data$hurda_tesvikli_OTV_tutari)*data$kdv_orani

data$hurda_tesvikli_fiyat <-      data$net_fiyat+data$hurda_tesvikli_OTV_tutari + data$hurda_tesvikli_KDV_tutari 

data$hurda_tesvikli_fiyat_degisimi <-      data$hurda_tesvikli_fiyat - data$yeni_fiyat


# tesvik sayesinde gelecek indirimin talep etkisi
data$hurda_tesvikli_fiyat_degisim_orani <- data$hurda_tesvikli_fiyat_degisimi/ data$yeni_fiyat

data$hurda_tesviki_talep_degisim_orani <- data$hurda_tesvikli_fiyat_degisim_orani*kendi_esnekligi

data$hurda_tesvikli_satis_miktari <- round(data$yeni_satis*(1+data$hurda_tesviki_talep_degisim_orani))

data$hurda_tesviki_ekstra_satis <- data$hurda_tesvikli_satis_miktari - data$yeni_satis

# Hurda tesviki sonrasinda vergi gelirleri
# Gelecek ekstra 86000 yeni arac talebi sayesinde OTVdeki kayip 2.5 milyar seviyesinde kaliyor.



