# TCO hesaplanmasi

data <- data %>% mutate(
  mevcut_tco =mevcut_fiyat + lifetime_mtv + mevcut_toplam_faiz_odemesi,
  yeni_tco_with_hurda_kredi = hurda_tesvikli_fiyat + yeni_lifetime_mtv_co2_araliklari + indirimli_faiz_odemesi) 
  

data$tco_degisim_orani <- (data$yeni_tco_with_hurda_kredi-data$mevcut_tco)/data$mevcut_tco

data$tco_yeni_talep <- round((1+(data$tco_degisim_orani*tco_esnekligi))*data$satis_2020)

