# kredi sistemi

lcv_data$kullanilan_kredi <- lcv_data$yeni_fiyat*lcv_kredi_orani

lcv_data <- lcv_data %>% mutate(avaliable_indirimli_kredi=case_when(
  uretim=="yerli" & kullanilan_kredi<  lcv_max_indirimli_kredi_miktari ~ kullanilan_kredi              ,
  uretim=="yerli" & kullanilan_kredi>= lcv_max_indirimli_kredi_miktari ~ max_indirimli_kredi_miktari,
  uretim=="ithal" ~ 0,
))


lcv_data$mevcut_toplam_kredi_odemesi <- lcv_data$kullanilan_kredi* ((1+mevcut_yillik_faiz)^lcv_ortalama_vade) 

lcv_data$mevcut_toplam_faiz_odemesi <- lcv_data$mevcut_toplam_kredi_odemesi - lcv_data$kullanilan_kredi

lcv_data$mevcut_yillik_faiz_odemesi <- lcv_data$mevcut_toplam_faiz_odemesi/lcv_ortalama_vade

lcv_data$indirimli_kredi_odemesi <- lcv_data$avaliable_indirimli_kredi * ((1+indirimli_yillik_faiz)^lcv_ortalama_vade) +
  (lcv_data$kullanilan_kredi - lcv_data$avaliable_indirimli_kredi) *((1+mevcut_yillik_faiz)^lcv_ortalama_vade) # bu kisim toplam kullanacagi kredi indirimli kisimdan buyuk olanlar icin


lcv_data$indirimli_faiz_odemesi <- lcv_data$indirimli_kredi_odemesi-lcv_data$kullanilan_kredi

lcv_data$indirimli_yillik_faiz_odemesi <- lcv_data$indirimli_faiz_odemesi/lcv_ortalama_vade

lcv_data$yillik_faiz_farki <- lcv_data$mevcut_yillik_faiz_odemesi-lcv_data$indirimli_yillik_faiz_odemesi

