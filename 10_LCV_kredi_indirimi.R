# kredi sistemi

lcv_data$kullanilan_kredi <- lcv_data$yeni_fiyat*lcv_kredi_orani

lcv_data <- lcv_data %>% mutate(avaliable_indirimli_kredi=case_when(
  uretim=="yerli" & kullanilan_kredi<  lcv_max_indirimli_kredi_miktari ~ kullanilan_kredi              ,
  uretim=="yerli" & kullanilan_kredi>= lcv_max_indirimli_kredi_miktari ~ lcv_max_indirimli_kredi_miktari,
  uretim=="ithal" ~ 0,
))


lcv_data$mevcut_toplam_kredi_odemesi <- lcv_data$kullanilan_kredi* ((1+mevcut_yillik_faiz)^lcv_ortalama_vade) 

lcv_data$mevcut_toplam_faiz_odemesi  <- lcv_data$mevcut_toplam_kredi_odemesi - lcv_data$kullanilan_kredi

lcv_data$mevcut_yillik_faiz_odemesi  <- lcv_data$mevcut_toplam_faiz_odemesi/lcv_ortalama_vade

lcv_data$indirimli_kredi_odemesi     <- lcv_data$avaliable_indirimli_kredi * ((1+indirimli_yillik_faiz)^lcv_ortalama_vade) +
  (lcv_data$kullanilan_kredi - lcv_data$avaliable_indirimli_kredi) *((1+mevcut_yillik_faiz)^lcv_ortalama_vade) # bu kisim toplam kullanacagi kredi indirimli kisimdan buyuk olanlar icin


lcv_data$indirimli_faiz_odemesi <- lcv_data$indirimli_kredi_odemesi-lcv_data$kullanilan_kredi

lcv_data$indirimli_yillik_faiz_odemesi <- lcv_data$indirimli_faiz_odemesi/lcv_ortalama_vade

lcv_data$yillik_faiz_farki <- lcv_data$mevcut_yillik_faiz_odemesi-lcv_data$indirimli_yillik_faiz_odemesi


# kredi indirimleri ile gelen satis fiyati
lcv_data <- lcv_data %>% mutate(kredi_maliyeti_degisimi        = indirimli_kredi_odemesi-mevcut_toplam_kredi_odemesi,
                        yuzde_kredi_maliyeti_degisimi  = kredi_maliyeti_degisimi / (hurda_tesvikli_fiyat+mevcut_toplam_faiz_odemesi),
                        kredi_kullanmayan_arac_miktari = hurda_tesvikli_satis_miktari*(1-kredi_kullanan_arac_orani),
                        kredi_kullanan_arac_miktari    = hurda_tesvikli_satis_miktari*kredi_kullanan_arac_orani,
                        kredi_indirimi_talep_etkisi    = kredi_kullanan_arac_miktari* ((yuzde_kredi_maliyeti_degisimi*kendi_esnekligi)),
                        kredi_indirimi_hurda_ve_ekstra = kredi_kullanan_arac_miktari + kredi_indirimi_talep_etkisi,
                        kredi_indirimli_satis          = round(kredi_kullanmayan_arac_miktari+kredi_indirimi_hurda_ve_ekstra)
)



