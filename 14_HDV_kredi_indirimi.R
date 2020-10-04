# kredi sistemi


#
hdv_data$kullanilan_kredi <- hdv_data$yeni_fiyat*hdv_kredi_orani

hdv_data <- hdv_data %>% mutate(avaliable_indirimli_kredi=case_when(
  segment %in% c( "midibus","kamyon")&uretim=="yerli" & kullanilan_kredi<  kamyon_max_indirimli_kredi_miktari ~ kullanilan_kredi              ,
  segment %in% c( "midibus","kamyon")&uretim=="yerli" & kullanilan_kredi>= kamyon_max_indirimli_kredi_miktari ~ kamyon_max_indirimli_kredi_miktari,
  segment =="otobus" &uretim=="yerli" & kullanilan_kredi<  otobus_max_indirimli_kredi_miktari ~ kullanilan_kredi              ,
  segment =="otobus" &uretim=="yerli" & kullanilan_kredi>= otobus_max_indirimli_kredi_miktari ~ otobus_max_indirimli_kredi_miktari,
  uretim=="ithal" ~ 0,
))



hdv_data$mevcut_toplam_kredi_odemesi <- hdv_data$kullanilan_kredi* ((1+mevcut_yillik_faiz)^hdv_ortalama_vade) 

hdv_data$mevcut_toplam_faiz_odemesi  <- hdv_data$mevcut_toplam_kredi_odemesi - hdv_data$kullanilan_kredi

hdv_data$mevcut_yillik_faiz_odemesi  <- hdv_data$mevcut_toplam_faiz_odemesi/hdv_ortalama_vade

hdv_data$indirimli_kredi_odemesi     <- hdv_data$avaliable_indirimli_kredi * ((1+indirimli_yillik_faiz)^hdv_ortalama_vade) +
  (hdv_data$kullanilan_kredi - hdv_data$avaliable_indirimli_kredi) *((1+mevcut_yillik_faiz)^hdv_ortalama_vade) # bu kisim toplam kullanacagi kredi indirimli kisimdan buyuk olanlar icin


hdv_data$indirimli_faiz_odemesi <- hdv_data$indirimli_kredi_odemesi-hdv_data$kullanilan_kredi

hdv_data$indirimli_yillik_faiz_odemesi <- hdv_data$indirimli_faiz_odemesi/hdv_ortalama_vade

hdv_data$yillik_faiz_farki <- hdv_data$mevcut_yillik_faiz_odemesi-hdv_data$indirimli_yillik_faiz_odemesi


# kredi indirimleri ile gelen satis fiyati
hdv_data <- hdv_data %>% mutate(kredi_maliyeti_degisimi        = indirimli_kredi_odemesi-mevcut_toplam_kredi_odemesi,
                                yuzde_kredi_maliyeti_degisimi  = kredi_maliyeti_degisimi / (hurda_tesvikli_fiyat+mevcut_toplam_faiz_odemesi),
                                kredi_kullanmayan_arac_miktari = hurda_tesvikli_satis_miktari*(1-kredi_kullanan_arac_orani),
                                kredi_kullanan_arac_miktari    = hurda_tesvikli_satis_miktari*kredi_kullanan_arac_orani,
                                kredi_indirimi_talep_etkisi    = kredi_kullanan_arac_miktari* ((yuzde_kredi_maliyeti_degisimi*kendi_esnekligi)),
                                kredi_indirimi_hurda_ve_ekstra = kredi_kullanan_arac_miktari + kredi_indirimi_talep_etkisi,
                                kredi_indirimli_satis          = round(kredi_kullanmayan_arac_miktari+kredi_indirimi_hurda_ve_ekstra)
)



