# kredi sistemi

data$kullanilan_kredi <- data$hurda_tesvikli_fiyat*kredi_orani

data <- data %>% mutate(avaliable_indirimli_kredi=case_when(
  uretim=="yerli" & kullanilan_kredi<  max_indirimli_kredi_miktari ~ kullanilan_kredi              ,
  uretim=="yerli" & kullanilan_kredi>= max_indirimli_kredi_miktari ~ max_indirimli_kredi_miktari,
  uretim=="ithal" ~ 0,
))


data$mevcut_toplam_kredi_odemesi <- data$kullanilan_kredi* ((1+mevcut_yillik_faiz)^ortalama_vade) 

data$mevcut_toplam_faiz_odemesi <- data$mevcut_toplam_kredi_odemesi - data$kullanilan_kredi

data$mevcut_yillik_faiz_odemesi <- data$mevcut_toplam_faiz_odemesi/ortalama_vade

data$indirimli_kredi_odemesi <- data$avaliable_indirimli_kredi * ((1+indirimli_yillik_faiz)^ortalama_vade) +
  (data$kullanilan_kredi - data$avaliable_indirimli_kredi) *((1+mevcut_yillik_faiz)^ortalama_vade) # bu kisim toplam kullanacagi kredi indirimli kisimdan buyuk olanlar icin


data$indirimli_faiz_odemesi <- data$indirimli_kredi_odemesi-data$kullanilan_kredi

data$indirimli_yillik_faiz_odemesi <- data$indirimli_faiz_odemesi/ortalama_vade

data$yillik_faiz_farki <- data$mevcut_yillik_faiz_odemesi-data$indirimli_yillik_faiz_odemesi


# kredi indirimleri ile gelen satis fiyati
data <- data %>% mutate(kredi_maliyeti_degisimi        = indirimli_kredi_odemesi-mevcut_toplam_kredi_odemesi,
                        yuzde_kredi_maliyeti_degisimi  = kredi_maliyeti_degisimi / (hurda_tesvikli_fiyat+mevcut_toplam_faiz_odemesi),
                        kredi_kullanmayan_arac_miktari = hurda_tesvikli_satis_miktari*(1-kredi_kullanan_arac_orani),
                        kredi_kullanan_arac_miktari    = hurda_tesvikli_satis_miktari*kredi_kullanan_arac_orani,
                        kredi_indirimi_talep_etkisi    = kredi_kullanan_arac_miktari* ((yuzde_kredi_maliyeti_degisimi*kendi_esnekligi)),
                        kredi_indirimi_hurda_ve_ekstra = kredi_kullanan_arac_miktari + kredi_indirimi_talep_etkisi,
                        kredi_indirimli_satis          = round(kredi_kullanmayan_arac_miktari+kredi_indirimi_hurda_ve_ekstra)
)
                                                        
                                                        



# data$kredi_indirimli_satis <- ifelse(data$yillik_faiz_farki>0,
#   round(data$hurda_tesvikli_satis_miktari*(1+ (mevcut_yillik_faiz- indirimli_yillik_faiz)*kredi_esnekligi)),
#   data$hurda_tesvikli_satis_miktari)


