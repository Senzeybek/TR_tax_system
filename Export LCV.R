


LCV_vergi_gelirleri <- lcv_data %>% 
  group_by(year) %>% 
  summarise(
    Mecut_toplam_talep                 = sum(sales),
    OTV_ve_Hurda_toplam_talep          = sum(hurda_tesvikli_satis_miktari),
    OTV_Hurda_Kredi_toplam_talep       = sum(kredi_indirimli_satis),
    Mevcut_muhtemel_OTV_geliri         = sum(mevcut_otv_tutari * (sales))/milyar,
    Mevcut_muhtemel_KDV_geliri         = sum(mevcut_kdv_tutari * (sales))/milyar,
    Hurda_tesvikli_otv_geliri          = sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_OTV_tutari)/milyar, 
    Hurda_tesvikli_kdv_geliri          = sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_KDV_tutari )/milyar, 
    Kredi_indirimli_otv_geliri         = sum(kredi_indirimli_satis*hurda_tesvikli_OTV_tutari)/milyar, 
    Kredi_indirimli_kdv_geliri         = sum(kredi_indirimli_satis*hurda_tesvikli_KDV_tutari )/milyar, 
    Mevcut_muhtemel_MTV_geliri         = sum(sales*lifetime_mtv)/milyar,
    Hurda_tesvikli_mtv_geliri_sadece_co2      = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
    Hurda_tesvikli_mtv_geliri_co2_araliklari  = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_co2_araliklari )/milyar,
    Kredi_indirimli_mtv_geliri_sadece_co2     = sum(kredi_indirimli_satis*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
    Kredi_indirimli_mtv_geliri_co2_araliklari = sum(kredi_indirimli_satis*yeni_lifetime_mtv_co2_araliklari )/milyar,
    Mevcut_yakit_vergisi_OTV_KDV              = sum(sales*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
    OTV_ve_hurda_yakit_vergisi_OTV_KDV        = sum(hurda_tesvikli_satis_miktari*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
    OTV_hurda_finansman_yakit_vergisi_OTV_KDV = sum(kredi_indirimli_satis*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
    Kredi_faiz_indirimi_maliyeti              = -1*sum(yillik_faiz_farki*kredi_indirimi_hurda_ve_ekstra)/(milyar),
    Kredi_kullanan_arac_miktari               = sum(kredi_kullanan_arac_miktari),
    Kredi_indirimi_talep_etkisi               = sum(kredi_indirimi_talep_etkisi)  
  )


for (i in 2:5) {
  for (j in 11:18) {
    LCV_vergi_gelirleri[i,j] <- LCV_vergi_gelirleri[i,j]+
      LCV_vergi_gelirleri[i-1,j] 
  }
}


for (i in 2:5) {
  for (j in 19) {
    LCV_vergi_gelirleri[i,j] <- LCV_vergi_gelirleri[i,j]+
      LCV_vergi_gelirleri[i-1,j] 
  }
  if (i>=4){ # 2024 ve 2025 yilinda 3 yil oncesinin kredisi olmasin
    LCV_vergi_gelirleri[i,j] <- LCV_vergi_gelirleri[i,j] -
      LCV_vergi_gelirleri[i-3,j]
  }
  if (i>=5){ # 2025den 2022 yi cikarirken iki defa ayni isi yapmis oluyoruz onu duzeltiyorum
    LCV_vergi_gelirleri[i,j] <- LCV_vergi_gelirleri[i,j] +
      LCV_vergi_gelirleri[i-4,j]
  }
}

LCV_gelir_result_path <- paste(output_path,"Muhtemel LCV vergi gelirleri",sep="/")
LCV_gelir_result_path <- paste(LCV_gelir_result_path,"xlsx",sep = ".")
export(LCV_vergi_gelirleri,LCV_gelir_result_path)


# Butun verilerin toplandigi Ozet veriler

LCV_toplam_vergi <- LCV_vergi_gelirleri %>% mutate(
  Mecut_toplam_vergi= Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri+Mevcut_muhtemel_MTV_geliri+Mevcut_yakit_vergisi_OTV_KDV,
  
 
  Hurda_tesvikli_sadece_co2_toplam_vergi= 
    Hurda_tesvikli_otv_geliri + 
    Hurda_tesvikli_kdv_geliri+
    Hurda_tesvikli_mtv_geliri_sadece_co2+
    OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  
  Kredi_indirimli_sadece_co2_toplam_vergi=
    Kredi_indirimli_otv_geliri+
    Kredi_indirimli_kdv_geliri+
    Kredi_indirimli_mtv_geliri_sadece_co2+
    OTV_hurda_finansman_yakit_vergisi_OTV_KDV+
    Kredi_faiz_indirimi_maliyeti,
  
  Hurda_tesvikli_co2_araliklari_toplam_vergi= 
    Hurda_tesvikli_otv_geliri + 
    Hurda_tesvikli_kdv_geliri+
    Hurda_tesvikli_mtv_geliri_co2_araliklari+
    OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  
  Kredi_indirimli_co2_araliklari_toplam_vergi=
    Kredi_indirimli_otv_geliri+
    Kredi_indirimli_kdv_geliri+
    Kredi_indirimli_mtv_geliri_co2_araliklari+
    OTV_hurda_finansman_yakit_vergisi_OTV_KDV+
    Kredi_faiz_indirimi_maliyeti
  
) %>% 
  select(year,Mecut_toplam_vergi:Kredi_indirimli_co2_araliklari_toplam_vergi)


ozet_LCV_result_path <- paste(output_path,"Ozet LCV toplam vergi",sep="/")
ozet_LCV_result_path <- paste(ozet_LCV_result_path,"xlsx",sep = ".")
export(LCV_toplam_vergi,ozet_LCV_result_path,sheet="binek")




#  LCV summary tables ----

LCV_segment_summary <-  lcv_data %>% group_by(year,govde_tipi) %>% 
  summarise( mevcut_satis        = sum(sales,na.rm = T),
             yeni_otv_ortalamasi = weighted.mean(yeni_toplam_otv_orani, yeni_satis,na.rm=T),
             grup_yeni_satis          = sum(yeni_satis),
             grup_hurda_tesvikli_satis= sum(hurda_tesvikli_satis_miktari,na.rm=T),
             hurda_tesvikli_otv_ortalamasi = weighted.mean(hurda_tesvikli_otv_orani, hurda_tesvikli_satis_miktari,na.rm=T),
             mevcut_fiyat_ortalamasi = weighted.mean(mevcut_fiyat,sales,na.rm=T),
             yeni_fiyat_ortalamasi = weighted.mean(yeni_fiyat,yeni_satis,na.rm=T),
             hurda_fiyat_ortalamasi = weighted.mean(hurda_tesvikli_fiyat,hurda_tesvikli_satis_miktari,na.rm=T),
             kredi_fiyat_ortalamasi = weighted.mean(hurda_tesvikli_fiyat,kredi_indirimli_satis,na.rm=T),
             
  ) %>%
  mutate(eski_market_payi       = mevcut_satis/sum(mevcut_satis),
         yeni_market_payi       = grup_yeni_satis/sum(grup_yeni_satis),
         eski_toplam_talep      = sum(mevcut_satis),
         yeni_toplam_talep      = sum(grup_yeni_satis),
         hurda_tesviki_talep   = sum(grup_hurda_tesvikli_satis))


lcv_otv_grup_summary_path <- paste(output_path,"OTV gruplarindaki degisim ozeti",sep="/")
lcv_otv_grup_summary_path <- paste(lcv_otv_grup_summary_path,"xlsx",sep = ".")
export(LCV_segment_summary,lcv_otv_grup_summary_path)



# Yerli - Yabanci oranlari degisimi

LCV_uretim_grup_summary <-  lcv_data %>% group_by(year,uretim) %>% 
  summarise(mevcut_satis        = sum(sales,na.rm = T),
            yeni_otv_ortalamasi = weighted.mean(yeni_toplam_otv_orani, yeni_satis,na.rm=T),
            yeni_satis          = sum(yeni_satis),
            hurda_tesvikli_satis= sum(hurda_tesvikli_satis_miktari,na.rm=T),
            hurda_tesvikli_otv_ortalamasi = weighted.mean(hurda_tesvikli_otv_orani, hurda_tesvikli_satis_miktari,na.rm=T),
            kredi_indirimli_satis = sum(kredi_indirimli_satis)
  ) %>%
  mutate(eski_market_payi       = mevcut_satis/sum(mevcut_satis),
         yeni_market_payi       = yeni_satis/sum(yeni_satis),
         hurda_tesvikli_market_payi = hurda_tesvikli_satis/sum(hurda_tesvikli_satis),
         kredi_indirimli_market_payi = kredi_indirimli_satis/sum(kredi_indirimli_satis),
         eski_toplam_talep      = sum(mevcut_satis),
         yeni_toplam_talep      = sum(yeni_satis),
         hurda_tesviki_talep   = sum(hurda_tesvikli_satis))


LCV_uretim_grup_summary <- paste(output_path,"Yerli-Ithal degisim ozeti",sep="/")
LCV_uretim_grup_summary <- paste(LCV_uretim_grup_summary,"xlsx",sep = ".")
export(LCV_uretim_grup_summary,LCV_uretim_grup_summary)








# LCV secilen modellerin yazdirilmasi ----

yazilacak_id <- c("9271")
lcv_model_karsilastirma<-  lcv_data %>% filter(year==2021, id %in% yazilacak_id) %>% select(id,model,year,fiyat,yeni_fiyat,co2,yeni_toplam_otv_orani)


lcv_model_karsilastirma_path <- paste(output_path,"Secilen LCV modellerin karsilastirilmasi",sep="/")
lcv_model_karsilastirma_path <- paste(lcv_model_karsilastirma_path,"xlsx",sep = ".")
export(lcv_model_karsilastirma,lcv_model_karsilastirma_path)




# LCV 15 yillik mtv ----

lcv_uzun_donem_mtv <- lcv_data %>% group_by(year) %>% summarise(
  Toplam_lifetime_mtv_15_yil = sum(lifetime_mtv_15_yil * sales)/milyar,
  Toplam_yeni_lifetime_mtv_sadece_co2_15_yil = sum(yeni_lifetime_mtv_sadece_co2_15_yil * kredi_indirimli_satis)/milyar,
  Toplam_yeni_lifetime_mtv_co2_araliklari_15_yil = sum(yeni_lifetime_mtv_co2_araliklari_15_yil*kredi_indirimli_satis)/milyar
)

lcv_uzun_donem_mtv_path <- paste(output_path,"15 yillik LCV MTV gelirleri ",sep="/")
lcv_uzun_donem_mtv_path <- paste(lcv_uzun_donem_mtv_path,"xlsx",sep = ".")
export(lcv_uzun_donem_mtv,lcv_uzun_donem_mtv_path)











