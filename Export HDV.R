

#butun veri ---
hdv_data_result_path <- paste(output_path,"HDV butun veri",sep="/")
hdv_data_result_path <- paste(hdv_data_result_path,"xlsx",sep = ".")
export(hdv_data,hdv_data_result_path)



#vergi gelirleri detayli -----


HDV_vergi_gelirleri <- hdv_data %>% 
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
    Hurda_tesvikli_arttirilmis_mtv_geliri  =  sum(hurda_tesvikli_satis_miktari*arttirilmis_mtv )/milyar,
    Kredi_indirimli_mtv_geliri_sadece_co2     = sum(kredi_indirimli_satis*yeni_lifetime_mtv_sadece_co2 )/milyar,
    Kredi_indirimli_arttirilmis_mtv_geliri = sum(kredi_indirimli_satis*arttirilmis_mtv )/milyar,
    Mevcut_yakit_vergisi_OTV_KDV              = sum(sales*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
    OTV_ve_hurda_yakit_vergisi_OTV_KDV        = sum(hurda_tesvikli_satis_miktari*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
    OTV_hurda_finansman_yakit_vergisi_OTV_KDV = sum(kredi_indirimli_satis*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
    Kredi_faiz_indirimi_maliyeti              = -1*sum(yillik_faiz_farki*kredi_indirimi_hurda_ve_ekstra)/(milyar),
    Kredi_kullanan_arac_miktari               = sum(kredi_kullanan_arac_miktari),
    Kredi_indirimi_talep_etkisi               = sum(kredi_indirimi_talep_etkisi)  
  )


for (i in 2:5) {
  for (j in 11:18) {
    HDV_vergi_gelirleri[i,j] <- HDV_vergi_gelirleri[i,j]+
      HDV_vergi_gelirleri[i-1,j]
  }
}
#
#
for (i in 2:5) {
  for (j in 19) {
    HDV_vergi_gelirleri[i,j] <- HDV_vergi_gelirleri[i,j]+
      HDV_vergi_gelirleri[i-1,j]
  }
  if (i>=4){ # 2024 ve 2025 yilinda 3 yil oncesinin kredisi olmasin
    HDV_vergi_gelirleri[i,j] <- HDV_vergi_gelirleri[i,j] -
      HDV_vergi_gelirleri[i-3,j]
  }
  if (i>=5){ # 2025den 2022 yi cikarirken iki defa ayni isi yapmis oluyoruz onu duzeltiyorum
    HDV_vergi_gelirleri[i,j] <- HDV_vergi_gelirleri[i,j] +
      HDV_vergi_gelirleri[i-4,j]
  }
}

HDV_gelir_result_path <- paste(output_path,"HDV Muhtemel vergi gelirleri",sep="/")
HDV_gelir_result_path <- paste(HDV_gelir_result_path,"xlsx",sep = ".")
export(HDV_vergi_gelirleri,HDV_gelir_result_path)


# Butun verilerin toplandigi Ozet veriler

HDV_toplam_vergi <- HDV_vergi_gelirleri %>% mutate(
  Mecut_toplam_vergi= 
    Mevcut_muhtemel_OTV_geliri+
    Mevcut_muhtemel_KDV_geliri+
    Mevcut_muhtemel_MTV_geliri+
    Mevcut_yakit_vergisi_OTV_KDV,
  
  
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
  
  Hurda_tesvikli_arttirilmis_mtv_toplam_vergi=
    Hurda_tesvikli_otv_geliri +
    Hurda_tesvikli_kdv_geliri+
    Hurda_tesvikli_arttirilmis_mtv_geliri+
    OTV_ve_hurda_yakit_vergisi_OTV_KDV,

  Kredi_indirimli_arttirilmis_mtv_toplam_vergi=
    Kredi_indirimli_otv_geliri+
    Kredi_indirimli_kdv_geliri+
    Kredi_indirimli_arttirilmis_mtv_geliri+
    OTV_hurda_finansman_yakit_vergisi_OTV_KDV+
    Kredi_faiz_indirimi_maliyeti
  
) %>% 
  select(year,Mecut_toplam_vergi:Kredi_indirimli_arttirilmis_mtv_toplam_vergi)


ozet_HDV_result_path <- paste(output_path,"HDV Ozet toplam vergi",sep="/")
ozet_HDV_result_path <- paste(ozet_HDV_result_path,"xlsx",sep = ".")
export(HDV_toplam_vergi,ozet_HDV_result_path,sheet="binek")




# Yerli - Yabanci oranlari degisimi

HDV_uretim_grup_summary <-  hdv_data %>% group_by(year,uretim
                                                  ) %>% 
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


HDV_uretim_grup_summary_path <- paste(output_path,"HDV Yerli-Ithal degisim ozeti",sep="/")
HDV_uretim_grup_summary_path <- paste(HDV_uretim_grup_summary_path,"xlsx",sep = ".")
export(HDV_uretim_grup_summary,HDV_uretim_grup_summary_path)

# ithal-yerli_toplam_ozet
HDV_uretim_grup_summary$arac_tipi <- "HDV"
LCV_uretim_grup_summary$arac_tipi <- "LCV"
uretim_grup_summary$arac_tipi <- "PC"
yerli_ithal <-  rbind(HDV_uretim_grup_summary,LCV_uretim_grup_summary)
yerli_ithal <- rbind(yerli_ithal,uretim_grup_summary)
yerli_ithal_dagilimi_toplam <-  yerli_ithal %>% group_by(year,uretim) %>% summarise(mevcut_toplam=sum(mevcut_satis),
                                                    yeni_toplam = sum(yeni_satis),
                                                    hurda_tesvikli_satis_toplam= sum(hurda_tesvikli_satis),
                                                    kredi_indirimli_satis_toplam= sum(kredi_indirimli_satis)) %>% 
  mutate(mevcut_toplam_pay=mevcut_toplam/sum(mevcut_toplam),
         yeni_toplam_pay = yeni_toplam/sum(yeni_toplam),
         hurda_tesvikli_satis_toplam_pay = hurda_tesvikli_satis_toplam/sum(hurda_tesvikli_satis_toplam),
         kredi_indirimli_satis_toplam_pay = kredi_indirimli_satis_toplam/sum(kredi_indirimli_satis_toplam))

yerli_ithal_uretim_grup_summary_path <- paste(output_path,"Yerli-Ithal toplam Ozet",sep="/")
yerli_ithal_uretim_grup_summary_path <- paste(yerli_ithal_uretim_grup_summary_path,"xlsx",sep = ".")
export(yerli_ithal_dagilimi_toplam,yerli_ithal_uretim_grup_summary_path)




# HDV secilen modellerin yazdirilmasi ----

# yazilacak_id <- c("9271")
# hdv_model_karsilastirma<-  hdv_data %>% filter(year==2021, id %in% yazilacak_id) %>% select(id,model,year,fiyat,yeni_fiyat,co2,yeni_toplam_otv_orani)
# 
# 
# hdv_model_karsilastirma_path <- paste(output_path,"HDV Secilen modellerin karsilastirilmasi",sep="/")
# hdv_model_karsilastirma_path <- paste(hdv_model_karsilastirma_path,"xlsx",sep = ".")
# export(hdv_model_karsilastirma,hdv_model_karsilastirma_path)




# HDV 15 yillik mtv ----

hdv_uzun_donem_mtv <- hdv_data %>% group_by(year) %>% summarise(
  Toplam_lifetime_mtv_15_yil = sum(lifetime_mtv_15_yil * sales)/milyar,
  Toplam_yeni_lifetime_mtv_sadece_co2_15_yil = sum(yeni_lifetime_mtv_sadece_co2_15_yil * kredi_indirimli_satis)/milyar,
  #Toplam_yeni_lifetime_mtv_co2_araliklari_15_yil = sum(yeni_lifetime_mtv_co2_araliklari_15_yil*kredi_indirimli_satis)/milyar
)

hdv_uzun_donem_mtv_path <- paste(output_path,"HDV 15 yillik MTV gelirleri ",sep="/")
hdv_uzun_donem_mtv_path <- paste(hdv_uzun_donem_mtv_path,"xlsx",sep = ".")
export(hdv_uzun_donem_mtv,hdv_uzun_donem_mtv_path)



# Agirlikli MTV ortalamalari ----

HDV_MTV_ortalamalari <- hdv_data %>% filter(year==2021) %>%
  summarise(
    Mevcut_MTV_ortalama = weighted.mean(lifetime_mtv,sales),
    Hurda_tesvikli_sadece_co2_ortalama_MTV = weighted.mean(yeni_mtv_sadece_co2,hurda_tesvikli_satis_miktari),
    # Hurda_tesvikli_CO2_araliklarina_dayali_MTV_ortalama= weighted.mean(yeni_lifetime_mtv_co2_araliklari,hurda_tesvikli_satis_miktari),
    Kredi_indirimli_sadece_co2_ortalama_MTV = weighted.mean(yeni_mtv_sadece_co2,kredi_indirimli_satis),
    # Kredi_indirimli_CO2_araliklarina_dayali_MTV_ortalama= weighted.mean(yeni_lifetime_mtv_co2_araliklari,kredi_indirimli_satis)
  )
# 
HDV_MTV_ortalamalari_path <- paste(output_path,"HDV MTV ortalamalari",sep="/")
HDV_MTV_ortalamalari_path <- paste(HDV_MTV_ortalamalari_path,"xlsx",sep = ".")
export(HDV_MTV_ortalamalari,HDV_MTV_ortalamalari_path)



# MTV ve CO2 araliklari dagilimi  -----

# hdv_mtv_co2_dagilimi <-  hdv_data %>% group_by(year,mtv_grubu,co2_grubu) %>% 
#   summarise(mevcut_satis=sum(sales), toplam_satis=sum(kredi_indirimli_satis),agirlikli_co2_emisyonu=weighted.mean(co2,kredi_indirimli_satis))
# 
# hdv_mtv_co2_dagilimi_result_path <- paste(output_path,"HDV MTV CO2 gruplari dagilimi",sep="/")
# hdv_mtv_co2_dagilimi_result_path <- paste(hdv_mtv_co2_dagilimi_result_path,"xlsx",sep = ".")
# export(hdv_mtv_co2_dagilimi,hdv_mtv_co2_dagilimi_result_path)




