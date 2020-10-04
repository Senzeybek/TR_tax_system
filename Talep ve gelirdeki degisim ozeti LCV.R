

# LCV 
LCV_gecici <- LCV_vergi_gelirleri %>% 
  
  mutate(
    yil=year,
    #LCV segmentinde talep degisimi ----
    Ongorulen_talep = Mecut_toplam_talep,
    Hurda_etkisi = OTV_ve_Hurda_toplam_talep - Mecut_toplam_talep,
    Finansman_etkisi = OTV_Hurda_Kredi_toplam_talep - OTV_ve_Hurda_toplam_talep,
    Toplam_LCV = OTV_Hurda_Kredi_toplam_talep,
    
    # MTV senaryolarina gore devlet gelirlerindeki toplam degisim  ---- 
    LCV_vergi_degisimi_Sadece_CO2ye_gore_vergilendirme_senaryosu =
      LCV_toplam_vergi$Kredi_indirimli_sadece_co2_toplam_vergi -
      LCV_toplam_vergi$Mecut_toplam_vergi,
    LCV_vergi_degisimi_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu =
      LCV_toplam_vergi$Kredi_indirimli_co2_araliklari_toplam_vergi-
      LCV_toplam_vergi$Mecut_toplam_vergi,
    
    #Modele tanitilan sistemler bazindaki etkilerin incelenmesi----

    #Hurda
    OTV_ve_hurda_uygulandiktan_sonra_devlet_gelirlerindeki_degisim = 
      (Hurda_tesvikli_otv_geliri+Hurda_tesvikli_kdv_geliri) -
      (Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri),
    #Finansman etkisi
    Faiz_yukunu_katmadan_OTV_Hurda_ve_finansman_kampanya_sonrasi_devlet_gelirleri=
      (Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri)-
      (Hurda_tesvikli_otv_geliri+Hurda_tesvikli_kdv_geliri),
    
    Faiz_yuku=
      Kredi_faiz_indirimi_maliyeti,
    Finansman_etkisi_Devlet_gelirlerindeki_degisim=
      Faiz_yukunu_katmadan_OTV_Hurda_ve_finansman_kampanya_sonrasi_devlet_gelirleri+
      Faiz_yuku,
    
    #MTV etkisi
    MTV_degisimi_Sadece_CO2ye_gore_vergilendirme_senaryosu=
      Kredi_indirimli_mtv_geliri_sadece_co2-
      Mevcut_muhtemel_MTV_geliri,
    
    Devlet_gelirlerindeki_degisim_Sadece_CO2ye_gore_vergilendirme_senaryosu =
      ((Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+ Kredi_faiz_indirimi_maliyeti)-
         (Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri))+
      MTV_degisimi_Sadece_CO2ye_gore_vergilendirme_senaryosu,
    

    
    MTV_degisimi_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu =
      Kredi_indirimli_mtv_geliri_co2_araliklari-
      Mevcut_muhtemel_MTV_geliri,
    
    Devlet_gelirlerindeki_degisim_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu =
      ((Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+ Kredi_faiz_indirimi_maliyeti)-
         (Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri))+
      MTV_degisimi_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu,
    
    #Yakit etkisi ----
    Yakit_gelirlerindeki_degisim = 
      OTV_hurda_finansman_yakit_vergisi_OTV_KDV-
      Mevcut_yakit_vergisi_OTV_KDV,
    
    Devlet_gelirlerindeki_toplam_degisim_Sadece_CO2ye_gore_vergilendirme_senaryosu =
      Devlet_gelirlerindeki_degisim_Sadece_CO2ye_gore_vergilendirme_senaryosu+
      Yakit_gelirlerindeki_degisim,
    
    
    Devlet_gelirlerindeki_toplam_degisim_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu=
      Devlet_gelirlerindeki_degisim_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu+
      Yakit_gelirlerindeki_degisim

  )  
LCV_gecici <- LCV_gecici %>% select(yil:ncol(LCV_gecici)) 

LCV_summary<-as.data.frame(t(LCV_gecici[,2:ncol(LCV_gecici)]))
# Set the column headings from the first column in the original table
colnames(LCV_summary) <-  t(LCV_gecici[,1] )

# rownameleri ayarladim
LCV_summary$Gelir = colnames(LCV_gecici[2:ncol(LCV_gecici)])

LCV_summary <- LCV_summary[,c(6,1:5)]
LCV_summary$Gelir<- str_replace_all(LCV_summary$Gelir,"_"," ")


talep_gelir_result_path <- paste(output_path,"Talep ve gelirdeki degisim ozeti",sep="/")
talep_gelir_result_path <- paste(talep_gelir_result_path,"xlsx",sep = ".")
export(LCV_summary,talep_gelir_result_path, which="HTA summary")