

# HDV 
HDV_gecici <- HDV_vergi_gelirleri %>% 
  
  mutate(
    yil=year,
    #HDV segmentinde talep degisimi ----
    Ongorulen_talep = Mecut_toplam_talep,
    Hurda_etkisi = OTV_ve_Hurda_toplam_talep - Mecut_toplam_talep,
    Finansman_etkisi = OTV_Hurda_Kredi_toplam_talep - OTV_ve_Hurda_toplam_talep,
    Toplam_HDV = OTV_Hurda_Kredi_toplam_talep,
    
    # MTV senaryolarina gore devlet gelirlerindeki toplam degisim  ---- 
    HDV_vergi_degisimi_Sadece_CO2ye_gore_vergilendirme_senaryosu =
      HDV_toplam_vergi$Kredi_indirimli_sadece_co2_toplam_vergi -
      HDV_toplam_vergi$Mecut_toplam_vergi,
    HDV_vergi_degisimi_arttirilmis_mtvye_gore_vergilendirme_senaryosu =
      HDV_toplam_vergi$Kredi_indirimli_arttirilmis_mtv_toplam_vergi-
      HDV_toplam_vergi$Mecut_toplam_vergi,
    
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
    
    
    
    MTV_degisimi_arttirilmis_mtvye_gore_vergilendirme_senaryosu =
      HDV_vergi_gelirleri$Kredi_indirimli_arttirilmis_mtv_geliri-
      Mevcut_muhtemel_MTV_geliri,
    
    Devlet_gelirlerindeki_degisim_arttirilmis_mtvye_gore_vergilendirme_senaryosu =
      ((Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri + Kredi_faiz_indirimi_maliyeti)-
         (Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri))+
      MTV_degisimi_arttirilmis_mtvye_gore_vergilendirme_senaryosu,
    
    #Yakit etkisi ----
    Yakit_gelirlerindeki_degisim = 
      OTV_hurda_finansman_yakit_vergisi_OTV_KDV-
      Mevcut_yakit_vergisi_OTV_KDV,
    
    Devlet_gelirlerindeki_toplam_degisim_Sadece_CO2ye_gore_vergilendirme_senaryosu =
      Devlet_gelirlerindeki_degisim_Sadece_CO2ye_gore_vergilendirme_senaryosu+
      Yakit_gelirlerindeki_degisim,
    
    Devlet_gelirlerindeki_toplam_degisim_arttirilmis_mtvye_gore_vergilendirme_senaryosu =
      Devlet_gelirlerindeki_degisim_arttirilmis_mtvye_gore_vergilendirme_senaryosu+
      Yakit_gelirlerindeki_degisim,
  )  
HDV_gecici <- HDV_gecici %>% select(yil:ncol(HDV_gecici)) 

HDV_summary<-as.data.frame(t(HDV_gecici[,2:ncol(HDV_gecici)]))
# Set the column headings from the first column in the original table
colnames(HDV_summary) <-  t(HDV_gecici[,1] )

# rownameleri ayarladim
HDV_summary$Gelir = colnames(HDV_gecici[2:ncol(HDV_gecici)])

HDV_summary <- HDV_summary[,c(6,1:5)]
HDV_summary$Gelir<- str_replace_all(HDV_summary$Gelir,"_"," ")


talep_gelir_result_path <- paste(output_path,"Talep ve gelirdeki degisim ozeti",sep="/")
talep_gelir_result_path <- paste(talep_gelir_result_path,"xlsx",sep = ".")
export(HDV_summary,talep_gelir_result_path, which="ATA summary")
