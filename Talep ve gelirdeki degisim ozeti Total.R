
Total_gecici <- data_frame(
  yil=Binek_gecici$yil,
  Mevcut_ongorulen_talep = 
    Binek_gecici$Ongorulen_talep+LCV_gecici$Ongorulen_talep+HDV_gecici$Ongorulen_talep,
  Binek=
    Binek_gecici$Toplam_binek-Binek_gecici$Ongorulen_talep,
  HTA=
    LCV_gecici$Toplam_LCV-LCV_gecici$Ongorulen_talep,
  ATA=
    HDV_gecici$Toplam_HDV-HDV_gecici$Ongorulen_talep,
  Toplam=
    Mevcut_ongorulen_talep+Binek+HTA+ATA,
  Hedeften_fark=
    1500000-Toplam,
  
  Devlet_gelirindeki_degisim_Binek=
    Binek_gecici$Devlet_gelirlerindeki_toplam_degisim_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu,
  Devlet_gelirindeki_degisim_HTA=
    LCV_gecici$Devlet_gelirlerindeki_toplam_degisim_OTV_gruplari_ve_CO2_araliklarina_gore_vergilendirme_senaryosu,
  Devlet_gelirindeki_degisim_ATA=
    HDV_gecici$Devlet_gelirlerindeki_toplam_degisim_arttirilmis_mtvye_gore_vergilendirme_senaryosu,
  Toplam_gelir_degisimi=
    Devlet_gelirindeki_degisim_Binek+Devlet_gelirindeki_degisim_HTA+Devlet_gelirindeki_degisim_ATA
  
)


Total_summary<-as.data.frame(t(Total_gecici[,2:ncol(Total_gecici)]))
# Set the column headings from the first column in the original table
colnames(Total_summary) <-  t(Total_gecici[,1] )

# rownameleri ayarladim
Total_summary$Degisken = colnames(Total_gecici[2:ncol(Total_gecici)])

Total_summary <- Total_summary[,c(6,1:5)]
Total_summary$Degisken<- str_replace_all(Total_summary$Degisken,"_"," ")


talep_gelir_result_path <- paste(output_path,"Talep ve gelirdeki degisim ozeti",sep="/")
talep_gelir_result_path <- paste(talep_gelir_result_path,"xlsx",sep = ".")
export(Total_summary,talep_gelir_result_path, which="Total")