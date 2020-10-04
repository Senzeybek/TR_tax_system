

Toplam_sonuclar <- data_frame(
  
  year=Binek_arac_vergi_gelirleri$year,
  
  Mevcut_toplam_talep = 
    Binek_arac_vergi_gelirleri$Mecut_toplam_talep+
    LCV_vergi_gelirleri$Mecut_toplam_talep+
    HDV_vergi_gelirleri$Mecut_toplam_talep,
  
  Sadece_OTV_indirimli_toplam_talep= # Sadece binek aracta var
    Binek_arac_vergi_gelirleri$Sadece_OTV_toplam_talep+
    LCV_vergi_gelirleri$Mecut_toplam_talep+
    HDV_vergi_gelirleri$Mecut_toplam_talep,
  
  Hurda_tesvikli_toplam_talep= # OTV de dahil
    Binek_arac_vergi_gelirleri$OTV_ve_Hurda_toplam_talep+
    LCV_vergi_gelirleri$OTV_ve_Hurda_toplam_talep+
    HDV_vergi_gelirleri$OTV_ve_Hurda_toplam_talep,
  
  Kredi_indirimli_toplam_talep =
    Binek_arac_vergi_gelirleri$OTV_Hurda_Kredi_toplam_talep+
    LCV_vergi_gelirleri$OTV_Hurda_Kredi_toplam_talep+
    HDV_vergi_gelirleri$OTV_Hurda_Kredi_toplam_talep,
  
  Mevcut_toplam_vergi_geliri=
    Binek_arac_toplam_vergi$Mecut_toplam_vergi+
    LCV_toplam_vergi$Mecut_toplam_vergi+
    HDV_toplam_vergi$Mecut_toplam_vergi,
  
  Sadece_OTV_indirimli_toplam_vergi_geliri=
    Binek_arac_toplam_vergi$Sadece_OTV_sadece_co2_toplam_vergi+
    LCV_toplam_vergi$Mecut_toplam_vergi+
    HDV_toplam_vergi$Mecut_toplam_vergi,
  
  Hurda_tesvikli_toplam_vergi_geliri =
    Binek_arac_toplam_vergi$Hurda_tesvikli_sadece_co2_toplam_vergi+
    LCV_toplam_vergi$Hurda_tesvikli_sadece_co2_toplam_vergi+
    HDV_toplam_vergi$Hurda_tesvikli_sadece_co2_toplam_vergi,
  
  Kredi_indirimli_toplam_vergi_geliri = 
    Binek_arac_toplam_vergi$Kredi_indirimli_sadece_co2_toplam_vergi+
    LCV_toplam_vergi$Kredi_indirimli_sadece_co2_toplam_vergi+
    HDV_toplam_vergi$Kredi_indirimli_sadece_co2_toplam_vergi,
  
) %>% mutate(
  Sadece_OTV_indirimli_vergi_degisimi= 
    Sadece_OTV_indirimli_toplam_vergi_geliri - Mevcut_toplam_vergi_geliri,
  
  Hurda_tesvikli_vergi_degisimi=
    Hurda_tesvikli_toplam_vergi_geliri - Mevcut_toplam_vergi_geliri,
  
  Kredi_indirimli_vergi_degisimi=
    Kredi_indirimli_toplam_vergi_geliri - Mevcut_toplam_vergi_geliri
  
  
) %>% 
  select(Mevcut_toplam_talep,Kredi_indirimli_toplam_talep,Kredi_indirimli_vergi_degisimi)


toplam_sonuclar_result_path <- paste(output_path,"Toplam Sonuclar",sep="/")
toplam_sonuclar_result_path <- paste(toplam_sonuclar_result_path,"xlsx",sep = ".")
export(Toplam_sonuclar,toplam_sonuclar_result_path)
