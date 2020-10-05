
# Fiyat- MTV kiyaslamasi 

Binek_model_bazinda_fiyat_mtv_kiyasi <- data %>% filter(id%in% c(9296,9503,9559) , year==2021)  %>% 
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_toplam_fiyat = yeni_fiyat + indirimli_faiz_odemesi,
         mevcut_mtv = lifetime_mtv_15_yil,
         yeni_mtv = yeni_lifetime_mtv_co2_araliklari_15_yil,
         fark = yeni_toplam_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) 
Binek_model_bazinda_fiyat_mtv_kiyasi <- Binek_model_bazinda_fiyat_mtv_kiyasi %>% select(model,mevcut_toplam_fiyat,yeni_toplam_fiyat,fark,mtv_farki,sales)


#Binek_arac_fiyat_model_kiyasi<- 
Binek_arac_ortalama_MTV_fiyat_degisimi<- 
 data %>% filter( year==2021)  %>% #group_by(mevcut_otv_grubu) %>%
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_toplam_fiyat = hurda_tesvikli_fiyat + indirimli_faiz_odemesi,
         mevcut_mtv = lifetime_mtv_15_yil,
         yeni_mtv = yeni_lifetime_mtv_co2_araliklari_15_yil,
         fark = yeni_toplam_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) %>% summarise(satis_farki = weighted.mean(fark,kredi_indirimli_satis),
                  mtv_farki = weighted.mean(mtv_farki,kredi_indirimli_satis))

Binek_arac_ortalama_MTV_fiyat_degisimi$Arac_tipi <- "Binek Arac"


#LCV 

LCV_model_bazinda_fiyat_mtv_kiyasi <- lcv_data %>% filter(id%in% c(9748,10200) , year==2021)  %>% 
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_toplam_fiyat = hurda_tesvikli_fiyat + indirimli_faiz_odemesi,
         mevcut_mtv = lifetime_mtv_15_yil,
         yeni_mtv = yeni_lifetime_mtv_co2_araliklari_15_yil,
         fark = yeni_toplam_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) 
LCV_model_bazinda_fiyat_mtv_kiyasi <- LCV_model_bazinda_fiyat_mtv_kiyasi %>% select(model,mevcut_toplam_fiyat,yeni_toplam_fiyat,fark,mtv_farki,sales)



LCV_ortalama_MTV_fiyat_degisimi<- 
  lcv_data %>% filter(year==2021)  %>% 
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_toplam_fiyat = hurda_tesvikli_fiyat + indirimli_faiz_odemesi,
         mevcut_mtv = lifetime_mtv_15_yil,
         yeni_mtv = yeni_lifetime_mtv_co2_araliklari_15_yil,
         fark = yeni_toplam_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) %>% summarise(satis_farki = weighted.mean(fark,kredi_indirimli_satis),
                  mtv_farki = weighted.mean(mtv_farki,kredi_indirimli_satis))

LCV_ortalama_MTV_fiyat_degisimi$Arac_tipi <- "Hafif Ticari"
 
# HDV

HDV_ortalama_MTV_fiyat_degisimi<- 
  hdv_data %>% filter(year==2021)  %>% 
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_toplam_fiyat = hurda_tesvikli_fiyat + indirimli_faiz_odemesi,
         mevcut_mtv = lifetime_mtv_15_yil,
         yeni_mtv = arttirilmis_mtv_15_yil,
         fark = yeni_toplam_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) %>% summarise(satis_farki = weighted.mean(fark,kredi_indirimli_satis),
                  mtv_farki = weighted.mean(mtv_farki,kredi_indirimli_satis))

HDV_ortalama_MTV_fiyat_degisimi$Arac_tipi <- "Agir Ticari"

Segment_MTV_fiyat_degisimleri<- rbind(Binek_arac_ortalama_MTV_fiyat_degisimi,LCV_ortalama_MTV_fiyat_degisimi)
Segment_MTV_fiyat_degisimleri <- rbind(Segment_MTV_fiyat_degisimleri,HDV_ortalama_MTV_fiyat_degisimi)
Segment_MTV_fiyat_degisimleri <- Segment_MTV_fiyat_degisimleri[,c(3,1,2)]

fiyat_MTV_kiyasi_result_path <- paste(output_path,"Fiyat-MTV degisiklikleri kiyaslamasi",sep="/")
fiyat_MTV_kiyasi_result_path <- paste(fiyat_MTV_kiyasi_result_path,"xlsx",sep = ".")
export(Segment_MTV_fiyat_degisimleri,fiyat_MTV_kiyasi_result_path, which="Segment Ortalamalari")
export(Binek_model_bazinda_fiyat_mtv_kiyasi,fiyat_MTV_kiyasi_result_path, which="Binek model kiyasi")
export(LCV_model_bazinda_fiyat_mtv_kiyasi,fiyat_MTV_kiyasi_result_path, which="LCV model kiyasi")




Binek_arac_otv_grubu_MTV_fiyat_degisimi<- 
  data %>% filter( year==2021)  %>% group_by(mevcut_otv_grubu) %>%
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_toplam_fiyat = hurda_tesvikli_fiyat + indirimli_faiz_odemesi,
         mevcut_mtv = lifetime_mtv_15_yil,
         yeni_mtv = yeni_lifetime_mtv_co2_araliklari_15_yil,
         fark = yeni_toplam_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) %>% summarise(satis_farki = weighted.mean(fark,kredi_indirimli_satis),
                  mtv_farki = weighted.mean(mtv_farki,kredi_indirimli_satis))
export(Binek_arac_otv_grubu_MTV_fiyat_degisimi,fiyat_MTV_kiyasi_result_path, which="Binek OTV grubu kiyaslamasi")



