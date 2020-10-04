
# Fiyat- MTV kiyaslamasi 

fiyat_mtv_kiyasi <- data %>% filter(id%in% c(9296,9503,9559) , year==2021)  %>% 
  mutate(mevcut_toplam_fiyat=
           mevcut_fiyat+mevcut_toplam_faiz_odemesi,
         yeni_fiyat_tesvikli = hurda_tesvikli_fiyat + indirimli_faiz_odemesi,
             mevcut_mtv = lifetime_mtv_15_yil,
          yeni_mtv = yeni_lifetime_mtv_co2_araliklari_15_yil,
         fark = yeni_fiyat - mevcut_toplam_fiyat,
         mtv_farki = yeni_mtv - mevcut_mtv
  ) 
fiyat_mtv_kiyasi_ozet<-  fiyat_mtv_kiyasi %>% select(model,mevcut_fiyat,yeni_fiyat,hurda_tesvikli_fiyat,fark,mtv_farki)

fiyat_mtv_result_path <- paste(output_path,"Fiyat MTV kiyaslamasi",sep="/")
fiyat_mtv_result_path <- paste(fiyat_mtv_result_path,"xlsx",sep = ".")
export(fiyat_mtv_kiyasi_ozet,fiyat_mtv_result_path)
