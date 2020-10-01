dir.create(file.path("r_output"))
output_path <- paste("Senaryo sonuclari", Sys.time(), sep = "_")
output_path <- str_replace_all(output_path,":","-")
output_path <- paste("r_output/", output_path, sep = "")
dir.create(output_path)



data_result_path <- paste(output_path,"Binek arac butun veri",sep="/")
data_result_path <- paste(data_result_path,"xlsx",sep = ".")
export(data,data_result_path)


#  Gelir hesaplamasi ----

#  Binek araclar ----


Binek_arac_vergi_gelirleri <- data %>% 
  group_by(year) %>% 
  summarise(
          Mecut_toplam_talep                 = sum(sales),
          Sadece_OTV_toplam_talep            = sum(yeni_satis),
          OTV_ve_Hurda_toplam_talep          = sum(hurda_tesvikli_satis_miktari),
          OTV_Hurda_Kredi_toplam_talep       = sum(kredi_indirimli_satis),
          Mevcut_muhtemel_OTV_geliri         = sum(mevcut_otv_tutari * (sales))/milyar,
          Mevcut_muhtemel_KDV_geliri         = sum(mevcut_kdv_tutari * (sales))/milyar,
          Yeni_toplam_OTV_geliri             = sum(yeni_toplam_otv_tutari * (yeni_satis))/milyar,
          Yeni_toplam_KDV_geliri             = sum(yeni_kdv_tutari * (yeni_satis))/milyar,
          Hurda_tesvikli_otv_geliri          = sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_OTV_tutari)/milyar, 
          Hurda_tesvikli_kdv_geliri          = sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_KDV_tutari )/milyar, 
          Kredi_indirimli_otv_geliri         = sum(kredi_indirimli_satis*hurda_tesvikli_OTV_tutari)/milyar, 
          Kredi_indirimli_kdv_geliri         = sum(kredi_indirimli_satis*hurda_tesvikli_KDV_tutari )/milyar, 
          Mevcut_muhtemel_MTV_geliri         = sum(sales*lifetime_mtv)/milyar,
          Yeni_muhtemel_MTV_geliri_sadece_co2       = sum(yeni_satis*yeni_lifetime_mtv_sadece_co2)/milyar,
          Yeni_muhtemel_MTV_geliri_otv_grubu_co2    = sum(yeni_satis*yeni_lifetime_mtv_otv_grubu_co2)/milyar,
          Yeni_muhtemel_MTV_geliri_co2_araliklari   = sum(yeni_satis*yeni_lifetime_mtv_co2_araliklari)/milyar,
          Hurda_tesvikli_mtv_geliri_sadece_co2      = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
          Hurda_tesvikli_mtv_geliri_otv_grubu_co2   = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_otv_grubu_co2 )/milyar,
          Hurda_tesvikli_mtv_geliri_co2_araliklari  = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_co2_araliklari )/milyar,
          Kredi_indirimli_mtv_geliri_sadece_co2     = sum(kredi_indirimli_satis*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
          Kredi_indirimli_mtv_geliri_otv_grubu_co2  = sum(kredi_indirimli_satis*yeni_lifetime_mtv_otv_grubu_co2 )/milyar,
          Kredi_indirimli_mtv_geliri_co2_araliklari = sum(kredi_indirimli_satis*yeni_lifetime_mtv_co2_araliklari )/milyar,
          Mevcut_yakit_vergisi_OTV_KDV              = sum(sales*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          Yeni_OTV_yakit_vergisi_OTV_KDV            = sum(yeni_satis*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          OTV_ve_hurda_yakit_vergisi_OTV_KDV        = sum(hurda_tesvikli_satis_miktari*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          OTV_hurda_finansman_yakit_vergisi_OTV_KDV = sum(kredi_indirimli_satis*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          Kredi_faiz_indirimi_maliyeti              = -1*sum(yillik_faiz_farki*kredi_indirimi_hurda_ve_ekstra)/(milyar),
          Kredi_kullanan_arac_miktari               = sum(kredi_kullanan_arac_miktari),
          Kredi_indirimi_talep_etkisi               = sum(kredi_indirimi_talep_etkisi)  
          )


for (i in 2:5) {
  for (j in 14:27) {
    Binek_arac_vergi_gelirleri[i,j] <- Binek_arac_vergi_gelirleri[i,j]+
      Binek_arac_vergi_gelirleri[i-1,j] 
  }
}


for (i in 2:5) {
  for (j in 28) {
    Binek_arac_vergi_gelirleri[i,j] <- Binek_arac_vergi_gelirleri[i,j]+
      Binek_arac_vergi_gelirleri[i-1,j] 
  }
  if (i>=4){ # 2024 ve 2025 yilinda 3 yil oncesinin kredisi olmasin
    Binek_arac_vergi_gelirleri[i,j] <- Binek_arac_vergi_gelirleri[i,j] -
      Binek_arac_vergi_gelirleri[i-3,j]
  }
  if (i>=5){ # 2025den 2022 yi cikarirken iki defa ayni isi yapmis oluyoruz onu duzeltiyorum
    Binek_arac_vergi_gelirleri[i,j] <- Binek_arac_vergi_gelirleri[i,j] +
      Binek_arac_vergi_gelirleri[i-4,j]
  }
}

gelir_result_path <- paste(output_path,"Binek arac Muhtemel vergi gelirleri",sep="/")
gelir_result_path <- paste(gelir_result_path,"xlsx",sep = ".")
export(Binek_arac_vergi_gelirleri,gelir_result_path,sheet="binek")



# Butun verilerin toplandigi Ozet veriler

Binek_arac_toplam_vergi <- Binek_arac_vergi_gelirleri %>% mutate(
  Mecut_toplam_vergi= Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri+Mevcut_muhtemel_MTV_geliri+Mevcut_yakit_vergisi_OTV_KDV,
  
  Sadece_OTV_sadece_co2_toplam_vergi = 
    Yeni_toplam_OTV_geliri+
    Yeni_toplam_KDV_geliri+
    Yeni_muhtemel_MTV_geliri_sadece_co2+
    Yeni_OTV_yakit_vergisi_OTV_KDV,
  
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
  
  Sadece_OTV_otv_grubu_co2_toplam_vergi = 
    Yeni_toplam_OTV_geliri+
    Yeni_toplam_KDV_geliri+
    Yeni_muhtemel_MTV_geliri_otv_grubu_co2+
    Yeni_OTV_yakit_vergisi_OTV_KDV,
  
  Hurda_tesvikli_otv_grubu_co2_toplam_vergi= 
    Hurda_tesvikli_otv_geliri + 
    Hurda_tesvikli_kdv_geliri+
    Hurda_tesvikli_mtv_geliri_otv_grubu_co2+
    OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  
  Kredi_indirimli_otv_grubu_co2_toplam_vergi=
    Kredi_indirimli_otv_geliri+
    Kredi_indirimli_kdv_geliri+
    Kredi_indirimli_mtv_geliri_otv_grubu_co2+
    OTV_hurda_finansman_yakit_vergisi_OTV_KDV+
    Kredi_faiz_indirimi_maliyeti,
  
  Sadece_OTV_co2_araliklari_toplam_vergi = 
    Yeni_toplam_OTV_geliri+
    Yeni_toplam_KDV_geliri+
    Yeni_muhtemel_MTV_geliri_co2_araliklari+
    Yeni_OTV_yakit_vergisi_OTV_KDV,
  
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


ozet_binek_result_path <- paste(output_path,"Binek arac Ozet toplam vergi",sep="/")
ozet_binek_result_path <- paste(ozet_binek_result_path,"xlsx",sep = ".")
export(Binek_arac_toplam_vergi,ozet_binek_result_path,sheet="binek")



#  PC OTV summary tables ----
toplam_satis <- data %>% group_by(year) %>% summarise(sum(sales,na.rm=T))
toplam_satis_yeni <- data %>% group_by(year) %>% summarise(sum(yeni_satis,na.rm=T))


otv_grup_summary <-  data %>% group_by(year,mevcut_otv_grubu) %>% 
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


otv_grup_summary_path <- paste(output_path,"Binek arac OTV gruplarindaki degisim ozeti",sep="/")
otv_grup_summary_path <- paste(otv_grup_summary_path,"xlsx",sep = ".")
export(otv_grup_summary,otv_grup_summary_path)


# Yerli - Yabanci oranlari degisimi
toplam_satis <- data %>% group_by(year) %>% summarise(sum(sales,na.rm=T))
toplam_satis_yeni <- data %>% group_by(year) %>% summarise(sum(yeni_satis,na.rm=T))


uretim_grup_summary <-  data %>% group_by(year,uretim) %>% 
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


uretim_grup_summary_path <- paste(output_path,"Binek arac Yerli-Ithal degisim ozeti",sep="/")
uretim_grup_summary_path <- paste(uretim_grup_summary_path,"xlsx",sep = ".")
export(uretim_grup_summary,uretim_grup_summary_path)

# PC secilen modellerin yazdirilmasi ----

yazilacak_id <- c("9939","9940")
model_karsilastirma<-  data %>% filter(year==2021, id %in% yazilacak_id) %>% select(id,model,year,fiyat,yeni_fiyat,co2,yeni_toplam_otv_orani)


model_karsilastirma_path <- paste(output_path,"Binek arac Secilen model sonuclari",sep="/")
model_karsilastirma_path <- paste(model_karsilastirma_path,"xlsx",sep = ".")
export(model_karsilastirma,model_karsilastirma_path)

# MTV ve CO2 araliklari dagilimi 
mtv_co2_dagilimi <-  data %>% group_by(year,mevcut_otv_grubu,co2_grubu) %>% 
  summarise(toplam_satis=sum(kredi_indirimli_satis),agirlikli_co2_emisyonu=weighted.mean(co2,kredi_indirimli_satis))

mtv_co2_dagilimi_result_path <- paste(output_path,"MTV CO2 gruplari dagilimi",sep="/")
mtv_co2_dagilimi_result_path <- paste(mtv_co2_dagilimi_result_path,"xlsx",sep = ".")
export(mtv_co2_dagilimi,mtv_co2_dagilimi_result_path)


# PC 15 yillik mtv ----

uzun_donem_mtv <- data %>% group_by(year) %>% summarise(
  Toplam_lifetime_mtv_15_yil = sum(lifetime_mtv_15_yil * sales)/milyar,
  Toplam_yeni_lifetime_mtv_sadece_co2_15_yil = sum(yeni_lifetime_mtv_sadece_co2_15_yil * kredi_indirimli_satis)/milyar,
  Toplam_yeni_lifetime_mtv_otv_grubu_co2_15_yil = sum(yeni_lifetime_mtv_otv_grubu_co2_15_yil * kredi_indirimli_satis)/milyar,
  Toplam_yeni_lifetime_mtv_co2_araliklari_15_yil = sum(yeni_lifetime_mtv_co2_araliklari_15_yil*kredi_indirimli_satis)/milyar
)

uzun_donem_mtv_path <- paste(output_path,"Binek arac 15 yillik MTV gelirleri ",sep="/")
uzun_donem_mtv_path <- paste(uzun_donem_mtv_path,"xlsx",sep = ".")
export(uzun_donem_mtv,uzun_donem_mtv_path)


# PC figures ---- 
proposal_result <- data%>%filter(year==2020) %>% group_by(marka,model,powertrain,mevcut_otv_orani)%>% 
  summarise(new_otv_rate=weighted.mean(yeni_toplam_otv_orani,yeni_satis,na.rm=T),
            co2_emission=weighted.mean(co2,yeni_satis,na.rm=T),
            price=weighted.mean(fiyat,yeni_satis,na.rm=T), sales=sum(yeni_satis,na.rm=T),
  ) %>%
  filter(sales>5000| (powertrain=='Hybrid'&sales>1000))
  #filter(model!='renault clio' | otv_rate!=0.5)
proposal_result<-  proposal_result%>% mutate(y_position=ifelse(new_otv_rate-mevcut_otv_orani>0,new_otv_rate-0.015,new_otv_rate+0.011))

proposal_result$tax_difference <- proposal_result$new_otv_rate - proposal_result$mevcut_otv_orani
proposal_result$powertrain<-toTitleCase(proposal_result$powertrain)
proposal_result$powertrain[proposal_result$powertrain=='Petrol'] <- 'Petrol'

ggplot(proposal_result)+ 
  geom_point(aes(co2_emission,mevcut_otv_orani,  size=sales, color=powertrain),shape=21)+
  geom_point(aes(co2_emission,new_otv_rate,color=powertrain))+
  theme_classic()+
  scale_color_manual(values = palet1)+
  scale_fill_manual(values =palet1)+
  labs(x=expression(bold('Ortalama ' *CO[2]* ' emisyonu (g/km)')),y='OTV yuzdesi')+
  theme( legend.text = element_text(size = 10),
         legend.title = element_blank(),
         legend.position = "right",
         legend.direction = "vertical",
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title  = element_text(size = 10,face = 'bold'),
         axis.text = element_text(size = 9,face = 'bold'))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),  limits = c(0.2,0.9))+
  scale_x_continuous(limits = c(80,160))+
  geom_segment(aes(x=co2_emission,xend=co2_emission, y=mevcut_otv_orani,  yend=y_position, color=powertrain),
               size=0.25,arrow = arrow(length = unit(0.25, "cm"),type = 'closed'))+
  geom_label_repel(data=proposal_result,aes(co2_emission,new_otv_rate,label =toTitleCase(model),fill=powertrain),
                   color='white',fontface='bold',
                   size=2.7,
                   force = 1,
                   box.padding   = 0, 
                   point.padding = 0,show.legend = F)
ggsave(filename = paste(output_path,'new otv system by CO2.pdf',sep="/"), width = 9.08,height = 5.82)



# Agirlikli MTV ortalamalari ----

MTV_ortalamalari <- data %>% filter(year==2021) %>% 
  summarise(
    Mevcut_MTV_ortalama = weighted.mean(lifetime_mtv,sales),
    Sadece_OTV_degisikliginde_sadece_co2ya_dayali_MTV_ortalamasi = weighted.mean(yeni_mtv_sadece_co2,yeni_satis), 
    Sadece_OTV_degisikliginde_OTV_gruplarina_dayali_MTV_ortalama = weighted.mean(yeni_lifetime_mtv_otv_grubu_co2,yeni_satis),
    Sadece_OTV_degisikliginde_CO2_araliklarina_dayali_MTV_ortalama= weighted.mean(yeni_lifetime_mtv_co2_araliklari,yeni_satis),
    Hurda_tesvikli_sadece_co2_ortalama_MTV = weighted.mean(yeni_mtv_sadece_co2,hurda_tesvikli_satis_miktari),
    Hurda_tesvikli_OTV_gruplarina_dayali_MTV_ortalama = weighted.mean(yeni_lifetime_mtv_otv_grubu_co2,hurda_tesvikli_satis_miktari),
    Hurda_tesvikli_CO2_araliklarina_dayali_MTV_ortalama= weighted.mean(yeni_lifetime_mtv_co2_araliklari,hurda_tesvikli_satis_miktari),
    Kredi_indirimli_sadece_co2_ortalama_MTV = weighted.mean(yeni_mtv_sadece_co2,kredi_indirimli_satis),
    Kredi_indirimli_OTV_gruplarina_dayali_MTV_ortalama = weighted.mean(yeni_lifetime_mtv_otv_grubu_co2,kredi_indirimli_satis),
    Kredi_indirimli_CO2_araliklarina_dayali_MTV_ortalama= weighted.mean(yeni_lifetime_mtv_co2_araliklari,kredi_indirimli_satis)
  )
    
MTV_ortalamalari_path <- paste(output_path,"Binek arac MTV ortalamalari",sep="/")
MTV_ortalamalari_path <- paste(MTV_ortalamalari_path,"xlsx",sep = ".")
export(MTV_ortalamalari,MTV_ortalamalari_path)

    
    
    
                                          
                                          
                                          

                                          
                                          