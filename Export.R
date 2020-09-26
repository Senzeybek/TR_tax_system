output_path <- paste("Senaryo sonuclari", Sys.time(), sep = "_")
output_path <- str_replace_all(output_path,":","-")
output_path <- paste("r_output/", output_path, sep = "")
dir.create(output_path)



data_result_path <- paste(output_path,"data",sep="/")
data_result_path <- paste(data_result_path,"xlsx",sep = ".")
export(data,data_result_path)


# Gelir hesaplamasi ----

# Binek ----


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
          Yeni_muhtemel_MTV_geliri_sadece_co2       = sum(sales*yeni_lifetime_mtv_sadece_co2)/milyar,
          Yeni_muhtemel_MTV_geliri_otv_grubu_co2    = sum(sales*yeni_lifetime_mtv_otv_grubu_co2)/milyar,
          Yeni_muhtemel_MTV_geliri_co2_araliklari   = sum(sales*yeni_lifetime_mtv_co2_araliklari)/milyar,
          Hurda_tesvikli_mtv_geliri_sadece_co2      = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
          Hurda_tesvikli_mtv_geliri_otv_grubu_co2   = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_otv_grubu_co2 )/milyar,
          Hurda_tesvikli_mtv_geliri_co2_araliklari  = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_co2_araliklari )/milyar,
          Kredi_indirimli_mtv_geliri_sadece_co2     = sum(kredi_indirimli_satis*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
          Kredi_indirimli_mtv_geliri_otv_grubu_co2  = sum(kredi_indirimli_satis*yeni_lifetime_mtv_otv_grubu_co2 )/milyar,
          Kredi_indirimli_mtv_geliri_co2_araliklari = sum(kredi_indirimli_satis*yeni_lifetime_mtv_co2_araliklari )/milyar,
          Mevcut_yakit_vergisi_OTV_KDV              = sum(sales*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          Yeni_OTV_yakit_vergisi_OTV_KDV            = sum(yeni_satis*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          OTV_ve_hurda_yakit_vergisi_OTV_KDV        = sum(hurda_tesvikli_satis_miktari*toplam_yakit_tuketimi*yakit_vergisi)/milyar,
          OTV_hurda_finansman_yakit_vergisi_OTV_KDV = sum(kredi_indirimli_satis*toplam_yakit_tuketimi*yakit_vergisi)/milyar
          )


Binek_arac_toplam_vergi <- Binek_arac_vergi_gelirleri %>% mutate(
  Mecut_toplam_vergi= Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri+Mevcut_muhtemel_MTV_geliri+Mevcut_yakit_vergisi_OTV_KDV,
  Sadece_OTV_sadece_co2_toplam_vergi = Yeni_toplam_OTV_geliri+Yeni_toplam_KDV_geliri+Yeni_muhtemel_MTV_geliri_sadece_co2+Yeni_OTV_yakit_vergisi_OTV_KDV,
  Sadece_OTV_otv_grubu_co2_toplam_vergi = Yeni_toplam_OTV_geliri+Yeni_toplam_KDV_geliri+Yeni_muhtemel_MTV_geliri_otv_grubu_co2+Yeni_OTV_yakit_vergisi_OTV_KDV,
  Sadece_OTV_co2_araliklari_toplam_vergi = Yeni_toplam_OTV_geliri+Yeni_toplam_KDV_geliri+Yeni_muhtemel_MTV_geliri_co2_araliklari+Yeni_OTV_yakit_vergisi_OTV_KDV,
  Hurda_tesvikli_sadece_co2_toplam_vergi= Hurda_tesvikli_otv_geliri + Hurda_tesvikli_kdv_geliri+Hurda_tesvikli_mtv_geliri_sadece_co2+OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  Hurda_tesvikli_otv_grubu_co2_toplam_vergi= Hurda_tesvikli_otv_geliri + Hurda_tesvikli_kdv_geliri+Hurda_tesvikli_mtv_geliri_otv_grubu_co2+OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  Hurda_tesvikli_co2_araliklari_toplam_vergi= Hurda_tesvikli_otv_geliri + Hurda_tesvikli_kdv_geliri+Hurda_tesvikli_mtv_geliri_co2_araliklari+OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  Kredi_indirimli_saece_co2_toplam_vergi=Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+Kredi_indirimli_mtv_geliri_sadece_co2+OTV_hurda_finansman_yakit_vergisi_OTV_KDV,
  Kredi_indirimli_otv_grubu_co2_toplam_vergi=Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+Kredi_indirimli_mtv_geliri_otv_grubu_co2+OTV_hurda_finansman_yakit_vergisi_OTV_KDV,
  Kredi_indirimli_co2_araliklari_toplam_vergi=Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+Kredi_indirimli_mtv_geliri_co2_araliklari+OTV_hurda_finansman_yakit_vergisi_OTV_KDV
  )


for (i in 2:6) {
  for (j in 14:27) {
    Binek_arac_vergi_gelirleri[i,j] <- Binek_arac_vergi_gelirleri[i,j]+
      Binek_arac_vergi_gelirleri[i-1,j] 
  }
}


gelir_result_path <- paste(output_path,"Muhtemel_binek_arac_vergi_gelirleri",sep="/")
gelir_result_path <- paste(gelir_result_path,"xlsx",sep = ".")
export(Binek_arac_vergi_gelirleri,gelir_result_path,sheet="binek")



# ozet 
Binek_arac_toplam_vergi <- Binek_arac_vergi_gelirleri %>% mutate(
  Mecut_toplam_vergi= Mevcut_muhtemel_OTV_geliri+Mevcut_muhtemel_KDV_geliri+Mevcut_muhtemel_MTV_geliri+Mevcut_yakit_vergisi_OTV_KDV,
  
  Sadece_OTV_sadece_co2_toplam_vergi = Yeni_toplam_OTV_geliri+Yeni_toplam_KDV_geliri+Yeni_muhtemel_MTV_geliri_sadece_co2+Yeni_OTV_yakit_vergisi_OTV_KDV,
  Hurda_tesvikli_sadece_co2_toplam_vergi= Hurda_tesvikli_otv_geliri + Hurda_tesvikli_kdv_geliri+Hurda_tesvikli_mtv_geliri_sadece_co2+OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  Kredi_indirimli_saece_co2_toplam_vergi=Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+Kredi_indirimli_mtv_geliri_sadece_co2+OTV_hurda_finansman_yakit_vergisi_OTV_KDV,
  
  Sadece_OTV_otv_grubu_co2_toplam_vergi = Yeni_toplam_OTV_geliri+Yeni_toplam_KDV_geliri+Yeni_muhtemel_MTV_geliri_otv_grubu_co2+Yeni_OTV_yakit_vergisi_OTV_KDV,
  Hurda_tesvikli_otv_grubu_co2_toplam_vergi= Hurda_tesvikli_otv_geliri + Hurda_tesvikli_kdv_geliri+Hurda_tesvikli_mtv_geliri_otv_grubu_co2+OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  Kredi_indirimli_otv_grubu_co2_toplam_vergi=Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+Kredi_indirimli_mtv_geliri_otv_grubu_co2+OTV_hurda_finansman_yakit_vergisi_OTV_KDV,
  
  Sadece_OTV_co2_araliklari_toplam_vergi = Yeni_toplam_OTV_geliri+Yeni_toplam_KDV_geliri+Yeni_muhtemel_MTV_geliri_co2_araliklari+Yeni_OTV_yakit_vergisi_OTV_KDV,
  Hurda_tesvikli_co2_araliklari_toplam_vergi= Hurda_tesvikli_otv_geliri + Hurda_tesvikli_kdv_geliri+Hurda_tesvikli_mtv_geliri_co2_araliklari+OTV_ve_hurda_yakit_vergisi_OTV_KDV,
  Kredi_indirimli_co2_araliklari_toplam_vergi=Kredi_indirimli_otv_geliri+Kredi_indirimli_kdv_geliri+Kredi_indirimli_mtv_geliri_co2_araliklari+OTV_hurda_finansman_yakit_vergisi_OTV_KDV

  ) %>% select(year,Mecut_toplam_vergi:Kredi_indirimli_co2_araliklari_toplam_vergi)


ozet_binek_result_path <- paste(output_path,"Ozet Binek_arac_toplam_vergi",sep="/")
ozet_binek_result_path <- paste(ozet_binek_result_path,"xlsx",sep = ".")
export(Binek_arac_toplam_vergi,ozet_binek_result_path,sheet="binek")


# LCV ----

LCV_arac_vergi_gelirleri <- lcv_data %>% group_by(year) %>% summarise(
  Mecut_toplam_talep = sum(sales),
  Sadece_OTV_toplam_talep = sum(yeni_satis),
  OTV_ve_Hurda_toplam_talep = sum(hurda_tesvikli_satis_miktari,na.rm=T),
  Mevcut_muhtemel_OTV_geliri=sum(mevcut_otv_tutari * (sales),na.rm=T)/milyar,
  Mevcut_muhtemel_KDV_geliri=sum(mevcut_kdv_tutari * (sales),na.rm=T)/milyar,
  Yeni_toplam_OTV_geliri= sum(yeni_toplam_otv_tutari * (yeni_satis),na.rm=T)/milyar,
  Yeni_toplam_KDV_geliri= sum(yeni_kdv_tutari * (yeni_satis),na.rm=T)/milyar,
  #Mevcut_muhtemel_MTV_geliri = sum(sales*lifetime_mtv)/milyar,
  #Yeni_muhtemel_MTV_geliri_sadece_co2=sum(sales*yeni_lifetime_mtv_sadece_co2)/milyar,
  #Yeni_muhtemel_MTV_geliri_otv_grubu_co2=sum(sales*yeni_lifetime_mtv_otv_grubu_co2)/milyar,
  #Yeni_muhtemel_MTV_geliri_co2_araliklari=sum(sales*yeni_lifetime_mtv_co2_araliklari)/milyar,
  Hurda_tesvikli_otv_geliri = sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_OTV_tutari,na.rm=T)/milyar, 
  Hurda_tesvikli_kdv_geliri =sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_KDV_tutari,na.rm=T )/milyar, 
  #Hurda_tesvikli_mtv_geliri_sadece_co2 = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
  #Hurda_tesvikli_mtv_geliri_otv_grubu_co2 = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_otv_grubu_co2 )/milyar,
  #Hurda_tesvikli_mtv_geliri_co2_araliklari= sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_co2_araliklari )/milyar
  )


lcv_gelir_result_path <- paste(output_path,"Muhtemel_LCV_vergi_gelirleri",sep="/")
lcv_gelir_result_path <- paste(lcv_gelir_result_path,"xlsx",sep = ".")
export(LCV_arac_vergi_gelirleri,lcv_gelir_result_path)

# OTV summary tables ----
toplam_satis <- data %>% group_by(year) %>% summarise(sum(sales,na.rm=T))
toplam_satis_yeni <- data %>% group_by(year) %>% summarise(sum(yeni_satis,na.rm=T))


otv_grup_summary <-  data %>% group_by(year,mevcut_otv_grubu) %>% 
  summarise(mevcut_satis        = sum(sales,na.rm = T),
            yeni_otv_ortalamasi = weighted.mean(yeni_toplam_otv_orani, yeni_satis,na.rm=T),
            yeni_satis          = sum(yeni_satis),
            hurda_tesvikli_satis= sum(hurda_tesvikli_satis_miktari,na.rm=T),
            hurda_tesvikli_otv_ortalamasi = weighted.mean(hurda_tesvikli_otv_orani, hurda_tesvikli_satis_miktari,na.rm=T),
            ) %>%
  mutate(eski_market_payi       = mevcut_satis/sum(mevcut_satis),
         yeni_market_payi       = yeni_satis/sum(yeni_satis),
         eski_toplam_talep      = sum(mevcut_satis),
         yeni_toplam_talep      = sum(yeni_satis),
          hurda_tesviki_talep   = sum(hurda_tesvikli_satis))


otv_grup_summary_path <- paste(output_path,"otv_grup_summary",sep="/")
otv_grup_summary_path <- paste(otv_grup_summary_path,"xlsx",sep = ".")
export(otv_grup_summary,otv_grup_summary_path)




# secilen modellerin yazdirilmasi

yazilacak_id <- c("9939","9940")
model_karsilastirma<-  data %>% filter(year==2020, id %in% yazilacak_id) %>% select(id,model,year,fiyat,yeni_fiyat,co2,yeni_otv_orani)


model_karsilastirma_path <- paste(output_path,"model_karsilastirma",sep="/")
model_karsilastirma_path <- paste(model_karsilastirma_path,"xlsx",sep = ".")
export(model_karsilastirma,model_karsilastirma_path)









# figures ---- 
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








