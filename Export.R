output_path <- paste("Senaryo sonuclari", Sys.time(), sep = "_")
output_path <- str_replace_all(output_path,":","-")
output_path <- paste("r_output/", output_path, sep = "")
dir.create(output_path)



data_result_path <- paste(output_path,"data",sep="/")
data_result_path <- paste(data_result_path,"xlsx",sep = ".")
export(data,data_result_path)


# Gelir hesaplamasi ----

# Binek ----


Binek_arac_vergi_gelirleri <- data %>% group_by(year) %>% summarise(
                                                                  Mecut_toplam_talep = sum(sales),
                                                                  Sadece_OTV_toplam_talep = sum(yeni_satis),
                                                                  OTV_ve_Hurda_toplam_talep = sum(hurda_tesvikli_satis_miktari),
                                                                  Mevcut_muhtemel_OTV_geliri=sum(mevcut_otv_tutari * (sales))/milyar,
                                                                  Mevcut_muhtemel_KDV_geliri=sum(mevcut_kdv_tutari * (sales))/milyar,
                                                                  Yeni_toplam_OTV_geliri= sum(yeni_toplam_otv_tutari * (yeni_satis))/milyar,
                                                                  Yeni_toplam_KDV_geliri= sum(yeni_kdv_tutari * (yeni_satis))/milyar,
                                                                  Mevcut_muhtemel_MTV_geliri = sum(sales*lifetime_mtv)/milyar,
                                                                  Yeni_muhtemel_MTV_geliri_sadece_co2=sum(sales*yeni_lifetime_mtv_sadece_co2)/milyar,
                                                                  Yeni_muhtemel_MTV_geliri_otv_grubu_co2=sum(sales*yeni_lifetime_mtv_otv_grubu_co2)/milyar,
                                                                  Yeni_muhtemel_MTV_geliri_co2_araliklari=sum(sales*yeni_lifetime_mtv_co2_araliklari)/milyar,
                                                                  Hurda_tesvikli_otv_geliri = sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_OTV_tutari)/milyar, 
                                                                  Hurda_tesvikli_kdv_geliri =sum(hurda_tesvikli_satis_miktari*hurda_tesvikli_KDV_tutari )/milyar, 
                                                                  Hurda_tesvikli_mtv_geliri_sadece_co2 = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_sadece_co2 )/milyar,                                                                  
                                                                  Hurda_tesvikli_mtv_geliri_otv_grubu_co2 = sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_otv_grubu_co2 )/milyar,
                                                                  Hurda_tesvikli_mtv_geliri_co2_araliklari= sum(hurda_tesvikli_satis_miktari*yeni_lifetime_mtv_co2_araliklari )/milyar)
  

gelir_result_path <- paste(output_path,"Muhtemel_binek_arac_vergi_gelirleri",sep="/")
gelir_result_path <- paste(gelir_result_path,"xlsx",sep = ".")
export(Binek_arac_vergi_gelirleri,gelir_result_path)


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
            ) %>%
  mutate(eski_market_payi       = mevcut_satis/sum(mevcut_satis),
         yeni_market_payi       = yeni_satis/sum(yeni_satis),
         eski_toplam_talep      = sum(mevcut_satis),
         yeni_toplam_talep      = sum(yeni_satis))


otv_grup_summary_path <- paste(output_path,"otv_grup_summary",sep="/")
otv_grup_summary_path <- paste(otv_grup_summary_path,"xlsx",sep = ".")
export(otv_grup_summary,otv_grup_summary_path)


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








