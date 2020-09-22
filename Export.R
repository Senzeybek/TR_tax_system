output_path <- paste("Senaryo sonuclari", Sys.time(), sep = "_")
output_path <- str_replace_all(output_path,":","-")
output_path <- paste("r_output/", output_path, sep = "")
dir.create(output_path)



data_result_path <- paste(output_path,"data",sep="/")
data_result_path <- paste(data_result_path,"xlsx",sep = ".")
export(data,data_result_path)


# Gelir hesaplamasi
yeni_gelirler<- data.frame(Yeni_muhtemel_MTV_geliri_co2_araliklari,Yeni_toplam_OTV_geliri)
eski_gelirler<- data.frame(Mevcut_muhtemel_MTV_geliri,Mevcut_muhtemel_OTV_geliri)
gelirler     <- cbind(eski_gelirler,yeni_gelirler)

gelir_result_path <- paste(output_path,"gelirler",sep="/")
gelir_result_path <- paste(gelir_result_path,"xlsx",sep = ".")
export(gelirler,gelir_result_path)


# Yeni OTV oranlari
OTV_kiyaslamasi <- data %>% group_by(mevcut_otv_grubu) %>%
  summarise("Yeni OTV orani (OTV+CO2)"= weighted.mean(yeni_toplam_otv_orani,satis_2020,na.rm = T),
            "Mevcut OTV orani"= weighted.mean(mevcut_otv_orani,satis_2020,na.rm = T),)


OTV_result_path <- paste(output_path,"OTV_kiyaslamasi",sep="/")
OTV_result_path <- paste(OTV_result_path,"xlsx",sep = ".")
export(OTV_kiyaslamasi,OTV_result_path)



# OTV summary tables
toplam_satis <- sum(data$satis_2020,na.rm=T)
toplam_satis_yeni <- sum(data$yeni_satis_segment_adjusted,na.rm=T)


otv_grup_summary <-  data %>% filter(powertrain!="Hybrid") %>%
  group_by(mevcut_otv_grubu) %>%
  summarise(mevcut_satis        = sum(satis_2020,na.rm = T),
            yeni_otv_ortalamasi = weighted.mean(yeni_toplam_otv_orani, yeni_satis_segment_adjusted,na.rm=T),
            yeni_satis          = sum(yeni_satis_segment_adjusted)) %>%
  mutate(eski_market_payi       = mevcut_satis/toplam_satis,
         yeni_market_payi       = yeni_satis/toplam_satis_yeni,
         eski_toplam_talep      = toplam_satis,
         yeni_toplam_talep      = toplam_satis_yeni)


otv_grup_summary_path <- paste(output_path,"otv_grup_summary",sep="/")
otv_grup_summary_path <- paste(otv_grup_summary_path,"xlsx",sep = ".")
export(otv_grup_summary,otv_grup_summary_path)


# figure 
proposal_result <- data%>%group_by(marka,model,powertrain,mevcut_otv_orani)%>% 
  summarise(new_otv_rate=weighted.mean(yeni_toplam_otv_orani,yeni_satis_segment_adjusted,na.rm=T),
            co2_emission=weighted.mean(co2,yeni_satis,na.rm=T),
            price=weighted.mean(fiyat,yeni_satis_segment_adjusted,na.rm=T), sales=sum(yeni_satis_segment_adjusted,na.rm=T),
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
  labs(x=expression(bold('Ortalama ' *CO[2]* ' emisyonu (g/km)')),y='ÖTV yüzdesi')+
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




# graph by fiyat and otv
ggplot(proposal_result)+ 
  geom_point(aes(price,mevcut_otv_orani,  size=sales, color=powertrain),shape=21)+
  geom_point(aes(price,new_otv_rate,color=powertrain))+
  theme_classic()+
  scale_color_manual(values = palet1)+
  scale_fill_manual(values =palet1)+
  labs(x=expression(bold('Aracin fiyati')),y='ÖTV yüzdesi')+
  theme( legend.text = element_text(size = 10),
         legend.title = element_blank(),
         legend.position = "right",
         legend.direction = "vertical",
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title  = element_text(size = 10,face = 'bold'),
         axis.text = element_text(size = 9,face = 'bold'))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),  limits = c(0.2,0.9))+
  scale_x_continuous(limits = c(100000,400000),labels = scales::comma)+
  geom_segment(aes(x=price,xend=price, y=mevcut_otv_orani,  yend=y_position, color=powertrain),
               size=0.25,arrow = arrow(length = unit(0.25, "cm"),type = 'closed'))+
  geom_label_repel(data=proposal_result,aes(price,new_otv_rate,label =toTitleCase(model),fill=powertrain),
                   color='white',fontface='bold',
                   size=2.7,
                   force = 1,
                   box.padding   = 0, 
                   point.padding = 0,show.legend = F)
ggsave(filename = paste(output_path,'new otv system by price.pdf',sep="/"), width = 9.08,height = 5.82)






