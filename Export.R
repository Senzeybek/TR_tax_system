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