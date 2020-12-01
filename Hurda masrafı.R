
binek_hurda_masrafi <- data %>% filter(mevcut_otv_grubu%in%c(1,2))  %>% group_by(year)%>% summarise(hurdasiz_otv = sum(hurda_tesvikli_satis_miktari * (mevcut_otv_tutari+mevcut_kdv_tutari))/milyar,
                                                                      hurdali_otv=sum(hurda_tesvikli_satis_miktari * (hurda_tesvikli_OTV_tutari+hurda_tesvikli_KDV_tutari))/milyar,
                                                                     hurda_masrafi= hurdali_otv-hurdasiz_otv) %>% mutate(arac_tipi="Binek")

                                              
lcv_hurda_masrafi <-lcv_data   %>% group_by(year)%>% summarise(hurdasiz_otv = sum(hurda_tesvikli_satis_miktari * (mevcut_otv_tutari+mevcut_kdv_tutari))/milyar,
                                                                             hurdali_otv=sum(hurda_tesvikli_satis_miktari * (hurda_tesvikli_OTV_tutari+hurda_tesvikli_KDV_tutari))/milyar,
                                                                             hurda_masrafi= hurdali_otv-hurdasiz_otv) %>% mutate(arac_tipi="LCV")

hdv_hurda_masrafi <-hdv_data   %>% group_by(year)%>% summarise(hurdasiz_otv = sum(hurda_tesvikli_satis_miktari * (mevcut_otv_tutari+mevcut_kdv_tutari))/milyar,
                                           hurdali_otv=sum(hurda_tesvikli_satis_miktari * (hurda_tesvikli_OTV_tutari+hurda_tesvikli_KDV_tutari))/milyar,
                                           hurda_masrafi= hurdali_otv-hurdasiz_otv)%>% mutate(arac_tipi="HDV")

hurda_masrafi <- rbind(binek_hurda_masrafi,lcv_hurda_masrafi)
hurda_masrafi <- rbind(hurda_masrafi,hdv_hurda_masrafi)

hurda_masrafi_result_path <- paste(output_path,"Hurda masrafi hesaplama",sep="/")
hurda_masrafi_result_path <- paste(hurda_masrafi_result_path,"xlsx",sep = ".")
export(hurda_masrafi,hurda_masrafi_result_path)