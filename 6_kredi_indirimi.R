# toplam kredi hacmi

toplam_sektor_hacmi <- sum(data$yeni_fiyat*data$yeni_satis_segment_adjusted)/milyar

kredi_hacmi <- toplam_sektor_hacmi*kredi_orani

arac_basina_kredi <- kredi_hacmi*milyar/sum(data$yeni_satis_segment_adjusted)

arac_basina_kredi_odemesi <- arac_basina_kredi* ((1+mevcut_yillik_faiz)^ortalama_vade) 

yillik_faiz_odemesi <- (arac_basina_kredi_odemesi - arac_basina_kredi)/ortalama_vade

indirimli_kredi_odemesi <- arac_basina_kredi * ((1+indirimli_yillik_faiz)^ortalama_vade) 

indirimli_yillik_faiz_odemesi <- (indirimli_kredi_odemesi - arac_basina_kredi)/ortalama_vade

ortalama_yillik_faiz_indirimi <- yillik_faiz_odemesi - indirimli_yillik_faiz_odemesi

faiz_indirim_orani <- ortalama_yillik_faiz_indirimi/weighted.mean(data$net_fiyat,data$yeni_satis_segment_adjusted)
