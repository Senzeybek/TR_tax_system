# Hurda tesviki

tesvik_grubu_ortalama_net_fiyat<- data %>% filter(mevcut_otv_grubu%in% c(1,2)) %>%
  summarise(ortalama_net_fiyat=weighted.mean(net_fiyat,yeni_satis_segment_adjusted))

tesvik_saglanacak_miktar <- tesvik_grubu_ortalama_net_fiyat*hurda_tesviki_indirim_orani #ekstra kdv indirimi dahil edilmedi

yararlanacak_max_arac_sayisi <- hurda_tesviki_paketi/tesvik_saglanacak_miktar
