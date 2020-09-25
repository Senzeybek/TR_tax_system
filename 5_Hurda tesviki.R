# Hurda tesviki

# tesvik verilecek gruolar ve tesvik indirimi
data$hurda_tesvikli_otv_orani <- ifelse(data$mevcut_otv_grubu%in% c(1,2),
                                        data$yeni_toplam_otv_orani-hurda_tesviki_indirim_orani,data$yeni_toplam_otv_orani) 


# yeni arac fiyati hesaplanmasi
data$hurda_tesvikli_OTV_tutari <- data$net_fiyat*data$hurda_tesvikli_otv_orani

data$hurda_tesvikli_KDV_tutari <- (data$net_fiyat+data$hurda_tesvikli_OTV_tutari)*data$kdv_orani

data$hurda_tesvikli_fiyat <-      data$net_fiyat+data$hurda_tesvikli_OTV_tutari + data$hurda_tesvikli_KDV_tutari 

data$hurda_tesvikli_fiyat_degisimi <-      data$hurda_tesvikli_fiyat - data$yeni_fiyat


# tesvik sayesinde gelecek indirimin talep etkisi
data$hurda_tesvikli_fiyat_degisim_orani <- data$hurda_tesvikli_fiyat_degisimi/ data$yeni_fiyat

data$hurda_tesviki_talep_degisim_orani <- data$hurda_tesvikli_fiyat_degisim_orani*kendi_esnekligi

data$hurda_tesvikli_satis_miktari <- round(data$yeni_satis*(1+data$hurda_tesviki_talep_degisim_orani))

data$hurda_tesviki_ekstra_satis <- data$hurda_tesvikli_satis_miktari - data$yeni_satis

# Hurda tesviki sonrasinda vergi gelirleri
# Gelecek ekstra 86000 yeni arac talebi sayesinde OTVdeki kayip 2.5 milyar seviyesinde kaliyor.

Hurda_tesvikli_otv_geliri <- sum(data$hurda_tesvikli_satis_miktari*data$hurda_tesvikli_OTV_tutari )/milyar 

Hurda_tesvikli_kdv_geliri <- sum(data$hurda_tesvikli_satis_miktari*data$hurda_tesvikli_KDV_tutari )/milyar 

Hurda_tesvikli_mtv_geliri_co2_araliklari <- sum(data$hurda_tesvikli_satis_miktari*data$yeni_lifetime_mtv_co2_araliklari )/milyar 

Hurda_tesvikli_mtv_geliri_otv_grubu_co2<- sum(data$hurda_tesvikli_satis_miktari*data$yeni_lifetime_mtv_otv_grubu_co2 )/milyar 

Hurda_tesvikli_mtv_geliri_sadece_co2<- sum(data$hurda_tesvikli_satis_miktari*data$yeni_lifetime_mtv_sadece_co2 )/milyar 





