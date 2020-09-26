
#esneklik

data$yuzde_satis_degisimi<- (data$yuzde_fiyat_degisimi*kendi_esnekligi)

# yeni satis
data$yeni_satis<- round(data$sales*(1+data$yuzde_satis_degisimi))


