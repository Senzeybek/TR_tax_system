


data <-  data%>% separate(segment,     into = c("segment_text", "segment_num"), 
                          sep = "(?<=[A-Za-z])(?=[0-9])")

data$segment_text <- tolower(data$segment_text)



# segments with kleit segments
data$kleit_segment <- segment_look_up$kleit_segment[match(data$segment_text,segment_look_up$segment_text)]

# suv segments are numbered as 7 in odd data
data[data$segment_num==7 & data$agirlik>2000,]$kleit_segment <- 'large_suv'
data[data$segment_num==7 & data$agirlik<2000,]$kleit_segment <- 'small_suv'


# rakip esnekliklerinin hesaplanmasi ----
j =ncol(data)+1
for(i in 1:nrow(data)){
  segment_change <<- data[-i,]%>% filter(kleit_segment==data[i,]$kleit_segment)%>% 
    summarise(ortalama_rakip_degisimi=weighted.mean(yuzde_fiyat_degisimi,sales,na.rm = T))  
  data[i,j] <- segment_change
  i=i+1
}


data$yuzde_satis_degisimi<- (data$yuzde_fiyat_degisimi*kendi_esnekligi) + (data$ortalama_rakip_degisimi*rakip_esnekligi)
data$yeni_satis<- round(data$sales*(1+data$yuzde_satis_degisimi))

segment_fiyat_degisimi <-  data%>% group_by(kleit_segment)%>% 
  summarise(mevcut_toplam=sum(sales),yuzde_fiyat_degisimi=weighted.mean(yuzde_fiyat_degisimi,sales))

# segment cross esnekliklerinin hesaplanmasi ----
# kleit segment esnekliklerinin dahil edilmesi
segment_capraz_esneklik<- gather(segment_capraz_esneklik,key='cross_segment', 'elasticity',small_car:van) %>%arrange(own_segment)

# segmentlerin ortalama fiyat degisikliklerinin bulunmasi
# Daha sonrasinda esneklikler ve ortalama fiyat degisiklikleri bulunarak 
# yeni segment satis rakamlarina ulasilmasi


# cross segmentin ortalama fiyat degisikligi
segment_capraz_esneklik$capraz_fiyat_degisimi<- segment_fiyat_degisimi$yuzde_fiyat_degisimi[match(segment_capraz_esneklik$cross_segment,segment_fiyat_degisimi$kleit_segment)]

# cross segment fiyat degisimi x esneklik
segment_capraz_esneklik$etki<- segment_capraz_esneklik$elasticity*segment_capraz_esneklik$capraz_fiyat_degisimi

# butun cross segment fiyat degisikliklerinin toplanmasi
segment_capraz_esneklik <-  segment_capraz_esneklik%>% na.omit()%>% group_by(own_segment)%>% summarise(toplam_etki= sum(etki))

# segment mevcut satis rakamlari
segment_capraz_esneklik$mevcut_toplam<- segment_fiyat_degisimi$mevcut_toplam[match(segment_capraz_esneklik$own_segment,segment_fiyat_degisimi$kleit_segment)] 

# segment yeni satis rakamlarinin esneklikle bulunmasi
segment_capraz_esneklik$yeni_toplam_by_kleit <- round((1+segment_capraz_esneklik$toplam_etki)*segment_capraz_esneklik$mevcut_toplam)

# segment yeni market paylarinin bulunmasi
segment_capraz_esneklik$yeni_market_payi_by_kleit<- segment_capraz_esneklik$yeni_toplam_by_kleit/sum(segment_capraz_esneklik$yeni_toplam_by_kleit,na.rm=T)


# Daha sonra mevcut segment sharelari ile kleit sharelari karsilastirildi
# O nedenle Kleit segment paylarini mevcut paylara bolup oran bulundu

mevcut_yeni_satislar<- data %>% group_by(kleit_segment)%>% summarise(mevcut_satislar=sum(sales,na.rm=T)) %>% 
  mutate(mevcut_market_payi=mevcut_satislar/sum(mevcut_satislar,na.rm=T))

segment_capraz_esneklik$mevcut_satislar<- mevcut_yeni_satislar$mevcut_satislar[match(segment_capraz_esneklik$own_segment, mevcut_yeni_satislar$kleit_segment)]
segment_capraz_esneklik$mevcut_market_payi<- mevcut_yeni_satislar$mevcut_market_payi[match(segment_capraz_esneklik$own_segment, mevcut_yeni_satislar$kleit_segment)]
segment_capraz_esneklik$segment_ayarlamasi <- segment_capraz_esneklik$yeni_market_payi_by_kleit/segment_capraz_esneklik$mevcut_market_payi

# Daha sonra araclarin satislarini segment ayarlamalari ile carparak kleit modelini de ekledim
data$segment_ayarlamalari <- segment_capraz_esneklik$segment_ayarlamasi[match(data$kleit_segment,segment_capraz_esneklik$own_segment)]
data$yeni_satis <- round(data$yeni_satis*data$segment_ayarlamalari)


# Toplam yeni satis arasindaki fark ----
data$adet_degisimi <- data$yeni_satis-data$sales
Yeni_toplam_OTV_geliri <-data %>% group_by(year) %>% summarise(sum(yeni_toplam_otv_tutari*yeni_satis)/milyar)

#ekstra yakit tuketimi ----
ekstra_yakit_tuketimi <- data %>% group_by(powertrain) %>% summarise(degisim=sum(adet_degisimi), 
                                                                     ortalama_yakit_tuketimi= weighted.mean(yakit_tuketimi,adet_degisimi,na.rm=T)) 


ekstra_yakit_tuketimi$gercek_tuketim <- 1.21*ekstra_yakit_tuketimi$ortalama_yakit_tuketimi

ekstra_yakit_tuketimi$toplam_yakit_tuketimi <- (arac_yillik_km/100)*ekstra_yakit_tuketimi$gercek_tuketim
ekstra_yakit_tuketimi$yakit_otv<- ifelse(ekstra_yakit_tuketimi$powertrain=="Dizel",dizel_litre_otv,benzin_litre_otv)
Toplam_ekstra_yakit_geliri= sum(ekstra_yakit_tuketimi$toplam_yakit_tuketimi*ekstra_yakit_tuketimi$yakit_otv*ekstra_yakit_tuketimi$degisim)/milyar  


