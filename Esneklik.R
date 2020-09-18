source("1_preparation.R")
source("OTV oranlarinin hesaplanmasi.R")


data <-  data%>% separate(segment,     into = c("segment_text", "segment_num"), 
                          sep = "(?<=[A-Za-z])(?=[0-9])")

data$segment_text <- tolower(data$segment_text)



#matching segments with kleit segments
data$kleit_segment <- segment_look_up$kleit_segment[match(data$segment_text,segment_look_up$segment_text)]

#suv segments are numbered as 7 in odd data
#convertion ihs segments to kleit segment
data[data$segment_num==7 & data$agirlik>2000,]$kleit_segment <- 'large_suv'
data[data$segment_num==7 & data$agirlik<2000,]$kleit_segment <- 'small_suv'


# rakip esnekliklerinin hesaplanmasi ----

for(i in 1:nrow(data)){
  segment_change <<- data[-i,]%>% filter(kleit_segment==data[i,]$kleit_segment)%>% 
    summarise(ortalama_rakip_degisimi=weighted.mean(yuzde_fiyat_degisimi,satis_2020,na.rm = T))  
  data[i,60]<- segment_change
  i=i+1
}


# segment cross esnekliklerinin hesaplanmasi ----

data$yuzde_satis_degisimi<- (data$yuzde_fiyat_degisimi*kendi_esnekligi) + (data$ortalama_rakip_degisimi*rakip_esnekligi)
data$yeni_satis<- round(data$satis_2020*(1+data$yuzde_fiyat_degisimi))

segment_fiyat_degisimi <-  data%>% group_by(kleit_segment)%>% 
                                  summarise(mevcut_toplam=sum(satis_2020),yuzde_fiyat_degisimi=weighted.mean(yuzde_fiyat_degisimi,satis_2020))


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

mevcut_yeni_satislar<- data %>% group_by(kleit_segment)%>% summarise(mevcut_satislar=sum(satis_2020,na.rm=T)) %>% 
  mutate(mevcut_market_payi=mevcut_satislar/sum(mevcut_satislar,na.rm=T))

segment_capraz_esneklik$mevcut_satislar<- mevcut_yeni_satislar$mevcut_satislar[match(segment_capraz_esneklik$own_segment, mevcut_yeni_satislar$kleit_segment)]
segment_capraz_esneklik$mevcut_market_payi<- mevcut_yeni_satislar$mevcut_market_payi[match(segment_capraz_esneklik$own_segment, mevcut_yeni_satislar$kleit_segment)]
segment_capraz_esneklik$segment_ayarlamasi <- segment_capraz_esneklik$yeni_market_payi_by_kleit/segment_capraz_esneklik$mevcut_market_payi

# Daha sonra araclarin satislarini segment ayarlamalari ile carparak kleit modelini de ekledim
data$segment_ayarlamalari <- segment_capraz_esneklik$segment_ayarlamasi[match(data$kleit_segment,segment_capraz_esneklik$own_segment)]
data$yeni_satis_segment_adjusted <- round(data$yeni_satis*data$segment_ayarlamalari)


data%>% group_by(fuel_type) %>% summarise(weighted.mean(yuzde_fiyat_degisimi,satis_2020,na.rm=T),weighted.mean(yuzde_fiyat_degisimi,yeni_satis_segment_adjusted,na.rm=T))


#hurda paketi 
#faiz indirimi