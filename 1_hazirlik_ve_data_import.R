# Packages ------------------------------------------------------------------------------------
# butun paketlerin yuklu oldunudan emin olmak icin check
list_of_packages <- c("rio", "stringr","ggthemes","tidyverse","tidyr",'forecast','scales','tools',"ggrepel") 
new.packages <- list_of_packages[!(list_of_packages%in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# importing the packages
lapply(list_of_packages,library,character.only=T)




# data import ----
odd_2016 <- import("r_input/odd_kpmg_2016.xls",sheet="Sheet1",skip=2)  %>% mutate(year=2016)
odd_2017 <- import("r_input/odd_kpmg_2017.xls",sheet="Sheet1",skip=2) %>% mutate(year=2017) 
odd_2018 <- import("r_input/odd_kpmg_2018.xls",sheet="Sheet1",skip=2) %>% mutate(year=2018)
odd_2019 <- import("r_input/odd_kpmg_2019.xls",sheet="Sheet1",skip=2) %>% mutate(year=2019)
odd_2020 <- import("r_input/odd_kpmg_2020.xls",sheet="Sheet1",skip=2) %>% 
  mutate(year=2020,fiyat=as.numeric(fiyat),engine_displacement=as.numeric(engine_displacement),co2=as.numeric(co2),
         yakit_tuketimi=as.numeric(as.character(sub("," , ".", yakit_tuketimi))))





#otv oranlari
eski_otv_oranlari <- import("r_input/otv_structure.xlsx",sheet="ice_eski")
mevcut_otv_oranlari <- import("r_input/otv_structure.xlsx",sheet="ice_mevcut")
yeni_otv_oranlari<- import("r_input/otv_structure.xlsx",sheet="yeni_otv")
sales_forecast<-  import("r_input/sales_forecast.xls")





#mtv oranlari ----
mtv_oranlari <- import("r_input/mtv_oranlari.xlsx",sheet="2020_mtv")
arac_omru <- import("r_input/mtv_oranlari.xlsx",sheet="degiskenler")[1,2]
mtv_per_co2 <- import("r_input/mtv_oranlari.xlsx",sheet="degiskenler")[2,2]
otv_grubuna_dayali_mtv <- import("r_input/mtv_oranlari.xlsx",sheet="OTV_grubuna_dayali_MTV")
co2_gruplari <- import("r_input/mtv_oranlari.xlsx",sheet="co2_araliklari")
otv_grubu_co2_araliklari <- import("r_input/mtv_oranlari.xlsx",sheet="otv_grubu_co2_araliklari")

lcv_mtv_oranlari <- import("r_input/mtv_oranlari.xlsx",sheet="LCV_MTV_oranlari")
lcv_co2_gruplari <- import("r_input/mtv_oranlari.xlsx",sheet="lcv_co2_araliklari")
lcv_mtv_grubu_co2_araliklari <- import("r_input/mtv_oranlari.xlsx",sheet="lcv_co2_aralikli_mtv")

hdv_mtv_oranlari <- import("r_input/mtv_oranlari.xlsx",sheet="HCV_MTV")
hdv_mtv_per_co2 <- import("r_input/mtv_oranlari.xlsx",sheet="degiskenler")[3,2]



# segment ve esneklik----
segment_look_up<- import('r_input/esneklik.xlsx',sheet="segments")
kendi_esnekligi <- import('r_input/esneklik.xlsx',sheet="esneklik (binek)")[1,2]
rakip_esnekligi <- import('r_input/esneklik.xlsx',sheet="esneklik (binek)")[2,2]
tco_esnekligi <- import('r_input/esneklik.xlsx',sheet="esneklik (binek)")[3,2]
segment_capraz_esneklik <- import('r_input/esneklik.xlsx',sheet="kleit_segment_elasticity")
lcv_esneklik <- import('r_input/esneklik.xlsx',sheet="LCV esneklik")[1,2]
hdv_esneklik <- import('r_input/esneklik.xlsx',sheet="HDV")[1,2]


# yakit tuketimi----
benzin_litre_otv <-  import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[1,2]
dizel_litre_otv  <-  import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[2,2]
arac_yillik_km     <-  import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[3,2]
benzin_litre_kdv <-  import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[4,2]
dizel_litre_kdv  <-  import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[5,2]
PC_yillik_km <- import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[6,2]
LCV_yillik_km <- import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[7,2]
HDV_yillik_km <- import("r_input/yakit_tuketimi.xlsx",sheet="degiskenler")[8,2]

#hurda tesviki----
yeni_arac_indirim_orani_grup1 <- import("r_input/hurda_tesviki.xlsx",sheet="binek_arac") [1,2]
yeni_arac_indirim_orani_grup2 <- import("r_input/hurda_tesviki.xlsx",sheet="binek_arac") [2,2]
LCV_hurda_tesvik_orani <- import("r_input/hurda_tesviki.xlsx",sheet="LCV") [1,2]
HDV_hurda_tesvik_orani <- import("r_input/hurda_tesviki.xlsx",sheet="HDV") [1,2]




# kredi indirimi----
kredi_orani <- import("r_input/kredi_indirimi.xlsx",sheet="binek") [1,2]
ortalama_vade <- import("r_input/kredi_indirimi.xlsx",sheet="binek") [2,2]
mevcut_yillik_faiz <- import("r_input/kredi_indirimi.xlsx",sheet="binek") [3,2]
indirimli_yillik_faiz <- import("r_input/kredi_indirimi.xlsx",sheet="binek") [4,2]
max_indirimli_kredi_miktari <- import("r_input/kredi_indirimi.xlsx",sheet="binek") [5,2]
kredi_kullanan_arac_orani <- import("r_input/kredi_indirimi.xlsx",sheet="binek") [6,2]

#lcv
lcv_kredi_orani <- import("r_input/kredi_indirimi.xlsx",sheet="LCV") [1,2]
lcv_ortalama_vade <- import("r_input/kredi_indirimi.xlsx",sheet="LCV") [2,2]
lcv_max_indirimli_kredi_miktari <- import("r_input/kredi_indirimi.xlsx",sheet="LCV") [5,2]

#hdv
hdv_kredi_orani <- import("r_input/kredi_indirimi.xlsx",sheet="HDV") [1,2]
hdv_ortalama_vade <- import("r_input/kredi_indirimi.xlsx",sheet="HDV") [2,2]
kamyon_max_indirimli_kredi_miktari <- import("r_input/kredi_indirimi.xlsx",sheet="HDV") [5,2]
otobus_max_indirimli_kredi_miktari <- import("r_input/kredi_indirimi.xlsx",sheet="HDV") [7,2]

# Agustosa kadar yillik satislar----
odd_agustos_2019<-  odd_2019 %>% select(-eylul:-aralik)
odd_agustos_2018<-  odd_2018 %>% select(-eylul:-aralik)
odd_agustos <- rbind(odd_2020,odd_agustos_2019)
odd_agustos <- rbind(odd_agustos,odd_agustos_2018)
odd_agustos$toplam <- rowSums(odd_agustos[,39:46])



# LCV segmenti----
odd_lcv_2020 <- import("r_input/LCV_odd_kpmg_2020.xls",sheet="Sheet1",skip=2) %>% 
  mutate(fiyat=as.numeric(as.character(sub("," , ".", fiyat))),
         engine_displacement=as.numeric(as.character(sub("," , ".", engine_displacement))),
         co2=as.numeric(as.character(sub("," , ".", co2))),
         agirlik=as.numeric(as.character(sub("," , ".", agirlik))),
         yakit_tuketimi=as.numeric(as.character(sub("," , ".", yakit_tuketimi))))


lcv_eski_otv_oranlari <- import("r_input/LCV_HCV inputs.xlsx",sheet="LCV_OTV")



#HDV segmenti -----
 odd_hdv_2020 <- import("r_input/LCV_HCV inputs.xlsx",sheet="HCV") #%>% 
#   mutate(
#          co2=as.numeric(as.character(sub("," , ".", co2))),
#          yakit_tuketimi=as.numeric(as.character(sub("," , ".", yakit_tuketimi))))


# Grafik ve renkler ---- 
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

koyu_mavi = "#28364A"
acik_mavi = "#B2D3E1"
kirmizi = "#370617"
sari = "#F7C45F"  
yesil = "#1a535c"

mavi= "#7293CB"	
gri = "#E1974C"


palet <- c("#555b6e","#89b0ae","#faf9f9","#ffd6ba","#bee3db")
palet1 <- c(koyu_mavi,kirmizi,yesil)
milyar<-1000000000


# Grafik theme 
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}











