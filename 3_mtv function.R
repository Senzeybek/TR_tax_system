#data#####
#clearing passenger car data
vehicle_on_the_road <- import("r_input/mtv_data.xlsx")
mtv_amount<- import("r_input/2018_mtv.xlsx") 
mtv_amount<-mtv_amount%>% gather(key ='age group','mtv amount', `1-3 age`:`16 or older`)
vehicle_on_the_road <- vehicle_on_the_road %>% mutate_all(tolower)
PassengerCar <- filter(vehicle_on_the_road,cinsadi=='otomobil') 
PassengerCar <- PassengerCar %>% select(fuel=yakiti, model=Modeli, engine_displacement = Silindir,total=Sayi)
PassengerCar$model <-as.numeric(PassengerCar$model)
PassengerCar$total <-as.numeric(PassengerCar$total)
PassengerCar$fuel<- gsub('benzinli','gasoline',PassengerCar$fuel)
PassengerCar$fuel<- gsub('benzin','gasoline',PassengerCar$fuel)
PassengerCar$fuel<- gsub('dizel','diesel',PassengerCar$fuel)
PassengerCar$fuel<- gsub('elektrik','electric',PassengerCar$fuel)
PassengerCar$fuel<- gsub('electricli','electric',PassengerCar$fuel)

PassengerCar[PassengerCar=='bilinmiyor'] <- NA  # engine displacement is missing for 600,000 cars
PassengerCar$age <- 2018- PassengerCar$model
PassengerCar$engine_group <- ifelse(
  PassengerCar$engine_displacement == "1300-", 1,
  ifelse(PassengerCar$engine_displacement == '1301-1400',2,
         ifelse(PassengerCar$engine_displacement == "1401-1500",2,
                ifelse(PassengerCar$engine_displacement == "1501-1600",2,
                       ifelse(PassengerCar$engine_displacement == "1601-2000",3,
                              ifelse(PassengerCar$engine_displacement == "2001+",4,NA))))))

PassengerCar$age_group <- case_when(
  between(PassengerCar$age,1,3)   ~1,
  between(PassengerCar$age,4,6)   ~2,
  between(PassengerCar$age,7,11)  ~3,
  between(PassengerCar$age,12,15) ~4,
  PassengerCar$age>=16 ~5
)

#clearing mtv amount data
colnames(mtv_amount) <- c('mtv_group','age','mtv')
mtv_amount$age<- gsub('age','',mtv_amount$age)
mtv_amount$age<- gsub(' or older','-Inf',mtv_amount$age)
mtv_amount$mtv_group<- gsub('4000 - Inf','4000-Inf',mtv_amount$mtv_group)
a <-cbind(str_split_fixed(mtv_amount$`mtv_group`,'-',2),str_split_fixed(mtv_amount$age,'-',2))
mtv_amount<-cbind(a,mtv_amount)
mtv_amount <- select(mtv_amount, engine_min=1,engine_max=2,age_min=3,age_max=4,mtv='mtv')
mtv_amount$engine_group <-case_when(
  mtv_amount$engine_min==0 ~ 1,
  mtv_amount$engine_min==1301~2,
  mtv_amount$engine_min==1601~3,
  mtv_amount$engine_min==1801~3,
  mtv_amount$engine_min==2001~4,
  mtv_amount$engine_min==2501~4,
  mtv_amount$engine_min==3001~4,
  mtv_amount$engine_min==3501~4,
  mtv_amount$engine_min==4000~4,
  TRUE ~4
)

mtv_amount$age_group <- case_when(
  mtv_amount$age_min==1 ~ 1 ,
  mtv_amount$age_min==4~2,
  mtv_amount$age_min==7~3,
  mtv_amount$age_min==12~4,
  mtv_amount$age_min==16~5
)
rm(a)

#there is not any 1600-1800cc or 2000-2500 segment on passenger car data, 
#therefore we made this calculation, ratios (weights) are found by using odd_pc data 
#filter(odd_pc,between(engine_size,1600,2000)) %>% group_by(engine_size<=1600)%>% summarise(a=sum(sales)) %>% mutate(b=a/sum(a))
#filter(odd_pc,engine_size>2000) %>% group_by(engine_size<=2500,
#                                                between(engine_size,2501,3000),between(engine_size,3001,3500),
#                                                 between(engine_size,3501,4000),engine_size>4000)%>%
#      summarise(a=sum(sales))%>% mutate(b=a/84968)

mtv_amount[mtv_amount$engine_group==3,]$mtv<-  rep(filter(mtv_amount,engine_group==3,engine_min==1601)$mtv*0.17 +
                                          filter(mtv_amount,engine_group==3,engine_min==1801)$mtv*0.83,each=2)

mtv_amount[mtv_amount$engine_group==4,]$mtv <- rep(filter(mtv_amount,engine_group==4,engine_min==2001)$mtv*0.41 +
filter(mtv_amount,engine_group==4,engine_min==2501)$mtv*0.51+
filter(mtv_amount,engine_group==4,engine_min==3001)$mtv*0.02+
filter(mtv_amount,engine_group==4,engine_min==3501)$mtv*0.03+
filter(mtv_amount,engine_group==4,engine_min== 4000)$mtv*0.03,each=5)



PassengerCar<- (inner_join(PassengerCar,select(mtv_amount,age_group,engine_group,mtv), by=c('age_group','engine_group')))
# yukaridaki gibi yapinda  birlestirdigin gruplar icin duplicate rakamlar olustu #
# ayni satir birkac defa yazildi# onu toparlamak icin asagidaki gibi devam ettin # 
PassengerCar <- unique(PassengerCar)
sum(PassengerCar$total*PassengerCar$mtv)/1000000000

odd_pc$engine_group <-case_when (
  between(odd_pc$engine_size,0,1300) ~ 1,
  between(odd_pc$engine_size,1301,1600)~2,
  between(odd_pc$engine_size,1601,2000)~3,
  odd_pc$engine_size>2000~4
)





#backcasting####
emission_average <- data.frame((odd_pc%>% group_by(engine_group,fuel_type,year) %>% summarise(co2 =weighted.mean(co2_emission,sales,na.rm=T))) %>% na.omit())
emission_average <- emission_average %>% filter ( fuel_type %in% c('gasoline','diesel'))
emission_average$id <-tolower(paste(emission_average$engine_group,emission_average$fuel_type,sep = '_') ) 
emission_average<- emission_average %>% select(id=id,year=year,co2=co2) 

co2_backcast<-NULL
a<- unique(emission_average$id)

for(i in 1:length(unique(emission_average$id))) {
abc<- emission_average%>% filter(id==a[i])%>%  select(co2)
tsabc<- as.ts(abc)
x <- tsabc
h<-length(1984:2006)
f<- frequency(abc)
revx <- ts(rev(x), frequency=f)
#model secimi icin auto.arima(revx,trace=T) fonksiyonunu denedin ama bazilari drifsiz cikti aralarinda en iyi sonucu c(0,1,0) verdi
# eski degerlere ait forecasti asagidaki for fonksiyonu ile yaptin
fc <- forecast(Arima(revx,order = c(0,1,0), include.drift = T), h)
plot(fc)

fc$mean <- ts(rev(fc$mean),end=tsp(x)[1] - 1/f, frequency=f)
fc$upper <- fc$upper[h:1,]
fc$lower <- fc$lower[h:1,]
fc$x <- x
plot(fc,xlim=c(tsp(x)[1]-h/f, tsp(x)[2]),main = a[i],ylab = 'CO2')
backcast <- data.frame(co2=fc$mean,model=1984:2006 )
abc$model<- 2007:2017

b<-arrange(rbind(select(abc,model,co2),backcast),model)
b$id <-a[i]
co2_backcast<- rbind(co2_backcast,b)
}
rm(abc);rm(a);rm(b)
#buldugun sonuclari bir araya asagidaki gibi getirdin
co2_backcast$engine_group<-as.numeric(str_split_fixed(co2_backcast$id,'_',length(co2_backcast))[,1])
co2_backcast$fuel<- str_split_fixed(co2_backcast$id,'_',length(co2_backcast))[,2]
#lpg ile benzin emisyonlari aynidir dedin
lpg<- filter(co2_backcast,fuel=='gasoline')
lpg$fuel<- 'lpg' 
co2_backcast<- rbind(co2_backcast,lpg)
PassengerCar<- (full_join(PassengerCar,co2_backcast,by=c('engine_group','fuel','model')))
PassengerCar$id <- paste(PassengerCar$engine_group,PassengerCar$fuel,PassengerCar$model,sep = '_')
revenue <- PassengerCar%>% group_by(fuel,engine_group,model) %>% summarise(tax_revenue=sum(mtv*total))

rm(backcast,co2_backcast,emission_average)

#toplam vergi geliri
PassengerCar$co2_tax <- PassengerCar$mtv / PassengerCar$co2
 sum(PassengerCar$co2_tax*PassengerCar$co2*PassengerCar$total,na.rm=T)/1000000000
 
 
# birlestirmek icin 
new_mtv_long<-PassengerCar%>% group_by(engine_group,age_group,fuel) %>%  
        summarise(co2=weighted.mean(co2,total,na.rm=T),mtv=weighted.mean(mtv,total,na.rm=T),co2_tax=weighted.mean(co2_tax,total,na.rm=T),total_sales=sum(total)) %>% na.omit()
## yil yerine age group olarak hesaplamak icin asagidaki kodu yazdin
PassengerCar$co2_tax<- NULL
PassengerCar<- inner_join(PassengerCar,select(new_mtv_long,engine_group,age_group,fuel,co2_tax ),by=c('engine_group','age_group','fuel'))


new_mtv<- spread(select(new_mtv_long,engine_group,fuel,co2_tax,age_group),key = age_group,value = co2_tax) %>% group_by(engine_group,fuel)
new_mtv$engine_group <- ifelse(new_mtv$engine_group=='1','≤ 1300cc',
                               ifelse(new_mtv$engine_group=='2','1301-1600cc',
                                      ifelse(new_mtv$engine_group=='3','1601-2000cc',
                                             '>2000cc')))

colnames(new_mtv) <- c('engine_group','fuel','1-3 age','4-6 age', '7-11 age', '12-15 age', 'older than 16')



##### No Age System ###
lpg_gasoline <- PassengerCar
lpg_gasoline$fuel <- ifelse(lpg_gasoline$fuel=='lpg','gasoline',lpg_gasoline$fuel)
no_age <- na.omit(lpg_gasoline)%>% group_by(fuel) %>% 
  summarise(average_age=weighted.mean(age,total,na.rm=T),average_mtv= weighted.mean(mtv,total,na.rm=T),
            average_emission=weighted.mean(co2,total,na.rm=T),total_sales=sum(total,na.rm=T), total_revenue = sum(mtv*total,na.rm=T),
            total_emission=sum(co2*total,na.rm=T))%>%
  filter(total_emission>0)
no_age$co2_tax <- no_age$total_revenue/no_age$total_emission 
(no_age)
no_age_long <- new_mtv_long
no_age_long$co2_tax <- ifelse(no_age_long$fuel=='diesel',8.386948,
                              ifelse(no_age_long$fuel %in% c('gasoline','lpg'),2.819387,0)) 
no_age_long$mtv<- no_age_long$co2*no_age_long$co2_tax
PassengerCar$no_age_mtv <- ifelse(PassengerCar$fuel %in% c('gasoline','lpg'),2.819387*PassengerCar$co2,
                                  ifelse(PassengerCar$fuel=='diesel',8.386948*PassengerCar$co2,NA)) 
no_age_mtv <- spread(select(no_age_long,engine_group,fuel,mtv,age_group),key = age_group,value = mtv) %>% group_by(engine_group,fuel)
no_age_mtv$engine_group <- ifelse(no_age_mtv$engine_group=='1','≤ 1300cc',
                               ifelse(no_age_mtv$engine_group=='2','1301-1600cc',
                                      ifelse(no_age_mtv$engine_group=='3','1601-2000cc',
                                             '>2000cc')))
colnames(no_age_mtv) <- c('engine_group','fuel','1-3 age','4-6 age', '7-11 age', '12-15 age', 'older than 16')
sum(no_age_long$mtv*no_age_long$total_sales,na.rm=T)
no_age_mtv
rm(lpg,lpg_gasoline)





#revenue----
PassengerCar%>% group_by(age,fuel) %>% summarise(mtv=sum(mtv*total),no_age=sum(no_age_mtv*total))%>% mutate(share_mtv=mtv/sum(mtv),share_no_age=no_age/sum(no_age))
#revenue byfuel
PassengerCar%>% group_by(fuel) %>% summarise(emission=sum(co2*total), mtv=sum(mtv*total),no_age=sum(no_age_mtv*total),co2_mtv=sum(total*co2*co2_tax))%>% mutate(share_mtv=mtv/sum(mtv),share_no_age=no_age/sum(no_age))
#revenue by age
PassengerCar%>% group_by(age_group) %>% summarise(emission=sum(co2*total), mtv=sum(mtv*total),no_age=sum(no_age_mtv*total),co2_mtv=sum(total*co2*co2_tax))%>% mutate(share_mtv=mtv/sum(mtv),share_no_age=no_age/sum(no_age))


#Euro norms------

emission_standard_limits <-import("~/Desktop/Turkey- tax System/R /R_input/emission_standard_limits.xlsx")

PassengerCar$emission_standard <- case_when(
  PassengerCar$model %in% c(2017,2016) ~'Euro6',
  PassengerCar$model %in% c(2015,2014,2013) ~ 'Euro5',
  PassengerCar$model %in% c(2012,2011,2010) ~ 'Euro4',
  PassengerCar$model %in% c(2009,2008,2007) ~ 'Euro3',
  PassengerCar$model %in% c(2006,2005,2004) ~ 'Euro2',
  PassengerCar$model < 2004 ~ 'Euro1'
)
PassengerCar<- left_join(PassengerCar,emission_standard_limits,by=c('emission_standard','fuel'))



