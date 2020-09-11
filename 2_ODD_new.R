
#ODD Data#--------------------------------------------------------------------------------------
j<-1
odd_pc<-NULL
  for (i in 2007:2017) {
  a <- read_excel("r_input/ODD TURKEY PASSENGER CAR RETAIL SALES (2007-2017).xls", 
                  sheet = j, skip = 1)
  colnames(a) <- c('make','model','submodel','segment','version','engine_size','transmission','body_type',
                   'fuel_type','production','motor_power','gross_vehicle_weight','co2_emission','sales')
  a$year<- i
  odd_pc<-rbind(a,odd_pc)
  odd_pc<- filter(odd_pc,!is.na(odd_pc$make))
  j<- j+1
  rm(a)
  }

#not: bu arada engine_power horsepower olarak yazilmis 

odd_pc$mtvgroup <- ifelse(odd_pc$engine_size<=1300,1,
                          ifelse(between(odd_pc$engine_size,1301,1600),2,
                                 ifelse(between(odd_pc$engine_size,1601,1800),3,
                                        ifelse(between(odd_pc$engine_size,1801,2000),4,   
                                               ifelse(between(odd_pc$engine_size,2001,2500),5,
                                                      ifelse(between(odd_pc$engine_size,2501,3000),6,
                                                             ifelse(between(odd_pc$engine_size,3001,3500),7,
                                                                    ifelse(between(odd_pc$engine_size,3501,4000),4,
                                                                           9))))))))

odd_pc<- odd_pc%>%mutate_all(tolower)
cols.num <- c("engine_size","motor_power",'gross_vehicle_weight','co2_emission','sales','mtvgroup','year')
odd_pc[cols.num] <- sapply(odd_pc[cols.num],as.numeric)

odd_2017 <- import("R_input/Turkey Car and LCV Retail Sales Jan -Dec'2017.xls",sheet = 1, skip = 2) %>% 
  filter(!is.na(MODEL)) %>% select(MAKE,MODEL,SUBMODEL='SUB MODEL',SEGMENT,VERSION, engine_size="ENGINE SIZE (CC)",
                                   TRANSMISSION,BODY_TYPE="BODY TYPE",FUEL_TYPE="FUEL TYPE",PRODUCTION="IMPORT/ \nPRODUCTION",
                                   MOTOR_POWER="MOTOR POWER (PS)",GROSS_VEHICLE_WEIGHT="GROSS VEHICLE WEIGHT(kg)",
                                   SALES="Top-17")
odd_2017<- filter(odd_2017,SALES>0)
colnames(odd_2017)<- tolower(colnames(odd_2017))
odd_2017<- odd_2017%>%mutate_all(tolower)
cols.num <- c("engine_size","motor_power",'gross_vehicle_weight','sales')
odd_2017[cols.num] <- sapply(odd_2017[cols.num],as.numeric)
raw_2017<- odd_2017
co2<- filter(odd_pc,year==2017) %>% group_by(make,model,submodel,segment,version,engine_size,transmission,body_type,fuel_type,production,motor_power,gross_vehicle_weight,sales,mtvgroup)%>% summarise(co2_emission=weighted.mean(co2_emission,sales,na.rm=T))
co2<- na.omit(co2)
# no CO2 data is in odd_2017, so we matched it with odd_pc(2017). 32000 cars data is gone after this merge
odd_2017 <- left_join( unique(odd_2017),unique(co2), by = c('make','model','submodel','segment',"version","engine_size",'transmission',"body_type","fuel_type",'production','motor_power','gross_vehicle_weight'))
odd_2017<- odd_2017 %>% select('make','model','submodel','segment','version','engine_size','transmission','body_type',
                               'fuel_type','production','motor_power','gross_vehicle_weight','co2_emission',sales="sales.x","mtvgroup")


sum(odd_2017$sales,na.rm=T)
odd_2017$year <- 2017

odd_pc <- filter(odd_pc, year != 2017)
odd_pc<-  rbind(odd_pc,odd_2017)
odd_pc$co2_emission<-as.numeric(as.character(odd_pc$co2_emission))
odd_pc$age<- 2018-odd_pc$year

odd_pc$fuel_type<- gsub('dizel','diesel',odd_pc$fuel_type)
odd_pc$fuel_type<- gsub('benzin','gasoline',odd_pc$fuel_type)
odd_pc$fuel_type<- gsub('elektrik','electric',odd_pc$fuel_type)


#adding oems#
oem_masterdatabase<- import('R_input/OEM_Masterdatabase.xlsx') %>% mutate_all(tolower)
odd_pc$oem <- oem_masterdatabase$oem[match(odd_pc$make,oem_masterdatabase$make)]
odd_pc$oem[odd_pc$make=='opel' & odd_pc$year!=2017] <- 'gm'
#lcv
j<-1
odd_lcv <- NULL
for (i in 2007:2016) {
  b  <- read_excel("R_input/ODD TURKEY LIGHT COMMERCIAL VEHICLE RETAIL SALES (2007-2017).xls",   
                   sheet = j, skip = 1)
  colnames(b) <- c('MAKE','MODEL','SUBMODEL','VERSION','engine_size','TRANSMISSION','BODY_TYPE','FUEL_TYPE',
                   'PRODUCTION','MOTOR_POWER','GROSS_VEHICLE_WEIGHT','CO2_EMISSION','SALES')
  b$year <- i
  odd_lcv <- rbind(odd_lcv,b)
  odd_lcv<- filter(odd_lcv,!is.na(odd_lcv$MAKE))
  j<- j+1
  rm(b)
}


odd_pc$group_names<- odd_pc$oem
odd_pc$group_names<- case_when(
  odd_pc$oem=='fiat' ~ 'FCA',
  odd_pc$oem=="volkswagen"~ 'Volkswagen Group',
  odd_pc$oem=="bmw"~ 'BMW Group',
  odd_pc$oem=="psa"~ 'Groupe PSA',
  odd_pc$oem=="renault"~ 'renault-nissan',
  odd_pc$oem=="chrysler" ~ 'FCA',
  TRUE~odd_pc$group_names
)




#IHS Data---------------------------------------------------------------------------------
turkey<- read.csv("R_input/turkey_2014_2015")
turkey <- data.frame( MAKE = turkey$Manufacturer,MODEL = turkey$Model, SEGMENT = turkey$Segment,engine_size = turkey$engine_capacity,
                      TRANSMISSION = turkey$transmission_type, BODY_TYPE = turkey$body_type, FUEL_TYPE = turkey$fuel_type,
                      MOTOR_POWER = turkey$engine_power, MASS = turkey$mass_running_order_average, CO2_EMISSION = turkey$CO2_Emissions_Average,
                      sales = turkey$total, year = turkey$year, price = turkey$Price_Average, emission_standart = turkey$emission_standard) 
turkey2016 <- read_excel("R_input/Turkey2016.xlsx") 
turkey2016 <- data.frame( MAKE = turkey2016$Manufacturer,MODEL = turkey2016$Model, SEGMENT = turkey2016$`Segment (vehicle type)`,engine_size = turkey2016$`Engine ccm`,
                          TRANSMISSION = turkey2016$`Transmission type`, BODY_TYPE = turkey2016$`Body type`, FUEL_TYPE = turkey2016$`Fuel type`,
                          MOTOR_POWER = turkey2016$`Engine kw`, MASS = NA, CO2_EMISSION = turkey2016$`Carbon emission`,
                          sales = turkey2016$Total, year = 2016, price = turkey2016$Price, emission_standart = turkey2016$`Emission standard`)
ihs_pc <- rbind(turkey,turkey2016)
rm(turkey);rm(turkey2016)
ihs_pc[ihs_pc=='Unspecified' | ihs_pc==-1] <- NA
#lcv
lcv_turkey <- read.csv("R_input/turkey_lcv_2014_2015")
lcv_turkey <- data.frame( MAKE = lcv_turkey$Manufacturer,MODEL = lcv_turkey$Model, SEGMENT = lcv_turkey$Segment,engine_size = lcv_turkey$Engine_ccm,
                          TRANSMISSION = lcv_turkey$Transmission_type, BODY_TYPE = lcv_turkey$Body_type, FUEL_TYPE = lcv_turkey$Fuel_type,
                          MOTOR_POWER = lcv_turkey$Engine_kW, MASS = lcv_turkey$mass_in_running_order, CO2_EMISSION = lcv_turkey$co2_emission,
                          sales = lcv_turkey$total, year = lcv_turkey$Year, price = NA , emission_standart = lcv_turkey$emission_standard) 

lcv_turkey2016 <- read_excel('R_input/lcv_turkey2016.xlsx')
lcv_turkey2016 <- data.frame( MAKE = lcv_turkey2016$Manufacturer,MODEL = lcv_turkey2016$Model, SEGMENT = lcv_turkey2016$`Segment (vehicle type)`,engine_size = lcv_turkey2016$`Engine ccm`,
                              TRANSMISSION = lcv_turkey2016$`Transmission type`, BODY_TYPE = lcv_turkey2016$`Body type`, FUEL_TYPE = lcv_turkey2016$`Fuel type`,
                              MOTOR_POWER = lcv_turkey2016$`Engine kw`, MASS = NA, CO2_EMISSION = NA,
                              sales = lcv_turkey2016$`1/2016-12/2016` , year = 2016, price = NA , emission_standart = NA)

ihs_lcv <- rbind(lcv_turkey,lcv_turkey2016)
rm(lcv_turkey);rm(lcv_turkey2016);rm(i);rm(j)


#Updated Price------------------------------------------------------------------------------
updated_price_2014<- read.csv('R_input/Updated_Price_2014')
updated_price_2015 <- read.csv('R_input/Updated_Price_2015')
price_2014_2015 <- rbind(updated_price_2014,updated_price_2015) ; rm(updated_price_2014,updated_price_2015)
taxdata_2016 <- read.csv('R_input/Updated_Price_2016')
taxdata_2016$NOx<- ifelse(taxdata_2016$FUEL=='HYBRID',60,taxdata_2016$NOx)
taxdata_2016 <- taxdata_2016 %>% select(MAKE,MODEL,IMPORT.,ENGINE,FUEL,MOTOR,EMISSION,NOx,sales,price,netprice) %>% na.omit()
taxdata_2016$NOx <- ifelse(taxdata_2016$NOx==180,80,taxdata_2016$NOx) # every car will have euro6
taxdata_2016$price_2016 <- taxdata_2016$price
taxdata_2016[taxdata_2016$MODEL=='EGEA' & taxdata_2016$sales==1950,]$EMISSION<-143  # the emission was wrong##
#2017 Price
taxdata_2016$netprice<- taxdata_2016$netprice/3.83*4.6
taxdata_2016$group <- ifelse(between(taxdata_2016$ENGINE,0,1600) & between(taxdata_2016$netprice,0,46000),1,
                             ifelse(between(taxdata_2016$ENGINE,0,1600) & between(taxdata_2016$netprice,46000,80000),2,
                                    ifelse(between(taxdata_2016$ENGINE,0,1600) & between(taxdata_2016$netprice,80000,Inf),3,
                                           ifelse(between(taxdata_2016$ENGINE,1601,2000) & between(taxdata_2016$netprice,0,114000),4,
                                                  ifelse(between(taxdata_2016$ENGINE,1601,2000) & between(taxdata_2016$netprice,114000,Inf),5,
                                                         6)))))

taxdata_2016$otv <- ifelse(between(taxdata_2016$ENGINE,0,1600) & between(taxdata_2016$netprice,0,46000),0.45*taxdata_2016$netprice,
                           ifelse(between(taxdata_2016$ENGINE,0,1600) & between(taxdata_2016$netprice,46000,80000),(0.5)*taxdata_2016$netprice,
                                  ifelse(between(taxdata_2016$ENGINE,0,1600) & between(taxdata_2016$netprice,80000,Inf),(0.6)*taxdata_2016$netprice,
                                         ifelse(between(taxdata_2016$ENGINE,1601,2000) & between(taxdata_2016$netprice,0,114000),1*taxdata_2016$netprice,
                                                ifelse(between(taxdata_2016$ENGINE,1601,2000) & between(taxdata_2016$netprice,114000,Inf),(1.1)*taxdata_2016$netprice,
                                                       1.6*taxdata_2016$netprice)))))
taxdata_2016$price <- (taxdata_2016$netprice+ taxdata_2016$otv)*1.18
taxdata_2016$taxrate<- taxdata_2016$otv/taxdata_2016$netprice

##


#IHS2017----
rm(taxdata_2016)
ihs_2017<- import('R_input/ihs_turkey_2017.xlsx') %>% mutate_all(tolower)
ihs_2017$Price_Average<- as.numeric(ihs_2017$Price_Average)
ihs_2017$engine_capacity<- as.numeric(ihs_2017$engine_capacity)
ihs_2017$total<- as.numeric(ihs_2017$total)
ihs_2017$OEM <- ifelse(ihs_2017$OEM=="chrysler",'fiat',ihs_2017$OEM)

