#taxdata for 2017 ----

price_2017 <- import(file = 'r_input/price_2017.xlsx')%>% select(model,body_type,fuel_type,engine_size,transmission,price_tl,price_euro) %>% na.omit()
colnames(price_2017)
taxdata<- odd_2017
taxdata$ihs_segment<-ihs_2017$Segment[match(taxdata$model,ihs_2017$j_model)] 
taxdata[taxdata$segment=='b5',]$ihs_segment<- "a car derived vans"

taxdata$transmission<- case_when(
  taxdata$transmission=='otomatik'~'a',
  taxdata$transmission=='düz'~'m')



taxdata$model<- paste(odd_2017$make,odd_2017$model)
taxdata$fuel_type<- case_when(
  taxdata$fuel_type=='benzin'~'petrol',
  taxdata$fuel_type=='dizel'~'diesel',
  taxdata$fuel_type=='elektrik'~'electric',
  taxdata$fuel_type=='hybrid'~"hybrid/petrol"
)

price_2017$id<- paste(price_2017$model,price_2017$body_type,price_2017$fuel_type,price_2017$engine_size,price_2017$transmission)
taxdata$id <- paste(taxdata$model,taxdata$body_type,taxdata$fuel_type,taxdata$engine_size,taxdata$transmission)
taxdata$price <- price_2017$price_tl[match(taxdata$id,price_2017$id)]
taxdata %>% filter(!is.na(price))%>% summarise(sum(sales))
#oem
taxdata$oem <- oem_masterdatabase$oem[match(taxdata$make,oem_masterdatabase$make)]

#changing the wrong names for the graphs
taxdata$model<- gsub('serisi','series',taxdata$model)
taxdata$model<- gsub('mercedes-benz c series','mercedes-benz C series',taxdata$model)
taxdata$model<- gsub('toyota c-hr','toyota C-HR',taxdata$model)
taxdata$model<- gsub('citroen c-elysee','citroen C-elysee',taxdata$model)

#segment conversion ----
segment_look_up<- import('R_input/segment conversation.xlsx')

taxdata <- taxdata%>% separate(segment, 
                    into = c("segment_text", "segment_num"), 
                    sep = "(?<=[A-Za-z])(?=[0-9])")
# suv segments are numbered as 7 in odd data
taxdata[is.na(taxdata$ihs_segment)&taxdata$segment_num==7,]$ihs_segment<- "f off-road"
#matching empty ihs segments by odd letters
taxdata[is.na(taxdata$ihs_segment),]$ihs_segment<- segment_look_up$ihs_segment[match(taxdata[is.na(taxdata$ihs_segment),]$segment_text,segment_look_up$segment_text)]
#convertion ihs segments to kleit segment
taxdata$kleit_segment<-segment_look_up$kleit_segment[match(taxdata$ihs_segment,segment_look_up$ihs_segment)] 
taxdata$kleit_segment[taxdata$ihs_segment=='f off-road' & taxdata$gross_vehicle_weight>3080]<- 'large_suv'

#tax calculations ----

otv_thresholds <- c( 70000*1.45*1.18, 120000*1.5*1.18, 170000*2.0*1.18, 85000*1.45*1.18,135000*1.5*1.18,170000*2.0*1.18)

taxdata$otv_group<-case_when(
  taxdata$engine_size<=1600 & taxdata$price < otv_thresholds[1] ~1,
  taxdata$engine_size<=1600 & between(taxdata$price,otv_thresholds[1],otv_thresholds[2]) ~2,
  taxdata$engine_size<=1600 & taxdata$price>otv_thresholds[2] ~3,
  between(taxdata$engine_size,1601,2000) & taxdata$price<=otv_thresholds[3] ~4,
  between(taxdata$engine_size,1601,2000) & taxdata$price>otv_thresholds[3] ~ 5,
  taxdata$engine_size > 2000 ~ 6,
)
# tax exemptions for hybrid cars with bigger than 1.6l engines
taxdata[taxdata$fuel_type=='hybrid/petrol', ]$otv_group <- case_when(
  between(taxdata[taxdata$fuel_type=='hybrid/petrol', ]$engine_size,1601,2000) & taxdata[taxdata$fuel_type=='hybrid/petrol', ]$price <otv_thresholds[4] ~1,
  between(taxdata[taxdata$fuel_type=='hybrid/petrol', ]$engine_size,1601,2000) & between(taxdata[taxdata$fuel_type=='hybrid/petrol', ]$price,otv_thresholds[4],otv_thresholds[5]) ~2,
  between(taxdata[taxdata$fuel_type=='hybrid/petrol', ]$engine_size,1601,2000) & taxdata[taxdata$fuel_type=='hybrid/petrol', ]$price > otv_thresholds[4] ~3,
  between(taxdata[taxdata$fuel_type=='hybrid/petrol', ]$engine_size,2000,2500) & taxdata[taxdata$fuel_type=='hybrid/petrol', ]$price <=otv_thresholds[6] ~4,
  between(taxdata[taxdata$fuel_type=='hybrid/petrol', ]$engine_size,2000,2500) & taxdata[taxdata$fuel_type=='hybrid/petrol', ]$price >otv_thresholds[6] ~5,
  TRUE~taxdata[taxdata$fuel_type=='hybrid/petrol' , ]$otv_group 
)

taxdata$netprice<- case_when(
  taxdata$otv_group==1~ taxdata$price/1.45/1.18,
  taxdata$otv_group==2~ taxdata$price/1.50/1.18,
  taxdata$otv_group==3~ taxdata$price/1.60/1.18,
  taxdata$otv_group==4~ taxdata$price/2.00/1.18,
  taxdata$otv_group==5~ taxdata$price/2.10/1.18,
  taxdata$otv_group==6~ taxdata$price/2.60/1.18
)

taxdata$otv_rate<- case_when(
  taxdata$otv_group==1~ 0.45,
  taxdata$otv_group==2~ 0.50,
  taxdata$otv_group==3~ 0.60,
  taxdata$otv_group==4~ 1.00,
  taxdata$otv_group==5~ 1.10,
  taxdata$otv_group==6~ 1.60
)


taxdata$otv <- taxdata$otv_rate*taxdata$netprice
taxdata$kdv <- (taxdata$netprice+taxdata$otv)*0.18
taxdata<- filter(taxdata,!is.na(price)&!is.na(co2_emission))

sum(taxdata$otv*taxdata$sales,na.rm=T) /1000000000 



#correcting the mistakes about the emissions of Fiat Egea 
taxdata[taxdata$model=='fiat egea' & taxdata$fuel_type=='petrol' & taxdata$co2_emission==110,]$co2_emission <- 146
taxdata[taxdata$model=='fiat egea' & taxdata$fuel_type=='petrol' & taxdata$co2_emission==99,]$co2_emission<-132




# Tax proposal 1 -----
proposal1<- taxdata
co2_tax_rate_single <-taxdata %>% group_by(fuel_type)%>% summarise(co2_average=weighted.mean(co2_emission,sales,na.rm=T),
                                                             netprice=weighted.mean(netprice,sales,na.rm=T),
                                                             otv=weighted.mean(otv,sales,na.rm=T),
                                      otv_rate=weighted.mean(otv_rate,sales,na.rm=T),
                                                             total=sum(sales,na.rm=T))%>% 
                  mutate( tax_per_co2= otv/co2_average,
                         tax_percentage=tax_per_co2/netprice, trial= otv_rate/co2_average )


proposal1$tax_for_co2_gram <- ifelse(proposal1$fuel_type=='diesel',0.005,0.004)
proposal1$new_otv_rate<- proposal1$co2_emission*proposal1$tax_for_co2_gram
proposal1$new_otv<- proposal1$new_otv_rate*proposal1$netprice
proposal1$new_price<- (proposal1$netprice+proposal1$new_otv)*1.18
proposal1$price_change_percent <- (proposal1$new_price-proposal1$price)/proposal1$price
proposal1$new_sales<- round((1+(proposal1$price_change_percent*-1.5))*proposal1$sales)
proposal1$sales_difference <-  proposal1$new_sales-proposal1$sales
proposal1%>%   summarise(weighted.mean(co2_emission,new_sales,na.rm=T))

# Tax Proposal 2 --------

# to calculate proper tax rate in every group
proposal2 <- taxdata
co2_tax_rate_multiple <-(taxdata%>%filter(fuel_type%in% c('diesel','petrol'))%>% group_by(otv_group) %>% summarise(co2_average=weighted.mean(co2_emission,sales,na.rm=T),
                                                             netprice=weighted.mean(netprice,sales,na.rm=T),
                                                             otv=weighted.mean(otv,sales,na.rm=T),
                                                             total=sum(sales,na.rm=T))%>% 
                  mutate(share=total/sum(total), tax_per_co2= otv/co2_average,
                         tax_percentage=tax_per_co2/netprice))

co2_tax_rate_multiple$round_tax_rate<- c(0.003,0.004,0.005,0.008,0.009,0.011)


#to use CO2 penalty in main data
proposal2$tax_for_co2_gram <- co2_tax_rate_multiple$round_tax_rate[match(proposal2$otv_group,co2_tax_rate_multiple$otv_group)]
proposal2$new_otv_rate<- proposal2$co2_emission*proposal2$tax_for_co2_gram
proposal2%>% group_by(fuel_type) %>% summarise(weighted.mean(new_otv_rate,sales),weighted.mean(co2_emission,sales))

#when tax system only based on co2 emissions diesel cars have advantages. The average tax rate is 10% lower for diesel cars
#average CO2 emissions of the diesel cars are 110g/km. to reduce benefits of the diesel cars I applied extra tax for them
#diesel cars need to pay extra 0.0005 for every gram of CO2 emissons 

#new tax rate and price changes
proposal2$new_otv_rate<- proposal2$co2_emission*proposal2$tax_for_co2_gram
proposal2$new_otv_rate<- ifelse(proposal2$fuel_type=='diesel',proposal2$new_otv_rate+(0.001*proposal2$co2_emission),proposal2$new_otv_rate)
proposal2$new_otv<- proposal2$new_otv_rate*proposal2$netprice
proposal2$new_price<- (proposal2$netprice+proposal2$new_otv)*1.18
proposal2$price_change_percent <- (proposal2$new_price-proposal2$price)/proposal2$price


#Elasticity ----
segment_averages<- proposal2%>% group_by(ihs_segment)%>% summarise(segment_price_change=weighted.mean(price_change_percent,sales),total_sales=sum(sales))  

# price changes of rivals in the same segment
for(i in 1:nrow(proposal2)){
  segment_change <<- proposal2[-i,]%>% filter(ihs_segment==proposal2[i,]$ihs_segment)%>% 
    summarise(segment_price_change=weighted.mean(price_change_percent,sales))  
  proposal2[i,33]<- segment_change
  i=i+1
  }

proposal2$sales_percent_change<- (proposal2$price_change_percent*-1.66) + (proposal2$segment_price_change*0.82)
proposal2$new_sales<- round(proposal2$sales*(1+proposal2$sales_percent_change))
proposal2 %>% group_by(ihs_segment)%>% summarise(total_sales=sum(new_sales))  


## kleit used different segments. Therefore I adapted my segments first. Then average price change for kleit's segments are found 
segment_price_change <-  proposal2%>% group_by(kleit_segment)%>% summarise(old_total=sum(sales),price_change=weighted.mean(price_change_percent,sales))

#kleit's elasticiest are importet and turned to a long format
segment_elasticities <- import(file = 'r_input/kleit_2004.xlsx') 
segment_elasticities<- gather(segment_elasticities,key='cross_segment', 'elasticity',small_car:van) %>%arrange(own_segment)

# the average price changes of kleit's segments are found. 
# Then I used elasticies from kleit and found new total sales and shares of the segments
segment_elasticities$cross_price_change<- segment_price_change$price_change[match(segment_elasticities$cross_segment,segment_price_change$kleit_segment)]
segment_elasticities$effect<- segment_elasticities$elasticity*segment_elasticities$cross_price_change
segment_elasticities<-  segment_elasticities%>% na.omit()%>% group_by(own_segment)%>% summarise(total_effect= sum(effect))
segment_elasticities$old_total<- segment_price_change$old_total[match(segment_elasticities$own_segment,segment_price_change$kleit_segment)] 
segment_elasticities$new_total_by_kleit <- round((1+segment_elasticities$total_effect)*segment_elasticities$old_total)
segment_elasticities$new_share_by_kleit <- segment_elasticities$new_total_by_kleit/sum(segment_elasticities$new_total_by_kleit,na.rm=T)

# Later I compared kleit's segment shares with alper's. I wanted to use kleit's segment share. 
# Therefore I divided kleits segment shares to alper's to find adjustment rates.
current_new_sales<- proposal2 %>% group_by(kleit_segment)%>% summarise(current_new_sales=sum(new_sales,na.rm=T)) %>% 
  mutate(current_new_share=current_new_sales/sum(current_new_sales,na.rm=T))
segment_elasticities$current_new_sales<- current_new_sales$current_new_sales[match(segment_elasticities$own_segment, current_new_sales$kleit_segment)]
segment_elasticities$current_new_share<- current_new_sales$current_new_share[match(segment_elasticities$own_segment, current_new_sales$kleit_segment)]
segment_elasticities$segment_adjusting<- segment_elasticities$new_share_by_kleit/ segment_elasticities$current_new_share

# I multiplied car model sales with their segment adjustment ratios to have kleit's segment share in the end
proposal2$segment_adjusting <- segment_elasticities$segment_adjusting[match(proposal2$kleit_segment,segment_elasticities$own_segment)]
proposal2$new_sales_segment_adjusted <- round(proposal2$new_sales*proposal2$segment_adjusting)



proposal2%>%  na.omit()%>% summarise(weighted.mean(co2_emission,new_sales_segment_adjusted,na.rm=T))
proposal2%>%na.omit()%>%  group_by(fuel_type) %>% summarise(weighted.mean(price_change_percent,sales,na.rm=T),weighted.mean(price_change_percent,new_sales_segment_adjusted,na.rm=T))



# asagidaki farkliligin nedenini bulamadin bir turlu, cok buyuk sicislar soz konusu olabilir
sum(proposal2$new_otv*proposal2$new_sales_segment_adjusted,na.rm=T)/1000000000
sum(proposal2$otv*proposal2$sales,na.rm = T)/1000000000


#graphs for tax paper----
library(forecast)
library(icct)
library(ggrepel)
library(ggfortify)
# Figure 1 ----
eu_regulations<- data_frame(year=c(2015,2021,2025,2030), limits=c(130,95,81,59), labels=c('EU 2015 target: \n130 g/km','EU 2021 target: \n95 g/km', 'EU 2025 target: \n81 g/km', 'EU 2030 target: \n59 g/km')) 

tr_co2<- odd_pc%>% group_by(year) %>% summarise(co2_average=weighted.mean(co2_emission,sales,na.rm=T)) %>% mutate(country='turkey')
eu_co2<-tr_co2
eu_co2$co2_average<- c(159.0,	154.3,	147.5,	142.6,	137.7,	133.4,	126.8,	123.5,	119.6,	118.1, 119 )  # ugly but fast soltion
eu_co2$country<-'EU'
eu_future<-  data.frame(co2_average=c(119,113,107,101,95,91.5,88,84.5,81,76.6,72.2,67.8,63.4,59),year=2017:2030) # even uglier solutuon
tr_forecast<-  forecast(auto.arima(ts(tr_co2$co2_average,start = 2007)),h=13)

autoplot(tr_forecast,ts.colour = ICCTred, predict.linetype = 'dashed',
         predict.colour = ICCTred, size=1.2, conf.int.fill= ICCTred,conf.int.alpha=0.1 )+
geom_line(data = eu_co2,aes(year,co2_average),color=ICCTblue,size=1.2, inherit.aes = F)+
  geom_line(data = eu_future,aes(year,co2_average),color=ICCTblue,size=0.8,linetype='dotted', inherit.aes = F)+
  geom_point(data=eu_regulations,aes(x=year,y=limits), shape=21,size=1, color=ICCTblue,stroke=1)+
  geom_text_repel(data=eu_regulations, aes(x=year,y=limits,label =labels),direction = 'x',color=ICCTblue,
                   size=2.8,force = 1, nudge_x=c(+0.25,-0.25,-0.25,-0.25), fontface='bold')+
  geom_dl(data=tr_forecast,aes(x=2030,y=86,label= 'Forecasted \nAverage for Turkey'),color=ICCTred,
          method = list(dl.combine('last.bumpup'), cex = 0.7,fontface='bold'))+
  scale_color_manual(values = c(ICCTblue,ICCTred))+
  theme_pvs_helvetica + 
  scale_y_continuous(limits = c(55,160))+
  scale_x_continuous(limits = c(2007,2035), expand=c(0,0), breaks = seq(2010,2030,by=5))+
  labs(y=expression(bold('Average ' *CO[2]* ' emission values (g/km, NEDC)')))+
  theme(legend.position = 'top',
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.text.y = element_text(size = 10,face = 'bold'), 
        axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm"))+
  annotate("text", x=2008, y=159, label= "EU",size=3.5,color=ICCTblue,fontface='bold')+
  annotate("text", x=2008, y=143, label= "Turkey",size=3.5,color=ICCTred,fontface='bold')

ggsave(filename = '~/Desktop/co2 forecast.pdf', width = 9,height = 5.3)


proposal2%>% filter(!is.na(new_sales_segment_adjusted))%>% summarise(weighted.mean(co2_emission,new_sales_segment_adjusted))







# Figure 2 ----
scale_fill_fuel<- c(Gasoline=ICCTblue,Diesel=ICCTbrown,'Hybrid/Petrol'=ICCTgreen)


graph_data <- taxdata%>%group_by(make,model,fuel_type,otv_rate,production)%>% 
  summarise(co2_emission=weighted.mean(co2_emission,sales,na.rm=T),price=weighted.mean(price,sales,na.rm=T), sales=sum(sales,na.rm=T))
#there were two renault clio 
graph_data[graph_data$model=='renault clio',] <-  graph_data%>% filter(model=='renault clio') %>% group_by(make,model,fuel_type)%>% summarise(otv_rate=0.45,production='yerli', co2_emission=weighted.mean(co2_emission,sales),price=weighted.mean(price,sales),sales=sum(sales) )
graph_data[graph_data$model=='renault clio',] <- distinct(graph_data[graph_data$model=='renault clio',]) 
graph_data$fuel_type[graph_data$fuel_type=='petrol']<- 'gasoline'
graph_data$fuel_type<- toTitleCase(graph_data$fuel_type)
graph_data<- graph_data[!duplicated(graph_data), ]

ggplot(graph_data %>%  filter(sales>7500),
       aes(co2_emission,otv_rate, color=fuel_type, size=sales))+ 
  geom_point()+
  theme_pvs_helvetica+
  scale_color_manual(values=scale_fill_fuel)+
  scale_fill_manual(values = scale_fill_fuel)+
  theme( legend.title = element_blank(),
    legend.text = element_text(size = 10),
         plot.title = element_text(size = 10),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title  = element_text(size = 10,face = 'bold'),
         axis.text.x = element_text(size = 8.5,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
         axis.text.y = element_text(size = 9,face = 'bold'))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.45,0.5,0.6), limits = c(0.42,0.63))+
  scale_x_continuous(limits = c(85,160))+
  labs(x=expression(bold('Average ' *CO[2]* ' emission values (g/km)')),y='ÖTV rates')+
  geom_text_repel(data=graph_data%>% filter(sales>7500), aes(label =toTitleCase(model),color=fuel_type),
                   nudge_y = c(+0.007,-0.015),
                   direction = 'x',
                   vjust=0,
                   size=2.85,
                  fontface='bold'
                   )


# figure 4 ----

scale_color_country<- c( Netherlands=ICCTyellow,Norway=ICCTbrown,France=ICCTblue,'United Kingdom'= ICCTpurple, Germany=ICCTgreen,Turkey=ICCTred)
country_co2_averages<- import('r_input/countries co2 averages.xlsx') %>% filter(Country!='Norway')
country_co2_averages%>% ggplot(aes(year,co2_average,color=Country,linetype=Country=='Turkey',size=Country=='Turkey'))+
  geom_line() + 
  scale_linetype_manual(values =c('dotted','solid'))+
  theme_pvs_helvetica + labs(y=expression(bold('Average ' *CO[2]* ' emission values (g/km)')))+
  scale_color_manual(values = scale_color_country)+
  scale_size_manual(values = c(0.6,1.2)) +
  theme(legend.position ='none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
        axis.text.y = element_text(size = 10,face = 'bold'), 
        axis.title = element_text(size = 10,face = 'bold'),
        plot.caption = element_text(size = 8,face='bold'),
        plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm")) + 
  scale_x_continuous(limits=c(2007,2019.8),breaks = seq(2007, 2017, by =1),expand = c(0,0))+
  scale_y_continuous( limits = c(100,180))+ 
  geom_dl( aes(label =toTitleCase(Country), colour=Country), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))

# figure 5 ----
scale_color_taxgroups<- c('Tax Group 3'=ICCTblue,'Tax Group 2'='#66afbe','Tax Group 1'='#99c9d4','Tax Group 4'=ICCTorange,'Tax Group 5'=ICCTred,'Tax Group 6'=ICCTbrown)
current_system <- data.frame(step=rep('Current system',200), co2_level=1:200)%>% mutate('Tax Group 1'=0.45,'Tax Group 2'=0.50,
                                'Tax Group 3'=0.60,'Tax Group 4'=1.00,'Tax Group 5'=1.10,'Tax Group 6'=1.60)
step1<- data.frame(step=rep('A Scenario',200), co2_level=1:200)%>% mutate('Tax Group 1'=co2_level*co2_tax_rate_multiple$round_tax_rate[1],
'Tax Group 2'=co2_level*co2_tax_rate_multiple$round_tax_rate[2],
'Tax Group 3'=co2_level*co2_tax_rate_multiple$round_tax_rate[3],
'Tax Group 4'=co2_level*co2_tax_rate_multiple$round_tax_rate[4],
'Tax Group 5'=co2_level*co2_tax_rate_multiple$round_tax_rate[5],
'Tax Group 6'=co2_level*co2_tax_rate_multiple$round_tax_rate[6])

step2<- step1
step2<- step2 %>% mutate(step='Second step', `Tax Group 1`=`Tax Group 2`, `Tax Group 2`=NA,`Tax Group 3`=NA, `Tax Group 5`=NA)
final_version<- step1
final_version<-final_version %>% mutate(step='Final version',`Tax Group 1`=`Tax Group 3`*1.2, `Tax Group 3`=NA, `Tax Group 2`=NA, `Tax Group 4`=NA,`Tax Group 5`=NA,`Tax Group 6`=NA)
steps<- do.call("rbind", list(current_system, step1,step2,final_version))%>% gather(tax_group,tax_level,`Tax Group 1`:`Tax Group 6` ) 
steps$tax_group <-factor(steps$tax_group, levels = rev(unique(steps$tax_group))) 

steps%>%filter(step%in% c('Current system','A Scenario'))%>% ggplot(aes(co2_level,tax_level,color=tax_group))+
  geom_line(size=1.5)+
  facet_wrap(~step, nrow = 1)+
  theme_pvs_helvetica+
  scale_color_manual(values=scale_color_taxgroups)+
  labs(y = 'ÖTV rate',x=expression(bold('' *CO[2]* ' emission level (g/km)'))) +
  theme( legend.text=element_text(size=8.5),
        legend.title=element_blank(),
        legend.key.height = unit(2,"line"),
        plot.caption = element_text(size=8),
        axis.text = element_text(size = 8.5,face = 'bold'), 
        axis.title =element_text(size=10),
        plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm'))+
  scale_y_continuous(labels=percent_format(accuracy = 1))+
  scale_x_continuous(limits = c(0,200))+
  theme(strip.background = element_rect(fill=ICCTgray),
        strip.text = element_text(size=11, colour="white",face='bold'))
  



# figure 6 ----

proposal2_result <- proposal2%>%group_by(make,model,fuel_type,otv_rate,fuel_type)%>% 
  summarise(new_otv_rate=weighted.mean(new_otv_rate,sales,na.rm=T),co2_emission=weighted.mean(co2_emission,sales,na.rm=T),
            price=weighted.mean(price,sales,na.rm=T), sales=sum(sales,na.rm=T),
            ) %>%
  filter(sales>7500| (fuel_type=='hybrid/petrol'&sales>2000))%>%filter(model!='renault clio' | otv_rate!=0.5)
proposal2_result<-  proposal2_result%>% mutate(y_position=ifelse(new_otv_rate-otv_rate>0,new_otv_rate-0.015,new_otv_rate+0.011))

proposal2_result$tax_difference <- proposal2_result$new_otv_rate - proposal2_result$otv_rate
proposal2_result$fuel_type<-toTitleCase(proposal2_result$fuel_type)
proposal2_result$fuel_type[proposal2_result$fuel_type=='Petrol'] <- 'Gasoline'

ggplot(proposal2_result)+ 
  geom_point(aes(co2_emission,otv_rate,  size=sales, color=fuel_type),shape=21)+
  geom_point(aes(co2_emission,new_otv_rate,color=fuel_type))+
  theme_pvs_helvetica+
  scale_color_manual(values = scale_fill_fuel)+
  scale_fill_manual(values =scale_fill_fuel)+
  labs(x=expression(bold('Average ' *CO[2]* ' emission values (g/km)')),y='ÖTV rates')+
  theme( legend.text = element_text(size = 10),
         legend.title = element_blank(),
         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
         plot.caption =element_text(size = 8,face='bold'),
         axis.title  = element_text(size = 10,face = 'bold'),
         axis.text = element_text(size = 9,face = 'bold'))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),  limits = c(0.2,0.7))+
  scale_x_continuous(limits = c(80,160))+
  geom_segment(aes(x=co2_emission,xend=co2_emission, y=otv_rate,  yend=y_position, color=fuel_type),
               size=0.25,arrow = arrow(length = unit(0.25, "cm"),type = 'closed'))+
  geom_label_repel(data=proposal2_result,aes(co2_emission,new_otv_rate,label =toTitleCase(model),fill=fuel_type),
                   color='white',fontface='bold',
                   size=2.7,
                   force = 1,
                   box.padding   = 0, 
                   point.padding = 0,show.legend = F)
ggsave(filename = '~/Desktop/new otv system.pdf', width = 9.08,height = 5.82)





# by bars ----
proposal2_result$model_fuel <- paste(proposal2_result$model,proposal2_result$fuel_type)
proposal2_result$model_fuel<- toTitleCase(proposal2_result$model_fuel)
proposal2_result<-proposal2_result%>%  arrange((tax_difference))
proposal2_result$model_fuel<- factor(proposal2_result$model_fuel,levels = unique(proposal2_result$model_fuel))

ggplot(proposal2_result%>% filter(sales>1000), aes(model_fuel, tax_difference,fill=tax_difference)) + 
  geom_bar(stat = 'identity', position = 'identity') +
  theme_pvs_helvetica +
  labs(x="")+ ggtitle('Price changes')+guides(col = guide_legend(nrow = 2, byrow = TRUE))+
  coord_flip()+ 
  theme( plot.title = element_text( face = 'bold',size = 12),
         legend.position = 'none',
         axis.title.x = element_blank(), 
         axis.text.x = element_text(size = 10,face='bold',margin=margin(5,0,10,0)),
         axis.text.y = element_text(size = 10,face='bold'), 
         axis.title.y = element_text(size = 10,face = 'bold'),
         plot.caption = element_text(size = 8,face='bold'), 
         plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm"))+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_continuous()

  geom_text(data = filter(brand, share>0.01), aes(label= make),position = position_stack(vjust = 0.45),color='white',size=3.2,fontface='bold')


  # local cars tax groups ( not used)
  #ggplot(data =taxdata %>% group_by(production,otv_group,fuel_type) %>%
  #         summarise(total_registration=sum(sales,na.rm=T))%>% mutate(share=total_registration/sum(total_registration)),
  #      aes(x=1, y=share,fill=fuel_type))+
  #geom_bar(position='fill', stat="identity") + 
  #  scale_fill_manual(values = scale_fill_fuel)+
  #  facet_grid(otv_group~production) + 
  # coord_polar(theta = "y") +
  # scale_y_continuous(breaks = NULL) +
  # scale_x_continuous(name = element_blank(), breaks = NULL)+
  #  theme( legend.text = element_text(size = 10),
  #        plot.title = element_text(size = 10),
  #         plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
  #        plot.caption =element_text(size = 8,face='bold'),
  #        axis.title  = element_text(size = 10,face = 'bold'),
  #         axis.text.x = element_text(size = 8.5,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
  #         axis.text.y = element_text(size = 9,face = 'bold'))


  #effect of diesel surcharge
  proposal2%>%filter(!is.na(new_sales_segment_adjusted))%>% group_by( fuel_type)%>% summarise( new_tax=weighted.mean( new_otv_rate,new_sales_segment_adjusted), new_co2=weighted.mean(co2_emission,new_sales_segment_adjusted))
  
  
  
  
# TURKCE -----
# grafik 1  ----
  
  eu_regulations<- data_frame(year=c(2015,2021,2025,2030), limits=c(130,95,81,59), labels=c('AB 2015 hedefi: \n130 g/km','AB 2021 hedefi: \n95 g/km', 'AB 2025 hedefi: \n81 g/km', 'AB 2030 hedefi: \n59 g/km')) 
  
  tr_co2<- odd_pc%>% group_by(year) %>% summarise(co2_average=weighted.mean(co2_emission,sales,na.rm=T)) %>% mutate(country='turkey')
  eu_co2<-tr_co2
  eu_co2$co2_average<- c(159.0,	154.3,	147.5,	142.6,	137.7,	133.4,	126.8,	123.5,	119.6,	118.1, 119 )  # ugly but fast soltion
  eu_co2$country<-'AB'
  eu_future<-  data.frame(co2_average=c(119,113,107,101,95,91.5,88,84.5,81,76.6,72.2,67.8,63.4,59),year=2017:2030) # even uglier solutuon
  tr_forecast<-  forecast(auto.arima(ts(tr_co2$co2_average,start = 2007)),h=13)
  
  autoplot(tr_forecast,ts.colour = ICCTred, predict.linetype = 'dashed',
           predict.colour = ICCTred, size=1.2, conf.int.fill= ICCTred,conf.int.alpha=0.1 )+
    geom_line(data = eu_co2,aes(year,co2_average),color=ICCTblue,size=1.2, inherit.aes = F)+
    geom_line(data = eu_future,aes(year,co2_average),color=ICCTblue,size=0.8,linetype='dotted', inherit.aes = F)+
    geom_point(data=eu_regulations,aes(x=year,y=limits), shape=21,size=1, color=ICCTblue,stroke=1)+
    geom_text_repel(data=eu_regulations, aes(x=year,y=limits,label =labels),direction = 'x',color=ICCTblue,
                    size=2.8,force = 1, nudge_x=c(+0.25,-0.25,-0.25,-0.25), fontface='bold')+
    geom_dl(data=tr_forecast,aes(x=2030,y=86,label= 'Türkiye icin\n tahmini ortalama'),color=ICCTred,
            method = list(dl.combine('last.bumpup'), cex = 0.7,fontface='bold'))+
    scale_color_manual(values = c(ICCTblue,ICCTred))+
    theme_pvs_helvetica + 
    scale_y_continuous(limits = c(55,160))+
    scale_x_continuous(limits = c(2007,2035), expand=c(0,0), breaks = seq(2010,2030,by=5))+
    labs(y=expression(bold('Ortalama ' *CO[2]* ' emisyonu (g/km, NEDC)')))+
    theme(legend.position = 'top',
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 10,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
          axis.text.y = element_text(size = 10,face = 'bold'), 
          axis.title = element_text(size = 10,face = 'bold'),
          plot.caption = element_text(size = 8,face='bold'),
          plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm"))+
    annotate("text", x=2008, y=159, label= "AB",size=3.5,color=ICCTblue,fontface='bold')+
    annotate("text", x=2008, y=143, label= "Türkiye",size=3.5,color=ICCTred,fontface='bold')
  
  ggsave(filename = '~/Desktop/co2 tahmini.pdf', width = 8,height = 5.3)
  
  
  proposal2%>% filter(!is.na(new_sales_segment_adjusted))%>% summarise(weighted.mean(co2_emission,new_sales_segment_adjusted))
  
  
  
  
  
  
  
  # grafik 2 ----
  scale_fill_yakit<- c(Benzinli=ICCTblue,Dizel=ICCTbrown,'Hibrit'=ICCTgreen)
  
  
  graph_data_tr <- taxdata%>%group_by(make,model,fuel_type,otv_rate,production)%>% 
    summarise(co2_emission=weighted.mean(co2_emission,sales,na.rm=T),price=weighted.mean(price,sales,na.rm=T), sales=sum(sales,na.rm=T))
  #there were two renault clio 
  graph_data_tr[graph_data_tr$model=='renault clio',] <-  graph_data_tr%>% filter(model=='renault clio') %>% group_by(make,model,fuel_type)%>% summarise(otv_rate=0.45,production='yerli', co2_emission=weighted.mean(co2_emission,sales),price=weighted.mean(price,sales),sales=sum(sales) )
  graph_data_tr[graph_data_tr$model=='renault clio',] <- distinct(graph_data_tr[graph_data_tr$model=='renault clio',]) 
  graph_data_tr$fuel_type[graph_data_tr$fuel_type=='petrol']<- 'benzinli'
  graph_data_tr$fuel_type[graph_data_tr$fuel_type=='diesel']<- 'dizel'
  graph_data_tr$fuel_type<- toTitleCase(graph_data_tr$fuel_type)
  graph_data_tr<- graph_data_tr[!duplicated(graph_data_tr), ]
  
  ggplot(graph_data_tr %>%  filter(sales>7500),
         aes(co2_emission,otv_rate, color=fuel_type, size=sales))+ 
    geom_point()+
    theme_pvs_helvetica+
    scale_color_manual(values=scale_fill_yakit)+
    scale_fill_manual(values = scale_fill_yakit)+
    theme( legend.title = element_blank(),
           legend.text = element_text(size = 10),
           plot.title = element_text(size = 10),
           plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
           plot.caption =element_text(size = 8,face='bold'),
           axis.title  = element_text(size = 10,face = 'bold'),
           axis.text.x = element_text(size = 8.5,face = 'bold',margin = unit(c(0.2,0,0.4,0.5), "cm")),
           axis.text.y = element_text(size = 9,face = 'bold'))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(0.45,0.5,0.6), limits = c(0.42,0.63))+
    scale_x_continuous(limits = c(85,160))+
    labs(x=expression(bold('Ortalama ' *CO[2]* ' emisyonu (g/km)')),y='ÖTV yüzdesi')+
    geom_text_repel(data=graph_data_tr%>% filter(sales>7500), aes(label =toTitleCase(model),color=fuel_type),
                    nudge_y = c(+0.007,-0.015),
                    direction = 'x',
                    vjust=0,
                    size=2.85,
                    fontface='bold'
    )
  ggsave(filename = '~/Desktop/grafik2.pdf', width = 8,height = 5.3)
  
  
  # grafik 4 ----
  
  scale_color_country<- c( Netherlands=ICCTyellow,Norway=ICCTbrown,France=ICCTblue,'United Kingdom'= ICCTpurple, Germany=ICCTgreen,Turkey=ICCTred)
  country_co2_averages<- import('r_input/countries co2 averages.xlsx') %>% filter(Country!='Norway')
  country_co2_averages$tr_country <- case_when(
    country_co2_averages$Country=='Netherlands'~'Hollanda',
    country_co2_averages$Country=='United Kingdom'~ 'Birlesik Krallik',
    country_co2_averages$Country=='Turkey'~'Türkiye',
    country_co2_averages$Country=='France'~'Fransa',
    country_co2_averages$Country=='Germany'~'Almanya',
  )
  country_co2_averages%>% ggplot(aes(year,co2_average,color=Country,linetype=Country=='Turkey',size=Country=='Turkey'))+
    geom_line() + 
    scale_linetype_manual(values =c('dotted','solid'))+
    theme_pvs_helvetica + labs(y=expression(bold('Ortalama ' *CO[2]* ' emisyonu (g/km)')))+
    scale_color_manual(values = scale_color_country)+
    scale_size_manual(values = c(0.6,1.2)) +
    theme(legend.position ='none',
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 10,face = 'bold',margin = unit(c(0.3,0.2,0.2,0.2), "cm")),
          axis.text.y = element_text(size = 10,face = 'bold'), 
          axis.title = element_text(size = 10,face = 'bold'),
          plot.caption = element_text(size = 8,face='bold'),
          plot.margin = unit(c(0.3,0.2,0.2,0.2), "cm")) + 
    scale_x_continuous(limits=c(2007,2019.8),breaks = seq(2007, 2017, by =1),expand = c(0,0))+
    scale_y_continuous( limits = c(100,180))+ 
    geom_dl( aes(label =toTitleCase(tr_country), colour=Country), method = list(dl.combine('last.bumpup'), cex = 0.8,fontface='bold'))
  ggsave(filename = '~/Desktop/grafik4.pdf', width = 8,height = 5.3)
  
  
  # grafik 5 ----
  scale_color_taxgroups<- c('Vergi Grubu 3'=ICCTblue,'Vergi Grubu 2'='#66afbe','Vergi Grubu 1'='#99c9d4','Vergi Grubu 4'=ICCTorange,'Vergi Grubu 5'=ICCTred,'Vergi Grubu 6'=ICCTbrown)
  current_system <- data.frame(step=rep('Mevcut sistem',200), co2_level=1:200)%>% mutate('Vergi Grubu 1'=0.45,'Vergi Grubu 2'=0.50,
                                                                                          'Vergi Grubu 3'=0.60,'Vergi Grubu 4'=1.00,'Vergi Grubu 5'=1.10,'Vergi Grubu 6'=1.60)
  step1<- data.frame(step=rep('Alternatif Senaryo',200), co2_level=1:200)%>% mutate('Vergi Grubu 1'=co2_level*co2_tax_rate_multiple$round_tax_rate[1],
                                                                            'Vergi Grubu 2'=co2_level*co2_tax_rate_multiple$round_tax_rate[2],
                                                                            'Vergi Grubu 3'=co2_level*co2_tax_rate_multiple$round_tax_rate[3],
                                                                            'Vergi Grubu 4'=co2_level*co2_tax_rate_multiple$round_tax_rate[4],
                                                                            'Vergi Grubu 5'=co2_level*co2_tax_rate_multiple$round_tax_rate[5],
                                                                            'Vergi Grubu 6'=co2_level*co2_tax_rate_multiple$round_tax_rate[6])
  
  step2<- step1
  step2<- step2 %>% mutate(step='Second step', `Vergi Grubu 1`=`Vergi Grubu 2`, `Vergi Grubu 2`=NA,`Vergi Grubu 3`=NA, `Vergi Grubu 5`=NA)
  final_version<- step1
  final_version<-final_version %>% mutate(step='Final version',`Vergi Grubu 1`=`Vergi Grubu 3`*1.2, `Vergi Grubu 3`=NA, `Vergi Grubu 2`=NA, `Vergi Grubu 4`=NA,`Vergi Grubu 5`=NA,`Vergi Grubu 6`=NA)
  steps<- do.call("rbind", list(current_system, step1,step2,final_version))%>% gather(tax_group,tax_level,`Vergi Grubu 1`:`Vergi Grubu 6` ) 
  steps$tax_group <-factor(steps$tax_group, levels = rev(unique(steps$tax_group))) 
  
  steps%>%filter(step%in% c('Mevcut sistem','Alternatif Senaryo'))%>% ggplot(aes(co2_level,tax_level,color=tax_group))+
    geom_line(size=1.5)+
    facet_wrap(~step, nrow = 1)+
    theme_pvs_helvetica+
    scale_color_manual(values=scale_color_taxgroups)+
    labs(y = 'ÖTV yüzdesi',x=expression(bold('' *CO[2]* ' emisyon seviyesi (g/km)'))) +
    theme( legend.text=element_text(size=8.5),
           legend.title=element_blank(),
           legend.key.height = unit(2,"line"),
           plot.caption = element_text(size=8),
           axis.text = element_text(size = 8.5,face = 'bold'), 
           axis.title =element_text(size=10),
           plot.margin = unit(c(0.2,0.2,0.2,0.2),'cm'))+
    scale_y_continuous(labels=percent_format(accuracy = 1))+
    scale_x_continuous(limits = c(0,200))+
    theme(strip.background = element_rect(fill=ICCTgray),
          strip.text = element_text(size=11, colour="white",face='bold'))
  ggsave(filename = '~/Desktop/grafik5.pdf', width = 8,height = 5.3)
  
  
  
  
  # grafik 6 ----
  
  proposal2_result <- proposal2%>%group_by(make,model,fuel_type,otv_rate,fuel_type)%>% 
    summarise(new_otv_rate=weighted.mean(new_otv_rate,sales,na.rm=T),co2_emission=weighted.mean(co2_emission,sales,na.rm=T),
              price=weighted.mean(price,sales,na.rm=T), sales=sum(sales,na.rm=T)) %>%
    filter(sales>7500| (fuel_type=='hybrid/petrol'&sales>2000))%>%filter(model!='renault clio' | otv_rate!=0.5)
  proposal2_result<-  proposal2_result%>% mutate(y_position=ifelse(new_otv_rate-otv_rate>0,new_otv_rate-0.015,new_otv_rate+0.011))
  
  proposal2_result$tax_difference <- proposal2_result$new_otv_rate - proposal2_result$otv_rate
  proposal2_result$fuel_type<-toTitleCase(proposal2_result$fuel_type)
  proposal2_result$fuel_type[proposal2_result$fuel_type=='Petrol'] <- 'Gasoline'
  proposal2_result$tr_fuel <- case_when(
    proposal2_result$fuel_type=='Gasoline'~ 'Benzinli',
    proposal2_result$fuel_type=='Diesel'~'Dizel',
    proposal2_result$fuel_type=='Hybrid/Petrol'~ 'Hibrit')
    
    
  ggplot(proposal2_result)+ 
    geom_point(aes(co2_emission,otv_rate,  size=sales, color=tr_fuel),shape=21)+
    geom_point(aes(co2_emission,new_otv_rate,color=tr_fuel))+
    theme_pvs_helvetica+
    scale_color_manual(values = scale_fill_yakit)+
    scale_fill_manual(values =scale_fill_yakit)+
    labs(x=expression(bold('Ortalama ' *CO[2]* ' emisyonu (g/km)')),y='ÖTV yüzdesi')+
    theme( legend.text = element_text(size = 10),
           legend.title = element_blank(),
           plot.margin= unit(c(0.2,0.2,0.2,0.2), "cm"),
           plot.caption =element_text(size = 8,face='bold'),
           axis.title  = element_text(size = 10,face = 'bold'),
           axis.text = element_text(size = 9,face = 'bold'))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),  limits = c(0.2,0.7))+
    scale_x_continuous(limits = c(80,160))+
    geom_segment(aes(x=co2_emission,xend=co2_emission, y=otv_rate,  yend=y_position, color=tr_fuel),
                 size=0.25,arrow = arrow(length = unit(0.25, "cm"),type = 'closed'))+
    geom_label_repel(data=proposal2_result,aes(co2_emission,new_otv_rate,label =toTitleCase(model),fill=tr_fuel),
                     color='white',fontface='bold',
                     size=2.7,
                     force = 1,
                     box.padding   = 0, 
                     point.padding = 0,show.legend = F)
  ggsave(filename = '~/Desktop/grafik6.pdf', width = 9.08,height = 5.82)
  
  
  
  