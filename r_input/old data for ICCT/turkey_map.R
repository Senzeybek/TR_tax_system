#turkey map------
tur <- readRDS("R_input/gadm36_TUR_1_sp.rds")
plot(tur)
tur_for <- fortify(tur)
province<-import("R_input/province_car.xls")  %>% mutate(share=total/sum(total))
id_and_cities<- data_frame(id = rownames(tur@data),
                           il = tur@data$NAME_1) %>% 
  left_join(province, by = "il")
final_map <- left_join(tur_for, id_and_cities, by = "id")
head(final_map)

ggplot(final_map) +
  geom_polygon( aes(x = long, y = lat, group = group, fill = share),
                color = ICCTgray) +
  coord_map() +
  theme_void() + scale_fill_continuous(low = b1,high = b5,labels = scales::percent)+
  labs(title = "Total Passenger Cars on the Road (2018)",
       subtitle = paste0("Total: ", sum(province$total)),
       caption = "Source: Turkstat") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),legend.title=element_blank())