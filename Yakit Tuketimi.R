# yakit tuketimi ----

# PC
data$real_world_yakit_tuketimi <- data$yakit_tuketimi*1.38

data <- data %>% mutate(
  yakit_vergisi=case_when(  data$powertrain=="Dizel" ~ dizel_litre_otv+dizel_litre_kdv,
                            data$powertrain%in% c("Benzin","Hybrid") ~ benzin_litre_otv+benzin_litre_kdv),
  toplam_yakit_tuketimi=(arac_yillik_km/100)*real_world_yakit_tuketimi
  )  


# LCV
lcv_data$real_world_yakit_tuketimi <- lcv_data$yakit_tuketimi*1.38

lcv_data <- lcv_data %>% mutate(
  yakit_vergisi=case_when(  lcv_data$powertrain=="Dizel" ~ dizel_litre_otv+dizel_litre_kdv,
                            lcv_data$powertrain%in% c("Benzin","Hybrid") ~ benzin_litre_otv+benzin_litre_kdv),
  toplam_yakit_tuketimi=(arac_yillik_km/100)*real_world_yakit_tuketimi
)  

