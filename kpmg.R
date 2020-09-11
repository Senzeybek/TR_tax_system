odd_2016 <- import("r_input/odd_kpmg_2016.xls",skip=2)  %>% mutate(year=2016)
odd_2017 <- import("r_input/odd_kpmg_2017.xls",sheet="Sheet1",skip=2) %>% mutate(year=2017) 
odd_2018 <- import("r_input/odd_kpmg_2018.xls",sheet="Sheet1",skip=2) %>% mutate(year=2018)
odd_2019 <- import("r_input/odd_kpmg_2019.xls",sheet="Sheet1",skip=2) %>% mutate(year=2019)
odd_2020 <- import("r_input/odd_kpmg_2020.xls",sheet="Sheet1",skip=2) %>% mutate(year=2020)

(colnames(odd_2018))
colnames(odd_2019) 

