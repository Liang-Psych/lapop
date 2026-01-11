library(haven)
library(tidyverse)
library(data.table)

library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::first)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::wday)

# select(idnum, q1tc_r, q2, edre, pais, nationality, estratopri, 
#        estratosec, strata, prov, municipio, upm, ur, cluster, 
#        l1n, m1, env2b, mil10a, year)

lapop_limit <- read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta") %>% 
  filter(pais==17) %>% 
  # select(idnum, q1tc_r, q2, edre, pais, nationality, estratopri, 
  #        estratosec, strata, prov, municipio, upm, ur, cluster, 
  #        l1n, m1, env2b, mil10a, year) %>% 
  # filter(!is.na(mil10a))
  #& year %in% c('2012','2014','2016','2018','2021','2023'))
  
lapop_limit <- read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta") %>% 
  filter(pais %in% c(8,9,10,11,12,13,14,15,17,27)) %>%
  # select(idnum, q1tc_r, q2, edre, pais, nationality, estratopri, 
  #        estratosec, strata, prov, municipio, upm, ur, cluster, 
  #        l1n, m1, env2b, mil10a, year) %>% 
  filter(!is.na(mil10a))
# & year %in% c('2012','2014','2016','2018','2021','2023'))

lapop_sav = read_sav('Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREEen.sav') %>% 
  filter(pais == 17)


lapop_limit %>% 
  # filter(pais == 17) %>% 
  select(pais,year,mil10a) %>% 
  distinct() %>% 
  print(n=100)

lapop_agen <- read_dta('ARG_2023_LAPOP_AmericasBarometer_v1.0_w.dta')

lapop_agen %>% select(pais,mil10a)

read_dta("BRA_2023_LAPOP_AmericasBarometer_v1.0_w.dta") %>% 
  write_csv('lapop_bri.csv')

lapop_bri %>% colnames()

lapop_ag <- read_dta("Argentina_2023.dta") %>% 
  select(idnum, q2, pais, estratopri, upm, m1, mil10a) %>% 
  filter(!is.na(mil10a))


lapop_limit <- read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta") %>% 
  select(year, idnum, q2, pais, estratopri, prov, municipio,upm, m1, mil10a) %>% 
  filter(pais %in% c(8,9,10,11,12,13,14,15,17,27)
         & year %in% c('2012','2014','2016','2018','2021','2023')
         & !is.na(mil10a) 
         # & !is.na(prov) 
         # & !is.na(municipio)
  ) 

unique(lapop_limit$year)

unique(lapop_limit$pais)

aid_23 <- aid_cu %>% 
  filter(end_year<2023) %>%
  group_by(city) %>% 
  summarise(cu_count = max(cu_count),
            last_aid = max(end_year)) %>% 
  mutate(year=2023)

bra_lapop_23 <- read_dta('Brazil_2023.dta') %>% 
  # read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta") %>% 
  filter(pais == 15
         & !is.na(prov) 
         & !is.na(municipio)
         & !is.na(mil10a)) %>%
  select(idnum, q1tc_r, q2, edre, pais, nationality, estratopri,
         estratosec, strata, prov, municipio, upm, ur, cluster,
         l1n, m1, env2b, mil10a, year) %>% 
  left_join(municipio_label_df,by=c('municipio'='label')) %>% 
  mutate(codigo=tolower(codigo)) %>% 
  left_join(aid_23,by=c('codigo'='city')) %>% 
  filter(!is.na(codigo))


aid_21 <- aid_cu %>% 
  filter(end_year<2021) %>%
  group_by(city) %>% 
  summarise(cu_count=max(cu_count),
            last_aid = max(end_year)) %>% 
  mutate(year = 2021)

aid_19 <- aid_cu %>% 
  filter(end_year<2019) %>%
  group_by(city) %>% 
  summarise(cu_count=max(cu_count),
            last_aid = max(end_year)) %>% 
  mutate(year = 2019)

aid_17 <- aid_cu %>% 
  filter(end_year<2017) %>%
  group_by(city) %>% 
  summarise(cu_count=max(cu_count),
            last_aid = max(end_year)) %>% 
  mutate(year = 2017)

aid_14 <- aid_cu %>% 
  filter(end_year<2014) %>%
  group_by(city) %>% 
  summarise(cu_count=max(cu_count),
            last_aid = max(end_year)) %>% 
  mutate(year = 2014)

aid_all <- aid_cu %>% 
  filter(end_year<2012) %>%
  group_by(city) %>% 
  summarise(cu_count=max(cu_count),
            last_aid = max(end_year)) %>% 
  mutate(year = 2012) %>% 
  rbind(aid_14,aid_17,aid_19,aid_21,aid_23)

read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta") %>% 
  write('lapop_data.csv')

read_dta("Grand_Merge_2004-2023_LAPOP_AmericasBarometer_v1.0_FREE.dta") %>% 
  filter(pais == 15
         # & !is.na(prov) 
         # & !is.na(municipio)
         & !is.na(mil10a)) %>%
  write('lapop_bra.csv')


bra_lapop_23 <- read_dta('Brazil_2023.dta') %>%  
  filter(pais == 15
         # & !is.na(prov) 
         & !is.na(municipio)
         & !is.na(mil10a)) %>%
  select(id = idnum, 
         gender=q1tc_r, 
         age = q2, 
         edu = edre, 
         country_id =pais, 
         nationality, 
         # area = estratopri,
         # city_level = estratosec, 
         # area_city = strata, 
         city_id = municipio, 
         # sample_id = upm, 
         urban_rural = ur, 
         # living_cluster = cluster,
         # Political_Stance = l1n, 
         # Presidential_approval_rating = m1, 
         # env_attitude = env2b, 
         china_attitude = mil10a, 
         year) %>% 
  left_join(municipio_label_df,by=c('city_id'='label')) %>% 
  mutate(codigo=tolower(codigo)) %>% 
  left_join(aid_all,by=c('codigo'='city','year')) %>% 
  filter(!is.na(codigo))

bra_lapop_21 <- read_dta('Brazil_2021.dta') %>%  
  filter(pais == 15
         # & !is.na(prov) 
         & !is.na(municipio1t)
         & !is.na(mil10a)) %>%
  select(id = idnum, 
         gender=sexi, 
         age = q2, 
         edu = edr, 
         country_id =pais, 
         nationality, 
         # area = estratopri,
         # city_level = estratosec, 
         # area_city = strata, 
         city_id = municipio1t, 
         # sample_id = upm, 
         urban_rural = ur1new, 
         # living_cluster = cluster,
         # Political_Stance = l1n, 
         # Presidential_approval_rating = m1, 
         # env_attitude = env2b, 
         china_attitude = mil10a, 
         year) %>% 
  left_join(municipio_label_df,by=c('city_id'='label')) %>% 
  mutate(codigo=tolower(codigo)) %>% 
  left_join(aid_all,by=c('codigo'='city','year')) %>% 
  filter(!is.na(codigo))



read_dta('Brazil_2021.dta') %>% colnames()
