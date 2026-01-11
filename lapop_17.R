library(haven)
library(tidyverse)
library(data.table)
library(sf)
library(broom)
library(conflicted)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::first)
conflicts_prefer(lubridate::year)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::wday)

state_city <- read_dta("Brazil_2017.dta") %>% 
  select(state_id = prov, 
         city_id = municipio) %>% 
  filter(!is.na(state_id) & !is.na(city_id)) %>% 
  distinct()

state_id <- tibble(state_id = attr(state_city$state_id, "labels"),
                   state_name = names(state_id))

city_id <- tibble(city_id = attr(state_city$city_id, "labels"),
                  city_name = names(city_id))

city_info <- state_city %>% 
  left_join(state_id,by=c('state_id')) %>% 
  left_join(city_id,by=c('city_id')) %>% 
  mutate(city_name = tolower(city_name),
         state_name = tolower(state_name),
         state_name = stringi::stri_trans_general(state_name, "Latin-ASCII"),
         city_name = stringi::stri_trans_general(city_name, "Latin-ASCII"))

mun <- geobr::read_municipality(code_muni="all", year=2019) %>%
  st_transform(4326) %>% 
  mutate(name_muni = tolower(name_muni),
         name_state = tolower(name_state),
         name_state = stringi::stri_trans_general(name_state, "Latin-ASCII"),
         name_muni = stringi::stri_trans_general(name_muni, "Latin-ASCII"))


city_geo <- city_info %>% 
  left_join(mun,by=c('state_name'='name_state',
                     'city_name'='name_muni')) %>% 
  # filter(!is.na(code_muni)) %>%
  st_as_sf() %>% 
  st_set_crs(4326)

# sf::st_write(city_geo, "bra_city_geo.gpkg", driver = "GPKG")

br_aid <- st_read('brazil_aid.gpkg') %>% 
  filter(!is.na(Completion.Year))

# st_crs(br_aid)
# st_crs(city_geo)

aid_with_city <- st_join(br_aid, city_geo, join = st_intersects)

aid_n_year <- aid_with_city %>%
  st_drop_geometry() %>% 
  select(id,end_year=Completion.Year,abbrev_state,state_name,city_name,code_muni) %>% 
  filter(!is.na(city_name)) %>% 
  group_by(code_muni,abbrev_state,state_name,city_name,end_year) %>% 
  summarise(count=n()) %>% 
  mutate(end_year = end_year+1)

aid_cu <- aid_with_city %>%
  st_drop_geometry() %>% 
  select(id,end_year=Completion.Year,abbrev_state,state_name,city_name,code_muni) %>% 
  filter(!is.na(city_name)) %>% 
  group_by(code_muni,abbrev_state,state_name,city_name,end_year) %>% 
  summarise(count=n()) %>% 
  mutate(cu_count = cumsum(count)) %>% 
  arrange(city_name,end_year) %>% 
  filter(end_year<2017) %>%
  group_by(code_muni,abbrev_state,state_name,city_name) %>% 
  summarise(aid_n = max(cu_count),
            first_aid = min(end_year)) %>% 
  mutate(year=2017)


bra_lapop_17 <- read_dta('Brazil_2017.dta') %>%
  filter(!is.na(municipio)
         & !is.na(mil10a)) %>%
  select(id = uniq_id, 
         gender = q1, 
         age = q2, 
         edu = ed, 
         country_id =pais, 
         nationality, 
         # area = estratopri,
         # city_level = estratosec,
         # area_city = strata,
         city_id = municipio, 
         # sample_id = upm,
         urban_rural = ur, 
         # living_cluster = cluster,
         political_stance = l1,
         presidential_approval_rating = m1,
         # env_attitude = env2b,
         china_attitude = mil10a, 
         # russia_attitude=mil10b,
         america_attitude=mil10e,
         pe_fina = idio2,#受访者个人经济状况与12个月前相比的变化。
         job = ocup4a,#职业类型
         ing4,#受访者对民主制度的看法(民主制度倾向性)
         # cp13,#受访者参与政党或政治运动的频率
         fam_fina = q10new,#家庭收入
         etid,#人种
         news_read = gi0# gi0n,通过公共媒体关注新闻频率
         # smedia3n,#社交媒体是否关注政治话题
         # smedia3b,#社交媒体关注政治话题频率
         ) %>% 
  mutate(year = 2017) %>% 
  left_join(city_info,by=c('city_id')) %>% 
  mutate(state_name=tolower(state_name),
         city_name=tolower(city_name)) %>% 
  left_join(aid_cu,by=c('state_name','city_name','year')) %>% 
  mutate(aid_n = coalesce(aid_n, 0)) %>% 
  mutate(if_aid = ifelse(aid_n == 0, 0, 1)) %>%
  mutate(aid_time = ifelse(!is.na(first_aid), year - first_aid, NA))

rm(list = setdiff(ls(), c("bra_lapop_17","bra_lapop_19","bra_lapop_21","bra_lapop_23")))




