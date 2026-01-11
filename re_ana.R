library(tidyverse)
library(lmtest) # 用于稳健标准误
library(sandwich) # 用于稳健标准误
library(psych)
library(haven)
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


source("~/Rcode/lapop/lapop_23.R", echo=TRUE)
source("~/Rcode/lapop/lapop_21.R", echo=TRUE)
source("~/Rcode/lapop/lapop_19.R", echo=TRUE)
source("~/Rcode/lapop/lapop_17.R", echo=TRUE)
source("~/Rcode/lapop/lapop_14.R", echo=TRUE)
source("~/Rcode/lapop/lapop_12.R", echo=TRUE)

bra_lapop_12 %>% write_delim('bra_lapop_12.txt')
bra_lapop_14 %>% write_delim('bra_lapop_14.txt')
bra_lapop_17 %>% write_delim('bra_lapop_17.txt')
bra_lapop_19 %>% write_delim('bra_lapop_19.txt')
bra_lapop_21 %>% write_delim('bra_lapop_21.txt')
bra_lapop_23 %>% write_delim('bra_lapop_23.txt')

bra_lapop_12 = read_delim('bra_lapop_12.txt')
bra_lapop_14 = read_delim('bra_lapop_14.txt')
bra_lapop_17 = read_delim('bra_lapop_17.txt')
bra_lapop_19 = read_delim('bra_lapop_19.txt')
bra_lapop_21 = read_delim('bra_lapop_21.txt')
bra_lapop_23 = read_delim('bra_lapop_23.txt')


bra_lapop_12 <- bra_lapop_12 %>% mutate(china_attitude = zap_labels(china_attitude),
                                        america_attitude = zap_labels(america_attitude),
                                        id = as.numeric(id)) 
bra_lapop_14 <- bra_lapop_14 %>% mutate(china_attitude = zap_labels(china_attitude),
                                        america_attitude = zap_labels(america_attitude),
                                        id = as.numeric(id))
bra_lapop_17 <- bra_lapop_17 %>% mutate(china_attitude = zap_labels(china_attitude),
                                        america_attitude = zap_labels(america_attitude),
                                        id = as.numeric(id))
bra_lapop_19 <- bra_lapop_19 %>% mutate(china_attitude = zap_labels(china_attitude),
                                        america_attitude = zap_labels(america_attitude),
                                        id = as.numeric(id))
bra_lapop_21 <- bra_lapop_21 %>% mutate(china_attitude = zap_labels(china_attitude),
                                        america_attitude = zap_labels(america_attitude),
                                        id = as.numeric(id))
bra_lapop_23 <- bra_lapop_23 %>% mutate(china_attitude = zap_labels(china_attitude),
                                        america_attitude = zap_labels(america_attitude),
                                        id = as.numeric(id))


pib_21 <- readxl::read_excel('PIB_2010-2021.xlsx') %>% #colnames()
  filter(Ano==2021) %>% 
  select(1:8,gdp_per="Produto Interno Bruto per capita, \r\na preços correntes\r\n(R$ 1,00)") %>% 
  rename(area_id="Código da Grande Região",
         area_name="Nome da Grande Região",
         state_id="Código da Unidade da Federação",
         state_sim="Sigla da Unidade da Federação",
         state_name="Nome da Unidade da Federação",
         city_id="Código do Município",
         city_name="Nome do Município") %>% 
  mutate(dev_level = case_when(
    gdp_per >= 42893.73 ~ 1,
    gdp_per < 42893.73 ~ 0),
    city_name = tolower(city_name),
    state_name = tolower(state_name),
    state_name = stringi::stri_trans_general(state_name, "Latin-ASCII"),
    city_name = stringi::stri_trans_general(city_name, "Latin-ASCII"))


bra_lapop <- bra_lapop_23 %>% select(year,state_name,city_name,america_attitude,china_attitude,gender,age,pe_fina,fam_fina, edu, urban_rural,if_aid,aid_n,aid_time,
                                     presidential_approval_rating,ing4,etid,news_read) %>% 
  bind_rows(bra_lapop_21[,c('year','state_name','city_name','america_attitude','china_attitude','gender','age','pe_fina','fam_fina','edu', 'urban_rural','if_aid','aid_n',
                            'aid_time','presidential_approval_rating','ing4','etid','news_read')]) %>%
  bind_rows(bra_lapop_19[,c('year','state_name','city_name','america_attitude','china_attitude','gender','age','pe_fina','fam_fina','edu', 'urban_rural','if_aid','aid_n',
                            'aid_time','presidential_approval_rating','ing4','etid','news_read')]) %>% 
  bind_rows(bra_lapop_17[,c('year','state_name','city_name','america_attitude','china_attitude','gender','age','pe_fina','fam_fina', 'edu', 'urban_rural','if_aid','aid_n',
                            'aid_time','presidential_approval_rating','ing4','etid','news_read')]) %>% 
  bind_rows(bra_lapop_14[,c('year','state_name','city_name','america_attitude','china_attitude','gender','age','pe_fina','fam_fina', 'edu', 'urban_rural','if_aid','aid_n',
                            'aid_time','presidential_approval_rating','ing4','etid','news_read')]) %>% 
  bind_rows(bra_lapop_12[,c('year','state_name','city_name','america_attitude','china_attitude','gender','age','pe_fina','fam_fina', 'edu', 'urban_rural','if_aid','aid_n',
                            'aid_time','presidential_approval_rating','ing4','etid','news_read')]) %>% 
  rename(democracy_prefer = ing4) %>% 
  left_join(pib_21, by=c('state_name','city_name')) %>% 
  mutate(news_read = case_when(
    news_read %in% c(1,2) ~ 1,
    TRUE ~ 0),
    log_gdp_per = log(gdp_per)) %>% 
  filter(!is.na(china_attitude) & !is.na(america_attitude))




bra_lapop %>%
  select(china_attitude,america_attitude,age,pe_fina,edu, urban_rural,if_aid,aid_n,
         presidential_approval_rating,news_read, democracy_prefer) %>%
  describe() 


bra_lapop %>% filter(is.na(log_gdp_per))



library(fixest)


model_11 <- lm(china_attitude ~ aid_n,
                   data = bra_lapop)
summary(model_11)

model_11 <- lm(america_attitude ~ aid_n,
               data = bra_lapop)
summary(model_11)

model_12 <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + urban_rural + 
                 presidential_approval_rating+ democracy_prefer + news_read,
            data = bra_lapop)

summary(model_12)


model_12 <- lm(america_attitude ~ aid_n + gender + age + edu + pe_fina + urban_rural + 
                 presidential_approval_rating+ democracy_prefer + news_read,
               data = bra_lapop)

summary(model_12)

model_13c <- lm(china_attitude ~ aid_n + log_gdp_per + gender + age + edu + pe_fina + urban_rural + 
                 presidential_approval_rating+ democracy_prefer + news_read,
               data = bra_lapop)

summary(model_13c)

model_13a <- lm(america_attitude ~ aid_n + log_gdp_per + gender + age + edu + pe_fina + urban_rural + 
                 presidential_approval_rating+ democracy_prefer + news_read,
               data = bra_lapop)

summary(model_13a)


model_14c <- feols(china_attitude ~ aid_n + gender + age + edu + pe_fina +
                      urban_rural + presidential_approval_rating + log_gdp_per +
                      democracy_prefer + news_read | year + city_id,
                    cluster = "city_id", data = bra_lapop)


summary(model_14c)


model_14a <- feols(america_attitude ~ aid_n + gender + age + edu + pe_fina +
                    urban_rural + presidential_approval_rating + log_gdp_per +
                    democracy_prefer + news_read | year + city_id,
                  cluster = "city_id", data = bra_lapop)


summary(model_14a)


model_15c <- feols(china_attitude ~ aid_n + gender + age + edu + pe_fina +
                    urban_rural + log_gdp_per + presidential_approval_rating + democracy_prefer + 
                    news_read | year,
                  # cluster = "year", 
                  data = bra_lapop )

summary(model_15c)


model_15a <- feols(america_attitude ~ aid_n + gender + age + edu + pe_fina +
                    urban_rural + log_gdp_per + presidential_approval_rating + democracy_prefer + 
                    news_read | year,
                  # cluster = "year", 
                  data = bra_lapop )

summary(model_15a)

model_16c <- feols(china_attitude ~ aid_n + gender + age + edu + pe_fina +
                    urban_rural + presidential_approval_rating + log_gdp_per +
                    democracy_prefer + news_read | city_id,
                  cluster = "city_id", data = bra_lapop)


summary(model_16c)

model_16a <- feols(america_attitude ~ aid_n + gender + age + edu + pe_fina +
                    urban_rural + presidential_approval_rating + log_gdp_per +
                    democracy_prefer + news_read | city_id,
                  cluster = "city_id", data = bra_lapop)


summary(model_16a)


#################################################################################


model_21 <- lm(china_attitude ~ if_aid,
            data = bra_lapop)

summary(model_21)

model_22 <- lm(china_attitude ~ if_aid + gender + age + edu + pe_fina + urban_rural + 
                 presidential_approval_rating+ democracy_prefer + news_read,
               data = bra_lapop)

summary(model_22)

model_23 <- lm(china_attitude ~ if_aid + log_gdp_per + gender + age + edu + pe_fina + urban_rural + 
                 presidential_approval_rating+ democracy_prefer + news_read,
               data = bra_lapop)

summary(model_23)

# model_24 <- feols(china_attitude ~ if_aid + gender + age + edu + pe_fina + 
#                     urban_rural + presidential_approval_rating + log_gdp_per +
#                     democracy_prefer + news_read | year + city_id,
#                   cluster = "city_id", data = bra_lapop)
# 
# summary(model_24)



model_urban <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + 
                    log_gdp_per + presidential_approval_rating + 
                    democracy_prefer + news_read,
                  data = bra_lapop %>% filter(urban_rural == 1))
summary(model_urban)


model_rural <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + 
                    log_gdp_per + presidential_approval_rating + 
                    democracy_prefer + news_read,
                  data = bra_lapop %>% filter(urban_rural == 2))
summary(model_rural)


model_high_dev <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + urban_rural + 
                       presidential_approval_rating + democracy_prefer + news_read,
                  data = bra_lapop %>% filter(dev_level == 1))
summary(model_high_dev)


model_low_dev <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + urban_rural + 
                      presidential_approval_rating + democracy_prefer + news_read,
                  data = bra_lapop %>% filter(dev_level == 0))
summary(model_low_dev)


model_more_news <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + urban_rural + 
                        log_gdp_per + presidential_approval_rating + democracy_prefer,
                     data = bra_lapop %>% filter(news_read == 1))
summary(model_more_news)



model_less_news <- lm(china_attitude ~ aid_n + gender + age + edu + pe_fina + urban_rural + 
                        log_gdp_per + presidential_approval_rating + democracy_prefer,
                    data = bra_lapop %>% filter(news_read == 0))
summary(model_less_news)





