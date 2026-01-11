library(tidyverse)
library(censobr)

#only available for the years: 1960 1970 1980 1991 2000 2010

data_dictionary(year = 2010,
                dataset = 'population')

data_dictionary(year = 2010,
                dataset = 'households',
                cache = FALSE)
data_dictionary(year = 2010, dataset = 'tracts')


house_prop <- read_households(year = 2010,
                              columns = c('code_muni','V0206','V0207','V0208',
                                          'V0209','V0210','V0211'),
                              add_labels = 'pt',
                              showProgress = FALSE) %>% 
  group_by(code_muni) %>%
  summarise(
    v206 = mean(V0206 %in% c("Sim")),
    v207 = mean(V0207 %in% c("Rede geral de esgoto ou pluvial","Fossa séptica","Fossa rudimentar")),
    v208 = mean(V0208 %in% c("Rede geral de distribuição","Poço ou nascente na propriedade")),
    v209 = mean(V0209 %in% c("Sim, em pelo menos um cômodo","Sim, só na propriedade ou terreno")),
    v210 = mean(V0210 %in% c("Coletado diretamente por serviço de limpeza","Colocado em caçamba de serviço de limpeza")),
    v211 = mean(V0211 %in% c("Sim, de companhia distribuidora","Sim, de outras fontes")),
    city_dev = (v206+v207+v208+v209+v210+v211)/6
  ) %>% 
  select(code_muni,city_dev) %>% 
  collect()



pop <- read_population(year = 2010,
                       add_labels = 'pt',
                       showProgress = FALSE)

df <- pop |>
  filter(abbrev_state == "RJ") |>                                                 
  compute() |>
  group_by(V0606) |>                                                                 
  summarize(higher_edu = sum(V0010[which(V6400=="Superior completo")]) / sum(V0010), 
            pop = sum(V0010) ) |>
  collect()


tract_df <- read_tracts(year = 2022,
                  dataset = 'Preliminares',
                  showProgress = TRUE) %>% 
  collect()

# short questionnaire
questionnaire(year = 2022, 
              type = 'short')

# long questionnaire
questionnaire(year = 2022, 
              type = 'long')
