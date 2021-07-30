#Script Inicial - Código simples para buscar "oportunidades"
library (tidyverse)
library(here)

files <- fs::dir_ls(here::here("data-raw"))

df <- purrr::map_df(files, read_csv)

df %>%  
  View()


df <- comerciobr::sh4_df %>% 
  filter(path == "EXP") %>% 
  filter(co_ano > 2016 & co_ano != max(co_ano))

# comerciobr::dic_

df %>% 
  group_by(co_ano, co_sh4) %>% 
  summarise(value = sum(value)) %>% 
  slice_max(value, n = 10) %>% 
  ungroup() %>% 
  distinct(co_sh4) %>% 
  left_join(comerciobr::dic_sh6_sh4) %>% 
  distinct(no_sh4_por)
  pull()
  pivot_wider(names_from = co_ano, values_from = value)

#Importação dos dados do arquivo csv com dados de exportação dos últimos 4 anos completos HS4 do padrão 2017
Exp_2017_2020 <- read_csv(here("data-raw/brazil_exports_2017-2020_hs(2017)4.csv"))

#Exp_2017_2020 %>% 
#  janitor::clean_names()


#Organizar por HS4 e colocar lado a lado
Exp_2017_2020 %>% 
  filter(Year == 2017) %>%
  select(Commodity, Trade.Value..US..) %>%
  rename(Value2017 = Trade.Value..US..) -> year_by_year

filter (Exp_2017_2020, Year == 2018) %>%
  select (Commodity, Trade.Value..US..) %>%
  rename (Value2018 = Trade.Value..US..) %>%
  full_join(year_by_year) -> year_by_year

filter (Exp_2017_2020, Year == 2019) %>%
  select (Commodity, Trade.Value..US..) %>%
  rename (Value2019 = Trade.Value..US..) %>%
  full_join(year_by_year) -> year_by_year

filter (Exp_2017_2020, Year == 2020) %>%
  select (Commodity, Trade.Value..US..) %>%
  rename (Value2020 = Trade.Value..US..) %>%
  full_join(year_by_year) -> year_by_year

# year_by_year %>% 
#   pivot_longer(cols = Value2020:Value2017, names_to = "ano", values_to = "value") %>% 
#   pivot_wider(names_from = ano, values_from = value)
#   ggplot() +
#   geom_point(aes(ano, value, color = value), show.legend = F)

#Como vamos filtrar?
ggplot(data = year_by_year, mapping = aes(x = Value2020, y = Value2017)) + 
  geom_point(mapping = aes(x = Value2020, y = Value2017), colour= "blue") +
  geom_point(mapping = aes(x = Value2020, y = Value2018), colour= "black")+ 
  geom_point(mapping = aes(x = Value2020, y = Value2019), colour= "green") +
  geom_abline() +
  coord_fixed()

#Tirar produtos que tenham tido valor de exportação menor que um limiar (exportações consistentes)
limiar <- 2e+09
filter (year_by_year, Value2017 > limiar & 
          Value2018 > limiar & 
          Value2019 > limiar & 
          Value2020 > limiar )

year_by_year %>% 
  filter(if_all(.cols = everything(), ~ . > limiar))
