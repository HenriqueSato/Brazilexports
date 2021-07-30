#Parte 1 Quais são as oportunidades produto x país?

#Busquei no Comtrade esses top17 HS4 com dados para 2018 e 2019 para verificar quais estavam crescendo no biênio pré COVID
top17_Imp_2018_2019 <- read.csv(here::here("data-raw/mundo_imports_2018-2019_hs(2017)4top17.csv"))

#Organizar por HS4 e colocar lado a lado
filter (top17_Imp_2018_2019, Year == 2018 & Reporter != "Brazil") %>%
  select (Commodity, Reporter, Trade.Value..US..) %>%
  rename (Import2018 = Trade.Value..US..) -> oportunidades

filter (top17_Imp_2018_2019, Year == 2019 & Reporter != "Brazil") %>%
  select (Commodity, Reporter, Trade.Value..US..) %>%
  rename (Import2019 = Trade.Value..US..) %>%
  full_join(oportunidades) -> oportunidades

#Incluir coluna de variação
mutate(oportunidades, Variacao = (Import2019 - Import2018)/Import2018) -> oportunidades

#desprezar NA's e variações menores que 5% positivo
filter (oportunidades, Variacao > 5e-1) -> oportunidades

ggplot(data = filter(oportunidades), mapping = aes(x = Import2019, y = Variacao)) +
  geom_point()
