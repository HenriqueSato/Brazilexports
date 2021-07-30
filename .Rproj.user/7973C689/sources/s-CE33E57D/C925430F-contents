#Parte 2 - Participação Brasil

#Importando df das exportações brasileiras em 2019
top17_BRA_Exp_20182019 <- read.csv(here::here("data-raw/exportações brasil top 17por parceiro 2018-2019.csv"))

#Calcular a participação brasileira
top17_BRA_Exp_20182019 %>%
  filter (Year == 2019) %>%
  select (Commodity, Partner, Trade.Value..US..) %>%
  rename (ExportBR2019 = Trade.Value..US.., Reporter = Partner) %>%
  right_join(oportunidades, by = c("Reporter", "Commodity")) -> oportunidades2

#substituir os NA por 0
oportunidades2[is.na(oportunidades2)] <- 0

#Calcular a participação e potencial
oportunidades2 %>% 
  mutate (Participacao_BR =  ExportBR2019/ Import2019,
          Potencial = Import2019 - ExportBR2019) -> oportunidades2

#Descartar participação brasileira maior que x%
filter (oportunidades2, Participacao_BR < 0.1) %>%

#Organizar por potencial
arrange (desc(Potencial)) -> oportunidades2

