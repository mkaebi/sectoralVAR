library(tidyverse)
library(lubridate)
library(zoo)
library(sidrar)
library(rbcb)
library(seasonal)

# Code for gathering data and merging into a database

# 1) Selic ----
code <- c(selic_daily = 1178)
juros_db <- rbcb::get_series(code, "2000-01-01") # Selic data from BCB SGS

## Monthly average from daily data
juros <- juros_db %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  summarise(selic = mean(selic_daily)) %>%
  mutate(day = 1, date = as.Date(paste(year, month, day, sep = "-"))) %>%
  ungroup() %>%
  select(date, selic) %>%
  arrange(date)

rm(code, juros_db)

# 2) Exchange rate ----

code <- c(cambio = 3698) # Taxa de câmbio - Livre - Dólar americano (venda) - Média de período - mensal 
usd <- rbcb::get_series(code, "2000-02-01") # Exchange rate data from BCB SGS

# 3) Credit ----

code <- c(credito = 21277) # Concessões de crédito com recursos livres - Série encadeada ao crédito referencial - Total
credito <- rbcb::get_series(code, "2000-02-01") # Nonearmarked new operations - Series chained to reference credit - Total data from BCB SGS
credito <- credito %>% 
  dplyr::mutate(credito_sa = final(seas(ts(credito, start = c(2000, 6), frequency = 12)))) %>% 
  select(date, credito_sa)

# 4) Money supply ----

code <- c(money_supply = 27841) # Meios de pagamento - M1 (saldo em final de período) - Novo - sazonalmente ajustado
money <- rbcb::get_series(code, "2000-02-01") # Money supply - M1 (end-of-period balance) - New - seasonally adjusted data from BCB SGS

rm(code)

# 5) IPCA ----
ipca <- '/t/1737/n1/all/v/2266/p/all/d/v2266%2013' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  dplyr::select(date, value = Valor) %>%  # keep only relevant columns
  dplyr::mutate(
    IPCA_A = (value / dplyr::lag(value, 12) - 1) * 100 # YoY Inflation in %
  ) %>%  
  dplyr::filter(date >= as.Date("2000-01-01")) %>% 
  select(date, IPCA_A)

# 6) Produção industrial (PIM-PF)----

# Índice base fixa com ajuste sazonal (Base: média 2012 = 100)

## Bens de capital, Bens intermediários, bens de consumo duráveis, bens de consumo semiduráveis e não duráveis
pim_1 <-
  '/t/3651/n1/all/v/3134/p/all/c543/129278,129283,129301,129305/d/v3134%201' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, "Grandes categorias econômicas", Valor) %>%
  pivot_wider(names_from = "Grandes categorias econômicas",
              values_from = Valor)

## Indústria geral, extrativa e transformação
pim_2 <-
  '/t/3653/n1/all/v/3135/p/all/c544/129314,129315,129316/d/v3135%201' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, "Seções e atividades industriais (CNAE 2.0)", Valor) %>%
  pivot_wider(names_from = "Seções e atividades industriais (CNAE 2.0)",
              values_from = Valor)

## Joining previous pim tables and renaming columns
PIM <- left_join(pim_2, pim_1, by = "date") %>% 
  rename("Indústria geral" = "1 Indústria geral",
         "Indústrias extrativas" = "2 Indústrias extrativas",
         "Indústrias de transformação" = "3 Indústrias de transformação",
         "Bens de capital" = "1 Bens de capital",
         "Bens intermediários" = "2 Bens intermediários",
         "Bens de consumo duráveis" = "31 Bens de consumo duráveis",
         "Bens de consumo semiduráveis e não duráveis" = "32 Bens de consumo semiduráveis e não duráveis")

rm(pim_1, pim_2)


# 7) Atividade do comércio (PMC)----

# Índice base fixa com ajuste sazonal (Base: média 2014 = 100)

## Índice de volume de vendas no comércio varejista
pmc_1 <-
  '/t/3416/n1/all/v/564/p/all/c11046/40312/d/v564%201' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, "Variável", Valor) %>%
  pivot_wider(names_from = "Variável",
              values_from = Valor)


## Índice de volume de vendas no comércio varejista por tipo de atividade
pmc_2 <-
  '/t/3419/n1/all/v/1186/p/all/c11046/40312/c85/2759,90671,90672,90673,103159/d/v1186%201' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, "Atividades", Valor) %>%
  pivot_wider(names_from = "Atividades",
              values_from = Valor)

## Joining previous pmc tables
PMC <- left_join(pmc_1, pmc_2, by = "date")
rm(pmc_1, pmc_2)


# 8) Atividade dos serviços (PMS)----

# Índice do volume de serviços (2014 = 100) - dessaz.

## Índice do volume de serviços
pms_1 <-
  '/t/6442/n1/all/v/8677/p/all/c11046/40312/d/v8677%201' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, "Variável", Valor) %>%
  pivot_wider(names_from = "Variável",
              values_from = Valor)


## Índice do volume de serviços por tipo de serviço
pms_2 <-
  '/t/6443/n1/all/v/8676/p/all/c11046/40312/c12355/31399,106869,106874,106876/d/v8676%201' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  select(date, "Atividades de serviços", Valor) %>%
  pivot_wider(names_from = "Atividades de serviços",
              values_from = Valor)

## Joining previous pms tables
PMS <- left_join(pms_1, pms_2, by = "date")%>% 
  rename("Serviços profissionais, administrativos e complementares" = "3. Serviços profissionais, administrativos e complementares",
         "Serviços prestados às famílias" = "1. Serviços prestados às famílias",
         "Serviços de informação e comunicação" = "2. Serviços de informação e comunicação",
         "Transportes, serviços auxiliares aos transportes e correio" = "4. Transportes, serviços auxiliares aos transportes e correio")
rm(pms_1, pms_2)


# Database 1 - Industrial sector ----

db_industry <- left_join(PIM, juros, by = 'date') %>%
  left_join(ipca, by = 'date') %>%
  left_join(usd, by = 'date') %>%
  left_join(credito, by = 'date') %>%
  left_join(money, by = 'date')


# Database 2 - Retail sector ----

db_retail <- left_join(PMC, juros, by = 'date') %>%
  left_join(ipca, by = 'date') %>%
  left_join(usd, by = 'date') %>%
  left_join(credito, by = 'date') %>%
  left_join(money, by = 'date') %>% 
  drop_na()

# Database 3 - Services sector ----

db_services <- left_join(PMS, juros, by = 'date') %>%
  left_join(ipca, by = 'date') %>%
  left_join(usd, by = 'date') %>%
  left_join(credito, by = 'date') %>%
  left_join(money, by = 'date')
