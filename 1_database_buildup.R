library(tidyverse)
library(lubridate)
library(zoo)
library(sidrar)
library(rbcb)

# Code for gathering data

# Selic ----
code <- c(selic_daily = 1178)
juros_db <- rbcb::get_series(code, "2000-02-01") # Selic data from BCB SGS

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

# IPCA ----
ipca <- '/t/1737/n1/all/v/2266/p/all/d/v2266%2013' %>%
  get_sidra(api = .) %>%
  dplyr::mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>%
  dplyr::select(date, value = Valor) %>%  # keep only relevant columns
  dplyr::mutate(
    IPCA_A = (value / dplyr::lag(value, 12) - 1) * 100 # YoY Inflation in %
  ) %>%  
  dplyr::filter(date >= as.Date("2002-01-01"))

# Produção industrial (PIM-PF)----

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


# Atividade do comércio (PMC)----

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