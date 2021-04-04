rm(list = ls())

library(tidyverse)
library(ggthemes)
library(ggsci) # color palette
library(gridExtra)
library(ggtext) # to change font
library(extrafont)
library(ggcorrplot)
library(timetk)

# Theme ----
loadfonts(device = "win")
windowsFonts(LM = windowsFont("lmromandemi10-regular"))
windowsFonts(LM2 = windowsFont("lmroman10-regular"))

theme_set(theme_classic())
theme_update(panel.grid.major.y = element_line(linetype = "dotted", color = "gray70"),
             axis.title = element_text(size = 10, color = "black"),
             axis.text = element_text(size = 10, color = "black"),
             plot.title = element_text(family = 'LM', size = 16, color = 'black'),
             plot.caption = element_text(family = 'LM2', size = 13, color = 'black'),
             plot.subtitle = element_text(family = 'LM2', size = 13, color = 'black'))



# loading Databases ----
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/db_industry.RData")
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/db_retail.RData")
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/db_services.RData")
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/non_sectoral_data.RData")


# Line Plot - ALL ----

## Non sectoral variables ----

## 1) Selic 

g1 <- ggplot(non_sectoral, aes(x = date, y = selic))+
  geom_line(size = 1, color = "#1874CD")+
  scale_y_continuous(breaks = seq(0, 25, 5), labels = function(x) paste0(x, "%"))+
  labs(x = "", 
       y = "",
       title = "(1) Instrumento de política monetária",
       subtitle = "Taxa de juros - Over / Selic (% a.a) - Média mensal")


## 2) Inflation 

g2 <- ggplot(non_sectoral, aes(x = date, y = IPCA_A))+
  geom_line(size = 1, color = "#1874CD")+
  scale_y_continuous(breaks = seq(0, 20, 5), labels = function(x) paste0(x, "%"))+
  labs(x = "", 
       y = "",
       title = "(2) Inflação",
       subtitle = "Variação acumulada em doze meses do IPCA")

## 3) Exchange rate

g3 <- ggplot(non_sectoral, aes(x = date, y = cambio))+
  geom_line(size = 1, color = "#1874CD")+
  scale_y_continuous(breaks = seq(0, 5, 1))+
  labs(x = "", 
       y = "R$ / US$",
       title = "(3) Taxa de câmbio",
       subtitle = "Taxa de câmbio R$ / US$ - venda - média do período")

## 4) Credit

g4 <- ggplot(non_sectoral, aes(x = date, y = credito_sa))+
  geom_line(size = 1, color = "#1874CD")+
  labs(x = "", 
       y = "Milhões de R$",
       title = "(4) Crédito",
       subtitle = "Concessões de crédito - livres - série encadeada ao crédito referencial - dessaz. - Total")

## 5) Money supply
options(scipen=10000)
g5 <- ggplot(non_sectoral, aes(x = date, y = money_supply))+
  geom_line(size = 1, color = "#1874CD")+
  labs(x = "", 
       y = "u.m.c (mil)",
       title = "(5) Agregado monetário",
       subtitle = "Meios de pagamento - M1 (saldo em final de período) - dessaz.")

## Saving 1 to 5
layout_matrix <- matrix(c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 
                          4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5), nrow = 2, byrow = TRUE)

grid <- grid.arrange(g1, g2, g3, g4, g5, layout_matrix = layout_matrix)
ggsave("descritiva_1.png", grid, width = 15, height = 8.5, units = "in", dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")

## Sectoral activity ----

### Industry 
g1 <- ggplot(db_industry, aes(x = date, y = `Indústria geral`))+
  geom_line(size = 1, color = "#006400")+
  labs(x = "", 
       y = "",
       title = "(6.1) Produto Industrial - Geral",
       subtitle = "Índice dessaz. (média 2012 = 100)")

### Retail
g2 <- ggplot(db_retail, aes(x = date, y = `Índice de volume de vendas no comércio varejista`))+
  geom_line(size = 1, color = "#006400")+
  labs(x = "", 
       y = "",
       title = "(7.1) Produto do Comércio - Geral",
       subtitle = "Índice dessaz. (média 2014 = 100)")

### Services
g3 <- ggplot(db_services, aes(x = date, y = `Índice de volume de serviços`))+
  geom_line(size = 1, color = "#006400")+
  labs(x = "", 
       y = "",
       title = "(8.1) Produto de Serviços - Geral",
       subtitle = "Índice dessaz. (média 2014 = 100)")

## Saving 1 to 3
layout_matrix <- matrix(c(1, 1, 2, 2, 
                          4, 3, 3, 4), nrow = 2, byrow = TRUE)

grid <- grid.arrange(g1, g2, g3, layout_matrix = layout_matrix)
ggsave("descritiva_2.png", grid, width = 15, height = 8.5, units = "in", dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")


# Correlation plots ----
## Corr plot - Industry ----
db_industry_ren <- db_industry %>% 
  rename(Selic = selic,
         `Inflação` = IPCA_A,
         `Câmbio` = cambio,
         `Crédito` = credito_sa,
         `Agregado monetário` = money_supply)
  
corr <- round(cor(db_industry_ren[, -1]), 1)

g1 <- ggcorrplot(corr, type = "lower",
           lab = F,
           lab_size = 3,
           method="square", 
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlações - Indústria",
           ggtheme = theme_bw)+
  theme(plot.title = element_text(family = 'LM', size = 14, color = 'black'),
        axis.text = element_text(family = 'LM'),
        legend.text = element_text(family = 'LM', size = 10))

## Corr plot - Retail ----

db_retail_ren <- db_retail %>% 
  rename(Selic = selic,
         `Inflação` = IPCA_A,
         `Câmbio` = cambio,
         `Crédito` = credito_sa,
         `Agregado monetário` = money_supply,
         `Produto do Comércio - Geral` = `Índice de volume de vendas no comércio varejista`,
         `Produto do Comércio I` = `Móveis e eletrodomésticos`,
         `Produto do Comércio II` = `Combustíveis e lubrificantes`,
         `Produto do Comércio III` = `Hipermercados, supermercados, produtos alimentícios, bebidas e fumo`,
         `Produto do Comércio IV` = `Tecidos, vestuário e calçados`,
         `Produto do Comércio V` = `Veículos, motocicletas, partes e peças`)

corr <- round(cor(db_retail_ren[, -1]), 1)

g2 <- ggcorrplot(corr, type = "lower",
                 lab = F,
                 lab_size = 3,
                 method="square", 
                 colors = c("tomato2", "white", "springgreen3"),
                 title="Correlações - Comércio",
                 ggtheme = theme_bw)+
  theme(plot.title = element_text(family = 'LM', size = 14, color = 'black'),
        axis.text = element_text(family = 'LM'),
        legend.text = element_text(family = 'LM', size = 10))


## Corr plot - Services ----

db_services_ren <- db_services %>% 
  rename(Selic = selic,
         `Inflação` = IPCA_A,
         `Câmbio` = cambio,
         `Crédito` = credito_sa,
         `Agregado monetário` = money_supply,
         `Produto de Serviços - Geral` = `Índice de volume de serviços`,
         `Produto de Serviços I` = `Serviços profissionais, administrativos e complementares`,
         `Produto de Serviços II` = `Serviços prestados às famílias`,
         `Produto de Serviços III` = `Serviços de informação e comunicação`,
         `Produto de Serviços IV` = `Transportes, serviços auxiliares aos transportes e correio`)

corr <- round(cor(db_services_ren[, -1]), 1)

g3 <- ggcorrplot(corr, type = "lower",
                 lab = F,
                 lab_size = 3,
                 method="square", 
                 colors = c("tomato2", "white", "springgreen3"),
                 title="Correlações - Serviços",
                 ggtheme = theme_bw)+
  theme(plot.title = element_text(family = 'LM', size = 14, color = 'black'),
        axis.text = element_text(family = 'LM'),
        legend.text = element_text(family = 'LM', size = 10))

### Saving corr plots ----
layout_matrix <- matrix(c(1, 1, 2, 2, 
                          4, 3, 3, 4), nrow = 2, byrow = TRUE)

grid <- grid.arrange(g1, g2, g3, layout_matrix = layout_matrix)
ggsave("correlations.png", grid, width = 15, height = 9, units = "in", dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")

#ggsave(g1, filename = 'correlations_industry.png', width = 7, height = 6,  dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")
#ggsave(g2, filename = 'correlations_retail.png', width = 7, height = 6,  dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")
#ggsave(g3, filename = 'correlations_services.png', width = 7, height = 6,  dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")


# Correlation analysis - unfinished ----


plot_acf_diagnostics(.data = db_industry,
                     .date_var = date,
                     .lags = 36,
                     .value = `Indústria geral`,
                     .ccf_vars = c(selic, IPCA_A, credito_sa, cambio, money_supply),
                     .facet_ncol = 2,
                     .show_ccf_vars_only = TRUE,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE,
                     .point_size = 2,
                     .line_size = 0.75,
                     .white_noise_line_color = "blue")
