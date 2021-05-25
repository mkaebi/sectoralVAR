rm(list = ls())

library(tidyverse)
library(lubridate)
library(gridExtra)
library(vars)
library(tseries)
library(VARtests)
library(extrafont)
library(readxl)
library(ipeadatar)
library(fredr)
fredr_set_key("123456") # insirir chave de acesso ao API

# Theme ----
loadfonts(device = "win")
windowsFonts(LM = windowsFont("lmromandemi10-regular"))
windowsFonts(LM2 = windowsFont("lmroman10-regular"))

theme_set(theme_classic())
theme_update(panel.grid.major.y = element_line(linetype = "dotted", color = "gray70"),
             axis.title = element_text(size = 9, color = "black"),
             axis.text = element_text(size = 8, color = "black"),
             plot.title = element_text(family = 'LM', size = 12, color = 'black'),
             plot.caption = element_text(family = 'LM2', size = 13, color = 'black'),
             plot.subtitle = element_text(family = 'LM2', size = 13, color = 'black'))

# loading Database ----
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/db_services.RData")

dummy_recession <- read_excel("Dados/rececoes_codace.xlsx")
dummy_recession$date <- as.Date(dummy_recession$date)
dummy_recession <- dummy_recession %>% 
  filter(date >= as.Date(db_services$date[1])) # keeps same start date

fedfunds <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date(db_services$date[1]),
  observation_end = as.Date("2021-02-01")
) %>%
  dplyr::select(date, value) %>% 
  rename(fedfunds = value)

commodities <- fredr(
  series_id = "PALLFNFINDEXM",
  observation_start = as.Date(db_services$date[1]),
  observation_end = as.Date("2021-02-01")
) %>%
  dplyr::select(date, value)%>% 
  rename(commodities = value)

EMBI_db <- ipeadata("JPM366_EMBI366", quiet = FALSE) %>% 
  dplyr::select(date, value)
EMBI <- EMBI_db %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(month, year) %>%
  summarise(EMBI = mean(value)) %>%
  mutate(day = 1, date = as.Date(paste(year, month, day, sep = "-"))) %>%
  ungroup() %>%
  dplyr::select(date, EMBI) %>%
  arrange(date) %>% 
  filter(date >= as.Date(db_services$date[1]), date <= as.Date("2021-02-01"))

dummies <- as.matrix(cbind(dummy_recession[, 2], fedfunds[, 2], log(commodities[, 2]), log(EMBI[, 2])))
rm(dummy_recession, EMBI_db, EMBI, fedfunds, commodities)

# ADF tests ----

db_services_adf <- db_services %>%
  dplyr::mutate(
    #log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(-date, -IPCA_A, -cambio, -credito_sa, -money_supply) # table to use in ADF tests function

summary(ur.df(db_services_adf$IPCA_M, type = 'drift', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$selic, type = 'none', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$log_money_supply, type = 'trend', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$log_credito_sa, type = 'trend', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$log_cambio, type = 'trend', selectlags = "AIC", lags = 12))

summary(ur.df(db_services_adf$`Índice de volume de serviços`, type = 'trend', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$`Serviços profissionais, administrativos e complementares`, type = 'drift', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$`Serviços prestados às famílias`, type = 'drift', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$`Serviços de informação e comunicação`, type = 'drift', selectlags = "AIC", lags = 12))
summary(ur.df(db_services_adf$`Transportes, serviços auxiliares aos transportes e correio`, type = 'trend', selectlags = "AIC", lags = 12))


# 1) Índice de volume de serviços ----
## Data ----
db_services_mod1 <- db_services %>%
  dplyr::mutate(
    #log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de serviços`,
    #log_IPCA_A,
    IPCA_M,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_services = `Índice de volume de serviços`)

db_services_mod1 <- ts(db_services_mod1[, -1], start = c(2011, 1), frequency = 12)

## Lag selection ----

VARselect(db_services_mod1, 
          lag.max = 12, 
          type = 'trend', 
          exogen = dummies, 
          season = 12) # 2 lags selected based on all criterias

## VAR Model ----

model1_var <- VAR(db_services_mod1,
                  p = 2,
                  type = 'trend',
                  season = 12,
                  exogen = dummies)


#plot(model1_var)
#summary(model1_var)
roots(model1_var)
serial.test(model1_var, lags.pt = 36)
normality.test(model1_var, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model1_var, lags.multi = 5, multivariate.only = TRUE) # heteroskedasticity test


## Cointegration (Johansen Procedure) ----

jotest1 <- ca.jo(
  db_services_mod1,
  type = "eigen",
  K = 2, # num of lags
  ecdet = "cons", # 
  spec = "longrun",
  dumvar = dummies,
  season = 12
) # estimates VECM with 1 lag, trend and constant

summary(jotest1) # has cointegration

## Model ----

model1 <- vec2var(jotest1, r = 5) # transforms previous VECM to VAR for IRF estimation

## Residual diagnostics ----
serial.test(model1, lags.pt = 36)
normality.test(model1, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model1, lags.multi = 5, multivariate.only = TRUE) # heteroskedasticity test


## Impulse response function ----
n_ahead <- 36

Y_IRF_mod1 <-
  irf(
    model1,
    impulse = "selic",
    response = "Y_services",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g1 <- tibble(
  IRF = Y_IRF_mod1$irf$selic,
  Lower = Y_IRF_mod1$Lower$selic,
  Upper = Y_IRF_mod1$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#6812B3") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#6812B3") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Produto de Serviços - Geral',
       x = 'Meses após o choque',
       y = '')
g1

# ________________________________________----
# ________________________________________

# 2) Índice de volume de serviços + Serviços profissionais, administrativos e complementares----
## Data ----
db_services_mod2 <- db_services %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de serviços`,
    `Serviços profissionais, administrativos e complementares`,
    #log_IPCA_A,
    IPCA_M,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_services = `Índice de volume de serviços`,
         Y_prof_adm = `Serviços profissionais, administrativos e complementares`)

db_services_mod2 <- ts(db_services_mod2[, -1], start = c(2011, 1), frequency = 12)

## Lag selection ----

VARselect(db_services_mod2, 
          lag.max = 12, 
          type = 'trend', 
          exogen = dummies, 
          season = 12) # 2 lags selected based on all criterias

## VAR Model ----

model2_var <- VAR(db_services_mod2,
                  p = 2,
                  type = 'cons',
                  season = 12,
                  exogen = dummies)


#plot(model2_var)
#summary(model2_var)
roots(model2_var)
serial.test(model2_var, lags.pt = 36)
normality.test(model2_var, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model2_var, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Cointegration (Johansen Procedure) ----

jotest2 <- ca.jo(
  db_services_mod2,
  type = "eigen",
  K = 2, # num of lags
  ecdet = "cons", # 
  spec = "longrun",
  dumvar = dummies,
  season = 12
) # estimates VECM with 1 lag, trend and constant

summary(jotest2) # has cointegration

## Model ----

model2 <- vec2var(jotest2, r = 4) # transforms previous VECM to VAR for IRF estimation

## Residual diagnostics ----
serial.test(model2, lags.pt = 36)
normality.test(model2, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model2, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Impulse response function ----
n_ahead <- 36

Y_IRF_mod2 <-
  irf(
    model2,
    impulse = "selic",
    response = "Y_prof_adm",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g2 <- tibble(
  IRF = Y_IRF_mod2$irf$selic,
  Lower = Y_IRF_mod2$Lower$selic,
  Upper = Y_IRF_mod2$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#6812B3") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#6812B3") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Profissionais e Administrativos',
       x = 'Meses após o choque',
       y = '')
g2
# ________________________________________----
# ________________________________________
# 3) Índice de volume de serviços + Serviços prestados às famílias----
## Data ----
db_services_mod3 <- db_services %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de serviços`,
    `Serviços prestados às famílias`,
    #log_IPCA_A,
    IPCA_M,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_services = `Índice de volume de serviços`,
         Y_fam = `Serviços prestados às famílias`)

db_services_mod3 <- ts(db_services_mod3[, -1], start = c(2011, 1), frequency = 12)

## Lag selection ----

VARselect(db_services_mod3, 
          lag.max = 12, 
          type = 'trend', 
          exogen = dummies, 
          season = 12) # 2 lags selected based on all criterias

## VAR Model ----

model3_var <- VAR(db_services_mod3,
                  p = 4,
                  type = 'trend',
                  season = 12,
                  exogen = dummies)


#plot(model3_var)
#summary(model3_var)
roots(model3_var)
serial.test(model3_var, lags.pt = 36)
normality.test(model3_var, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model3_var, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Cointegration (Johansen Procedure) ----

jotest3 <- ca.jo(
  db_services_mod3,
  type = "eigen",
  K = 4, # num of lags
  ecdet = "cons", # 
  spec = "longrun",
  dumvar = dummies,
  season = 12
) # estimates VECM with 1 lag, trend and constant

summary(jotest3) # has cointegration

## Model ----

model3 <- vec2var(jotest3, r = 4) # transforms previous VECM to VAR for IRF estimation

## Residual diagnostics ----
serial.test(model3, lags.pt = 36)
normality.test(model3, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model3, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Impulse response function ----
n_ahead <- 36

Y_IRF_mod3 <-
  irf(
    model3,
    impulse = "selic",
    response = "Y_fam",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g3 <- tibble(
  IRF = Y_IRF_mod3$irf$selic,
  Lower = Y_IRF_mod3$Lower$selic,
  Upper = Y_IRF_mod3$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#6812B3") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#6812B3") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Prestados às Famílias',
       x = 'Meses após o choque',
       y = '')
g3

# ________________________________________----
# ________________________________________
# 4) Índice de volume de serviços + Serviços de informação e comunicação----
## Data ----
db_services_mod4 <- db_services %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de serviços`,
    `Serviços de informação e comunicação`,
    #log_IPCA_A,
    IPCA_M,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_services = `Índice de volume de serviços`,
         Y_info_com = `Serviços de informação e comunicação`)

db_services_mod4 <- ts(db_services_mod4[, -1], start = c(2011, 1), frequency = 12)

## Lag selection ----

VARselect(db_services_mod4, 
          lag.max = 12, 
          type = 'trend', 
          exogen = dummies, 
          season = 12) # 2 lags selected based on all criterias

## VAR Model ----

model4_var <- VAR(db_services_mod4,
                  p = 2,
                  type = 'trend',
                  season = 12,
                  exogen = dummies)


#plot(model4_var)
#summary(model4_var)
roots(model4_var)
serial.test(model4_var, lags.pt = 36)
normality.test(model4_var, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model4_var, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Cointegration (Johansen Procedure) ----

jotest4 <- ca.jo(
  db_services_mod4,
  type = "eigen",
  K = 2, # num of lags
  ecdet = "cons", # 
  spec = "longrun",
  dumvar = dummies,
  season = 12
) # estimates VECM with 1 lag, trend and constant

summary(jotest4) # has cointegration

## Model ----

model4 <- vec2var(jotest4, r = 5) # transforms previous VECM to VAR for IRF estimation

## Residual diagnostics ----
serial.test(model4, lags.pt = 36)
normality.test(model4, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model4, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Impulse response function ----
n_ahead <- 36

Y_IRF_mod4 <-
  irf(
    model4,
    impulse = "selic",
    response = "Y_info_com",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g4 <- tibble(
  IRF = Y_IRF_mod4$irf$selic,
  Lower = Y_IRF_mod4$Lower$selic,
  Upper = Y_IRF_mod4$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#6812B3") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#6812B3") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Informação e Comunicação',
       x = 'Meses após o choque',
       y = '')
g4

# ________________________________________----
# ________________________________________
# 5) Índice de volume de serviços + Transportes, serviços auxiliares aos transportes e correio----
## Data ----
db_services_mod5 <- db_services %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de serviços`,
    `Transportes, serviços auxiliares aos transportes e correio`,
    #log_IPCA_A,
    IPCA_M,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_services = `Índice de volume de serviços`,
         Y_transp = `Transportes, serviços auxiliares aos transportes e correio`)

db_services_mod5 <- ts(db_services_mod5[, -1], start = c(2011, 1), frequency = 12)

## Lag selection ----

VARselect(db_services_mod5, 
          lag.max = 12, 
          type = 'trend', 
          exogen = dummies, 
          season = 12) # 2 lags selected based on all criterias

## VAR Model ----

model5_var <- VAR(db_services_mod5,
                  p = 2,
                  type = 'trend',
                  season = 12,
                  exogen = dummies)


#plot(model5_var)
#summary(model5_var)
roots(model5_var)
serial.test(model5_var, lags.pt = 36)
normality.test(model5_var, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model5_var, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Cointegration (Johansen Procedure) ----

jotest5 <- ca.jo(
  db_services_mod5,
  type = "eigen",
  K = 2, # num of lags
  ecdet = "trend", # 
  spec = "longrun",
  dumvar = dummies,
  season = 12
) # estimates VECM with 1 lag, trend and constant

summary(jotest5) # has cointegration

## Model ----

model5 <- vec2var(jotest5, r = 5) # transforms previous VECM to VAR for IRF estimation

## Residual diagnostics ----
serial.test(model5, lags.pt = 36)
normality.test(model5, multivariate.only = TRUE) # Jarque-Bera normality test
vars::arch.test(model5, lags.multi = 4, multivariate.only = TRUE) # heteroskedasticity test


## Impulse response function ----
n_ahead <- 36

Y_IRF_mod5 <-
  irf(
    model5,
    impulse = "selic",
    response = "Y_transp",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g5 <- tibble(
  IRF = Y_IRF_mod5$irf$selic,
  Lower = Y_IRF_mod5$Lower$selic,
  Upper = Y_IRF_mod5$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#6812B3") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#6812B3") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Transportes',
       x = 'Meses após o choque',
       y = '')
g5

# ________________________________________----
# ________________________________________
# Saving plots ----
layout_matrix <- matrix(c(1, 1, 1, 1,
                          2, 2, 3, 3, 
                          4, 4, 5, 5), nrow = 3, byrow = TRUE)

grid <- grid.arrange(g1, g2, g3, g4, g5, layout_matrix = layout_matrix)
ggsave("IRF_services.png", grid, width = 7.7, height = 9.9, units = "in", dpi = 700)

