rm(list = ls())

library(tidyverse)
library(gridExtra)
library(vars)
library(tseries)
library(VARtests)
library(extrafont)

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
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/db_retail.RData")

# ADF tests ----

db_retail_adf <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(-date, -IPCA_A, -cambio, -credito_sa, -money_supply) # table to use in ADF tests function

adf_tests_matrix <- function(vars) {
  
  ##
  # Function to perform ADF tests on all columns of a table
  ##
  
  d <- as.matrix(vars) # convert data frame to Matrix
  n <- length(colnames(vars)) #total number of variables
  names <- colnames(vars) # names of variables
  result <-
    matrix(NA, nrow = n, ncol = 3) # empty matrix for results
  colnames(result) <- c('Variable', 'ADF on level', 'ADF on Diff')
  
  for (i in 1:n) {
    pvalue_level <- tseries::adf.test(d[, i])$p.value # pvalue of ADF test on level
    pvalue_diff <- tseries::adf.test(diff(d[, i]))$p.value # pvalue of ADF test on first diff
    
    result[i, 1] <- names[i]
    result[i, 2] <- round(as.numeric(pvalue_level), 3)
    result[i, 3] <- round(as.numeric(pvalue_diff), 3)
  }
  
  return(result)
}

adf_tests_matrix(db_retail_adf)
stargazer(adf_tests_matrix(db_retail_adf))

# 1) Índice de volume de vendas no comércio varejista ----
## Data ----
db_retail_mod1 <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de vendas no comércio varejista`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_retail = `Índice de volume de vendas no comércio varejista`)

db_retail_mod1 <- ts(db_retail_mod1[, -1], start = c(2001, 12), frequency = 12)

## Model ----
VARselect(db_retail_mod1, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model1 <- VAR(db_retail_mod1, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model1, modulus = TRUE) # AR must be < 1

ACtest(model1, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model1, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model1, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model1, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model1, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod1 <-
  irf(
    model1,
    impulse = "selic",
    response = "Y_retail",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#208A12") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#208A12") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Produto do comércio - Geral',
       x = 'Meses após o choque',
       y = '')

# ________________________________________----
# ________________________________________
# 2) Índice de volume de vendas no comércio varejista + Móveis e eletrodomésticos ----
## Data ----
db_retail_mod2 <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de vendas no comércio varejista`,
    `Móveis e eletrodomésticos`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_retail = `Índice de volume de vendas no comércio varejista`,
         Y_mov_elet = `Móveis e eletrodomésticos`)

db_retail_mod2 <- ts(db_retail_mod2[, -1], start = c(2001, 12), frequency = 12)

## Model ----
VARselect(db_retail_mod2, 
          lag.max = 12, 
          type = 'both') # 6 lags selected based on FPE and AIC

model2 <- VAR(db_retail_mod2, 
              p = 6, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model2, modulus = TRUE) # AR must be < 1

ACtest(model2, h = 7, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(7), H0: no AC)
ACtest(model2, h = 8, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(8), H0: no AC) 
ACtest(model2, h = 9, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(9), H0: no AC)

normality.test(model2, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model2, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod2 <-
  irf(
    model2,
    impulse = "selic",
    response = "Y_mov_elet",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#208A12") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#208A12") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Móveis e eletrodomésticos',
       x = 'Meses após o choque',
       y = '')

# ________________________________________----
# ________________________________________
# 3) Índice de volume de vendas no comércio varejista + Combustíveis e lubrificantes ----
## Data ----
db_retail_mod3 <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de vendas no comércio varejista`,
    `Combustíveis e lubrificantes`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_retail = `Índice de volume de vendas no comércio varejista`,
         Y_combs_lub = `Combustíveis e lubrificantes`)

db_retail_mod3 <- ts(db_retail_mod3[, -1], start = c(2001, 12), frequency = 12)

## Model ----
VARselect(db_retail_mod3, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all

model3 <- VAR(db_retail_mod3, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model3, modulus = TRUE) # AR must be < 1

ACtest(model3, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model3, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model3, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model3, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model3, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod3 <-
  irf(
    model3,
    impulse = "selic",
    response = "Y_combs_lub",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#208A12") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#208A12") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Combustíveis e lubrificantes',
       x = 'Meses após o choque',
       y = '')

# ________________________________________----
# ________________________________________
# 4) Índice de volume de vendas no comércio varejista + Hipermercados, supermercados, produtos alimentícios, bebidas e fumo ----
## Data ----
db_retail_mod4 <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de vendas no comércio varejista`,
    `Hipermercados, supermercados, produtos alimentícios, bebidas e fumo`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_retail = `Índice de volume de vendas no comércio varejista`,
         Y_sup_alim = `Hipermercados, supermercados, produtos alimentícios, bebidas e fumo`)

db_retail_mod4 <- ts(db_retail_mod4[, -1], start = c(2001, 12), frequency = 12)

## Model ----
VARselect(db_retail_mod4, 
          lag.max = 12, 
          type = 'both') # 3 lags selected based on FPE and AIC

model4 <- VAR(db_retail_mod4, 
              p = 3, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model4, modulus = TRUE) # AR must be < 1

ACtest(model4, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC)
ACtest(model4, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC) 
ACtest(model4, h = 6, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(6), H0: no AC)

normality.test(model4, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model4, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod4 <-
  irf(
    model4,
    impulse = "selic",
    response = "Y_sup_alim",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#208A12") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#208A12") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Supermercados e alimentos',
       x = 'Meses após o choque',
       y = '')

# ________________________________________----
# ________________________________________
# 5) Índice de volume de vendas no comércio varejista + Tecidos, vestuário e calçados ----
## Data ----
db_retail_mod5 <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de vendas no comércio varejista`,
    `Tecidos, vestuário e calçados`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_retail = `Índice de volume de vendas no comércio varejista`,
         Y_tec_vest = `Tecidos, vestuário e calçados`)

db_retail_mod5 <- ts(db_retail_mod5[, -1], start = c(2001, 12), frequency = 12)

## Model ----
VARselect(db_retail_mod5, 
          lag.max = 12, 
          type = 'both') # 3 lags selected based on FPE and AIC

model5 <- VAR(db_retail_mod5, 
              p = 3, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model5, modulus = TRUE) # AR must be < 1

ACtest(model5, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC)
ACtest(model5, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC) 
ACtest(model5, h = 6, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(6), H0: no AC)

normality.test(model5, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model5, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod5 <-
  irf(
    model5,
    impulse = "selic",
    response = "Y_tec_vest",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#208A12") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#208A12") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Tecidos, vestuário e calçados',
       x = 'Meses após o choque',
       y = '')

# ________________________________________----
# ________________________________________
# 6) Índice de volume de vendas no comércio varejista + Veículos, motocicletas, partes e peças ----
## Data ----
db_retail_mod6 <- db_retail %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Índice de volume de vendas no comércio varejista`,
    `Veículos, motocicletas, partes e peças`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_retail = `Índice de volume de vendas no comércio varejista`,
         Y_veic_mot = `Veículos, motocicletas, partes e peças`)

db_retail_mod6 <- ts(db_retail_mod6[, -1], start = c(2001, 12), frequency = 12)

## Model ----
VARselect(db_retail_mod6, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all

model6 <- VAR(db_retail_mod6, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model6, modulus = TRUE) # AR must be < 1

ACtest(model6, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model6, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model6, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model6, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model6, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod6 <-
  irf(
    model6,
    impulse = "selic",
    response = "Y_veic_mot",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g6 <- tibble(
  IRF = Y_IRF_mod6$irf$selic,
  Lower = Y_IRF_mod6$Lower$selic,
  Upper = Y_IRF_mod6$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#208A12") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#208A12") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Veículos, motocicletas e peças',
       x = 'Meses após o choque',
       y = '')

# ________________________________________----
# ________________________________________
# Saving plots ----
layout_matrix <- matrix(c(1, 1, 2, 2,
                          3, 3, 4, 4, 
                          5, 5, 6, 6), nrow = 3, byrow = TRUE)

grid <- grid.arrange(g1, g2, g3, g4, g5, g6, layout_matrix = layout_matrix)
ggsave("IRF_varejo.png", grid, width = 7.7, height = 9.9, units = "in", dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")
