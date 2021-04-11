rm(list = ls())

library(tidyverse)
library(gridExtra)
library(vars)
library(tseries)
library(VARtests)
library(extrafont)
library(stargazer)
library(egcm)
library(aTSA)

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
load("C:/Users/Mohammed/Desktop/TCC I/R project/TCC/db_industry.RData")

# ADF tests ----

db_industry_adf <- db_industry %>%
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

adf_tests_matrix(db_industry_adf)
stargazer(adf_tests_matrix(db_industry_adf))

## Cointegration ----

db_industry_coint <- as.matrix(dplyr::select(db_industry_adf, -`Indústria geral`))

coint.test(db_industry_adf$`Indústria geral`, db_industry_coint)

coint <- function(vars) {
  d <- as.matrix(vars) #convert data frame to Matrix
  n <- length(colnames(vars)) #calculate the total number of variables
  m <-
    combn(n, 2) #calculate all possible combinations of pairs for all variables
  col_m <- dim(m)[2] #number of all possible combinations
  result <- matrix(NA, nrow = col_m, ncol = 3) #empty result matrix
  colnames(result) <- c("Var_1", "Var_2", "p_Value")
  
  for (i in 1:col_m) {
    Var_1 <- m[1, i]
    Var_2 <- m[2, i]
    res <- lm(d[, Var_1] ~ d[, Var_2] + 0)$residuals
    p <-
      tseries::adf.test(res, alternative = "stationary", k = 0)$p.value
    result[i, 1] <- colnames(vars)[Var_1]
    result[i, 2] <- colnames(vars)[Var_2]
    result[i, 3] <- round(p, 4)
  }
  
  return(result)
}

coint(db_industry_mod1[, -1])


# 1) Industria Geral ----
## Data ----
db_industry_mod1 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`)

db_industry_mod1 <- ts(db_industry_mod1[, -1], start = c(2002, 1), frequency = 12)


## Model ----
VARselect(db_industry_mod1, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model1 <- VAR(db_industry_mod1, 
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
    response = "Y_industry",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Produção industrial geral',
       x = 'Meses após o choque',
       y = '') 

# ________________________________________
# ________________________________________
# 2) Industria Geral + extrativas ----
## Data ----
db_industry_mod2 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    `Indústrias extrativas`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`,
         Y_industry_extrat = `Indústrias extrativas`)

db_industry_mod2 <- ts(db_industry_mod2[, -1], start = c(2002, 1), frequency = 12)

## Model ----
VARselect(db_industry_mod2, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model2 <- VAR(db_industry_mod2, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model2, modulus = TRUE) # AR must be < 1

ACtest(model2, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model2, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model2, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model2, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model2, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod2 <-
  irf(
    model2,
    impulse = "selic",
    response = "Y_industry_extrat",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Indústrias extrativas',
       x = 'Meses após o choque',
       y = '') 


# ________________________________________
# ________________________________________
# 3) Industria Geral + transformação ----
## Data ----
db_industry_mod3 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    `Indústrias de transformação`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`,
         Y_industry_transf = `Indústrias de transformação`)

db_industry_mod3 <- ts(db_industry_mod3[, -1], start = c(2002, 1), frequency = 12)

## Model ----
VARselect(db_industry_mod3, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model3 <- VAR(db_industry_mod3, 
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
    response = "Y_industry_transf",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Indústrias de transformação',
       x = 'Meses após o choque',
       y = '') 

# ________________________________________
# ________________________________________
# 4) Industria Geral + Bens de capital ----
## Data ----
db_industry_mod4 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    `Bens de capital`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`,
         Y_industry_bens_cap = `Bens de capital`)

db_industry_mod4 <- ts(db_industry_mod4[, -1], start = c(2002, 1), frequency = 12)

## Model ----
VARselect(db_industry_mod4, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model4 <- VAR(db_industry_mod4, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model4, modulus = TRUE) # AR must be < 1

ACtest(model4, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model4, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model4, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model4, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model4, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod4 <-
  irf(
    model4,
    impulse = "selic",
    response = "Y_industry_bens_cap",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Bens de capital',
       x = 'Meses após o choque',
       y = '') 

# ________________________________________
# ________________________________________
# 5) Industria Geral + Bens intermediários ----
## Data ----
db_industry_mod5 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    `Bens intermediários`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`,
         Y_industry_bens_inter = `Bens intermediários`)

db_industry_mod5 <- ts(db_industry_mod5[, -1], start = c(2002, 1), frequency = 12)

## Model ----
VARselect(db_industry_mod5, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model5 <- VAR(db_industry_mod5, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model5, modulus = TRUE) # AR must be < 1

ACtest(model5, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model5, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model5, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model5, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model5, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod5 <-
  irf(
    model5,
    impulse = "selic",
    response = "Y_industry_bens_inter",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Bens intermediários',
       x = 'Meses após o choque',
       y = '')


# ________________________________________
# ________________________________________
# 6) Industria Geral + Bens de consumo duráveis ----
## Data ----
db_industry_mod6 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    `Bens de consumo duráveis`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`,
         Y_industry_bens_cons_dur = `Bens de consumo duráveis`)

db_industry_mod6 <- ts(db_industry_mod6[, -1], start = c(2002, 1), frequency = 12)

## Model ----
VARselect(db_industry_mod6, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model6 <- VAR(db_industry_mod6, 
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
    response = "Y_industry_bens_cons_dur",
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
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Bens de consumo duráveis',
       x = 'Meses após o choque',
       y = '')


# ________________________________________
# ________________________________________
# 7) Industria Geral + Bens de consumo não duráveis ----
## Data ----
db_industry_mod7 <- db_industry %>%
  dplyr::mutate(
    log_IPCA_A = log(IPCA_A),
    log_money_supply = log(money_supply),
    log_credito_sa = log(credito_sa),
    log_cambio = log(cambio)
  ) %>% # inflation, money supply, credit and FX rate in logs
  dplyr::select(
    date,
    `Indústria geral`,
    `Bens de consumo não duráveis`,
    log_IPCA_A,
    selic,
    log_money_supply,
    log_credito_sa,
    log_cambio
  ) %>% # ordering from least endogenous to more endogenous
  rename(Y_industry = `Indústria geral`,
         Y_industry_bens_cons_n_dur = `Bens de consumo não duráveis`)

db_industry_mod7 <- ts(db_industry_mod7[, -1], start = c(2002, 1), frequency = 12)

## Model ----
VARselect(db_industry_mod7, 
          lag.max = 12, 
          type = 'both') # 2 lags selected based on all criterias

model7 <- VAR(db_industry_mod7, 
              p = 2, 
              type = 'both') # estimates model with constant and trend

## Redidual diagnostics ----
roots(model7, modulus = TRUE) # AR must be < 1

ACtest(model7, h = 3, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(3), H0: no AC)
ACtest(model7, h = 4, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(4), H0: no AC) 
ACtest(model7, h = 5, univariate = FALSE) # LM test for error autocorrelation (alternative is a VAR(5), H0: no AC)

normality.test(model7, multivariate.only = TRUE) # Jarque-Bera normality test

arch.test(model7, multivariate.only = TRUE) # heteroskedasticity test

## Impulse response function ----
n_ahead <- 36

Y_IRF_mod7 <-
  irf(
    model7,
    impulse = "selic",
    response = "Y_industry_bens_cons_n_dur",
    n.ahead = n_ahead,
    boot = TRUE,
    ci = 0.95,
    runs = 250
  ) # calculates the impulse response values from an orthogonal shock in the monetary policy instrument

g7 <- tibble(
  IRF = Y_IRF_mod7$irf$selic,
  Lower = Y_IRF_mod7$Lower$selic,
  Upper = Y_IRF_mod7$Upper$selic
) %>%
  ggplot(aes(x = seq(0, n_ahead, 1))) +
  geom_line(aes(y = IRF), size = 1.3, color = "#1874CD") +
  geom_line(aes(y = Lower), color = 'red', linetype = "dashed") +
  geom_line(aes(y = Upper), color = 'red', linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              alpha = 0.2,
              fill = "#1874CD") +
  geom_hline(aes(yintercept = 0), color = "black") +
  labs(title = 'Bens de consumo não duráveis',
       x = 'Meses após o choque',
       y = '')


# ________________________________________
# ________________________________________
# Saving plots ----
layout_matrix <- matrix(c(1, 1, 1, 1,
                          2, 2, 3, 3, 
                          4, 4, 5, 5, 
                          6, 6, 7, 7), nrow = 4, byrow = TRUE)

grid <- grid.arrange(g1, g2, g3, g4, g5, g6, g7, layout_matrix = layout_matrix)
ggsave("IRF_industria.png", grid, width = 7.7, height = 9.9, units = "in", dpi = 700, path = "C:/Users/Mohammed/Desktop/TCC I/version_1")
