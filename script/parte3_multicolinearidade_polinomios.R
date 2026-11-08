# PARTE 3: Multicolinearidade, Regressão Polinomial, B-Splines e Padronização

# Bibliotecas
library(tidyverse)
library(tidymodels)
library(reglin)
library(car)
library(splines2)

# Carrega os dados preparados e os modelos ajustados
load("data/dados_preparados.RData")
load("data/parte2_modelos_lineares.RData")

# Seleciona o modelo para análise (o stepwise manteve todas as variáveis)
modelo <- mod_completo
n <- nrow(treino_casas)

# Parte 3.1: Multicolinearidade

# VIF de cada variável
vif_completo <- car::vif(modelo)
vif_completo
any(vif_completo > 10)

# GVIF tratando andares e vagas_garagem como fator
mod_fatores <- update(modelo, . ~ . - andares - vagas_garagem + factor(andares) + factor(vagas_garagem))
gvif_tab <- car::vif(mod_fatores)
gvif_tab

# Compara GVIF^(1/(2*Df)) com o limiar sqrt(10)
gvif_tab[, "GVIF^(1/(2*Df))"] > sqrt(10)

# Variância dos estimadores: V(beta_j) = sigma^2 * C_jj
X <- model.matrix(modelo)
C <- solve(t(X) %*% X)
all.equal(sigma(modelo)^2 * diag(C), diag(vcov(modelo)))

# Número de condição da matriz X
kappa(X, exact = TRUE)

# Parte 3.2: Regressão Polinomial

# Termo quadrático em area_lote
mod_quad <- update(modelo, . ~ . + I(area_lote^2))
summary(mod_quad)
anova(modelo, mod_quad)

# Modelos de grau 1 a 6 (raw = TRUE para manter a interpretação dos coeficientes)
mod_grau1 <- modelo
mod_grau2 <- update(modelo, . ~ . - area_lote + poly(area_lote, degree = 2, raw = TRUE))
mod_grau3 <- update(modelo, . ~ . - area_lote + poly(area_lote, degree = 3, raw = TRUE))
mod_grau4 <- update(modelo, . ~ . - area_lote + poly(area_lote, degree = 4, raw = TRUE))
mod_grau5 <- update(modelo, . ~ . - area_lote + poly(area_lote, degree = 5, raw = TRUE))
mod_grau6 <- update(modelo, . ~ . - area_lote + poly(area_lote, degree = 6, raw = TRUE))

modelos_grau <- list(mod_grau1, mod_grau2, mod_grau3, mod_grau4, mod_grau5, mod_grau6)

# Tabela de sensibilidade do grau
tab_grau <- data.frame(
  Grau = 1:6,
  AIC = sapply(modelos_grau, AIC),
  PRESS = sapply(modelos_grau, reglin::press),
  R2_adj = sapply(modelos_grau, function(m) summary(m)$adj.r.squared),
  Det_XtX = sapply(modelos_grau, function(m) det(crossprod(model.matrix(m)))),
  Kappa = sapply(modelos_grau, function(m) kappa(model.matrix(m), exact = TRUE)),
  Coef_NA = sapply(modelos_grau, function(m) sum(is.na(coef(m))))
)

print(tab_grau)

# Teste sequencial entre os graus
anova(mod_grau1, mod_grau2, mod_grau3, mod_grau4)

# Correlação entre area_lote e area_lote^2 (multicolinearidade do polinômio)
car::vif(mod_quad)
cor(treino_casas$area_lote, treino_casas$area_lote^2)

# Centralizando area_lote para reduzir essa correlação
treino_casas <- treino_casas %>% mutate(area_c = area_lote - mean(area_lote))
mod_quad_c <- update(modelo, . ~ . - area_lote + area_c + I(area_c^2), data = treino_casas)
car::vif(mod_quad_c)

# Transformação logarítmica em area_lote como alternativa
mod_logarea <- update(modelo, . ~ . - area_lote + log10(area_lote))
summary(mod_logarea)

# Escolhe o grau com menor PRESS
grau_otimo <- which.min(tab_grau$PRESS)
modelo_polinomial <- modelos_grau[[grau_otimo]]
summary(modelo_polinomial)

# Resíduos parciais (ggresiduals não aceita poly(), então usamos car::crPlots)
car::crPlots(modelo_polinomial)

# Parte 3.3: B-Splines com validação cruzada

receita_spline <- recipe(
  preco_log ~ area_lote + quartos + banheiros + andares + entrada_carro +
    area_recreacao + porao + aquecimento_gas + ar_condicionado +
    vagas_garagem + area_preferencial,
  data = treino_casas
) %>%
  step_spline_b(area_lote, deg_free = tune(), degree = 3) %>%
  step_dummy(all_nominal_predictors())

wf_spline <- workflow() %>%
  add_recipe(receita_spline) %>%
  add_model(linear_reg() %>% set_engine("lm"))

# Testa de 3 a 12 graus de liberdade nos 10 folds
set.seed(123)
tune_spline <- tune_grid(
  wf_spline,
  resamples = cv_folds,
  grid = data.frame(deg_free = 3:12),
  metrics = metric_set(rmse, rsq)
)

autoplot(tune_spline)
show_best(tune_spline, metric = "rmse")

# Escolhe o modelo mais simples dentro de 1 erro-padrão do melhor
melhor_df <- select_by_one_std_err(tune_spline, deg_free, metric = "rmse")
melhor_df

wf_spline_final <- finalize_workflow(wf_spline, melhor_df)
fit_spline_tm <- fit(wf_spline_final, data = treino_casas)

# Ajusta o mesmo modelo com lm para comparar com os demais
df_spline <- melhor_df$deg_free
modelo_spline <- update(modelo, . ~ . - area_lote + bSpline(area_lote, df = df_spline, degree = 3))
summary(modelo_spline)

# Comparação dos tratamentos de area_lote
tab_area <- data.frame(
  Modelo = c("Linear", "Polinomial", "Log de area_lote", "B-Spline"),
  AIC = c(AIC(modelo), AIC(modelo_polinomial), AIC(mod_logarea), AIC(modelo_spline)),
  PRESS = c(reglin::press(modelo), reglin::press(modelo_polinomial),
            reglin::press(mod_logarea), reglin::press(modelo_spline)),
  R2_adj = c(summary(modelo)$adj.r.squared, summary(modelo_polinomial)$adj.r.squared,
             summary(mod_logarea)$adj.r.squared, summary(modelo_spline)$adj.r.squared)
)

print(tab_area)

# Parte 3.4: Padronização de Variáveis

# Matriz das covariáveis sem o intercepto
Xs <- model.matrix(modelo)[, -1]
y <- treino_casas$preco_log

# Escala normal unitária (Z) e comprimento unitário (W)
Z <- apply(Xs, 2, standardization, type = "normal")
W <- apply(Xs, 2, standardization, type = "length")
yZ <- standardization(y, type = "normal")
yW <- standardization(y, type = "length")

# Modelos padronizados sem intercepto
mod_pad_normal <- lm(yZ ~ 0 + Z)
mod_pad_length <- lm(yW ~ 0 + W)

coef(mod_pad_normal)
coef(mod_pad_length)

# Verifica que (n - 1) W'W = Z'Z
all.equal((n - 1) * crossprod(W), crossprod(Z))

# Ranking da importância das variáveis
b_pad <- coef(mod_pad_normal)
names(b_pad) <- colnames(Xs)

ranking <- data.frame(Variavel = names(b_pad), Coef_Padronizado = b_pad) %>%
  arrange(desc(abs(Coef_Padronizado)))

print(ranking)

ggplot(ranking, aes(x = reorder(Variavel, abs(Coef_Padronizado)), y = Coef_Padronizado)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Coeficiente padronizado")

# Parte 3.5: Salva os resultados obtidos
save(vif_completo, gvif_tab, tab_grau, modelo_polinomial, mod_logarea, tune_spline,
     melhor_df, modelo_spline, tab_area, mod_pad_normal, ranking,
     file = "data/parte3_multicolinearidade_polinomios.RData")
