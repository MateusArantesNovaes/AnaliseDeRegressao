# PARTE 5: Alavancagem, Outliers, Medidas de Influência e Sensibilidade

# Bibliotecas
library(tidyverse)
library(reglin)
library(car)

# Carrega os dados preparados e os modelos ajustados
load("data/dados_preparados.RData")
load("data/parte2_modelos_lineares.RData")

# Seleciona o mesmo modelo usado na análise de resíduos
modelo <- modelo_stepwise
summary(modelo)

n <- nobs(modelo)
p <- length(coef(modelo))

# Parte 5.1: Medidas de Influência

infl <- influence.measures(modelo)
summary(infl)

h <- hatvalues(modelo)
dfb <- dfbetas(modelo)
dff <- dffits(modelo)
cook <- cooks.distance(modelo)
covr <- covratio(modelo)

# Parte 5.2: Análise Gráfica com os Limiares

limiares <- c(
  leverage = 3 * p / n,
  dfbetas = 1,
  dffits = 3 * sqrt(p / (n - p)),
  cooksd = qf(0.5, p, n - p),
  covratio = 3 * p / (n - p)
)
limiares

gginfluence(modelo, measure = "leverage")
gginfluence(modelo, measure = "dfbetas")
gginfluence(modelo, measure = "cooksd")
gginfluence(modelo, measure = "dffits")
gginfluence(modelo, measure = "covratio")

# Marca as observações que passam de cada limiar
diagnostico <- data.frame(
  obs = 1:n,
  preco = treino_casas$preco,
  area_lote = treino_casas$area_lote,
  alavanca = h > limiares["leverage"],
  dfbetas = apply(abs(dfb), 1, max) > limiares["dfbetas"],
  dffits = abs(dff) > limiares["dffits"],
  cook = cook > limiares["cooksd"],
  covratio = abs(1 - covr) > limiares["covratio"]
)
diagnostico$n_criterios <- rowSums(diagnostico[, c("alavanca", "dfbetas", "dffits", "cook", "covratio")])

# Quantidade de casas marcadas por critério
colSums(diagnostico[, c("alavanca", "dfbetas", "dffits", "cook", "covratio")])

# Teste de outlier de Bonferroni
car::outlierTest(modelo)

# Casas marcadas por 2 ou mais critérios
atipicas <- diagnostico %>%
  filter(n_criterios >= 2) %>%
  arrange(desc(n_criterios))
atipicas

# Características dessas casas
treino_casas[atipicas$obs, ]
summary(treino_casas[, c("preco", "area_lote", "quartos", "banheiros", "andares", "vagas_garagem")])

# Parte 5.3: Estudo de Sensibilidade

# Ajusta o modelo sem as casas atípicas
modelo_sem <- update(modelo, data = treino_casas[-atipicas$obs, ])

# Variação relativa dos coeficientes: VR = 100 * (beta_sem - beta_orig) / |beta_orig|
tab_sensibilidade <- data.frame(
  Original = coef(modelo),
  Sem_Atipicas = coef(modelo_sem),
  VR = 100 * (coef(modelo_sem) - coef(modelo)) / abs(coef(modelo)),
  p_Original = summary(modelo)$coefficients[, 4],
  p_Sem_Atipicas = summary(modelo_sem)$coefficients[, 4]
)

print(tab_sensibilidade)

# Compara as medidas globais dos dois modelos
data.frame(
  Modelo = c("Com todas as casas", "Sem as casas atípicas"),
  n = c(nobs(modelo), nobs(modelo_sem)),
  Sigma = c(sigma(modelo), sigma(modelo_sem)),
  R2_adj = c(summary(modelo)$adj.r.squared, summary(modelo_sem)$adj.r.squared)
)

# Parte 5.4: Salva os resultados obtidos
save(infl, limiares, diagnostico, atipicas, modelo_sem, tab_sensibilidade,
     file = "data/parte5_influencia_sensibilidade.RData")
