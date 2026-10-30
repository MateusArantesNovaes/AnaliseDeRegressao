library(tidyverse)
library(tidymodels)
library(car)
library(GGally)

dados <- read_csv("data/tb-reg-lin-casas.csv")

# Trato as variaveis categoricas
dados <- dados %>%
  mutate(
    across(c(entrada_carro, area_recreacao, porao, aquecimento_gas, 
             ar_condicionado, area_preferencial), as.factor)
  )

# Ajustei o modelo preliminar p/ que o car::powerTransform sugira o lambda ideal
modelo_preliminar <- lm(preco ~ ., data = dados)
boxcox_teste <- car::powerTransform(modelo_preliminar)

# resultado do antes anterior
summary(boxcox_teste)

# Criei a variavel resposta seguindo teste box-cox, criando log na base 10 do preco
dados <- dados %>%
  mutate(preco_log = log10(preco))

set.seed(123)

# Dividi os dados (Split 80/20) e Reamostragem (10-fold CV)
split_casas <- initial_split(dados, prop = 0.80, strata = preco)
treino_casas <- training(split_casas)
teste_casas  <- testing(split_casas)

# Criei 10 dobras (folds) de validação cruzada baseadas no conjunto de treino
cv_folds <- vfold_cv(treino_casas, v = 10, strata = preco)

# Exportando o conjunto de dados tratados e preparados para usarmos
save(dados, split_casas, treino_casas, teste_casas, cv_folds, 
     file = "data/dados_preparados.RData")