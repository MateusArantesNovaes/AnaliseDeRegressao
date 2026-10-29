library(tidyverse)
library(tidymodels)
library(car)
library(GGally)

dados <- read_csv("data/tb-reg-lin-casas.csv")

dados <- dados %>%
  mutate(
    across(c(entrada_carro, area_recreacao, porao, aquecimento_gas, 
             ar_condicionado, area_preferencial), as.factor)
  )

modelo_preliminar <- lm(preco ~ ., data = dados)
boxcox_teste <- car::powerTransform(modelo_preliminar)

summary(boxcox_teste)

dados <- dados %>%
  mutate(preco_log = log10(preco))