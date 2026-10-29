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

#conferindo tratamento das variaveis categoricas
X_exemplo <- model.matrix(~ ., data = head(dados, 5))
print("Matriz de desenho")
print(head(X_exemplo))
