# PARTE 2: Modelos Lineares Múltiplos, Somas de Quadrados Extra e Stepwise

# Carrega os pacotes necessários
library(tidyverse)
library(reglin)
library(car)
library(broom)

# Carrega os dados preparados e já divididos em treino e teste
load("data/dados_preparados.RData")

# Ajuste do modelo linear múltiplo
# Utiliza todas as variáveis disponíveis no conjunto de treinamento para prever preco_log (usado aqui para estabilizar a variância, de acordo com o teste de box-cov feito antes)
mod_completo <- lm(
  preco_log ~ area_lote + quartos + banheiros + andares + 
    entrada_carro + area_recreacao + porao + aquecimento_gas + 
    ar_condicionado + vagas_garagem + area_preferencial, 
  data = treino_casas
)

# Resumo do modelo (teste t individuais e R2 ajustado)
summary(mod_completo)

# Tabela ANOVA (teste f)
reglin::tab_anova(mod_completo)

# Somas de Quadrados Extra
# Aqui, fazemos o teste para ver se é possível simplificar o modelo agrupando variáveis por características

# Removendo características internas da casa
mod_sem_internas <- update(mod_completo, . ~ . - quartos - banheiros - aquecimento_gas - ar_condicionado)
anova(mod_sem_internas, mod_completo)

# Removendo características estruturais da casa e vagas na garagem
mod_sem_estruturas <- update(mod_completo, . ~ . - andares - entrada_carro - area_recreacao - porao - vagas_garagem)
anova(mod_sem_estruturas, mod_completo)

# Removendo a variável de área de preferência
mod_sem_preferencia <- update(mod_completo, . ~ . - area_preferencial)
anova(mod_sem_preferencia, mod_completo)

# Seleção automática de variáveis (Stepwise)
mod_nulo <- lm(preco_log ~ 1, data = treino_casas)

# Executa o algoritmo de Stepwise
step_both <- stats::step(mod_completo, direction = "both", scope = list(lower = mod_nulo, upper = mod_completo), trace = FALSE)
step_backward <- stats::step(mod_completo, direction = "backward", scope = list(lower = mod_nulo), trace = FALSE)
step_forward <- stats::step(mod_nulo, direction = "forward", scope = list(upper = mod_completo), trace = FALSE)

summary(step_both)
summary(step_backward)
summary(step_forward)

# Comparação de Modelos (AIC e PRESS)
tab_comparativa <- data.frame(
  Modelo = c("Completo", "Sem Internas", "Sem Estruturas e Vagas", "Sem Preferência", "Stepwise (Both)"),
  AIC = c(AIC(mod_completo), AIC(mod_sem_internas), AIC(mod_sem_estruturas), AIC(mod_sem_preferencia), AIC(step_both)),
  PRESS = c(reglin::press(mod_completo), reglin::press(mod_sem_internas), 
            reglin::press(mod_sem_estruturas), reglin::press(mod_sem_preferencia), 
            reglin::press(step_both)),
  R2_adj = c(summary(mod_completo)$adj.r.squared, 
             summary(mod_sem_internas)$adj.r.squared,
             summary(mod_sem_estruturas)$adj.r.squared, 
             summary(mod_sem_preferencia)$adj.r.squared,
             summary(step_both)$adj.r.squared)
)

print(tab_comparativa)

# Salva os objetos usados nessa parte para os colegas
modelo_stepwise <- step_both
save(mod_completo, mod_sem_internas, mod_sem_estruturas, mod_sem_preferencia, 
     modelo_stepwise, tab_comparativa, 
     file = "data/parte2_modelos_lineares.RData")