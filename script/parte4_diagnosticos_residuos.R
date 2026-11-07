# PARTE 4: Análise de Resíduos, Gráficos Parciais e Ponderação

# Bibliotecas
library(reglin)

# Carrega os dados preparados e os modelos ajustados
load("data/dados_preparados.RData")
load("data/parte2_modelos_lineares.RData")

# Seleciona e confere o modelo para análise
modelo <- modelo_stepwise
summary(modelo)

# Parte 4.1: Tipos de Resíduos

# Resíduos Ordinários
residuos <- residuals(modelo)

# Resíduos Estudentizados Internamente
residuos_ei <- rstandard(modelo)

# Resíduos Estudentizados Externamente
residuos_ee <- rstudent(modelo)

summary(residuos)
summary(residuos_ei)
summary(residuos_ee)

# Parte 4.2: Análise Gráfica dos Resíduos
ggresiduals(modelo)
testResiduals(modelo)

# Parte 4.3: Gráficos de Diagnóstico Parcial

# Added-variable plots - avPlots
ggresiduals(modelo, type = "avPlots")

# Component-plus-residual plots - crPlots
ggresiduals(modelo, type = "crPlots")

# Adequação de area_lote
modelo_area_lote_corrigido <- update(modelo, . ~ . - area_lote + poly(area_lote, degree = 2, raw = FALSE))
car::crPlots(modelo_area_lote_corrigido, terms = ~ poly(area_lote, degree = 2, raw = FALSE))

# Parte 4.4: Comparação preço original x log

modelo_original <- update(modelo, preco ~ .)
modelo_log <- modelo

formula(modelo_original)
formula(modelo_log)

# Análise Gráfica de Cada Modelo
ggresiduals(modelo_original)
ggresiduals(modelo_log)

testResiduals(modelo_original)
testResiduals(modelo_log)

# Parte 4.5: Salva os resultados obtidos
save(modelo, modelo_original, modelo_log, residuos, residuos_ei, residuos_ee, 
     file = "data/parte4_diagnosticos_residuos.RData")