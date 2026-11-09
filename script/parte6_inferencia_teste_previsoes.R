# PARTE 6: inferencia final, comparacao no teste e previsoes
# Execute na raiz do projeto do grupo ou nesta pasta separada.
library(tidymodels)
library(reglin)
library(splines2)
library(kknn)

pasta_dados <- if (dir.exists("data")) "data" else "../AnaliseDeRegressao/data"
load(file.path(pasta_dados, "dados_preparados.RData"))
load(file.path(pasta_dados, "parte2_modelos_lineares.RData"))
load(file.path(pasta_dados, "parte3_multicolinearidade_polinomios.RData"))

# 1. ANOVA, intervalos dos coeficientes e efeitos percentuais
modelo_final <- modelo_stepwise
anova_final <- reglin::tab_anova(modelo_final)
ic_beta <- confint(modelo_final, level = 0.95)
coeficientes <- cbind(beta = coef(modelo_final), ic_beta)
print(coeficientes)
# Resposta log10(preco): efeito de +1 unidade, ou sim versus nao.
# O intercepto e excluido dessa interpretacao percentual.
efeitos_percentuais <- 100 * (10^coeficientes[-1, ] - 1)
print(efeitos_percentuais)

# 2. Workflows: mesmas formulas e escolhas feitas no treino
formula_completa <- formula(mod_completo)
espec_lm <- linear_reg() |> set_engine("lm")
wf_linear <- workflow() |>
  add_formula(formula_completa) |> add_model(espec_lm)
wf_stepwise <- workflow() |>
  add_formula(formula(modelo_final)) |> add_model(espec_lm)
receita_spline <- recipe(formula_completa, data = treino_casas) |>
  step_spline_b(area_lote, deg_free = melhor_df$deg_free[1], degree = 3) |>
  step_dummy(all_nominal_predictors())
wf_spline <- workflow() |>
  add_recipe(receita_spline) |> add_model(espec_lm)

# KNN: padronizacao em cada dobra; escolha de k somente no treino.
receita_knn <- recipe(formula_completa, data = treino_casas) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())
wf_knn <- workflow() |>
  add_recipe(receita_knn) |>
  add_model(nearest_neighbor(neighbors = tune(), weight_func = "rectangular",
                             dist_power = 2) |>
              set_engine("kknn", scale = FALSE) |> set_mode("regression"))
set.seed(606)
ajuste_knn <- tune_grid(wf_knn, resamples = cv_folds,
                       grid = data.frame(neighbors = seq(3L, 51L, 2L)),
                       metrics = metric_set(rmse, rsq))
melhor_knn <- select_by_one_std_err(ajuste_knn, desc(neighbors), metric = "rmse")
wf_knn_final <- finalize_workflow(wf_knn, melhor_knn)
print(melhor_knn)

# 3. Avaliacao final: os quatro modelos usam o mesmo teste reservado
workflows_finais <- list(Linear = wf_linear, Stepwise = wf_stepwise,
                         `B-Spline` = wf_spline, KNN = wf_knn_final)
set.seed(607)
resultados_teste <- lapply(workflows_finais, function(wf) {
  last_fit(wf, split = split_casas, metrics = metric_set(rmse, rsq, rsq_trad))
})
comparacao <- imap_dfr(resultados_teste, function(x, nome) {
  collect_metrics(x) |> mutate(Modelo = nome)
}) |>
  select(Modelo, .metric, .estimate) |>
  pivot_wider(names_from = .metric, values_from = .estimate) |>
  arrange(rmse)
print(comparacao)
# rmse: erro em log10(CAD); rsq: correlacao ao quadrado;
# rsq_trad: 1 - SQ dos erros / SQ total no teste (pode ser negativo).

predicoes_teste <- imap_dfr(resultados_teste, function(x, nome) {
  collect_predictions(x) |> mutate(Modelo = nome)
})
grafico_teste <- ggplot(predicoes_teste, aes(preco_log, .pred)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(alpha = 0.65, color = "steelblue") +
  facet_wrap(~Modelo, ncol = 2) + coord_equal() + theme_minimal() +
  labs(x = "Observado: log10(CAD)", y = "Previsto: log10(CAD)")
print(grafico_teste)

# Comparacao complementar em CAD: inversao direta, sem correcao da media.
metricas_cad <- predicoes_teste |>
  group_by(Modelo) |>
  summarise(RMSE_CAD = rmse_vec(10^preco_log, 10^.pred),
            R2_pred = rsq_trad_vec(10^preco_log, 10^.pred), .groups = "drop") |>
  arrange(RMSE_CAD)
print(metricas_cad)

# 4. Cenarios: variacao de area, banheiros e bairro; demais valores fixos
cenarios <- expand.grid(
  area_lote = c(300, 600), banheiros = c(1, 2),
  area_preferencial = levels(treino_casas$area_preferencial),
  quartos = 3, andares = 2, vagas_garagem = 1,
  entrada_carro = "sim", porao = "sim",
  area_recreacao = levels(treino_casas$area_recreacao)[1],
  aquecimento_gas = levels(treino_casas$aquecimento_gas)[1],
  ar_condicionado = levels(treino_casas$ar_condicionado)[1],
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
categoricas <- names(treino_casas)[vapply(treino_casas, is.factor, logical(1))]
for (v in categoricas) {
  cenarios[[v]] <- factor(cenarios[[v]], levels = levels(treino_casas[[v]]))
}
cenarios$id <- paste0("C", seq_len(nrow(cenarios)))
ic_log <- predict(modelo_final, cenarios, interval = "confidence", level = 0.95)
ip_log <- predict(modelo_final, cenarios, interval = "prediction", level = 0.95)
print(cbind(cenarios[, c("id", "area_lote", "banheiros", "area_preferencial")], ic_log))
# IC acima: media de log10(preco). 10^IC e para media geometrica, nao aritmetica.
# 10^IP fornece o intervalo para o preco de uma nova casa, sob o modelo.

# 5. Media aritmetica em CAD: correcao lognormal e IC por bootstrap
# Mantido para distinguir a media monetaria da media geometrica.
# Depende de erros normais/homocedasticos; nao inclui a selecao Stepwise.
X <- model.matrix(modelo_final)
Xnovo <- model.matrix(delete.response(terms(modelo_final)), cenarios,
                     contrasts.arg = modelo_final$contrasts, xlev = modelo_final$xlevels)
a <- log(10)
media_cad <- exp(a * ic_log[, "fit"] + 0.5 * a^2 * sigma(modelo_final)^2)
set.seed(608)
medias_boot <- replicate(2000, {
  yb <- fitted(modelo_final) + rnorm(nrow(X), sd = sigma(modelo_final))
  mb <- lm.fit(X, yb)
  s2b <- sum(mb$residuals^2) / (nrow(X) - mb$rank)
  exp(a * drop(Xnovo %*% mb$coefficients) + 0.5 * a^2 * s2b)
})
ic_media_cad <- t(apply(medias_boot, 1, quantile, probs = c(0.025, 0.975)))
previsoes_cenarios <- data.frame(
  cenarios, mediana_cad = 10^ic_log[, "fit"], media_cad,
  ic_media_inf = ic_media_cad[, 1], ic_media_sup = ic_media_cad[, 2],
  ip_nova_inf = 10^ip_log[, "lwr"], ip_nova_sup = 10^ip_log[, "upr"]
)
print(previsoes_cenarios[, c("id", "media_cad", "ic_media_inf", "ic_media_sup",
                             "ip_nova_inf", "ip_nova_sup")])
# Interpretacoes ficam no Quarto. Nenhum arquivo da equipe e sobrescrito.

