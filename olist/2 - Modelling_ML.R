# Carregando pacotes necessários ----
source('util/pacotes_necessarios.R')

base_model <- readRDS('data/base_model.rds')

# Algoritmo: Regressão Logística
# Estratégia de Checagem de Qualidade: Simples Holdout Set (amostra de teste)

set.seed(1234)

n_treino = floor(0.7 * nrow(base_model))
train_ind = sample(seq_len(nrow(base_model)), size = n_treino)

base_treino <- base_model[train_ind,]
base_teste <- base_model[-train_ind,]

cat('n Base Treino: ', nrow(base_treino), '\n',
    'n Base Teste: ', nrow(base_teste), '\n', 
    'n Base Total: ', nrow(base_model))

# Originalmente, o modelo daria problema se tivesse o -Infinito no logaritmo do Frete sobre Preço
# Lembre-se do Fluxo de iteração entre modelagem e transformação
reg_logistica <- glm(review_alto_numerico ~ ., 
                     data = base_treino, 
                     family = binomial(link = 'logit'))

# saveRDS(reg_logistica, 'data/models/reg_logistica.rds')

summary(reg_logistica)

p <- predict(reg_logistica, base_teste, type = "response")

base_teste %>% 
  mutate(probabilidade_predita = p) %>% 
  View()

# Checando AUC e plotando a curva ROC
PRROC_obj <- roc.curve(scores.class0 = p, 
                       weights.class0 = base_teste$review_alto_numerico,
                       curve = TRUE)
plot(PRROC_obj)
auc_calculada <- PRROC_obj$auc

# Gráfico de Distribuição Acumulada entre os grupos
aux <- tibble(prob_predita = p, group = as.factor(base_teste$review_alto_numerico))
ggplot(aux, aes(x = prob_predita, col = as.factor(group))) +
  stat_ecdf(geom = "step") +
  ggtitle(paste0('AUC = ', round(auc_calculada, 4)))

# Modelo com baixo poder discriminativo


# Vamos ver se a gente consegue melhorar ele com alguma outra estratégia.

# Algoritmo: Diversos com AutoML do H2O (restringindo para somente modelos baseados em árvores)
# Estratégia de Validação: K-Fold Cross_validation
# Estratégia de Checagem de Qualidade: o mesmo Holdout Set de antes
# Fonte: https://towardsdatascience.com/a-minimal-example-combining-h2os-automl-and-shapley-s-decomposition-in-r-ba4481282c3c

h2o.init()

df_treino <- base_treino %>% 
  rename(y = review_alto_numerico) %>% 
  mutate(y = as.factor(y)) %>% 
  mutate_if(is.character, factor)

df_teste <- base_teste %>% 
  rename(y = review_alto_numerico) %>% 
  mutate(y = as.factor(y)) %>% 
  mutate_if(is.character, factor)

df_frame_treino <- as.h2o(df_treino)
df_frame_teste <- as.h2o(df_teste)

automl_model <- h2o.automl( 
  y = 'y',
  balance_classes = TRUE,
  training_frame = df_frame_treino,
  nfolds = 4,
  max_runtime_secs = 60 * 1, # Tempo Máximo de k minutos: 60 * k
  #include_algos = c('DRF', 'GBM', 'XGBoost'),
  exclude_algos = "StackedEnsemble", # Importância Global de Modelos Stacked não são triviais
  sort_metric = "AUC",
  seed = 1234)

lb <- as.data.frame(automl_model@leaderboard)
aml_leader <- automl_model@leader

#h2o.saveModel(object = aml_leader,
#              path = 'data/models',
#              force = TRUE)

# Melhorou a métrica AUC no Holdout set?
pred <- h2o.predict(object = aml_leader, newdata = df_frame_teste) %>% 
  as.data.frame() %>% 
  pull(p1)

# Checando AUC e plotando a curva ROC
PRROC_obj <- roc.curve(scores.class0 = pred, 
                       weights.class0 = base_teste$review_alto_numerico,
                       curve = TRUE)
plot(PRROC_obj)

# Gráfico de Importância de Variância
h2o.varimp_plot(aml_leader)

# Dependência Parcial com relação a algumas variáveis
h2o.partialPlot(object = aml_leader, 
                data = df_frame_treino, 
                cols = "dias_antecipacao_na_entrega")

h2o.partialPlot(object = aml_leader, 
                data = df_frame_treino, 
                cols = "log_frete_sobre_preco_desloc")

h2o.partialPlot(object = aml_leader, 
                data = df_frame_treino, 
                cols = "payment_type")
