# Carregando pacotes necessários ----
source('util/pacotes_necessarios.R')

# Carrega algumas funções úteis para plotagem exploratória ----
source('util/funcoes_plot.R')


# No sistema da Olist cada pedido é designado a um unique customerid. 
# Isso significa que cada consumidor terá diferentes ids para diferentes pedidos. 
# O propósito de ter um customerunique_id na base é permitir identificar consumidores que fizeram recompras na loja. 
# Caso contrário, você encontraria que cada ordem sempre tivesse diferentes consumidores associados.

# customer_unique_id é único por pessoa (como se fosse um "CPF")
# n_distinct(clientes$customer_id) == n_distinct(orders$order_id)
# Tanto o "customer_id" quanto o "order_id" são únicos por compra

# Import ----
clientes <- read_csv('data/olist_customers_dataset.csv')
orders <- read_csv('data/olist_orders_dataset.csv')
reviews <- read_csv('data/olist_order_reviews_dataset.csv')
pagamentos <- read_csv('data/olist_order_payments_dataset.csv')
produtos <- read_csv('data/olist_products_dataset.csv')
vendedores <- read_csv('data/olist_sellers_dataset.csv')
categorias <- read_csv('data/product_category_name_translation.csv')
geolocalizacao <- read_csv('data/olist_geolocation_dataset.csv')
order_items <- read_csv('data/olist_order_items_dataset.csv')

# Explicação do pipe (%>%):

# pegue isso então faça isso, então faça essa outra coisa
# pegue isso %>% faça isso %>% faça essa outra coisa

# Juntando Dados ("Tidying") ----
base_completa <- clientes %>% 
  left_join(orders) %>% 
  left_join(order_items) %>% # Uma ordem pode ter muito itens (por isso a base expande aqui)
  left_join(reviews) %>% 
  left_join(pagamentos) %>% 
  left_join(produtos) %>%
  left_join(vendedores) %>% 
  left_join(categorias) %>% 
  left_join(geolocalizacao %>% distinct(geolocation_zip_code_prefix), by = c('customer_zip_code_prefix' = 'geolocation_zip_code_prefix'))

# Lembrando que a análise por item expande a base, mas a maoria dos pedidos é de um item somente.

order_items %>% 
  count(order_item_id) %>% # Número de itens do mesmo pedido
  mutate(prop = n / sum(n)) # 87% dos pedidos tem só um item

# IMPORTANTE: existem duplicidades: 
# reviews %>% janitor::get_dupes(order_id)
# reviews %>% janitor::get_dupes(review_id)
# order_items %>% janitor::get_dupes(order_id)

# Além disso, geolocalizacao %>% distinct() instabilidade com relação aos distintos. Por exemplo, zip_code_prefix 01046, possui dois valores de latitude

# Para simplificar, vamos limitar nossa análise somente para pedidos com um item
base_completa <- base_completa %>% 
  filter(order_item_id == 1)

# Verifica qtd. de linhas e colunas e estrutura das colunas
glimpse(base_completa)

View(base_completa) # Visualiza base similarmente a uma planilha excel


# Transform ----

# Nesta etapa, após a análise inicial da estrutura dos dados, criamos variáveis que serão úteis na análise.
# Criação de uma variável de review_alto: "1" se score do review for 5, e "0" caso contrário. Não existem valores faltantes nessa variável sum(is.na(base_completa$review_score)) 
# Criação de uma variável de diferença entrega prazo de entrega dado para o cliente e data de entrega propriamente dita.
# Criação de uma variável que relaciona valores de Frete e Preço.
# Criação de uma variável de frete gratuito
# Reduzimos o escopo da análise somente para entregas concluídas (order_status == 'delivered').

base_analise <- base_completa %>% 
  mutate(review_alto = ifelse(review_score == 5, 'Nota Máxima', 'Menor que 5'),
         review_alto_numerico = ifelse(review_score == 5, 1, 0), # É útil termos a variável dependente como numérica para algumas análises exploratórias
         order_delivered_customer_date = as.Date(order_delivered_customer_date),
         order_estimated_delivery_date = as.Date(order_estimated_delivery_date),
         dias_antecipacao_na_entrega = order_estimated_delivery_date - order_delivered_customer_date,
         atrasou = ifelse(dias_antecipacao_na_entrega < 0, 1, 0),
         frete_sobre_preco = freight_value / price,
         frete_gratuito = ifelse(freight_value == 0, 1, 0)) %>% 
  select(customer_state, 
         order_status, 
         price, 
         freight_value, 
         payment_type, 
         payment_value, 
         product_category_name,
         product_photos_qty,
         review_score,
         review_alto,
         review_alto_numerico,
         dias_antecipacao_na_entrega,
         atrasou,
         order_estimated_delivery_date,
         order_delivered_customer_date,
         review_comment_title,
         review_comment_message,
         frete_sobre_preco,
         frete_gratuito,
         customer_city,
         seller_city) %>% 
  filter(order_status == 'delivered')

# Verificando quantidade de missing por coluna e ordenando
map_df(base_analise, ~sum(is.na(.))) %>% 
  gather() %>% 
  arrange(desc(value))

sum(is.na(base_analise$atrasou)) # Opa, temos alguns Missings na variável 'atrasou'! Porque temos algumas linhas que não tem data de entrega.

# Retirando esses casos da Base
base_analise <- base_analise %>% 
  drop_na(atrasou)

# Como é a distribuição de Missings dos nossos Dados?

# Visualização PARCIAL (20% da base) de missings
set.seed(1234)
vis_dat(base_analise %>% 
          sample_frac(0.2))
vis_miss(base_analise %>% 
          sample_frac(0.2))


# Exploração e Visualização ----

# Antes, vale a pena olhar no Grammar of Graphics:
# https://vita.had.co.nz/papers/layered-grammar.html


# Algumas Análises Univariadas ----

# Variável Dependente: Review Score e Review Alto
base_analise %>% 
  count(review_score) %>% 
  mutate(prop = n / sum(n))

base_analise %>% 
  count(review_score) %>% 
  ggplot(aes(x = review_score, y = n)) +
  geom_bar(stat ='identity')

base_analise %>% 
  count(review_alto) %>% 
  mutate(prop = n / sum(n))

base_analise %>% 
  count(review_alto) %>% 
  ggplot(aes(x = review_alto, y = n)) +
  geom_bar(stat ='identity')

# Estado

base_analise %>% 
  count(customer_state, sort = T) %>% 
  mutate(prop = n / sum(n))

# payment_type

base_analise %>% 
  count(payment_type, sort = T) %>% 
  mutate(prop = n / sum(n))

# product_category_name

base_analise %>% 
  count(product_category_name, sort = T) %>% 
  mutate(prop = n / sum(n))

# Preço

summary(base_analise$price)

base_analise %>% 
  ggplot(aes(y = price)) +
  geom_boxplot() # Boxplot com diversos outliers

base_analise %>% 
  ggplot(aes(x = price)) +
  geom_density()
# Preço é extremamente assimétrico! Temos outliers com preços acima de R$6000
# O mais comum é transformar essa variável para estabilizá-la se é desejável incluí-la na modelagem

base_analise %>% 
  ggplot(aes(x = log(price))) +
  geom_density() +
  ggtitle('Variável de Preço com Transformação Logarítmica')

base_analise %>% 
  ggplot(aes(y = log(price))) +
  geom_boxplot() +
  ggtitle('Boxplot de preço mais estável com a Transformação Logarítmica')

# Nota: Boxplot pode ser problemático por não refletir a distribuição dos dados. Veremos adiante um exemplo melhor.

# Frete

summary(base_analise$freight_value)

base_analise %>% 
  ggplot(aes(x = freight_value)) +
  geom_density()

base_analise %>% 
  ggplot(aes(x = log(freight_value))) +
  geom_density() +
  ggtitle('Variável de Frete com Transformação Logarítmica')

# A variável de Frete parece ser mais problemática, pois tem valores 0 e alguns saltos
# Como eu poderia identificar rapidamente aonde estão esses pontos de corte???
# Com interatividade gráfica!

(base_analise %>% 
  filter(freight_value > 0) %>% # Tira os fretes gratuitos
  ggplot(aes(x = freight_value)) +
  geom_density()) %>% ggplotly()

# O ponto no entorno do 10 parece um ponto de atenção
# Temos muitos valores de frete repetidos.
# Talvez seria interessante transformar essa variável em categórica (ou somar uma constante).

# IMPORTANTE:
# Além disso, o frete deve ter relação com a distância entre customer e seller, 
# isso poderia ser levado em consideração fazendo algum tipo de manipulação de 
# localização (que está por zipcode). 
# Os dados permitiriam fazer essa análise de maneira fácil?

# Frete sobre preço

summary(base_analise$frete_sobre_preco)

base_analise %>% 
  ggplot(aes(x = frete_sobre_preco)) +
  geom_density()

base_analise %>% 
  ggplot(aes(x = log(frete_sobre_preco))) +
  geom_density()

# Importante: o logaritmo do Frete, quando ele é zero, resulta em -Infinito!

# Frete vs. Preço (neste caso, é uma análise bivariada)

base_analise %>% 
  sample_frac(0.2) %>% # Para reduzir o custo computacional
  ggplot(aes(x = log(price),
             y = log(freight_value))) +
  geom_point() +
  geom_smooth() # Mostrar tendência crescente

# Frete Gratuito

base_analise %>% 
  count(frete_gratuito, sort = T) %>% 
  mutate(prop = n / sum(n))
# Qauntidade Irrisória, não vale a pena modelar

# Dias de Antecipação na entrega e Atrasou

summary(as.numeric(base_analise$dias_antecipacao_na_entrega))

base_analise %>% 
  ggplot(aes(x = dias_antecipacao_na_entrega)) +
  geom_density()

base_analise %>% 
  count(atrasou) %>% 
  mutate(prop = n / sum(n))


# product_photos_qty

base_analise %>% 
  count(product_photos_qty)
# Seria interessante imputar 0 nos NA e reclassificar essa variável


# Análises Bivariadas relacionando com a var. dependente/resposta ----

# Estado vs. Review

base_analise %>% 
  group_by(customer_state) %>% 
  summarize(n = n(),
            n_review_alto = sum(review_alto_numerico),
            tx_review_alto = mean(review_alto_numerico)) %>% 
  View()

# Visualização Interativa usando plotly
base_analise %>% 
  plota_tx_interesse(var_x = 'customer_state',
                     var_y = 'review_alto',
                     flag_interesse = 'Nota Máxima')

# Aparentemente o Estado não influencia tanto o Review


# payment_type vs. Review
base_analise %>% 
  plota_tx_interesse(var_x = 'payment_type',
                     var_y = 'review_alto',
                     flag_interesse = 'Nota Máxima',
                     ylim = NA)

# product_category_name vs. Review
base_analise %>% 
  plota_tx_interesse(var_x = 'product_category_name',
                     var_y = 'review_alto',
                     flag_interesse = 'Nota Máxima',
                     ylim = NA)

# Muitas classes, vamos agrupar em algumas
base_analise %>% 
  mutate(product_category_name_cat = fct_lump(product_category_name, 7)) %>% 
  plota_tx_interesse(var_x = 'product_category_name_cat',
                     var_y = 'review_alto',
                     flag_interesse = 'Nota Máxima',
                     ylim = NA)

# Preço/Frete/Frete sobre Preço vs. Review

base_analise %>% 
  group_by(review_alto) %>% 
  summarize(n = n(),
            media = mean(price, na.rm = T),
            desvio_padrao = sd(price, na.rm = T),
            min = min(price, na.rm = T),
            max = max(price, na.rm = T))

base_analise %>% 
  group_by(review_alto) %>% 
  summarize(n = n(),
            media = mean(freight_value, na.rm = T),
            desvio_padrao = sd(freight_value, na.rm = T),
            min = min(freight_value, na.rm = T),
            max = max(freight_value, na.rm = T))

base_analise %>% 
  group_by(review_alto) %>% 
  summarize(n = n(),
            media = mean(frete_sobre_preco, na.rm = T),
            desvio_padrao = sd(frete_sobre_preco, na.rm = T),
            min = min(frete_sobre_preco, na.rm = T),
            max = max(frete_sobre_preco, na.rm = T))


# ---- Interlúdio: o problema de Boxplots ---- #

# Relação de preço com product_category_name

base_analise %>% 
  mutate(cat_product_category_name = fct_lump(product_category_name, n = 7)) %>% 
  ggplot(aes(x = log(price), fill = cat_product_category_name)) +
  geom_density(alpha = 0.35)
# Difícil de Visualizar

base_analise %>% 
  mutate(cat_product_category_name = fct_lump(product_category_name, n = 7),
         log_price = log(price)) %>%
  ggplot(aes(x = log_price, 
             y = cat_product_category_name,
             fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.001) +
  scale_fill_viridis_c(name = "Preço (em log)")
# Melhor de Visualizar

base_analise %>% 
  mutate(cat_product_category_name = fct_lump(product_category_name, n = 7),
         log_price = log(price)) %>% 
  ggplot(aes(fill = cat_product_category_name,
             y = log_price,
             x = cat_product_category_name)) +
  geom_boxplot() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 45))
# Boxplot pode não refletir a distribuição dos dados


# ---- Fim do Interlúdio ---- #



# Dias de Antecipação na entrega/Atrasou vs. Review

base_analise %>% 
  group_by(review_alto) %>% 
  summarize(n = n(),
            media = mean(dias_antecipacao_na_entrega, na.rm = T),
            desvio_padrao = sd(dias_antecipacao_na_entrega, na.rm = T),
            min = min(dias_antecipacao_na_entrega, na.rm = T),
            max = max(dias_antecipacao_na_entrega, na.rm = T))

base_analise %>% 
  ggplot(aes(x = dias_antecipacao_na_entrega, fill = as.factor(review_alto))) +
  geom_density(alpha = 0.5)

# Analisando pela quantidade de dias não é tão clara a diferença,
# mas analisando pela variável dicotômica "Atrasou", fica clara a relevância

base_analise %>% 
  mutate(atrasou = ifelse(atrasou == 1, 'Sim', 'Não')) %>% 
  plota_tx_interesse(var_x = 'atrasou',
                     var_y = 'review_alto',
                     flag_interesse = 'Nota Máxima',
                     ylim = NA)

# product_photos_qty vs. Review
# Imputando NA's e Categorizando uma variável numérica!
base_analise %>% 
  replace_na(list(product_photos_qty = 0)) %>% 
  plota_tx_interesse(var_x = 'product_photos_qty',
                     var_y = 'review_alto',
                     flag_interesse = 'Nota Máxima',
                     ylim = NA)
# Diferença pontual entre taxas ainda pequena

# Visualização múltipla: o poder do Grammar of Graphics

# Gráfico de dispersão: Preço vs. Frete vs. Review vs. Estado vs. Atrasou vs. Qtd. de Photos
base_analise %>% 
  sample_frac(0.1) %>% 
  replace_na(list(product_photos_qty = 0)) %>% 
  mutate(customer_state = fct_lump(customer_state, 5)) %>% 
  ggplot(aes(x = log(price),
             y = log(freight_value),
             col = review_alto,
             size = product_photos_qty)) +
  geom_point(alpha = 0.5) +
  facet_wrap(customer_state ~ atrasou, 
             labeller = "label_both"
             #, scales = "free"
             ) +
  ggtitle('Preço vs. Frete vs. Review vs. Estado vs. Atrasou vs. Qtd. de Photos') +
  theme_light()

# Observação: é possível incluir interatividade no gráfico acima facilmente
# Com a função "ggplotly" (alguns ajustes podem ser requeridos como o scales = "free")

# Visualização conjunta de variáveis numéricas, misturando data wrangling e data visualization

# Lembre da Distribuição assimétrica de frete_sobre_preco e price
# Aplicando a transformação logarítmica
base_analise %>% 
  mutate(dias_antecipacao_na_entrega = as.numeric(dias_antecipacao_na_entrega),
         log_price = log(price),
         frete_sobre_preco_desloc = (freight_value + 1) / (price + 1), # Desloca-se para evitar -Infinito
         log_frete_sobre_preco_desloc = log(frete_sobre_preco_desloc)) %>% # Mantém mesmo tipo de dado para gather
  select(log_price, 
         log_frete_sobre_preco_desloc, 
         dias_antecipacao_na_entrega, 
         review_alto) %>% 
  gather(variavel, valor, -review_alto) %>% 
  ggplot(aes(x = valor, 
             fill = as.factor(review_alto))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variavel, scales = "free")


# salva a base de Dados para modelagem posterior

base_model <- base_analise %>% 
  mutate(dias_antecipacao_na_entrega = as.numeric(dias_antecipacao_na_entrega),
         log_price = log(price),
         frete_sobre_preco_desloc = (freight_value + 1) / (price + 1), # Desloca-se para evitar -Infinito
         log_frete_sobre_preco_desloc = log(frete_sobre_preco_desloc),
         product_category_name_cat = fct_lump(product_category_name, 7)
         ) %>% 
  replace_na(list(product_photos_qty = 0)) %>% 
  select(customer_state,
         log_price,
         log_frete_sobre_preco_desloc,
         payment_type,
         product_category_name_cat,
         product_photos_qty,
         dias_antecipacao_na_entrega,
         atrasou,
         review_alto_numerico)
  
# Ainda temos NA's na base?
base_model %>% 
  gather() %>% 
  group_by(key) %>% 
  summarize(n_missings = sum(is.na(value)))

# Criamos uma classe explícita de NA para produto e imputamos NA no tipo de pagamento pela categoria mais provável

base_model <- base_model %>% 
  mutate(product_category_name_cat = fct_explicit_na(product_category_name_cat)) %>% 
  replace_na(list(payment_type = 'credit_card'))
  

saveRDS(base_model, 'data/base_model.rds')
