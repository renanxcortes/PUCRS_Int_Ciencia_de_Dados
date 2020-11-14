# Carregando pacotes necessários ----
library(tidyverse)
library(lubridate)
library(visdat)
library(ggridges)
library(GGally)
library(plotly)

# Carrega algumas funções úteis para plotagem exploratória ----
source('funcoes_plot.R')


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

# geolocalizacao %>% distinct() instabilidade com relação aos distintos. Por exemplo, zip_code_prefix 01046, possui dois valores de latitude

# Verifica qtd. de linhas e colunas e estrutura das colunas
glimpse(base_completa)

View(base_completa) # Visualiza base similarmente a uma planilha excel


# Transform ----

# Nesta etapa, após a análise inicial da estrutura dos dados, criamos variáveis que serão úteis na análise.
# Criação de uma variável de review_alto: "1" se score do review for 4 ou 5, e "0" caso contrário. Não existem valores faltantes nessa variável sum(is.na(base_completa$review_score)) 
# Criação de uma variável de diferença entrega prazo de entrega dado para o cliente e data de enrega propriamente dita.
# Crição de uma variávek que relaciona valores de Frete e Preço.
# Reduzimos o escopo da análise somente para entregas concluídas (order_status == 'delivered').

base_analise <- base_completa %>% 
  mutate(review_alto = ifelse(review_score %in% c(4, 5), 1, 0),
         order_delivered_customer_date = as.Date(order_delivered_customer_date),
         order_estimated_delivery_date = as.Date(order_estimated_delivery_date),
         dias_antecipacao_na_entrega = order_estimated_delivery_date - order_delivered_customer_date,
         atrasou = ifelse(dias_antecipacao_na_entrega < 0, 1, 0),
         frete_sobre_preco = freight_value / price) %>% 
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
         dias_antecipacao_na_entrega,
         atrasou,
         order_estimated_delivery_date,
         order_delivered_customer_date,
         review_comment_title,
         review_comment_message,
         frete_sobre_preco,
         customer_city,
         seller_city) %>% 
  filter(order_status == 'delivered')

sum(is.na(base_analise$atrasou)) # Opa, temos alguns Missings na variável 'atrasou'!

# Como é a distribuição de Missings dos nossos Dados?

# Visualização PARCIAL (20% da base) de missings
set.seed(1234)
vis_dat(base_analise %>% 
          sample_frac(0.2))
vis_miss(base_analise %>% 
          sample_frac(0.2))


# Exploração e Visualização ----

# Estado vs. Review

base_analise %>% 
  group_by(customer_state) %>% 
  summarize(n = n(),
            n_review_alto = sum(review_alto),
            tx_review_alto = mean(review_alto)) %>% 
  View()

base_analise %>% 
  group_by(customer_state) %>% 
  summarize(n = n(),
            tx_review_alto = mean(review_alto)) %>% 
  ggplot(aes(x = fct_reorder(customer_state, -n), 
             y = n)) +
  geom_bar(stat = 'identity') +
  geom_line(aes(x = fct_reorder(customer_state, -n), 
                y = tx_review_alto/0.00002, 
                col ='red',
                group = 1), inherit.aes = FALSE) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.00002, 
                                         name = "Taxa de Score")) +
  labs(x = 'Estados',
       y = 'Freq.') +
  theme_minimal()

# Visualização Interativa usando plotly
base_analise %>% 
  plota_tx_interesse(var_x = 'customer_state',
                     var_y = 'review_alto',
                     flag_interesse = 1)

# Aparentemente o Estado não influencia tanto o Review


# payment_type vs. Review
base_analise %>% 
  plota_tx_interesse(var_x = 'payment_type',
                     var_y = 'review_alto',
                     flag_interesse = 1,
                     ylim = NA)

# product_category_name vs. Review
base_analise %>% 
  plota_tx_interesse(var_x = 'product_category_name',
                     var_y = 'review_alto',
                     flag_interesse = 1,
                     ylim = NA)

# Preço vs. Review

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
            media = mean(frete_sobre_preco, na.rm = T),
            desvio_padrao = sd(frete_sobre_preco, na.rm = T),
            min = min(frete_sobre_preco, na.rm = T),
            max = max(frete_sobre_preco, na.rm = T))

base_analise %>% 
  group_by(review_alto) %>% 
  summarize(n = n(),
            media = mean(dias_antecipacao_na_entrega, na.rm = T),
            desvio_padrao = sd(dias_antecipacao_na_entrega, na.rm = T),
            min = min(dias_antecipacao_na_entrega, na.rm = T),
            max = max(dias_antecipacao_na_entrega, na.rm = T))

# Visualização conjunta dessas variáveis numéricas:

base_analise %>% 
  mutate(dias_antecipacao_na_entrega = as.numeric(dias_antecipacao_na_entrega)) %>% # Mantém mesmo tipo de dado para gather
  select(price, frete_sobre_preco, dias_antecipacao_na_entrega, review_alto) %>% 
  gather(variavel, valor, -review_alto) %>% 
  ggplot(aes(x = valor, 
             fill = as.factor(review_alto))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variavel, scales = "free")

# Distribuição assimétrica de frete_sobre_preco e price
# Aplicando a transformação logarítmica
base_analise %>% 
  mutate(dias_antecipacao_na_entrega = as.numeric(dias_antecipacao_na_entrega),
         log_frete_sobre_preco = log(frete_sobre_preco),
         log_price = log(price)) %>% # Mantém mesmo tipo de dado para gather
  select(log_price, 
         log_frete_sobre_preco, 
         dias_antecipacao_na_entrega, 
         review_alto) %>% 
  gather(variavel, valor, -review_alto) %>% 
  ggplot(aes(x = valor, 
             fill = as.factor(review_alto))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variavel, scales = "free")


# Atrasou vs. Review

base_analise %>% 
  group_by(atrasou) %>% 
  summarize(n = n(),
            tx_review_alto = mean(review_alto))

# Photos vs. Tipo vs. Review

base_analise %>% 
  select(product_category_name, product_photos_qty, review_alto)


# product_category_name tem alguns missing, se vamos usá-lo, vamos imputar uma classe, 'None'
# Além disso, posdemos agrupar, pois tem muitas categorias.

# Além disso, o product_photos_qty também pode ser agrupada pra 10 ou mais

base_analise %>% 
  group_by(product_category_name) %>% 
  summarize(n = n(),
            tx_review_alto = mean(review_alto)) %>% View()

# Móveis e Escritório tem vários itens e taxa de review baixa
base_analise %>%
  filter(product_category_name == 'moveis_escritorio') %>% View()

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

# https://gomesfellipe.github.io/post/2017-12-24-diagnostico-de-modelo/diagnostico-de-modelos/
ggpairs(base_analise %>% 
          select(-customer_state, 
                 -product_category_name, 
                 -review_comment_message,
                 -customer_city,
                 -seller_city,
                 -review_comment_title,
                 -order_status,
                 -review_score,
                 -order_estimated_delivery_date,
                 -order_delivered_customer_date) %>% 
          mutate(dias_antecipacao_na_entrega = as.numeric(dias_antecipacao_na_entrega)) %>% 
          sample_frac(0.001),
        ggplot2::aes(colour = as.factor(atrasou)))  # Rápida Análise Descritiva de toda a base
ggpairs(SwissLabor, ggplot2::aes(colour = participation))  # Colorindo pela variável dependente
