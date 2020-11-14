#' Descritivas por valores absolutos e relativos de uma covariável numérica
#' Plota os gráficos descritivos por valores absolutos e relativos (numéricos)
#'
#' @param df DataFrame a ser analisado
#' @param var_x String que representa o nome da variável de df a ser analisada.
#' @param var_y String que representa o nome da variável resposta de df.
#' @param flag_interesse String que representa a classe de interesse.
#' @param k Numérico inteiro (somente para covariáveis numéricas). Numero de classes que a covariável numérica será 'quebrada' em frequências aproximadamente homogêneas. Obs.: a função cut_number é equivalente ao quantile com probabilidades homogêneas. Não deve ser utilizados junto com o breaks.
#' @param breaks Vetor de valores personalizados para serem quebrados. Não deve ser utilizados junto com o k.
#' @param ylim Vetor numérico que representa o limite da escala dos valores de y do eixo secundário. Default c(0,1). Se 'NA', as escalas serão livres.
#' @param keep_na Booleano se mantém os NAs como uma classe específica. Default é FALSE.
#' @export
#'
#' @return \code{plotly}
plota_tx_interesse_quantis <- function(df, var_x, var_y, flag_interesse = 'categoria_de_interesse', k = 10, breaks = NA, ylim = c(0,1), keep_na = FALSE) {
  
  color_1 <- '#ff0000'
  color_2 <- '#00997c'
  
  if(df %>% select(var_x) %>% pull() %>% sd(na.rm = T) == 0) { print('Desvio Igual a zero') } else {
    
    if(any(is.na(breaks))){
      
      df %>%
        {if(keep_na)
          mutate(., quantis = fct_explicit_na(cut(get(var_x), breaks = unique(c(quantile(get(var_x), probs = seq(0, 1, l = k+1), na.rm = T))), include.lowest = TRUE))) else
            mutate(., quantis = cut(get(var_x), breaks = unique(c(quantile(get(var_x), probs = seq(0, 1, l = k+1), na.rm = T))), include.lowest = TRUE))} %>%
        group_by(quantis) %>%
        summarize(n = n(),
                  tx_de_interesse := sum(get(var_y) == flag_interesse, na.rm = T) / n) %>%
        plot_ly(x = ~quantis,
                y = ~n,
                type ='bar',
                text = ~n,
                marker = list(color = color_2),
                textposition = 'outside',
                name = "Fq.") %>%
        add_trace(x = ~quantis,
                  y = ~tx_de_interesse,
                  type = 'scatter',
                  mode = 'lines',
                  marker = list(color = 'black'),
                  line = list(color = color_1),
                  yaxis = "y2",
                  name = paste0("Tx. ", flag_interesse)) %>%
        layout(title = paste0('Tx. de ', flag_interesse, ' e Fq. por ', var_x),
               yaxis2 = list(overlaying = "y",
                             side = "right",
                             range = ylim),
               xaxis = list(title = paste0('Quantis de ', var_x))) %>%
        config(
          modeBarButtonsToRemove = list(
            'pan2d',
            'resetScale2d',
            'autoScale2d',
            'zoomIn2d',
            'zoomOut2d',
            'select2d',
            'zoom2d',
            'hoverClosestCartesian',
            'lasso2d',
            'toggleSpikelines',
            'sendDataToCloud'
          )
        )
      
    }
    
    else {
      
      vetor <- df %>%
        select(var_x) %>%
        pull()
      breaks <- unique(c(min(vetor), breaks, max(vetor)))
      
      df %>%
        {if(keep_na)
          mutate(., quantis = fct_explicit_na(cut(get(var_x), breaks = breaks, include.lowest = TRUE))) else
            mutate(., quantis = cut(get(var_x), breaks = breaks, include.lowest = TRUE))}  %>%
        group_by(quantis) %>%
        summarize(n = n(),
                  tx_de_interesse := sum(get(var_y) == flag_interesse, na.rm = T) / n) %>%
        plot_ly(x = ~quantis,
                y = ~n,
                type ='bar',
                text = ~n,
                marker = list(color = color_2),
                textposition = 'outside',
                name = "Fq.") %>%
        add_trace(x = ~quantis,
                  y = ~tx_de_interesse,
                  type = 'scatter',
                  mode = 'lines',
                  marker = list(color = 'black'),
                  line = list(color = color_1),
                  yaxis = "y2",
                  name = paste0("Tx. ", flag_interesse)) %>%
        layout(title = paste0('Tx. de ', flag_interesse, ' e Fq. por ', var_x),
               yaxis2 = list(overlaying = "y", side = "right"),
               xaxis = list(title = paste0('Quantis de ', var_x))) %>%
        config(
          modeBarButtonsToRemove = list(
            'pan2d',
            'resetScale2d',
            'autoScale2d',
            'zoomIn2d',
            'zoomOut2d',
            'select2d',
            'zoom2d',
            'hoverClosestCartesian',
            'lasso2d',
            'toggleSpikelines',
            'sendDataToCloud'
          )
        )
      
    }
    
  }
  
}


#' Descritivas por valores absolutos e relativos de uma covariável categórica
#' Plota os gráficos descritivos por valores absolutos e relativos (categórico)
#'
#' @param df DataFrame a ser analisado
#' @param var_x String que representa o nome da variável de df a ser analisada.
#' @param var_y String que representa o nome da variável resposta de df.
#' @param flag_interesse String que representa a classe de interesse.
#' @param ylim Vetor numérico que representa o limite da escala dos valores de y do eixo secundário. Default c(0,1). Se 'NA', as escalas serão livres.
#' @param keep_na Booleano se mantém os NAs como uma classe específica. Default é FALSE.
#' @export
#'
#' @return \code{plotly}
plota_tx_interesse_classes <- function(df, var_x, var_y, flag_interesse = 'categoria_de_interesse', ylim = c(0, 1), keep_na = FALSE) {
  
  color_1 <- '#ff0000'
  color_2 <- '#00997c'
  
  df %>%
    {if(keep_na)
      mutate(., aux = fct_explicit_na(as.factor(get(var_x)))) else
        mutate(., aux = as.factor(get(var_x)))} %>%
    group_by(aux) %>%
    summarize(n = n(),
              tx_de_interesse := sum(get(var_y) == flag_interesse, na.rm = T) / n) %>%
    rename(!!var_x := aux) %>%
    plot_ly(x = ~get(var_x),
            y = ~n,
            type ='bar',
            text = ~n,
            marker = list(color = color_2),
            textposition = 'outside',
            name = "Fq.") %>%
    add_trace(x = ~get(var_x),
              y = ~tx_de_interesse,
              type = 'scatter',
              mode = 'lines',
              marker = list(color = 'black'),
              line = list(color = color_1),
              yaxis = "y2",
              name = paste0("Tx. ", flag_interesse)) %>%
    layout(title = paste0('Tx. de ', flag_interesse, ' e Fq. por ', var_x),
           yaxis2 = list(overlaying = "y",
                         side = "right",
                         range = ylim),
           xaxis = list(title = var_x)) %>%
    config(
      modeBarButtonsToRemove = list(
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud'
      )
    )
}



#' Descritivas por valores absolutos e relativos de uma covariável
#' Plota os gráficos descritivos por valores absolutos e relativos (numéricos ou categórico)
#' A função aceita valores categóricos ou numéricos
#'
#' @param df DataFrame a ser analisado
#' @param var_x String que representa o nome da variável de df a ser analisada.
#' @param var_y String que representa o nome da variável resposta de df.
#' @param flag_interesse String que representa a classe de interesse.
#' @param k Numérico inteiro (somente para covariáveis numéricas). Numero de classes que a covariável numérica será 'quebrada' em frequências aproximadamente homogêneas. Obs.: a função cut_number é equivalente ao quantile com probabilidades homogêneas. Não deve ser utilizados junto com o breaks.
#' @param breaks Vetor de valores personalizados para serem quebrados. Não deve ser utilizados junto com o k.
#' @param ylim Vetor numérico que representa o limite da escala dos valores de y do eixo secundário. Default c(0,1). Se 'NA', as escalas serão livres.
#' @param keep_na Booleano se mantém os NAs como uma classe específica. Default é FALSE.
#' @export
#'
#' @return \code{plotly}
plota_tx_interesse <- function(df, var_x, var_y, flag_interesse = 'categoria_de_interesse', k = 10, breaks = NA, ylim = c(0,1), keep_na = FALSE) {
  
  if(is.numeric(df %>% select(var_x) %>% pull())) {
    
    plota_tx_interesse_quantis(df, var_x, var_y, flag_interesse = flag_interesse, k = k, breaks = breaks, ylim = ylim, keep_na = keep_na)
    
  }
  
  else {
    
    plota_tx_interesse_classes(df, var_x, var_y, flag_interesse, ylim = ylim, keep_na = keep_na)
    
  }
  
  
}
