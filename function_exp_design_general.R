# library ----
library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(nortest)
library(forcats)
library(GGally)

# TODO : la funcion median_variable hay que mejorar su tiempo
# de ejecucion

# conn ----
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "LAPTOP-V50CPP72", 
                      Database = "wines_data", 
                      Trusted_Connection = "True")

# Return the results for query
wine <- dbGetQuery(con, "SELECT * FROM winequality") %>%
  as_tibble()

# variables of interest
var_interest <- wine[wine %>% 
                       names() != 'quality'] %>% names()

# The median for all variables
median_variable <- function(data, variable){
  # This function calculated the median for all variables
  # regarding the quality rating of wine 
  # and filter of interest variable
  
  # - data: It's a data frame of SQL query
  # - variable: filter of interest variable
  
  # the median for all variables
  data_wine <- data %>%
    mutate(quality = as.factor(quality))
  
  with_media_value <-data_wine %>%
    gather('var', 'value', 1:11) %>% 
    split(.$quality) %>%
    map(~ tibble(.$quality,
                 .$var,
                 .$value) %>%
          group_by(`.$var`) %>%
          summarise(median_value = median(`.$value`),
                    quality = unique(`.$quality`))
    ) %>%
    map_df(~set_names(.x, seq_along(.x))) %>%
    rename(var_cat = `1`,
           median_value = `2`,
           quality = `3`) %>%
    filter(var_cat == variable) %>%
    # select(quality,median_value) %>%
    right_join(data_wine, by = 'quality')

  # This is result all variable with the
  # median of interest variable as reference
  with_media_value %>%
    mutate(
      value_category = if_else(with_media_value[names(with_media_value) == variable]>median_value,
                                    'greater',
                                    'less')) %>%
    gather('var', 'value',4:14)
  
}

# graph of bar error
graph_interst <- function(data, variable){
  # graph of bar error: interest variable regarding 
  # of categories variables (quality and median of 
  # interest variable as reference )
  
  data %>%
    group_by(quality, value_category) %>%
    filter(var == variable) %>%
    summarise(mean = mean(value),
              lower = min(value),
              upper = max(value)) %>%
    ggplot(aes(x = quality, y = mean, group = value_category, color = value_category)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper),width=.2, position = position_dodge(0.05)) +
    ggtitle('mean respect to quality and value_category')
  
}

# Is a data frame for apply in the model function
general_result <- function(data, variable){
  # - data: It's a data frame of SQL query
  # - variable: interest variable
  # - value_category: filter of interest variable
  
  data %>%
    group_by(quality, value_category) %>%
    filter(var == variable) %>%
    rename(
      trt = quality,
      bq = value_category)%>%
    select(!c(var,median_value))
}

# select of the sample
sample_data <- function(data, category, variable){
  # - data: sample data frame
  # - variable: interest variable trt
  # - category: filter of interest variable por bloque  
  
  data %>%
    filter(var == variable, value_category == category) %>%
    select(var_cat, var, quality,value_category,value) %>%
    split(.$quality) %>%
    map(~ tibble(
                 .$var_cat,
                 .$var,
                 .$quality,
                 .$value_category,
                 .$value) %>%
          sample_n(size=4,replace=FALSE)
    ) %>%
    map_df(~set_names(.x, seq_along(.x))) %>%
    rename(
           var_bloque = `1`,
           var_tratamiento = `2`,
           quality = `3`,
           value_category = `4`,
           value = `5`)
}

# TODO: minizar la funcion y verificar la informacion resultante 
# nuevamente

# extraccion de la muestra, pero tiene que ser
# todas las posibles combinaciones sin incluir la misma variable
# seleccionada como el bloque

sample_prove_function <- function(data_with_median,var_trat){
  sample_data(data_with_median, 'less', var_trat) %>%
    rbind(sample_data(data_with_median, 'greater', var_trat)) %>%
    rename(
      trt = quality,
      bq = value_category)
}

var_quality_select <- function(data_with_var_cat){
  
  var_cat <- data_with_var_cat %>% 
    select(var_cat) %>% 
    unique() %>% 
    as.list()
  
  all_combination <- var_interest[var_interest != var_cat] %>%
    map(~sample_prove_function(data_with_var_cat,.))
  
  models <- all_combination %>%
    map(~lm(value ~ trt + bq, data = .x)) %>%
    map(aov)
  
  var_in_model <-all_combination %>%
    map(~ tibble(.) %>%
          select(var_bloque, var_tratamiento) %>%
          unique() %>%
          rename( trt = var_tratamiento,
                  bq = var_bloque) %>%
          gather('term', 'combination')) %>%
    enframe() %>%
    unnest()
  
  
  var_bloque <- var_in_model %>%
    filter(term == 'bq') %>%
    select(combination) %>%
    unique() %>%
    as.list()
  
  all_summaries <- var_in_model %>%
    mutate(var_bloq = var_bloque) %>%
    unnest() %>%
    cbind(
      models %>%
        map(tidy) %>%
        map(~tibble(.) %>%
              select(p.value) %>%
              filter(!is.na(p.value)))  %>%
        map_df(~.)
    )
  
  norm_residuals <- models %>%
    map(residuals) %>%
    map(shapiro.test) %>%
    map(~.$p.value) %>%
    enframe() %>%
    unnest() %>%
    mutate(
      h0_residual = if_else(value < .05,
                            'no norm',
                            'norm')
    ) %>%
    filter(h0_residual == 'norm') %>%
    select(!value)
  
  left_join(all_summaries %>%
              mutate(
                h0_param = if_else(p.value < .05,
                                   'sign',
                                   'no sign')),
            norm_residuals, by = 'name') %>%
    as_tibble()
  
}
