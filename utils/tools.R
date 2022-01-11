# library ----
library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(nortest)
library(forcats)
library(GGally)
library(broom)

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

# select of the sample
sample_data <- function(data, category, variable){
  # - data: sample data frame
  # - variable: interest variable trt
  # - category: filter of interest variable por bloque  
  set.seed(0)
  
  wine %>%
    mutate(quality = as.factor(quality)) %>%
    gather('var', 'value', 1:11) %>%
    split(.$var) %>%
    map(~ tibble(.) %>%
          split(.$quality) %>%
          map(~sample_n(.,size=8,replace=FALSE) %>%
                select(!var)) 
    ) %>%
    enframe() %>%
    unnest() %>%
    unnest()
  
}

# The median for all variables
median_variable <- function(data, variable){
  # This function calculated the median for all variables
  # regarding the quality rating of wine 
  # and filter of interest variable
  
  # - data: It's a data frame of SQL query
  # - variable: filter of interest variable
  
  # the median for all variables
  with_media_value <- data %>%
    group_by(var, quality) %>%
    filter(var == variable)%>%
    summarise(median_value = median(value),
              quality = unique(quality)) %>%
    right_join(data %>%
                 pivot_wider(quality,var), by = 'quality') %>%
    unnest()
  
  # This is result all variable with the
  # median of interest variable as reference
  with_media_value %>%
    mutate(
      value_category =
        if_else(with_media_value[names(with_media_value) == variable] > median_value,
                               'greater',
                               'less')) %>%
    gather('var', 'value',4:14) %>%
    filter(var != variable) %>%
    mutate(name_bq = variable)
    # rename(
    #   bq = value_category
    # )
  
}

median_variable_inv <- function(data, variable){
  # This function calculated the median for all variables
  # regarding the quality rating of wine 
  # and filter of interest variable
  
  # - data: It's a data frame of SQL query
  # - variable: filter of interest variable
  
  # the median for all variables
  with_media_value <- data %>%
    group_by(var, quality) %>%
    filter(var == variable)%>%
    summarise(median_value = median(value),
              quality = unique(quality)) %>%
    right_join(data %>%
                 pivot_wider(quality,var), by = 'quality') %>%
    unnest()
  
  # This is result all variable with the
  # median of interest variable as reference
  with_media_value %>%
    mutate(
      value_category =
        if_else(with_media_value[names(with_media_value) == variable] > median_value,
                'greater',
                'less')) %>%
    gather('var', 'value',4:14) %>%
    #filter(var != variable) %>%
    mutate(name_bq = variable)
  # rename(
  #   bq = value_category
  # )
  
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

# calcultate all models
general_models <- function(data_with_var_cat){
  # data_with_var_cat: all data, but in this function
  # calculte of sample of sample_prove_function and finally, 
  # calcultate all 
  
  # all_combination: all possible combination between 
  # of the variable category selection and the variable 
  # tratamient disagregate by quality rating
  all_combination <- data_with_var_cat %>%
    split(.$var)

  models <- all_combination %>%
     map(~ tibble(.) %>%
           rename( trt = quality,
                   bq = value_category)) %>%
    map(~lm(value ~ bq + trt, data = .x)) %>%
    map(aov)
   
  # var_in_model :auxialy variable, this set have all possible combination
  # between two variables (variable bloq and variable tratamient)
  var_in_model <-all_combination %>%
    map(~ tibble(.) %>%
          select(name_bq, var) %>%
          unique() %>%
          rename( trt = var,
                  bq = name_bq) %>%
          gather('term', 'combination')) %>%
    enframe() %>%
    select(!name) %>%
    unnest()
 
  # var_bloque: only variable considerer as bloq
  var_bloque <- var_in_model %>%
    filter(term == 'bq') %>%
    select(combination) %>%
    unique() %>%
    as.list()

  # all_summaries: all summary of models of the all combination
  # only p value, because I considerer the most important value.
  all_summaries <- var_in_model %>%
    mutate(var_bloq = var_bloque) %>%
    select(!term) %>%
    unnest() %>%
    cbind(
      models %>%
        map(tidy) %>%
        enframe() %>%
        unnest() %>%
        filter(!is.na(p.value)) %>%
        mutate(
          h0_param = if_else(p.value < .05,
                             'sign',
                             'no sign'))
    )

  # norm_residuals: It's p value of the residual of models for
  #  each combination
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

  #norm_residuals
  # This is of all result, I showed all combination with p-value
  # for with each param and residual values in each models.
  left_join(all_summaries,
            norm_residuals, by = 'name') %>%
    as_tibble() %>%
    filter(!is.na(h0_residual)) %>%
    select(!c(var_bloq,name,df))
}
