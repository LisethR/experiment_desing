# library ----
library(dbplyr)
library(DBI)
library(odbc)
library(tidyverse)
library(nortest)
library(forcats)
library(GGally)

# conn ----
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "LAPTOP-V50CPP72", 
                      Database = "wines_data", 
                      Trusted_Connection = "True")

# Return the results for an arbitrary query
wine <- dbGetQuery(con, "SELECT * FROM winequality") %>%
  as_tibble()


median_variable <- function(data, variable){
  
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
    rename(var = `1`,
           median_value = `2`,
           quality = `3`) %>%
    filter(var == variable) %>%
    select(quality,median_value) %>%
    right_join(data_wine, by = 'quality')
  
  with_media_value %>%
    mutate(
      value_category = if_else(with_media_value[names(with_media_value) == variable]>median_value,
                                    'greater',
                                    'less')) %>%
    gather('var', 'value', 3:13)
  
}


graph_interst <- function(data, variable){
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


general_result <- function(data, variable){
  data %>%
    group_by(quality, value_category) %>%
    filter(var == variable) %>%
    rename(
      trt = quality,
      bq = value_category)%>%
    select(!c(var,median_value))
}


sample_data <- function(data, category,variable){
  data %>%
    #count(quality, sugar_category, var) %>%
    filter(var == variable, value_category == category) %>%
    select(quality,value_category,value) %>%
    split(.$quality) %>%
    map(~ tibble(.$quality,
                 .$value_category,
                 .$value) %>%
          sample_n(size=4,replace=FALSE)
    ) %>%
    map_df(~set_names(.x, seq_along(.x))) %>%
    rename(quality = `1`,
           value_category = `2`,
           value = `3`)
}

