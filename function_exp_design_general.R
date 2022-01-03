# library ----
library(tidyverse)
library(dbplyr)
library(DBI)
library(odbc)
library(nortest)
library(forcats)
library(GGally)

# conn ----
con <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "SQL Server", 
                      Server = "LAPTOP-V50CPP72", 
                      Database = "wines_data", 
                      Trusted_Connection = "True")

# Return the results for query
wine <- dbGetQuery(con, "SELECT * FROM winequality") %>%
  as_tibble()

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
    rename(var = `1`,
           median_value = `2`,
           quality = `3`) %>%
    filter(var == variable) %>%
    select(quality,median_value) %>%
    right_join(data_wine, by = 'quality')
  
  # This is result all variable with the 
  # median of interest variable as reference 
  with_media_value %>%
    mutate(
      value_category = if_else(with_media_value[names(with_media_value) == variable]>median_value,
                                    'greater',
                                    'less')) %>%
    gather('var', 'value', 3:13)
  
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
sample_data <- function(data, category,variable){
  # - data: It's a data frame of SQL query
  # - variable: interest variable
  # - category: filter of interest variable  
  
  data %>%
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

