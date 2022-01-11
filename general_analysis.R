source('utils/tools.R')

wine %>% names()

# variables of interest
wine_fact <- wine %>%
  mutate(quality = as.factor(quality))

variables_div <- wine_fact %>%
  gather('var', 'value', 1:11)  

# var interest data ----
data_sample <- var_interest %>% 
  map(~median_variable(variables_div,.)) %>%
  suppressMessages() %>%
  map(~tibble(.) %>%
        split(.$var) %>%
        map(~tibble(.)%>%
              split(.$value_category) %>%
              map(~tibble(.) %>%
                    split(.$quality) %>%
                    map(~sample_n(.,size=4,replace=FALSE)) %>%
                    enframe() %>%
                    unnest()
                  ) %>%
              enframe() %>%
              unnest()
              )%>%
        enframe() %>%
        unnest() %>% 
        select(!c(name2,name1))) 

#-----
data_sample %>%
  map(~general_models(.)) %>%
  enframe() %>%
  unnest() %>%
  mutate(across(where(is.numeric), round, 3))
