source('function_exp_design_general.R')
library(broom)

# graph ----
# graph 1: boxplots of the all variables regarding to quality rating
wine %>%
  mutate(quality = as.factor(quality)) %>%
  gather('var', 'value', 1:11) %>%
  ggplot(aes(x= var, y=value, color = quality)) +
  geom_boxplot() +
  facet_wrap(~var, scales = 'free')

# graph 2: scatter plot of the all variables regarding to quality rating
ggpairs(wine%>%
          mutate(quality = as.factor(quality)), 
        columns = 1:11, ggplot2::aes(colour=quality,alpha = .3)) 

# see all variables ----
wine %>% names()

# firt interest variable
var_quality <- "density"
var_trt <- "alcohol"

# ref
data_wine_var <- median_variable(wine,var_quality)

# for the balance sample
sample_wine <- sample_data(data_wine_var, 'less', var_trt) %>%
  rbind(sample_data(data_wine_var, 'greater', var_trt)) %>%
  rename(
    trt = quality,
    bq = value_category)

# the last graph and results ----
# graph 3: of bar error
graph_interst(sample_wine %>%
                rename(
                  quality = trt,
                  value_category = bq
                ),var)

# print
result <- lm(value ~ trt + bq, data = sample_wine)
model <- aov(result)

# resultado que imprimire sobre los que
# generaron algun tipo de interes a nivel de significancia
summary(model)

# print
ss <- shapiro.test(model$residuals)
ss$p.value
# Diagnostics of model
plot(lm(value ~ trt + bq, data = sample_wine))
# TODO : cambiar la grafica

# -----------------
# generalizacion de la prueba
var_interest <- wine[wine %>% 
                       names() != 'quality'] %>% names()

all_median <- var_interest %>% 
                map(~median_variable(wine,.))

# extraccion de la muestra, pero tiene que ser
# todas las posibles combinaciones sin incluir la misma variable
# seleccionada como el bloque
all_median %>%
  map(~.)

da <- sample_wine %>% select(var_bloque) %>% unique() %>% as.list() 

sample_prove_function <- function(data_with_median,var_trt){
  sample_data(data_with_median, 'less', var_trt) %>%
    rbind(sample_data(data_with_median, 'greater', var_trt)) %>%
    rename(
      trt = quality,
      bq = value_category)
}

data_hola <- var_interest[var_interest != da] %>%
  map(~sample_prove_function(data_wine_var,.)) 

models <- data_hola %>%
            map(~lm(value ~ trt + bq, data = .x)) %>%
            map(aov)

summary_model_only_pvalue <-data_hola %>% 
  map(~ tibble(.) %>%
            select(var_bloque, var_tratamiento) %>%
            unique() %>%
            rename( trt = var_tratamiento,
                    bq = var_bloque) %>%
            gather('term', 'combination')) %>%
  enframe() %>%
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
    h0 = if_else(value > .05, 'no norm', 'norm') 
  ) %>%
  filter(h0 == 'norm') %>%
  select(!value)

left_join(summary_model_only_pvalue, 
          norm_residuals, by = 'name') %>%
  as_tibble()
