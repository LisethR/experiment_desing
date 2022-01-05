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
shapiro.test(model$residuals)
# Diagnostics of model
plot(lm(value ~ trt + bq, data = sample_wine))
# TODO : cambiar la grafica


# generalizacion de la prueba ----
all_median <- var_interest %>% 
                map(~median_variable(wine,.))


all_median %>%
  map(~var_quality_select(.)) 
  
  
