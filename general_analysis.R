source('utils/tools.R')

wine %>% names()
# graph ----
# graph 1: boxplots of the all variables regarding to quality rating
sample_data_by_name <- sample_data(wine) 

sample_data_by_name %>%
  ggplot(aes(x= name, y=value, color = quality)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free')

ggpairs(sample_data_by_name %>%
          pivot_wider(quality,name) %>%
          unnest(), 
        columns = 2:12, ggplot2::aes(colour=quality,alpha = .3)) 

# show the most important result of all models of all possible combination
all_median <- var_interest %>% 
  map(~median_variable(sample_data_by_name,.))%>%
  suppressMessages()

all_median %>%
  map(~general_models(.)) 

# detail of the most important combination
# firt interest variable
var_quality <- "pH"
var_trt <- "volatile_acidity"

# ref
data_wine_var <- median_variable(sample_data_by_name,var_quality)

# the last graph and results ----
# graph 3: of bar error
graph_interst(data_wine_var,var_trt)

# print
sample_variables_int <- data_wine_var %>%
                          as.data.frame() %>%
                          filter(var == var_trt) %>%
                          select(quality, value_category, var, value) 
  
sample_variables_int %>%
  count(quality, value_category)

result <- lm(value ~ value_category + quality, data = sample_variables_int)
model <- aov(result)

# resultado que imprimire sobre los que
# generaron algun tipo de interes a nivel de significancia
summary(model)

# print
shapiro.test(model$residuals)
bartlett.test(sample_variables_int$value, sample_variables_int$quality)
bartlett.test(sample_variables_int$value, sample_variables_int$value_category)

# Diagnostics of model
plot(lm(value ~ trt + bq, data = sample_wine))
# TODO : cambiar la grafica







