source('function_exp_design_general.R')

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
        columns = 1:11, ggplot2::aes(colour=quality)) 

# see all variables
wine %>% names()

# firt interest variable
var <- "alcohol"

# ref
data_wine_var <- median_variable(wine,"density")

# for the balance sample
sample_wine <- sample_data(data_wine_var, 'less', var) %>%
  rbind(sample_data(data_wine_var, 'greater', var)) %>%
  rename(
    trt = quality,
    bq = value_category)

# graph 3: of bar error
graph_interst(sample_wine %>%
                rename(
                  quality = trt,
                  value_category = bq
                ),var)

# print
model <- aov(lm(value ~ bq + trt, data = sample_wine))
summary(model)

# print
shapiro.test(model$residuals)

# Diagnostics of model
plot(lm(value ~ trt + bq, data = sample_wine))



