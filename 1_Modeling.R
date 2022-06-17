# Author: Felix Zurek @felixzurek
#
# Data: pro-football-reference.com
#
#=================================

#loading packages
library(tidyverse)
library(mgcv)

#reading in data
draft_av <- read_csv('data/data_draft_2000-2017.csv') %>% 
  filter(!is.na(av_five), !is.na(age)) %>% # removing NA's
  filter(av_five >= 0) %>%  # remove the three negative AV's
  filter(!position %in% c('P', 'K', 'LS')) # remove specialists
  
#gam with negative binomial distributed data
model_te <- gam(av_five ~ te(pick, age, k = 15),
                data = draft_av,
                family = 'nb',
                method = 'REML',
                select = TRUE)

#model checking
summary(model_te)
plot(model_te)
gam.check(model_te)

#values to plot the model
plot_values <- expand_grid(pick = c(1,10,50,100),
            age = seq(21,25, by = .1))

#getting the model's prediction at the respective data points 
plot_values <- plot_values %>% mutate(fit = predict.gam(model_te, newdata = plot_values) %>%
  exp()) %>%
  group_by(pick) %>% 
  mutate(fit_percentage = fit/max(fit))

#plotting
ggplot(plot_values, aes(x = age, y = fit_percentage, color = factor(pick)))+
  geom_line(size = 1.75)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = 'Age', y = 'AV Percentage', color = 'Pick',
       title = 'Age Matters More Later In The Draft')+
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))





