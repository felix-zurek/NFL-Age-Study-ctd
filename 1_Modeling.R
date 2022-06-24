# Author: Felix Zurek @felixzurek
#
# Data: pro-football-reference.com
#
#=================================

#loading packages
library(tidyverse)
library(mgcv)
library(tictoc)

#reading in data
draft_av <- read_csv('data/data_draft_2000-2017.csv') %>% 
  filter(!is.na(av_five), !is.na(age)) %>% # removing NA's
  filter(av_five >= 0) %>%  # remove the three negative AV's
  filter(!position %in% c('P', 'K', 'LS')) %>%  # remove specialists
  filter(position != 'QB')
  
#gam with negative binomial distributed data
tic('Fitting the model:')
model_te <- gam(av_five ~ te(pick, age, k = 18),
                data = draft_av,
                family = 'nb',
                method = 'REML',
                select = TRUE)
toc()

#model checking
summary(model_te)
plot(model_te)
gam.check(model_te)

#values to plot the model
plot_values <- expand_grid(pick = c(1,10,33,50,100),
            age = seq(21,24, by = .1))

#getting the model's prediction at the respective data points 
plot_values <- plot_values %>% mutate(fit = predict.gam(model_te, newdata = plot_values) %>%
  exp()) %>%
  group_by(pick) %>% 
  mutate(fit_percentage = fit/max(fit))

#plotting
p <- ggplot(plot_values, aes(x = age, y = fit_percentage, color = factor(pick)))+
  geom_line(size = 1.75)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x = 'Age', y = 'AV as Percentage of Maximum', color = 'Pick',
       title = 'Age Matters More Later In The Draft',
       subtitle = str_c('Dropoff in Expected AV in The First Five Years Depending on Age',
                        'AV = Approximate Value',
                        'Excluding QBs and Specialists',
                        sep = ' | '),
       caption = 'by @felixzurek | Data: pro-football-reference.com')+
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"))

p

ggsave('plots/interactive_model_no_qbs.png', p, width = 12, height = 9, bg = "white", dpi = 300)

draft_new <- read_csv('data/data_draft_2017-2022.csv')

model_exp_age <- gam(age ~ s(pick),
                     data = draft_new,
                     method = 'REML')

exp_age <- tibble(pick = 1:250) %>%
  mutate(age = predict.gam(model_exp_age, newdata = tibble(pick = 1:250)))

fit_exp_age <- exp_age %>%
  mutate(fit_exp_age = predict.gam(model_te, newdata = exp_age) %>% exp()) %>% 
  select(-age)

gm_draft_gain <- draft_new %>% mutate(fit = predict.gam(model_te, draft_new) %>% exp()) %>% 
  left_join(fit_exp_age) %>% 
  select(-college, - conference) %>% 
  mutate(gain = (fit - fit_exp_age)/fit_exp_age) %>% 
  group_by(top_executive, team) %>% 
  summarise(gain = mean(gain, na.rm = T),
            n = n(),
            last_two_draft = any(season >= 2021))
  
q <- gm_draft_gain %>%
  filter(n >= 15) %>% 
  ggplot(aes(x = gain, y = reorder(top_executive,gain)))+
  geom_col(aes(fill = team), width = 0.67)+
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = .025)+
  nflplotR::scale_fill_nfl()+
  scale_x_continuous(breaks = c(-.05,0,.05,.1),
                     labels = scales::percent_format(accuracy = 1,
                                                     style_positive = "plus"))+
  labs(x = 'Average Gain in Expected AV',
       title = 'Which GMs Gain The Most AV Due To Age ?',
       subtitle = str_c('Average Gain in Expected AV In The First 5 Years Due To Difference Between Draftee Age ',
                        'And Expected Age At Pick ',
                        '\nDrafts 2017-2022 | Minimum 15 Picks'),
       caption = 'by @felixzurek | Data: pro-football-reference.com')+
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_blank())

q

ggsave('plots/interactive_model_gms.png', q, width = 12, height = 9, bg = "white", dpi = 300)

