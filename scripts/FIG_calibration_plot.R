# Calibration plot

library(tidyverse)
library(broom)
# load data
load("data/data_predict.rda")

# fit model
model <- glm(formula = dead ~ age + sex + sbp + bmi,
             family = "binomial",
             data = data_predict)

df_model <- model %>%
  # augment creates new columns with some useful information from the model
  # .fitted = predicted values
  augment(type.predict = "link") %>%  
  mutate(dead_count = as.numeric(dead) - 1) %>% 
  arrange(.fitted)

# calculate alpha (calibration at large) & beta (calibration slope)
# alpha is calculated by constraining beta to 1
# beta is calculated by constraining alpha to 0

df_beta <- glm(dead_count ~ .fitted,
                data = df_model,
                family = binomial) %>% 
  tidy(conf.int = TRUE)

df_alpha <- glm(dead_count ~ offset(.fitted),
               data = df_model,
               family = binomial) %>% 
  tidy() %>% 
  mutate(conf.low = estimate - qnorm(0.975) * std.error,
         conf.high = estimate + qnorm(0.975) * std.error)

# calculate mean predicted results per decile
df_sum_decile <- model %>%
  # augment creates new columns with some useful information from the model
  # .fitted = predicted values
  augment(type.predict = "response") %>%  
  mutate(dead_count = as.numeric(dead) - 1) %>% 
  arrange(.fitted) %>% 
  mutate(decile = ntile(.fitted, 10)) %>% 
  group_by(decile) %>% 
  summarize(mean_prediction = mean(.fitted), 
            sd_prediction = sd(.fitted),
            prop_observed = mean(dead_count)) %>% 
  mutate(dummy = "decile")

# plotting
ggplot(df_sum_decile, aes(x = mean_prediction, y = prop_observed)) +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # geom_abline(slope = 1, intercept = df_alpha$estimate[1], color = "#1b5299") +
  geom_point(aes(color = dummy)) +
  scale_color_manual(values = "steelblue") +
  labs(x = "Mean Predicted", y = "Mean Observed", title = "Calibration plot") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 2.5,
           label = paste("calibration-in-the-large =",
                         round(df_alpha$estimate[1], 2))) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.1, vjust = 4.5,
           label = paste("calibration slope =",
                         round(df_beta$estimate[2], 2))) +
  theme(legend.title = element_blank())

ggsave("lecture_slides/figure/calibration_plot.png")


