## Packages

library(tidyverse)
library(devtools)
library(ggplot2)
library(dplyr)
library(gam)
library(broom)
library(tidymodels)
library(rsample)
library(yardstick)
library(ranger)
library(xgboost)
library(vip)
library(forcats)
library(kableExtra)
library(webshot2)
library(gt)

options(scipen = 100)


## Loading in Data

all_relays = read_csv("relays.csv")

## head(all_relays)


## Converting time variables from milliseconds to seconds

all_relays = all_relays %>% 
  mutate(retrieval_time = retrieval_time/1000,
         transition_time = retrieval_time/1000)

## Quality Control Check of All Relays

second_relays = all_relays %>% 
  filter(ball_final_base == "second")

third_relays = all_relays %>% 
  filter(ball_final_base == "third")

home_relays = all_relays %>% 
  filter(ball_final_base == "home")

naughty_relays = c("y1_d084_NXS_ARN_245", "y1_d112_XGM_PHD_430", "y1_d066_VAS_PHD_142", "y1_d198_JGO_VAS_94", "y1_d167_EMB_ANI_170", "y1_d123_MSE_ANI_239", 
                   "y1_d143_KVN_VAS_230", "y1_d108_THF_ARN_406", "y1_d095_NFP_PHD_5", "y1_d181_WIF_ANI_99", "y1_d181_WIF_ANI_105", "y1_d210_PWH_ARN_156",
                   "y1_d201_JGO_VAS_90", "y1_d137_HYG_PHD_130", "y1_d207_ATL_PHD_91", "y1_d115_BXH_VAS_96", "y1_d083_NXS_ARN_64", "y1_d081_JJP_PHD_167",
                   "y1_d172_OWV_VAS_48", "y1_d145_ALC_ARN_65", "y1_d108_THF_ARN_36", "y1_d088_ZGC_ARN_150", "y1_d127_NWM_ARN_74", "y1_d125_NWM_ARN_267", 
                   "y1_d136_HYG_PHD_109", "y1_d200_TES_ARN_71", "y1_d089_PHD_VAS_159", "y1_d176_MTW_VAS_170", "y1_d154_VAS_PHD_165", "y1_d061_VKA_PHD_22",
                   "y1_d182_CRQ_ARN_245"
                   )


relay_set = all_relays %>% ### filters relay set to exclude naughty_relays
  filter(!game_string_ppg %in% naughty_relays)


## Creating dummy variables

relay_set = relay_set %>% 
  mutate(outcome = as.factor(outcome),
         cutoff = ifelse(is_cutoff == "yes", 1, 0),
         end_2b = ifelse(ball_final_base == "second", 1, 0),
         end_3b = ifelse(ball_final_base == "third", 1, 0),
         end_home = ifelse(ball_final_base == "home", 1, 0)
         ) %>% 
  select(game_string_ppg, ball_final_base, relay_dist, cutoff, runner_speed, retrieval_time, throw_velo, transition_time, final_throw_dist, 
         runner_dist_at_final_throw, outcome)


## Splitting data into 75/25 train/test split

set.seed(1993)

split = initial_split(relay_set, prop = .75)

relays_train = training(split)
relays_test = testing(split)



## Exploratory Data Analysis Plots

relay_base_chart = relay_set %>% 
  ggplot(aes(x = ball_final_base, fill = ball_final_base)) + geom_bar(color = "black") + labs(
    title = "Total Number of Relays at 2B, 3B, and Home",
    x = "Final Base",
    y = "Count",
    fill = "Final Base"
  ) + theme_minimal()

ggsave("relay_base_chart.png", relay_base_chart, width = 4.5, height = 3)

## relay_base_chart

outcome_bar_chart = relay_set %>% 
  ggplot(aes(x = outcome, fill = outcome)) + geom_bar(color = "black") + labs(
    title = "Outcome of Each Relay",
    x = "Outcome",
    y = "Count",
    fill = "Outcome"
  ) + theme_minimal()

ggsave("outcome_chart.png", outcome_bar_chart, width = 4.5, height = 3)

## outcome_bar_chart


## Basic Logistic Regression Model


basic_log_model = logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(outcome ~ relay_dist + cutoff + runner_speed + retrieval_time + throw_velo + transition_time + final_throw_dist + runner_dist_at_final_throw,
      data = relays_train)

## summary(basic_log_model)

## raw_log_model = extract_fit_engine(basic_log_model) ## needed to create the residual plot




## Residual plot to check Logistic Regression Fit

#arm::binnedplot(
  #x = raw_log_model$fitted,
  #y = raw_log_model$resid,
  #xlab = "Predicted Probabilities",
  #main = "Bin Resid v. Pred Prob",
  #col.int = FALSE
#)


## Random Forest

random_forest_model = rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") %>%
  fit(outcome ~ relay_dist + cutoff + runner_speed + retrieval_time + throw_velo + transition_time + final_throw_dist + runner_dist_at_final_throw,
      data = relays_train)



## GAM Logistic Regression

gam_model = gam(outcome ~ s(relay_dist, df = 6) + cutoff + runner_speed + s(retrieval_time, df = 6) + 
                  s(transition_time, df = 6) + s(final_throw_dist, df = 6) + s(runner_dist_at_final_throw, df = 6), 
                data = relays_train, family = "binomial")


## XGboost

xgboost_model = boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  fit(outcome ~ relay_dist + cutoff + runner_speed + retrieval_time + throw_velo + transition_time + final_throw_dist + runner_dist_at_final_throw,
      data = relays_train)


## Predictions

relay_train_predictions = data.frame(
  actual_outcome = relays_train$outcome,
  basic_log_class = predict(basic_log_model, 
                            relays_train, 
                            type = "class") %>% 
    pull(),
  basic_log_prob = predict(basic_log_model, 
                           relays_train, 
                           type = "prob")[1] %>% 
    pull(),
  random_class = predict(random_forest_model, 
                         relays_train, 
                         type = "class") %>% 
    pull(),
  random_prob = predict(random_forest_model, 
                        relays_train, 
                        type = "prob")[1] %>% 
    pull(),
  gam_prob = predict(gam_model, 
                     relays_train, 
                     type = "response"),
  xgboost_class = predict(xgboost_model, 
                          relays_train, 
                          type = "class") %>% 
    pull(),
  xgboost_prob = predict(xgboost_model, 
                         relays_train, 
                         type = "prob")[1] %>% 
    pull()
)

relay_train_predictions = relay_train_predictions %>% 
  mutate(gam_class = as.factor(ifelse(gam_prob > 0.5, "out", "safe"))) %>% 
  relocate(gam_class, .after = random_prob)


relay_test_predictions = data.frame(
  actual_outcome = relays_test$outcome,
  basic_log_class = predict(basic_log_model, 
                            relays_test, 
                            type = "class") %>% 
    pull(),
  basic_log_prob = predict(basic_log_model, 
                           relays_test, 
                           type = "prob")[1] %>% 
    pull(),
  random_class = predict(random_forest_model, 
                         relays_test, 
                         type = "class") %>% 
    pull(),
  random_prob = predict(random_forest_model, 
                        relays_test, 
                        type = "prob")[1] %>% 
    pull(),
  gam_prob = predict(gam_model, 
                     relays_test, 
                     type = "response"),
  xgboost_class = predict(xgboost_model, 
                          relays_test, 
                          type = "class") %>% 
    pull(),
  xgboost_prob = predict(xgboost_model, 
                         relays_test, 
                         type = "prob")[1] %>% 
    pull()
)

relay_test_predictions = relay_test_predictions %>% 
  mutate(gam_class = as.factor(ifelse(gam_prob > 0.5, "out", "safe"))) %>% 
  relocate(gam_class, .after = random_prob)


## Confusion Matrices

log_model_matrix = relay_test_predictions %>% 
  conf_mat(actual_outcome, basic_log_class)

random_forest_matrix = relay_test_predictions %>% 
  conf_mat(actual_outcome, random_class)

gam_matrix = relay_test_predictions %>% 
  conf_mat(actual_outcome, gam_class)

xgboost_matrix = relay_test_predictions %>% 
  conf_mat(actual_outcome, xgboost_class)


log_model_train_matrix = relay_train_predictions %>% 
  conf_mat(actual_outcome, basic_log_class)

random_forest_train_matrix = relay_train_predictions %>% 
  conf_mat(actual_outcome, random_class)

gam_train_matrix = relay_train_predictions %>% 
  conf_mat(actual_outcome, gam_class)

xgboost_train_matrix = relay_train_predictions %>% 
  conf_mat(actual_outcome, xgboost_class)



## Model Performance Table

log_summary = tibble(summary(log_model_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>%
  mutate(model_type = "log_model",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename("test_estimate" = .estimate)

rand_forest_summary = tibble(summary(random_forest_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>%
  mutate(model_type = "random_forest",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename("test_estimate" = .estimate)


gam_summary = tibble(summary(gam_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>% 
  mutate(model_type = "gam",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename("test_estimate" = .estimate)

xgboost_summary = tibble(summary(xgboost_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>% 
  mutate(model_type = "XGboost",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename("test_estimate" = .estimate)

log_train_summary = tibble(summary(log_model_train_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>% 
  mutate(model_type = "log_model",
         metric = c("accuracy", "precision", "recall")
         ) %>% 
  select(model_type, metric, .estimate) %>% 
  rename(
    "train_estimate" = .estimate
  )

rand_train_summary = tibble(summary(random_forest_train_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>% 
  mutate(model_type = "random_forest",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename(
    "train_estimate" = .estimate
  )

gam_train_summary = tibble(summary(gam_train_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>%
  mutate(model_type = "gam",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename("train_estimate" = .estimate)

xgboost_train_summary = tibble(summary(xgboost_train_matrix)) %>% 
  filter(.metric %in% c("accuracy", "precision", "recall")) %>% 
  mutate(model_type = "XGboost",
         metric = c("accuracy", "precision", "recall")) %>% 
  select(model_type, metric, .estimate) %>% 
  rename("train_estimate" = .estimate)

log_performance = inner_join(log_summary, log_train_summary, by = c("model_type", "metric"))

random_forest_performance = inner_join(rand_forest_summary, rand_train_summary, by = c("model_type", "metric"))

gam_performance = inner_join(gam_summary, gam_train_summary, by = c("model_type", "metric"))

xgboost_performance = inner_join(xgboost_summary, xgboost_train_summary, by = c("model_type", "metric"))

model_performance = rbind(log_performance, random_forest_performance, gam_performance, xgboost_performance) %>% 
  mutate(test_estimate = test_estimate * 100,
         train_estimate = train_estimate * 100)

model_performance = pivot_wider(model_performance, names_from = metric, values_from = c(test_estimate, train_estimate)) %>% 
  select(model_type, test_estimate_accuracy, test_estimate_recall, test_estimate_precision) %>% 
  rename("test_accuracy" = test_estimate_accuracy,
         "test_recall" = test_estimate_recall,
         "test_precision" = test_estimate_precision)

## model_performance


gt_model_comparison = gt(model_performance)

gtsave(gt_model_comparison, "gt_model_comparison.png", vwidth = 1400, vheight = 1200)

## Pulling Most Important Features From Random Forest Model

raw_random_forest = extract_fit_engine(random_forest_model) # removing the fit engine; necessary for variable importance plot

variable_importance = tibble(
  refined_name = c("Relay Distance", "Cutoff", "Runner Speed", "Retrieval Time", "Throw Velocity", "Exchange Time",
                   "Distance of Final Throw", "Runner Distance at Final Throw"),
  variable_name = names(raw_random_forest$variable.importance),
  importance = raw_random_forest$variable.importance
) %>% 
  arrange(desc(importance))

## Variable Importance Plot

variable_importance_plot = variable_importance %>% 
  ggplot(aes(x = reorder(refined_name, importance), y = importance)) + geom_col(fill = "blue") + coord_flip() +
  labs(
    title = "Feature Importance",
    x = "Feature Name",
    y = "Importance"
  )

ggsave("variable_importance.png", plot = variable_importance_plot, width = 4.5, height = 3, dpi = 300)

#### variable_importance_plot

## Random Forest Model Confusion Matrix

rand_forest_paper_matrix = autoplot(random_forest_matrix, type = "heatmap") + scale_fill_gradient(low = "lightblue", high = "darkblue") + 
  geom_label(aes(label = Freq), fill = "white") + labs(
    x = "Actual Outcome",
    y = "Predicted Outcome"
  )

ggsave("random_forest_matrix.png", plot = rand_forest_paper_matrix, width = 4.5, height = 3, dpi = 300)

#### rand_forest_paper_matrix


