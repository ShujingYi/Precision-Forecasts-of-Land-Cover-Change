# Regression modeling

source("1_Package_Setup.R")

set.seed(717)
theme_set(theme_bw())

"%!in%" <- Negate("%in%")
g <- glimpse

port14_df <- readRDS( "~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port14_df.rds")
port18_df <- readRDS("~/Github/Precision-Forecasts-of-Land-Cover-Change/Data/output/port18_df.rds")

port14_df <- port14_df %>% mutate(
  lcre = case_when(lcchange == -1 ~ 1,
                   lcchange == 1 ~ 0,
                   lcchange == 0 ~ 0)
)
port14_df$lcchange <- as.factor(port14_df$lcchange)
port14_df$lcre <- as.factor(port14_df$lcre)
## try 2-classs/ multi-class
port14_sample <- sample_n(port14_df,500000)

### Initial Split for Training and Test
data_split <- initial_split(port14_sample, strata = "lcchange", prop = 0.75)



port_train <- training(data_split)
port_test  <- testing(data_split)


### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(port_train,  
                                group = "GEOID")
print(cv_splits_geo)

# Create recipe
model_rec <- recipe(lcre ~ ., data = port_train) %>%
  update_role(GEOID, new_role = "GEOID")  %>%
  update_role(lcchange, new_role = "lcchange")%>%
  step_ns(x, y, options = list(df = 2))

# Model specifications
glm_plan <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")  %>%
  set_args(penalty = tune(), mixture = tune()) %>%
  add_predictions()


rf_plan <- rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")


xgb_plan <- boost_tree() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Modify the hyperparameter grid for each model

glm_grid <- expand.grid(
  penalty = c("ridge", "lasso", "elasticnet"), 
  mixture = seq(0, 1, by = 0.25))
rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))

# Create the workflow
glm_wf <- workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glm_plan)


rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)

xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(xgb_plan)


# Fit model to workflow and calculate metrics


# Tune hyperparameters
control <- control_resamples(save_pred = TRUE, verbose = TRUE)
metrics <- metric_set(accuracy, precision, recall, f_meas)
glm_tuned <- glm_wf %>%
  tune_grid(resamples = cv_splits_geo,
            grid      = glm_grid,
            control = control,
            metrics = metrics)


rf_tuned <- rf_wf %>%
  tune::tune_grid(
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metrics)


xgb_tuned <- xgb_wf %>%
  tune::tune_grid(resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metrics)

# Select best model
glm_best_params <- select_best(glm_tuned, metric = "accuracy")
rf_best_params <- select_best(rf_tuned, metric = "accuracy")
xgb_best_params <- select_best(rf_tuned, metric = "accuracy")
glm_best_wf <- finalize_workflow(glm_wf, glm_best_params)
rf_best_wf <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf <- finalize_workflow(xgb_wf, xgb_best_params)

# Evaluate on test set
glm_val_fit_geo <- glm_best_wf %>% 
  last_fit(split = data_split,
           control = control,
           metrics = metrics)

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split = data_split,
           control = control,
           metrics = metrics)

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split = data_split,
           control = control,
           metrics = metrics)

# Show best model and its parameters
show_best(glm_tuned, metric = "accuracy")
show_best(rf_tuned, metric = "accuracy")
show_best(xgb_tuned, metric = "accuracy")

