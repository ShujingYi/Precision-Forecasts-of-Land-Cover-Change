# Regression modeling

source("1_Package_Setup.R")

set.seed(717)
theme_set(theme_bw())

"%!in%" <- Negate("%in%")
g <- glimpse

nn_function <- function(measureFrom,measureTo,k) {
  library(FNN)
  nn <-   
    FNN::get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

### Data Preparation

### Set up Ames Housing Data
ames <- make_ames() %>% 
  dplyr::select(-matches("Qu")) %>% 
  filter(Neighborhood  %!in% c("Green_Hills", "Landmark", "Blueste", "Greens")) %>% 
  mutate(Neighborhood = as.character(Neighborhood)) %>% 
  dplyr::select(Sale_Price, Latitude, Longitude, Pool_QC, Paved_Drive,
                Garage_Area, Fireplaces, First_Flr_SF, Full_Bath, 
                Neighborhood, Lot_Area, Bldg_Type) %>% 
  mutate(home_ID = seq(1:n()))

ames <- sample_n(ames, 1000)

## NN feature creation
ames_sf <- ames %>% 
  st_as_sf(., coords = c("Longitude", "Latitude"),
           remove = FALSE,
           crs = 4326) 

## Make spatial NN feature
ames_sf <- ames_sf %>% 
  mutate(FP_NN = nn_function(st_coordinates(st_transform(ames_sf,32619)), 
                             st_coordinates(filter(st_transform(ames_sf,32619), 
                                                   Fireplaces >= 2)),3))


mapview(ames_sf, zcol = "FP_NN")

ames <- st_drop_geometry(ames_sf)

### Initial Split for Training and Test
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.75)
ames_train <- training(data_split)
ames_test  <- testing(data_split)


### Cross Validation
## LOGOCV on Neighborhood with group_vfold_cv()
cv_splits_geo <- group_vfold_cv(ames_train,  
                                group = "Neighborhood")
print(cv_splits_geo)

### Create Recipes

# Feature Creation
model_rec <- recipe(Sale_Price ~ ., data = ames_train) %>%
  update_role(Neighborhood, new_role = "Neighborhood") %>%
  step_other(Neighborhood, threshold = 0.005) %>%
  step_dummy(all_nominal(), -Neighborhood) %>%
  step_log(Sale_Price, skip = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors(), -Sale_Price) %>%
  step_scale(all_predictors(), -Sale_Price) %>%
  step_ns(Latitude, Longitude, options = list(df = 4))

# See the data after all transformations
glimpse(model_rec %>% prep() %>% juice())


## Model specifications
lm_plan <- 
  linear_reg() %>% 
  set_engine("lm")

glmnet_plan <- 
  linear_reg() %>% 
  set_args(penalty  = tune()) %>%
  set_args(mixture  = tune()) %>%
  set_engine("glmnet")

# Change the metric set to classification metrics
metrics <- metric_set(accuracy, precision, recall, f_meas)

# Modify the set_mode() function in each model
lm_plan <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

rf_plan <- rand_forest() %>%
  set_args(mtry  = tune()) %>%
  set_args(min_n = tune()) %>%
  set_args(trees = 1000) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

# Modify the hyperparameter grid for each model
rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))

# Modify the recipe to preprocess data for classification tasks
model_rec <- recipe(target ~ ., data = data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric()) %>%
  step_naomit(all_predictors())

# Create the workflow
lm_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)

# Fit model to workflow and calculate metrics
lm_tuned <- lm_wf %>%
  tune::fit_resamples(.,
                      resamples = cv_splits_geo,
                      control   = control,
                      metrics   = metrics)

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metrics)


# Hyperparameter grid for glmnet (penalization)
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))
rf_grid <- expand.grid(mtry = c(2,5), 
                       min_n = c(1,5))
xgb_grid <- expand.grid(mtry = c(3,5), 
                        min_n = c(1,5))


# create workflow
lm_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)
glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)
rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)
xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)


# fit model to workflow and calculate metrics
control <- control_resamples(save_pred = TRUE, verbose = TRUE)
metrics <- metric_set(rmse, rsq, mape, smape)
lm_tuned <- lm_wf %>%
  tune::fit_resamples(.,
                      resamples = cv_splits_geo,
                      control   = control,
                      metrics   = metrics)

glmnet_tuned <- glmnet_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = glmnet_grid,
                  control   = control,
                  metrics   = metrics)

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metrics)

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits_geo,
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metrics)


## metrics across grid
autoplot(xgb_tuned)
collect_metrics(xgb_tuned)

## 'Best' by some metric and margin
show_best(lm_tuned, metric = "rsq", n = 15)
show_best(glmnet_tuned, metric = "rsq", n = 15)
show_best(rf_tuned, metric = "rsq", n = 15)
show_best(xgb_tuned, metric = "rsq", n = 15)

lm_best_params     <- select_best(lm_tuned, metric = "rmse"    )
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse")
rf_best_params     <- select_best(rf_tuned, metric = "rmse"    )
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse"   )

## Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)


# last_fit() emulates the process where, after determining the best model, the final fit on the entire training set is needed and is then evaluated on the test set.
lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metrics)

glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metrics)

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metrics)

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metrics)


