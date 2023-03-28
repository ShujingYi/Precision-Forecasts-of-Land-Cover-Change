##########################################################
# This script follows after 2_Regression_models_Geo.R
##########################################################

# Pull best hyperparam preds from out-of-fold predictions
glm_best_OOF_preds <- collect_predictions(glm_tuned) 
rf_best_OOF_preds <- collect_predictions(rf_tuned) 
xgb_best_OOF_preds <- collect_predictions(xgb_tuned) 
# collect validation set predictions from last_fit model
glm_val_pred_geo <- collect_predictions(glm_val_fit_geo)
xgb_val_pred_geo <- collect_predictions(xgb_val_fit_geo)
rf_val_pred_geo <- collect_predictions(rf_val_fit_geo)
# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(
  data.frame(dplyr::select(glm_best_OOF_preds, .pred_class, lcre), model = "glm"),
  data.frame(dplyr::select(rf_best_OOF_preds, .pred_class,lcre), model = "rf"),
  data.frame(dplyr::select(xgb_best_OOF_preds, .pred_class, lcre), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(
    accuracy = yardstick::accuracy_vec(lcre, .pred_class),
    precision = yardstick::precision_vec(lcre, .pred_class),
    recall = yardstick::recall_vec(lcre, .pred_class)) %>% 
  ungroup() %>% 
  mutate(model = factor(model, levels=c("glm","rf","xgb")))




# average error for each model
ggplot(data = OOF_preds %>% 
         dplyr::select(model, accuracy) %>% 
         distinct() , 
       aes(x = model, y = accuracy, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(accuracy,1),"%"))) +
  theme_bw()

# OOF predicted versus actual
ggplot(OOF_preds, aes(x = lcre, y = .pred_class, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()


# Aggregate predictions from Validation set
val_preds <- rbind(
  data.frame(glm_val_pred_geo, model = "glm"),
  data.frame(rf_val_pred_geo, model = "rf"),
  data.frame(xgb_val_pred_geo, model = "xgb")) %>% 
  left_join(., jame14_sample %>% 
              rowid_to_column(var = ".row") %>% 
              dplyr::select(GEOID, .row), 
            by = ".row") %>% 
  group_by(model) %>%
  mutate(
    accuracy = yardstick::accuracy_vec(lcre, .pred_class),
    precision = yardstick::precision_vec(lcre, .pred_class),
    recall = yardstick::recall_vec(lcre, .pred_class)) %>% 
  ungroup() %>% 
  mutate(model = factor(model, levels=c("glm","rf","xgb")))

# plot accuracy by model type
ggplot(data = val_preds %>% 
         dplyr::select(model, accuracy) %>% 
         distinct() , 
       aes(x = model, y = accuracy, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(accuracy,1),"%"))) +
  theme_bw()

# Validation Predicted vs. actual
ggplot(val_preds, aes(x = lcre, y = .pred_class, group = model)) +
  geom_point(alpha = 0.3) +
  geom_abline(linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  coord_equal() +
  facet_wrap(~model, nrow = 2) +
  theme_bw()

# join test data back to make spatial
val_pred_sf <- val_preds %>% 
  group_by(model) %>% 
  rowwise() %>% 
  mutate(accuracy = yardstick::accuracy_vec(lcre, .pred_class),
         precision = yardstick::precision_vec(lcre, .pred_class),
         recall = yardstick::recall_vec(lcre, .pred_class))

# map errors by point

# aggregate val error to GEOID 
val_accuracy_by_hood <- val_preds %>% 
  group_by(GEOID, model) %>% 
  summarise(accuracy = yardstick::accuracy_vec(lcre, .pred_class),
            precision = yardstick::precision_vec(lcre, .pred_class),
            recall = yardstick::recall_vec(lcre, .pred_class)) %>% 
  ungroup() 

# plot accuracy by Hood
ggplot(val_accuracy_by_hood, aes(x = reorder(GEOID, accuracy), y = accuracy)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10,1)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = -45, hjust = 0)
  )

## Fit and Extract final model
## Final fit on all data, then extract model

full_fit_rf <- rf_best_wf %>% fit(jame14_df)
## ?
predict(full_fit_rf, new_data = ames[3,]) %>% 
  mutate(.pred_original = exp(.pred_class))


# extract final model object
glmnet_full_mod <- full_fit_glmnet$fit$fit$fit
rf_full_mod     <- full_fit_rf  $fit$fit$fit
xgb_full_mod    <- full_fit_xgb $fit$fit$fit










