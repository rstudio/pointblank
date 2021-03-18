library(tidymodels)
library(tidyverse)
library(pointblank)
library(gt)
library(blastula)

# Use the 'hotels' dataset
hotels <- 
  read_csv('https://tidymodels.org/start/case-study/hotels.csv') %>%
  mutate_if(is.character, as.factor) 

# Get a preview of the data
gt_preview(hotels)

set.seed(23)
# Define how the input data should be split
splits      <- initial_split(hotels, pro = 3/4, strata = children)
hotel_other <- training(splits)   # 75%
hotel_test  <- testing(splits)    # 25% 

set.seed(23)
# Split `hotel_other` into a Validation Set Split (0.8/0.2)
# using stratification
val_set <- 
  validation_split(
    hotel_other, 
    strata = children, 
    prop = 0.80
  )
val_set

# Let's generate a Random Forest model; the specification will be
# done with functions from parsnip
rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = parallel::detectCores()) %>% 
  set_mode("classification")

# Using the recipes package, we can define how to process the
# data (`hotel_other`)
rf_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 

# We can combine the model spec and recipe for data treatment
# into a single workflow object
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

set.seed(23)
# Taking the existing workflow, tune using grid search with our
# Validation Set Split object; this gives us tuning results (`rf_res`)
rf_res <- 
  rf_workflow %>% 
  tune_grid(
    val_set,
    grid = 25,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )

# Show the best performing models; this is for inspection...
rf_res %>% show_best(metric = "roc_auc")

# ...but we can select the best-performing model with the
# `select_best` function (filters and select only the necessary columns)
rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

# We need to see how this model performs; recall that the workflow
# still uses the `hotel_other` data so predictions will be made on that
rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Random Forest")

# Plot the ROC with ggplot
ggplot(rf_auc, aes(x = 1 - specificity, y = sensitivity)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal()

# Redefine the Random Forest model in terms of the settings
# that were best performing
best_rf_mod <- 
  rand_forest(mtry = 6, min_n = 9, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# Update the `rf_workflow` so that the model is replaced with `best_rf_mod`
best_rf_workflow <- 
  rf_workflow %>% 
  update_model(best_rf_mod)

# Fit the final best model to the training set and evaluate the test set
set.seed(23)
best_rf_fit <- 
  best_rf_workflow %>% 
  last_fit(splits)
best_rf_fit

# Collect metrics for the `best_rf_fit` object
metrics_best_rf_fit <- best_rf_fit %>% collect_metrics()

# Obtain the model accuracy
model_accuracy <-
  metrics_best_rf_fit %>%
  filter(.metric == "accuracy") %>%
  pull(.estimate)

# Get a trained model from the `best_rf_workflow`
best_rf_trained <-
  best_rf_workflow %>%
  fit(data = hotel_test)

# Generate a simple function to couple new data (labelled,
# ground truth data never used to train the model) with
# predictions made by the in-production model; the ground-truth
# data should comprise a regular period of time (1 recent week of
# fully QC'ed data) and act as a time window (e.g., offset 5 days
# back from the time of validation)
compare_model_grnd_truth <- function(grnd_truth_data,
                                     predicted_col,
                                     trained_wkflow) {
  
  dplyr::bind_cols(
    predict(trained_wkflow, grnd_truth_data),
    grnd_truth_data %>% select({{ predicted_col }})
  ) %>%
    dplyr::rename(predicted = 1, actual = 2)
}

# Generate a pointblank validation to see if significant
# model drift had occurred; this should be as self-contained
# as possible to enter this agent into production
agent <- 
  create_agent(
    read_fn = ~ compare_model_grnd_truth(
      grnd_truth_data = sample_frac(hotels, size = 0.15),
      predicted_col = "children",
      trained_wkflow = best_rf_trained
    ),
    tbl_name = "ground_truth_data",
    label = paste(
      "Comparison of ground truth data against predictions. Model accuracy: ",
      model_accuracy
    )
  ) %>%
  col_vals_equal(
    columns = vars(predicted),
    value = vars(actual),
    label = "Comparing predicted against 'actual' (ground truth).",
    actions = action_levels(
      warn_at = 1 - (model_accuracy - 0.02), # Set offsets here for
      stop_at = 1 - (model_accuracy - 0.05), # model accuracy drift
      notify_at = 1 - (model_accuracy - 0.05) # <- a NOTIFY condition that matches STOP threshold
    )
  ) %>%
  col_vals_gt(
    columns = vars(n),
    value = 5000,
    preconditions = ~ . %>% dplyr::count(),
    label = "The input table should have more than 5000 rows.",
    actions = action_levels(stop_at = 1)
  ) %>%
  col_schema_match(
    schema = col_schema(
      predicted = "factor",
      actual = "factor"
    ),
    label = "Ensure that input table has the correct schema",
    actions = action_levels(stop_at = 1)
  ) %>%
  interrogate()

# View the agent report
agent

# Getting an x-list provides a wealth of information
# on how the validation went down
x_list <- get_agent_x_list(agent)

# Get the model accuracy from the new data
accuracy_new_data <- x_list$f_passed[1]

# If the 'NOTIFY' condition is entered, then send an
# email to the person responsible for model quality;
# this condition is entered if the model accuracy is
# 0.05 less than the accuracy during training
if (any(na.omit(x_list$notify))) {
  
  email <- email_create(agent)
  
  smtp_send(
    email = email,
    to = "joe@example.com",
    from = "validator@example.com", 
    subject = paste0(
      "Model ground-truth accuracy is unacceptably lower than the ",
      "trained accuracy (",
      accuracy_new_data, " < ", model_accuracy, ")."
      ),
    credentials = creds_envvar(...)
  )
}
