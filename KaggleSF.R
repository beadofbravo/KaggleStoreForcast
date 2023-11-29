library(tidymodels)
library(tidyverse)
library(timetk)
library(vroom)
library(patchwork)
library(modeltime)
library(timetk)
library(forecast)
library(prophet)

train <- vroom("./train.csv")
test <- vroom("./test.csv")


###################
#nStores <- max(store_f_train$store)
#nItems <- max(store_f_train$item)
#for(s in 1:nStores){
#  for(i in 1:nItems){
#    store_f_train_actual <- store_f_train %>%
#    filter(store==s, item==i)
#    store_f_test_actual <- store_f_test %>%
#    filter(store==s, item==i)
    ## Fit storeItem models here
#
    ## Predict storeItem sales
#
    ## Save storeItem predictions
#    if(s==1 & i==1){
#      all_preds <- preds
#    } else {
#      all_preds <- bind_rows(all_preds, preds)
#    }
#    
#  }
#}


tests1i1 <- train %>%
  filter(store == 1, item ==1)

plot1 <- forecast::ggAcf(tests1i1$sales)

tests1i10 <- train %>%
  filter(store == 1, item ==10)

plot2 <- forecast::ggAcf(tests1i10$sales)

tests4i7 <- train %>%
  filter(store == 4, item ==7)

plot3 <- forecast::ggAcf(tests4i7$sales)

tests10i20 <- train %>%
  filter(store == 10, item ==20)

plot4 <- forecast::ggAcf(tests10i20$sales)


plot1 + plot2 + plot3 + plot4
###################

store2item3<- train %>%
  filter(store == 2, item == 3)

my_recipe <- recipe(sales ~., data = store2item3) %>%
  ## make year variable
  step_date(date, features = c("year")) %>%
  ## make year a factor
  step_mutate(date_year = factor(date_year)) %>%
  ## get days of the week
  step_date(date, features = "dow") %>%
  ## get days of the year
  step_date(date, features = "doy") %>%
  ##step lag would be good but have to feature it in lag to when you have data
  step_date(date, features="decimal") %>%
  
  prep()

prep<- prep(my_recipe)
bake <- bake(prep, new_data = NULL)

###################################
## Random Forest for store2item3 ##
###################################

my_mod_crf <- rand_forest(mtry = tune(),
                          min_n = tune(),
                          trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

class_reg_tree_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod_crf)

tuning_grid_crf <- grid_regular(min_n(),
                                mtry(range = c(1, 10)),
                                levels = 5)

folds <- vfold_cv(store2item3, v = 5, repeats = 1)

CV_results_crf <- class_reg_tree_wf %>%
  tune_grid(resamples = folds, 
            grid = tuning_grid_crf,
            metrics=metric_set(smape))

best_Tune <- CV_results_crf %>%
  select_best("smape")

collect_metrics(CV_results_crf) %>%
  filter(mtry == 5, min_n == 21) %>%
  pull(mean)


final_wf_crf <- class_reg_tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data=store2item3)

plr_predictions <- predict(final_wf_crf, new_data = empl_access_test, type = "prob")


amazon_predictions_plr <- plr_predictions %>%
  bind_cols(., empl_access_test) %>%
  select(id, .pred_1) %>%
  rename(ACTION = .pred_1)  
  

##################
## SARIMA Model ##
##################

#######
## 1 ##
#######

store2item3<- train %>%
  filter(store == 2, item == 3)

my_recipe1 <- recipe(sales ~., data = store2item3) %>%
  ## make year variable
  step_date(date, features = c("year")) %>%
  ## make year a factor
  step_mutate(date_year = factor(date_year)) %>%
  ## get days of the week
  step_date(date, features = "dow") %>%
  ## get days of the year
  step_date(date, features = "doy") %>%
  ##step lag would be good but have to feature it in lag to when you have data
  step_date(date, features="decimal") %>%
  
  prep()


arima_recipe1 <- my_recipe1 # For the linear model part
arima_model1 <- arima_reg(seasonal_period=5,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
set_engine("auto_arima")

cv_split1 <- time_series_split(store2item3, assess = "3 months", cumulative = TRUE)



arima_wf1 <- workflow() %>%
  add_recipe(arima_recipe1) %>%
  add_model(arima_model1) %>%
  fit(data=training(cv_split1))

cv_results1 <- modeltime_calibrate(arima_wf1, new_data = testing(cv_split1))

cv_results1 %>%
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)


## Visualize & Evaluate CV accuracy
cv_results1 %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


#######
## 2 ##
#######
store3item3<- train %>%
  filter(store == 3, item == 3)

my_recipe2 <- recipe(sales ~., data = store3item3) %>%
  ## make year variable
  step_date(date, features = c("year")) %>%
  ## make year a factor
  step_mutate(date_year = factor(date_year)) %>%
  ## get days of the week
  step_date(date, features = "dow") %>%
  ## get days of the year
  step_date(date, features = "doy") %>%
  ##step lag would be good but have to feature it in lag to when you have data
  step_date(date, features="decimal") %>%
  prep()

cv_split2 <- time_series_split(store3item3, assess = "3 months", cumulative = TRUE)


arima_recipe2 <- my_recipe2 # For the linear model part
arima_model2 <- arima_reg(seasonal_period=5,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune
) %>%
  set_engine("auto_arima")

arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe2) %>%
  add_model(arima_model2) %>%
  fit(data=training(cv_split2))

cv_results2 <- modeltime_calibrate(arima_wf2, new_data = testing(cv_split2))

cv_results2 %>%
  modeltime_forecast(new_data = testing(cv_split2),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)



## Refit best model to entire data and predict

p1 <- modeltime_calibrate(arima_model1,
                          new_data = testing(cv_split1)) %>%
  modeltime_forecast(
    new_data = testing(cv_split1),
    actual_data = train) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p2 <- modeltime_calibrate(arima_model2,
                          new_data = testing(cv_split2)) %>%
  modeltime_forecast(
    new_data = testing(cv_split2),
    actual_data = store3item3) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p3 <- modeltime_calibrate(arima_model3,
                          new_data = testing(cv_split3)) %>%
  modeltime_refit(data = store4item3) %>%
  plot_modeltime_forecast(.interactive=FALSE)

p4 <- modeltime_calibrate(arima_model4,
                          new_data = testing(cv_split4)) %>%
  modeltime_refit(data = store5item3) %>%
  plot_modeltime_forecast(.interactive=FALSE)

p1 + p2 + p3 + p4

plotly::subplot(p1, p2, p3, p4, nrows=2)











##################
## Prohet Model ##
##################

store2item3<- train %>%
  filter(store == 2, item == 3)

my_recipe <- recipe(sales ~., data = store2item3) %>%
  ## make year variable
  step_date(date, features = c("year")) %>%
  ## make year a factor
  step_mutate(date_year = factor(date_year)) %>%
  ## get days of the week
  step_date(date, features = "dow") %>%
  ## get days of the year
  step_date(date, features = "doy") %>%
  ##step lag would be good but have to feature it in lag to when you have data
  step_date(date, features="decimal") %>%
  prep()

cv_split1 <- time_series_split(store2item3, assess = "3 months", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split1))


## Calibrate (i.e. tune) workflow

prophet_model <- modeltime_calibrate(prophet_model,
                                     new_data = testing(cv_split1))

## visualize

prophet_model %>%
  modeltime_forecast(new_data = testing(cv_split1),
                     actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive = TRUE)

## fit to the whole data set

fullfit <- prophet_model %>%
  modeltime_refit(data=train)

fullfit %>%
  modeltime_forecast(
                   new_data = test,
                   actual_data = train) %>%
  plot_modeltime_forecast(.interactive=TRUE)






