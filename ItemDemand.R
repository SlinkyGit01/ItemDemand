######################## Item ############################

library(tidyverse)
library(vroom)
library(forecast)
library(patchwork)
library(embed)
library(tidymodels)

setwd("~/School/F2023/STAT348/STAT348/ItemDemand")

###################### Data #######################

train <- vroom("train.csv")
test <- vroom("test.csv")

####################### General Approach #################

nStores <- max(train$store)
nItems <- max(train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
    filter(store==s, item==i)
    storeItemTest <- test %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here
    
    ## Predict storeItem sales
    
    ## Save storeItem predictions
    # if(s==1 & i==1){
    #   all_preds <- preds
    # } else {
    #   all_preds <- bind_rows(all_preds, preds)
    # }
    
  }
}

storeItemTrain <- train %>%
  filter(store==1, item==1)

plot1 <- storeItemTrain %>%
pull(sales) %>% 
forecast::ggAcf(.)

storeItemTrain <- train %>%
  filter(store==2, item==1)

plot2 <- storeItemTrain %>%
  pull(sales) %>% 
  forecast::ggAcf(.)

storeItemTrain <- train %>%
  filter(store==2, item==2)

plot3 <- storeItemTrain %>%
  pull(sales) %>% 
  forecast::ggAcf(.)

storeItemTrain <- train %>%
  filter(store==2, item==4)

plot4 <- storeItemTrain %>%
  pull(sales) %>% 
  forecast::ggAcf(.)

(plot1 + plot2) / (plot3 + plot4)



######################## Time Series ML ########################



storeItems <- train %>% 
  filter(store == 5, item == 17)

## Recipe

my_recipe <- recipe(sales ~ ., data = storeItems) %>%
  step_date(date, features="doy") %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy))

## model and workflow

my_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

mod_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

## Set up grid of tuning values

tuning_grid <- grid_regular(mtry(range = c(1,(ncol(storeItems)-1))),
                            min_n(),
                            levels = 3) ## L^2 total tuning possibilities, you choose L

## Set up K-fold CV

folds <- vfold_cv(storeItems, v = 3, repeats=1)

## Find best tuning parameters

CV_results <- mod_wf %>% tune_grid(resamples=folds, grid=tuning_grid, metrics=metric_set(smape)) #Or leave metrics NULL56

bestTune <- CV_results %>% select_best("smape")

collect_metrics(CV_results) %>%
  filter(mtry==2, min_n==40) %>% 
  pull(mean)

##########################################################



library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions

train1 <- train %>% filter(store==8, item==20)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

cv_split %>%
tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p1 <- cv_results %>%
modeltime_forecast(
                   new_data = testing(cv_split),
                   actual_data = train1
) %>%
plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
modeltime_accuracy() %>%
table_modeltime_accuracy(
                         .interactive = FALSE
)

## Refit to all data then forecast
es_fullfit <- cv_results %>%
modeltime_refit(data = train1)

es_preds <- es_fullfit %>%
modeltime_forecast(h = "3 months") %>%
rename(date=.index, sales=.value) %>%
select(date, sales) %>%
full_join(., y=test, by="date") %>%
select(id, sales)

p2 <- es_fullfit %>%
modeltime_forecast(h = "3 months", actual_data = train1) %>%
plot_modeltime_forecast(.interactive=FALSE)

####################### Part 2


train1 <- train %>% filter(store==7, item==20)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

es_model <- exp_smoothing() %>%
  set_engine("ets") %>%
  fit(sales~date, data=training(cv_split))

## Cross-validate to tune model5
cv_results <- modeltime_calibrate(es_model,
                                  new_data = testing(cv_split))

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Refit to all data then forecast
es_fullfit <- cv_results %>%
  modeltime_refit(data = train1)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date=.index, sales=.value) %>%
  select(date, sales) %>%
  full_join(., y=test, by="date") %>%
  select(id, sales)

p4 <- es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train1) %>%
  plot_modeltime_forecast(.interactive=FALSE)


plotly::subplot(p1,p3,p2,p4, nrows = 2)



################################# SARIMA ##############################



library(forecast)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(embed)
library(doParallel)

num_cores <- parallel::detectCores()

cl <- makePSOCKcluster(num_cores)

registerDoParallel(cl)

train1 <- train %>% filter(store==8, item==20)

test1 <- test %>% filter(store==8, item==20)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

arima_recipe <- recipe(sales ~ ., data = train1) %>%
  step_date(date, features="doy") %>%
  step_date(date, features="month") %>% 
  step_date(date, features="dow") %>% 
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) 
  # %>% step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales))# For the linear model part

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune8
                         ) %>%
                          set_engine("auto_arima")

arima_wf <- workflow() %>%
            add_recipe(arima_recipe) %>%
            add_model(arima_model) %>%
            fit(data=training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Visualize CV results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

arima_fullfit <- cv_results %>%
  modeltime_refit(data = train1)

p2 <- arima_fullfit %>% 
  modeltime_forecast(
    new_data = test1,
    actual_data = train1,
  ) %>% 
  plot_modeltime_forecast(.interactive=T)


# Part 2

train1 <- train %>% filter(store==7, item==20)

test1 <- test %>% filter(store==7, item==20)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2 #default max D to tune8
) %>%
  set_engine("auto_arima")

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train1
  ) %>%
  plot_modeltime_forecast(.interactive=T)

arima_fullfit <- cv_results %>%
  modeltime_refit(data = train1)

p4 <- arima_fullfit %>% 
  modeltime_forecast(
    new_data = test1,
    actual_data = train1,
  ) %>% 
  plot_modeltime_forecast(.interactive=T)

plotly::subplot(p1,p3,p2,p4, nrows = 2)

stopCluster(cl)

################################### Prophet ####################################



library(forecast)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions
library(embed)

train1 <- train %>% filter(store==8, item==20)

test1 <- test %>% filter(store==8, item==20)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
                 set_engine(engine = "prophet") %>%
                 fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Visualize CV results
p1 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

arima_fullfit <- cv_results %>%
  modeltime_refit(data = train1)

p2 <- arima_fullfit %>% 
  modeltime_forecast(
    new_data = test1,
    actual_data = train1,
  ) %>% 
  plot_modeltime_forecast(.interactive=T)

train1 <- train %>% filter(store==7, item==20)

test1 <- test %>% filter(store==7, item==20)

cv_split <- time_series_split(train1, assess="3 months", cumulative = TRUE)

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

## Visualize CV results
p3 <- cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train1
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

arima_fullfit <- cv_results %>%
  modeltime_refit(data = train1)

p4 <- arima_fullfit %>% 
  modeltime_forecast(
    new_data = test1,
    actual_data = train1,
  ) %>% 
  plot_modeltime_forecast(.interactive=T)

plotly::subplot(p1,p3,p2,p4, nrows = 2)








































































