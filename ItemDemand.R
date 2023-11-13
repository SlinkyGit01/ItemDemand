######################## Item ############################

library(tidyverse)
library(vroom)
library(forecast)
library(patchwork)

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













