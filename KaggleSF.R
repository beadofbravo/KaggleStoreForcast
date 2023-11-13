library(tidymodels)
library(tidyverse)
library(timetk)
library(vroom)
library(patchwork)

train <- vroom("./train.csv")
test <- vroom("./test.csv")




nStores <- max(store_f_train$store)
nItems <- max(store_f_train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    store_f_train_actual <- store_f_train %>%
    filter(store==s, item==i)
    store_f_test_actual <- store_f_test %>%
    filter(store==s, item==i)
    ## Fit storeItem models here

    ## Predict storeItem sales

    ## Save storeItem predictions
#    if(s==1 & i==1){
#      all_preds <- preds
#    } else {
#      all_preds <- bind_rows(all_preds, preds)
    }
    
  }
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








