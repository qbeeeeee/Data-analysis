install.packages("tidyverse")
library(tidyverse)
library(readxl)

rm(list = ls())

source("MyFunctions.R")
#a) arxikopoihsh
train_data <- read_excel("train_data.xls")
test_data <- read_excel("test_data.xls")

readers_training <- length(train_data$TRAIN_READER)
readers_test <- length(test_data$TEST_READER)
columns_training <- ncol(train_data)

k_NN <- 3


books <- c("TRUE_BELIEVER", "THE_DA_VINCI_CODE","THE_WORLD_IS_FLAT","MY_LIFE_IS_SO_FAR","THE.TAKING","THE_KITE_RUNNER","RUNNY.RABBIT","HARRY_POTTER")


recommendations <- data.frame(reader= character(), book = character(), rank = numeric())
MAE <- c()

sim <- calculate_similarities()
print(sim)
for (i in 1:readers_test) {
  k_nearest <- get_k_nearest(i,k_NN)
  print("K_NEAREST")
  print(k_nearest)
  
  predictions <- calculate_predictions(i, k_nearest)
  print("PREDICTIONS")
  print(predictions)
  
  need_recommendation <- spot_the_NAs(i)
  print("NEED RECOMMENDATIONS")
  print(need_recommendation)
  
  recommendations <- calculate_recommendations(i,need_recommendation,predictions,recommendations)
  print("RECOMMENDATIONS")
  print(recommendations)
}


