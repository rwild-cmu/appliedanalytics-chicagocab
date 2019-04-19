#==================================================================#
# Applied Analytics: the Machine Learning Pipeline
# Creation date: April 19, 2019
# Last modification: April 19, 2019
# Created by: David Mitre Becerril                                 
# Objective: Build machine learning models for predicting weekly revenue of taxi driver in Chicago
# Models: SVM, Random forest
#==================================================================#


# Install libraries
packages<-c("dplyr", "caret", "doParallel", "Metrics", "ranger", "e1071", "ggplot2")
lapply(packages, require, character.only=TRUE)
rm(packages)


# Data 2016 -----
#Import data and split in training/testing
df <- read.csv("weekly_stats_2016_nonzero.csv")
set.seed(95845)
train_index <- sample(round(nrow(df)*.70)) 
df.train <- df %>% select(-X, -sampleId) %>% slice(train_index)
df.test <- df %>% select(-X, -sampleId) %>% slice(-train_index)
rm(train_index, df); gc(reset=TRUE)

#Build models without cross-validation
clf_rf <- ranger(revenue~., data = df.train, num.trees = 100, mtry = 10, min.node.size = 5, splitrule = "variance")
#clf_svm <- svm(revenue~., data = df.train, cost = 1)

#Measure performance
pred_rf <- predict(clf_rf, data = df.test %>% select(-revenue))
mse_rf <- mse(actual = df.test$revenue, predicted = pred_rf$predictions)
paste0("MSE Random Forest: ", round(mse_rf, digits = 2))



# Data 2017 -----
#Import data and split in training/testing
df <- read.csv("weekly_stats_2017_nonzero.csv")
set.seed(95845)
train_index <- sample(round(nrow(df)*.70)) 
df.train <- df %>% select(-X, -sampleId) %>% slice(train_index)
df.test <- df %>% select(-X, -sampleId) %>% slice(-train_index)
rm(train_index, df); gc(reset=TRUE)

#Build models without cross-validation
clf_rf <- ranger(revenue~., data = df.train, num.trees = 100, mtry = 10, min.node.size = 5, splitrule = "variance")
#clf_svm <- svm(revenue~., data = df.train, cost = 1)

#Measure performance
pred_rf <- predict(clf_rf, data = df.test %>% select(-revenue))
mse_rf <- mse(actual = df.test$revenue, predicted = pred_rf$predictions)
paste0("MSE Random Forest: ", round(mse_rf, digits = 2))

df.plot <- data.frame("truth" = df.test$revenue, "prediction" = pred_rf$predictions)
ggplot(df.plot, aes(x = truth, y = prediction)) +
  geom_point(alpha=0.2, color = "darkgreen", size = 0.4) +
  theme_bw() +
  ylim(0,2000) +
  xlim(0,2000) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")




