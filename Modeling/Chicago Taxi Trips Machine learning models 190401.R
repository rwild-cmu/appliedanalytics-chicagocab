#==================================================================#
# Applied Analytics: the Machine Learning Pipeline
# Creation date: April 19, 2019
# Last modification: April 28, 2019
# Created by: David Mitre Becerril                                 
# Objective: Build machine learning models for predicting weekly revenue of taxi driver in Chicago
# Models: Random forest
#==================================================================#


# Install libraries
packages<-c("dplyr", "caret", "doParallel", "Metrics", "ranger", "e1071", "ggplot2")
lapply(packages, require, character.only=TRUE)
rm(packages)


# Data 2017 -----
#Import dataa and remove outliers
df <- read.csv("weekly_stats_2017_nonzero.csv")
quantiles = quantile(df$revenue)
maxval = quantiles[2] + 1.5 * (quantiles[2]-quantiles[1])
df = df %>% filter(revenue <= maxval)
rm(maxval, quantiles)

#Split in training/testing
set.seed(95845)
train_index <- sample(round(nrow(df)*.70)) 
dftrain <- df %>% select(-X, -sampleId) %>% slice(train_index)
dftest <- df %>% select(-X, -sampleId) %>% slice(-train_index)
rm(train_index)

#Parameter tuning
set.seed(95845)
kfolds = 5
folds_index <- createFolds(dftrain$revenue, k = kfolds) #create k folds indices
best_mse <- Inf
best_param <- NA
param <- expand.grid(mtry = c(10,20,30), min.node.size = c(5,15,25)) #tuning parameters
dfmse <- data.frame(mse = NA, fold = NA, mtry = NA, min.node.size = NA) #to keep track of the mse of each fold
counter <- 1
for (i in 1:nrow(param)){
  mse_folds <- vector()
  for (f in 1:kfolds) {
    
    #Create a train and testing set for each fold
    test_index <- folds_index[[f]]
    cvtrain <- dftrain[-test_index,]
    cvtest <- dftrain[test_index,]
    
    #train and test model for each fold
    clf <- ranger(revenue~., data = cvtrain, mtry = param[i,"mtry"], min.node.size = param[i,"min.node.size"], 
                  num.trees = 50, splitrule = "variance")
    pred <- predict(clf, data = cvtest %>% select(-revenue))
    mse <- mse(actual = cvtest$revenue, predicted = pred$predictions)
    mse_folds <- c(mse_folds, mse)
    
    #Keep track of every mse to assess its variability
    dfmse[counter,] <- c(mse, f, param[i,"mtry"], param[i,"min.node.size"])
    counter <- counter + 1 
  }  
  #Keep track of the best model using the mean mse of the k-folds
  if (mean(mse_folds) <= best_mse){
    best_param <- param[i,]
    best_mse <- mean(mse_folds)
  }
  print(paste(i, "param")) #to keep track of the loop
}
rm(kfolds, folds_index, counter, test_index, cvtrain, cvtest, mse, mse_folds, f, i, clf, pred)

#Visualize how the mse changes across different parameters
plot(dfmse$mtry, dfmse$mse)
plot(dfmse$min.node.size, dfmse$mse)

#Build best model
clf_rf <- ranger(revenue~., data = dftrain, num.trees = 100, splitrule = "variance",
                 mtry = best_param$mtry, min.node.size = best_param$min.node.size)

#Measure performance
pred_rf <- predict(clf_rf, data = dftest %>% select(-revenue))
mse_rf <- mse(actual = dftest$revenue, predicted = pred_rf$predictions)
paste0("MSE Random Forest: ", round(mse_rf, digits = 2))

#Plot predicted vs truth
dfplot <- data.frame("truth" = dftest$revenue, "prediction" = pred_rf$predictions)
ggplot(dfplot, aes(x = truth, y = prediction)) +
  geom_point(alpha=0.2, color = "darkgreen", size = 0.4) +
  theme_bw() +
  xlim(0,660) +
  ylim(0,660) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")




