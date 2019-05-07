library(dplyr)
library(keras)
library(readr)
library(ggplot2)

remove_outliers = function(data){
  quantiles = quantile(data$revenue)
  maxval = quantiles[2] + 1.5 * (quantiles[2]-quantiles[1])
  return(data %>% filter(revenue <= maxval))
}


data = read.csv("/home/raphael/Documents/AppliedAnalytics/Project/weekly_stats_2016_perc.csv") %>%
  dplyr::select(-X,-sampleId) %>%
  remove_outliers()

###split dataset###
size = nrow(data)
train_size = 0.8 * size
n_feature = ncol(data) - 1
set.seed(95845)
traini = sample(1:size,0.8*size)
train_data = data[traini,]
test_data = data[-traini,]

###train neural network###
model = keras_model_sequential()

model %>% layer_dense(units = 64, activation = 'sigmoid', input_shape = n_feature) %>%
  layer_dense(units = 256, activation = 'sigmoid') %>%
  layer_dense(units = 64, activation = 'linear') %>%
  layer_dense(units = 1, activation = 'linear')

model %>% compile(
  optimizer = optimizer_nadam(),
  loss = 'mse'
)

inner_epochs = 40
early_stopping = callback_early_stopping(monitor = "val_loss",
                                         patience = inner_epochs/2)

model %>% fit(
  x = train_data %>% dplyr::select(-revenue) %>% as.matrix(),
  y = train_data %>% dplyr::select(revenue) %>% as.matrix(),
  epochs = inner_epochs,
  shuffle = T,
  callbacks = c(early_stopping),
  validation_split = 0.2)

###test neural network###
model %>% evaluate(x = test_data %>% dplyr::select(-revenue) %>% as.matrix(),
                   y = test_data %>% dplyr::select(revenue) %>% as.matrix())

((model %>% predict_on_batch(test_data %>% dplyr::select(-revenue) %>% as.matrix())
  - test_data$revenue)/test_data$revenue) %>% head(100) %>% plot()


data2 = read.csv("/home/raphael/Documents/AppliedAnalytics/Project/weekly_stats_2017_nonzero_perc.csv") %>%
  select(-X,-sampleId) %>%
  remove_outliers()

model %>% evaluate(x = data2 %>% select(-revenue) %>% as.matrix(),
                   y = data2 %>% select(revenue) %>% as.matrix())

((model %>% predict_on_batch(data2 %>% dplyr::select(-revenue) %>% as.matrix())
  - data2$revenue)/data2$revenue) %>% head(100) %>% plot()

###compare against mean revenue per trip as a simple heuristic###
mean2016 = mean(data$revenue/data$total)
pred_revenue = data$total * mean2016
(pred_revenue - data$revenue)^2 %>% mean()
