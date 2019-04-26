library(dplyr)
library(keras)
library(readr)
library(ggplot2)

data = read.csv("/home/raphael/Documents/AppliedAnalytics/Project/weekly_stats_2017_nonzero_perc.csv") %>% select(-X,-sampleId,-company)

###remove outliers###
quantiles = quantile(data$revenue)
maxval = quantiles[2] + 1.5 * (quantiles[2]-quantiles[1])
data = data %>% filter(revenue <= maxval)

###split dataset###
size = nrow(data)
train_size = 0.8 * size
n_feature = ncol(data) - 1
traini = sample.int(n = size,size=train_size)
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
  loss = 'mse',
  metrics = 'mape'
)

model %>% fit(
  x = train_data %>% select(-revenue) %>% as.matrix(),
  y = train_data %>% select(revenue) %>% as.matrix(),
  epochs = 20, shuffle = T)

###test neural network###
model %>% evaluate(x = test_data %>% select(-revenue) %>% as.matrix(), y = test_data %>% select(revenue) %>% as.matrix())
((model %>% predict_on_batch(test_data %>% select(-revenue) %>% as.matrix()) - test_data$revenue)/test_data$revenue) %>% head(100) %>% plot()

                                                                             