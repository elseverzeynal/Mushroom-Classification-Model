library(tidyverse)
library(data.table)
library(rstudioapi)
library(skimr)
library(car)
library(h2o)
library(rlang)
library(glue)
library(highcharter)
library(lime)

setwd("C:/Users/99470/Downloads")
df <- fread('mushrooms.csv')

df %>% glimpse()

#Shows the proportion of the classes
df$class %>% table() %>% prop.table()

#target to factor
df$class <- df$class %>% recode(" 'e'=1 ; 'p'=0 ") %>% as_factor()

#check type of target variable
df1 <- lapply(df,as.factor) %>% as.data.frame()
df <- df1
3...# --------------------------------- Modeling ----------------------------------
h2o.init()

h2o_data <- df %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'class'
features <- df %>% select(-class) %>% names()

# Fitting h2o model ----
model <- h2o.automl(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  leaderboard_frame = test,
  stopping_metric = "AUC",
  nfolds = 10, seed = 123,
  max_runtime_secs = 480)

model@leaderboard %>% as.data.frame()
model@leader




# ----------------------------- Model evaluation -----------------------------

# Evaluation Metrices ----------------------------
#Finding the threshold by f1 score
model@leader %>% h2o.performance(newdata = test) %>% h2o.find_threshold_by_max_metric('f1')->threshold
# Predictions
pred_test <- model@leader %>% h2o.predict(test) %>% as.data.frame()
pred_train <- model@leader %>% h2o.predict(train) %>% as.data.frame()
#high performance
model@leader%>% h2o.performance(newdata = test)
model@leader %>% h2o.performance(newdata = train)

#accuracy
model@leader %>% h2o.performance(newdata = test) %>% h2o.accuracy()
model@leader %>% h2o.performance(newdata = train) %>% h2o.accuracy()
#auc/roc/gini
model@leader %>% h2o.performance(newdata = test) %>% h2o.auc()
model@leader %>% h2o.performance(newdata = train)%>% h2o.auc()

model@leader %>% 
  h2o.performance(test) %>% 
  h2o.metric() %>% 
  select(threshold,precision,recall,tpr,fpr) %>% 
  add_column(tpr_r=runif(nrow(.),min=0.001,max=1)) %>% 
  mutate(fpr_r=tpr_r) %>% 
  arrange(tpr_r,fpr_r) -> deep_metrics

model@leader %>% 
  h2o.performance(test) %>% 
  h2o.auc() %>% round(2) -> auc

highchart() %>% 
  hc_add_series(deep_metrics, "scatter", hcaes(y=tpr,x=fpr), color='green', name='TPR') %>%
  hc_add_series(deep_metrics, "line", hcaes(y=tpr_r,x=fpr_r), color='red', name='Random Guess') %>% 
  hc_add_annotation(
    labels = list(
      point = list(xAxis=0,yAxis=0,x=0.3,y=0.6),
      text = glue('AUC = {enexpr(auc)}'))
  ) %>%
  hc_title(text = "ROC Curve") %>% 
  hc_subtitle(text = "Model is performing much better than random guessing") 

model@leader %>% 
  h2o.performance(train) %>% 
  h2o.metric() %>% 
  select(threshold,precision,recall,tpr,fpr) %>% 
  add_column(tpr_r=runif(nrow(.),min=0.001,max=1)) %>% 
  mutate(fpr_r=tpr_r) %>% 
  arrange(tpr_r,fpr_r) -> deep_metrics

model@leader %>% 
  h2o.performance(train) %>% 
  h2o.auc() %>% round(2) -> auc

highchart() %>% 
  hc_add_series(deep_metrics, "scatter", hcaes(y=tpr,x=fpr), color='green', name='TPR') %>%
  hc_add_series(deep_metrics, "line", hcaes(y=tpr_r,x=fpr_r), color='red', name='Random Guess') %>% 
  hc_add_annotation(
    labels = list(
      point = list(xAxis=0,yAxis=0,x=0.3,y=0.6),
      text = glue('AUC = {enexpr(auc)}'))
  ) %>%
  hc_title(text = "ROC Curve") %>% 
  hc_subtitle(text = "Model is performing much better than random guessing") 

model@leader %>%
  h2o.auc(train = T,
          valid = T,
          xval = T) %>%
  as_tibble() %>%
  round(2) %>%
  mutate(data = c('train','test','cross_val')) %>%
  mutate(gini = 2*value-1) %>%
  select(data,auc=value,gini)
