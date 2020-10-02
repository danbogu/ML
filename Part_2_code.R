library(tidyverse)
library(caTools)
library(DataExplorer)
library(fastDummies)
library(keras)
library(rpart)
library(rpart.plot)
library(rlist)
library(cluster)
library(fpc)
library(clv)
library(pROC)

# Loading the data --------------------------------------------------------
data <- read.csv(file= "Xy_train.csv") 

# Data preprocessing ------------------------------------------------------

#Features:
categorial_fetures <- c("gender","cp","restecg","exang","thal","ca","slope")
numerical_features <- c("age", "chol","trestbps", "thalach", "oldpeak")
data <- update_columns(data, c(categorial_fetures), as.factor)

weak_features <- c("fbs")

data <- select(data, -all_of(weak_features))

# Fix abnormal ages

data_low_ages <- data %>% 
  filter(age < 80)

#Linear model
age_lm <- lm(age~ gender + cp + trestbps + chol + restecg + thalach + exang + oldpeak + slope + ca + thal, 
             data_low_ages) 

data_high_ages <- data %>% 
  filter(age > 80) 

#Predictions:
data_high_ages$age <- predict(age_lm, newdata = data_high_ages)
data_ages_fixed <- rbind(data_low_ages, data_high_ages)

data$age <- data_ages_fixed$age


# split to train and test -------------------------------------------------


set.seed(101)
train_data <- sample_frac(data, 0.7)
test_data <- anti_join(data, train_data, by = c("id"))

# Decision Trees ------------------------------------------------------

#adding features based on part 1 exploration
dt_data <- data %>% 
  mutate(chol_disc = case_when(chol < 200 ~ 0,
                               chol < 240 ~ 1,
                               TRUE ~ 2),
         trestbps_disc = case_when(trestbps < 90 ~ 0,
                                   trestbps < 140 ~ 1,
                                   trestbps < 180 ~ 2,
                                   TRUE ~ 3
         ))


#New features interactions:

dt_data <- dt_data %>% 
  mutate(bps_chol = trestbps * chol,
         oldpeak_bps = oldpeak * trestbps,
         bps_thal = trestbps * thalach,
         age_chol = age * chol)

dt_train <- semi_join(dt_data, train_data, by = c("id"))
dt_test <- semi_join(dt_data, test_data, by = c("id"))

# Full tree -------------------------------------------------

full_tree <- rpart(y~.-id, data = dt_train, method="class", minbucket=1, minsplit=1, cp=-1, maxdepth=10, xval=5, maxsurrogate=3)

rpart.plot(full_tree, extra = 106)

##Confusion matrix on validation set
dt_test_predict <- predict(full_tree, dt_test, type = 'class')

dt_test_cm <- table(dt_test$y, dt_test_predict)

dt_test_acc <- sum(diag(dt_test_cm)) / sum(dt_test_cm)
        
dt_test_acc

##confusion matrix on train set

dt_train_predict <- predict(full_tree, dt_train, type = 'class')

dt_train_cm <- table(dt_train$y, dt_train_predict)

acc <- sum(diag(dt_train_cm)) / sum(dt_train_cm)

acc

# HP Tuning -------------------------------------------------

set.seed(111)

k.fold<-function(num, data_set, seed=100){
  
  library('rlist')
  iterations <- num
  data_list <- list()
  for (i in 1:iterations){
    set.seed(seed)
    rows_num <- sample(1:nrow(data_set), (1/num) * nrow(data_set))
    set <- data_set[rows_num,]
    data_list <- list.append(data_list,set)
    data_set <- data_set[-rows_num, ]
    if(num==0) {break()}
    num<-num-1
  }
  return(data_list)
}

#Tuning validation/train set size:
dt_mean_train_acc <- c()
dt_mean_test_acc <- c()

for(i in seq(0.6, 0.95, by=0.05)){
  dt_val_acc <- c()
  dt_train_acc <- c()
  
  data_set <- k.fold((1/(1-i)), dt_train) #Using only the train.set for CV

  
  #In decision trees we dont have to standardize the data.
  for(k in seq(1,1/(1-i), by=1)){
    
    train_set_list <- data_set[c(-k)]
    train_set <- train_set_list[[1]]
    test_set <- data_set[[k]]
    if(length(train_set_list)>1){
      for(l in 2:length(train_set_list)){
        train_set <- rbind(train_set, train_set_list[[l]])
      }
    }
    
    tree <- rpart(y~.-id, data = train_set, method="class") 
    
    
    #confusion matrix on validation set
    prediction_validation <- predict(tree, test_set, type = 'class')
    con_mat <- table(test_set$y, prediction_validation)
    con_mat
    acc <- (sum(diag(con_mat)) / sum(con_mat))
    dt_val_acc <- c(dt_val_acc, acc)
    #confusion matrix on train set
    prediction_train <- predict(tree, train_set, type = 'class')
    con_mat <- table(train_set$y, prediction_train)
    con_mat
    acc<-(sum(diag(con_mat)) / sum(con_mat))
    dt_train_acc <- c(dt_train_acc, acc)
  }
  dt_mean_train_acc <- c(dt_mean_train_acc, mean(dt_train_acc))
  dt_mean_test_acc <- c(dt_mean_test_acc, mean(dt_val_acc))
}#for loop

plot(seq(0.6,0.95, by=0.05), seq(0.6, 0.95, by=0.05), type="l", col="black")
lines(seq(0.6, 0.95, by=0.05), dt_mean_train_acc, col="green")
lines(seq(0.6,0.95,by=0.05), dt_mean_test_acc, col="blue")
legend("top", legend=c("Train", "Test"),
       col=c("green", "blue"), lty=1, cex=0.8)

#Best parameter:
seq(0.6, 0.95, by=0.05)[which(dt_mean_test_acc == max(dt_mean_test_acc))]

max(dt_mean_test_acc)
#Best train set config: 95% with 79% accuracty


#Tuning minsplit size:

set.seed(112)

dt_mean_train_acc <- c()
dt_mean_test_acc <- c()

for(i in seq(2, 30, by=1)){
  
  dt_val_acc <- c()
  dt_train_acc <- c()
  data_set <- k.fold((1/(1- 0.75)), dt_train) #Using only the train.set for CV
  
  for(k in seq(1,1/(1-0.75), by=1)){
    train_set_list <- data_set[c(-k)]
    train_set <- train_set_list[[1]]
    test_set <- data_set[[k]]
    if(length(train_set_list)>1){
      for(l in 2:length(train_set_list)){
        train_set <- rbind(train_set, train_set_list[[l]])
      }
    }
    
    tree <- rpart(y~.-id, data = train_set, method="class", minsplit=i)
    
    
    #confusion matrix on validation set
    prediction_validation <- predict(tree, test_set, type = 'class')
    con_mat <- table(test_set$y, prediction_validation)
    con_mat
    acc <- (sum(diag(con_mat)) / sum(con_mat))
    dt_val_acc <- c(dt_val_acc, acc)
    
    #confusion matrix on train set
    prediction_train <- predict(tree, train_set, type = 'class')
    con_mat <- table(train_set$y, prediction_train)
    con_mat
    acc <- (sum(diag(con_mat)) / sum(con_mat))
    dt_train_acc <- c(dt_train_acc, acc)
  }
  dt_mean_train_acc <- c(dt_mean_train_acc, mean(dt_train_acc))
  dt_mean_test_acc <- c(dt_mean_test_acc, mean(dt_val_acc))
}#for loop

plot(seq(2,30,by=1), seq(0.7, 1, length.out = 29), type="l", col="black")
lines(seq(2,30,by=1), dt_mean_train_acc, col="green")
lines(seq(2,30,by=1), dt_mean_test_acc, col="blue")
legend("top", legend=c("Train", "Test"),
       col=c("green", "blue"), lty=1, cex=0.8)

#Best parameter:
seq(2,30,by=1)[which(dt_mean_test_acc==max(dt_mean_test_acc))]
max(dt_mean_test_acc)
#Best minsplits are 11/12 with 79% accuracy.

#Tuning minbucket size:
set.seed(113)
dt_mean_train_acc <- c()
dt_mean_test_acc <- c()

for(i in seq(2,10, by=1)){
  dt_val_acc <- c()
  dt_train_acc <- c()
  
  data_set <- k.fold((1/(1-0.95)), dt_train)
  
  for(k in seq(1,1/(1-0.95), by=1)){
  
    train_set_list <- data_set[c(-k)]
    train_set <- train_set_list[[1]]
    test_set <- data_set[[k]]
    if(length(train_set_list) > 1) {
      for(l in 2:length(train_set_list)) {
        train_set<-rbind(train_set, train_set_list[[l]])
      }
    }
    
    tree <- rpart(y~.-id, data = train_set, method="class", minsplit=12, minbucket=i) 
    
    #confusion matrix on validation set
    prediction_validation <- predict(tree, test_set, type = 'class')
    con_mat <- table(test_set$y, prediction_validation)
    con_mat
    acc <- (sum(diag(con_mat)) / sum(con_mat))
    dt_val_acc <- c(dt_val_acc, acc)
    #confusion matrix on train set
    prediction_train <- predict(tree, train_set, type = 'class')
    con_mat <- table(train_set$y, prediction_train)
    con_mat
    acc<-(sum(diag(con_mat)) / sum(con_mat))
    dt_train_acc <- c(dt_train_acc, acc)
  }
  dt_mean_train_acc <- c(dt_mean_train_acc, mean(dt_train_acc))
  dt_mean_test_acc <- c(dt_mean_test_acc, mean(dt_val_acc))
}#for loop

plot(seq(2, 10, by=1), seq(0.7, 1, length.out = 9)  ,type="l", col="black")
lines(seq(2, 10, by=1), dt_mean_train_acc, col="green")
lines(seq(2,10, by=1), dt_mean_test_acc, col="blue")
legend("top", legend=c("Train", "Test"),
       col=c("green", "blue"), lty=1, cex=0.8)

#Best parameter:
seq(2, 10, by=1)[which(dt_mean_test_acc == max(dt_mean_test_acc))]
max(dt_mean_test_acc)
#Best minbucket is 3 with 82.3% accuracy

#Tuning maxdepth size:
set.seed(114)
dt_mean_train_acc <- c()
dt_mean_test_acc <- c()

for(i in seq(2,30,by=1)){
  dt_val_acc<-c()
  dt_train_acc<-c()
  
  data_set <- k.fold((1/(1-0.95)), dt_train)
  
  for(k in seq(1,1/(1-0.95),by=1)){
    train_set_list <- data_set[c(-k)]
    train_set <- train_set_list[[1]]
    test_set <- data_set[[k]]
    if(length(train_set_list) > 1){
      for(l in 2:length(train_set_list)){
        train_set <- rbind(train_set, train_set_list[[l]])
      }
    }
    
    tree<-rpart(y~.-id, data = train_set, method = "class", minsplit = 11, minbucket = 3, maxdepth = i) 
    
    
    #confusion matrix on validation set
    prediction_validation <- predict(tree, test_set,type = 'class')
    con_mat<-table(test_set$y, prediction_validation)
    con_mat
    acc<-(sum(diag(con_mat)) / sum(con_mat))
    dt_val_acc<-c(dt_val_acc, acc)
    #confusion matrix on train set
    prediction_train<- predict(tree, train_set, type = 'class')
    con_mat <- table(train_set$y, prediction_train)
    con_mat
    acc <- (sum(diag(con_mat)) / sum(con_mat))
    dt_train_acc <- c(dt_train_acc,acc)
  }
  dt_mean_train_acc <- c(dt_mean_train_acc, mean(dt_train_acc))
  dt_mean_test_acc <- c(dt_mean_test_acc, mean(dt_val_acc))
}#for loop

plot(seq(2, 30, by=1), seq(0.7, 1, length.out = 29), type="l", col="black")
lines(seq(2, 30, by=1), dt_mean_train_acc, col="green")
lines(seq(2,30,by=1), dt_mean_test_acc, col="blue")
legend("top", legend = c("Train", "Test"),
       col = c("green", "blue"), lty=1, cex=0.8)


#Best parameter:
seq(2, 30, by=1)[which(dt_mean_test_acc==max(dt_mean_test_acc))]
max(dt_mean_test_acc)
#Best maxdepth is 4 with 84.2% accuracy

# Final DT model-----
set.seed(115)
final_tree <-rpart(y~.-id, data = dt_train, method="class", minsplit = 12, minbucket = 3, maxdepth = 4) 
rpart.plot(final_tree,extra = 106)

##confusion matrix on validation set
dt_test_predictions <- predict(final_tree, dt_test, type = 'class')
dt_test_cm <- table(dt_test$y, dt_test_predictions)
dt_test_acc <-(sum(diag(dt_test_cm)) / sum(dt_test_cm))
dt_test_acc

##confusion matrix on train set
dt_train_predictions <- predict(final_tree, dt_train, type = 'class')
dt_cm <- table(dt_train$y, dt_train_predictions)
acc <- (sum(diag(dt_cm)) / sum(dt_cm))
acc


# Neural Networks ---------------------------------------------------------

#For this part we will go with the original (not discretizied) features

# normalising features ------------------------------------------------------

#all numeric features will be normalised based on mean and sd of training set

data_means <- data %>% 
  summarise(across(all_of(numerical_features), mean))

data_sds <- data %>% 
  summarise(across(all_of(numerical_features), sd))


data_norm <- data %>% 
  mutate(across(all_of(numerical_features), ~ (.x - data_means[[cur_column()]]) / data_sds[[cur_column()]])) %>%
  mutate(across(all_of(categorial_fetures), ~ to_categorical(.x)))



# Splitting to train & test ------------------------------------------------------

nn_train_data <- semi_join(data_norm, train_data, by = c("id"))
nn_train_data_f <- select(nn_train_data, !c(y, id)) %>% as.matrix()
nn_train_data_l <- nn_train_data$y

nn_test_data <- semi_join(data_norm, test_data, by = c("id"))
nn_test_data_f <- select(nn_test_data, !c(y, id)) %>% as.matrix()
nn_test_data_l <- nn_test_data$y

# Train simple nn  ------------------------------------------------------


fit_custom <- function(nn_model, iterations, b_size = NULL) {
  fit(nn_model,
      x = nn_train_data_f,
      y = nn_train_data_l,
      epochs = iterations,
      batch_size = b_size,
      callbacks = callback_early_stopping(monitor = "val_loss", mode = "min", verbose = 1),
      validation_data = list(nn_test_data_f, nn_test_data_l),
      verbose = 2)}

fit_silent <- function(nn_model, iterations, b_size = NULL) {
  fit(nn_model,
      x = nn_train_data_f,
      y = nn_train_data_l,
      epochs = iterations,
      batch_size = b_size,
      callbacks = callback_early_stopping(monitor = "val_loss", mode = "min", verbose = 0),
      validation_data = list(nn_test_data_f, nn_test_data_l),
      verbose = 0)}

# "default" params: 1 layer with 10 hidden units

simple_nn <- keras_model_sequential() %>%
  layer_dense(units = 10, activation = 'relu', input_shape = c(28)) %>% 
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(loss = 'binary_crossentropy', optimizer = 'sgd', metrics = c('accuracy'))

summary(simple_nn)

set.seed(120)

simple_nn_history <-  fit_custom(simple_nn, 1, nrow(nn_train_data))

simple_nn %>% 
  evaluate(nn_test_data_f, nn_test_data_l)

predictions <- simple_nn %>% predict_classes(nn_test_data_f)
table(nn_test_data_l, predictions)

# optimizing two layers ------------------------------------------------------

layer_1_n = c(1:15)
layer_2_n = c(0:10)
layers <- expand_grid(layer_1_n, layer_2_n)

layers <- layers[!(layers$layer_1_n<layers$layer_2_n),]

layers$acc <- as.double(0.0)

set.seed(121)

for(i in 1:nrow(layers)) {
  nn <- keras_model_sequential() %>%
    layer_dense(units = layers[i,1], activation = 'relu', input_shape = c(28))
  
  if(layers[i, 2] > 0) {
    layer_dense(nn, units = layers[i,2], activation = 'relu')
  }
  nn %>%
    layer_dense(units = 1, activation = 'sigmoid') %>% 
    compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))
  
  fit_silent(nn, 1000)
  nn_eval <- evaluate(nn, nn_test_data_f, nn_test_data_l)
  layers[i, 3]  <- nn_eval$acc
}


layers %>%
  ggplot(aes(x = layer_1_n, y = acc)) +
  geom_point(aes(size = layer_2_n)) +
  scale_x_continuous(breaks = c(1:15))

layers %>% 
  arrange(desc(acc))

# Chosen model --------------
#highest results come from the 9/5/1 network and we will proceed with this model

chosen_nn_model_adam <- keras_model_sequential() %>%
  layer_dense(units = 9, activation = 'relu', input_shape = c(28)) %>% 
  layer_dense(units = 5, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(loss = 'binary_crossentropy', optimizer = 'adam', metrics = c('accuracy'))

set.seed(205)
chosen_nn_adam_history <- fit_custom(chosen_nn_model_adam, iterations = 100)

plot(chosen_nn_adam_history) +
  coord_cartesian(xlim = c(0,100)) 

chosen_nn_model_adam %>% 
  evaluate(nn_test_data_f, nn_test_data_l)

predictions <- chosen_nn_model_adam %>% 
  predict_classes(nn_test_data_f)

table(nn_test_data_l, predictions)

roc <- roc(nn_test_data_l, predictions,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Clustering --------------------------------------------------------------

data %>% select(all_of(numerical_features)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(vars(name), scales = "free_x") 

numerical_data <- select(data, c(y, all_of(numerical_features)))

clust_train <- select(numerical_data, -y)
clust <- kmeans(scale(clust_train), centers = 2)

numerical_data$cluster <- clust$cluster


clustering_agg <- numerical_data %>% 
  group_by(cluster) %>%
  summarise(label_1_cnt = sum(y),
            label_0_cnt = n() - sum(y),
            label_1_prop = label_1_cnt / n()) %>%
  arrange(label_1_prop) %>%
  mutate(label = c(0,1))

clust_1_label <- clustering_agg[[1, "label"]]
clust_2_label <- clustering_agg[[2, "label"]]


numerical_data <- numerical_data %>%
  mutate(cluster_label = ifelse(cluster == 1, clust_1_label, clust_2_label))



table(numerical_data$y, predictions = numerical_data$cluster_label)

sum(numerical_data$y == numerical_data$cluster_label) / nrow(numerical_data)

with(numerical_data, pairs(clust_train, col = c(1:12)[clust$cluster]))
clusplot(clust_train, clust$cluster, color = TRUE, shade = TRUE)


# Finding the best K -------------

# Can we find the best K with simple metrics?
# Define experiment parameters and variables
k_max <- 10
dunn <- c(); 
davies_bouldin <- c(); 

for(k in 2:k_max){
  clust_data <- kmeans(scale(clust_train), centers=k)
  scatt_data <- cls.scatt.data(scale(clust_train), clust=clust_data$cluster, dist='euclidean')
  dunn <- c(dunn, clv.Dunn(scatt_data, 'centroid', 'centroid'))
  davies_bouldin <- c(davies_bouldin, clv.Davies.Bouldin(scatt_data, 'centroid', 'centroid'))
}

clust_metrics <- data.frame(K = rep(seq(2, k_max, 1), 2), 
                            value = c(dunn, davies_bouldin), 
                            metric = c(rep('Dunn',k_max-1), 
                                       rep('davies_bouldin',k_max-1)))
ggplot(clust_metrics, aes(x = K, y = value, color=factor(metric))) + geom_point() + geom_line()



# Pam clustering ----------------------------------------------------------

pam_clust <- pam(scale(clust_train), k = 2)

numerical_data$pam_cluster <- pam_clust$cluster


pam_clustering_agg <- numerical_data %>% 
  group_by(pam_cluster) %>%
  summarise(label_1_cnt = sum(y),
            label_0_cnt = n() - sum(y),
            label_1_prop = label_1_cnt / n()) %>%
  arrange(label_1_prop) %>%
  mutate(label = c(0,1))

pam_clust_1_label <- pam_clustering_agg[[1, "label"]]
pam_clust_2_label <- pam_clustering_agg[[2, "label"]]


numerical_data <- numerical_data %>%
  mutate(pam_cluster_label = ifelse(pam_cluster == 1, pam_clust_1_label, pam_clust_2_label))



table(numerical_data$y, predictions = numerical_data$pam_cluster_label) 

numerical_data %>% group_by(pam_cluster, cluster) %>%
  summarise(n = n())

sum(numerical_data$y == numerical_data$pam_cluster_label) / nrow(numerical_data)


clusplot(clust_train, pam_clust$cluster, color = TRUE, shade = TRUE)

# final predictions ----------------------------------------------------------

x_test <- read.csv("X_test.csv")

categorial_fetures <- c("gender","cp","restecg","exang","thal","ca","slope")
numerical_features <- c("age", "chol","trestbps", "thalach", "oldpeak")
x_test <- update_columns(x_test, c(categorial_fetures), as.factor)
weak_features <- c("fbs")
x_test <- select(x_test, -all_of(weak_features))

sum((x_test$age > 80) == TRUE) # 0

x_test_norm <- x_test %>% 
  mutate(across(all_of(numerical_features), ~ (.x - data_means[[cur_column()]]) / data_sds[[cur_column()]])) %>%
  mutate(across(all_of(categorial_fetures), ~ to_categorical(.x)))

x_test_norm_f <- x_test_norm %>%
  select(-id) %>% as.matrix()

nn_test_data_f

predictions <- chosen_nn_model_adam %>% 
  predict_classes(x_test_norm_f) %>% as.vector()

pred <- data.frame(y = predictions)

pred_sample <- read.csv("preds_sample.csv")

write.csv(x = pred, file = "pred_team_23")
