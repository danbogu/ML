source("functions.R")
library(tidyverse)
library(DataExplorer)
library(Amelia)
library(ModelMetrics)
library(corrplot)
library(corrr)
library(grid)
library(cowplot)
library(reshape2)
library(gridExtra)
library(ggpubr)


train_adjusted <- train_predicted

str(train_adjusted)
introduce(train_adjusted)
plot_histogram(train_adjusted)
missmap(train_adjusted)
plot_correlation(train_adjusted[, sapply(train_adjusted, is.numeric)]) # correlation only on the numeric columns
plot_bar(train_adjusted)

#no caregorical features with high cardinality which is good

train_prepared <- update_columns(train_adjusted, c("y", "gender", "cp", "fbs", "restecg", "exang", "slope", "ca", "thal"), 
                                 as.factor)

y_prior_risk <- sum(train_prepared$y == 1) / nrow(train)

#Numerical features


#age plots and info gain
risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$age,
                           prior = y_prior_risk), 
                                   prior = p, class_labels =c("true","false"))
ggplot(train_prepared, aes(x = age, fill = y)) +
  geom_density(alpha = 0.9)

calc_info_gain(train_prepared$y, train_prepared$age)

#trestbps plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$trestbps,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))
ggplot(train_prepared, aes(x = trestbps, fill = y)) +
  geom_density(alpha = 0.9)

calc_info_gain(train_prepared$y, train_prepared$trestbps)

#chol plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$chol,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))
ggplot(train_prepared, aes(x = chol, fill = y)) +
  geom_density(alpha = 0.9)

calc_info_gain(train_prepared$y, train_prepared$chol)

#oldpeak plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$oldpeak,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))
ggplot(train_prepared, aes(x = oldpeak, fill = y)) +
  geom_density(alpha = 0.9)

calc_info_gain(train_prepared$y, train_prepared$oldpeak)

#thalach plots and info gain
               
risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$thalach,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))
ggplot(train_prepared, aes(x = thalach, fill = y)) +
  geom_density(alpha = 0.9)

calc_info_gain(train_prepared$y, train_prepared$thalach)

#Categorical features
#gender plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$gender,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$gender)

#cp plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$cp,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$cp)


#fbs plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$fbs,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$fbs)

#restecg plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$restecg,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$restecg)

#exang plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$exang,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$exang)



#slope plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$slope,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$slope)

#ca plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$ca,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$ca)

#thal plots and info gain

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_prepared$thal,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

calc_info_gain(train_prepared$y, train_prepared$thal)


#scatter plots

ggplot(train_prepared, aes(x = trestbps, y = chol)) +
  geom_point(aes(color = y))

ggplot(train_prepared, aes(x = trestbps, y = oldpeak)) +
  geom_point(aes(color = y))

ggplot(train_prepared, aes(x = trestbps, y = thalach)) +
  geom_point(aes(color = y))

ggplot(train_prepared, aes(x = age, y = oldpeak)) +
  geom_point(aes(color = y))

ggplot(train_prepared, aes(x = age, y = chol)) +
  geom_point(aes(color = y))


female_age_ntile <- train_prepared %>%
  filter(gender == 0) %>%
  mutate(age_gender_pct = ntile(age, 10))

male_age_ntile <- train_prepared %>%
  filter(gender == 1) %>%
  mutate(age_gender_pct = ntile(age, 10))

train_final <- rbind(female_age_ntile, male_age_ntile) %>%
  mutate(chol_disc = case_when(chol < 200 ~ "low_chol",
                               chol < 240 ~ "med_chol",
                               TRUE ~ "high_chol"))

train_final$chol_disc = factor(train_final$chol_disc, 
                               levels = c("low_chol", "med_chol", "high_chol"))

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_final$age_gender_pct,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))

risk_plot(category_risk_df(train_prepared$y == 1,
                           train_final$chol_disc,
                           prior = y_prior_risk), 
          prior = p, class_labels =c("true","false"))


calc_info_gain(train_final$y, train_final$age_gender_pct)
calc_info_gain(train_final$y, train_final$chol_disc)