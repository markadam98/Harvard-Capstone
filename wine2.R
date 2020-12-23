library(tidyverse)
library(dplyr)
library(outliers)
library(ggplot2)
library(DAAG)
library(simputation)
library(caret)
library(boot)
library(kernlab)
library(tree)
library(mice)
library(randomForest)
library(kknn)
#import and view red wine data set 
red <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', sep=';')
class(red)
dim(red)
head(red)
sum(is.na(red))





#import and view white wine data set 
white<- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', sep=';')
head(white)
dim(white)
sum(is.na(red))

# observe the quality ratings of data set
sort(unique(red$quality))
sort(unique(white$quality))

# check for outliers
for (i in 1:11) {
  print(paste("Outlier test for red wine", colnames(red)[i]))
  print(grubbs.test(red[,i]), type = 11)
  print(paste("Outlier test for white wine", colnames(red)[i]))
  print(grubbs.test(white[,i]), type = 11)
}



# In order to set up a classification predictor assign a binary variable to each data set
# Part of this project is to simulate predicting benign vs. malign results
# Because there are more whites, I simulate white as benign and assigned it to 0; red is malign so it is assigned a 1
white_binary <- white %>% mutate(class = 0)
red_binary <- red %>% mutate(class = 1)

# Combine the data sets, randomly shuffle the rows and reset the index
set.seed(12)
rw_comb <- rbind(white_binary, red_binary)[sample(nrow(white_binary) + nrow( red_binary)),] %>%
          mutate(class = as.factor(class))
row.names(rw_comb) <- NULL
dim(rw_comb)
head(rw_comb)

# overlaying histograms
for (i in 1:11) {
  print(ggplot(rw_comb, aes(x=rw_comb[,i], y=..density.., color = class, fill=class)) + 
    geom_histogram(bins=40, alpha=.5, position='identity')+
    labs(x=colnames(rw_comb)[i]))
}


rw_comb$alcohol

# scale the data
rw_comb_scale <- cbind(scale(rw_comb[,1:11]), rw_comb[,12:13]) 
#red_scale <- cbind(scale(red[,1:11]), red[,12])
#white_scale <- cbind(scale(white[,1:11]), white[,12])

# partition the data into the training set and test set 
set.seed(12)
test_idx <- createDataPartition(rw_comb_scale$class, times=1, p=.2, list=FALSE)
rw_comb_test <- rw_comb_scale[test_idx,]
rw_comb_train <- rw_comb_scale[-test_idx,]

#take the average of the training set
avg <- mean(rw_comb_train$quality)
print(avg)
med <- median(rw_comb_train$quality)
print(med)

#compute the RMSE of on the average
rmse_train_avg <- sqrt(mean((rw_comb_train$quality - avg)^2))
rmse_test_avg <- sqrt(mean((rw_comb_test$quality - avg)^2))

rmse_train_med <- sqrt(mean((rw_comb_train$quality - med)^2))
rmse_test_med <- sqrt(mean((rw_comb_test$quality - med)^2))

#build a table to compare RMSE
rmse_results <- data_frame(method = "Just the average on training set", RMSE = rmse_train_avg)
rmse_results <- bind_rows(rmse_results, data_frame(method = "Just the average on test set", RMSE = rmse_test_avg))
rmse_results <- bind_rows(rmse_results, data_frame(method = "Just the median on training set", RMSE = rmse_train_med))
rmse_results <- bind_rows(rmse_results, data_frame(method = "Just the median on test set", RMSE = rmse_test_med))


# linear regression to predict quality
lm_model <- lm(quality~., data=rw_comb_train)
summary(lm_model)
y_hat_train <- predict(lm_model)

y_hat_test <- predict(lm_model, rw_comb_test)


#calculate the RMSE on the linear regression model and at to rmse_results table
rmse_train <- sqrt(mean((rw_comb_train$quality - y_hat_train)^2))
rmse_test <- sqrt(mean((rw_comb_test$quality - y_hat_test)^2))
rmse_results <- bind_rows(rmse_results, data_frame(method = "Linear regression on training set", RMSE = rmse_train))
rmse_results <- bind_rows(rmse_results, data_frame(method = "Linear regression on test set", RMSE = rmse_test))



# what percentage of the rating can be predicted exactly from the rounded y_hat
train_accuracy <- mean(round(y_hat_train) == rw_comb_train$quality)
test_accuracy <- mean(round(y_hat_test) == rw_comb_test$quality)
accuracy_results <- data_frame(method = "Linear regression on training set", Accuracy = train_accuracy)
accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Linear regression on test set", Accuracy=test_accuracy))

#add the prediction to the the column 
rw_comb_test_pred <- rw_comb_test %>% mutate(y_hat = y_hat_test)

#review the plot of predicted (y-axis) as a function of actual (x-axis) quality ratings
ggplot(rw_comb_test_pred, aes(quality, round(y_hat_test), col=class)) + geom_jitter()+xlim(3,9)+ylim(3,9)+
  labs(x="Actual Quality Rating", y="Predicted Quality Rating", title='Jitter plot of actual vs predicted values of quality')+
  geom_abline()



# what about trying to predict the quality within + or - 1
train_acc_range <- mean( (round(y_hat_train) <= rw_comb_train$quality+1) & (round(y_hat_train) >= rw_comb_train$quality - 1))
test_acc_range <- mean( (round(y_hat_test) <= rw_comb_test$quality+1) & (round(y_hat_test) >= rw_comb_test$quality - 1))
accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Linear regresssion on training set with +- 1 range", 
                                                           Accuracy = train_acc_range))
accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Linear regression on test set with +-1 range", 
                                                           Accuracy = test_acc_range))


# variable selection
var_sel_model <- lm(quality~., data=rw_comb_train[, -c(3,5)])
summary(var_sel_model)

y_hat_train_var_sel <- predict(var_sel_model)

y_hat_test_var_sel <- predict(var_sel_model, rw_comb_test)

#add variable selection rmse to rmse_results table
rmse_train_Var_sel <- sqrt(mean((rw_comb_train$quality - y_hat_train_var_sel)^2))
rmse_test_var_sel <- sqrt(mean((rw_comb_test$quality - y_hat_test_var_sel)^2))

rmse_results <- bind_rows(rmse_results, data_frame(method = "Linear regression after variable selection on the training set",
                                                   RMSE = rmse_train_Var_sel))
rmse_results <- bind_rows(rmse_results, data_frame(method = "Linear regression after variable selection on the test set",
                                                   RMSE = rmse_test_var_sel))
rmse_results


rw_comb_test_pred_Var_sel <- rw_comb_test %>% mutate(y_hat = y_hat_test_var_sel)

# what percentage of the rating can be predicted exactly from the rounded y_hat
train_acc_var_sel <- mean(round(y_hat_train_var_sel) == rw_comb_train$quality)
test_acc_var_sel <- mean(round(y_hat_test_var_sel) == rw_comb_test$quality)
accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Linear regresssion on training set after variable selection", 
                                                           Accuracy = train_acc_var_sel))
accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Linear regression on test set after variable selection", 
                                                           Accuracy = test_acc_var_sel))

#review the plot of predicted (y-axis) as a function of actual (x-axis) quality ratings
ggplot(rw_comb_test_pred_Var_sel, aes(quality, round(y_hat_test_var_sel))) + geom_jitter(color='navy')+xlim(3,9)+ylim(3,9)+
  labs(x="Actual Quality Rating", y="Predicted Quality Rating")+geom_abline()



# what about trying to predict the quality within + or - 1
train_acc_var_sel_range <- mean((round(y_hat_train_var_sel) <= rw_comb_train$quality+1) & 
                                  (round(y_hat_train_var_sel) >= rw_comb_train$quality - 1))
test_acc_var_sel_range <- mean((round(y_hat_test_var_sel) <= rw_comb_test$quality+1) & 
                                 (round(y_hat_test_var_sel) >= rw_comb_test$quality - 1))

accuracy_results <- bind_rows(accuracy_results, 
                              data_frame(method = "Linear regresssion on training set after variable selection with +- 1 range", 
                                                           Accuracy = train_acc_var_sel_range))
accuracy_results <- bind_rows(accuracy_results, 
                              data_frame(method = "Linear regression on test set after variable selection with +-1 range", 
                                                           Accuracy = test_acc_var_sel_range))





# logistic regression as a classifier
logreg <- glm(class~., data=rw_comb_train, family=binomial(link = "logit"))
summary(logreg)
y_hat_train_log <- round(predict(logreg, type = 'response'))
y_hat_test_log <- round(predict(logreg, newdata=rw_comb_test, type = 'response'))

train_log_acc <- mean(y_hat_train_log == rw_comb_train$class)
test_log_acc <- mean(y_hat_test_log == rw_comb_test$class)

#confusion matrix on test set
confusionMatrix(as.factor(y_hat_test_log), as.factor(rw_comb_test$class))
log_conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat_test_log), as.factor(rw_comb_test$class)))

# Manually calculate the specificity 
logreg_spec <- log_conf_mat[2,2] / (log_conf_mat[2,2] + log_conf_mat[1,2])
rw_classifier_results <- data_frame(Method = "Logistic Regression with prediction rounded", Specificity = logreg_spec,
                                    False_Negatives = log_conf_mat[1,2],
                                    False_Positives = log_conf_mat[2,1])

# get the specificity to be 1
y_hat_test_log_spec <- as.numeric(predict(logreg, newdata=rw_comb_test, type = 'response')>=.2)
confusionMatrix(as.factor(y_hat_test_log_spec), as.factor(rw_comb_test$class))
log_spec_conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat_test_log_spec), as.factor(rw_comb_test$class)))

logreg_spec_adjusted <- log_spec_conf_mat[2,2] / (log_spec_conf_mat[2,2] + log_spec_conf_mat[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Logistc Regression with prediction adjusted for best specification',
                                              Specificity = logreg_spec_adjusted,
                                              False_Negatives = log_spec_conf_mat[1,2],
                                              False_Positives = log_spec_conf_mat[2,1]))

#support vector machine
svm_mod <- ksvm(class~., data = rw_comb_train,
                type="C-svc", kernel="vanilladot", C=1)
y_hat_test_svm <- predict(svm_mod, newdata=rw_comb_test)
confusionMatrix(as.factor(y_hat_test_svm), as.factor(rw_comb_test$class))
svm_conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat_test_svm), as.factor(rw_comb_test$class)))
svm_spec <- svm_conf_mat[2,2]/(svm_conf_mat[1,2] + svm_conf_mat[2,2])

rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Support Vector Machine',
                                              Specificity = svm_spec,
                                              False_Negatives = svm_conf_mat[1,2],
                                              False_Positives = svm_conf_mat[2,1]))


# knn testing for best k
kmax <- 30
specificity <- rep(0,kmax)
for (k in 1:kmax) {
  model <- kknn(formula = class~.,train = rw_comb_train,test=rw_comb_test,
                   k=k) # number of neighbors
  y_hat <- fitted(model)
  conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat), as.factor(rw_comb_test$class)))
  specificity[k] <- conf_mat[2,2]/(conf_mat[1,2]+conf_mat[2,2])
}
specificity
specificity[which.max(specificity)]

# set up knn model with k = 4
knn_model <- kknn(formula = class~.,train = rw_comb_train,test=rw_comb_test,
              k=4) # number of neighbors
y_hat_knn <- fitted(knn_model)
confusionMatrix(as.factor(y_hat_knn), as.factor(rw_comb_test$class))

knn_conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat_knn), as.factor(rw_comb_test$class)))
knn_spec <- knn_conf_mat[2,2] / (knn_conf_mat[2,2] + knn_conf_mat[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'K nearest neighbor with k=5',
                                              Specificity = knn_spec,
                                              False_Negatives = knn_conf_mat[1,2],
                                              False_Positives = knn_conf_mat[2,1]))

#tree for classification

wine_tree <- tree(class~., data=rw_comb_train)
plot(wine_tree)
text(wine_tree, cex = .7)
wine_tree$frame 
y_hat_tree <- round(predict(wine_tree, newdata=rw_comb_test))
confusionMatrix(as.factor(y_hat_tree[,2]), as.factor(rw_comb_test$class))
tree_conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat_tree[,2]), as.factor(rw_comb_test$class)))
tree_spec <- tree_conf_mat[2,2] / (tree_conf_mat[2,2] + tree_conf_mat[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Decision tree classifier',
                                              Specificity = tree_spec,
                                              False_Negatives = tree_conf_mat[1,2],
                                              False_Positives = tree_conf_mat[2,1]))

y_hat_tree_spec <- as.integer(predict(wine_tree, newdata=rw_comb_test)>=.05)
confusionMatrix(as.factor(y_hat_tree_spec[1301:2600]), as.factor(rw_comb_test$class))
tree_conf_mat_adj <- as.matrix(confusionMatrix(as.factor(y_hat_tree_spec[1301:2600]), as.factor(rw_comb_test$class)))
tree_spec_adj <- tree_conf_mat_adj[2,2] / (tree_conf_mat_adj[2,2] + tree_conf_mat_adj[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Decision tree classifier adjusted for best specificity',
                                              Specificity = tree_spec_adj,
                                              False_Negatives = tree_conf_mat_adj[1,2],
                                              False_Positives = tree_conf_mat_adj[2,1]))


#prune the tree
termnodes <- 4
prune.data <- prune.tree(wine_tree, best = termnodes)
plot(prune.data)
text(prune.data)
prune.data$frame
y_hat_tree_prune <- round(predict(wine_tree, newdata=rw_comb_test))
confusionMatrix(as.factor(y_hat_tree[,2]), as.factor(rw_comb_test$class))

tree_conf_mat_prune <- as.matrix(confusionMatrix(as.factor(y_hat_tree[,2]), as.factor(rw_comb_test$class)))
tree_spec_prune <- tree_conf_mat_prune[2,2] / (tree_conf_mat_prune[2,2] + tree_conf_mat_prune[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Decision tree classifier pruned',
                                              Specificity = tree_spec_adj,
                                              False_Negatives = tree_conf_mat_prune[1,2],
                                              False_Positives = tree_conf_mat_prune[2,1]))


# adjusted threshold to .1 instead of rounding to reduced false negatives
# false negatives are slightly reduced but at the expesne of a huge jump in false positives
yhat_wine_prune_spec_adj <- as.integer(predict(prune.data, newdata = rw_comb_test)>=.1)
confusionMatrix(as.factor(yhat_wine_prune_spec_adj[1301:2600]), as.factor(rw_comb_test$class))
tree_conf_mat_prune_adj <- as.matrix(confusionMatrix(as.factor(yhat_wine_prune_spec_adj[1301:2600]),
                                                     as.factor(rw_comb_test$class)))
tree_spec_prune_adj <- tree_conf_mat_prune_adj[2,2] / (tree_conf_mat_prune_adj[2,2] + tree_conf_mat_prune_adj[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Decision tree classifier pruned',
                                              Specificity = tree_spec_prune_adj,
                                              False_Negatives = tree_conf_mat_prune_adj[1,2],
                                              False_Positives = tree_conf_mat_prune_adj[2,1]))





# random forest classification 
set.seed(12)
wine_rf <- randomForest(class~., data = rw_comb_train, mtry = 6,
                        ntrees=1000, importance = TRUE)
y_hat_rf <- predict(wine_rf, newdata =  rw_comb_test)
importance(wine_rf)   
confusionMatrix(as.factor(y_hat_rf), as.factor(rw_comb_test$class))              

rf_conf_mat <- as.matrix(confusionMatrix(as.factor(y_hat_rf), as.factor(rw_comb_test$class))    )

rf_spec <- rf_conf_mat[2,2] / (rf_conf_mat[2,2]+rf_conf_mat[1,2])
rw_classifier_results <- bind_rows(rw_classifier_results, 
                                   data_frame(Method = 'Random Forest classifier',
                                              Specificity = rf_spec,
                                              False_Negatives = rf_conf_mat[1,2],
                                              False_Positives = rf_conf_mat[2,1]))

#tree for quality prediction
wine_tree_acc <- tree(quality~., data=rw_comb_train)
plot(wine_tree_acc)
text(wine_tree_acc)
wine_tree_acc$frame
y_hat_tree_acc <- round(predict(wine_tree_acc, newdata=rw_comb_test))     

tree_acc <- mean(y_hat_tree_acc == rw_comb_test$quality)
tree_rmse <-   sqrt(mean((y_hat_tree_acc - rw_comb_test$quality)^2))

accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Decision tree on test set", Accuracy = tree_acc))
rmse_results <- bind_rows(rmse_results, data_frame(method="Decision tree on test set", RMSE = tree_rmse))
                              



#random forest for quality prediction         
set.seed(12)
wine_rf_qual <- randomForest(quality~., data = rw_comb_train, mtry = 6,
                        ntrees=1000, importance = TRUE)
y_hat_rf_qual <- round(predict(wine_rf_qual, newdata =  rw_comb_test))

importance(wine_rf_qual)   
rf_acc <- mean(y_hat_rf_qual == rw_comb_test$quality)
rf_rmse <-   sqrt(mean((y_hat_rf_qual - rw_comb_test$quality)^2))
accuracy_results <- bind_rows(accuracy_results, data_frame(method = "Random forest on test set", Accuracy = rf_acc))
rmse_results <- bind_rows(rmse_results, data_frame(method="Random forest on test set", RMSE = rf_rmse))

rmse_results %>% knitr::kable()
