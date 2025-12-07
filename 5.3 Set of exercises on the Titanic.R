library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
install.packages('e1071', dependencies=TRUE)
library(broom)

# 3 significant digits
options(digits = 3)
# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)



# Question 1: Training and test sets
# Split titanic_clean into test and training sets - after running the setup code, it should have 891 rows 
# and 9 variables.Set the seed to 42, then use the caret package to create a 20% data partition based on
# the Survived column. Assign the 20% partition to test_set and the remaining 80% partition to train_set.
# Q: How many observations are in the training set? Q: How many observations are in the test set? 
# Q :What proportion of individuals in the training set survived?

set.seed(42, sample.kind = 'Rounding') # if R version >= 3.6
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]
nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)


# Question 2: Baseline prediction by guessing the outcome
# The simplest prediction method is randomly guessing the outcome without using additional predictors. 
# These methods will help us determine whether our machine learning algorithm performs better than chance.
# How accurate are two methods of guessing Titanic passenger survival?
# Set the seed to 3. For each individual in the test set, randomly guess whether that person survived or 
# not. Assume that each person has an equal chance of surviving or not surviving.

# Q: What is the accuracy of this guessing method?

set.seed(3, sample.kind = 'Rounding') # if R version >= 3.6
guess_ <- sample(c(0,1), nrow(test_set), replace = TRUE)
test_set %>% 
  filter(Survived == guess_) %>%
  summarize(n() / nrow(test_set))
# guess with equal probability of survival
#guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
#mean(guess == test_set$Survived)


# Question 3a: Predicting survival by sex
# Use the training set to determine whether members of a given sex were more likely to survive or die. 
# Apply this insight to generate survival predictions on the test set.

train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1))
# Q: What proportion of training set females survived? A: 0.731
# Q: What proportion of training set males survived? A: 0.197


# Question 3b: Question 3b: Predicting survival by sex
# Use the training set to determine whether members of a given sex were more likely to survive or die. 
# Apply this insight to generate survival predictions on the test set.
# Predict survival using sex on the test set: if the survival rate for a sex is over 0.5, predict survival 
# for all individuals of that sex, and predict death if the survival rate for a sex is under 0.5.

# What is the accuracy of this sex-based prediction method on the test set?

sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy


# Question 4a: Predicting survival by passenger class
# In which class(es) (Pclass) were passengers more likely to survive than die?

survival_class <- titanic_clean %>%
  group_by(Pclass) %>%
  summarize(PredictingSurvival = ifelse(mean(Survived == 1) >=0.5, 1, 0))
survival_class


#  Question 4b: Predicting survival by passenger class
#  Predict survival using passenger class on the test set: predict survival if the survival rate for a class is over 0.5, otherwise predict death.

# What is the accuracy of this class-based prediction method on the test set?
  
class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy

# Question 4c: Predicting survival by passenger class
# Group passengers by both sex and passenger class.
# Which sex and class combinations were more likely to survive than die?

survival_class <- titanic_clean %>%
  group_by(Sex, Pclass) %>%
  summarize(PredictingSurvival = ifelse(mean(Survived == 1) > 0.5, 1, 0))
survival_class

# or -right answer-

train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)


# Question 4d: What is the accuracy of this sex- and class-based prediction method on the test set?
# Predict survival using both sex and passenger class on the test set. 
# Predict survival if the survival rate for a sex/class combination is over 0.5, otherwise predict death.
# What is the accuracy of this sex- and class-based prediction method on the test set?

test_set %>%
  inner_join(survival_class, by=c('Sex', 'Pclass')) %>%
  summarize(PredictingSurvival = mean(Survived == PredictingSurvival))


# Question 5a: Confusion matrix
# Use the confusionMatrix() function to create confusion matrices for the sex model, class model, 
# and combined sex and class model. You will need to convert predictions and survival status to factors 
# to use this function.

# What is the "positive" class used to calculate confusion matrix metrics?

# Confusion Matrix: sex model
# tidy function dplyr or ggplot2
library(broom)
sex_model <- train_set %>%
  group_by(Sex) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set1 <- test_set %>%
  inner_join(sex_model, by = 'Sex')
cm1 <- confusionMatrix(data = factor(test_set1$Survived_predict), reference = factor(test_set1$Survived))
cm1 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm1 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate

# Confusion Matrix: class model
class_model <- train_set %>%
  group_by(Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set2 <- test_set %>%
  inner_join(class_model, by = 'Pclass')
cm2 <- confusionMatrix(data = factor(test_set2$Survived_predict), reference = factor(test_set2$Survived))
cm2 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm2 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate
# Confusion Matrix: sex and class model
sex_class_model <- train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived_predict = ifelse(mean(Survived == 1) > 0.5, 1, 0))
test_set3 <- test_set %>%
  inner_join(sex_class_model, by=c('Sex', 'Pclass'))
cm3 <- confusionMatrix(data = factor(test_set3$Survived_predict), reference = factor(test_set3$Survived))
cm3 %>%
  tidy() %>%
  filter(term == 'sensitivity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'specificity') %>%
  .$estimate
cm3 %>%
  tidy() %>%
  filter(term == 'balanced_accuracy') %>%
  .$estimate

# Question 5b: Confusion matrix
# What is the maximum value of balanced accuracy from Q5a?

confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))

# Question 6: F1 scores
# Use the F_meas function to calculate F1 scores for the sex model, class model, and combined sex and 
# class model. You will need to convert predictions to factors to use this function.

# Which model has the highest F1 score?
  
F_meas(data=factor(test_set1$Survived), reference = factor(test_set1$Survived_predict))
F_meas(data=factor(test_set2$Survived), reference = factor(test_set2$Survived_predict))
F_meas(data=factor(test_set3$Survived), reference = factor(test_set3$Survived_predict))

# or -also right-
F_meas(data = factor(sex_model), reference = test_set$Survived)
F_meas(data = factor(class_model), reference = test_set$Survived)
F_meas(data = factor(sex_class_model), reference = test_set$Survived)


# 5.3.2 Titanic Exercises, Part 2
# Question 7: Survival by fare - LDA and QDA
#  Train a model using linear discriminant analysis (LDA) with the caret lda method using Fare as the 
# only predictor. What is the accuracy on the test set for the LDA model?
library(MASS)
fit_lda <- train(Survived ~ Fare, data = train_set, method = 'lda')
Survived_hat <- predict(fit_lda, test_set)
mean(test_set$Survived == Survived_hat)


# Set the seed to 1. Train a model using quadratic discriminant analysis (QDA) with the caret qda method 
# using fare as the only predictor.

# What is the accuracy on the test set for the QDA model?
fit_qda <- train(Survived ~ Fare, data = train_set, method = 'qda')
Survived_hat <- predict(fit_qda, test_set)
mean(test_set$Survived == Survived_hat)

# or - right answer-

set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)


# Question 8: Logistic regression models
# Train a logistic regression model with the caret glm method using age as the only predictor. 
# What is the accuracy on the test set using age as the only predictor?

set.seed(1, sample.kind = "Rounding")set.seed(1, sample.kind = "Rounding")
fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)

# or
train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
glm_preds_age <- predict(train_glm_age, test_set)
mean(glm_preds_age == test_set$Survived)

# Train a logistic regression model with the caret glm method using four predictors: sex, class, fare, and age. What is the accuracy on the test set using these four predictors?

set.seed(1, sample.kind = "Rounding")
fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
mean(survived_hat_b == test_set$Survived)

# or
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
mean(glm_preds == test_set$Survived)

# Train a logistic regression model with the caret glm method using all predictors. 
# Ignore warnings about rank-deficient fit. What is the accuracy on the test set using all predictors?

set.seed(1, sample.kind = "Rounding")  
str(train_set)
fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
mean(survived_hat_c == test_set$Survived)

# or


#set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") if using R 3.6 or later
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
glm_all_preds <- predict(train_glm_all, test_set)
mean(glm_all_preds == test_set$Survived)


# Question 9a: kNN model
# Set the seed to 6. Train a kNN model on the training set using caret. Try tuning with k = seq(3, 51, 2). 
# What is the optimal value of the number of neighbors k?

set.seed(6, sample.kind = "Rounding")
# Method below doesn't give same result as EdX (though it is correct)
# ks <- seq(3,51,2)
# res_knn9a <- sapply(ks, function(k) {
#     fit_knn9a <- knn3(Survived ~ ., data = train_set, k = k)
#     survived_hat <- predict(fit_knn9a, train_set, type = "class") %>% factor(levels = levels(train_set$Survived))
#     cm_test <- confusionMatrix(data = survived_hat, reference = train_set$Survived)
#     cm_test$overall["Accuracy"]
# })
# ks[which.max(res_knn9a)]
# Other method using train function
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune

# or
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune


# Question 9b: kNN model
# Plot the kNN model to investigate the relationship between the number of neighbors and accuracy on the
# training set.
# Of these values of  k , which yields the highest accuracy?

ggplot(fit_knn9a)


# Question 9c: kNN model
# What is the accuracy of the kNN model on the test set?

set.seed(6, sample.kind = "Rounding")
survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

# or
knn_preds <- predict(train_knn, test_set)
mean(knn_preds == test_set$Survived)

# Question 10: Cross-validation
# Set the seed to 8 and train a new kNN model. Instead of the default training control, use 10-fold 
# cross-validation where each partition consists of 10% of the total.

# What is the optimal value of k using cross-validation? What is the accuracy on the test set using the 
# cross-validated kNN model?

set.seed(8, sample.kind = "Rounding")
fit_knn10 <- train(Survived ~ ., 
                   data=train_set, 
                   method = "knn",
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   trControl = trainControl(method = "cv", number=10, p=0.9))
fit_knn10
survived_hat <- predict(fit_knn10, test_set)
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]

# or
knn_cv_preds <- predict(train_knn_cv, test_set)
mean(knn_cv_preds == test_set$Survived)


# Question 11a: Classification tree model
# Set the seed to 10. Use caret to train a decision tree with the rpart method. Tune the complexity 
# parameter with cp = seq(0, 0.05, 0.002).

# What is the optimal value of the complexity parameter (cp)?

set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
train_rpart <- train(Survived ~ ., 
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
train_rpart$bestTune

#Accuracy
rpart_preds <- predict(train_rpart, test_set)
mean(rpart_preds == test_set$Survived)


# Question 11b: Classification tree model
# Inspect the final model and plot the decision tree. Which variables are used in the decision tree?
# Select ALL that apply.

fit_rpart11$finalModel
plot(fit_rpart11$finalModel, margin=0.1)
text(fit_rpart11$finalModel, cex = 0.75)


# Question 11c: Classification tree model
# Using the decision rules generated by the final model, predict whether the following individuals would
# survive.


# Question 12: Random forest model
# Set the seed to 14. Use the caret train() function with the rf method to train a random forest. 
# Test values of mtry = seq(1:7). Set ntree to 100.

# What mtry value maximizes accuracy?
# What is the accuracy of the random forest model on the test set?
# Use varImp on the random forest model object to determine the importance of various predictors to the
# random forest model.
# What is the most important variable?


set.seed(14, sample.kind = 'Rounding')
fit12_rf <- train(Survived ~., 
                  data = train_set,
                  method = "rf", 
                  tuneGrid = data.frame(mtry = seq(1, 7)), 
                  ntree = 100)
fit12_rf$bestTune
survived_hat <- predict(fit12_rf, test_set)
mean(survived_hat == test_set$Survived)
varImp(fit12_rf)


