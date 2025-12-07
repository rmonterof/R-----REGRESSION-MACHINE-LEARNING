# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Model Fitting and Recommendation Systems

## Case Study: MNIST

### Case Study: MNIST

library(tidyverse)
library(dslabs)
mnist <- read_mnist()

names(mnist)

dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$train$images), 1000)
x_test <- mnist$train$images[index,]
y_test <- factor(mnist$train$labels[index])

### Preprocessing MNIST Data

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

### Model Fitting for MNIST Data

colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index, col_index], y[index],
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], 
                   y, 
                   method = "Rborist", 
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index],y, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
     image(matrix(x_test[i,], 28, 28)[, 28:1], 
           main = paste("Our prediction:", y_hat_rf[i]),
           xaxt="n", yaxt="n")
}

### Variable Importance

library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
     image(matrix(x_test[i,], 28, 28)[, 28:1], 
           main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2)," but is a ",y_test[i]),
           xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
     image(matrix(x_test[i,], 28, 28)[, 28:1], 
           main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2), " but is a ",y_test[i]),
           xaxt="n", yaxt="n")
}

### Ensembles

p_rf <- predict(fit_rf, x_test[,col_index])$census  
p_rf<- p_rf / rowSums(p_rf)
p_knn  <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)



# Section 6: Model Fitting and Recommendation Systems 
# 6.1: Case Study: MNIST
# Comprehension Check: Ensembles
# For these exercises we are going to build several machine learning models for the mnist_27 dataset and
# then build an ensemble. Each of the exercises in this comprehension check builds on the last.
# Use the training set to build a model with several of the models available from the caret package. 
# We will test out all of the following models in this exercise:
  
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
              "gamboost",  "gamLoess", "qda", 
              "knn", "kknn", "loclda", "gam",
              "rf", "ranger",  "wsrf", "Rborist", 
              "avNNet", "mlp", "monmlp",
              "adaboost", "gbm",
              "svmRadial", "svmRadialCost", "svmRadialSigma")

# Apply all of these models using train() with all the default parameters. You may need to install some 
# packages. Keep in mind that you will probably get some warnings. Also, it will probably take a while 
# to train all of the models - be patient! Run the following code to train the various models:
  
library(caret)
library(dslabs)
library(tidyverse)
library(naivebayes)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


# Did you train all of the models? YES


# Q2 Now that you have all the trained models in a list, use sapply() or map() to create a matrix of 
# predictions for the test set. You should end up with a matrix with length(mnist_27$test$y) rows and 
# length(models) columns.

# What are the dimensions of the matrix of predictions?
# Number of rows: 200 Number of columns: 23

models_y_hat <- sapply(fits, function(fit_model){
  predict(fit_model, mnist_27$test)
})
dim(models_y_hat)

# Q3 Now compute accuracy for each model on the test set. Report the mean accuracy across all models.

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)


# Q4 Next, build an ensemble prediction by majority vote and compute the accuracy of the ensemble.
# What is the accuracy of the ensemble? - 0.845

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

## Q5 In Q3, we computed the accuracy of each method on the training set and noticed that the individual
# accuracies varied. How many of the individual methods do better than the ensemble? 1

# Which individual methods perform better than the ensemble?

ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

# Q6 What is the mean of these training set accuracy estimates?
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

## Q7
# Now let's only consider the methods with an estimated accuracy of greater than or equal to 0.8 when 
# constructing the ensemble. What is the accuracy of the ensemble now?
# !!! NB: It should be mentionned that the Ensemble is decided by majority vote of models

ind <- acc_hat >= 0.8
votes <- rowMeans(models_y_hat[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

########################################################

## 6.2.3 Comprehension Check: Recommendation Systems
# The following exercises all work with the movielens data, which can be loaded using the following code:

library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")


# Q1
# Compute the number of ratings for each movie and then plot it against the year the movie came out 
# using a boxplot for each year. Use the square root transformation on the y-axis (number of ratings) 
# when creating your plot. What year has the highest median number of ratings?

movielens %>%
  group_by(movieId) %>%
  summarize(n = n_distinct(userId), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Q2
# We see that, on average, movies that came out after 1993 get more ratings. We also see that with newer
# movies, starting in 1993, the number of ratings decreases with year: the more recent a movie is, the 
# less time users have had to rate it. Among movies that came out in 1993 or later, select the top 25 
# movies with the highest average number of ratings per year (n/year), and calculate the average rating of
# each of them. To calculate number of ratings per year, use 2018 as the end year. 

# What is the average rating for the movie The Shawshank Redemption?
# What is the average number of ratings per year for the movie Forrest Gump?

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 


# Q3 
# From the table constructed in Q2, we can see that the most frequently rated movies tend to have above 
# average ratings. This is not surprising: more people watch popular movies. To confirm this, stratify 
# the post-1993 movies by ratings per year and compute their average ratings. To calculate number of 
# ratings per year, use 2018 as the end year. Make a plot of average rating versus ratings per year and 
# show an estimate of the trend.

# What type of trend do you observe?

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()


# Q4
# Suppose you are doing a predictive analysis in which you need to fill in the missing ratings with some
# value. Given your observations in the exercise in Q3, which of the following strategies would be most 
# appropriate?

# A. Fill in the missing values with the average rating across all movies.
# B. Fill in the missing values with 0.  
# C. Fill in the missing values with a lower value than the average rating across all movies. <==
# D. Fill in the value with a higher value than the average rating across all movies.
# E. None of the above.

# Explanation: Because a lack of ratings is associated with lower ratings, it would be most appropriate 
# to fill in the missing value with a lower value than the average. You should try out different values 
# to fill in the missing value and evaluate prediction in a test set.


# Q5
# The movielens dataset also includes a time stamp. This variable represents the time and data in which 
# the rating was provided. The units are seconds since January 1, 1970. Create a new column date with 
# the date.

# Which code correctly creates this new column?
  
# A. movielens <- mutate(movielens, date = as.date(timestamp))
# B. movielens <- mutate(movielens, date = as_datetime(timestamp)) <==
# C. movielens <- mutate(movielens, date = as.data(timestamp))
# D. movielens <- mutate(movielens, date = timestamp)

library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))
movielens


# Q6
# Compute the average rating for each week and plot this average against day.
#  Hint: use the round_date function before you group_by. Q: What type of trend do you observe?

# A. There is strong evidence of a time effect on average rating.
# B. There is some evidence of a time effect on average rating. <==
# C. There is no evidence of a time effect on average rating.

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()


# Q7. Consider again the plot you generated in Q6.
# If we define du,i as the day for user's  u rating of movie  i, which of the following models is most 
# appropriate?
  
# A.  Yu,i=??+bi+bu+du,i+??u,i
# B.  Yu,i=??+bi+bu+du,i??+??u,i
# C.  Yu,i=??+bi+bu+du,i??i+??u,i
# D.  Yu,i=??+bi+bu+f(du,i)+??u,i, with  f a smooth function of  du,i  <==


# Q8
# The movielens data also has a genres column. This column includes every genre that applies to the 
# movie. Some movies fall under several genres. Define a category as whatever combination appears in this
# column. Keep only categories with more than 1,000 ratings. Then compute the average and standard error 
# for each category. Plot these as error bar plots.

# Which genre has the lowest average rating? - Comedy

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Q9
# The plot you generated in Q8 shows strong evidence of a genre effect. Consider this plot as you answer 
# the following question.If we define  gu,i as the day for user's  u rating of movie  i, which of the 
# following models is most appropriate?
  
# A.Yu,i=??+bi+bu+gu,i+??u,i
# B.Yu,i=??+bi+bu+gu,i??+??u,i
# C.Yu,i=??+bi+bu+???k=1Kxu,i??k+??u,i, with x^k_{u,i}$ = 1 if  gu,i is genre  k <==
# D.Yu,i=??+bi+bu+f(gu,i)+??u,i, with  f a smooth function of  gu,i
