# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Linear Regression for Prediction

### Linear Regression for Prediction

library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
     filter(childNum == 1 & gender == "male") %>%
     select(father, childHeight) %>%
     rename(son = childHeight)

# seed was not set so values may be different
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

### Predict Function

y_hat <- predict(fit, test_set)

# seed was not set so values may be different
mean((y_hat - test_set$son)^2)

?predict.lm
?predict.glm

### Regression for a Categorical Outcome

library(dslabs)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>%
     filter(round(height) == 66) %>%
     summarize(mean(sex == "Female"))

heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) %>%
     ggplot(aes(x, prop)) +
     geom_point()

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
     lm(y ~ height, data = .)

p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)

### Logistic Regression

heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) %>%
     ggplot(aes(x, prop)) +
     geom_point() + 
     geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

p <- seq(0.01,.99,len=100)
qplot(p, log( p/(1-p) ), geom="line")

glm_fit <- train_set %>% 
     mutate(y = as.numeric(sex == "Female")) %>%
     glm(y ~ height, data=., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>% 
     mutate(x = round(height)) %>%
     group_by(x) %>%
     filter(n() >= 10) %>%
     summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
     mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
     ggplot(aes(x, prop)) +
     geom_point() +
     geom_line(data = logistic_curve,
               mapping = aes(x, p_hat), lty = 2)


y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)

data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
     mutate(logistic = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x),
            regression = lm_fit$coef[1] + lm_fit$coef[2]*x) %>%
     gather(method, p_x, -x) %>%
     ggplot(aes(x, p_x, color = method)) + 
     geom_line() +
     geom_hline(yintercept = 0.5, lty = 5)

### Case Study: 2 or 7

library(tidyverse)
library(dslabs)
data("mnist_27")

mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
     expand.grid(Row=1:28, Column=1:28) %>%  
          mutate(label=titles[i],  
                 value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
     geom_raster() + 
     scale_y_reverse() +
     scale_fill_gradient(low="white", high="black") +
     facet_grid(.~label) + 
     geom_vline(xintercept = 14.5) +
     geom_hline(yintercept = 14.5)

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
     expand.grid(Row=1:28, Column=1:28) %>%  
          mutate(label=titles[i],  
                 value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) + 
     geom_raster() + 
     scale_y_reverse() +
     scale_fill_gradient(low="white", high="black") +
     facet_grid(.~label) + 
     geom_vline(xintercept = 14.5) +
     geom_hline(yintercept = 14.5)

fit <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")

p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
library(caret)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

mnist_27$true_p %>%
     ggplot(aes(x_1, x_2, fill=p)) +
     geom_raster() 

mnist_27$true_p %>%
     ggplot(aes(x_1, x_2, z = p, fill = p)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
     stat_contour(breaks=c(0.5), color="black")

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>%
     mutate(p_hat = p_hat) %>%
     ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
     geom_raster() +
     scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
     stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit, newdata = mnist_27$true_p)
mnist_27$true_p %>%
     mutate(p_hat = p_hat) %>%
     ggplot() +
     stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
     geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 



### Comprehension Check: 
# Section 3: Linear Regression for Prediction, Smoothing, and Working with Matrices
# 3.1: Linear Regression for Prediction

library(tidyverse)
library(caret)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
        data.frame() %>% setNames(c("x", "y"))

# Q1
set.seed(1)
rmse <- replicate(100, {
        test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
        train_set <- dat %>% slice(-test_index)
        test_set <- dat %>% slice(test_index)
        fit <- lm(y ~ x, data = train_set)
        y_hat <- predict(fit, newdata = test_set)
        sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

# rmse = root mean square error


# Q2
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
        Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
        dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
                data.frame() %>% setNames(c("x", "y"))
        rmse <- replicate(100, {
                test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
                train_set <- dat %>% slice(-test_index)
                test_set <- dat %>% slice(test_index)
                fit <- lm(y ~ x, data = train_set)
                y_hat <- predict(fit, newdata = test_set)
                sqrt(mean((y_hat-test_set$y)^2))
        })
        c(avg = mean(rmse), sd = sd(rmse))
})
res



# Q4
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
        data.frame() %>% setNames(c("x", "y"))

# Note what happens to RMSE - set the seed to 1 as before.

set.seed(1)
rmse <- replicate(100, {
        test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
        train_set <- dat %>% slice(-test_index)
        test_set <- dat %>% slice(test_index)
        fit <- lm(y ~ x, data = train_set)
        y_hat <- predict(fit, newdata = test_set)
        sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

# Q5
# C right answer

# Q6 - Create a data set using the following code.
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
        data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Which of the three models performs the best (has the lowest RMSE)?

# A. x_1 B. x_2 C. x_1 and x_2

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))


# Q8 Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
        data.frame() %>% setNames(c("y", "x_1", "x_2"))




o
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
        
        y <- rbinom(n, 1, p)
        f_0 <- rnorm(n, mu_0, sigma_0)
        f_1 <- rnorm(n, mu_1, sigma_1)
        x <- ifelse(y == 1, f_1, f_0)
        
        test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
        
        list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
             test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# Note that we have defined a variable x that is predictive of a binary outcome y: 
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# Set the seed to 1, then use the make_data() function defined above to generate 25 different datasets 
# with mu_1 <- seq(0, 3, len=25). Perform logistic regression on each of the 25 different datasets 
# (predict 1 if p>0.5) and plot accuracy (res in the figures) vs mu_1 (delta in the figures)


#The correct one:
set.seed(1)
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
        dat <- make_data(mu_1 = d)
        fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
        y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
        mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)




