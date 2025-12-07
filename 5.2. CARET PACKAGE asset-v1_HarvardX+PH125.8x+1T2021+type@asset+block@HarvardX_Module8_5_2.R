# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Classification with More than Two Classes and the Caret Package

## Caret Package

### Caret Package

# http://topepo.github.io/caret/available-models.html
# http://topepo.github.io/caret/train-models-by-tag.html

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

### Tuning Parameters with Caret

getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

train_knn$bestTune

train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
     ggplot(aes(x = k, y = Accuracy)) +
     geom_line() +
     geom_point() +
     geom_errorbar(aes(x = k, 
                       ymin = Accuracy - AccuracySD, 
                       ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
     tmp <- mnist_27$true_p
     if(!is.null(p_hat)){
          tmp <- mutate(tmp, p=p_hat)
     }
     tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
          geom_raster(show.legend = FALSE) +
          scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
          stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess", 
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

# Comprehension Check: Caret Package
# Q1 Load the rpart package and then use the caret::train() function with method = "rpart" to fit a 
# classification tree to the tissue_gene_expression dataset. Try out cp values of seq(0, 0.1, 0.01). 
# Plot the accuracies to report the results of the best model. Set the seed to 1991.

# Which value of cp gives the highest accuracy?
library(caret)
library(dslabs)
set.seed(1991, sample.kind = "Rounding")
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)  

# Q2 Note that there are only 6 placentas in the dataset. By default, rpart requires 20 observations 
# before splitting a node. That means that it is difficult to have a node in which placentas are the 
# majority. Rerun the analysis you did in Q1 with caret::train(), but this time with method = "rpart" 
# and allow it to split any node by using the argument control = rpart.control(minsplit = 0). 
# Look at the confusion matrix again to determine whether the accuracy increases. 
# Again, set the seed to 1991.

# What is the accuracy now?

library(caret)                    
library(rpart)
library(dslabs)
data("tissue_gene_expression")

# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)


# Q3 Plot the tree from the best fitting model of the analysis you ran in Q5.
# Which gene is at the first split?
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

# Q4 What value of mtry maximizes accuracy?
set.seed(1991, sample.kind = "Rounding")
library(randomForest)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)

#Q5 Use the function varImp() on the output of train() and save it to an object called imp:
imp <- #BLANK
imp <- varImp(fit)
imp
# What should replace #BLANK in the code above? Do not include spaces in your answer.
## rf variable importance
## 
##   only 20 most important variables shown (out of 500)
## 
##          Overall
## GPA33     100.00
## BIN1       64.65
## GPM6B      62.35
## KIF2C      62.15
## CLIP3      52.09
## COLGALT2   46.48
## CFHR4      35.03
## SHANK2     34.90
## TFR2       33.61
## GALNT11    30.70
## CEP55      30.49
## TCN2       27.96
## CAPN3      27.52
## CYP4F11    25.74
## GTF2IRD1   24.89
## KCTD2      24.34
## FCN3       22.68
## SUSD6      22.24
## DOCK4      22.02
## RARRES2    21.53


#Q6 The rpart() model we ran above in Q2 produced a tree that used just seven predictors. 
# Extracting the predictor names is not straightforward, but can be done. If the output of the call 
# to train was fit_rpart, we can extract the names like this:

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
## [1] "GPA33"  "CLIP3"  "CAPN3"  "CFHR4"  "CES2"   "HRH1"   "B3GNT4"


data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
        mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
        filter(term %in% tree_terms)

