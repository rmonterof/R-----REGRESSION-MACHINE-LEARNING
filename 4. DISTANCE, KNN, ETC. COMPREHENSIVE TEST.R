# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# COMPREHENSIVE TEST
# Distance, Knn, Cross-validation, and Generative Models

## Nearest Neighbors

### Distance

library(tidyverse)
library(dslabs)
data(tissue_gene_expression)

#This dataset includes a matrix x:
dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological samples representing seven 
# different tissues. The tissue type is stored in y:

table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance between each observation and 
# stores it in the object d?
  
d <- dist(tissue_gene_expression$x, distance='maximum')
d

d <- dist(tissue_gene_expression)
d

d <- dist(tissue_gene_expression$x)
d

d <- cor(tissue_gene_expression$x)
d

# Q2 Using the dataset from Q1, compare the distances between observations 1 and 2 (both cerebellum), 
# observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# right answer D - 4th option


# Q3 Make a plot of all the distances using the image() function to see if the pattern you observed 
# in Q2 is general.Which code would correctly make the desired plot?
  
image(d)

image(as.matrix(d))

d

image()

 


#Distance-wise, are samples from tissues of the same type closer to each other than tissues of different 
# type?




# COMPREHENSIVE TEST
# Distance, Knn, Cross-validation, and Generative Models

## Nearest Neighbors

### Assessment 2- Nearest Neighbors
# Q1
library(dslabs)
library(tidyverse)
library(caret)
data("heights")

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)

max(F_1)
## [1] 0.619469
which.max(F_1)


# Q2
# Next we will use the same gene expression example used in the Comprehension Check: Distance exercises. You can load it like this:
library(dslabs)
data("tissue_gene_expression")


set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})