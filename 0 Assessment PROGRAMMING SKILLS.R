library(tidyverse)
library(dslabs)
library(dplyr)
data(heights)
heights

#Q1
class(heights)
class(heights$sex)
class(heights$height)

class("Male")
class(75.00000)

#Q2
nrow(heights)

#Q3
heights$height[777]

#Q4
heights$sex[777]
heights[1, 777]
heights[777,1]

heights[777,]

# Q5: Maximum and Minimum --- What is the maximum height in inches?
max(heights$height)


# Which row has the minimum height?
min(heights$height)
heights[heights$height == min(heights$height),]

which.min(heights$height)

# Q6: Summary Statistics ----What is the mean height in inches?
mean(heights$height)
median(heights$height)

# Q7 What proportion of individuals in the dataset are male?
mean(heights$sex == "Male")

# Q8 Q8: Conditional Statements - 2 INDEXING
# How many individuals are taller than 78 inches (roughly 2 meters)?

ind<-heights$height>78 
sum(ind)


# Q9: Conditional Statements - 3 ---How many females in the dataset are taller than 78 inches?
x <- heights$height[heights$sex == "Female"]
fem_78<-x>78
sum(fem_78)

#or
sum(heights$sex == "Female" & heights$height > 78)
