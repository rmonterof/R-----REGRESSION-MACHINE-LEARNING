
library()
library(ti)
library(tidyverse)
library(ggplot2)
install.packages("dslabs")
library(dslabs)
tidyverse
tidyverse_packages()
installed.packages()
library(dslabs)
library(dplyr)
library(ggplot2)
a<- 1
b<- 1
c<- -1
a
print(a)
ls()
# solving the quadratic equation
(-b + sqrt(b^2 - 4*a*c) ) / ( 2*a )
(-b - sqrt(b^2 - 4*a*c) ) / ( 2*a )
ls()
help("log")
?log
args(log)
# You can change the default values by simply assigning another object:
log(8, base = 2)
2^3
help("+")
?"+"
help(">")
ls
co2
co2
pi
Inf

log(9)
base=exp(1)
args(log)
log(8, base=2)
co2
n<-100
n*(n+1)/2

library(dslabs)
data("murders")
class(ls)
class(seq)
class(murders)
str(murders)
head(murders)
murders$population
names(murders)
#vectors = murders$population

# every variable has a class. For example, the class can be a character, 
#numeric or logical. The function class() can be used to determine the class of 
#an object.

pop <- murders$population
length(pop)
class(murders$region) #factor for cathegorical data. In R we store levels 
#as integers
levels(murders$region)

1:5
seq(1,5)
levels(murders$region)
levels(murders$abb)

log(1024, base=4)
(2*x)^2-x-4=0
data(movielens)
str(movielens)
levels(movielens)
names(movielens)
class(title)
str(movielens)
nlevels(movielens$genres)
(sqrt(2)-4)/2*2
1/4+(1/4)*sqrt(33)
1/4+(-1/4)*sqrt(33)
  
# 2.6 VECTORS
# We may create vectors of class numeric or character with the concatenate 
#function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

class(1:10)
class(seq(1, 10, 0.5))

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements 
#of a vector
codes[2]
codes[c(1,3)]
codes[1:2]
codes[1:3]

# If the entries of a vector are named, they may be accessed by referring to 
# their name
codes["canada"]
codes[c("egypt","italy")]

# In general, coercion is an attempt by R to be flexible with data types by guessing what was meant when an entry does not match the expected. For example, when defining x as
#>x <- c(1, "canada", 3)
#R coerced the data into characters. It guessed that because you put a character string in the vector, you meant the 1 and 3 to actually be character strings "1" and "3".
#The functionas.character() turns numbers into characters.
#The function as.numeric() turns characters into numbers.
#In R, missing data is assigned the value NA.


x <- 1:5
y <- as.character(x)
y
#> [1] "1" "2" "3" "4" "5"
# You can turn it back with as.numeric:
  
  as.numeric(y)
#> [1] 1 2 3 4 5
  
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
codes

# 2.9 SORTING
library(dslabs)
data(murders)
sort(murders$total)

# However, this does not give us information about which states have which 
# murder totals. For example, we don't know which state had 1257.

x <- c(31, 4, 15, 92, 65)
sort(x)
# Rather than sort the input vector, the function order returns the index that 
# sorts input vector:
index <- order(x)
x[index]
# This is the same output as that returned by sort(x). If we look at this index,
# we see why it works:
x
order(x)
murders$state[1:6]
murders$abb[1:6]
ind <- order(murders$total) 
murders$abb[ind]  

# 2.9.3 max and which.max
max(murders$total)
i_max <- which.max(murders$total)
murders$state[i_max]
i_min <- which.min(murders$total)
murders$state[i_min]

# 2.9.4 rank
x <- c(31, 4, 15, 92, 65)
rank(x)

# 2.9.5 Beware of recycling
x <- c(1,2,3)
y <- c(10, 20, 30, 40, 50, 60, 70)
x+y

murders
help(ind)
help("!")

# 2.11 Vector Arithmetic
# California had the most murders, but does this mean it is the most dangerous 
# state? What if it just has many more people than any other state? 
# We can quickly confirm that California indeed has the largest population

# ==>
murders$state[which.max(murders$population)]

# What we really should be computing is the murders per capita. 
# The reports we describe in the motivating section used murders per 100,000 
# as the unit.

murder_rate <- murders$total / murders$population * 100000

# Once we do this, we notice that California is no longer near the top of the 
# list. In fact, we can use what we have learned to order the states by murder 
# rate:

murders$abb[order(murder_rate)]
murders$state[order(murder_rate)]
murders$state[order(murder_rate, decreasing=TRUE)]
murder_rate<-(murders$total/murders$population)*100000
murder_rate
murders$state[order(murder_rate, decreasing=TRUE)]

#Assesment L1 A2
x <- c(2, 43, 27, 96, 18)
sort(x)
order(x)
rank(x)
x
min(x)
which.min(x)
max(x)
which.max(x)

# Mandi, Amy, Nicole, and Olivia all ran different distances in different time 
#intervals. Their distances (in miles) and times (in minutes) are as follows:
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
1/60
time*0.01666667
# speed= distance / time
distance / (time*0.01666667)

#2.13 INDEXING
# Key Points: We can use logicals to index vectors.
# Using the function >sum()on a logical vector returns the number of entries 
# that are true. The logical operator "&" makes two logicals true only when they 
# are both true.
 
Code
# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is 
# less than or equal to 0.71

index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

# 2.13.3 which
ind <- which(murders$state == "California")
murder_rate[ind]

# 2.13.4 match
#to find out the murder rates for several states, say New York, Florida, and 
# Texas, we can use the function match. This function tells us which indexes 
# of a second vector match each of the entries of a first vector:

ind <- match(c("New York", "Florida", "Texas"), murders$state)
ind

# Now we can look at the murder rates:
murder_rate[ind]

# 2.13.5 %in%
# If rather than an index we want a logical that tells us whether or not each 
# element of a first vector is in a second, we can use the function %in%.
c("Boston", "Dakota", "Washington") %in% murders$state

#Advanced: There is a connection between match and %in% through which. 
# To see this, notice that the following two lines produce the same index 
# (although in different order):

match(c("New York", "Florida", "Texas"), murders$state)
which(murders$state%in%c("New York", "Florida", "Texas"))
# BOTH ALGORITHMS ARE *IDENTICAL*

abbs<-c("MA", "ME", "MI", "MO", "MU")
ind<-which(!abbs%in%murders$abb)
abbs[ind]

# 3.2 Basic Data Wrangling
library(dplyr)

CODE# 
# adding a column with mutate
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)
head(murders)

# 3.2.2 Creating Data Frames
# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)


# 3.2. Exercises 
murders <- mutate(murders, rate =  total / population * 100000, rank = (-rate))
# in the solution to the previous exercise we did the following:
  
# Created a table 
my_states <- filter(murders, region %in% c("Northeast", "West") & rate < 1)

# Used select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)

# The pipe %>% permits us to perform both operation sequentially and without 
# having to define an intermediate variable my_states. For example we could 
# have mutated and selected in the same line like this:
  
mutate(murders, rate =  total / population * 100000, rank = (-rate)) %>% 
select(state, rate, rank)

my_states<- murders%>%mutate(rate=(total/population)*100000,rank=rank(-rate))%>%filter(region%in%c("Northeast","West")&rate<1)%>%select(state,rate,rank)
my_states

# 3.3 Basic Plots

Code
# a simple scatterplot of total murders versus population
#scatterplot= esp. diagrama / gráfico de dispersión
x <- murders$population / 10^6
y <- murders$total
plot(x, y)

# a histogram  of murder rates= histograma / diagrama de barras
hist(murders$rate)

# boxplots of murder rates by region = diagramas de cajas / bigotes
boxplot(rate~region, data = murders)

# ????????? SECTION 3 ASSESMENT For questions 1-8, load the dslabs dataset heights:
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

head(heights)

#??? 1 First, determine the average height in this dataset. Then create a logical 
# vector ind with the indices for those individuals who are above average height.
# How many individuals in the dataset are above average height?
ind<-mean(heights$height)
filter(heights, height>ind)

#??? 2 How many individuals in the dataset are above average height and 
# are female?
filter(heights, height>ind&heights$sex=="Female")

#??? 3 If you use mean on a logical ( TRUE/FALSE ) vector, it returns the proportion of
# observations that are TRUE. What proportion of individuals in the dataset are female?
mean(heights$sex=="Female")

#??? 4a Determine the minimum height in the heights dataset.
min(heights$height)

#??? 4b Use the match() function to determine the index of the individual with 
# the minimum height.
which.min(heights$height)

#??? 4c Subset the sex column of the dataset by the index in 4b to determine the
# individual's sex.
# table$datayouwanttoaccess[indexofthedatayouwant]

index<-which.min(heights$height%in% heights$sex)
heights$sex[index]

#??? 5a Determine the maximum height.
min(heights$height)
max(heights$height)

#??? 5b Write code to create a vector x that includes the integers between the
# minimum and maximum heights.
x<-50:82
x

#??? 5c How many of the integers in x are NOT heights in the dataset?
x<-50:82
sum(!x%in%heights$height)

#??? 6 Using the heights dataset, create a new column of heights in centimeters 
# named ht_cm.Save the resulting dataset as heights2.
data(heights)
heights2<-mutate(heights, ht_cm=heights$height*2.54)
print(heights2)

#??? 6a # What is the height in centimeters of the 18th individual (index 18)?
heights2$ht_cm[18]

#??? 6b What is the mean height in centimeters?
mean(heights2$ht_cm)

#??? 7a Create a data frame with only female individuals
# How many females are in the heights2 dataset?
females<-filter(heights2, sex=="Female", 
                            heights2$height, 
                            heights2$ht_cm)
females

#??? 7b What is the mean height of the females in centimeters?
females<-filter(heights2, sex=="Female", 
                heights2$height, 
                heights2$ht_cm)
data.frame(females)
mean(females$ht_cm)

#??? 8 Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. What relationship do
# you see?
  
library(dslabs)
data(olive)
head(olive)

x <- olive$palmitic
y <- olive$palmitoleic
plot(x, y)

#??? 9
hist(olive$eicosenoic)

#??? 10
boxplot(palmitic~region, data = olive)
#??? Interpreting box plots
# https://www.khanacademy.org/math/ap-statistics/summarizing-quantitative-data-ap/stats-box-whisker-plots/v/interpreting-box-plots


murders[25, 1]
murders[2, ]
murders[ , , ]
ls()
str(heights2)
str(murders)
names(murders)
filter(murders)


# 4.2 PROGRAMMING BASICS: CONDITIONALS

# Key Points

# The most common conditional expression in programming is an if-else statement,
# which has the form "if [condition], perform [expression], else perform 
# [alternative expression]".
# The ifelse() function works similarly to an if-else statement, but it is 
# particularly useful since it works on vectors by examining each element of the 
# vector and returning a corresponding answer accordingly.
# The any() function takes a vector of logicals and returns true if any of the 
# entries are true.
# The all() function takes a vector of logicals and returns true if all of the 
# entries are true.

#Code
# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}

# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}

# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

# 4.3 Basic Functions
# Key points
# The R function, called function() tells R you are about to define a new function.
# Functions are objects, so must be assigned a variable name with the arrow 
# operator. The general way to define functions is: (1) decide the function name,
# which will be an object, (2) type function() with your function's arguments in
# parentheses, (3) write all the operations inside brackets.
# Variables defined inside a function are not saved in the workspace.

# Code
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
# my_function <- function(VARIABLE_NAME){
  # perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
# }

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

x

# 4.4. FOR-LOOPS
# Key points
# For-loops perform the same task over and over while changing the variable.  
# They let us define the range that our variable takes, and then changes the
# value with each loop and evaluates the expression every time inside the loop.
# The general form of a for-loop is: "For i in [some range], do operations".  
# This i changes across the range of values and the operations assume i is a 
# value you're interested in computing on.
# At the end of the loop, the value of i is the last value of the range.

# Code
# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a very simple for-loop
 for(i in 1:5){
  print(i)
 }
  # a for-loop for our summation
  m <- 25
  s_n <- vector(length = m) # create an empty vector
  for(n in 1:m){
    s_n[n] <- compute_s_n(n)
  }
  
  # creating a plot for our summation function
  n <- 1:m
  plot(n, s_n)
  
  # a table of values comparing our function to the summation formula
  head(data.frame(s_n = s_n, formula = n*(n+1)/2))
  
  # overlaying our function with the summation formula
  plot(n, s_n)
  lines(n, n*(n+1)/2)
  
# R VISUALIZATION 1.2 DISTRIBUTION
# CODE
# load the dataset
library(dslabs)
data(heights)
  
# make a table of category proportions
prop.table(table(heights$sex))


# HOW TO MAKE Empirical Cumulative Distribution Function PLOT
plot(ecdf(murder_rate), xlab = "murders", ylab = "Fn(murder_rate>10)")

# CDF of a vector
empirical_cdf(dt$x, ubounds=seq(1, 4, by=1.0))

# CDF of column 'x' of dt
empirical_cdf(dt, ubounds=list(x=seq(1, 4, by=1.0)))

# CDF of columns 'x' and 'y' of dt
empirical_cdf(dt, ubounds=list(x=seq(1, 4, by=1.0), y=seq(1, 4, by=1.0)))


# VISUALIZATION 1.3 Normal Distribution
# Code
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
x

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum(x - average)^2)/length(x)

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

data(heights)
x<-heights$height[heights$sex=="Male"]
mean(x>69 & x<=72)

# pnorm
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)

# Examples:
# Suppose IQ's are normally distributed with a mean of 100 and a standard 
# deviation of 15.
# 1. What percentage of people have an IQ less than 125?
pnorm(125, mean = 100, sd = 15, lower.tail=TRUE)
# 2. What percentage of people have an IQ greater than 110?
pnorm(110, mean = 100, sd = 15, lower.tail=FALSE) 
# 3. What percentage of people have an IQ between 110 and 125?
pnorm(125, mean = 100, sd = 15, lower.tail=TRUE)
-pnorm(110, mean = 100, sd = 15, lower.tail=FALSE)

# 1.3 QQ-PLOTS QUANTILE QUANTILE 
#Code
Code
# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x) #standard units
x

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p) 
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

install.packages("titanic")
library("titanic")
head(Titanic)



#2. VISUALIZATION::: GGPLOT2
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)

ggplot(data = murders) # blank slate since no geometry has been defined
murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
print(p) or #p

# 7.4 Aesthetic mappings aes function
murders %>% ggplot() + 
  geom_point(aes(x = population/10^6, y = total))

# Instead of defining our plot from scratch, we can also add a layer to the 
# p object that was defined above as p <- ggplot(data = murders):

p + geom_point(aes(population/10^6, total))

# 2nd label = abb (regions)
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

# Tinkering with arguments
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)


# 7.7 Scales
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total, size = 3) +  
  geom_text(nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") 

p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total, size = 3) +  
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() 

# 7.8 Labels and titles
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total, size = 3) +  
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")


# 7.9 Categories as colors  
p <- ggplot(data = murders)
p <-  murders %>% ggplot(aes(population/10^6, total, label = abb)) +   
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")  
p + geom_point(size = 3, color ="blue")

# to assign color depending on the geographical region
p + geom_point(aes(col=region), size = 3)

# 7.10 Annotation, shapes, and adjustments
r <- murders %>% 
  summarize(rate = sum(total) /  sum(population) * 10^6) %>% 
  pull(rate)
p + geom_point(aes(col=region), size = 3) + 
  geom_abline(intercept = log10(r))

p <- p + scale_color_discrete(name = "Region") 

# 7.11 Add-on packages
ds_theme_set()
library(ggthemes)
p + theme_economist()
theme_fivethirtyeight()


# 7.12 Putting it all together
library(ggthemes)
library(ggrepel)

r <- murders %>% 
summarize(rate = sum(total) /  sum(population) * 10^6) %>%
pull(rate)

murders %>% ggplot(aes(population/10^6, total, label = abb)) +   
geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
geom_point(aes(col=region), size = 3) +
scale_x_log10() +
scale_y_log10() +
xlab("Populations in millions (log scale)") + 
ylab("Total number of murders (log scale)") +
ggtitle("US Gun Murders in 2010") + 
scale_color_discrete(name = "Region") +
theme_economist()


#7.13 Quick plots with qplot
data(murders)
x <- log10(murders$population)
y <- murders$total

data.frame(x = x, y = y) %>% 
  ggplot(aes(x, y)) +
  geom_point()

#The qplot function sacrifices the flexibility provided by the ggplot approach,
# but allows us to generate a plot quickly.
qplot(x, y)

# 7.14 Grids of plots
# There are often reasons to graph plots next to each other. The gridExtra 
# package permits us to do that:

library(gridExtra)
p1 <- qplot(x)
p2 <- qplot(x,y)
grid.arrange(p1, p2, ncol = 2)


# 3.1 Summarizing with dplyr 1)SUMMARIZE Code
library(tidyverse)
library(dslabs)
data(heights)

# compute average and standard deviation for males
s <- heights %>%
  filter(sex == "Male") %>%
  summarize(average = mean(height), standard_deviation = sd(height))
s

# access average and standard deviation from summary table
s$average
s$standard_deviation

# compute median, min and max
heights %>%
  filter(sex == "Male") %>%
  summarize(median = median(height),
            minimum = min(height),
            maximum = max(height))

# alternative way to get min, median, max in base R
quantile(heights$height, c(0, 0.5, 1))

# generates an error: summarize can only take functions that return a single value
heights %>%
  filter(sex == "Male") %>%
  summarize(range = quantile(height, c(0, 0.5, 1)))

#2) The Dot Placeholder
# calculate US murder rate, generating a data frame
us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000)
us_murder_rate

# extract the numeric US murder rate with the dot operator
us_murder_rate %>% .$rate

# calculate and extract the murder rate with one pipe
us_murder_rate <- murders %>%
  summarize(rate = sum(total) / sum(population * 100000)) %>%
  .$rate
us_murder_rate

#3) group_by() function
# libraries and data
library(tidyverse)
library(dslabs)
data(heights)
data(murders)

# compute separate average and standard deviation for male/female heights
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# compute median murder rate in 4 regions of country
murders <- murders %>%
  mutate(murder_rate = total/population * 100000)
murders %>%
  group_by(region) %>%
  summarize(median_rate = median(murder_rate))

#4) Sorting Data Tables
# set up murders object
murders <- murders %>%
  mutate(murder_rate = total/population * 100000)

# arrange by population column, smallest to largest
murders %>% arrange(population) %>% head()

# arrange by murder rate, smallest to largest
murders %>% arrange(murder_rate) %>% head()

# arrange by murder rate in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

# arrange by region alphabetically, then by murder rate within each region
murders %>% arrange(region, murder_rate) %>% head()

# show the top 10 states with highest murder rate, not ordered by rate
murders %>% top_n(10, murder_rate)

# show the top 10 states with highest murder rate, ordered by rate
murders %>% arrange(desc(murder_rate)) %>% top_n(10)



#GAPMINDER 
# load and inspect gapminder data
library(dslabs)
library(tidyverse)
data(gapminder)
head(gapminder)

# compare infant mortality in Sri Lanka and Turkey
gapminder %>% filter(year=="2015" & country %in% c("Sri Lanka", "Turkey")) %>% 
  select(country, infant_mortality)

# Life Expectancy and Fertility Rates
# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

# add color as continent
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color= continent)) +
  geom_point()

# GAPMINDER - FACETING
# facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(continent ~ year)

# facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

# Time Series Plots
# Code: Single time series
# scatterplot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# Code: Multiple time series
# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

# Code: Adding text labels to a plot
# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "0")


# TRANSFORMATIONS
# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# histogram of dollars per day
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")


# STRATIFY AND BOXPLOT geom_boxplot and element_text
# Code: Boxplot of GDP by region
# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Code: The reorder function
# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)


# Code: Enhanced boxplot ordered by median income, scaled, and showing data
# reorder by median income and color by continent
# geom_point function
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)


# COMPARING DISTRIBUTIONS
# add dollars per day variable and define past year
gapminder <- gapminder %>%
mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# 9.7.3 Example: 1970 versus 2010 income distributions (FACET GRID)
# Histogram of income in West versus developing world, 1970 and 2010
past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder %>% 
  filter(year %in% years & !is.na(gdp)) %>%
  mutate(west = ifelse(group == "West", "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") + 
  facet_grid(year ~ west)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

# Code: Income distribution of West versus developing world, only countries
# with data 

# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

# Code: Boxplots of income in West versus developing world, 1970 and 2010
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))


# DENSITY PLOTS
# Code: Faceted smooth density plots
# see the code below the previous video for variable definitions

# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing"))%>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)


# Code: Add new region groups with case_when
# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

# Code: Stacked density plot
# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

# Code: Weighted stacked density plot
# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

# ECOLOGICAL FALLACY
# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)


# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 



5. DATA VISUALIZATION PRINCIPLES
library(tidyverse)
library(dslabs)
library(gridExtra)
# ggplot2 geometries
# Barplots
murders %>% ggplot(aes(region)) + geom_bar()

# We often already have a table with a distribution that we want to present as a barplot. 
# Here is an example of such a table:

data(murders)
tab <- murders %>% 
  count(region) %>% 
  mutate(proportion = n/sum(n))
tab

# We no longer want geom_bar to count, but rather just plot a bar to the height provided
# by the proportion variable. For this we need to provide x (the categories) and y 
# (the values) and use the stat="identity" option.

tab %>% ggplot(aes(region, proportion)) + geom_bar(stat = "identity")

# HISTOGRAMS

heights %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(height)) + 
  geom_histogram()

# If we run the code above, it gives us a message:
  
  # stat_bin() using bins = 30. Pick better value with binwidth.

# We previously used a bin size of 1 inch, so the code looks like this:
  
  heights %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(height)) + 
  geom_histogram(binwidth = 1)

  
# Finally, if for aesthetic reasons we want to add color, we use the arguments described in the help file. We also add labels and a title:
  
  heights %>% 
    filter(sex == "Female") %>% 
    ggplot(aes(height)) +
    geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Male heights in inches") + 
    ggtitle("Histogram")
  

# 8.16.3 Density plots geom_density
# To create a smooth density, we use the geom_density. To make a smooth density plot with the data previously shown as a histogram we can use this code:
    
    heights %>% 
    filter(sex == "Female") %>%
    ggplot(aes(height)) +
    geom_density()
# To fill in with color, we can use the fill argument.
  
  heights %>% 
    filter(sex == "Female") %>%
    ggplot(aes(height)) +
    geom_density(fill="blue")
  
  
# To change the smoothness of the density, we use the adjust argument to multiply the 
# default value by that adjust. For example, if we want the bandwidth to be twice as big we use:
  
  heights %>% 
    filter(sex == "Female") %>%
    ggplot(aes(height)) +
    geom_density(fill="blue", adjust = 2)

  
# 8.16.5 QQ-plots
# For qq-plots we use the geom_qq geometry. From the help file, we learn that we need to specify the sample (we will learn about samples in a later chapter). Here is the qqplot for men heights.
  
  heights %>% filter(sex=="Male") %>%
    ggplot(aes(sample = height)) +
    geom_qq()  
  
# Order categories by a meaningful value
  data(murders)
  murders %>% mutate(murder_rate = total / population * 100000) %>%
      ggplot(aes(state, murder_rate)) +
    geom_bar(stat="identity") +
    coord_flip() +
    theme(axis.text.y = element_text(size = 6)) +
    xlab("")

# reorder function==>
  
  data(murders)
  murders %>% mutate(murder_rate = total / population * 100000) %>%
    mutate(state = reorder(state, murder_rate)) %>%
    ggplot(aes(state, murder_rate)) +
    geom_bar(stat="identity") +
    coord_flip() +
    theme(axis.text.y = element_text(size = 6)) +
    xlab("")

# 5.2 Data Visualization Principles, Part 2  Show the Data
# This brings us to our first principle: show the data. This simple ggplot2 code already generates a more informative plot than the barplot by simply showing all the data points:
  
  heights %>% 
    ggplot(aes(sex, height)) + 
    geom_point() 
  
#  we point out two ways we can improve a plot showing all the points.The first is to 
# add jitter, which adds a small random shift to each point.
# To add jitter, use the  geom_jitter() geometry instead of geom_point()

  heights %>% 
    ggplot(aes(sex, height)) +
    geom_jitter(width = 0.1, alpha = 0.2) 
  
# 10.6.2 Align plots vertically to see horizontal changes and horizontally to see 
# vertical changes

  heights %>% 
    ggplot(aes(height, ..density..)) +
    geom_histogram(binwidth = 1, color="black") +
    facet_grid(sex~.)
  
  heights %>% 
    ggplot(aes(sex, height)) + 
    geom_boxplot(coef=3) + 
    geom_jitter(width = 0.1, alpha = 0.2) +
    ylab("Height in inches")

# 5.3 Data Visualization Principles, Part 3  Slope Charts
# Code: Slope chart
  library(tidyverse)
  library(dslabs)
  data(gapminder)
  
  west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
  
  dat <- gapminder %>%
    filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
  
  dat %>%
    mutate(location = ifelse(year == 2010, 1, 2),
           location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                             location + 0.22, location),
           hjust = ifelse(year == 2010, 1, 0)) %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
    xlab("") +
    ylab("Life Expectancy") 
  
# 5.3 Data Visualization Principles, Part 3  Code: Bland-Altman plot
  library(ggrepel)
  install.packages("ggrepel")
  dat %>%
    mutate(year = paste0("life_expectancy_", year)) %>%
    select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
    mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
           difference = life_expectancy_2015 - life_expectancy_2010) %>%
    ggplot(aes(average, difference, label = country)) +
    geom_point() +
    geom_text_repel() +
    geom_abline(lty = 2) +
    xlab("Average of 2010 and 2015") +
    ylab("Difference between 2015 and 2010")

install.packages("ggrepel")


# 5.3 Data Visualization Principles, Part 3  
# Ease Comparisons: Compared Visual Cues Should Be Adjacent
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)


# Assessment 11 - 4: Making a box plot
data("murders")
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region=reorder(region, rate, FUN=median)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot() +
  geom_point()


library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)
head(us_contagious_diseases)


# CASE STUDY VACCINES
# Code: Tile plot of measles rate by year and state
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))
dat

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")


# Code: Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
avg


# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

str(us_contagious_diseases)
levels(us_contagious_diseases$disease)

# R BASICS Section 4 Assessment
# Write an ifelse() statement that returns 1 if the sex is Female and 2 if the sex is
# Male. What is the sum of the resulting vector?
library(dslabs)
data(heights)

x<-ifelse(heights$sex=="Female", 1, 2)
sum(x)

Question 2

# Write an ifelse() statement that takes the height column and returns the height if it
# is greater than 72 inches and returns 0 otherwise.
# What is the mean of the resulting vector?
z<-ifelse(heights$height>72, z, 0)
z
mean(z)

# Question 3
# Write a function inches_to_ft that takes a number of inches x and returns the
# number of feet. One foot equals 12 inches. What is inches_to_ft(144) ?

inches_to_ft<-heights$height/12
inches_to_ft(144)

# How many individuals in the heights dataset have a height less than 5 feet?
x<-heights$height<(5*12)
sum(x)


# Which of the following are TRUE?
any(TRUE, TRUE, TRUE)
any(TRUE, TRUE, FALSE)
any(TRUE, FALSE, FALSE)
any(FALSE, FALSE, FALSE)
all(TRUE, TRUE, TRUE)
all(TRUE, TRUE, FALSE)
all(TRUE, FALSE, FALSE)
all(FALSE, FALSE, FALSE)

# Question 5 Given an integer x, the factorial of x is called x! and is the product of 
# all integers up to and including x. The factorial() function computes factorials in R. For example,
# factorial(4) returns 4! = 4 × 3 × 2 × 1 = 24 .
# Complete the code above to generate a vector of length m where the 1st entry is 1!
# the 2nd 2!, and so up to m!.

# define a vector of length m
m <- 10
f_n <- vector(length = m)
# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}
# inspect f_n
f_n


# Titanic Survival Exercises
Titanic Survival Exercises
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(Titanic)
library(dslabs)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
str(Titanic)

# ASSESSMENT 11 EX 1
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(dslabs)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>% 
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !weeks_reporting<10) %>% 
  mutate(rate = count / population * 10000) %>% 
  mutate(state = reorder(state, rate))%>%
  ggplot(aes(year, state, fill = rate)) + 
  geom_tile(color = "grey50") + 
  scale_x_continuous(expand=c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
  theme_minimal() + 
  theme(panel.grid = element_blank()) + 
  ggtitle(the_disease) + 
  ylab("") + 
  xlab("")

# EX 2
the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & !weeks_reporting<10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")


# EX 3
us_contagious_diseases %>% filter(state=="California" & !weeks_reporting<10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color=disease)) + 
  geom_line()

# EX 4
us_contagious_diseases%>%filter(!is.na(population)) %>% group_by(year, disease) %>% 
summarize(rate=sum(count)/sum(population)*10000)%>% 
ggplot(aes(year, rate, color=disease))+
geom_line()



# ASSESSMENT 12 - TITANIC

options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(dslabs)
library(dplyr)
library(ggplot2)
str(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))


# Question 2: Demographics of Titanic Passengers
# combinations of faceting, alpha blending, stacking and using variable counts on the 
# y-axis

# A faceted plot is useful for comparing the distributions of males and females for A. 
# Each sex has the same general shape with two modes at the same locations, though 
# proportions differ slightly across ages and there are more males than females.
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex ~ .)

# A stacked density plot with count on the y-axis is useful for answering B, C and D. 
# The main mode is around age 25 and a second smaller mode is around age 4-5. 
# There are more males than females as indicated by a higher total area and higher counts 
# at almost all ages. With count on the y-axis, it is clear that more males than females
# are age 40.

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, position = "stack")

# A plot filled by sex with alpha blending helps reveal the answers to E, F and G. 
# There is a higher proportion of females than males below age 17, a higher proportion of 
# males than females for ages 18-35, approximately the same proportion of males and females 
#age 35-55, and a higher proportion of males over age 55. The oldest individuals are male.

titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2)


# Question 3: QQ-plot of Age Distribution
# Use geom_qq() to make a QQ-plot of passenger age and add an identity line with geom_abline().
# Filter out any individuals with an age of NA first. Use the following object as the 
# dparams argument in geom_qq():
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
  
# Question 4: Survival by Sex
# To answer the following questions, make barplots of the Survived and Sex variables 
# using geom_bar(). Try plotting one variable and filling by the other variable. 
# You may want to try the default plot, then try adding position = position_dodge() to 
# geom_bar() to make separate bars for each group.
# You can read more about making barplots in the textbook section on ggplot2 geometries.

# Which of the following are true?
# Less than half of passengers survived.
# Most of the survivors were female.
# Most of the males survived.
# Most of the females survived.

#plot 1 - survival filled by sex
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()
# plot 2 - survival filled by sex with position_dodge
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
#plot 3 - sex filled by survival
titanic %>%
  ggplot(aes(Sex, fill = Survived)) +
  geom_bar()


# Question 5: Survival by Age
# Make a density plot of age filled by survival status. 
# Change the y-axis to count and set alpha = 0.2.
# Which age group is the only group more likely to survive than die?
titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)


# Question 6: Survival by Fare
# Filter the data to remove individuals who paid a fare of 0. 
# Make a boxplot of fare grouped by survival status. Try a log2 transformation of fares. Add the data points with jitter and alpha blending.
# Which of the following are true?

#my plot
titanic %>% filter(Fare>0) %>%
ggplot(aes(Survived, Fare, fill=Survived))+
geom_boxplot()+
scale_y_continuous(trans="log2")

# right boxplot
titanic %>%
filter(Fare > 0) %>%
ggplot(aes(Survived, Fare)) +
geom_boxplot() +
scale_y_continuous(trans = "log2") +
geom_jitter(alpha = 0.2)
 
  

Question 7 - Survival by Passenger Class
# barplot of passenger class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() +
  ylab("Proportion")
# barplot of passenger class filled by survival with position_fill
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
# Barplot of survival filled by passenger class with position_fill
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")

# Question 8: Survival by Age, Sex and Passenger Class
# Create a grid of density plots for age, filled by survival status, with count on the 
# y-axis, faceted by sex and passenger class.

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(position = "stack") +
  facet_grid(Sex ~ Pclass)


titanic %>%
mutate(group=Age, Pclass)%>% 
ggplot(aes(Age, fill = Survived) +
geom_density(alpha=0.2, position="stack", y="count") +
facet_grid(Sex~Pclass)
  

str(titanic)

