
# PROBABILITY
library(tidyverse)
library(dslabs)
library(ggplot2)
library(RColorBrewer)
library(gtools)
library(dplyr)

# 1.1 DISCRETE PROBABILITY
# rep sample
beads <- rep(c("red", "blue"), times = c(2,3))
beads

sample(beads, 1)

# replicate sample
B <- 10000
B
events <- replicate(B, sample(beads, 1))
events

# To see the distribution we use the table function 
tab<- table(events)
tab
# blue  red 
# 5990 4010 

# prop.table gives us the proportions
prop.table(tab)
#  blue   red 
# 0.599 0.401 

# The sample() function draws random outcomes from a set of options.
# The replicate() function repeats lines of code a set number of times. It is used with 
# sample() and similar functions to run Monte Carlo simulations.

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions


# 13.2.1 Setting the random seed

set.seed(1986) 
?set.seed

# 13.2.2 With and without replacement
sample(beads, 5)
sample(beads, 5)
sample(beads, 5)

sample(beads, 6) # Error in sample.int(length(x), size, replace, prob) : 
# cannot take a sample larger than the population when 'replace = FALSE'


# sample with replacement
# the sample function can be used directly, without the use of replicate, to repeat the 
# same experiment of picking 1 out of the 5 beads, continually, under the same conditions
# To do this, we sample with replacement: 
# We can tell sample to do this by changing the replace argument, which defaults to FALSE,
# to replace = TRUE:

events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

# Not surprisingly, we get results very similar to those previously obtained with replicate.



# Using the mean Function for Probability
# Applying the mean function to a logical vector, returns the proportion of elements that
# are true. We use mean to calculate probabilities.

beads <- rep(c("red", "blue"), times = c(2,3))
beads

# To find the probability of drawing a blue bead at random, you can run:
mean(beads == "blue")
mean(beads == "red")

# This code is broken down into steps inside R. First, R evaluates the logical statement 
# beads == "blue", which generates the vector: FALSE FALSE TRUE TRUE TRUE
# When the mean function is applied, R coerces the logical values to numeric values, 
# changing TRUE to 1 and FALSE to 0:

# 0 0 1 1 1
# The mean of the zeros and ones thus gives the proportion of TRUE values. As we have learned
# and will continue to see, probabilities are directly related to the proportion of events
# that satisfy a requirement.




# Assessment: Introduction to Discrete Probability
# 1 Probability of cyan

ball<-rep(c("cyan", "magenta", "yellow"), times=c(3,5,7))
ball
sample(ball, 1)
B<-10000
cyan_p<-replicate(B, sample(ball,1))
cyan_p
tab<-table(cyan_p)
tab
prop.table(tab)
# cyan_p
# cyan magenta  yellow 
# 0.2027  0.3310  0.4663

# right answer
cyan <- 3
magenta <- 5
yellow <- 7
balls <- cyan + magenta + yellow
p_cyan <- cyan/balls
p_cyan

# EX 2 Probability of not cyan

cyan <- 3
magenta <- 5
yellow <- 7
balls <- cyan + magenta + yellow
p_cyan <- cyan/balls
p_not_cyan= 1 - p_cyan
p_not_cyan

#or
p_not_cyan= (magenta+yellow)/balls
p_not_cyan


# EX 3 Sampling without replacement
(3/15)*(12/14)

# right answer
#p=p1_cyan * p2_not_cyan, without replacement
p_1= cyan/balls 
p_2=1 - (cyan-1)/(balls-1)
p_1 * p_2


# EX 4 Sampling with replacement
cyan <- 3
magenta <- 5
yellow <- 7
balls <- cyan + magenta + yellow
p_1= cyan/balls 
p_2=1 - (cyan)/(balls)
p_1 * p_2


# 1.2 COMBINATIONS AND PERMUTATIONS
# Code: Introducing paste() and expand.grid()
# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))


# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# Code: Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)
deck

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

# Code: Permutations and combinations
library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter


# Code: Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)
sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

# Code: Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v=deck) # all possible hands
hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)


# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))

# Code: Monte Carlo simulation of natural 21 in blackjack
# Note that your exact values will differ because the process is random and the seed is not set.

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)
 

# THE BIRTHDAY PROBLEM
# Code: The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


# Code: Function for birthday problem Monte Carlo simulations
# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
n


# Code: Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)



# Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob



# HOW MANY MONTE CARLO EXPERIMENTS ARE ENOUGH?
# The larger the number of Monte Carlo replicates  B , the more accurate the estimate.
# Determining the appropriate size for  B  can require advanced statistics.
# One practical approach is to try many sizes for  B  and look for sizes that provide 
# stable estimates.

# Code: Estimating a practical value of B
# This code runs Monte Carlo simulations to estimate the probability of shared birthdays 
# using several B values and plots the results. When B is large enough that the estimated 
# probability stays stable, then we have selected a useful value of B.

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 


# =================>
# 1.4 Assessment: Discrete Probability  Question 1: Olympic running
# Question 1a How many different ways can the 3 medals be distributed across 8 runners?
permutations(8,3)
# 336

# Question 1b
# How many different ways can the three medals be distributed among the 3 runners from Jamaica?
factorial(3)
permutations(3,3)
#6

# Question 1 What is the probability that all 3 medals are won by Jamaica?
df <- combinations(8,3)
df
1/56


# Question 1d Run a Monte Carlo simulation on this vector representing the countries of 
# the 8 runners in this race:
# For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 
# runners representing the 3 medalists and check whether they are all from Jamaica. 
# Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
# Calculate the probability that all the runners are from Jamaica.
# RIGHT ANSWER= 0.0.174

# Build runner countries and reset seed
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)

# Run Monte Carlo 10k
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica" & winners[3] %in% "Jamaica")
})

mean(results)

# Question 2: Restaurant management
# Different meals = 6
# Different sides = 15
combinations(6,2)
# Different drink = 2
6*15*2

# Question 2b The manager has one additional drink he could add to the special.
# How many combinations are possible if he expands his original special to 3 drink options?
# Different meals = 6
# Different sides = 15
combinations(6,2)
# Different drink = 3
6*15*3

#??? Question 2c
# How many meal combinations are there if customers can choose from
#  6 entrees, 3 drinks, and select 3 sides from the current 6 options?
# Different meals = 6
# Different sides = 20
combinations(6,3)
# Different drink = 3
6*20*3


# QUESTION 2d 
# Write a function that takes a number of entree choices and returns the number
#  of meal combinations possible given that number of entree options, 3 drink
#  choices, and a selection of 2 sides from 6 options.

f <- function(entree){
  print(3*15*entree)
}
f

# Use sapply to apply the function to entree option counts ranging from 1 to 12.
# What is the minimum number of entree options required in order to generate more
#  than 365 combinations?
options <- seq(1:12)
sapply(options, f)

#* the function SAPPLY(x,f) allows any other function f to be applied element wise
# to the vector x.


# QUESTION 2e #######
# Write a function that takes a number of side choices and returns the number of
#  meal combinations possible given 6 entree choices, 3 drink choices, and a selection
#  of 2 sides from the specified number of side choices.

ff <- function(sides){
  3*6*nrow(combinations(sides,2))
}

# Use sapply to apply the function to side counts ranging from 2 to 12.
# What is the minimum number of side options required in order to generate
#  more than 365 combinations?
options <- 2:12
sapply(options, ff)


# Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
install.packages(esoph)
head(esoph)

# QUESTION 3a/b/c #######
# How many groups are in the study?
nrow(esoph)


# How many cases are there?
# Save this value as all_controls for later problems.
all_cases<-esoph$ncases
all_cases
sum(all_cases)


# Question 3c How many controls are there?
# Save this value as all_controls for later problems.
all_controls<-esoph$ncontrols
all_controls
sum(all_controls)


# Question 4a
# What is the probability that a subject in the highest alcohol consumption group is a 
# cancer case?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), 
            probability=sum_cases/tot)


# Question 4b What is the probability that a subject in the lowest alcohol consumption 
# group is a cancer case?

print(esoph$alcgp) # lowest= -0-39g/day
esoph %>% filter(alcgp<="39") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), 
            probability=sum_cases/tot)

esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols)+sum(ncases), probability=sum_cases/tot)

# Explanation
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)



# Question 4c
# Given that a person is a case, what is the probability that they smoke 10g or
# more a day?
esoph %>% summarize(tot_cases = sum(ncases))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))
122/200

# Explanation ou can find the probability using this code:
  
  tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%                  # pull function= to extract a column or a data frame from a dataset
  sum()

tob_cases/all_cases

# Given that a person is a control, what is the probability that they smoke 10g or
#  more a day?
tob_control <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

tob_control/all_controls


esoph %>% summarize(tot_cases = sum(ncontrols))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols)) # smoking cases= 450
450/975


# Question 5a For cases, what is the probability of being in the highest alcohol group?

print(esoph$alcgp)
alc_cases <- esoph %>%
  filter(alcgp != "120+") %>%
  pull(ncases) %>%                  # pull function= to extract a column or a data frame from a dataset
  sum(alc_cases)

alc_cases/all_cases

# For cases, what is the probability of being in the highest alcohol group? # nrwade0 GITHUB
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases))
45/all_cases

sum(all_cases)
45/200


# Question 5b # For cases, what is the probability of being in the highest tobacco group?
head(esoph)
print(esoph$tobgp)
esoph%>% filter(tobgp=="30+") %>%
  summarize(tob_cases=sum(ncases))

sum(all_cases)
tob_cases/all_cases
31/200


# Question 5 c # For cases, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?

esoph%>% filter(tobgp=="30+"&alcgp=="120+")%>%
  summarize(alc_tob=sum(ncases))
sum(all_cases)
alc_tob/all_cases
10/200


# Question 5 d # For cases, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph%>% filter(tobgp=="30+"| alcgp=="120+")%>%
  summarize(alc_ortob=sum(ncases))
sum(all_cases)
66/all_cases
66/200


# QUESTION 6a/b/c/d/e/f #######
# For controls, what is the probability of being in the highest alcohol group?
esoph%>%filter(alcgp=="120+") %>%
  summarize(alc_con=sum(ncontrols))
67/all_controls
sum(all_controls)
67/975


# QUESTION 6b
# How many times more likely are cases than controls to be in the highest alcohol
#  group?
(45/200)/(67/975)


esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

# QUESTION 6c
# For controls, what is the probability of being in the highest tobacco group?
print(esoph$tobgp)
esoph%>% filter(tobgp=="30+") %>%
  summarize(tob_cases=sum(ncontrols))
sum(all_controls)
82/975


# QUESTION 6d
# For controls, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?

esoph%>% filter(alcgp=="120+"& tobgp=="30+") %>%
  summarize(tob_cases=sum(ncontrols))
13/all_controls
13/975

# QUESTION 6e
# For controls, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?

esoph%>% filter(alcgp=="120+"|tobgp=="30+") %>%
  summarize(tob_cases=sum(ncontrols))
136/all_controls
136/975

# QUESTION 6f
# How many times more likely are cases than controls to be in the highest alcohol
#  group or the highest tobacco group?
(66/200)/(67/975)

esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)
# =============================>
# End of the assessment



# Section 2: Continuous Probability  
# Code: Cumulative distribution function
# Define x as male heights from the dslabs heights dataset:
  
library(tidyverse)
library(dslabs)
library(dplyr)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
x

# Given a vector x, we can define a function for computing the CDF of x using:
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches


# 2.1 Continuous Probability  Theoretical Distribution
# Given male heights x:
  
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# We can estimate the probability that a male is taller than 70.5 inches using:
  
1 - pnorm(70.5, mean(x), sd(x))


# Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


# ===>
#  2.2 Assessment: Continuous Probability  Questions 1 and 2: ACT scores, part 1
# Question 1a What is the mean of act_scores?
# Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and 
# standard deviation of 5.7. Save these values as act_scores.

set.seed(16)
act_scores<-rnorm(10000, mean=20.9, sd=5.7)
mean(act_scores)

# Question 1b What is the standard deviation of act_scores?
sd(act_scores)

# Question 1c A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

# Question 1d In act_scores, what is the probability of an ACT score greater than 30?
sum(act_scores > 30)/length(act_scores)

# Question 1e In act_scores, what is the probability of an ACT score less than or equal to 10?
sum(act_scores <= 10)/length(act_scores)

######### QUESTION 2 #########
# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value
#  of the probability density function over x given a mean of 20.9 and standard
#  deviation of 5.7; save the result as f_x. Plot x against f_x.

x<-1:36
f_x<-dnorm(x, mean=20.9, sd=5.7)
plot(x, f_x)
f_x


######### QUESTION 3a/b/c #########
# Convert act_scores to Z-scores. Recall from Data Visualization that to standardize
#  values (convert values into Z-scores, that is, values distributed with a mean of
#  0 and standard deviation of 1), you must subtract the mean and then divide by the
#  standard deviation. Use the mean and standard deviation of act_scores, not the
#  original values used to generate random test scores.

# What is the probability of a Z-score greater than 2 (2 standard deviations above
#  the mean)?
z_act_scores <- (act_scores-mean(act_scores))/sd(act_scores)
sum(z_act_scores > 2)/length(z_act_scores)

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*sd(act_scores)+mean(act_scores)

# A Z-score of 2 corresponds roughly to the 97.5th percentile. Use qnorm to determine
#  the 97.5th percentile of normally distributed data with the mean and standard
#  deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(0.975, mean(act_scores), sd(act_scores))


######### QUESTION 4a/b/c/d #########
# Write a function that takes a value and produces the probability of an ACT score less
#  than or equal to that value (the CDF). Apply this function to the range 1 to 36.
# What is the minimum integer score such that the probability of that score or lower
#  is at least .95?
ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))

# or
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
min(which(cdf >= .95))
plot(cdf)


# Use qnorm to determine the expected 95th percentile, the value for which the
#  probability of receiving that score or lower is 0.95, given a mean score of
#  20.9 and standard deviation of 5.7. What is the 95th percentile of act_scores?
qnorm(0.95, 20.9, 5.7)


# As discussed in the Data Visualization course, we can use quantile to determine
# sample quantiles from the data.
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st
# through 99th percentiles of the act_scores data. Save these as sample_quantiles.
# In what percentile is a score of 26? Note that a score between the 98th and 99th
# percentile should be considered the 98th percentile, for example, and that quantile
# numbers are used as names for the vector sample_quantiles.

# FALSCH!! sample_quantiles <- seq(0.01, 0.99, 0.01)
# FALSCH!! quantile(act_scores, sample_quantiles)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])


# Make a corresponding set of theoretical quantiles using qnorm over the interval
#  p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these
#  as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis
#  versus theoretical_quantiles on the x-axis.

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()




# SECTION 3: RANDOM VARIABLES 
# Code: Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)


# Sampling Models
# Monte Carlo simulation: Chance of casino losing money on roulette
# We build a sampling model for the random variable  S  that represents the casino's total winnings. 
# sampling model 1: define urn, then sample

color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S


# We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})
mean(S < 0)    # probability of the casino losing money



# We can plot a histogram of the observed values of S as well as the normal density curve based 
# on the mean and standard deviation of S.

library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")
Previous


# =============================>
# 3.3 Assessment: Random Variables, Sampling Models, and the Central Limit Theorem 

options(digits = 3)

#### Useful Formulas
# Expected values of a random variable
# ap + b(1- p)
# Expected value of the sum of n draws of a random variable
# n * (ap + b(1-p))
# Standard deviation of an urn with two values
# abs(b - a) * sqrt(p(1 - p))
# Standard error of the sum of n draws of a random variable
# sqrt(n) * abs(b - a) * sqrt(p(1 - p))
####


######### QUESTION 1a/b/c/d/e/f #########
# An old version of the SAT college entrance exam had a -0.25 point penalty for
#  every incorrect answer and awarded 1 point for a correct answer. The quantitative
#  test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose
#  a student chooses answers by guessing for all questions on the test.

# What is the probability of guessing correctly for one question?
1/5

# What is the expected value of points for guessing on one question?
1/5 * 1 + -0.25* 4/5

# What is the expected score of guessing on all 44 questions?
mu <- 44 * (1/5 * 1 + -0.25* 4/5)
mu

# What is the standard error of guessing on all 44 questions?
er <- sqrt(44) * abs(1+0.25) * sqrt(1/5*4/5)
er


# Use the Central Limit Theorem to determine the probability that a guessing student
#  scores 8 points or higher on the test.
1-pnorm(8,mu,er)


# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing
#  on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
X <- replicate(10000, {
  sum(sample(c(1,-0.25), size=44, replace=TRUE, prob=c(1/5,4/5)))
})
sum(X>=8)/10000 # or mean(tests >= 8)


######### QUESTION 2a/b/c #########
# Suppose that the number of multiple choice options is 4 and that there is no
#  penalty for guessing - that is, an incorrect question gives a score of 0.
# What is the expected value of the score when guessing on this new test?
mu <- 44 * (1/4 * 1 + 0* 3/4)
mu


# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05)
#  representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)

fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
}

sapply(p,fu) # doesn't work

p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

#Answer: 0.85 



######### QUESTION 3a/b/c/d/e/f/g #########
# A casino offers a House Special bet on roulette, which is a bet on five pockets
#  (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other
#  words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants
#  to know the chance of losing money if he places 500 bets on the roulette House
#  Special.

# What is the expected value of the payout for one bet?
(6*5/38 + -1*(1 - 5/38))

# Right code

p <- 5/38
a <- 6
b <- -1
mu <- a*p + b*(1-p)
mu

# What is the standard error of the payout for one bet?
abs(-1 - 6) * sqrt(5/38*(1 - 5/38))

# Right answer:
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

# What is the expected value of the average payout over 500 bets? Remember there
#  is a difference between expected value of the average and expected value of
#  the sum. Same as one bet.
(6*5/38 + -1*(1 - 5/38))


# What is the standard error of the average payout over 500 bets? Remember there
# is a difference between the standard error of the average and standard error of
# the sum.
(abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))/sqrt(500)

#Right answer: 
n <- 500
sigma/sqrt(n)


# What is the expected value of the sum of 500 bets?
mu <- 500 * (6*5/38 + -1*(1 - 5/38))
mu


# What is the standard error of the sum of 500 bets?
er <- sqrt(500) * (abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))
er

# Use pnorm with the expected value of the sum and standard error of the sum to
# calculate the probability of losing money over 500 bets,  Pr(???????0) .
pnorm(0, mu, er)



# 4.1 The Big Short: Interest Rates Explained
# Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)


# Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})


# Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")


# Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error


# Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error


# Code: Calculating interest rates for expected value of 0
# We can calculate the amount  x  to add to each loan so that the expected value is 0 using the 
# equation  lp+x(1???p)=0 . Note that this equation is the definition of expected value given a 
# loss per foreclosure  l  with foreclosure probability  p  and profit  x  if there is no 
# foreclosure (probability  1???p ).

# We solve for  x=???lp1???p  and calculate  x :
  
x = - loss_per_foreclosure*p/(1-p)
x

# On a $180,000 loan, this equals an interest rate of:
x/180000

# Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans


# Code: Monte Carlo simulation for 1% probability of losing money
# Note that your results will vary from the video because the seed is not set.

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money




# 4.2.2 The Big Short 
# Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)


# Code: Calculating number of loans for desired probability of losing money
# The number of loans required is:
  
  z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans


# Code: Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given a known probability of default
# p=0.04 . Note that your results will differ from the video because the seed is not set.

B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})

mean(profit)


# Code: Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given an unknown probability of 
# default  0.03???p???0.05 , modeling the situation where an event changes the probability of default
# for all borrowers simultaneously. Note that your results will differ from the video because 
# the seed is not set.

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million



# 4.2 Assessment: The Big Short
data(death_prob)
head(death_prob)
str(death_prob)

#### Useful Formulas
# Expected values of a random variable
# ap + b(1- p)
# Expected value of the sum of n draws of a random variable
# n * (ap + b(1-p))
# Standard deviation of an urn with two values
# abs(b - a) * sqrt(p(1 - p))
# Standard error of the sum of n draws of a random variable
# sqrt(n) * abs(b - a) * sqrt(p(1 - p))
####


######### QUESTION 1a/b/c/d/e/f #########
# The death_prob data frame contains information about the estimated probability
#  of death within 1 year (prob) for different ages and sexes.
# Use death_prob to determine the death probability of a 50 year old female, p.

p <- death_prob %>% filter(age==50, sex=="Female") %>% pull(prob)
p

# The loss in the event of the policy holder's death is -$150,000 and the gain
#  if the policy holder remains alive is the premium $1,150.
# What is the expected value of the company's net profit on one policy for a
#  50 year old female?
-150000*p + 1150*(1-p)

# o...
a <- -150000
b <- 1150

mu <- a*p + b*(1-p)
mu


# Calculate the standard error of the profit on one policy for a 50 year old female.
abs(-150000-1150)*sqrt(p*(1-p))

# o...
sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

# What is the expected value of the company's profit over all 1,000 policies for 50
#  year old females?
1000*(-150000*p + 1150*(1-p))

# o...
n <- 1000
n*mu

# What is the standard error of the sum of the expected value over all 1,000 policies
#  for 50 year old females?
sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p)))

# o...
sqrt(n) * sigma


# Use the Central Limit Theorem to calculate the probability that the insurance
#  company loses money on this set of 1,000 policies.
pnorm(0, 1000*(-150000*p + 1150*(1-p)), sqrt(1000)*(abs(-150000-1150)*sqrt(p*(1-p))))

# o...
pnorm(0, n*mu, sqrt(n)*sigma)


######### QUESTION 2a/b/c/d #########
# Use death_prob to determine the probability of death within one year for
#  a 50 year old male.
p <- death_prob %>% filter(age==50, sex=='Male') %>% pull(prob)
p

# o...
p_male <- death_prob %>%
  filter(sex == "Male" & age == "50") %>%
  pull(prob)
p_male

# Suppose the company wants its expected profits from 1,000 50 year old males
#  with $150,000 life insurance policies to be $700,000. Use the formula for
#  expected value of the sum of draws with the following values and solve for the
#  premium b:
# where   E[S] = mu_S = 700000
#         n = 1000
#         p = death probability of 50 year old males
#         a = 150000 loss
#         b = premium to solve
# E[S] = n * (ap + b(1-p))
# --> b = ((E[S]/n) - ap)/(1-p)
# What premium should be charged?
b <- ((700000/1000) - -150000*p)/(1-p)
b

#o...
p <- p_male
mu_sum <- 700000
n <- 1000
a <- -150000

b <- (mu_sum/n-a*p)/(1-p)
b

# Using the new 50 year old male premium rate, calculate the standard error of
#  the sum of 1,000 premiums.
err <- sqrt(1000) * abs(b - -150000) *sqrt(p*(1-p))
err

# o...
sigma_sum <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
sigma_sum


# What is the probability of losing money on a series of 1,000 policies to 50
# year old males? Use the Central Limit Theorem.
pnorm(0, 1000*(-150000*p + b*(1-p)), err)

# o...
pnorm(0, mu_sum, sigma_sum)


######### QUESTION 3a/b/c/d/e/f #########
# a lethal pandemic disease increases the probability of death within 1 year
#  for a 50 year old to .015. Unable to predict the outbreak, the company has
#  sold 1,000 $150,000 life insurance policies for $1,150.
# What is the expected value of the company's profits over 1,000 policies?
mu <- 1000 * (-150000*0.015 + 1150*(1-0.015))
mu

# o...
p <- .015    # probability of claim
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

exp_val <- n*(a*p + b*(1-p))
exp_val


# What is the standard error of the expected value of the company's profits over
#  1,000 policies?
err <- sqrt(1000) * abs(-150000 - 1150) * sqrt(0.015*(1 - 0.015))
err

# or...
se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
se



# What is the probability of the company losing money?
pnorm(0, mu, err)


# Suppose the company can afford to sustain one-time losses of $1 million, but
#  larger losses will force it to go out of business. What is the probability of
#  losing more than $1 million?
pnorm(-1000000,mu,err)


# Investigate death probabilities p <- seq(.01, .03, .001). What is the lowest
#  death probability for which the chance of losing money exceeds 90%?
p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000

p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
  pull(p) %>%
  min()


# or...
p <- seq(.01, .03, .001) a <- -150000
b <- 1150
n <- 1000

p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})

data.frame(p, p_lose_money) %>% filter(p_lose_money > 0.9)



# Investigate death probabilities p <- seq(.01, .03, .0025). What is the lowest
#  death probability for which the chance of losing over $1 million exceeds 90%?
p_lose_million <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})

data.frame(p, p_lose_million) %>%
  filter(p_lose_million > 0.9) %>%
  pull(p) %>%
  min()


######### QUESTION 4a/b #########
# Define a sampling model for simulating the total profit over 1,000 loans with
#  probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit
#  of $1,150 when there is no claim. Set the seed to 25, then run the model once.
# What is the reported profit (or loss) in millions (that is, divided by 10^6)?
set.seed(25)
n <- 1000
p_loss <- 0.015

X <- sample(c(0,1), n, replace=TRUE, prob=c((1-p_loss),p_loss))
loss <- -150000*sum(X==1)/10^6 # in millions
profit <- 1150*sum(X==0)/10^6
loss+profit


# Set the seed to 27, then run a Monte Carlo simulation of your sampling model
#  with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27)
S <- replicate(10000, {
  X <- sample(c(0, 1), 1000, replace=TRUE, prob=c((1-0.015), 0.015))
  loss <- -150000*sum(X==1)/10^6 # in millions
  profit <- 1150*sum(X==0)/10^6
  loss+profit
})
sum(S<=-1)/10000


######### QUESTION 5a/b/c/d #########
# Suppose that there is a massive demand for life insurance due to the pandemic,
#  and the company wants to find a premium cost for which the probability of losing
#  money is under 5%, assuming the death rate stays stable at p = 0.015
# Calculate the premium required for a 5% chance of losing money given n = 1000 loans
#  probability of death p = 0.015, and loss per claim l=-150000. Save this premium
#  as x for use in further questions.
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

# What is the expected profit per policy at this rate?
l*p + x*(1-p)

# What is the expected profit over 1,000 policies?
n*(l*p + x*(1-p))

# Run a Monte Carlo simulation with B=10000 to determine the probability of losing
#  money on 1,000 policies given the new premium x, loss on a claim of $150,000,
#  and probability of claim p=0.015. Set the seed to 28 before running your 
#  simulation.
# What is the probability of losing money here?
set.seed(28)
S <- replicate(10000, {
  X <- sample(c(0,1), n, replace = TRUE, prob=c((1-p), p))
  loss <- l*sum(X==1)/10^6 # in millions
  profit <- x*sum(X==0)/10^6
  loss+profit
})
sum(S<0)/10000

# or...
set.seed(28)
B <- 10000
profit <- replicate(B, {
  draws <- sample(c(x, l), n,
                  prob=c(1-p, p), replace = TRUE)
  sum(draws)
})

mean(profit < 0)


######### QUESTION 6a/b #########
# The company cannot predict whether the pandemic death rate will stay stable. Set
#  the seed to 29, then write a Monte Carlo simulation that for each of B=10000
#  iterations:
#  - randomly changes p by adding a value between -0.01 and 0.01 with
#    sample(seq(-0.01, 0.01, length = 100), 1)
#  - uses the new random p to generate a sample of n=1000 policies with premium x
#    and loss per claim l=-150000
#  - returns the profit over n policies (sum of random variable)
# The outcome should be a vector of B total profits


# n, p, l and x as defined in the problem information
set.seed(29, sample.kind="Rounding")    # in R 3.6, set.seed(29, sample.kind="Rounding")

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, 
                  prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})

# What is the expected value over 1,000 policies?
mean(profit)

# What is the probability of losing money?
mean(profit<0)

# probability of losing more than one million dollars?
mean(profit < -1*10^6)





