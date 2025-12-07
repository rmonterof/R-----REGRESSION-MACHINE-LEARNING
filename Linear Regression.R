# The visualization of choice when exploring the relationship between two variables like home runs and 
# runs is a scatterplot.
# Code: Scatterplot of the relationship between HRs and wins
library(Lahman)
library(tidyverse)
library(dslabs)
library(Rtools)
library(dplyr)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Code: Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth()

# Code: Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Assesment Question 4
# You want to know whether teams with more at-bats per game have more runs per game.
# What R code below correctly makes a scatter plot for this relationship?

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  ggplot(aes(AB, R)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_line()

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point() +
  geom_smooth()

# Assesment Question 5
?Teams

# Assesment Question 6
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point()

# Right Answer
library(Lahman)
library(tidyverse)
library(ellipsis)
library(dslabs)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)+
  geom_smooth()


# Assesment Question 7
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Win_Rate = W / G, Error_per_game = E / G) %>%
  ggplot(aes(Win_Rate, Error_per_game)) + 
  geom_point(alpha = 0.5)+
  geom_smooth()

# Assesment Question 8
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Triples_per_game = X3B / G, Doubles_per_game = X2B / G) %>%
  ggplot(aes(Triples_per_game, Doubles_per_game)) + 
  geom_point(alpha = 0.5) +
  geom_smooth()




# 1.2. CORRELATION
# Correlation Coefficient - Galton - Fathers' and sons' heights ::: Code
# create the dataset
library(tidyverse)
library(dslabs)
library(HistData)
install.packages("GaltonFamilies")
set.seed(1983)

galton_heights<- GaltonFamilies %>% filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
galton_heights
  
# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)


# 1.2 Sample Correlation is a Random Variable
# Code
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

# Assessment Q 7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(R/G,AB/G)) %>% pull(r)

# Assessment Q 8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(W/G,E/G)) %>% pull(r)

# Assessment Q 9
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  summarize(r = cor(X2B/G,X3B/G)) %>% pull(r)


# Section 1: Introduction to Regression / 1.3: Stratification and Variance Explained
# Anscombe's Quartet/Stratification

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)


# Bivariate Normal Distribution

galton_heights<- GaltonFamilies %>% filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)


# Variance Explained
# There are Two Regression Lines

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y
m_2
b_2

# Assessment Q8 REGRESSION
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
library(tidyverse)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


female_heights %>% summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))
sd(female_heights$daughter)
female_heights %>% summarize(r = cor(mother, daughter))
# or
cor(female_heights$mother, female_heights$daughter)

# Q9 slope and intercept of the regression line predicting daughters' heights given mothers' heights
# y = b + m * x
# m = ??*??y/??x  b = ??y ??? m??x

# 1st
r <- cor(female_heights$mother, female_heights$daughter)
s_y <- sd(female_heights$daughter)
s_x <- sd(female_heights$mother)
r * s_y/s_x

# 2nd
mu_y <- mean(female_heights$daughter)
mu_x <- mean(female_heights$mother)
mu_y - (r * s_y/s_x)*mu_x

# 3rd
r * s_y/s_x

#Q10 formula r^2*100
1 - (1 ??? 0.325**2)
r^2*100

# Q11
# The expected value E(Y|X = x) of the daughter's height can be computed using the regression line. 
# You should already have the slope and intercept of the regression line, so you can calculate the 
# conditional expected value of the daughter's height given the mother's height by using the regression 
# line formula. Hope this helps

m = r * s_y/s_x
b = mu_y - (r * s_y/s_x)*mu_x
x = 60
m*x+b








