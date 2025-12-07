

# Section 2- Linear Models Assessment- Linear Models (Verified Learners only)
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance) 

# Q1
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, avg_R=R/G, avg_HR=HR/G) %>%
  do(tidy(lm(avg_attendance~avg_R, data=.),conf.int = TRUE)
     
lm(avg_attendance, data=Teams_small)
     
Teams_small
     
     
# Q1
Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G, avg_R= R/G, avg_HR=HR/G) 
     
lm(avg_attendance~avg_R, data=Teams_small)
lm(avg_attendance~avg_HR, data=Teams_small)

# a
# find regression line predicting attendance from R and take slope
Teams_small %>% 
mutate(R_per_game = R/G) %>% 
lm(avg_attendance ~ R_per_game, data = .) %>% 
.$coef %>%
.[2]
     
# b
Teams_small %>% 
mutate(HR_per_game = HR/G) %>% 
lm(avg_attendance ~ HR_per_game, data = .) %>% 
.$coef %>%
.[2]
     
# Q1B
#Question 1b
# Use number of wins to predict average attendance; do not normalize for number of games.
# For every game won in a season, how much does average attendance increase?
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, W)

nol<-lm(avg_attendance ~ W, data = Teams_small) 
nol

# or
Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .) %>% 
  .$coef %>%
  .[2]
     
# Suppose a team won zero games in a season.Predict the average attendance.
# Coefficients:
# (Intercept)     W  
# 1129.2        slope=0 

Teams_small %>% 
  lm(avg_attendance ~ W, data = .) %>% 
  .$coef %>%
  .[1]

# Question 1c
# Use year to predict average attendance.
# How much does average attendance increase each year?
#  model, E(Y|X) = B0 + B1*X tells us the increase in attendance each year when X is the year and Y is 
# the average attendance?

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, yearID)

lm(avg_attendance ~ yearID, data = Teams_small) 

# or
Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .) %>% 
  .$coef %>%
  .[2]


# Question 2
# correlation between W and R/G
Teams_small %>%
  summarize(r = cor(W,R/G)) %>% pull(r) # or cor(Teams_small$W, Teams_small$R/Teams_small$G)

# What is the correlation coefficient for wins and home runs per game?
Teams_small %>%
  summarize(r = cor(W, HR/G)) %>% pull(r) # or cor(Teams_small$W, Teams_small$HR/Teams_small$G)

# Question 3
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(W_strata=round(W/10, 0)) 

dat <- Teams_small %>% filter(W_strata == 8)
dat

# also right:
dat <- Teams_small %>%
  mutate(W_strata = round(W/10)) %>%
  filter(W_strata >= 5 & W_strata <= 10)

sum(dat$W_strata == 8)

# Question 3b
# Calculate the slope of the regression line predicting average attendance given HR per game for each of
# the win strata. Which win stratum has the largest regression line slope?

# get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)
# Book: https://rafalab.github.io/dsbook/linear-models.html - Confounding

# 3B a
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_strata = round(W/10)), 
  BB_per_game = BB / G,
  R_per_game = R / G,
  avg_attendance= attendance/G) %>%
  filter(W_strata >= 5 & W_strata <= 10)  
  
dat %>% 
  ggplot(aes(avg_attendance, R/G)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata) 

#Q3B b

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
mutate(W_strata = round(W/10)), 
BB_per_game = BB / G,
R_per_game = R / G,
HR_per_game= HR / G,
avg_attendance= attendance/G) %>%
  filter(W_strata >= 5 & W_strata <= 10)  

dat %>% 
  ggplot(aes(avg_attendance, HR/G)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata) 

# or
# a
# calculate slope of regression line after stratifying by R per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))


# W_strata slope
# <dbl> <dbl>
# 1        5 4362.
# 2        6 4343.
# 3        7 3888.
# 4        8 3128.
# 5        9 3701.
# 6       10 3107.

# or
# b

# calculate slope of regression line after stratifying by HR per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

# A tibble: 6 x 2
# W_strata  slope
# <dbl>  <dbl>
# 1        5 10192.
# 2        6  7032.
# 3        7  8931.
# 4        8  6301.
# 5        9  5863.
# 6       10  4917.
  
# Question 3c


# Question 4
# Fit a multivariate regression determining the effects of runs per game, home runs per game, wins, 
# and year on average attendance. Use the original Teams_small wins column, not the win strata from 
# question 3. What is the estimate of the effect of runs per game on average attendance?

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, avg_R=R/G, avg_HR=HR/G)
lm(avg_attendance~avg_R + avg_HR + W + yearID, data=Teams_small)

# or Explanation - The estimate can be found using the following code:
  
 #1#  
fit <- Teams_small %>% 
  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
  tidy(fit) %>%
  filter(term == "R_per_game") %>%
  pull(estimate)

  #2# 
  
  tidy(fit) %>%
    filter(term == "HR_per_game") %>%
    pull(estimate)
  
  #3# 
  tidy(fit) %>%
    filter(term == "W") %>%
    pull(estimate)
  
# Question 5
# Use the multivariate regression model from Question 4. Suppose a team averaged 5 runs per game, 
# 1.2 home runs per game, and won 80 games in a season.
# What would this team's average attendance be in 2002?
# What would this team's average attendance be in 1960?
  Teams_small <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(avg_attendance = attendance/G, avg_R=R/G, avg_HR=HR/G) %>%
  lm(avg_attendance~avg_R + avg_HR + W + yearID, data=Teams_small)

model <- Teams_small %>% mutate(R_per_game = R/G, HR_per_game = HR/G) %>% 
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)

predict(model, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
predict(model, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))



# Question 6
# Use your model from Question 4 to predict average attendance for teams in 2002 in the original Teams 
# data frame. What is the correlation between the predicted attendance and actual attendance?

newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)
