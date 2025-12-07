# Section 2: Linear Models  2.1: Introduction to Linear Models
# Confounding: Are BBs More Predictive? - Code :::


# find regression line for predicting runs from BBs

library(tidyverse)
library(dplyr)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))


# Stratification and Multivariate Regression :::

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

# Assessment Linear Models
# Q2

lm(formula = son ~ father, data = galton_heights)

# Q3
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

# We run a linear model using this centered fathers' height variable

lm(formula = son ~ father_centered, data = galton_heights)


# Assessment: Least Squares Estimates, part 1 ::: Q1

library(GaltonFamilies)
set.seed(1983)

galton_heights<- GaltonFamilies %>% filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
galton_heights

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# Q3
# right answer
library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

# regression with BB, singles, doubles, triples, HR :::almost right
library(modelr)
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
summary(fit)
coefs <- tidy(fit, conf.int = TRUE)
coefs


# Q4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse


# Q5
# Option 1
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

# Option 2
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# Option 3
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# Option 4
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


# Assessment: Least Squares Estimates, part 2 
# QUESTION 7

set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits


female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

lm(mother~daughter, data=female_heights) # right!

# rights answer
fit <- lm(mother ~ daughter, data = female_heights)
fit$coef[2]

fit$coef[1]

# Question 8
# Predict mothers' heights using the model.
# What is the predicted height of the first mother in the dataset?

first_mother<-predict(female_heights$mother)
first_mother

# or predict(fit)[1]

head(female_heights$mother)

#or female_heights$mother[1]

# Q9


library(Lahman)
library(dplyr)

str(Batting)


# right answer[1]
bat_03 <- Batting %>% filter(yearID %in% c("1999","2000","2001")) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>% 
  filter(pa >= 100) %>% select(playerID, singles, bb)

by_id<-bat_03%>%group_by(playerID)

summarize<-by_id%>%summarize(mean_single=mean(singles) , mean_bb=mean(bb))

single_2<-summarize%>%filter(mean_single>0.2) 
single_2 
nrow(single_2)

bb_2<-summarize%>%filter(mean_bb>0.2) 
nrow(bb_2)


# right answer[2]
# Explanation
# The following code can be used to determine the number of players:
  
  bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
sum(bat_99_01$mean_singles > 0.2)

# The following code can be used to determine the number of players:
sum(bat_99_01$mean_bb > 0.2)



# Question 10

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa) %>%
  filter(pa >= 100) %>%
  summarize(playerID, singles)
bat_02

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles))
bat_99_01

dat<-inner_join(bat_02, bat_99_01)
cor(dat)


#######

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_99 <- Batting %>% filter(yearID %in% 1999:2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID)%>%
summarize(mean_singles=mean(singles),mean_bb=mean(bb))

tab<-inner_join(bat_02,bat_99)

cor(tab$singles,tab$mean_singles)
cor(tab $ bb, tab $ mean_bb)

# another right answer
dat <- inner_join(bat_02, bat_99_01)
cor(dat$singles, dat$mean_singles)

cor(dat$bb, dat$mean_bb)


# Question 11
library(ggplot2)
dat %>%
  ggplot(aes(singles, mean_singles)) +
  geom_point()

library(ggplot2)
dat %>%
  ggplot(aes(bb, mean_bb)) +
  geom_point()
  

# Question 12
###### Use the code from Question 10

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_99 <- Batting %>% filter(yearID %in% 1999:2001 ) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID)%>%
  summarize(mean_singles=mean(singles),mean_bb=mean(bb))

dat<-inner_join(bat_02,bat_99)

dat <- inner_join(bat_02, bat_99)
lm(dat$singles~dat$mean_singles)

dat <- inner_join(bat_02, bat_99)
lm(dat$bb ~ dat$mean_bb)


# right answer 
# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
# What is the coefficient of mean_singles, the slope of the fit?
fit_singles <- lm(singles ~ mean_singles, data = dat)
fit_singles$coef[2]

# Fit a linear model to predict 2002 bb given 1999-2001 mean_bb
# What is the coefficient of mean_bb, the slope of the fit?
fit_bb <- lm(bb ~ mean_bb, data = dat)
fit_bb$coef[2]
  
  
# Assessment: Tibbles, do, and broom, part 1
# Question 5 You have used 1 of 2 attemptsSome problems have options such as save, reset, hints, or show 
# answer. These options follow the Submit button.
# You want to take the tibble dat, which we used in the video on the do() function, and run the linear 
# model R ~ BB for each strata of HR. Then you want to add three new columns to your grouped tibble: 
# the coefficient, standard error, and p-value for the BB term in the model.

# You've already written the function get_slope(), shown below.
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# What additional code could you write to accomplish your goal?
# 1
dat %>% 
  group_by(HR) %>% 
  do(get_slope)

# 2 ::: right answer
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))

# 3
dat %>% 
  group_by(HR) %>% 
  do(slope = get_slope(.))

# 4
dat %>% 
  do(get_slope(.))


# Question 7
# You want to know whether the relationship between home runs and runs per game varies by baseball 
# league. You create the following dataset:

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

# What code would help you quickly answer this question?
#           ::: right answer
dat %>% 
group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

# 
dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

#
dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

#
dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))

