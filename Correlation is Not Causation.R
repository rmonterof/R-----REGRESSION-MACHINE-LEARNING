# https://rafalab.github.io/dsbook/association-is-not-causation.html#spurious-correlation

# Correlation is Not Causation: Spurious Correlation

Code
# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# A tibble: 1,000,000 x 2
# group     r
# <int> <dbl>
# 1 940797 0.857
# 2  50918 0.805
# 3 957546 0.776
# 4 396796 0.762
# 5 271496 0.761
# 6 808183 0.760
# 7 497036 0.757
# 8 628513 0.755
# 9  49894 0.753
# 10 977555 0.753
# ... with 999,990 more rows

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))



### OUTLIERS

# Correlations can be caused by outliers.
# The Spearman correlation is calculated based on the ranks of data.
# Code
# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")


#### Reversing Cause and Effect

# Another way association can be confused with causation is when the cause and effect are reversed.
# As discussed in the video, in the Galton data, when father and son were reversed in the regression, 
# the model was technically correct. The estimates and p-values were obtained correctly as well. 
# What was incorrect was the interpretation of the model.

# Code
# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))

# A tibble: 2 x 5
# term        estimate std.error statistic  p.value
# <chr>          <dbl>     <dbl>     <dbl>    <dbl>
# 1 (Intercept)   34.0      4.57        7.44 4.31e-12
# 2 son            0.499    0.0648      7.70 9.47e-13


# CONFOUNDERS
# If X and Y are correlated, we call Z a confounder if changes in Z causes changes in both X and Y.
# Code
# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))


# SIMPSON'S PARADOX
# https://rafalab.github.io/dsbook/association-is-not-causation.html#simpsons-paradox

#> `summarise()` ungrouping output (override with `.groups` argument)


#### Assessment: Confounding (Verified Learners only)
library(dslabs)
data("research_funding_rates")
research_funding_rates

# Q1

totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 

totals

# or
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

# Q2

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women)) #??? rounding: 17.7 & 15

# or
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

# &
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)


# Q3 chi-squared test
rate <- totals %>%
  summarize(percent_total = 
              (yes_men + yes_women)/
              (yes_men + no_men +yes_women + no_women)) %>%
  pull(percent_total)
rate

two_by_two <- data.frame(awarded = c("no", "yes"), 
                         men = c(totals$no_men, totals$yes_men),
                         women = c(totals$no_women, totals$yes_women))
two_by_two

# The general idea of the Chi-square test is to compare this two-by-two table to what you expect to see,
# which would be:

data.frame(awarded = c("no", "yes"), 
           men = (totals$no_men + totals$yes_men) * c(1 - rate, rate),
           women = (totals$no_women + totals$yes_women) * c(1 - rate, rate))


# The Chi-square test tells us how likely it is to see a deviation this large or larger. This test uses
# an asymptotic result, similar to the CLT, related to the sums of independent binary outcomes.

# The R function chisq.test takes a two-by-two table and returns the results from the test:
chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()

chisq_test$p.value

# or (rigth answer)
two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)


# Question 4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat


dat %>% ggplot(aes(success, discipline, col = gender, size = applications)) + geom_point()

# or 
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()


