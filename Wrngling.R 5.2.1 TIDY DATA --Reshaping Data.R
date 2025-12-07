library(tidyverse)
library(dslabs)
data(gapminder)
# create and inspect a tidy data frame
tidy_data <- gapminder %>%
filter(country %in% c("South Korea", "Germany")) %>%
select(country, year, fertility)
head(tidy_data)
# plotting tidy data is simple
tidy_data %>%
ggplot(aes(year, fertility, color = country)) +
geom_point()
# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)
# Reshaping Data
# Code
# original wide data
library(tidyverse)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>%
filter(country %in% c("South Korea", "Germany")) %>%
select(country, year, fertility)
# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)
# Reshaping Data
# Code
# original wide data
library(tidyverse)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>%
filter(country %in% c("South Korea", "Germany")) %>%
select(country, year, fertility)
# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)
# gather all columns except country
new_tidy_data <- wide_data %>%
gather(year, fertility, -country)
new_tidy_data

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)
# ggplot works on new tidy data
new_tidy_data %>%
ggplot(aes(year, fertility, color = country)) +
geom_point()
# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)
new_wide_data

# 5.2.3. Separate and Unite
# Code
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)
# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]
# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))
# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"),
fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")
# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
spread(variable_name, value)
# separate then unite
dat %>%
separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
unite(variable_name, first_variable_name, second_variable_name, sep="_")
# full code for tidying data
dat %>%
separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
spread(variable_name, value) %>%
rename(fertility = fertility_NA)
# Assesment
d <- read_csv("times.csv")
# Assesment
library(times)
# Assesment
path<- system.file("extdata", package="times.csv")
filename<-file.path(path, "times.csv")
d <- read_csv("times.csv")
path<- system.file("extdata", package="times.csv")
filename<-file.path(path, "times.csv")
d <- read_csv("times.csv")
d
# 5.2.2. Reshaping Data
# Code
# original wide data
library(tidyverse)
# Assesment
path<- system.file("extdata", package="times.csv")
filename<-file.path(path, "times.csv")
d <- read_csv("times.csv")
CO2



# Assesment 
path<- system.file("extdata", package="times.csv")
filename<-file.path(path, "times.csv")
d <- read_csv("times.csv")
d

library(tidyverse)
library(dslabs)

CO2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide

# Question 10
# Option 1
co2_tidy <- gather(co2_wide,month,co2,year)
co2_tidy

# Option 2 YES
co2_tidy <- gather(co2_wide,co2,month,-year)
co2_tidy

# Option 3
co2_tidy <- gather(co2_wide,co2,month,year)
co2_tidy

# Option 4 
co2_tidy <- gather(co2_wide,month,co2,-year)
co2_tidy


# Question 11
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()


# Question 12
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat

# Option1 
dat_tidy <- spread(dat, major, admitted)
# Option 2
dat_tidy <- spread(dat, gender, major)
dat_tidy
# Option 3 YES
dat_tidy <- spread(dat, gender, admitted)
dat_tidy
# Option 4
dat_tidy <- spread(dat, admitted, gender)
dat_tidy

# Question 13
# Now use the admissions dataset to create the object tmp, which has columns major, 
# gender, key and value:

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

# Combine the key and gender and create a new column called column_name to get a variable with the 
# following values: admitted_men, admitted_women, applicants_men and applicants_women. Save the new 
# data as tmp2.

# Option 1
tmp2 <- spread(admissions, column_name, key, gender)
tmp2

# Option 2
tmp2 <- gather(admissions, column_name, c(gender,key))
tmp2

# 3
tmp2 <- unite(tmp, column_name, c(gender, key))
tmp2

#4 
tmp2 <- spread(tmp, column_name, c(key,gender))
tmp2

# 5 Correct Answer
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2

# Question 14
# Which function can reshape tmp2 to a table with six rows and five columns named major, admitted_men, 
# admitted_women, applicants_men and applicants_women?

# 1
gather(tmp2)

# 2 Correct Answer

spread(tmp2)
spread(tmp2, column_name, value)
# 3

separate(tmp2)

# 4
unite(tmp2)
