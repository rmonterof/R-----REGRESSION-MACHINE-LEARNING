# Assessment: Combining Tables
# Q1 What are the dimensions of the table dat, created by the following command?
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
tab1
tab2

dim(tab1)
dim(tab2)
dat <- left_join(tab1, tab2, by = "state")
dat

# Q2 What join command would create a new table "dat" with three rows and two columns?
dat <- right_join(tab1, tab2, by = "state")
dat
dat <- full_join(tab1, tab2, by = "state") 
dat
dat <- inner_join(tab1, tab2, by = "state")
dat
dat <- semi_join(tab1, tab2, by = "state")
dat

# Q3 The Batting data frame contains the offensive statistics



# Q4
df1 = data.frame(x=c("a","b"), y=c("a","a"), stringsAsFactors=F); df1
df2 = data.frame(x=c("a","a"), y=c("a","b"), stringsAsFactors=F); df2
# Which command would result in the following table?

# x y
# b a

dplyr::setdiff(df1, df2)


# Q5 
library(dplyr)
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

# Also Inspect the Master data frame, which has demographic information for all players:

Master %>% as_tibble()


# Identify the join or bind that fills the blank in this code to create the correct table:
# Which bind or join function fills the blank to generate the correct table?

top_names <- top %>% rbind(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_names <- top %>% cbind(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_names <- top %>% left_join(Master)%>%   # correct answer
  select(playerID, nameFirst, nameLast, HR)
top_names

top_names <- top %>% right_join(Master)%>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_names <- top %>% full_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_names <- top%>% anti_join(Master)%>%
  select(playerID, nameFirst, nameLast, HR)nrow(df)
top_names

# Q6
# Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, 
# then use the correct bind join function to add a salary column to the top_names data frame from # the previous question.
# Name the new data frame top_salary. Use this code framework:

top_salary <- Salaries %>%filter(yearID == 2016) %>%
  rbind(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

top_salary <- Salaries %>%filter(yearID == 2016) %>%
  cbind(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

top_salary <- Salaries %>%filter(yearID == 2016) %>%
  left_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

top_salary <- Salaries %>%filter(yearID == 2016) %>% # correct answer
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

top_salary <- Salaries %>%filter(yearID == 2016) %>%
  full_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

top_salary <- Salaries %>%filter(yearID == 2016) %>%
  anti_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

# Q7
# Inspect the AwardsPlayers table. Filter awards to include only the year 2016.
# How many players from the top 10 home run hitters won at least one award in 2016?
# AwardsPlayers

AwardsPlayers

Awards_2016 <- AwardsPlayers %>% filter(yearID == 2016)
Awards_2016
length(intersect(Awards_2016$playerID, top_names$playerID))


# How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?

length(setdiff(Awards_2016$playerID, top_names$playerID))
