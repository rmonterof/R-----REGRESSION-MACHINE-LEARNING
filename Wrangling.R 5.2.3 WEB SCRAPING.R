# import a webpage into R
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)


# CSS Selectors

# For the guacamole recipe page, we already have done this and determined that we need the following 
# selectors:
  
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

# You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:
  
guacamole <- list(recipe, prep_time, ingredients)
guacamole

# Since recipe pages from this website follow this general layout, we can use this code to create 
# a function that extracts this information:
  
get_recipe <- function(url){
    h <- read_html(url)
    recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
    return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  } 
# and then use it on any of their webpages:
  
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


# https://selectorgadget.com/
# https://foodnetwork.co.uk/recipes/guacamole/?utm_source=foodnetwork.com&utm_medium=domestic


# Assessment: Web Scraping

# Load the following web page, which contains information about Major League Baseball payrolls, into
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

# We learned that tables in html are associated with the table node.  Use the html_nodes() function 
# and the table node type to extract the first table. Store it in an object nodes:
  
nodes <- html_nodes(h, "table")

# The html_nodes() function returns a list of objects of class xml_node. We can see the content of each
# one using, for example, the html_text() function. You can see the content for an arbitrarily picked 
# component like this:
  
html_text(nodes[[8]])

#If the content of this object is an html table, we can use the html_table() function to convert it to a 
# data frame:
  
html_table(nodes[[8]])
# You will analyze the tables from this HTML page over questions 1-3.


# Q1
# Many tables on this page are team payroll tables, with columns for rank, team, and one or more money values.
# Convert the first four tables in nodes to data frames and inspect them.
# Which of the first four nodes are tables of team payroll?
# Check all correct answers. Look at table content, not column names.

sapply(nodes[1:4], html_table)    # 2, 3, 4 give tables with payroll info


# Question 2

# For the last 3 components of nodes, which of the following are true? (Check all correct answers.)
# Check all correct answers.

html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

# Question 3
# Create a table called tab_1 using entry 10 of nodes. Create a table called tab_2 using entry 19 of nodes.

tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
col_names <- c("Team", "Payroll", "Average")
tab_1 <- tab_1[-1, -1]
tab_2 <- tab_2[-1,]
names(tab_2) <- col_names
names(tab_1) <- col_names
full_join(tab_1,tab_2, by = "Team")

# QQ 4 + 5
# Use the rvest library to read the HTML from this Wikipedia page (make sure to copy both lines of the URL):

library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

# Question 4
# Assign tab to be the html nodes of the "table" class.How many tables are in this Wikipedia page?
tab <- read_html(url) %>% html_nodes("table")
length(tab)
tab

# # Question 5

# Inspect the first several html tables using html_table() with the argument fill=TRUE (you can read about this
# argument in the documentation). Find the first table that has 9 columns with the first column named "Date(s) 
# conducted".
# What is the first table number to have 9 columns where the first column is named "Date(s) conducted"?

tab[[1]] %>% html_table(fill = TRUE) %>% names() # inspect column names
tab[[2]] %>% html_table(fill = TRUE) %>% names() # inspect column names
tab[[3]] %>% html_table(fill = TRUE) %>% names() # inspect column names
tab[[4]] %>% html_table(fill = TRUE) %>% names() # inspect column names
tab[[5]] %>% html_table(fill = TRUE) %>% names() # inspect column names CORRECT ANSWER
tab[[6]] %>% html_table(fill = TRUE) %>% names() # inspect column names
tab[[7]] %>% html_table(fill = TRUE) %>% names() # inspect column names
tab[[8]] %>% html_table(fill = TRUE) %>% names() # inspect column names






