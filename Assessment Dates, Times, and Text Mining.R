# Q6

library(tidyverse)
library(dplyr)
library(gutenbergr)
library(gutenberg_metadata)
library(tidytext)
options(digits = 3)

gutenberg_download()
library(stringr)
gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

#Q7 Notice that there are several versions of the book. The gutenberg_works() function filters this table 
#. to remove replicates and include only English language works. Use this function to find the ID for 
# Pride and Prejudice. What is the correct ID number?

gutenberg_works(title == "Pride and Prejudice")$gutenberg_id

# Question 8
# Use the gutenberg_download() function to download the text for Pride and Prejudice. Use the tidytext 
# package to create a tidy table with all the words in the text. Save this object as words.
# How many words are present in the book?

#point to a different library for the right version of the book
library(janeaustenr)
#load "book" with the ojbect austen_books with just Pride & Prejudice
book <-  janeaustenr::prideprejudice() %>% filter %>% unnest_tokens(word, text)

nrow(book)

book %>% unnest_tokens(word, text)

words %>% count(word) %>% nrow()

#Q9
words <- words %>% anti_join(stop_words)nrow(words)
words









#Q11
# Analyze the most frequent words in the novel after removing stop words and tokens with digits.
# How many words appear more than 100 times in the book?

words %>%count(word) %>%filter(n > 100) %>%nrow()

codewords %>%count(word) %>%top_n(1, n) %>%pull(word)


#Q12
# Define the afinn lexicon:

library(dplyr)
library(tidytext)
install.packages(textdata)
afinn <- get_sentiments("afinn")


