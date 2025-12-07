
# Chapter 5.1 Importing data
#??? To copy a file

filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename) # to generate a full path from a relative path and a file name
file.copy(fullpath, "murders.csv")

# This code does not read the data into R, it just copies a file.
# Let's import the data with read_csv function from the readr package, 
# which is part of the tidyverse.

library(tidyverse)
dat <- read_csv(filename)
dat

getwd()

# 5.1 Paths and the working directory
# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)
dir(system.file("extdata", package="dslabs")) #directamente, sin "list.files" ni vector "path"

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename) # Use file.path() instead of paste()
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, "murders.csv")

# check if the file exists
file.exists(filename)



# 6.1. The readr and readxl Packages
# Code
library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read_csv(filename)

#read using full path
dat <- read_csv(fullpath)
head(dat)

#Ex:
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))
dat
dat1
dat2


# 6.1. Importing Data Using R-base Functions
# Code
# filename is defined in the previous video
# read.csv converts strings to factors ::: see==>
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)


# 6.1. Downloading Files from the Internet
# Code
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)


# Assessment Part 2: Data Import

url<-"http://nrvis.com/data/mldata/breast-cancer-wisconsin_wdbc.csv"
filename <-"winsconsin_wdbc.csv"
read_file(url, "winsconsin_wdbc.csv")


read_excel(url)
read_table(url)
read_csv(url)


df <- read_csv(url, col_names = FALSE)
nrow(df)
ncol(df)
