
# ASSESSMENT 13 - Properties of Stars Exercises
library(tidyverse)
library(dslabs)
library(ggplot2)
data(stars)
options(digits = 3)   # report 3 significant digits
str(stars)
library(RColorBrewer)

# QUESTION 1 MEAN MAGNITUDE
mean(stars$magnitude)
sd(stars$magnitude)

# QUESTION 2 -DENSITY PLOT OF THE MAGNITUDE - HOW MANY PEAKS?
stars%>%
  ggplot(aes(magnitude, temperature, y=..count..))+
  geom_density(alpha=0.2, position="stack")

# QUESTION 3 - Examine the distribution of star temperature.
# Which of these statements best characterizes the temperature distribution?
stars %>%
  ggplot(aes(temp)) +
  geom_density()

# Question 4
# Make a scatter plot of the data with temperature on the x-axis and magnitude on the 
# y-axis and examine the relationship between the variables. 
# Recall that lower magnitude means a more luminous (brighter) star.
# Most stars follow a _______________ trend. These are called main sequence stars.

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()

# Question 5 scale_y_reverse()

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()+
  scale_x_continuous(log10)+
  scale_x_reverse()+
  scale_y_reverse()

# Question 6 = 4 white dwarfs
# answer=These stars are in the lower left of the plot from question 5. 
# There are 4 stars in this region.

# Question 7 = 5000K

# Question 8

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()+
  scale_x_continuous(log10)+
  scale_x_reverse()+
  scale_y_reverse()+
  geom_text(aes(label = star, color=type), size=3, show.legend = FALSE)

# QUESTION 9
# Remove the text labels and color the points by star type. This classification describes
# the properties of the star's spectrum, the amount of light produced at various wavelengths.
# Which star type has the lowest temperature?
# Which star type has the highest temperature?
# The Sun is classified as a G-type star. 
#Is the most luminous G-type star in this dataset also the hottest?

stars %>%
  ggplot(aes(temp, magnitude, color=type)) +
  geom_point()+
  scale_x_continuous(log10)+
  scale_x_reverse()+
  scale_y_reverse()


t<-order(stars$temp)
t

# ASSESSMENT 14 Climate Change Exercises: Questions 1-7
library(tidyverse)
library(dslabs)
library(ggplot2)
library(dplyr)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)


library(dslabs)
install.packages(temp_carbon)
print(temp_carbon$year) # 2018

# Which of these code blocks return the latest year for which carbon emissions are reported?

temp_carbon %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)

# QUESTION 2
# What is the first year for which carbon emissions (carbon_emissions) data are available?
temp_carbon%>%filter(carbon_emissions, year)

## What is the last year for which carbon emissions data are available?
temp_carbon%>%filter(carbon_emissions, year==1751)

# How many times larger were carbon emissions in the last year relative to the first year?
temp_carbon%>%filter(carbon_emissions, year==2018)

# QUESTION 3
# Inspect the difference in temperature in temp_carbon from the first available year 
#to the last available year.
# What is the first year for which global temperature anomaly (temp_anomaly) data are 
# available?
temp_carbon%>%select(temp_anomaly, year)

# QUESTION 4
# Create a time series line plot of the temperature anomaly. 
# Only incl"ude years where temperatures are reported. Save this plot to the object p.
# Which command adds a blue horizontal line indicating the 20th century mean temperature?

p<- temp_carbon%>% ggplot(aes(year,temp_anomaly))+
  geom_line()+
  geom_text(aes(label=year), size=2)


p + geom_vline(aes(xintercept = 0), col = "blue")

p + geom_hline(aes(y = 0), col = "blue")

p + geom_hline(aes(yintercept = 0, col = "blue"))

p + geom_hline(aes(yintercept = 0), col = "blue")


# QUESTION 5
# Continue working with p, the plot created in the previous question.
# Change the y-axis label to be "Temperature anomaly (degrees C)". 
# Add a title, "Temperature anomaly relative to 20th century mean, 1880-2018". 
# Also add a text layer to the plot: the x-coordinate should be 2000, the y-coordinate 
# should be 0.05, the text should be "20th century mean", and the text color should be 
# blue.
# Which of the following code blocks is correct?
# right answer: no.#4

p<-temp_carbon%>% ggplot(aes(year,temp_anomaly))+
  geom_line()+
  geom_text(aes(label=year), size=2, color="red")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")


# right answer (code)
temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018")

# Question 6
# When was the earliest year with a temperature above the 20th century mean?
p<-temp_carbon%>% 
  filter(temp_anomaly>0.05) # answer: 1940=0.16
p
# When was the last year with an average temperature below the 20th century mean?
p<-temp_carbon%>% 
  filter(temp_anomaly<0.05) # answer: 1976
p
# In what year did the temperature anomaly exceed 0.5 degrees Celsius for the first time?
p<-temp_carbon%>% 
  filter(temp_anomaly==0.51)
p

# Question 7
# Add layers to the previous plot to include line graphs of the temperature anomaly 
# in the ocean (ocean_anomaly) and on land (land_anomaly). 
# Assign different colors to the lines. Compare the global temperature anomaly to the land 
# temperature anomaly and ocean temperature anomaly.

# Which region has the largest 2018 temperature anomaly relative to the 20th century mean?
temp_carbon%>% 
ggplot(aes(year,land_anomaly))+
geom_line()+
  geom_hline(aes(yintercept = 0.05), col = "blue") +
geom_text(aes(label=year), size=2, color="brown")

temp_carbon%>%select(year>"1900", year<2000, temp_anomaly, ocean_anomaly, land_anomaly)
         
# Which region has the largest change in temperature since 1880?
temp_carbon%>% 
  ggplot(aes(year,ocean_anomaly))+
  geom_line()+
  geom_hline(aes(yintercept = 0.05), col = "blue") +
  geom_text(aes(label=year), size=2, color="orange")

l<-c(temp_carbon$land_anomaly, na.rm=FALSE)
l
t<-(temp_carbon$temp_anomaly)
t
o<-(temp_carbon$ocean_anomaly)
o

# Which region has a temperature anomaly pattern that more closely matches the global 
# pattern?
temp_carbon %>% 
  group_by(land_anomaly) %>% 
summarize(average = mean(land_anomaly=="1:98"))

# right answer: compare charts with naked eye


# Question 8
# Complete the code outline below to make a line plot of concentration on the y-axis by 
# year on the x-axis. Facet by gas, aligning the plots vertically so as to ease comparisons
# along the year axis. Add a vertical line with an x-intercept at the year 1850, noting 
# the unofficial start of the industrial revolution and widespread fossil fuel consumption.
# Note that the units for ch4 and n2o are ppb while the units for co2 are ppm.

str(greenhouse_gases)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850), col = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")          


# Question 10
# Make a time series line plot of carbon emissions (carbon_emissions) from the temp_carbon dataset. 
# The y-axis is metric tons of carbon emitted per year.
# Which of the following are true about the trend of carbon emissions?
temp_carbon %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept = 1850), col = "blue")
# right answer: points 1,4,5,6

# Question 11
# Make a line plot of co2 concentration over time (year), 
# coloring by the measurement source (source). Save this plot as co2_time for later use.
# Which of the following are true about co2_time, the time series of co2 over the last 
# 800,000 years?
str(historic_co2)

historic_co2%>%
ggplot(aes(year, co2), color=source) +
  geom_line() +
  ggtitle("co2_time")     

# QUESTION 12
# Change the x-axis limits to -800,000 and -775,000. About how many years did it take 
# for co2 to rise from 200 ppmv to its peak near 275 ppmv?
#  scale_x_continuous(limits=c(-800,000, -775,000)) or coord_cartesian(xlim = c(, )) 

historic_co2%>%
  ggplot(aes(year, co2), color=source) +
  coord_cartesian(xlim=c(-8e+05, -775e+3))+
   geom_line() +
  ggtitle("co2_time")

# Change the x-axis limits to -375,000 and -330,000. About how many years did it take for
# co2 to rise from the minimum of 180 ppm to its peak of 300 ppmv?
historic_co2%>%
  ggplot(aes(year, co2), color=source) +
  coord_cartesian(xlim=c(-375e+03, -330e+3))+
  geom_line() +
  ggtitle("co2_time")

# Change the x-axis limits to -140,000 and -120,000. About how many years did it take for
# co2 to rise from 200 ppmv to its peak near 280 ppmv?
historic_co2%>%
  ggplot(aes(year, co2), color=source) +
  coord_cartesian(xlim=c(-140e+03, -120e+3))+
  geom_line() +
  ggtitle("co2_time")

# Change the x-axis limits to -3000 and 2018 to investigate modern changes in co2. 
# About how many years did it take for co2 to rise from its stable level around 275 ppmv 
# to the current level of over 400 ppmv?
historic_co2%>%
  ggplot(aes(year, co2), color=source) +
  coord_cartesian(xlim=c(1e+03, 20e+02))+
  geom_line() +
  ggtitle("co2_time")


