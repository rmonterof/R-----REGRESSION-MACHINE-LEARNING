
# ecdf in R (Example) | Compute & Plot the Empirical Cumulative Distribution Function
# Basic R Syntax of ecdf():

# ecdf(x)

# Definition of ecdf():
# The ecdf function computes the Empirical Cumulative Distribution Function of a numeric 
# input vector.

# Example: Compute and Plot ECDF in R
# Before we can start with the example, we need to create a numeric example vector in R:
  
set.seed(19191)     # Set seed for reproducibility
x <- rnorm(50)      # Normal distribution with 50 values

ecdf(x)             # Compute ecdf values
# Empirical CDF 
# Call: ecdf(x)
# x[1:50] = -2.5138, -2.0871, -1.8105,  ...,  2.033, 2.2279
# The RStudio output of the ecdf function is not really helpful, but however, we can also use this output to plot the ECDF:
  
plot(ecdf(x))       # Create ecdf plot in R



# QQPLOT EXAMPLES
# =====>
# Quantile-Quantile Plot in R (4 Examples) | qqplot, qqnorm & qqline Functions | ggplot2 Package
# Example 1: Basic QQplot & Interpretation
# In this example I'll show you the basic application of QQplots (or Quantile-Quantile plots) in R. In the example, we'll use the following normally distributed numeric vector:
  
set.seed(5432)                   # Set seed for reproducibility
x <- rnorm(10000)                # Create random normally distributed values

# Our vector contains 10000 random values, which follows a normal distribution.
# Now, we can use the qqnorm function to create a QQplot of this vector.

qqnorm(x)                        # QQplot of normally distributed values

#.and the qqline function to add a theoretical line according to the normal distribution:
  
qqline(x, col = "red")           # Add qqline to plot

# Figure 1 shows the output of the previous R code: A QQplot of our normally distributed random data compared to the theoretical normal distribution and a QQline.

# We can interpret the graphic as follows:
# The quantiles of our sampled random data and the theoretical quantiles follow the QQline almost perfectly.
# For that reason, the QQplot indicates that our random values are normally distributed.



# Example 2: QQplot of Logistically Distributed Values
# Let's apply the same R code as in Example 1 to a different probability distribution in R:
  
y <- rlogis(10000)               # Random values according to logistic distribution

# Our new random data follows a logistic distribution. Now, let's draw a QQplot and a QQline to see the difference 
# compared to Examples 1:
  
qqnorm(y)                        # QQplot of logistic distribution
qqline(y, col = "red")

# Figure 2 shows the result. In contrast to Figure 1, the QQplot is not following the straight QQline, 
# indicating that our random values do not follow a normal distribution



# Example 3: Compare Two Data Sets with QQplot
# So far, we have only compared one input data set vs. a theoretical normal distribution. However, 
# it is also possible to compare two input data sets with each other.
# Consider the following random values with a student t distribution:
  
z <- rt(10000, 3)                # Random values according to student t distribution

# We can use the qqplot function to compare this distribution with the logistically distributed vector that we have 
# created in Example 2:
  
qqplot(y, z)                     # QQplot of logistic & student t distribution

# As you can see based on Figure 3, our two input data sets do now follow the same distribution, since the QQplot is 
# not following a straight line.



# Example 4: Create QQplot with ggplot2 Package
# Until now, we have used the base installation of R to produce our QQplots. 
# However, there are many packages, which provide prettier representations of QQplots.
# Probably the most common package for graphics in R is the ggplot2 package.

install.packages("ggplot2")      # Install & load ggplot2
library("ggplot2")

# The ggplot2 package takes data frames as input, so let's convert our numeric vector of Example 1 to a data frame:
data <- data.frame(x)            # Create data frame containing x

# Now, we can use the stat_qq and stat_qq_line functions of the ggplot2 package to create a QQplot:
# <pre lang="csharp">ggplot(data, aes(sample = x)) +  # Create QQplot with ggplot2 package
stat_qq() +
stat_qq_line(col = "red")
# ======>
# END QQPLOT



# QUANTILE Function in R (6 Examples)
# Quantile function
# The inverse cumulative distribution function (quantile function) of the logistic distribution is a generalization of
# the logit function. Its derivative is called the quantile density function. 

# Basic R Syntax of quantile():
quantile(x)

# ==>
# Example 1: Basic Application of quantile() in R
# In the first example, I'll illustrate how to use the quantile function in its simplest way. 
# Let's create an exemplifying numeric vector first:
  
set.seed(15051)                         # Set seed for reproducibility 
x <- round(runif(1000, 0, 100))         # Create uniformly distributed data
x                                       # Print data to RStudio console

# Our example vector contains 1,000 elements between the range of 1 and 100.
# Now, we can apply the quantile R function to this vector as follows:
  
quantile(x)                             # Apply quantile function

# Note: By default, the quantile function is returning the quartile (i.e. five cutpoints). 
# Later on, I'll show you how to get other metrics as well.
# Our vector contains 10000 random values, which follows a normal distribution.
# Now, we can use the qqnorm function to create a QQplot of this vector.

qqnorm(x)   # QQplot of normally distributed values

# and the qqline function to add a theoretical line according to the normal distribution:
qqline(x, col = "red")           # Add qqline to plot


# ==>
# Example 2: Handling NA Values with the quantile Function
# In this example, you'll learn how to deal with missing data (i.e. NA values) in the input vector. 
# Let's first insert an NA value to our example data:
  
x_NA <- c(x, NA)                        # Create example data with NA

#Now, if we apply the quantile function to this vector, the quantile function returns an error message:
  
quantile(x_NA)                          # Apply quantile function to NA vector

# Error in quantile.default(x_NA)
# Fortunately, we can easily fix this error by specifying na.rm = TRUE within the quantile command:
  
  quantile(x_NA, na.rm = TRUE)            # Use na.rm argument
# 0%  25%  50%  75% 100% 
# 0   23   50   75  100
  
  

# ==>
# Example 3: Extract Quantile Values Only  
# AS you have seen based on the previous examples, the quantile function returns the 
# cutpoints AND the corresponding values to the RStudio console. In some cases, 
# however, we might prefer to keep only the quantile values.
# In this case, we can simply apply the unname function to the output of the quantile
# function. Have a look at the following R code:
  
unname(quantile(x))                     # Get only the quantile values 


# ==>
# Example 4: Quantile by Group
# In this example I'll show you how to calculate the quantiles of certain subgroups. 
# For the example, I'm going to use the Iris data matrix. Let's load the data to R:

data(iris)                              # Load Iris data
head(iris)                              # Head of Iris data

# The Iris data set contains several numeric variables and the grouping variable Species.

# In order to compute the quantile by group, we also need some functions of the dplyr 
# environment. We can install and load the dplyr package as follows:
  
install.packages("dplyr")               # Install dplyr package
library("dplyr")                        # Load dplyr package

# We can now produce a data matrix of quantiles of the first column grouped by the 
# Species column with the following R syntax:
  
do.call("rbind",
tapply(iris$Sepal.Length,       # Specify numeric column
iris$Species,                   # Specify group variable
    quantile))  
  

# ==> 
# Example 5: Quartiles, Quintiles, Deciles, Percentiles & Many More
# As I told you before, the quantile function returns the quartile of the input vector by 
# default. However, we can use the probs argument to get basically any quantile metric that we 
# want. With the following R codes, we can calculate the median.

quantile(x, probs = 0.5)                # Median

# .tertiles.

quantile(x, probs = seq(0, 1, 1/3))     # Tertiles

# .quartiles (as it would also be computed by default).

quantile(x, probs = seq(0, 1, 1/4))     # Quartiles
# 0%  25%  50%  75%  100% 
# 0   23   50   75   100
.quintiles.

quantile(x, probs = seq(0, 1, 1/5))     # Quintiles
# 0%  20%  40%  60%  80% 100% 
# 0   18   40   61   80  100
.sextiles.

quantile(x, probs = seq(0, 1, 1/6))     # Sextiles
# 0% 16.66667% 33.33333% 50% 66.66667% 83.33333% 100% 
# 0  15        34        50  68        83        100
.septiles.

quantile(x, probs = seq(0, 1, 1/7))     # Septiles
# 0% 14.28571% 28.57143% 42.85714% 57.14286% 71.42857% 85.71429% 100% 
# 0  13        27        43        58        72        86        100
.octiles.

quantile(x, probs = seq(0, 1, 1/8))     # Octiles
# 0%     12.5%   25%     37.5%   50%     62.5%   75%     87.5%  100% 
# 0.000  11.875  23.000  38.000  50.000  63.000  75.000  88.000 100.000
.deciles.

quantile(x, probs = seq(0, 1, 1/10))    # Deciles
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 0   9    18   29   40   50   61   71   80   90  100

#.duo-deciles.

quantile(x, probs = seq(0, 1, 1/12))    # Duo-deciles or dodeciles

#.hexadeciles.

quantile(x, probs = seq(0, 1, 1/16))    # Hexadeciles

#.ventiles.

quantile(x, probs = seq(0, 1, 1/20))    # Ventiles, vigintiles, or demi-deciles

#.percentiles.

quantile(x, probs = seq(0, 1, 1/100))   # Percentiles

#.or permilles:
  
quantile(x, probs = seq(0, 1, 1/1000))  # Permilles or milliles
  
  
# Example 6: How to Visualize Quantiles
# Quantiles are often used for data visualization, most of the time in so called Quantile-Quantile plots.

# Quantile-Quantile plots can be created in R based on the qqplot function. Let's do this in practice!
# First, we need to create a second vector:
  
y <- x + rnorm(1000, 0, 30)             # Create y-data

# Now, we can print a qqplot of our two example vectors with the qqplot function as follows:
  
qqplot(x, y)                            # Quantile-Quantile plot of x & y





  
