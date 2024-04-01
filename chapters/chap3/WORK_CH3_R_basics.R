## basics of R

# Setting Seed
number <- 123
set.seed(number)

# This is a comment
x <- c(1, 2, 3, 4, 5)  # Create a numeric vector
y <- mean(x)            # Calculate the mean of the vector
print(y)                # Print the result


# vectors or lists can be specified like this
vec <- c(1, 2, 3)

# access element
vec[0]

# matrices
mat <- matrix(1:4, nrow=2, ncol=2)


# dataframes
df <- data.frame(name=c("Alice", "Bob"), age=c(25, 30))

# Inspecting Data
n = 10 # (top 10 rows to inspect)
head(df , n)

# list
lst <- list(name="Alice", age=25, grades=c(80, 90))

# factors
x <- c(1, 2, 3) 
factor_x <- structure(x, class = "factor", .Label = c("low", "medium", "high")) 

# sequences
seq1 <- 1:5
seq2 <- seq(10, 100, 2)

#  
# Basic Statistical Functions
x <- 1:20
mean(x)
sd(x)


#### Other essential Functions and Approaches
# view current working dir
getwd()
# set working dir
setwd("path string")

# lists objects
ls()
# removes them
rm()

# Installing Packages
# install.packages("PackageName")

# Package Documentation
help(package = "ggplot")

# Documentation 
help.start()
# for general help, `args(function)` for function arguments.
