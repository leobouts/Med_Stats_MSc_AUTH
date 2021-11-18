
#--------------------------------------------------------------------------------#
# DATA STRUCTURES
#--------------------------------------------------------------------------------#
# Author: Konstantinos I. Bougioukas
# Date: 2021-10-07





# scalars (one-element atomic vector) -----------------------------------------


x <- 2.5   # number
x


# we can also use = but better use  <-
x <-  5
x

# note: 'x' is overwritten




x <- TRUE  # logical
x




x <- "Hello World!"    # character
x

x <- 'Hello World!'    # character
x

# note: character strings need to be in quotes (double or single) and can have spaces







# numeric vectors -----------------------------------------------------------------

a <- c(2,5,4)  # numeric vector
a

# note: c() stands for `concatenate' function

# we can add spaces after the comma (suggested)

a <- c(2, 5, 4)
a




# quickly create a vector of consecutive numbers with the colon operator

a <- c(20:100)
a

# actually we don't need c() here

a <- 20:100
a

# note: if the vector is too long to fit into a single line, the output is
# wrapped and the numbers in brackets (e.g., [1]) indicate the position of
# the value that comes next






# For more general sequences we can use the `seq()` function

a <- seq(from = 20, to = 100, by = 0.5)  # explicitly named arguments
a

# positional matching: match the arguments in the order that they appeared
seq(20, 100, 0.5)


seq(20, 100)
# Note: Since we didn't specify step size with 'by', the default value 1 is used


# What about this?
seq(100, 20, by = -0.5)





# logical vectors ----------------------------------------------------------------
b <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
b
class(b)


# can be abbreviated to T/F

b <- c(T, F, T, T, T, T, T, F)
b
class(b)


# note: the logicals behave like numbers (F=0, T=1) in mathematical functions
table(b)
sum(b) 
mean(b)




# character/string vectors --------------------------------------------------------

d <- c("Male", "Male", "Male", "Female", "Female", "Female")  # character vector
d

class(d)




# special types of vectors --------------------------------------------------------

# a factor variable is a special kind of vector

f <- as.factor(d)  
f  

class(f)
levels(f)  



# also dates are objects with their own class

g <- as.Date(c("2020-02-18", "2020-05-27"))
g
class(g)




# coercion: mixing things ---------------------------------------------------------

# hierarchy for coercion: logical <  numeric < character



v <- c(2, 5, "Bob", "Sue", "John")  # numbers and characters
v

class(v)




u <- c("false", FALSE, T, "True")   # logicals and characters
u

class(u)



# if you mix logicals and numbers, you get a numeric vector (coercion: F=0, T=1)

w <- c(TRUE, FALSE, 1)
w
class(w)




# missing values -----------------------------------------------------------------

x <- c(2, ,4)

x <- c(2, NA, 4)  # note: NA = not available (another special keyword)
x

# NA is of logical class
class(NA)


# vectorized operations -----------------------------------------------------------

x <- c(2, 4, 3, 5, 7)
x


y <- c(3, 5, 4, 8, 9)
y

x + y    # add the elements of two vectors in an element-wise fashion
x * y    # multiply the elements of two vectors in an element-wise fashion



# Note: Vector recycling if we supply a sorter vector
# Example:
x

y2 <- c(3, 5, 4)
y2

x + y2



# comparisons with relational operators --------------------------------------------
# note: that they lead to logicals)

x <- c(-2, 4, 6, 3, 5)
x

x > 3
x >= 3
x < 3
x <= 3
x == 3

x != 3


# and/or for vectors (the parentheses are not necessary here, but make the code clearer)

(x > 3) & (x < 6)
(x > 3) | (x < -2)





# Subsetting vectors

z <- c("a", "b", "c", "d", "e")

z[2]                       # extract only the second element

z[2:4]                     # extract the 2nd, 3rd, and 4th elements

z[4:2]                     # return in the order that we specify

z[c(4, 4, 1, 2, 2)]        # extract the same element more than once



# skipping elements

z[-2]                       # extract only the second element

z[-2:4]                     # this is wrong!

z[-(2:4)]                  # this is what we want


z[c(TRUE, FALSE, FALSE, FALSE, TRUE)]   # use logicals (indexing by condition)






# matrix ----------------------------------------------------------------------------

# 5x4 matrix filled by columns (default)

y1 <- matrix(1:20, nrow = 5, ncol = 4)
y1

# matrix filled by rows
y2 <- matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
y2

# logical matrix
x1 <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)
y3 <- matrix(x1, nrow = 2, ncol = 3)
y3

# select the second row
y1
y1[2, ]


# select the 2nd column
y1[ , 2]

# select the element in the 2nd row, 3rd column
y1[2, 3]



# The transpose (anastrofos in greek) of a matrix

A <-matrix(c(4, -1, -5, 0, 1, -2), 2, 3, byrow = TRUE)
A

t(A)


# Multiplying a scalar with a matrix

-3 * A


# Element-wise multiplication of two matrices of the same dimensions

B <-matrix(c(3, 1, -5, 0, 2, -2), 2, 3, byrow = TRUE)
B

A * B



# The dot product (inner product) of two matrices

P <-matrix(c(3, 0, -5, -1, -3, 4), nrow = 2, ncol = 3, byrow = TRUE)
P

Q <-matrix(c(-5, 5, 2, 1, -2, 0), nrow = 3, ncol = 2, byrow = TRUE)
Q


P %*% Q 



# Matrix crossproduct

A
B

t(A) %*% B  # or crossprod(A, B)




# Application: calculate the average using matrices

my_values <- c(2, 5, 7, -4, 8, 6, 3)

mean(my_values) # using the mean() function



n <- length(my_values) # get the length (number of elements) of vector
U <- matrix(1, n, 1)
U

V <- matrix(my_values, n, 1)
V


average_my_values <- t(U) %*% V/n   # average using matrices and inner product
average_my_values




# Arrays

my_array <- array(1:24, dim = c(2,3,4))
my_array





# Lists
my_list <- list(
  numbers = 1:5,
  strings = c("apple", "orange"),
  logicals = TRUE)

my_list


my_list[1]  # extract the first list item and preserve output as a list

class(my_list[1])




my_list[[1]]  # extract the first list item and simplify it to a vector

class(my_list[[1]])


my_list$numbers  # access the content of the list by typing the name of a list item




# Subset list to get individual elements out of a list item

my_list[[2]][2] # using the index

my_list[["strings"]][2] # using the name of the list item





# data frame ----------------------------------------------------------------------

# build dataframes using vectors
library(tidyverse)
library(lubridate)
library(rstatix)



patientID <- c(1, 2, 3, 4, 5, 6, 7, 8)

age <- c(25, 30, 28, 22, 31, 45, 37, 43)

weight <- c(94, 83, 71, 87, 94, 73, 89, 74)

diabetes <- c("Type 1", "Type 2", "Type 1", "Type 1",
              "Type 2", "Type 1", "Type 1", "Type 2")

status <- c("Poor", "Improved", "Excellent", "Poor",
            "Poor","Excellent", "Improved", "Improved")

dates <- ymd("2020-10-09", "2020-10-12", "2020-10-18", "2020-10-27",
             "2020-11-04", "2020-11-09", "2020-11-22", "2020-12-02")



# create the dataframe
patient_data <- tibble(patientID, age, weight, diabetes, status, dates) # insert all the variables
patient_data


typeof(patient_data)
class(patient_data)
dim(patient_data)
attributes(patient_data)


# remove the variables (we don't need them as we have the 'patient_data')
rm("patientID", "age", "weight", "diabetes", "status", "dates")



# access the variable age
age
patient_data$age

# example with $
table(patient_data$diabetes, patient_data$status)



# convert from character to factor

patient_data <- convert_as_factor(patient_data, diabetes, status)
patient_data


# or using mutate() function from the {tidyverse}
#patient_data <- mutate(patient_data, across(where(is.character), factor))




# show the levels of `diabetes` variable
patient_data$diabetes


# show the levels of `status variable
patient_data$status


# reverse the order of the levels
patient_data$status <- fct_rev(patient_data$status)

patient_data$status



# The status variable has a natural ordering between its categories

patient_data$status <- factor(patient_data$status, ordered = TRUE)
patient_data$status

