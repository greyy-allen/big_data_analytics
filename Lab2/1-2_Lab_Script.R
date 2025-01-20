# Variable Assignment -----------------------------------------------------

50 + 40
a <- 50 + 40
a
a <- 100
a
a <- "happy"
a
a <- 100
a <- a + 10 # evaluate the right first, then assign
a


# Vectors -----------------------------------------------------------------

my_vector <- c(6, 7, 8) # vector: a group of elements
my_vector
another_vector <- c("apple", "banana")
another_vector
class(my_vector)
class(another_vector)
bad_vector <- c(1, "banana")
class(bad_vector)

my_vector[2]
another_vector[2]


# Functions ---------------------------------------------------------------

sum(my_vector)
mean(my_vector)
max(my_vector)
min(my_vector) # functions have this form: functionName()


# Lists -------------------------------------------------------------------

my_list <- list(fruits = c("apple", "banana"), nums = c(1, 2, 3, 4), chars = c("a", "b", "c"))
my_list
class(my_list)
str(my_list)
my_list$fruits
my_list$nums
my_list$chars
my_list$fruits[2]
my_list$chars[2]
sum(my_list$nums)


# Data Frames: a special kind of List -------------------------------------------------------------

data(mtcars)
View(mtcars)
class(mtcars)
str(mtcars)
mtcars$mpg
max(mtcars$mpg)
which(mtcars$mpg == max(mtcars$mpg))
mtcars[20, ] # notice the comma
mtcars[20:22, ]


# save workspace
saveRDS(my_vector, file = "myVector.rds")
# clean workspace
myVector <- readRDS("myVector.rds")
# clean workspace
# load workspace

# Packages ----------------------------------------------------------------

# install ggplot2
library(ggplot2)
data(package = "ggplot2")
data(mpg)
str(mpg)
View(mpg)
ggplot(data = mpg, aes(x = hwy, y = cty)) +
  geom_point(aes(color = cyl))


# vosonDash ---------------------------------------------------------------

install.packages("remotes")
library(remotes)
remotes::install_github("vosonlab/vosonSML") # only do this once on your own PC
remotes::install_github("vosonlab/VOSONDash") # only do this once on your own PC
install.packages(c("DT","shinydashboard","shinyjs","visNetwork")) # only do this once on your own PC
library(vosonSML)
library(VOSONDash)
runVOSONDash()
