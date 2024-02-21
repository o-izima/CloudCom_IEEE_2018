#install.packages("readr")

require(readr)
#require(readxl)

IsValuable <- function (x0_column) {
  # Checks if the column contains different values.
  #
  # Args:
  #   x0_column: a vector (a x0_full column in this case)
  #
  # Returns:
  #   True if the column contains different values, and False otherwise.
  number.of.values <- length(unique(x0_column))
  is.valuable = ifelse(number.of.values == 1, FALSE, TRUE)
  return (is.valuable)
}
#####Original script
#
#x0 <- read_csv("X0_only.csv")
#x0_re <- x0[, sapply(x0, IsValuable)]
#write_csv(x0_re, 'X0_altered.csv')

X_r <- read_csv("X.csv")
x0 <- X_r
x0_re <- x0[, sapply(x0, IsValuable)]

#Remove constantly increasing values
IsConstant <- function (x0_column) {
  # Checks for constantly increasing numbers / pairwise 
  #
  # Args:
  #   x0_column: a vector (a x0_full column in this case)
  #
  # Returns:
  #   True if the column contains different values, and False otherwise.
  
  aa = diff(x0_column)
  number.of.values <- length(unique(aa))
  is.valuable = ifelse(number.of.values == 1, FALSE, TRUE)
  return (is.valuable)
}


#x0 <- read_csv("test.csv")
x0_re <- x0_re[, sapply(x0_re, IsConstant)]
x0_re <- x0_re[,-1]
write_csv(x0_re, 'X_flash.csv')

X_flash <- x0_re
