
library(readr)
#require(readxl)

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


x0 <- read_csv("test.csv")
x0_re <- x0[, sapply(x0, IsConstant)]
write_csv(x0_re, 'test_1.csv')




