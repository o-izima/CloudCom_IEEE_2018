library(tidyverse)
library(readr)
library(dplyr)

setwd("G:/My Drive/realm-cnsm2015/periodic-load")
#read in the entire file and select only the metrics for server X0
X <- read_csv("X.csv")

X0 <- select(X, starts_with("X0"))


write_csv(X0, 'X0_only.csv')
