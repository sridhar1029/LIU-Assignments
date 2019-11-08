library(ggplot2)
library(dplyr)

# A1

temp_data = read.csv("TempLinkoping.txt", sep = "\t")
temp_data = tbl_df(temp_data)
print(temp_data)

my_rinv_chisquare <- function(n, df, tow){
  X <- rchisq(n, df)
  return(((df)*tow)/X)
}

rm(list = ls())
