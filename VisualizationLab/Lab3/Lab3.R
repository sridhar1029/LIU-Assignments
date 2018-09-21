library(ggplot2)

data1 = read.csv("aegypti_albopictus.csv", header = TRUE, row.names = 2)
head(data1)


## Assignment 2

data2 = read.csv("SwedishHousehold.csv", skip = 3, col.names = c("Region", "Age", "Income"))
head(data2)
my_strip <- function(region) {
  region = strsplit(as.character(region), " ")[[1]][2]
}

data2$Region = lapply(data2$Region, my_strip)
levels(data2$Age) = c("Young", "Adult", "Senior")
head(data2)



ggplot(data2, aes(factor(Age), Income)) ? geom_violin()
ggplot()