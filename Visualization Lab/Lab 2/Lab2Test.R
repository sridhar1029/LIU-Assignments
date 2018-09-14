library(ggplot2)
library(plotly)

d = read.csv("olive.csv", header = T)
head(d)


ggplot(d, aes(palmitic, oleic, col = linolenic)) + geom_point()
