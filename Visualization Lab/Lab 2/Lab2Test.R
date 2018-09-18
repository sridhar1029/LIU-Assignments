library(ggplot2)
library(plotly)
#Assignment 1
#Q1
d = read.csv("olive.csv", header = T, row.names = 1)
head(d, 10)

ggplot(d, aes(palmitic, oleic, col = linolenic)) + geom_point()

disc = cut_interval(d$linolenic, 4)
ggplot(d, aes(palmitic, oleic, col = disc)) + geom_point()


#Q2
ggplot(d, aes(palmitic, oleic, col = disc)) + geom_point()
ggplot(d, aes(palmitic, oleic, size = disc)) + geom_point()
ggplot(d, aes(palmitic, oleic)) + geom_point() + 
    geom_spoke(angle = d$linolenic, radius = 40)


?geom_spoke()

#Q3
ggplot(d, aes(oleic, eicosenoic, col = Region)) + geom_point()


#Q4
ggplot(d, aes(d$oleic, d$eicosenoic, col = cut_interval(d$linoleic, 3),
              shape = cut_interval(d$palmitic, 3), 
              size = cut_interval(d$palmitoleic, 3))) + geom_point()


#Q5
ggplot(d, aes(d$oleic, d$eicosenoic, col = d$Region,
              shape = cut_interval(d$palmitic, 3), 
              size = cut_interval(d$palmitoleic, 3))) + geom_point()

#Q6
p <- plot_ly(d, labels = ~Area, type = 'pie', showlegend = FALSE) %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960')
p

#Q7
ggplot(d, aes(d$linoleic, d$eicosenoic)) + geom_density2d()


#Assignment 2
#Q1
library(xlsx)
d2 = read.xlsx("baseball-2016.xlsx", sheetName = "Sheet1", header = TRUE, 
               row.names = 1)
head(d2)

