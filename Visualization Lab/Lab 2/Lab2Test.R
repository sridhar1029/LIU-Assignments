library(ggplot2)
library(plotly)
#Assignment 1
#Q1
ol = read.csv("olive.csv", header = T, row.names = 1)
head(ol, 10)

ggplot(ol, aes(palmitic, oleic, col = linolenic)) + geom_point()

disc = cut_interval(ol$linolenic, 4)
ggplot(ol, aes(palmitic, oleic, col = disc)) + geom_point()


#Q2
ggplot(ol, aes(palmitic, oleic, col = disc)) + geom_point()
ggplot(ol, aes(palmitic, oleic, size = disc)) + geom_point()
ggplot(ol, aes(palmitic, oleic)) + geom_point() + 
    geom_spoke(angle = ol$linolenic, radius = 40)

#Q3
ggplot(ol, aes(oleic, eicosenoic, col = Region)) + geom_point()


#Q4
ggplot(ol, aes(ol$oleic, ol$eicosenoic, col = cut_interval(ol$linoleic, 3),
              shape = cut_interval(ol$palmitic, 3), 
              size = cut_interval(ol$palmitoleic, 3))) + geom_point()


#Q5
ggplot(ol, aes(ol$oleic, ol$eicosenoic, col = ol$Region,
              shape = cut_interval(ol$palmitic, 3), 
              size = cut_interval(ol$palmitoleic, 3))) + geom_point()

#Q6
p <- plot_ly(ol, labels = ~Area, type = 'pie', showlegend = FALSE) %>%
  layout(title = 'Pie Chart Area')
p

#Q7
ggplot(ol, aes(ol$linoleic, ol$eicosenoic)) + geom_density2d()


#Assignment 2
#Q1
library(xlsx)
library(MASS)
bball = read.xlsx("baseball-2016.xlsx", sheetName = "Sheet1", header = TRUE,
                  row.names = 1)
head(bball)

#Q2
bball.numeric = scale(bball[,3:27])
distance = dist(bball.numeric)
res = isoMDS(distance, k=2)
coords = res$points

coordsMDS = as.data.frame(coords)
coordsMDS$name = rownames(coordsMDS)
coordsMDS$league = bball$League
plot_ly(coordsMDS, x=~V1, y=~V2, type="scatter", mode = "markers"
        , hovertext=~name, color= ~league)


#Q3
sh <- Shepard(distance, coords)
delta <-as.numeric(distance)
D<- as.numeric(dist(coords))

n=nrow(coords)
index=matrix(1:n, nrow=n, ncol=n)
index1=as.numeric(index[lower.tri(index)])

n=nrow(coords)
index=matrix(1:n, nrow=n, ncol=n, byrow = T)
index2=as.numeric(index[lower.tri(index)])

plot_ly()%>%
  add_markers(x=~delta, y=~D, hoverinfo = 'text',
              text = ~paste('Obj1: ', rownames(music)[index1],
                            '<br> Obj 2: ', rownames(music)[index2]))%>%
  add_lines(x=~sh$x, y=~sh$yf)

#Q4
