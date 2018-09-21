library(ggplot2)
library(plotly)
library(xlsx)
library(MASS)
library(gridExtra)
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
ggplot(ol, aes(ol$linoleic, ol$eicosenoic)) + geom_point()


#Assignment 2
#Q1

bball = read.xlsx("baseball-2016.xlsx", sheetName = "Sheet1", header = TRUE,
                  row.names = 1)
head(bball)

#Q2
bball.numeric = bball[,3:27]
distance = dist(bball.numeric)
res = isoMDS(distance, k=2, p=2)
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
              text = ~paste('Obj1: ', rownames(bball)[index1],
                            '<br> Obj 2: ', rownames(bball)[index2]))%>%
  add_lines(x=~sh$x, y=~sh$yf)

#Q4
bball$V1 = coordsMDS$V1
bball$V2 = coordsMDS$V2

cols_bball = colnames(bball)
dim(bball)[2]
plots_bball = list()
for(i in 2:27){
  pl_name = paste("P", i, sep = '')
  col_name = cols_bball[i]
  plots_bball[[pl_name]] = ggplot(bball, aes_string("V1", col_name)) + 
    geom_point() + geom_line(aes(x=0))
}
grid.arrange(grobs = plots_bball, ncol = 6, nrow = 6)

plots_bball$P2
plots_bball$P3
plots_bball$P4 #Runs per game
plots_bball$P5
plots_bball$P6 #AB
plots_bball$P7 #Runs **
plots_bball$P8 #Hits **
plots_bball$P9
plots_bball$P10
plots_bball$P11
plots_bball$P12 #RBI **
plots_bball$P13
plots_bball$P14
plots_bball$P15
plots_bball$P16
plots_bball$P17 #BAvg
plots_bball$P18
plots_bball$P19 #SLG
plots_bball$P20 #OPS  **
plots_bball$P21 #TB  **
plots_bball$P22
plots_bball$P23
plots_bball$P24
plots_bball$P25
plots_bball$P26
plots_bball$P27

plots_bball2 = list()
for(i in 2:27){
  pl_name = paste("P", i, sep = '')
  col_name = cols_bball[i]
  plots_bball2[[pl_name]] = ggplot(bball, aes_string("V2", col_name)) + 
    geom_point() + geom_line(aes(x=0))
}
grid.arrange(grobs = plots_bball2, ncol = 6, nrow = 6)
plots_bball2$P2
plots_bball2$P3
plots_bball2$P4
plots_bball2$P5
plots_bball2$P6
plots_bball2$P7
plots_bball2$P8
plots_bball2$P9
plots_bball2$P10
plots_bball2$P11
plots_bball2$P12
plots_bball2$P13
plots_bball2$P14
plots_bball2$P15
plots_bball2$P16
plots_bball2$P17
plots_bball2$P18
plots_bball2$P19
plots_bball2$P20
plots_bball2$P21
plots_bball2$P22
plots_bball2$P23
plots_bball2$P24
plots_bball2$P25
plots_bball2$P26
plots_bball2$P27

