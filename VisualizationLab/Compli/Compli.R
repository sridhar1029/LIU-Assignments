library(ggplot2)
library(plotly)
library(readxl)
library(MASS)
library(gridExtra)
#Lab 2 Assignment 2
#Q1

bball = read_xlsx("baseball-2016.xlsx", sheet = "Sheet1", col_names = TRUE)
head(bball)
smp_size <- floor(0.30 * nrow(bball))
## set the seed to make your partition reproducible
set.seed(12345)
rand_ind <- sample(seq_len(nrow(bball)), size = smp_size)
rand_selected = bball[rand_ind, ]
head(rand_selected)

#Q2
bball.numeric = scale(rand_selected[,3:27])
distance = dist(bball.numeric)
res = isoMDS(distance, k=2, p=2)
coords = res$points

coordsMDS = as.data.frame(coords)
coordsMDS$name = rownames(coordsMDS)
coordsMDS$league = rand_selected$League
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
              text = ~paste('Obj1: ', rownames(rand_selected)[index1],
                            '<br> Obj 2: ', rownames(rand_selected)[index2]))%>%
  add_lines(x=~sh$x, y=~sh$yf)

#Q4
rand_selected$V1 = coordsMDS$V1
rand_selected$V2 = coordsMDS$V2

cols_bball = c("Team", "League", "Won", "Lost", "Runs.per.game", "HR.per.game", 
               "AB", "Runs", "Hits", "TwoB", "ThreeB", "HR", "RBI", "StolenB", "CaughtS",
               "BB", "SO", "BAvg", "OBP", "SLG", "OPS", "TB", "GDP", "HBP", "SH",
               "SF", "IBB", "LOB", "V1", "V2")
colnames(rand_selected) = cols_bball
dim(rand_selected)[2]
plots_bball = list()
for(i in 2:27){
  pl_name = paste("P", i, sep = '')
  col_name = cols_bball[i]
  plots_bball[[pl_name]] = ggplot(rand_selected, aes_string("V1", col_name)) + 
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
  plots_bball2[[pl_name]] = ggplot(rand_selected, aes_string("V2", col_name)) + 
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






##Assignment 1
###Q1
library(visNetwork)
library(tidyverse)
library(igraph)
library(plotly)
library(seriation)
library(tourr)

nodes<-read.table("trainMeta.dat")
smp_size <- floor(0.30 * nrow(nodes))
set.seed(12345)
rand_ind <- sample(seq_len(nrow(nodes)), size = smp_size)
nodes = nodes[rand_ind, ]
colnames(nodes)<-c("label","group")
nodes$id<-rownames(nodes)
nodes<-nodes[,c(3,1,2)]
nodes$title<-nodes$label
nodes$color<-ifelse(nodes$group==1,"red","blue")
nodes<-data.frame(nodes)

all_links<-read.table("trainData.dat")
temp_links =  all_links[all_links$V1 == nodes$id[1],]
for(i in 2:length(nodes$id)){
  temp = all_links[all_links$V1 == nodes$id[i],]
  temp_links = rbind(temp_links, temp)
}
links =  temp_links[temp_links$V2 == nodes$id[1],]
for(i in 2:length(nodes$id)){
  temp = temp_links[temp_links$V2 == nodes$id[i],]
  links = rbind(links, temp)
}

colnames(links)<-c("from","to","value")
links<-data.frame(links)

weight_nodes<-graph.data.frame(d=links,vertices=nodes,directed = F)
degree_nodes<-degree(weight_nodes,mode="all")
nodes$value<-degree_nodes[match(nodes$id,names(degree_nodes))]


q1<-visNetwork(nodes,links)%>%visPhysics(solver="repulsion") %>%
  visOptions(highlightNearest = list(enabled = T,degree=0,
                                     hover = T),nodesIdSelection=TRUE,
             selectedBy = "group") 

q1



###Q2
q2<-visNetwork(nodes,links)%>%visPhysics(solver="repulsion") %>%
  visOptions(highlightNearest = list(enabled = T,degree=1,
                                     hover = T),nodesIdSelection=TRUE,
             selectedBy = "group")

q2


###Q3
nodes1<-nodes
ceb<-cluster_edge_betweenness(weight_nodes)
nodes1$group<-ceb$membership
visNetwork(nodes1,links)%>%visIgraphLayout(layout = "layout_nicely")%>%
  visOptions(highlightNearest = list(enabled = T,degree=1,
                                     hover = T),nodesIdSelection=TRUE, selectedBy = "group") 



###Q4
netm<-get.adjacency(weight_nodes,sparse = F)
colnames(netm)<-nodes$label
rownames(netm)<-nodes$label
rowdist<-dist(netm)

row_order<-seriate(rowdist,"HC")
order1<-get_order(row_order)
netm_reord<-netm[order1,order1]

plot_ly(z=~netm_reord,x=~colnames(netm_reord),
        y=~rownames(netm_reord),type="heatmap")%>% 
  layout(title = " Madrid Bombing Heatmap for finding  clusters")

