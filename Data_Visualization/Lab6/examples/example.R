#network.r
library(ggraph)
library(igraph)
library(visNetwork)

nodes <- read.csv("nodes.csv", header=T, as.is=T)
links <- read.csv("edges.csv", header=T, as.is=T)

## Collapsing multiple links into one

links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL

nodes$label=nodes$media
#net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
#visIgraph(net)
visNetwork(nodes, links)

nodes$group=nodes$type.label
nodes$value=nodes$audience.size
links$width=links$weight
visNetwork(nodes, links)%>%visLegend()

visNetwork(nodes,links)%>%visIgraphLayout(layout="layout_in_circle")

# Community identification
nodes1<-nodes
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
ceb <- cluster_edge_betweenness(net) 
nodes1$group=ceb$membership
visNetwork(nodes1,links)%>%visIgraphLayout()


#adjacency representation

netm <- get.adjacency(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

rowdist<-dist(netm)

library(seriation)
order1<-seriate(rowdist, "HC")
ord1<-get_order(order1)

reordmatr<-netm[ord1,ord1]

library(plotly)

plot_ly(z=~reordmatr, x=~colnames(reordmatr), 
        y=~rownames(reordmatr), type="heatmap")














#motion.r
library(plotly)

m=matrix(nrow=0,ncol=3)
for (a in seq(0,3,by=0.03)) {
  x<-seq(0,2,0.01)
  y<-x^a
  m<-rbind(m,cbind(x,y,a))
}

df=as.data.frame(m)

plot_ly(df, x=~x, y=~y, frame =~a)%>%add_lines()%>%animation_opts(
  100, easing = "cubic", redraw = F
)
















#tours.r
#A modified code from plotly's website

library(tourr)
library(plotly)

mat <- rescale(mtcars[,c(3:5)])
set.seed(12345)
tour <- new_tour(mat, grand_tour(), NULL)
#tour<- new_tour(mat, guided_tour(cmass), NULL)

steps <- c(0, rep(1/15, 200))
Projs<-lapply(steps, function(step_size){  
  step <- tour(step_size)
  if(is.null(step)) {
    .GlobalEnv$tour<- new_tour(mat, guided_tour(cmass), NULL)
    step <- tour(step_size)
  }
  step
}
)

# projection of each observation
tour_dat <- function(i) {
  step <- Projs[[i]]
  proj <- center(mat %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], state = rownames(mat))
}

# projection of each variable's axis
proj_dat <- function(i) {
  step <- Projs[[i]]
  data.frame(
    x = step$proj[,1], y = step$proj[,2], variable = colnames(mat)
  )
}

stepz <- cumsum(steps)

# tidy version of tour data

tour_dats <- lapply(1:length(steps), tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)

# tidy version of tour projection data
proj_dats <- lapply(1:length(steps), proj_dat)
proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
proj_dat <- dplyr::bind_rows(proj_datz)

ax <- list(
  title = "", showticklabels = FALSE,
  zeroline = FALSE, showgrid = FALSE,
  range = c(-1.1, 1.1)
)

# for nicely formatted slider labels
options(digits = 3)
tour_dat <- highlight_key(tour_dat, ~state, group = "A")
tour <- proj_dat %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black")) %>%
  add_segments(xend = 0, yend = 0, color = I("gray80")) %>%
  add_text(text = ~variable) %>%
  add_markers(data = tour_dat, text = ~state, ids = ~state, hoverinfo = "text") %>%
  layout(xaxis = ax, yaxis = ax)#%>%animation_opts(frame=0, transition=0, redraw = F)
tour


#linking
mtcars1<-mtcars
mtcars1$state<-rownames(mtcars)
mtcars2<-highlight_key(mtcars1, ~state, group="A")
barChart<-plot_ly(mtcars2,x=~factor(cyl))%>%add_histogram()



subplot(tour, barChart%>%layout(barmode="overlay"))%>%
  highlight(persistent = TRUE, dynamic = TRUE)%>%hide_legend()
















#treeMap.r
library(ggraph)
library(igraph)

graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)

ggraph(graph, 'treemap', weight = 'size') + 
  geom_node_tile(aes(fill = depth), size = 0.25)+
  geom_node_text(label=flare$vertices$shortName, size=3)

devtools::install_github("d3TreeR/d3TreeR")

library(treemap)
library(d3treeR)
data(GNI2014)
d3tree2(treemap(GNI2014,
                index=c("continent", "iso3"),
                vSize="population",
                vColor="GNI",
                type="value",
                format.legend = list(scientific = FALSE, big.mark = " ")),
        rootname = "World")



















#treeVis.r
library(ggraph)
library(igraph)
graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)

ggraph(graph, 'circlepack', weight = 'size') + 
  geom_edge_link() + 
  geom_node_point(aes(colour = depth)) +
  geom_node_text(label=flare$vertices$shortName, size=1)+
  coord_fixed()

library(visNetwork)
library(rpart)

# Basic classification tree
crabs=read.csv("australian-crabs.csv")
res <- rpart(as.factor(sex)~., data=crabs)
visTree(res, main = "Iris classification Tree", width = "100%")















#sunBurst.r
library(ggraph)
library(igraph)

graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)

ggraph(graph, 'partition', circular = TRUE) + 
  geom_node_arc_bar(aes(fill = depth), size = 0.25)+
  geom_node_text(label=flare$vertices$shortName, size=3)


















#circlePacking.r
library(ggraph)

graph <- graph_from_data_frame(flare$edges, vertices = flare$vertices)

ggraph(graph, 'circlepack', weight = 'size') + 
  geom_node_circle(aes(fill = depth), size = 0.25, n = 50) + 
  coord_fixed()

