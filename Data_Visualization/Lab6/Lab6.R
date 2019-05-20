##Assignment 1
###Q1
library(visNetwork)
library(tidyverse)
library(igraph)
library(plotly)
library(seriation)
library(tourr)


nodes<-read.table("trainMeta.dat")
colnames(nodes)<-c("label","group")
nodes$id<-rownames(nodes)
nodes<-nodes[,c(3,1,2)]
nodes$title<-nodes$label
nodes$color<-ifelse(nodes$group==1,"red","blue")
nodes<-data.frame(nodes)

links<-read.table("trainData.dat")
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





##Assignment 2

###Q1

oil_data = read.csv("Oilcoal.csv", header = TRUE, sep = ";", dec = ",")
oil_data$X = NULL
head(oil_data)


p1 <- oil_data %>%
  plot_ly(
    x = ~Oil, 
    y = ~Coal,
    color = ~Country,
    frame = ~Year, 
    type = 'scatter',
    mode = 'markers',
    text = ~Country, 
    hoverinfo = "text",
    size = ~Marker.size
  ) %>% 
  layout(
    xaxis = list(
      title = "Oil",
      zeroline = F
    ),
    yaxis = list(
      title = "Coal",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    redraw = FALSE
  )
p1


##I thought France and Germany had quiet similar motion patterns, though the rates of change were not 
##similar but the motion pattern was similar
###Q2
fil_d = oil_data[oil_data$Country == "France"| oil_data$Country == "Germany",]
p2 <- fil_d %>%
  plot_ly(
    x = ~Oil, 
    y = ~Coal,
    color = ~Country,
    frame = ~Year, 
    type = 'scatter',
    mode = 'markers',
    text = ~Country, 
    hoverinfo = "text",
    size = ~Marker.size
  ) %>% 
  layout(
    xaxis = list(
      title = "Oil",
      zeroline = F
    ),
    yaxis = list(
      title = "Coal",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    redraw = FALSE
  )
p2





###Q3
oil_data$oil_p = (oil_data$Oil/(oil_data$Oil+oil_data$Coal))*100
temp = oil_data[,]
temp$oil_p = 0
new_oil_data = rbind(oil_data, temp)

p3 <- new_oil_data %>%
  plot_ly(
    x = ~oil_p, 
    y = ~Country,
    split = ~Country,
    frame = ~Year, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F, width = 20)
  ) %>% 
  layout(
    xaxis = list(
      title = "Oil_P",
      zeroline = F
    ),
    yaxis = list(
      title = "Country",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    redraw = FALSE
  )
p3



###Q4
p4 <- new_oil_data %>%
  plot_ly(
    x = ~oil_p, 
    y = ~Country,
    split = ~Country,
    frame = ~Year, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F, width = 20)
  ) %>% 
  layout(
    xaxis = list(
      title = "Oil_P",
      zeroline = F
    ),
    yaxis = list(
      title = "Country",
      zeroline = F
    )
  ) %>% 
  animation_opts(
    redraw = FALSE,
    easing = "elastic"
  )
p4




#Q5
mat <- read.csv2("Oilcoal.csv",sep=";")
mat<-mat[,1:3]
mat <- mat %>%spread(Country, Coal)
mat_scaled <- rescale(mat[,2:9])

rownames(mat_scaled) <- mat[,1]

set.seed(12345)
#tour <- new_tour(mat, grand_tour(), NULL)
tour<- new_tour(mat_scaled, guided_tour(cmass), NULL)

steps <- c(0, rep(1/15, 200))
Projs<-lapply(steps, function(step_size){ 
  step <- tour(step_size)
  if(is.null(step)) {
    .GlobalEnv$tour<- new_tour(mat_scaled, guided_tour(cmass), NULL)
    step <- tour(step_size)
  }
  step
}
)

# projection of each observation
tour_dat <- function(i) {
  step <- Projs[[i]]
  proj <- center(mat_scaled %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], state = rownames(mat_scaled))
}

# projection of each variable's axis
proj_dat <- function(i) {
  step <- Projs[[i]]
  data.frame(
    x = step$proj[,1], y = step$proj[,2], variable = colnames(mat_scaled)
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
  layout(xaxis = ax, yaxis = ax,title="Animated guided tour of Coal consumption per Country")
tour
