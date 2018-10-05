  ## interaction.r

library(plotly)
library(crosstalk)
library(tidyr)

crabs<- read.csv("australian-crabs.csv")

d <- SharedData$new(crabs)

#LINKING EXAMPLES

#Bar-scatter
scatterCrab <- plot_ly(d, x = ~CL, y = ~RW) %>%
  add_markers(color = I("black"))


barCrab <-plot_ly(d, x=~sex)%>%add_histogram()%>%layout(barmode="overlay")

subplot(scatterCrab,barCrab)%>%
  highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%hide_legend()


#Scatter-scatter

scatterCrab2 <- plot_ly(d, x = ~CL, y = ~BD) %>%
  add_markers(color = I("black"))
subplot(scatterCrab,scatterCrab2)%>%
  highlight(on="plotly_select", dynamic=T, persistent=T, opacityDim = I(1))%>%hide_legend()

#Parallel coords

library(GGally)
p<-ggparcoord(crabs, columns = c(4:6))

d<-plotly_data(ggplotly(p))%>%group_by(.ID)
d1<-SharedData$new(d, ~.ID, group="crab")
p1<-plot_ly(d1, x=~variable, y=~value)%>%
  add_lines(line=list(width=0.3))%>%
  add_markers(marker=list(size=0.3),
              text=~.ID, hoverinfo="text")


crabs2=crabs
crabs2$.ID=1:nrow(crabs)
d2<-SharedData$new(crabs2, ~.ID, group="crab")
p2<-plot_ly(d2, x=~factor(sex) )%>%add_histogram()%>%layout(barmode="stack")

ps<-htmltools::tagList(p1%>%
                         highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%
                         hide_legend(),
                       p2%>%
                         highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%
                         hide_legend()
)
htmltools::browsable(ps)  

#3D-plot and parcoord
df=read.csv("flea.csv")
d2<-SharedData$new(df)

p<-ggparcoord(flea, columns = c(6,7,2))

d<-plotly_data(ggplotly(p))%>%group_by(.ID)
d1<-SharedData$new(d, ~.ID, group="flea")
p1<-plot_ly(d1, x=~variable, y=~value)%>%
  add_lines(line=list(width=0.3))%>%
  add_markers(marker=list(size=0.3),
              text=~.ID, hoverinfo="text")

flea2=flea[, c("tars1", "aede2", "aede3")]
flea2$.ID=1:nrow(flea)
d2<-SharedData$new(flea2, ~.ID, group="flea")

p3<-plot_ly(d2,x=~tars1,y=~aede2,z=~aede3)%>%add_markers()
bscols(p1%>%highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%
         hide_legend(),
       p3%>%highlight(on="plotly_click", dynamic=T, persistent = T)%>%hide_legend())

#Filter - check crosstalk package

d <- SharedData$new(crabs)
scatterCrab <- plot_ly(d, x = ~CL, y = ~RW) %>%
  add_markers(color = I("black"))


barCrab <-plot_ly(d, x=~sex)%>%add_histogram()%>%layout(barmode="overlay")

bscols(widths=c(2, NA),filter_slider("FL", "Frontal Lobe", d, ~FL)
       ,subplot(scatterCrab,barCrab)%>%
         highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%hide_legend())


## Variable selection

ButtonsX=list()
for (i in 4:7){
  ButtonsX[[i-3]]= list(method = "restyle",
                        args = list( "x", list(crabs[[i]])),
                        label = colnames(crabs)[i])
}

ButtonsY=list()
for (i in 4:7){
  ButtonsY[[i-3]]= list(method = "restyle",
                        args = list( "y", list(crabs[[i]])),
                        label = colnames(crabs)[i])
}

p <- plot_ly(d, x = ~CL, y = ~CW, alpha = 0.8) %>%
  add_markers() %>%
  layout(xaxis=list(title=""), yaxis=list(title=""),
         title = "Select variable:",
         updatemenus = list(
           list(y=0.9, buttons = ButtonsX),
           list(y=0.6, buttons = ButtonsY)
         )  )

p    






## interaction with shiny

#Borrowed from PLOTLYs website

library(plotly)
library(shiny)

# compute a correlation matrix
correlation <- round(cor(mtcars), 3)
nms <- names(mtcars)

ui <- fluidPage(
  mainPanel(
    plotlyOutput("heat"),
    plotlyOutput("scatterplot")
  ),
  verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  output$heat <- renderPlotly({
    plot_ly(x = nms, y = nms, z = correlation, 
            key = correlation, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(mtcars[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        add_lines(y = ~yhat) %>%
        layout(xaxis = list(title = s[["x"]]), 
               yaxis = list(title = s[["y"]]), 
               showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })
  
}

shinyApp(ui, server)


## steam graph

#devtools::install_github("hrbrmstr/streamgraph")

library(dplyr)
library(ggplot2movies)
library(streamgraph)

ggplot2movies::movies %>%
  select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
  tidyr::gather(genre, value, -year) %>%
  group_by(year, genre) %>%
  tally(wt=value) %>%
  ungroup %>%
  streamgraph("genre", "n", "year") %>%
  sg_axis_x(20) %>%
  sg_fill_brewer("PuOr") %>%
  sg_legend(show=TRUE, label="Genres: ")





## Wordcloud

data<-read.table("examples/romeo.txt",header=F, sep='\n') #Read file
library(tm)
library(wordcloud)
library(RColorBrewer)
data$doc_id=1:nrow(data)
colnames(data)[1]<-"text"

#Here we interpret each line in the document as separate document
mycorpus <- Corpus(DataframeSource(data)) #Creating corpus (collection of text data)
mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, function(x) removeWords(x, stopwords("english")))
tdm <- TermDocumentMatrix(mycorpus) #Creating term-document matrix
m <- as.matrix(tdm)

#here we merge all rows
v <- sort(rowSums(m),decreasing=TRUE) #Sum up the frequencies of each word
d <- data.frame(word = names(v),freq=v) #Create one column=names, second=frequences
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1:2)] #Create palette of colors
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=F, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
