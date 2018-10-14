library(RColorBrewer)
library(wordcloud)
library(tm)

five<-read.table("Five.txt",header=F, sep='\n')
five <- data.frame(doc_id=row.names(five),
                       text=five$V1)
mycorpus1 <- Corpus(DataframeSource(five)) #Creating corpus (collection of text data)
mycorpus1 <- tm_map(mycorpus1, removePunctuation)
mycorpus1 <- tm_map(mycorpus1, function(x) removeWords(x, stopwords("english")))
tdm1 <- TermDocumentMatrix(mycorpus1) #Creating term-document matrix
m1 <- as.matrix(tdm1)

#here we merge all rows
v1 <- sort(rowSums(m1),decreasing=TRUE) #Sum up the frequencies of each word
d1 <- data.frame(word = names(v1),freq=v1) #Create one column=names, second=frequences
pal1 <- brewer.pal(8,"Dark2")
pal1 <- pal1[-(1:2)] #Create palette of colors
wordcloud(d1$word,d1$freq, scale=c(8,.3),min.freq=2,max.words=100, 
          random.order=F, rot.per=.15, colors=pal1, vfont=c("sans serif","plain"))


oneTwo<-read.table("OneTwo.txt",header=F, sep='\n')
oneTwo <- data.frame(doc_id=row.names(oneTwo),
                   text=oneTwo$V1)
mycorpus2 <- Corpus(DataframeSource(oneTwo)) #Creating corpus (collection of text data)
mycorpus2 <- tm_map(mycorpus2, removePunctuation)
mycorpus2 <- tm_map(mycorpus2, function(x) removeWords(x, stopwords("english")))
tdm2 <- TermDocumentMatrix(mycorpus2) #Creating term-document matrix
m2 <- as.matrix(tdm2)

#here we merge all rows
v2 <- sort(rowSums(m2),decreasing=TRUE) #Sum up the frequencies of each word
d2 <- data.frame(word = names(v2),freq=v2) #Create one column=names, second=frequences
pal2 <- brewer.pal(8,"Dark2")
pal2 <- pal2[-(1:2)]
wordcloud(d2$word,d2$freq, scale=c(8,.3),min.freq=2,max.words=100, 
          random.order=F, rot.per=.15, colors=pal2, vfont=c("sans serif","plain"))








library(plotly)
library(crosstalk)
library(tidyr)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(GGally)







x <- list(
  title = "Eicosenoic"
)
y <- list(
  title = "Linoleic"
)

olive<- read.csv("olive.csv")
d <- SharedData$new(olive)
scatterOlive <- plot_ly(d, x = ~eicosenoic, y = ~linoleic) %>%
  add_markers(color = I("blue")) %>% layout(xaxis = x, yaxis = y)
scatterOlive




##

olive$Region<-as.factor(olive$Region)
levels(olive$Region)<-c("North","South","Sardinia Island")
barOlive <-plot_ly(d, x=~Region)%>%add_histogram()%>%layout(barmode="overlay")

sb = subplot(scatterOlive,barOlive)%>%
  highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%hide_legend()

bscols(widths=c(3, NA),filter_slider("Slider", "Stearic", d, ~stearic)
       ,sb%>%highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%hide_legend())








scatterOlive2 <- plot_ly(d, x = ~arachidic, y = ~linolenic) %>%
  add_markers(color = I("black"))
subplot(scatterOlive,scatterOlive2)%>%
  highlight(on="plotly_select", dynamic=T, persistent=T, opacityDim = I(1))%>%hide_legend()





p<-GGally::ggparcoord(olive, columns = c(4:11))

dcro<-plotly_data(ggplotly(p))%>%group_by(.ID)
d1<-SharedData$new(dcro, ~.ID, group="olive")
p1<-plot_ly(d1, x=~variable, y=~value)%>%
  add_lines(line=list(width=0.3))%>%
  add_markers(marker=list(size=0.3),
              text=~.ID, hoverinfo="text")

olive2=olive
olive2$.ID=1:nrow(olive)
d2<-SharedData$new(olive2, ~.ID, group="olive")
p2<-plot_ly(d2, x=~factor(Region) )%>%add_histogram()%>%layout(barmode="stack")

ButtonsX=list()
for (i in 4:11){
  ButtonsX[[i-3]]= list(method = "restyle",
                        args = list( "x", list(olive[[i]])),
                        label = colnames(olive)[i])
}

ButtonsY=list()
for (i in 4:11){
  ButtonsY[[i-3]]= list(method = "restyle",
                        args = list( "y", list(olive[[i]])),
                        label = colnames(olive)[i])
}
ButtonsZ=list()
for (i in 4:11){
  ButtonsZ[[i-3]]= list(method = "restyle",
                        args = list( "z", list(olive[[i]])),
                        label = colnames(olive)[i])
}

olive3=olive
olive3$.ID=1:nrow(olive)
d3<-SharedData$new(olive3, ~.ID, group="olive")

p3<-plot_ly(d3,x=~ButtonsX,y=~ButtonsY,z=~ButtonsZ)%>%add_markers()%>% 
  layout(xaxis=list(title=ButtonsX), yaxis=list(title=ButtonsY),zaxis=list(title=ButtonsZ),
                                                                               title = "Select variable:",
                                                                               updatemenus = list(
                                                                                 list(y=0.9, buttons = ButtonsX),
                                                                                 list(y=0.6, buttons = ButtonsY),
                                                                                 list(y=0.3, buttons = ButtonsZ)
                                                                               )  )

ps<-htmltools::tagList(p1%>%
                         highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%
                         hide_legend(),
                       p2%>%
                         highlight(on="plotly_select", dynamic=T, persistent = T, opacityDim = I(1))%>%
                         hide_legend(),
                       p3%>%highlight(on="plotly_click", dynamic=T, persistent = T)%>%hide_legend()
) 


htmltools::browsable(ps)
