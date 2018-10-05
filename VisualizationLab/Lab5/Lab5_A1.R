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

