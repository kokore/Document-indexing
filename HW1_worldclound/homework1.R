
library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)

#create coupus and this are plotstory from novel by stephen king
docs <- Corpus(DirSource("paper"))
#show example paper no.15
writeLines(as.character(docs[[15]]))

#pre-processing
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "\\-")
docs <- tm_map(docs, toSpace, "\\:")
docs <- tm_map(docs, toSpace, "\\'")
docs <- tm_map(docs, toSpace, "\\?")
docs <- tm_map(docs, toSpace, "\\.")
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)


#steaming data in corpus
docs <- tm_map(docs,stemDocument)


#create Term-Document matrix
dtm <- DocumentTermMatrix(docs)
dtm

#show all term in corpus
freq <- colSums(as.matrix(dtm))
length(freq)

#show most and least frequency term
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#show frequency term most 50
findFreqTerms(dtm,50)

#plot histogram frequency term most 50
wf=data.frame(term=names(freq),occurrences=freq)
p <- ggplot(subset(wf, freq>50), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#create wordcloud
set.seed(42)
wordcloud(names(freq),freq, min.freq=50)
