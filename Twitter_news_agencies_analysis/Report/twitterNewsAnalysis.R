##
#     Author :  Joydeep
#
#
##

setwd("C:/Users/joydeep/Desktop/DA/Twitter")

library(twitteR)
library(tm)
api_key <- "c5PmYlnCAAKFL1Hmjr1rsA2ee"

api_secret <- "3EWsRwTVEjZESVyPfdlsDO5dZt5opZjrIfXF3u5r24QLqiOySM"

access_token <- "392795679-rullwLdaQTb2Pu58DTRQ8opVnulnWTzAyeAuWTjO"

access_token_secret <- "xsC7ehC1bvdWxPNiWwYnvsb3BUbhyRgrbAaM2JV6EB8xU"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


rdmTweets <- userTimeline("@BBCIndia", n=1000)
n <- length(rdmTweets)
rdmTweets[1:3]


# The tweets are first converted to a data frame and then to a corpus.
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

#====================================================manual processing=============================
write.csv(df,"excelProcessing.csv")
df_processed <- read.csv("excelProcessing.csv")

dim(df)
library(tm)
# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(df_processed$text))


myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)

# remove stopwords
myStopwords <- c(stopwords('english'), "available", "via")



# idx <- which(myStopwords == "r")
# myStopwords <- myStopwords[-idx]

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# In many cases, words need to be stemmed to retrieve their radicals. 
# For instance, "example" and "examples" are both stemmed to "exampl". 
# However, after that, one may want to complete the stems to their original forms, 
# so that the words would look "normal".

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
inspect(myCorpus[1:3])

myCorpus <- tm_map(myCorpus, PlainTextDocument)

# erron in next line of code 
# stem completion
# myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

inspect(myDtm[266:270,31:40])


findFreqTerms(myDtm, lowfreq=10)

library(wordcloud)
m <- as.matrix(myDtm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="india")
myNames[k] <- "india"

d <- data.frame(word=myNames, freq=v)
d <- d[1:30,]
wordcloud(d$word, d$freq, min.freq=5)


termDocMatrix <- as.matrix(myDtm)
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1

termMatrix <- termDocMatrix %*% t(termDocMatrix)
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]


library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")

# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)




