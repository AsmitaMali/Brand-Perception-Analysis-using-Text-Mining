#install.packages("bit64")
#install.packages("twitteR")
#install.packages("ROAuth")
#library("twitteR")
install.packages("rtweet")

library(ROAuth)
library(rtweet)
library(tidyverse)
library(tidytext)


#Set the working directory
setwd("D:/Personal/PG-BABI/web-social/group")
getwd()

#Topic Selection( Product, Brand, celebrity etc.) and scrapping of tweets using Twitter API(atleast 1000 tweets atleast)


#Topic Selection
#We choose "Zoom" because of its recent relevance in the lockdown situation. 
#Zoom was also a topic of interesting in the wake of Chinese App ban in India
#Zoom has its share of criticism for compromising with security.



api_key <- "OmdK90LrAmxRgJUqe3Jw2lbOq"
api_secret <- "scFt16snW9QgfO2sPN8yR1AInlEAxj02zR2YtSEozUQOZ6vI1r"
access_token <- "949315530938200064-4b1GIro58LU4nWPefmnIrgDh3wkmB0K"
access_token_secret <- "SntWz2t5rwpi4OOkniUN0A8CjE1OPDobrGx49adjdNAi9"

#token setting
create_token(
  app = "mytwitterapp",
  api_key,
  api_secret,
  access_token = "949315530938200064-4b1GIro58LU4nWPefmnIrgDh3wkmB0K",
  access_secret = "SntWz2t5rwpi4OOkniUN0A8CjE1OPDobrGx49adjdNAi9",
  set_renv = TRUE
)



#search for tweets in which mentions the brand "Zoom", excluding retweets to avoid Repetition  of text and filtering english lang 
zoom_tweets <- search_tweets(q="@zoom_us",n=3000,lang="en",include_rts = FALSE)

#converting to data frames
zoom_tweets_df <- zoom_tweets
dim(zoom_tweets_df)
View(zoom_tweets_df)
zoom_tweets_df <- apply(zoom_tweets_df,2,as.character)

#getting extract of tweets downloaded earlier
write.csv(zoom_tweets_df,file = paste("zoom.csv"))


#loading the tweets
tweets_raw = read.csv("zoom.csv", stringsAsFactors=FALSE)

#2) EDA of data

dim(tweets_raw)
view(tweets_raw)

#There are 3000 tweet records
#the tweet extracted had 91 columns givinig infomration such as text of tweet, userid, screen name, hashtags, retwees attributes
#Some interesting facts
# there is one such tweet which has been favourited 14268 times and the same on retweeted 2964 times
#2301 tweets have hashtag in them
#93% of  the tweet are from Twitter for Android , iPhone and webapp



# 3) Data Cleaning- removing stop words, user name, URLs, white spaces etc

#extracting the tweet text for data cleaning and further analysis 

tweets <- tweets_raw$text

library(tm)
library(NLP)



#converting text into document format
zoom_source <- VectorSource(tweets)

#converting it into a R object 
zoom_corpus <- VCorpus(zoom_source)

#viewing some tweets
zoom_corpus[[25]]
zoom_corpus[[25]][1]
zoom_corpus[[130]][1]


library(SnowballC)



#creating a function to clean corpus

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords,c(stopwords("en"),"zoom", "zoomus"))
  corpus <- tm_map(corpus, content_transformer(removeURL))
  return(corpus)
}

#creating a clean corpus
clean_corp <- clean_corpus(zoom_corpus)
clean_corp[["300"]][1]
clean_corp[["1500"]][1]

#termdocument and documentterm matrix format
zoom_tdm <- TermDocumentMatrix(clean_corp)
zoom_dtm <- DocumentTermMatrix(clean_corp)

#create as matrix
zoom_m <- as.matrix(zoom_tdm)
zoom_mc <- as.matrix(zoom_dtm)


#view few terms
zoom_mc[100:103, 200:205]
zoom_mc[104:113, 150:160]



# 4) Analyzing Text frequency

#calcuate frequency of terms
term_frequency <-rowSums(zoom_m)

#sort frequcny of terms
term_frequency <- sort(term_frequency, decreasing = TRUE)

#view top 10 freqeuncy words
term_frequency[1:10]

#plotting the frequncy
barplot(term_frequency[1:10], col="red", last =2)



# 5) word cloud( overall, positive, negative all 3 separately)

# Visualizing Corpus for Frequent Terms
library(wordcloud)
#overall word cloud, we have set minimum frequency as 30
wordcloud(clean_corp,colors=rainbow(7),max.words=50, min.freq=30)


# assesing sentiments of the words, converting tidy model
zoom_dtm_as_tidy <- tidy(zoom_dtm)

# Using bing lexicon: you can use other dictionary  as well(nrc/afinn)
#this will list positive and negative words form "bing" dictionary
bing <- get_sentiments("bing")

#checking with words with the our dtm of tweets
as_bing_words <- inner_join(zoom_dtm_as_tidy,bing,by = c("term"="word"))
# check positive and negative words 

#explain few terms from the output the freqeuncy
as_bing_words  

#selecting positive words
positive_words <- as.matrix(as_bing_words[as_bing_words$sentiment=="positive",2])
#positive word cloud
wordcloud(positive_words,colors=rainbow(7),max.words=50)


#selecting negative words
negative_words <- as.matrix(as_bing_words[as_bing_words$sentiment=="negative",2])
#negative word cloud
wordcloud(negative_words,colors=rainbow(7),max.words=50)

#6) sentiment analysis, polarity- positive or negative
#sentiment analysis of the tweets
library(SentimentAnalysis)

#analysisng each tweet to find out sentiment
sentiment <- analyzeSentiment(zoom_dtm)

#listing first five tweet and the sentiment score
sentiment[1:5,]

#the sentiment scores are in decimals let convert them to sentiment direction  (positive / negative)
#-1 mean negative seniment ,+1 positive sentiment
#We will QDAP dictionary

sentimentdirection <- convertToDirection(sentiment$SentimentQDAP)

# view sentiments - classified as neutral, positive and negative
sentimentdirection

#let understand summmary
table(sentimentdirection)

#negative  neutral positive 
#275     1208     1516 

plot(sentimentdirection)
#overall it seems to be postive as more than 50% is classified as positive

#this is also visible from summary  sentiment  scores and plot
summary(sentiment$SentimentQDAP)
plot(sentiment$SentimentQDAP)


#7) correlation chart of top keywords, including word association	5

#clustering on words
# we will remove words that are sparse and do a clustering - unsupervised learnign
# to find correlation of terms we will use hierarchical clustering
tdm2 <- removeSparseTerms(zoom_tdm, sparse = 0.975)
tdm_m <- as.matrix(tdm2)
tdm_df <- data.frame(tdm_m)

#euclidean distance is calcuated
tweets_dist <- dist(tdm_df)
#hierarchical clustering
hc <- hclust(tweets_dist)

#plotting dendrogram
plot(hc)


#let's find out few word assocation i.e., 
#words having close correlation used in similar context

#create association to find the words "help" is used with
#0.2 indicates the corelation limit
associations1 <-findAssocs(zoom_tdm, "help", 0.2)

#view assocations
associations1

#lets plot the association
#install.packages("qdapTools")
library(qdapTools)
library(ggplot2)
associations_df1 <- list_vect2df(associations1)[,2:3]
ggplot(associations_df1, aes(y=associations_df1[,1])) +geom_point(aes(x=associations_df1[,2]),data=associations_df1, size=3)

#We can see that volunteers and psychologically  seems to behigly related to "help"


#one more word association for "video"
associations2 <-findAssocs(zoom_tdm, "video", 0.2)
associations_df2 <- list_vect2df(associations2)[,2:3]
ggplot(associations_df2, aes(y=associations_df2[,1])) +geom_point(aes(x=associations_df2[,2]),data=associations_df2, size=3)
#video seem to be used most with conferencing, mgt,zoomvideoconferencing - which makes logical sense


