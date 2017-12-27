## Vasuprad.Kanade@accenture.com
## Course 10-Capstone Task 7
## Build Word Prediction
## This program store datagrams into data files to be used by Shiny App to reduce application load times

library(tm) # framework for text mining
library(SnowballC) # provides wordStem() for stemming
library(RColorBrewer) # generate palette of colours for plots
library(ggplot2) # plot word frequencies
library(scales) # format axis scales for plots
library(tidyr) # assists in cleaning & preparing data
library(dplyr) # assists in data manipulation, transformation, & summarization
library(RWeka)
library(stringr)
library(stringi)

setwd("~/Vasuprad/Accenture/Official/Trainings/Coursera/Data Science/Assignments/Course 10-Capstone/Project")

#############################################################################################
## import the news dataset in binary mode
#############################################################################################

conTwitter <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt", open="rb")
twitter1 <- readLines(conTwitter, encoding="UTF-8")
close(conTwitter)
rm(conTwitter)

# drop non UTF-8 characters
twitter1 <- iconv(twitter1, from = "latin1", to = "UTF-8", sub="")
twitter1 <- stri_replace_all_regex(twitter1, "\u2019|`","'")
twitter1 <- stri_replace_all_regex(twitter1, "\u201c|\u201d|u201f|``",'"')

conUSBlogs <- file("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt", open="rb")
blog1 <- readLines(conUSBlogs, encoding="UTF-8")
close(conUSBlogs)
rm(conUSBlogs)

# drop non UTF-8 characters
blog1 <- iconv(blog1, from = "latin1", to = "UTF-8", sub="")
blog1 <- stri_replace_all_regex(blog1, "\u2019|`","'")
blog1 <- stri_replace_all_regex(blog1, "\u201c|\u201d|u201f|``",'"')

conUSNews <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt", open="rb")
news1 <- readLines(conUSNews, encoding="UTF-8")
close(conUSNews)
rm(conUSNews)

# drop non UTF-8 characters
news1 <- iconv(news1, from = "latin1", to = "UTF-8", sub="")
news1 <- stri_replace_all_regex(news1, "\u2019|`","'")
news1 <- stri_replace_all_regex(news1, "\u201c|\u201d|u201f|``",'"')

# Sample data (10,000 of each)
sample_blogs   <- sample(blog1, 10000)
sample_news    <- sample(news1, 10000)
sample_twitter <- sample(twitter1, 10000)

# Save samples
save(sample_blogs, sample_news, sample_twitter, file= "./WordPrediction/data/sample/sampleData.RData")

rm(blog1)
rm(news1)
rm(twitter1)

load("./WordPrediction/data/sample/sampleData.RData")
sample.ls <- list(Blog = sample_blogs, News = sample_news, Twitter = sample_twitter)

#############################################################################################
## Pre-processing the Corpus
#############################################################################################

# Transform words to lower case
##sample.ls <- lapply(sample.ls, function(x) apply(x, 1, function(x) tolower(x)))
sample.ls <- lapply(sample.ls, tolower)

# Remove all punctuations
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[[:punct:]]", ""))

# Remove all non-alphanumeric characters
sample.ls <- lapply(sample.ls, function(x) str_replace_all(x, "[^[:alpha:]]", " "))

# Trim white space
sample.ls <- lapply(sample.ls, function(x) str_trim(x, side = "both"))

vecSource <- VectorSource(sample.ls)
sample <- VCorpus(vecSource,readerControl=list(reader=readPlain,language="english",encoding='ANSI'))

# Profanity filter
profanity <- readLines("./WordPrediction/data/bad_words.txt")

profanity <- profanity$V1
sample <- tm_map(sample, removeWords, profanity)

sample <- tm_map(sample, stripWhitespace)

rm(sample.ls)

#############################################################################################
# Create N-Grams
#############################################################################################

#### Bi-Gram
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions
sample <- tm_map(sample, PlainTextDocument)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(sample, control = list(removePunctuation = TRUE,stopwords = TRUE,tokenize = BigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

bi.gram <- df.tdm %>%
  mutate(Total = Blog + News + Twitter) %>%
  select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

#### Tri-Grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = TrigramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

tri.gram <- df.tdm %>%
  mutate(Total = Blog + News + Twitter) %>%
  select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

#### Quad-Grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuadgramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quad.gram <- df.tdm %>%
  mutate(Total = Blog + News + Twitter) %>%
  select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

#### Quin-Grams
options(mc.cores=1) # **IMPORTANT** must call this prior to using token functions

sample <- tm_map(sample, PlainTextDocument)

QuingramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
tdm <- TermDocumentMatrix(sample, control = list(tokenize = QuingramTokenizer))
tdm <- removeSparseTerms(tdm, 0.75)

# turn TermDocumentMatrix into a dataframe
matrix.tdm <- as.matrix(tdm)
df.tdm <- as.data.frame(matrix.tdm, row.names = FALSE)
df.tdm$token <- rownames(matrix.tdm)
names(df.tdm)[1:3] <- c("Blog", "News", "Twitter")
df.tdm <- select(df.tdm, token, Blog, News, Twitter)

quin.gram <- df.tdm %>%
  mutate(Total = Blog + News + Twitter) %>%
  select(token, Total)

rm(tdm, matrix.tdm, df.tdm)

#############################################################################################
## Analyze N-Grams to identify and remove low probability N-Grams
#############################################################################################
bi.gram <- bi.gram %>%
  arrange(desc(Total)) %>%
  mutate(Percent = Total/sum(Total)) %>%
  mutate(Cummulative = cumsum(Percent))

tri.gram <- tri.gram %>%
  arrange(desc(Total)) %>%
  mutate(Percent = Total/sum(Total)) %>%
  mutate(Cummulative = cumsum(Percent))

quad.gram <- quad.gram %>%
  arrange(desc(Total)) %>%
  mutate(Percent = Total/sum(Total)) %>%
  mutate(Cummulative = cumsum(Percent))

quin.gram <- quin.gram %>%
  arrange(desc(Total)) %>%
  mutate(Percent = Total/sum(Total)) %>%
  mutate(Cummulative = cumsum(Percent))


#############################################################################################
## Create Output Database to be used in the WordPrediction App
#############################################################################################

bi.gram$count <- 2
tri.gram$count <- 3
quad.gram$count <- 4
quin.gram$count <- 5

bi.gram <- bi.gram %>%
  mutate(key = word(token, 1), predict = word(token, -1)) %>%
  select(key, predict, total = Total, count)

tri.gram <- tri.gram %>%
  mutate(key = word(token, 1, 2), predict = word(token, -1)) %>%
  select(key, predict, total = Total, count)

quad.gram <- quad.gram %>%
  mutate(key = word(token, 1, 3), predict = word(token, -1)) %>%
  select(key, predict, total = Total, count)

quin.gram <- quin.gram %>%
  mutate(key = word(token, 1, 4), predict = word(token, -1)) %>%
  select(key, predict, total = Total, count)

freqDS.dff <- rbind(bi.gram, tri.gram, quad.gram, quin.gram) #has 4M unique tokens

test <- freqDS.dff %>% filter(total > 2)


freqDS.dff <- read.csv("./WordPrediction/data/sample/freqDS.dff.csv", stringsAsFactors=FALSE)

test <- freqDS.dff %>%
  group_by(count) %>%
  mutate(sum_total = sum(total)) %>%
  mutate(cum_sum = cumsum(total)) %>%
  mutate(cum_perc = cum_sum/sum_total) %>%
  filter(cum_perc < .75) %>%
  select(key, predict, total, count)

saveRDS(test, "./WordPrediction/data/frequencyDS.RDS")

write.csv(test, file = "./WordPrediction/data/sample/freqDS.dff.csv", row.names = FALSE)

uniGramData <- freqDS.dff %>% 
  group_by(predict) %>% 
  summarise(total = sum(total)) %>% 
  arrange(desc(total)) %>%
  mutate(probability = total/sum(total)) %>%
  filter(row_number() <= 100)

saveRDS(uniGramData, "./WordPrediction/data/uniGramData.RDS")
