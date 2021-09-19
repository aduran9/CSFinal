# title: 	"Capstone Final Project: Process DataSample"
# author: 	"aduran9"
# date: 	"15/9/2021"

# Preload required R libraries
library(R.utils)
library(parallel)
library(wordcloud)

setwd("~/R/Capstone/CSFinal_Shiny")
destfile <- "./Coursera-SwiftKey.zip"

# Download and unzip the data
if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile)
    unzip("Coursera-SwiftKey.zip")
}
blog <- readLines("./final/en_US/en_US.blogs.txt", warn = FALSE, encoding = "UTF-8")
news <- readLines("./final/en_US/en_US.news.txt", warn = FALSE, encoding = "UTF-8")
twit <- readLines("./final/en_US/en_US.twitter.txt", warn = FALSE, encoding = "UTF-8")

# Sample, Cleaning and Prepare DataSample
library(NLP)			# Load libraries
library(tm)
set.seed(2021)			# Set seed for reproducability
sampleSize <- 0.01		# Sample 1% of Datasets

# Create data sample
dataSample <- c(sample(blog, length(blog) * sampleSize),
                sample(news, length(news) * sampleSize),
                sample(twit, length(twit) * sampleSize))

# remove variables no longer needed to free up memory
#rm(blog, news, twit)

# Create corpus from the joined subsets and cleaning
corpus <- VCorpus(VectorSource(dataSample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

# Remove offensive words (https://www.cs.cmu.edu/~biglou/resources/bad-words.txt)
profanities <- read.csv("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",header =FALSE, strip.white = TRUE, stringsAsFactors = FALSE)
corpus <- tm_map(corpus, removeWords, profanities$V1)

# remove variables no longer needed to free up memory
#rm(dataSample, toSpace, profanities, destfile, sampleSize)

# Use Tokenize, create and remove sparse terms for *Uni, Duo, Tri and Qua*grams.
library(RWeka) 			# Weka is a collection of machine learning algorithms for data mining
library(dplyr)
library(ggplot2)

# Define function to make N grams
tdm_Ngram <- function (textcp, n) {
    NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
    tdm_ngram <- TermDocumentMatrix(textcp, control = list(tokenizer = NgramTokenizer))
    tdm_ngram
}
# Calculate N-Grams
tdm_1gram <- tdm_Ngram(corpus, 1)
tdm_2gram <- tdm_Ngram(corpus, 2)
tdm_3gram <- tdm_Ngram(corpus, 3)
tdm_4gram <- tdm_Ngram(corpus, 4)

# Define function to extract the N grams and sort
ngram_sorted_df <- function (tdm_ngram) {
    tdm_ngram_m <- as.matrix(tdm_ngram)
    tdm_ngram_df <- as.data.frame(tdm_ngram_m)
    colnames(tdm_ngram_df) <- "Count"
    tdm_ngram_df <- tdm_ngram_df[order(-tdm_ngram_df$Count), , drop = FALSE]
    tdm_ngram_df
}
# Extract term-count tables from N-Grams and sort 
memory.limit(size=56000)
tdm_1gram_df <- ngram_sorted_df(tdm_1gram)
tdm_2gram_df <- ngram_sorted_df(tdm_2gram)
tdm_3gram_df <- ngram_sorted_df(tdm_3gram)
tdm_4gram_df <- ngram_sorted_df(tdm_4gram)

# remove variables no longer needed to free up memory
#rm(corpus, tdm_1gram, tdm_2gram, tdm_3gram, tdm_4gram)

# Save data frames into r-compressed files
if (!file.exists("ngrams")) {
    dir.create("ngrams")
}

quadgram <- data.frame(rows=rownames(tdm_4gram_df), count=tdm_4gram_df$Count)
quadgram$rows <- as.character(quadgram$rows)
quadgram_split <- strsplit(as.character(quadgram$rows), split=" ")
quadgram <- transform(quadgram, first=sapply(quadgram_split, "[[", 1), second=sapply(quadgram_split, "[[", 2), third=sapply(quadgram_split, "[[", 3), fourth=sapply(quadgram_split, "[[", 4))
quadgram <- data.frame(unigram=quadgram$first, bigram=quadgram$second, trigram=quadgram$third, quadgram=quadgram$fourth, freq=quadgram$count, stringsAsFactors=FALSE)
write.csv(quadgram[quadgram$freq > 1, ], "./ngrams/quadgram.csv", row.names=F)
quadgram <- read.csv("./ngrams/quadgram.csv", stringsAsFactors=F)
saveRDS(quadgram, "./ngrams/quadgram.RData")

trigram <- data.frame(rows=rownames(tdm_3gram_df), count=tdm_3gram_df$Count)
trigram$rows <- as.character(trigram$rows)
trigram_split <- strsplit(as.character(trigram$rows), split=" ")
trigram <- transform(trigram, first=sapply(trigram_split, "[[", 1), second=sapply(trigram_split, "[[", 2), third=sapply(trigram_split, "[[", 3))
trigram <- data.frame(unigram=trigram$first, bigram=trigram$second, trigram=trigram$third, freq=trigram$count, stringsAsFactors=FALSE)
write.csv(trigram[trigram$freq > 1, ], "./ngrams/trigram.csv", row.names=F)
trigram <- read.csv("./ngrams/trigram.csv", stringsAsFactors=F)
saveRDS(trigram, "./ngrams/trigram.RData")

bigram <- data.frame(rows=rownames(tdm_2gram_df), count=tdm_2gram_df$Count)
bigram$rows <- as.character(bigram$rows)
bigram_split <- strsplit(as.character(bigram$rows), split=" ")
bigram <- transform(bigram, first=sapply(bigram_split, "[[", 1), second=sapply(bigram_split, "[[", 2))
bigram <- data.frame(unigram=bigram$first, bigram=bigram$second, freq=bigram$count, stringsAsFactors=FALSE)
write.csv(bigram[bigram$freq > 1, ], "./ngrams/bigram.csv", row.names=F)
bigram <- read.csv("./ngrams/bigram.csv", stringsAsFactors=F)
saveRDS(bigram, "./ngrams/bigram.RData")