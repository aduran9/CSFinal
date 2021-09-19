# title: 	"Capstone Final Project: Shiny Server"
# author: 	"aduran9"
# date: 	"15/9/2021"
# Back Off Algorithm
#
# Predict the next word of the user input sentence
# 1. For prediction of the NextWord, Quadgram is first used (first three words of Quadgram are the last three words of the user provided sentence).
# 2. If no Quadgram is found, back off to Trigram (first two words of Trigram are the last two words of the sentence).
# 3. If no Trigram is found, back off to Bigram (first word of Bigram is the last word of the sentence)
# 4. If no Bigram is found, back off to the most common word with highest frequency 'the' is returned.
#

suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# Load Quadgram,Trigram & Bigram Data frame files
quadgram <- readRDS("./ngrams/quadgram.RData");
trigram <- readRDS("./ngrams/trigram.RData");
bigram <- readRDS("./ngrams/bigram.RData");
mesg <<- ""

# Cleaning of user input before predicting the next word
Predict <- function(x) {
    xclean <- removeNumbers(removePunctuation(tolower(x)))
    xs <- strsplit(xclean, " ")[[1]]
    if (length(xs)>= 3) {
        xs <- tail(xs,3)
        if (identical(character(0),head(quadgram[quadgram$unigram == xs[1] & quadgram$bigram == xs[2] & quadgram$trigram == xs[3], 4],1))){
            Predict(paste(xs[2],xs[3],sep=" "))
        }
        else {mesg <<- "NextWord predicted using 4-gram."; head(quadgram[quadgram$unigram == xs[1] & quadgram$bigram == xs[2] & quadgram$trigram == xs[3], 4],1)}
    }
    else if (length(xs) == 2){
        xs <- tail(xs,2)
        if (identical(character(0),head(trigram[trigram$unigram == xs[1] & trigram$bigram == xs[2], 3],1))) {
            Predict(xs[2])
        }
        else {mesg<<- "NextWord predicted using 3-gram."; head(trigram[trigram$unigram == xs[1] & trigram$bigram == xs[2], 3],1)}
    }
    else if (length(xs) == 1){
        xs <- tail(xs,1)
        if (identical(character(0),head(bigram[bigram$unigram == xs[1], 2],1))) {mesg<<-"No match found. Most common word 'the' is returned."; head("the",1)}
        else {mesg <<- "NextWord predicted using 2-gram."; head(bigram[bigram$unigram == xs[1],2],1)}
    }
}


shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- Predict(input$inputString)
        output$text2 <- renderText({mesg})
        result
    });
    
    output$text1 <- renderText({
        input$inputString});
}
)