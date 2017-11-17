#
# This script is used to explore the data provided as part of the Johns Hopkins
# Data Science Curriculum Capstone Project. The project goal is to develop
# a "next word predictor" that would be useful for smart phone keyboard users.
# 
# There are three different types of data provided in each of several different
# languages. For the purposes of this project, we will focus on the English 
# language files. The three different types of data are: tweets, blog entries,
# and news articles. 
# 
# The basic idea is to read in the data, clean it up, "tokenize" it, and then
# use that data to calculate the probability of any particular word being the next 
# one given a preceding sequence of words. 
# 
# The "quanteda" and "tm" packages can be used for most of what needs to be done.
#  
# Some things to be learned from the exploratory analysis:
# - Should we develop different models for the different types of corpora?
#   My intuition is that the vocabulary and phrase patterns are likely to be quite
#   different between tweets, blog entries, and news items, and that prediction
#   accuracy would be greatly improved by modeling each independently. Part of
#   the exploratory analysis is to compare these aspects of the data to see if
#   indeed they are dissimilar in these respects.
#   
# - It seems that identifying the beginning of a sentence is also very important.
#   For example, if we have the following text: 
#        "...it was really fun. I would like..." 
#   then the n-grams "fun", "really fun", "was really fun", and "it was really fun"
#   are unlikely to be very good predictors for the next word ("I"), for the simple
#   reason that they are only very loosely related - there is no grammatical
#   connection between them. We should look at the possibility of eliminating 
#   n-grams that span sentence boundaries, and identifying the words that are most
#   frequently used at the beginning of a sentence (and this is possibly very different
#   for tweets, blogs, and news items).
#   
# - Should we remove symbols in the cleaning process? For example, hashtags 
#   (character strings prefixed with the "#" symbol) are used extensively 
#   in tweets, to a moderate degree in blog entries, and rarely in news stories. 
#   Is a hashtag likely to be a good predictor of the next word in a tweet?
#   

install.packages("tm")
install.packages("quanteda")
library(tm)
library(quanteda)


setwd("C:/Users/Dave/Documents/Data Science/Coursera/Data Science Specialization Capstone Project/Coursera-SwiftKey")

createSample <- function (filepath, 
                          samplepct = 0.01, 
                          stats = TRUE) {
    # 
    # Read the file specified by filepath and write out a subset based on
    # a random sample representing "samplepct" of the original lines.
    # 
    funcname <- "createSample"
    
    lastperiod <- regexpr("\\.[^\\.]*$",filepath)[[1]]   #Location of last period in pathname
    samplefile <- paste0(substr(filepath,1,lastperiod-1),
                         "_sample",
                         substr(filepath,lastperiod,nchar(filepath))
    )
    inputcon <- file(filepath, "r") 
    
    if (stats) print(paste(funcname,"Sample file name:",samplefile))
    
    fullfile <- readLines(inputcon, warn = FALSE)
    nooflines <- length(fullfile)
    maxline <- max(nchar(fullfile))
    if (stats) {print(paste(funcname,"Number of lines in original file:",nooflines))
        print(paste(funcname,"Maximum line length:",maxline)) }
    close(inputcon)
    
    # Generate a random sample consisting of samplesize % of the lines in the file
    set.seed(0)
    sample <- rbinom(nooflines, 1, samplepct)
    
    if (stats) print(paste(funcname,"Number of lines in sample file:", sum(sample)))
    
    inputcon <- file(filepath, "r") 
    samplecon <- file(samplefile,"w")
    
    for (i in 1:nooflines) {
        if (sample[i]) writeLines(readLines(inputcon,1, warn = FALSE),samplecon)
    }
    close(inputcon) 
    close(samplecon)
    
    return(samplefile)
}





inputcon <- file("en_US.twitter.txt", "r") 
outputcon <- file("en_us.twittersample.txt","w")

profanityFilter <- "fuck|shit|cunt|bitch|bastard|asshole|nigger|^cum$"


createTokens <- function (filepath, 
                          samplesize = 0.10, 
                          stats = TRUE, 
                          profanityFilter = NA, 
                          remove_numbers = FALSE, 
                          remove_punct = FALSE) {
    inputcon <- file(filepath, "r") 
    samplecon <- file(paste0(substr(filepath,1,regexpr("\\.",filepath)[[1]]-1),"_sample.csv"),"w")
    
    # Figure out how many lines of text there are in the file
    readsizeof <- 20000
    nooflines <- 0
    ( while((linesread <- length(readLines(inputcon,readsizeof))) > 0 ) 
        nooflines <- nooflines+linesread )
    close(inputcon)
    if (stats) print(paste("Number of lines in file:",nooflines))
    
    # Generate a random sample consisting of 10% of the lines in the file
    set.seed(0)
    sample <- rbinom(nooflines, 1, 0.10)
    
    for (i in 1:nooflines) {
        if (sample[i]) writeLines(readLines(inputcon,1),samplecon)
    }
    close(inputcon) 
    close(samplecon)
    
    sample <- readLines(samplecon)
    close(samplecon)
    
    # Tokenize the file
    toksamp <- unlist(tokens(sample))
    if (stats) head(toksamp, n=50)
    
    # Remove profanities (at least what I consider profane...)
    
    if (is.na(profanityFilter)) profanityFilter <- "fuck|shit|cunt|bitch|bastard|asshole|nigger|^cum$"
    
    profanewords <- grepl(profanityFilter, sample, ignore.case = TRUE)

    cleantok <- toksamp[!profanewords]
    if (stats) {
        head(cleantok, n=50)
        str(cleantok)}
    return(toksamp)
}
# Create a sample of the twitter data for further examination
inputcon <- file("en_US.twitter.txt", "r") 
outputcon <- file("en_us.twittersample.txt","w")

# Figure out how many lines of text there are in the file
readsizeof <- 20000
nooflines <- 0
( while((linesread <- length(readLines(inputcon,readsizeof))) > 0 ) 
    nooflines <- nooflines+linesread )
close(inputcon)
nooflines

# Generate a random sample consisting of 10% of the lines in the file
set.seed(0)
sample <- rbinom(nooflines, 1, 0.10)

inputcon <- file("en_US.twitter.txt", "r") 
for (i in 1:nooflines) {
    if (sample[i]) writeLines(readLines(inputcon,1),outputcon)
}
close(inputcon) 
close(outputcon)

samplecon <- file("en_us.twittersample.txt","r")
twittersample <- readLines(samplecon)
close(samplecon)
# Let's tokenize the sample

library(tm)
library(quanteda)

# Since I don't know what I am doing yet, I am trying two different tokenizers
toksamp <- MC_tokenizer(twittersample)
head(toksamp, n=50)

toksamp2 <- unlist(tokens(twittersample))
head(toksamp2, n=50)
# Remove profanities (at least what I consider profane...)

profanityFilter <- "fuck|shit|cunt|bitch|bastard|asshole|nigger|^cum$"

profanewords <- grepl(profanityFilter, toksamp, ignore.case = TRUE)
profanewords2 <- grepl(profanityFilter, toksamp2, ignore.case = TRUE)

cleantok <- toksamp[!profanewords]
head(cleantok, n=50)
str(cleantok)

cleantok2 <- toksamp2[!profanewords2]
head(cleantok2, n=50)
str(cleantok2)

# code to answer a question in quiz 1
inputcon <- file("en_US.twitter.txt", "r") 
fullfile <- readLines(inputcon)
close(inputcon)

sum(grepl(" love ", fullfile))
sum(grepl(" hate ", fullfile))

sum(grepl(" love ", fullfile))/sum(grepl(" hate ", fullfile))
