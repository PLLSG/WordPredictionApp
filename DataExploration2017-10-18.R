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
library(ngram)
library(dplyr)
library(stringr)
library(quanteda)

setwd("C:/Users/Dave/Documents/Data Science/Coursera/Data Science Specialization Capstone Project/Coursera-SwiftKey")

# Read in each of the three corpora files and gather some basic statistics about them:
# 1. Number of lines, sentences and words in each
# 2. Word frequencies
# 

twitter <- "en_US.twitter.txt"
blog <- "en_US.blogs.txt"
news <- "en_US.news.txt"

# Need some basic functions to produce the needed information

#
# CreateSampleFile takes a subset of data from an input file to create a sample file for further exploration
# 
createSampleFile <- function (filepath,
                              sampleType = c("random","segment"),
                              startpct = 0,
                              endpct = 0.10,
                              samplepct = 0.10,
                              seed = 0,
                              suffix = "train",
                              stats = TRUE) {
    
    funcname <- "createSampleFile"
    
    lastperiod <- regexpr("\\.[^\\.]*$",filepath)[[1]]   #Location of last period in pathname
    samplefile <- paste0(substr(filepath,1,lastperiod-1),
                         "_",
                         suffix,
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
    set.seed(seed)
    sampleSeq <- seq(1:nooflines)
    
    if (sampleType == "random") sample <- rbinom(nooflines, 1, samplepct)
    else sample <- (sampleSeq >= startpct*nooflines & sampleSeq < endpct*nooflines)
    
    if (stats) print(paste(funcname,"Number of lines in sample file:", sum(sample)))
    
    samplecon <- file(samplefile,"w")
    
    for (i in 1:nooflines) {
        if (sample[i]) writeLines(fullfile[i],samplecon)
    }
    close(samplecon)
    
    return(list(pathname = samplefile, textdata = fullfile[sample]))
}

GetSentences <- function   (filepath = NA, 
                            textdata = NA,
                            stats = TRUE, 
                            remove_numbers = TRUE, 
                            remove_punct = TRUE,
                            remove_symbols = TRUE) {
    
    funcname <- "GetSentences"
    if (!is.na(filepath)) {
        inputcon <- file(filepath, "r") 
        if (stats) print(paste(funcname, "Reading input file:",filepath))
        textdata <- readLines(inputcon, warn = FALSE)
        close(inputcon)
    }
    
    if (length(textdata)==1 & is.na(textdata[1])) { 
        print("Error: A value must be given for either filepath or text data")
        return()
    }
    
    if (stats) {
        print(paste(funcname, "Number of lines in corpus:",length(textdata)))
        print(paste(funcname,
                    "remove_numbers =", remove_numbers,
                    "remove_punct =", remove_punct,
                    "remove_symbols =", remove_symbols))
    }
    
    # Break the data up into sentences
    textdata <- unlist(tokens(textdata, 
                              what = "sentence",
                              remove_numbers = remove_numbers,
                              remove_punct = remove_punct,
                              remove_symbols = remove_symbols))
    
    if (stats) print(paste(funcname, "Returning",length(textdata),"sentences"))
    return(textdata)
}

createNgrams <- function (filepath = NA, 
                          textdata = NA,
                          stats = TRUE, 
                          sentenceBreaks = TRUE,
                          ngrams = 1,
                          remove_numbers = TRUE, 
                          remove_punct = TRUE,
                          remove_symbols = TRUE) {
    
    funcname <- "createNgrams"
    if (!is.na(filepath)) {
        inputcon <- file(filepath, "r") 
        if (stats) print(paste(funcname, "Reading input file:",filepath))
        textdata <- readLines(inputcon, warn = FALSE)
        close(inputcon)
    }
    
    if (length(textdata)==1 & is.na(textdata[1])) { 
        print("Error: A value must be given for either filepath or text data")
        return()
    }
    
    if (stats) {
        print(paste(funcname, "Number of lines in corpus:",length(textdata)))
        print(paste(funcname,
                    "sentenceBreaks =", sentenceBreaks,
                    "ngrams =", ngrams,
                    "remove_numbers =", remove_numbers,
                    "remove_punct =", remove_punct,
                    "remove_symbols =", remove_symbols))
    }
    
    # Tokenize the file
    if (sentenceBreaks) 
        # The next statement is relying on the fact that R 
        # is a pass-by-value language!
        textdata <- unlist(tokens(textdata, what = "sentence"))
    tokendata <- (tokens(textdata,
                         what = "word",
                         ngrams = ngrams,
                         concatenator = " ",
                         remove_numbers = remove_numbers,
                         remove_punct = remove_punct,
                         remove_symbols = remove_symbols))
    
    if (stats) print(paste(funcname, "Returning",sum(ntoken(tokendata)),"n-grams"))
    return(unlist(tokendata))
}

ngramFrequencies <- function (tokendata, cums = TRUE) {
    # This function takes the input "tokendata", which is a character
    # vector of n-grams, and returns a list containing a data frame in which each row lists: 
    #  - a unique n-gram, 
    #  - the "n" value of the n-gram,
    #  - the number of times it occurred in tokendata (column Freq), 
    #  - the prefix of the n-gram (all but the last word),
    #  - the last word of the n-gram, and optionally (based on value of "cums"), 
    #  - the cumulative occurrences, and
    #  - the cumulative percent of each n-gram. 
    # 
    # The n-grams are first converted to all lower case; we are not interested in
    # distinguishing between cases where the only difference is in upper or lower
    # case variants.
    # 
    # If the logical variable cums is FALSE, then no cumulative values are returned,
    # and this function will run considerably faster. Cumulative values are useful
    # during exploratory analysis but not necessarily needed when building the model.
    # 
    # The data is sorted in descending order of the value Freq, so the first
    # n-gram listed is the one that occurs the most frequently in the input
    # data.
    # 
    tdf <- data.frame(ngram = tolower(tokendata))
    ngfreq <- data.frame(table(tdf$ngram, dnn = c("ngram"))) %>% arrange(desc(Freq))
    ngfreq$n <- str_count(ngfreq$ngram," ") + 1
    ngfreq$prefix <- substr(ngfreq$ngram,1,regexpr(" [^ ]*$",ngfreq$ngram)-1)
    nglastword <- substr(ngfreq$ngram,regexpr(" [^ ]*$",ngfreq$ngram)+1,999)
    ngfreq <- cbind(ngfreq, lw = nglastword, stringsAsFactors = FALSE)
    ngtot <- sum(ngfreq$Freq) # Get the total number of n-gram occurrences
    ngfreq$FreqPct <- ngfreq$Freq/ngtot # Calculate % of time this n-gram occurs
    if (cums) {
        cumfreq <- ngfreq$Freq[1]
        for (i in 2:length(ngfreq$Freq)) { cumfreq <- c(cumfreq, cumfreq[i-1]+ngfreq$Freq[i])}
        ngfreq <- cbind(ngfreq, cum = cumfreq)
        ngfreq <- cbind(ngfreq, cumpct = ngfreq$cum/ngtot)
    }
    return(ngfreq)
}

print(Sys.time())
twitterCorpora <- iconv(readLines(twitter, warn = FALSE), "latin1","ASCII", sub = "")
print(Sys.time())
twitterSentences <- GetSentences(textdata = twitterCorpora)
print(Sys.time())
twitterWords <- createNgrams(textdata = twitterSentences, sentenceBreaks = FALSE)
print(Sys.time())
twitterWordFreq <- ngramFrequencies(twitterWords)
print(Sys.time())

write.table(twitterWordFreq,"TwitterWordFreq")

blogCorpora <- iconv(readLines(blog, warn = FALSE), "latin1","ASCII", sub = "")
print(Sys.time())
blogSentences <- GetSentences(textdata = blogCorpora)
print(Sys.time())
blogWords <- createNgrams(textdata = blogSentences, sentenceBreaks = FALSE)
print(Sys.time())
blogWordFreq <- ngramFrequencies(blogWords)
print(Sys.time())

write.table(blogWordFreq,"BlogWordFreq")

newsCorpora <- iconv(readLines(news, warn = FALSE), "latin1","ASCII", sub = "")
print(Sys.time())
newsSentences <- GetSentences(textdata = newsCorpora)
print(Sys.time())
newsWords <- createNgrams(textdata = newsSentences, sentenceBreaks = FALSE)
print(Sys.time())
newsWordFreq <- ngramFrequencies(newsWords)
print(Sys.time())

write.table(newsWordFreq,"NewsWordFreq")

totalWordFreq <- ngramFrequencies(c(twitterWords, blogWords, newsWords))
print(Sys.time())

write.table(totalWordFreq,"TotalWordFreq")

twitterSummary <- data.frame(File = twitter,
                          SizeMB = as.numeric(object.size(twitterCorpora))/(1024*1024),
                          Lines = length(twitterCorpora),
                          Sentences = length(twitterSentences),
                          Words = length(twitterWords),
                          UniqueWords = nrow(twitterWordFreq),
                          WordsNintiethPctile = nrow(twitterWordFreq[twitterWordFreq$cumpct<0.90,]),
                          SingleOccurrenceWords = nrow(twitterWordFreq[twitterWordFreq$Freq==1,]))
blogSummary <- data.frame(File = blog,
                    SizeMB = as.numeric(object.size(blogCorpora))/(1024*1024),
                    Lines = length(blogCorpora),
                    Sentences = length(blogSentences),
                    Words = length(blogWords),
                    UniqueWords = nrow(blogWordFreq),
                    WordsNintiethPctile = nrow(blogWordFreq[blogWordFreq$cumpct<0.90,]),
                    SingleOccurrenceWords = nrow(blogWordFreq[blogWordFreq$Freq==1,]))
newsSummary <- data.frame(File = news,
                    SizeMB = as.numeric(object.size(newsCorpora))/(1024*1024),
                    Lines = length(newsCorpora),
                    Sentences = length(newsSentences),
                    Words = length(newsWords),
                    UniqueWords = nrow(newsWordFreq),
                    WordsNintiethPctile = nrow(newsWordFreq[newsWordFreq$cumpct<0.90,]),
                    SingleOccurrenceWords = nrow(newsWordFreq[newsWordFreq$Freq==1,]))
fileSummary <- rbind(twitterSummary, blogSummary, newsSummary)


totalSummary <- data.frame(File = "Totals",
                           SizeMB = sum(fileSummary$SizeMB),
                           Lines = sum(fileSummary$Lines),
                           Sentences = sum(fileSummary$Sentences),
                           Words = sum(fileSummary$Words),
                           UniqueWords = nrow(totalWordFreq),
                           WordsNintiethPctile = nrow(totalWordFreq[totalWordFreq$cumpct<0.90,]),
                           SingleOccurrenceWords = nrow(totalWordFreq[totalWordFreq$Freq==1,])
                           )
fileSummary <- rbind(fileSummary,totalSummary)
fileSummary$WordsPerSentence <- fileSummary$Words/fileSummary$Sentences
fileSummary

write.table(fileSummary,"FileSummary")

hashtags <- twitterWordFreq %>%
    filter(substr(ngram,1,1)=="#")
str(hashtags)
twitterUniqueWords <- nrow(twitterWordFreq)
twitterWordFreq25 <- twitterWordFreq %>% filter(row_number(n)/twitterUniqueWords <= 0.25)
ggplot(twitterWordFreq25,aes(row_number(n)/twitterUniqueWords,cumpct)) + 
    xlab("Cumulative % of Unique Words") +
    ylab("Cumulative % of Total Words in Corpora") +
    geom_line()
topWords <- totalWordFreq[1:100,]
topWords$ngram <- factor(topWords$ngram, levels = topWords$ngram)
ggplot(topWords,aes(ngram,Freq)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

