# install.packages("tm")
# install.packages("ngram")
# install.packages("openNLP")
# install.packages("qdap")
# install.packages("profmem")
library(quanteda)
library(ngram)
library(dplyr)
library(stringr)
library(pryr)
library(data.table)
library(profmem)

setwd("C:/Users/Dave/Documents/Data Science/Coursera/Data Science Specialization Capstone Project/Coursera-SwiftKey")

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
    
    return(samplefile)
}

createNgrams <- function (filepath = NA, 
                          textdata = NA,
                          stats = TRUE, 
                          sentenceBreaks = TRUE,
                          ngrams = 1,
                          maxseg = 250000,
                          remove_numbers = TRUE, 
                          remove_punct = TRUE,
                          remove_symbols = TRUE) {

    funcname <- "createNgrams"
    if (!is.na(filepath)) {
        inputcon <- file(filepath, "r") 
        if (stats) print(paste(Sys.time(), funcname, "Reading input file:",filepath))
        textdata <- readLines(inputcon, warn = FALSE)
        close(inputcon)
    }
    
    if (length(textdata)==1 & is.na(textdata[1])) { 
        print("Error: A value must be given for either filepath or text data")
        return()
    }
    
    textlines <- length(textdata)  
    if (stats) {
        print(paste(Sys.time(), funcname, "Number of lines in corpus:",textlines))
        print(paste(funcname,
                    "sentenceBreaks =", sentenceBreaks,
                    "ngrams =", paste0(min(ngrams),":",max(ngrams)),
                    "remove_numbers =", remove_numbers,
                    "remove_punct =", remove_punct,
                    "remove_symbols =", remove_symbols))
        print(paste0("Memory Utilization = ",
                     round(100*memory.size()/memory.limit(),0),
                     "%"))
    }
    
    # Tokenize the file
    if (sentenceBreaks) 
        # Break the corpora into sentences before creating the ngrams
        textdata <- unlist(tokens(textdata, what = "sentence"))
    
    firstline <- 1
    lineincrement <- maxseg
    ngfreq <- data.table(prefix=character(),lw=character(),freq=integer(), stringsAsFactors = FALSE)
    
    while (firstline <= textlines) {
        lastline <- min(firstline+lineincrement, textlines)
        if (stats) print(paste(Sys.time(), funcname, "processing lines", firstline, ":", lastline))
        ngramdata <- unlist(tokens(textdata[firstline:lastline],
                                   what = "word",
                                   ngrams = ngrams,
                                   concatenator = " ",
                                   remove_numbers = remove_numbers,
                                   remove_punct = remove_punct,
                                   remove_symbols = remove_symbols,
                                   remove_twitter = TRUE,
                                   remove_url = TRUE)
                            )

        firstline <- lastline + 1
        if (stats) {
            print(paste(Sys.time(), 
                        funcname, 
                        "total ngrams =", 
                        length(ngramdata)))
        }
        
        tempdf <- data.table(prefix = substr(ngramdata,1,regexpr(" [^ ]*$",ngramdata)-1),
                             lw = substr(ngramdata,regexpr(" [^ ]*$",ngramdata)+1,999))[, .(freq = .N), by = .(prefix, lw)] 

        ngfreq <- rbind(ngfreq, tempdf)
    }
    
    # Note that we eliminate any ngram with only a single occurrence
   
    ngfreq <- ngfreq[, .(freq = sum(freq)), by = .(prefix, lw)][freq > 1][order(prefix, -freq)]
    
    if (stats) {print(paste(Sys.time(), funcname, "returning"))
        print(paste0("Memory Utilization = ",
                     round(100*memory.size()/memory.limit(),0),
                     "%"))
    }
    return(ngfreq)
}

GetSentences <- function   (filepath = NA, 
                          textdata = NA,
                          stats = TRUE,
                          maxseg = 500000,
                          remove_numbers = TRUE, 
                          remove_punct = TRUE,
                          remove_symbols = TRUE) {
    
    funcname <- "GetSentences"
    if (!is.na(filepath)) {
        inputcon <- file(filepath, "r") 
        if (stats) print(paste(Sys.time(), funcname, "Reading input file:",filepath))
        textdata <- readLines(inputcon, warn = FALSE)
        close(inputcon)
    }
    
    if (length(textdata)==1 & is.na(textdata[1])) { 
        print("Error: A value must be given for either filepath or text data")
        return()
    }
    # Convert text to pure ASCII
    
    textdata <- iconv(textdata, "latin1","ASCII", sub = "")
    
    textlines <- length(textdata)
    if (stats) {
        print(paste(Sys.time(), funcname, "Number of lines in corpus:",textlines))
        print(paste(funcname,
                    "remove_numbers =", remove_numbers,
                    "remove_punct =", remove_punct,
                    "remove_symbols =", remove_symbols))
    }
    
    # Break the data up into sentences
    # In order to deal with potentially very large corpora, break up the data into 
    # pieces and loop through the calls to tokens
    # 
    sentences <- as.character()
    firstline <- 1
    lineincrement <- maxseg
    
    while (firstline <= textlines) {
        lastline <- min(firstline+lineincrement, textlines)
        if (stats) print(paste(Sys.time(), funcname, "processing lines", firstline, ":", lastline))
        sentences <- c(sentences, unlist(tokens(textdata[firstline:lastline], 
                              what = "sentence",
                              remove_numbers = remove_numbers,
                              remove_punct = remove_punct,
                              remove_symbols = remove_symbols)))  
        firstline <- lastline+1
        if (stats) {print(paste(Sys.time(), funcname, "Memory Utilization =",
                         round(100*memory.size()/memory.limit(),0),"%"))
        }
        
    }

    if (stats) print(paste(Sys.time(), funcname, "Returning",length(sentences),"sentences"))
    return(sentences)
}

FirstWords <- function(sentences, stats = TRUE) {
    funcname <- "FirstWords"
    # Get a list of the 50 most frequent first words found in all sentences
    if (stats) print(paste(Sys.time(), funcname, "Start"))
        
    fw <- substr(sentences,1,str_locate(sentences," ")[,1]-1)
    fwfreq <- as.data.table(table(fw))[order(-N)] 
    fwfreq <- fwfreq[1:min(50,nrow(fwfreq))]
    names(fwfreq) <- c("fw","freq")
    fwfreq$fw <- paste0(toupper(substr(fwfreq$fw,1,1)),
                          substr(fwfreq$fw,2,nchar(fwfreq$fw)))
    if (stats) print(paste(Sys.time(), funcname, "Returning"))
    return(fwfreq)
}


buildLookupTable <- function( sample,
                              getsentences = FALSE,
                              stats = TRUE,
                              depth = 1,
                              minFreq = 5,
                              cums = FALSE) {
    funcname <- "buildLookupTable"
    if (stats) print(paste(Sys.time(), funcname, 
                           "depth =", depth, 
                           "minFreq =", minFreq,
                           "cums =", cums))
    
    sampleSentences <- tolower(if (getsentences) GetSentences(textdata = sample) else sample)
    
    fw <- FirstWords(sampleSentences, stats = stats)
    
    lookup <- data.table() # initialize lookup table

    for (ng in depth) {
        lookup <- rbind(lookup,createNgrams(textdata = sampleSentences,
                                            sentenceBreaks = FALSE,
                                            ngrams = ng,
                                            stats = stats))
        if (stats) print(paste(Sys.time(), funcname, "Lookup Table Size =",
                                     object.size(lookup)))
    }
    # Remove any profane predicted words
    
    if (stats) print(paste(Sys.time(), funcname, "Removing profane predictions"))
    
    profanityFilter <- readLines("badwords.txt", skipNul = TRUE, warn = FALSE)
    # Need to strip trailing whitespace
    profanityFilter <- gsub("^\\s+|\\s+$","",profanityFilter)
    # Remove the multi-word entries - they will never match a predicted word
    profanityFilter <- profanityFilter[!grepl("\\s",profanityFilter)]
    lookup <- lookup[!(lw %in% profanityFilter)]
    
    # Restrict predictions to all-alpha values
    lookup <- lookup[!grepl("[^[:alpha:]]",lw)]
    
    # For the set of entries where the prefix is empty, we only need the first few 
    lookup <- lookup[(prefix=="" & freq > lookup[21,freq]) | !(prefix == "")]
    
    # Set the key to make lookups fast, and sort within prefix by frequency in descending 
    # order so we can just pluck the first n choices off the top
    setkey(lookup, prefix)
    setorder(lookup, prefix, -freq)
    
    if (stats) print(paste(Sys.time(), funcname, "returning"))
    return(list(lookup,fw))
}

predictNextWordsSentence <- function(sentence, lookupTable, depth = NA, choices = 5, stats = TRUE) {
    funcname <- "PredictNextWordSentence"
    # Break up the sentence into individual words
    ngramdata <- unlist(tokens(sentence,
                what = "word",
                ngrams = 1,
                concatenator = " ",
                remove_numbers = TRUE,
                remove_punct = TRUE,
                remove_symbols = TRUE,
                remove_twitter = TRUE,
                remove_url = TRUE))

    sentwords <- tolower(ngramdata)
    numwords <- length(sentwords)
    
    if(numwords == 0) return(list(score = rep(0,choices), total = 0, pct = rep(0,choices)))
    
    if (stats) {
        print(paste("choices:",choices))
        print(paste("sentence:", sentence))
        print(paste("words:", concatenate(sentwords)))
        print(paste("numwords =",numwords))
        str(lookupTable[[2]])
    }
    # Predict the first word in the sentence
    fwpredictions <- tolower(head(lookupTable[[2]]$fw, n = choices))
    score <- fwpredictions==sentwords[1]
    if (stats) {
        print(paste("First word:", sentwords[1]))
        print(paste("First word predictions:", concatenate(fwpredictions)))
        print(paste("Cum Score =",score))
    }
    # Create predictions for each next word in the sentence using Katz's backoff model
    if (numwords > 1) {
        for (i in 1:(numwords-1)) {
            predictions <- character()
            nGramSize <- min(depth,i)
            while (length(predictions) < choices & nGramSize>0) {
                lookupNgram <- concatenate(sentwords[(i-nGramSize+1):i])
                ngrampredictions <- head(lookupTable[[1]][prefix==lookupNgram,lw], 
                                                   n = choices)
                ngrampredictions <- setdiff(ngrampredictions, predictions) # remove any choices already included
                predictions <- c(predictions, head(ngrampredictions, n = choices - length(predictions)))
                
                nGramSize <- nGramSize - 1
            }
            if (length(predictions) < choices)
                predictions <- c(predictions, lookupTable[[1]][prefix == "",lw][1:(choices-length(predictions))])
            score <- score + (predictions == sentwords[i+1])

            if (stats) {
                print(paste("NGram =",lookupNgram))
                print(paste("Predictions:",concatenate(predictions)))
                print(paste("Next word:",sentwords[i+1]))
                print(paste("Cumulative score =",concatenate(score)))
                }
            }
    }
    if (stats) print(paste(Sys.time(), funcname, "Score:", concatenate(score)))
    return(list(score = score, total = numwords, pct = score/numwords))
}

predictNextWord <- function(ngram, lookupTable, depth = 5, choices = 5, stats = FALSE) {
    # Break up the sentence into individual words
    ngramdata <- unlist(tokens(ngram,
                               what = "word",
                               ngrams = 1,
                               concatenator = " ",
                               remove_numbers = TRUE,
                               remove_punct = TRUE,
                               remove_symbols = TRUE))
    
    numwords <- length(ngramdata)
    ngramsize <- min(numwords, depth-1)
    sentwords <- tolower(ngramdata[max(numwords-(depth-1)+1,1):numwords])
    if (stats) {
        print(paste("choices:",choices))
        print(paste("ngram:", ngram))
        print(paste("words:", concatenate(sentwords)))
        print(paste("ngramsize =",ngramsize))
    }
    # Create predictions for the next word using Katz's backoff model
        predictions <- character()
        i <- 1
        while (length(predictions) < choices & i <= ngramsize) {
            lookupNgram <- concatenate(sentwords[i:ngramsize])
            ngrampredictions <- head(lookupTable[[1]][prefix==lookupNgram,lw], 
                                     n = choices)
            if (stats) {
                print(paste("Lookup ngram:", lookupNgram))
                print(paste("Predictions:", concatenate(ngrampredictions)))
            }
            ngrampredictions <- setdiff(ngrampredictions, predictions) # remove any choices already included
            predictions <- c(predictions, head(ngrampredictions, n = choices - length(predictions)))
            i <- i + 1
        }
        if (length(predictions) < choices)
            predictions <- c(predictions, head(lookupTable[[1]][,lw], n = choices-length(predictions)))

        if (stats) {
            print(paste("NGram =",lookupNgram))
            print(paste("Predictions:",concatenate(predictions)))
        }
        
    return(predictions)
}


measureAccuracy <- function(testSample, 
                            lookupTable,
                            testSentences,
                            depth = 5,
                            choices = 5, 
                            maxSentences = NA, 
                            stats = FALSE) {
    funcname <- "measureAccuracy"

    if (!(length(testSentences)>0)) testSentences <- GetSentences(textdata = testSample)
    numSentences <- length(testSentences)
    maxSentences = min(maxSentences, numSentences, na.rm = TRUE)

    start.time <- Sys.time()
    print(start.time)
    cumResults <- list(score = rep(0,choices), total = 0, pct = rep(0, choices))
    for (i in 1:maxSentences){
        results <- predictNextWordsSentence(testSentences[i], lookupTable, depth = depth, stats = stats, choices = choices)
        if (stats) {print(paste(Sys.time(), funcname, "Results:", concatenate(results$score)))
                    print(results)}
        cumResults$score <- cumResults$score + results$score
        cumResults$total <- cumResults$total + results$total
        if (stats) print(paste(Sys.time(), funcname, "cumResults:", concatenate(cumResults$score)))
        if ((i %% max(1,round(maxSentences/100,0))) == 0) {
            print(paste0(Sys.time(), " ", funcname, " ", round(100*i/maxSentences,0), "% Complete (",
                         i, " sentences)",
                        " Total Score = ", round(100*sum(cumResults$score)/cumResults$total,2), "%",
                        " Score by Rank: ", concatenate(round(100*cumResults$score/cumResults$total,2))))
        }
    }
    end.time <- Sys.time()
    print(end.time)
    time.taken <- end.time - start.time
    time.taken
    return (list(score = cumResults$score,total = cumResults$total, pct = cumResults$score/cumResults$total, 
                 TotPct = sum(cumResults$score)/cumResults$total))
}

# Now let's build a model and test prediction accuracy for each of the three corporii

corpii <- c("en_US.twitter.txt","en_US.blogs.txt","en_US.news.txt")
#corpii <- c("en_US.twitter.txt")
#corpii <- c("en_US.news.txt")
#corpii <- c("en_US.blogs.txt")


testsent <- c("I have no idea how that happened. Can you explain it?")
measureAccuracy(testsent,ClookupTableSmall, maxSentences = 100, stats = T, choices = 5)

# Build separate results for each corpora
###################################################################
# Twitter
corpora <- "en_US.twitter.txt"
TtrainSamplePath <- createSampleFile(corpora, suffix = "train", 
                                    sampleType = "segment",
                                    startpct = 0, 
                                    endpct = 0.60)

TtrainSample <- readLines(TtrainSamplePath, warn = FALSE)
#TlookupTable <- buildLookupTable(TtrainSample)

# Create a separate test sample
TtestSamplePath <- createSampleFile(corpora, suffix = "test", 
                                   sampleType = "segment",
                                   startpct = 0.60, 
                                   endpct = 0.80)

TtestSample <- readLines(TtestSamplePath, warn = FALSE)
#Tresults <- measureAccuracy(TtestSample,TlookupTable) #maxSentences = 1000, stats = F, choices = 5)
#Tresults

###################################################################
# Blogs
corpora <- "en_US.blogs.txt"
BtrainSamplePath <- createSampleFile(corpora, suffix = "train", 
                                     sampleType = "segment",
                                     startpct = 0, 
                                     endpct = 0.60)

BtrainSample <- readLines(BtrainSamplePath, warn = FALSE)
#BlookupTable <- buildLookupTable(BtrainSample)

# Create a separate test sample
BtestSamplePath <- createSampleFile(corpora, suffix = "test", 
                                    sampleType = "segment",
                                    startpct = 0.60, 
                                    endpct = 0.80)

BtestSample <- readLines(BtestSamplePath, warn = FALSE)
#Bresults <- measureAccuracy(BtestSample,BlookupTable) #maxSentences = 1000, stats = F, choices = 5)
#Bresults

###################################################################
# News
corpora <- "en_US.news.txt"
NtrainSamplePath <- createSampleFile(corpora, suffix = "train", 
                                     sampleType = "segment",
                                     startpct = 0, 
                                     endpct = 0.60)

NtrainSample <- readLines(NtrainSamplePath, warn = FALSE)
#NlookupTable <- buildLookupTable(NtrainSample)

# Create a separate test sample
NtestSamplePath <- createSampleFile(corpora, suffix = "test", 
                                    sampleType = "segment",
                                    startpct = 0.60, 
                                    endpct = 0.80)

NtestSample <- readLines(NtestSamplePath, warn = FALSE)
#NtestSample <- readLines("en_US.news_sample.txt")
#Nresults <- measureAccuracy(NtestSample,lookupTable, maxSentences = 1000, stats = F, choices = 5)
#Nresults

# Now create a single model combining the training data from all three corporii

CtrainSample <- c(TtrainSample,BtrainSample,NtrainSample)
ClookupTable <- buildLookupTable(CtrainSample, depth = 5)

# Test it against each of the test samples
CtestSample <- c(TtestSample, BtestSample, NtestSample)
Csentences <- GetSentences(textdata = CtestSample)
testResults <- measureAccuracy(testSentences = Csentences, lookupTable = ClookupTable)
round(100*testResults$pct,2)
round(sum(100*testResults$pct),2)

CTresultsSmall <- measureAccuracy(TtestSample,ClookupTableSmall, maxSentences = 5000)
#CTresults <- measureAccuracy(TtestSample,ClookupTable, maxSentences = 1000)
CBresultsSmall <- measureAccuracy(BtestSample,ClookupTableSmall, maxSentences = 5000)
#CBresults <- measureAccuracy(BtestSample,ClookupTable, maxSentences = 1000)
CNresultsSmall <- measureAccuracy(NtestSample,ClookupTableSmall, maxSentences = 5000)
#CNresults <- measureAccuracy(NtestSample,ClookupTable)

CTresultsSmall
#CTresults
CBresultsSmall
#CBresults
CNresultsSmall
#CNresults

saveRDS(ClookupTableSmall, file = "WPMLookup")
WPMLookup <- readRDS("WPMLookup")

# Try building a lookup table based on the entire corpora

t <- readLines("en_US.twitter.txt", warn = FALSE)
b <- readLines("en_US.blogs.txt", warn = FALSE)
n <- readLines("en_US.news.txt", warn = FALSE)

corpora <- c(readLines("en_US.twitter.txt", warn = FALSE),
             readLines("en_US.blogs.txt", warn = FALSE),
             readLines("en_US.news.txt", warn = FALSE))

corpora <- c("This is the first line. It has two sentences.",
             "A second line has three sentences. Here is the second sentence. Guess which one this is?",
             "Now we move on to the next line...is this one sentence or two?")

csent <- GetSentences(textdata = corpora)

ng <- createNgrams(textdata = csent, sentenceBreaks = FALSE, ngrams = 1)
lookupTable <- buildLookupTable(csent, depth = 1:3)

saveRDS(csent, file = "FullCorporaSentences.rds")
sentences <- readRDS("FullCorporaSentences.rds")
lookupTable <- buildLookupTable(sentences, depth = 1:5)
saveRDS(lookupTable, file = "WPMLookupTables.rds")

lookupTable <- readRDS("WPMLookupTables.rds")
lookupTable[[1]] <- lookupTable[[1]][(prefix=="" & freq > lookupTable[[1]][21,freq]) | !(prefix == "")]
setkey(lookupTable[[1]], prefix)
setorder(lookupTable[[1]], prefix, -freq)
str(lookupTable[[1]])

# Create a reduced-size lookup table by eliminating any ngrams with 3 or fewer occurrences
lookupTable[[1]] <- lookupTable[[1]][freq>3]

# Fix up predictions of the word "i" to be upper-case
lookupTable[[1]][lw=="i"]$lw <- "I"

# Remove any non-ASCII character predictions
lookupTable[[1]] <- lookupTable[[1]][iconv(lw, "latin1", "ASCII", sub="") == lw]

saveRDS(lookupTable, file = "WPMLookupTablesGT3.rds")


predictNextWord("i'd live and i'd", lookupTable, choices = 50)
predictNextWord("telling me about his", lookupTable, choices = 50)
predictNextWord("see arctic monkeys this", lookupTable, choices = 50)
predictNextWord("and helps reduce your", lookupTable, choices = 50)
predictNextWord("time to take a", lookupTable, choices = 50)
predictNextWord("jury to settle the", lookupTable, choices = 50)
predictNextWord("of groceries in each", lookupTable, choices = 50)
predictNextWord("the bottom to the", lookupTable, choices = 50)
predictNextWord("and bruises from playing", lookupTable, choices = 50)
predictNextWord("all of Adam Sandler's", lookupTable, choices = 50)

lookupTable[[1]][prefix == "you must be"]


## Quiz 3 questions
lookupTable[[1]][prefix=="and i'd" & lw %in% c("sleep","die","eat","give")]
lookupTable[[1]][prefix=="his" & lw %in% c("marital","horticultural","spiritual","financial")]
lookupTable[[1]][prefix=="this" & lw %in% c("morning","month","decade","weekend")]
lookupTable[[1]][prefix=="reduce your" & lw %in% c("stress","sleepiness","hunger","happiness")]
lookupTable[[1]][prefix=="time to take a" & lw %in% c("walk","look","minute","picture")]
lookupTable[[1]][prefix=="to settle the" & lw %in% c("incident","matter","account","case")]
lookupTable[[1]][prefix=="in each" & lw %in% c("toe","hand","arm","finger")]
lookupTable[[1]][prefix=="the bottom to the" & lw %in% c("middle","side","top","center")]
lookupTable[[1]][prefix=="from playing" & lw %in% c("outside","daily","inside","weekly")]
lookupTable[[1]][prefix=="all of Adam Sandler's" & lw %in% c("novels","stories","movies","pictures")]


