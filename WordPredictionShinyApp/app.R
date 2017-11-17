library(shiny)
library(quanteda)
library(data.table)
library(ngram)
library(dplyr)

server <- function(input, output, session) {
    
    lookupTable <- readRDS("WPMLookupTables.rds")
    updateTextAreaInput(session, 
                        "mytext", 
                        label = "Type your message here:")
    updateTxt <- TRUE
    WordsUsed <- 0
    TotalWords <- 0
    htmltext <- ""
    wordcolors <- c("red","blue","green")
    subTableReset <- data.table() #data.table(startpos = 0, endpos = 0, word = "", choicenum = 0)
    subTable <- subTableReset
    
    GenDisplayText <- function(txt){
        print("GenDisplayText")

        if (nchar(txt)==0) return()
        
        if (nrow(subTable) > 0) {
            subTable <<- subTable[endpos < nchar(txt)+1] # deal with backspacing
            wordCount <- nrow(subTable)
        } else wordCount <- 0
        
        dtxt <- ""
        currpos <- 1
        if (wordCount > 0) 
            for (i in 1:wordCount) {
                dtxt <- concatenate(dtxt,
                                    substr(txt,currpos,subTable[i]$startpos-1),
                                    '<span style="color:', 
                                    wordcolors[subTable[i]$choicenum],
                                    '"><b>',
                                    subTable[i]$word,
                                    "</b></span>",
                                    collapse = "")
                currpos <- subTable[i]$endpos + 1
            }
        output$TextDisplay <- renderUI({HTML(concatenate(dtxt, substr(txt,currpos,nchar(txt)), collapse = ""))})
        }
    
    
    AddWord <- function(num){
        print("AddWord")
        txt <- input$mytext
        pred <- prediction()[num]
        currpos <- nchar(txt)+1

        if (nrow(subTable) > 0)
            subTable <<- subTable[endpos < currpos] # deal with backspacing
        subTable <<- rbind(subTable,
                           data.table(startpos = currpos,
                                      endpos = currpos+nchar(pred)-1,
                                      word = pred,
                                      choicenum = num))

        newtxt <- concatenate(txt,
                              pred,
                              " ",
                              collapse = "")
        WordsUsed <<- nrow(subTable)
        updateTextInput(session, "mytext", 
                        value = newtxt)
        UpdateStatistics(newtxt)
    }

    UpdateStatistics <- function(newtxt) {
        print("UpdateStatistics")
        txt <- trimws(newtxt, which = "r")
        if (nchar(txt) == 0) {
            TotalWords <<- 0
            print("Reset")
            subTable <<- subTableReset
        }
        else {
            spaces <- gregexpr(" ",txt)[[1]]
            if (spaces[1] == -1) TotalWords <<- 1
            else TotalWords <<- length(spaces)+1
        }
        WordsUsed <<- nrow(subTable)
        
        if (nrow(subTable) > 0) {
            print(subTable)
            detail <- data.table(subTable %>% 
                group_by(choicenum) %>% 
                summarize(Frequency = n(), 
                          TotalChars = sum(nchar(word))) %>% 
                mutate(AvgWordLength = TotalChars/Frequency,
                       KeyStrokeReduction = TotalChars - Frequency,
                       ReductionPct = KeyStrokeReduction/nchar(txt),
                       PredictionAccuracy = Frequency/max(1,TotalWords)))
            
            detail$choicenum <- paste0('<div style="color: ', 
                                              wordcolors[detail$choicenum], 
                                              ';"><span><b>',
                                              detail$choicenum,
                                              "</b></span></div>")
                
            ksr <- sum(detail$KeyStrokeReduction)
            
        } else
            ksr <- 0
        
        output$StatsTable <- renderUI(HTML(
            paste0('<table style="width:100%"> <tr align="center">',
                   '<td style="color:Teal"><h1>',TotalWords,"</h1> Total Words Typed</td>",
                   '<td style="color:Chocolate"><h1>', WordsUsed, "</h1> Successfully Predicted Words</td>",
                   '<td style="color:MediumBlue"><h1>', round(100*WordsUsed/max(1,TotalWords),0), "%", "</h1> Prediction Accuracy</td>",
                   "</tr></table"))
        )

        # output$StatsTable <- renderTable(
        #     data.table("# of Predicted Words" = WordsUsed,
        #                "Total Words" = TotalWords,
        #                 "Key Stroke Reduction" = paste0(round(100*ksr/max(1,nchar(txt)),0), "%"),
        #                 "Next Word Prediction Accuracy" = paste0(round(100*WordsUsed/max(1,TotalWords),0), "%")),
        #     align = "c", digits = 0, width = "100%"
        #     )
        
        if (nrow(subTable)>0) {
            tots <- data.table (choicenum = "Total",
                      Frequency = sum(detail$Frequency), 
                      AvgWordLength =  round(sum(detail$TotalChars)/sum(detail$Frequency),1), 
                      ReductionPct = ksr/nchar(txt),
                      PredictionAccuracy = sum(detail$Frequency)/max(1,TotalWords))

            pwt <- rbind(detail[,.(choicenum,
                Frequency, 
                AvgWordLength,
                ReductionPct,
                PredictionAccuracy)],
                tots)
            
            pwt <- pwt %>% mutate(ReductionPct = paste0(round(100*ReductionPct,0), "%"),
                                  PredictionAccuracy = paste0(round(100*PredictionAccuracy,0), "%"))
            
            colnames(pwt) <- c("Predicted Word Rank", 
                               "Predicted Word Frequency",
                               "Average Predicted Word Length",
                               "Key Stroke Reduction %", 
                               "Next Word Prediction Accuracy")
            
            print(pwt)
            output$PredictedWordTable <- renderTable({
                pwt
                }, 
                sanitize.text.function = function(x) x,
                align = "c", digits = 0, width = "100%")
            } 
    
    GenDisplayText(newtxt)
    
    }
    
    predictNextWord <- function(sentence, lookupTable, depth = 5, choices = 3, stats = FALSE) {
        print("predictNextWord")
        # 
        # Accept a string of text (sentence) and a lookup table and use them to predict the next word. 
        # The number of predictions returned is indicated by the variable "choices"; 
        # The variable "depth" specifies the maximum number of recent words to use when doing
        # lookups.
        # 
        
        # If the sentence is empty, assume we are at the beginning of a new sentence, and look up the
        # predictions from the first word table rather than the main ngram table
        
        if (nchar(sentence) == 0) return(head(lookupTable[[2]][,fw], n = choices))
        
        # Break up the sentence into individual words
        ngramdata <- unlist(tokens(sentence,
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
            print(paste("sentence:", sentence))
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
    
    prediction <- reactive({
        txt <- input$mytext
        print("prediction")
        
        # Extract the last sentence in the text
        EoS <- gregexpr("\\.|\\;|\\!|\\?",txt)[[1]]
        SoLS <- max(EoS[length(EoS)],0)
        ls <- trimws(substr(txt,SoLS+1,nchar(txt)), which = "l")
        
        if (nchar(ls) == 0 | substr(ls, nchar(ls), nchar(ls)) == " ") {
            UpdateStatistics(txt)
            # If we are at the beginning of a sentence or a new word
            # (indicated by a space character), 
            # get the next word predictions
            return(predictNextWord(ls, lookupTable))
        } 
        else return(c("","",""))
        
    })
    
    output$predictedWords <- renderUI({
        list(actionButton("p1",prediction()[1], width = "32.5%"),
             actionButton("p2",prediction()[2], width = "32.5%"),
             actionButton("p3",prediction()[3], width = "32.5%"))
    })
    
    observeEvent(input$p1, {
        print("p1")
        AddWord(1)
        # if (updateTxt) {
        #     updateTxt <- FALSE
        #     AddWord(1)
        # }
        # else updateTxt <- TRUE
        session$sendCustomMessage(type="refocus",message=list(NULL))
    })
    observeEvent(input$p2, {
        print("p2")
        AddWord(2)
        session$sendCustomMessage(type="refocus",message=list(NULL))
    })
    observeEvent(input$p3, {
        print("p3")
        AddWord(3)
        session$sendCustomMessage(type="refocus",message=list(NULL))
    })
    observeEvent(input$Reset, {
        print("ResetButton")
        WordsUsed <<- 0
        updateTextInput(session,"mytext",value = "")
        UpdateStatistics("")
        output$TextDisplay <- renderUI({HTML(" ")})
    })
}

ui <-   fluidPage( #tabsetPanel(
    #tabPanel("Text Messaging",
    tags$head(tags$style(HTML(".btn {border: none; padding: 0; background-color: #e7e7e7}")),
              tags$script(
                  'Shiny.addCustomMessageHandler("refocus",
                                                function(NULL) {
                                                document.getElementById("mytext").focus();});'),
               tags$style(HTML(".shiny-input-container:not(.shiny-input-container-inline) {
                               width: 100%; height: 100%;}"))
              ),
    fluidRow(
        column(8, h3("Text Messaging With Word Prediction")),
        column(4, br(), actionButton("Reset","Reset", width = "50%"))
    ),
    fluidRow(
        column(6,
               br(),
               textAreaInput("mytext", 
                              "Initializing ...wait for it...", 
                              width = "100%", height = "150px", resize = "vertical"),
                uiOutput("predictedWords"),
               hr(),
               htmlOutput("StatsTable") #,
               #tags$style(type="text/css", '#StatsTable {font-size:15px; color:grey;}')
               
               ),
        column(6,
               br(),
               h5(strong("Prediction Map")),
               wellPanel(htmlOutput("TextDisplay")),
               hr(),
               h5(strong("Prediction Statistics")),
               tableOutput("PredictedWordTable"),
               tags$style(type="text/css", '#PredictedWordTable {font-size:15px; color:grey;}'))
        )
)

shinyApp(ui = ui, server = server)