## Vasuprad.Kanade@accenture.com
## Course 10-Capstone Task 7
## Build Word Prediction
## This program store datagrams into data files to be used by Shiny App to reduce application load times

library(shiny)
library(ANLP)

library(scales)
library(tidyr) # assists in cleaning & preparing data
library(dplyr) # assists in data manipulation, transformation, & summarization
library(stringr)
library(wordcloud)

final.df <- readRDS("./data/frequencyDS.RDS")
top.unigram <- readRDS("./data/uniGramData.RDS")
isReady <- T


shinyServer(function(input, output, session) {
  observe({
    textDisplay <- reactive ({
    
      # user input
      user.input <- input$text
      user.input <- tolower(user.input)
      user.input <- as.vector(strsplit(user.input," ")[[1]])
      user.input <- str_replace_all(user.input, "[^[:alpha:]]", "")
      user.input <- grep('.', user.input, value=TRUE)
      user.input <- paste(user.input, sep=" ", collapse=" ")
      
      # get length of user input
      i <- length(strsplit(user.input," ")[[1]])
      
      if (i > 4){
        sub.input <- seq(i-3, i)
        user.input <- as.vector(strsplit(user.input," ")[[1]])[sub.input]
        i <- length(user.input)
      }
      
      # input for search
      full.input <- paste(user.input, sep=" ", collapse= " ")
      
      n <- i+1
      
      # subset based on length of user input
      source.df <- final.df %>%
        filter(count == n) %>%
        filter(key == full.input)
      
      # if search yields no results look for n-1 gram
      if(nrow(source.df) == 0 & i > 1){
        l = n
        l = l-1
        full.input2 <- strsplit(full.input, " ")[[1]][2:i]
        full.input2 <- paste(full.input2, sep=" ", collapse=" ")
        source.df <- final.df %>%
          filter(count == l) %>%
          filter(key == full.input2)
      }
      
      if(nrow(source.df) == 0 & i > 2){
        l = n
        l = l-2
        full.input3 <- strsplit(full.input, " ")[[1]][3:i]
        full.input3 <- paste(full.input3, sep=" ", collapse=" ")
        source.df <- final.df %>%
          filter(count == l) %>%
          filter(key == full.input3)
      }
      
      if(nrow(source.df) == 0 & i > 3){
        l = n
        l = l-3
        full.input4 <- strsplit(full.input, " ")[[1]][4:i]
        full.input4 <- paste(full.input4, sep=" ", collapse=" ")
        source.df <- final.df %>%
          filter(count == l) %>%
          filter(key == full.input4)
      }
      
      
      ###### MISSPELLINGS or APPROXIMATES########
      # find approximate of initial input
      if(nrow(source.df) == 0 & i > 3){
        
        sub.df <- final.df %>%
          filter(count == 5)
        full.input5 <- strsplit(full.input, " ")[[1]][c(1,3:i)]
        start <- paste("^",full.input5[1], sep="", collapse="")
        end <- paste(full.input5[3],"\\b", sep="", collapse="")
        middle <- paste("\\w+", full.input5[2], sep=" ", collapse="")
        term1 <- paste(start, middle, end, sep=" ", collapse="")
        
        full.input5 <- strsplit(full.input, " ")[[1]][c(1:2,i)]
        start <- paste("^",full.input5[1], sep="", collapse="")
        end <- paste(full.input5[3],"\\b", sep="", collapse="")
        middle <- paste(full.input5[2], "\\w+", sep=" ", collapse="")
        term2 <- paste(start, middle, end, sep=" ", collapse="")
        
        term <- paste(term1, "|", term2, sep="", collapse=" ")
        
        source.df <- sub.df[grep(term, sub.df$key),]
      }
      
      if(nrow(source.df) == 0 & i > 2){
        
        sub.df <- final.df %>%
          filter(count == 4)
        full.input5 <- strsplit(full.input, " ")[[1]][c(1,i)]
        start <- paste("^",full.input5[1], sep="", collapse="")
        end <- paste(full.input5[2],"\\b", sep="", collapse="")
        end <- paste("\\w+", end, sep=" ", collapse="")
        term <- paste(start, end, sep=" ", collapse="")
        
        source.df <- sub.df[grep(term, sub.df$key),]
      }
      
      source.df <- ungroup(source.df)
      source.df <- rbind(source.df[,2:3], top.unigram[,1:2])
      source.df$predict[1:10]
    })
    
    a1 <<- textDisplay()[1]
    a2 <<- textDisplay()[2]
    a3 <<- textDisplay()[3]

    output$prediction1 <- renderUI({
      actionButton("button1", label = a1)
      #HTML("<button id="prediction1" class="shiny-text-output"></button>")
    })
    
    output$prediction2 <- renderUI({
      actionButton("button2", label = a2)
      
    })
    output$prediction3 <- renderUI({
      actionButton("button3", label = a3)
      
    })
  
  })

  observeEvent(input$button1, {
    if(input$button1 == 1){
      name <- paste(input$text, a1)
      updateTextInput(session, "text", value=name)
    }
    
  })
  
  observeEvent(input$button2, {
    
    name <- paste(input$text, a2)
    updateTextInput(session, "text", value=name)
  })
  
  observeEvent(input$button3, {
    
    name <- paste(input$text, a3)
    updateTextInput(session, "text", value=name)
  })
})
