#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(textreadr)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(reshape2)
library(igraph)
library(ggraph)
library(tm)
library(topicmodels)
library(quanteda)

# install.packages("textdata")
library(textdata)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  
  ### import the text data to dataframe
  answers <- read_document(file="Speak to text survey_Text Analytics_EDITED_3.docx")
  answers_vector <- c(answers)

  a <- 28 # how many observations to you have
  b <- 5  # how many variables do you have
  textData <- as.data.frame(matrix(nrow=a, ncol=b))

  # Breaks the doc in to 5 variables based ont the questions (v1-v5)
  for(z in 1:b){
    for(i in 1:a){
      textData[i,z]<- answers_vector[i*b+z-b]
    }
  }
  
  output$question <- renderPrint({
    zeta <- c('What do you look for in a gaming console?', 'What genre of games do you prefer?', 'How much would you be willing to spend on the PS5 and why?', 'What was your earliest memory of playing with a PlayStation or similar gaming console?', 'Would you consider buying the PS5, yes or no?')
    questionNumber <- zeta[as.integer(input$Question)]
    questionNumber
  })
  
  
  output$sentiment <- renderPrint({
    columns <- paste('V', input$Question, sep='')
    Qeustion <- textData[, columns]
    Qeustion <- substr(Qeustion, start=11 , stop = 10000)
    
    ### into a dataframe
    mydf <- data_frame(id=1:a, text=Qeustion)
    data(stop_words)
    frequencies_tokens <- mydf %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word, sort=TRUE)
    
      nrc_data <- read.csv('lexicon.csv', header=T)
    
     top10 <- frequencies_tokens  %>%
      # inner_join(get_sentiments("nrc") %>%
      inner_join(nrc_data %>%
                   filter(sentiment == input$Sentiment)) %>%   
      # mutate(method='NRC') %>%
      count(word, sentiment, sort=TRUE) 
     
     if (nrow(top10) <= 10) {
       top10
     } else {
       top10[1:10,]
     }
       
  })
  
  
  output$question2 <- renderPrint({
    zeta <- c('What do you look for in a gaming console?', 'What genre of games do you prefer?', 'How much would you be willing to spend on the PS5 and why?', 'What was your earliest memory of playing with a PlayStation or similar gaming console?', 'Would you consider buying the PS5, yes or no?')
    questionNumber <- zeta[as.integer(input$Question2)]
    questionNumber
  })
  
  
  output$sentimentPlot <- renderPlot({
    columns <- paste('V', input$Question2, sep='')
    Qeustion <- textData[, columns]
    Qeustion <- substr(Qeustion, start=11 , stop = 10000)

    ### into a dataframe
    mydf <- data_frame(id=1:a, text=Qeustion)

    data(stop_words)
    frequencies_tokens <- mydf %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(word, sort=TRUE)

    frequencies_tokens  %>%
      inner_join(get_sentiments("nrc") %>%
                   filter(sentiment == input$Sentiment)) %>%
      count(word, sentiment, sort=TRUE) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  
  output$distPlot <- renderPlot({

    columns <- paste('V', input$Question2, sep='')
    Qeustion_1 <- textData[, columns]
    Qeustion_1 <- substr(Qeustion_1, start=11 , stop = 10000)
    
    ### into a dataframe
    mydf_1 <- data_frame(id=1:28, text=Qeustion_1)
    mydf_1
    
    
    bigrams_words <- mydf_1 %>%
      unnest_tokens(bigram, text, token = "ngrams", n=as.integer(input$Gramms))
    
    bigrams_separated <- bigrams_words %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word)
    
    bigram_counts <- bigrams_filtered %>%
      count(word1, word2, sort = TRUE)
    
    bigram_graph <- bigram_counts %>%
      # filter(n>0) %>%
      graph_from_data_frame()
    
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label=name), vjust =1, hjust=1)
    
    # game_dtm <- mydf_1 %>%
    #   unnest_tokens(word, text) %>%
    #   anti_join(stop_words) %>% 
    #   count(id, word, sort=TRUE) %>%
    #   cast_dtm(id, word, n)
    
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label=name), vjust =1, hjust=1)
    
  })
  
  
  output$dtm_topic <- renderPrint({
    
    textData2 <- textData
    textData2$combined = ''
    for (i in 1:5){
      textData2$combined = paste(textData2$combined, textData2[, i])
    }
    
    Qeustion_1 <- textData2$combined
    Qeustion_1 <- substr(Qeustion_1, start=11 , stop = 100000)
    
    ### into a dataframe
    mydf_1 <- data_frame(id=1:28, text=Qeustion_1)
    
    game_dtm <- mydf_1 %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% 
      count(id, word, sort=TRUE) %>%
      cast_dtm(id, word, n)
    
    game_lda <- LDA(game_dtm , k=input$Topics, control=list(seed=123))
    game_topics <- tidy(game_lda, matrix="beta")
    
    if (input$Order == '1') {
      beta_spread <- game_topics %>%
        mutate(topic=paste0("topic", topic)) %>%
        spread(topic, beta) %>%
        filter(topic1>.001 | topic2 >0.001) %>%
        mutate(log_rate = log2(topic2/topic1)) %>%
        arrange((log_rate))
      
      beta_spread
    } else {
      beta_spread <- game_topics %>%
        mutate(topic=paste0("topic", topic)) %>%
        spread(topic, beta) %>%
        filter(topic1>.001 | topic2 >0.001) %>%
        mutate(log_rate = log2(topic2/topic1)) %>%
        arrange(desc(log_rate))
      
      beta_spread
    }
    
  })

  
  output$NB_summary <- renderPrint({
    a <- 28
    textData2 <- textData
    columns <- paste('V', 5, sep='')
    Vi <- textData2[, columns]
    Vi <- substr(Vi, start=11 , stop = 10000)
    mydfi <- data_frame(id=1:a, text=Vi)
    frequency5 <- mydfi %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(id, word, sort=TRUE)
    
    frequency5 <- frequency5[1:(nrow(frequency5)-2), ]
    
    for (i in 1:4){
      columns <- paste('V', i, sep='')
      Vi <- textData2[, columns]
      Vi <- substr(Vi, start=11 , stop = 1000)
      mydfi <- data_frame(id=1:a, text=Vi)
      frequencyI <- mydfi %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(id, word, sort=TRUE)
      frequencyI <- frequencyI[1:(nrow(frequencyI)-1), ]
      frequency5 <- rbind(frequency5, frequencyI)
    }
    
    game_dtm <- frequency5 %>%
      cast_dtm(id, word, n)
    
    ap_td <- tidy(game_dtm)
    matrix_data <- ap_td %>%
      cast_dfm(document, term, count)
    
    labels <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1,
                1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1,
                1, 0, 1)

    testSize <- as.integer(as.double(input$Test_Size) * nrow(matrix_data))
    testIndex <- sample(1:nrow(matrix_data), testSize)
    
    data.train <- matrix_data[-testIndex, ]
    data.test <- matrix_data[testIndex, ]
    trainLabels <- labels[-testIndex]
    testLabels <- labels[testIndex]
  
    NB_classifier <- textmodel_nb(data.train, trainLabels)
    summary(NB_classifier)
  })
  
  output$NB_pred <- renderPrint({
    a <- 28
    textData2 <- textData
    columns <- paste('V', 5, sep='')
    Vi <- textData2[, columns]
    Vi <- substr(Vi, start=11 , stop = 10000)
    mydfi <- data_frame(id=1:a, text=Vi)
    frequency5 <- mydfi %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      count(id, word, sort=TRUE)

    frequency5 <- frequency5[1:(nrow(frequency5)-2), ]


    textData3 <- textData2
    textData3$combined <- ''
    for (i in 1:ncol(textData3)) {
      textData3$combined <- paste(textData3$combined, textData3[, i], sep=  ' ')
    }

    for (i in 1:4){
      columns <- paste('V', i, sep='')
      Vi <- textData2[, columns]
      Vi <- substr(Vi, start=11 , stop = 1000)
      mydfi <- data_frame(id=1:a, text=Vi)
      frequencyI <- mydfi %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(id, word, sort=TRUE)

      frequencyI <- frequencyI[1:(nrow(frequencyI)-1), ]
      frequency5 <- rbind(frequency5, frequencyI)
    }

    game_dtm <- frequency5 %>%
      cast_dtm(id, word, n)

    ap_td <- tidy(game_dtm)
    matrix_data <- ap_td %>%
      cast_dfm(document, term, count)

    labels <- c(1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1,
                1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1,
                1, 0, 1)


    testSize <- as.integer(as.double(input$Test_Size) * nrow(matrix_data))
    testIndex <- sample(1:nrow(matrix_data), testSize)


    data.train <- matrix_data[-testIndex, ]
    data.test <- matrix_data[testIndex, ]
    trainLabels <- labels[-testIndex]
    testLabels <- labels[testIndex]

    NB_classifier <- textmodel_nb(data.train, trainLabels)
    
    pred <- predict(NB_classifier, data.test)
    
    finalMatrix <- data.frame(true = testLabels, pred = pred)
    finalMatrix
  })
  
})
