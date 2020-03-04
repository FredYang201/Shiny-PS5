#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Analysis on PlayStation5"),
  
  sidebarLayout(
    sidebarPanel(
      h1('Sentiment: '),
      selectInput("Question",
                  "Please select a question: ",
                  choices = c('Q1'=1, 'Q2'=2, 'Q3' = 3, 'Q4' = 4, 'Q5' = 5)
      ),
      
      br(),
      selectInput("Sentiment",
                  "Please select a sentiment: ",
                  choices = c('positive'='positive', 'negative'='negative', 'joy' = 'joy', 'trust' = 'trust', 'surprise' = 'surprise')
      ),
      br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),
      h1('N-gramms: '),
      br(),
      selectInput("Question2",
                  "Please select a question: ",
                  choices = c('Q1'=1, 'Q2'=2, 'Q3' = 3, 'Q4' = 4, 'Q5' = 5)
      ),
      br(),
       sliderInput("Gramms",
                   "Number of gramms:",
                   min = 1,
                   max = 5,
                   value = 2),
      br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),
      br(),
      h1('DTM_Topics: '),
      br(),
      sliderInput("Topics",
                  "Please select number of topics: ",
                  min = 1,
                  max = 5,
                  value = 2),
      
      br(),
      selectInput("Order",
                  "Please select a order: ",
                  choices = c('Ascending'=1, 'Descending'=0)
      ),
      
      br(),br(),br(),br(), br(),
      h1('Naive Bayes Prediction: '),
      sliderInput("Test_Size",
                  "Please select pencentage of test size: ",
                  min = 0.1,
                  max = 0.3,
                  value = 0.2)
      
    ),

    mainPanel(
      h2('Question: '),
      verbatimTextOutput('question'),
      hr(),hr(),
      h2('Sentiment Analysis: '),
      verbatimTextOutput('sentiment'),
      hr(), hr(),
      h2('Question: '),
      verbatimTextOutput('question2'),
      hr(), hr(),
      h2('N-gramms Network: '),
      plotOutput("distPlot"),
      hr(), hr(),
      h2('DTM_Topic: '),
      verbatimTextOutput('dtm_topic'),
      hr(), hr(),
      h2('Naive Bayes: '),
      h4('Summary: '),
      verbatimTextOutput('NB_summary'),
      hr(), hr(),
      h4('Prediction: '),
      verbatimTextOutput('NB_pred')
   ) 
)))
