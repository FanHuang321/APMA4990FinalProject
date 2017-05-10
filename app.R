library(shiny)
library(ROCR)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(party)
library(rpart)
library(stringr)
library(shinythemes)

source("readmission-app/ReadmissionTree.R")

 ui <- navbarPage("Hospital Readmission Prediction APP",
   theme=shinytheme("yeti"),
  tabPanel("Heart Failure",
           helpText("©James&Bing"),
           "Data Source ", 
           span("Medicare.gov", style = "color:blue"),
  sidebarLayout(
    sidebarPanel(
      h3("Result of Prediction on Heart Failure:"),
      textOutput("text1")
  
      )
    ,
    mainPanel(
      fluidRow(
        
        column(3,
               numericInput("f1", 
                            label = strong(h5("Equipment")), br(),
                            value = 103)),  
        
        column(3,
               numericInput("f2", 
                            label = h5("Health Agency"), 
                            value = 816)),
        
        column(3, 
               numericInput("f3", 
                            label = h5("Hospice"), 
                            value = 122))
      ),
     
      fluidRow(
        
        column(3,
               numericInput("f4", 
                            label = h5("Skilled Nursing"), 
                            value = 3319)),
        
        column(3,
               numericInput("f5", 
                            label = h5("ED1b"), 
                            value = 300)),
        
        column(3, 
               numericInput("f6", 
                            label = h5("ED2b"), 
                            value = 200))
      ),
      
      fluidRow(
        
        column(3,
               numericInput("f7", 
                            label = h5("Care Transition"), 
                            value = 5)),
        
        column(3,
               numericInput("f8", 
                            label = h5("Cleanliness"), 
                            value = 5)),
        
        column(3, 
               numericInput("f9", 
                            label = h5("Discharge Info"), 
                            value = 5))
      ),
      fluidRow(
        
        column(3,
               numericInput("f10", 
                            label = h5("Medication Communication"), 
                            value = 5)),
        
        column(3,
               numericInput("f11", 
                            label = h5("Doctor Communication"), 
                            value = 5)),
        
        column(3, 
               numericInput("f12", 
                            label = h5("Nurse Communication"), 
                            value = 5))
        
      ),
      fluidRow(
        
        column(3,
               numericInput("f13", 
                            label = h5("Pain Management"), 
                            value = 5)),
        
        column(3,
               numericInput("f14", 
                            label = h5("Quietness"), 
                            value = 5)),
        
        column(3, 
               numericInput("f15", 
                            label = h5("Staffresponsiveness"), 
                            value = 5))
        
      )
      
    )
  )
),


tabPanel("COPD",
         helpText("©James&Bing"),
         "Data Source ", 
         span("Medicare.gov", style = "color:blue"),
         sidebarLayout(
           sidebarPanel(
             h3("Result of Prediction on COPD:"),
             textOutput("text2")
             
           )
           ,
           mainPanel(
             fluidRow(
               
               column(3,
                      numericInput("f16", 
                                   label = strong(h5("Equipment")), br(),
                                   value = 103)),  
               
               column(3,
                      numericInput("f17", 
                                   label = h5("Health Agency"), 
                                   value = 816)),
               
               column(3, 
                      numericInput("f18", 
                                   label = h5("Hospice"), 
                                   value = 122))
             ),
             
             fluidRow(
               
               column(3,
                      numericInput("f19", 
                                   label = h5("Skilled Nursing"), 
                                   value = 3319)),
               
               column(3,
                      numericInput("f20", 
                                   label = h5("ED1b"), 
                                   value = 300)),
               
               column(3, 
                      numericInput("f21", 
                                   label = h5("ED2b"), 
                                   value = 200))
             ),
             
             fluidRow(
               
               column(3,
                      numericInput("f22", 
                                   label = h5("Care Transition"), 
                                   value = 5)),
               
               column(3,
                      numericInput("f23", 
                                   label = h5("Cleanliness"), 
                                   value = 5)),
               
               column(3, 
                      numericInput("f24", 
                                   label = h5("Discharge Info"), 
                                   value = 5))
             ),
             fluidRow(
               
               column(3,
                      numericInput("f25", 
                                   label = h5("Medication Communication"), 
                                   value = 5)),
               
               column(3,
                      numericInput("f26", 
                                   label = h5("Doctor Communication"), 
                                   value = 5)),
               
               column(3, 
                      numericInput("f27", 
                                   label = h5("Nurse Communication"), 
                                   value = 5))
               
             ),
             fluidRow(
               
               column(3,
                      numericInput("f28", 
                                   label = h5("Pain Management"), 
                                   value = 5)),
               
               column(3,
                      numericInput("f29", 
                                   label = h5("Quietness"), 
                                   value = 5)),
               
               column(3, 
                      numericInput("f30", 
                                   label = h5("Staffresponsiveness"), 
                                   value = 5))
               
             )
             
           )
         )
),
tabPanel("Pneumonia",
         helpText("©James&Bing"),
         "Data Source ", 
         span("Medicare.gov", style = "color:blue"),
         sidebarLayout(
           sidebarPanel(
             h3("Result of Prediction on Pneumonia:"),
             textOutput("text3")
             
           )
           ,
           mainPanel(
             fluidRow(
               
               column(3,
                      numericInput("f31", 
                                   label = strong(h5("Equipment")), br(),
                                   value = 103)),  
               
               column(3,
                      numericInput("f32", 
                                   label = h5("Health Agency"), 
                                   value = 816)),
               
               column(3, 
                      numericInput("f33", 
                                   label = h5("Hospice"), 
                                   value = 122))
             ),
             
             fluidRow(
               
               column(3,
                      numericInput("f34", 
                                   label = h5("Skilled Nursing"), 
                                   value = 3319)),
               
               column(3,
                      numericInput("f35", 
                                   label = h5("ED1b"), 
                                   value = 300)),
               
               column(3, 
                      numericInput("f36", 
                                   label = h5("ED2b"), 
                                   value = 150))
             ),
             
             fluidRow(
               
               column(3,
                      numericInput("f37", 
                                   label = h5("Care Transition"), 
                                   value = 5)),
               
               column(3,
                      numericInput("f38", 
                                   label = h5("Cleanliness"), 
                                   value = 5)),
               
               column(3, 
                      numericInput("f39", 
                                   label = h5("Discharge Info"), 
                                   value = 5))
             ),
             fluidRow(
               
               column(3,
                      numericInput("f40", 
                                   label = h5("Medication Communication"), 
                                   value = 5)),
               
               column(3,
                      numericInput("f41", 
                                   label = h5("Doctor Communication"), 
                                   value = 5)),
               
               column(3, 
                      numericInput("f42", 
                                   label = h5("Nurse Communication"), 
                                   value = 5))
               
             ),
             fluidRow(
               
               column(3,
                      numericInput("f43", 
                                   label = h5("Pain Management"), 
                                   value = 5)),
               
               column(3,
                      numericInput("f44", 
                                   label = h5("Quietness"), 
                                   value = 5)),
               
               column(3, 
                      numericInput("f45", 
                                   label = h5("Staffresponsiveness"), 
                                   value = 5))
               
             )
             
           )
         )
)
)

#Readmission_Reduction <- read_csv('readmission-app/data/READMISSION REDUCTION.csv')
#HCAHPS <- read_csv('readmission-app/data/HCAHPS - Hospital.csv')
#EffectiveCare <- read_csv('readmission-app/data/Timely and Effective Care - Hospital.csv')
#SpendingClaim <- read_csv('readmission-app/data/Medicare Hospital Spending by Claim.csv')

 
server <- (function(input, output){
  
  output$text1 <- renderText({ 
    Readmission_Tree_HF(input$f1, input$f2, input$f3, input$f4, input$f5, input$f6, input$f7, input$f8, input$f9, input$f10,
                    input$f11, input$f12, input$f13, input$f14, input$f15)
  })
  output$text2 <- renderText({ 
    Readmission_Tree_COPD(input$f16, input$f17, input$f18, input$f19, input$f20, input$f21, input$f22, input$f23, input$f24, input$f25,
                     input$f26, input$f27, input$f28, input$f29, input$f30)
  })
  output$text3 <- renderText({ 
    Readmission_Tree_Pneumonia(input$f31, input$f32, input$f33, input$f34, input$f35, input$f36, input$f37, input$f38, input$f39, input$f40,
                     input$f41, input$f42, input$f43, input$f44, input$f45)
  })
})

shinyApp(ui = ui, server = server)

