#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
source("helper.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$bestword <- renderText({
        predictword(input$phrase,input$num)$words[1]
    })
    output$writetext<-renderText({
        words <- c('one', 'two', 'three', 'four',
                   'five', 'six', 'seven', 'eight', 'nine',
                   'ten')
        paste0("Other possible ",words[input$num-1]," choices ...")
    })
    table<-reactive({
        data.table(words = predictword(input$phrase,input$num)$words[2:input$num],
                   probs = predictword(input$phrase,input$num)$probs[2:input$num])
    })
    output$ptable<-DT::renderDataTable({
        datatable(table(),filter="none", selection="multiple", escape=FALSE, 
                  options = list(sDom  = '<"top"><"bottom">')) 
    })

})
