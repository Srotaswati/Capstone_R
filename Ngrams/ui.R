#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
    ),

    # Application title
    headerPanel("Data Science Capstone: Word Prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Begin typing a sentence to predict the next word in the sentence"),
            textInput(inputId = "phrase",label=""),
            sliderInput(inputId = "num", label = "Number of words to predict:", min = 1, max = 10, value = 5) 
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel(div("Output",style = "font-size:150%"),
                         tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 350px;
               left: 0px;
               width: 35%;
               text-align: left;
               padding: 0px 0px 0px 20px;
               font-weight: bold;
               font-size: 250%;
               color: #000000;
             }
          ")),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                          tags$div("I don't know what this app does but it will teach you patience...",id="loadmessage")),
                    h3("You were probably going to type ..."),
                    br(),
                    h3(textOutput("bestword")),
                    hr(),
                    h3(textOutput("writetext")),
                    div(DT::dataTableOutput("ptable"),style = "font-size:120%")
                ),
                tabPanel(div("Documentation",style = "font-size:120%"),
                    includeMarkdown("doc.Rmd")
                )
            )
        )
    )
))
