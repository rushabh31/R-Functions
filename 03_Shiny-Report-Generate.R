#
# This is a Shiny web application.You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      downloadButton('rpt_dl', label = "Generate report")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  distPlot_f <- function(bins) {
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    # bins <- input$bins
    bins <- seq(min(x), max(x), length.out = bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  }
  
  output$distPlot <- renderPlot({
    distPlot_f(bins = input$bins)
  })
  
  output$rpt_dl <- downloadHandler(
    filename =  'Report.html',
    contentType =  'text/html',
    content = function(filename) {
      library(knitr)
      
      if (file.exists('Report')) file.remove('Report')
      # if (file.exists('test.knit.md')) file.remove('test.knit.md')
      htmlKnitted<-rmarkdown::render('report.Rmd',quiet=TRUE) #"plain" version, without knitrBootstrap
      x<-readLines(con = htmlKnitted) #"plain" version, without knitrBootstrap
      #library(knitrBootstrap)
      #knit_bootstrap('knitr_report.Rmd') #fancy knitrBootstrap version
      #x<-readLines(con='knitr_report.html')#fancy knitrBootstrap version
      writeLines(x, con = filename)
      # file.rename('knitr_report.html', filename)
    })
}


########################################################################################################
#Calling in rmarkdown document
########################################################################################################

# ---
#   title: "test"
# author: "Oude Gao"
# date: "7/17/2019"
# output: html_document
# ---
#   
#   ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo = TRUE)```
# 
# 
# ## Test
# 
# ```{r}
# distPlot_f(bins = input$bins)```
