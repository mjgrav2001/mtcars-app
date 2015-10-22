library(shiny)

library(datasets)
library(plyr)
library(dplyr)
library(Hmisc)
library(reshape2)
library(ggplot2)

data(mtcars)
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

par(mfrow = c(1,2))
par(mar = c(4,4,2,2))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$text <- renderText({
    paste("You have selected this bin size: ", input$binsize)
  })
    
  output$distPlot <- renderPlot({
    # draw the histogram with the specified number of bins
    facet_names <- list("0" = "Automatic", "1" = "Manual")
    facet_labeller <- function(am, value){
      value <- as.character(value)
      return(facet_names[value])
    }
    g1 <- ggplot(mpgData, aes(mpg)) + geom_histogram(binwidth = input$binsize, col = "blue")
    g2 <- g1 + facet_grid(.~ am, labeller = facet_labeller) 
    g3 <- g2 + labs(title = "Miles per gallon for automatic or manual transmission") 
    g4 <- g3 + labs(x = "Miles per gallon [mpg]", y = "frequency")
    g4 
  })
  
  output$figure1 <- renderText({
    paste("Figure 1.")
  })
  
  output$figure2<- renderText({
    paste("Figure 2.")
  })
  
  output$figure3 <- renderText({
    paste("Figure 3.")
  })
  
  output$figure4 <- renderText({
    paste("Figure 4.")
  })
}
)