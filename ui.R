library(shiny)

library(datasets)
library(plyr)
library(dplyr)
library(Hmisc)
library(reshape2)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel(h1("Miles per gallon - manual versus automatic transmission", align = "center")),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Slider for bin size:", align = "center"),
      sliderInput("binsize", "Bin size:",
                  min = 1,
                  max = 40,
                  value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p("In this course project, data set ", tags$em("mtcars"), " was analyzed with regards to the two questions 
        whether an automatic or manual transmission is better for a car's gas mileage (miles per gallon) 
        and to quantify the mpg difference between automatic and manual transmissions. The column ", tags$em("mpg"), " 
        in the ", tags$em("mtcars"), " data set is selected as outcome. The following predictor variables are chosen: 
        ", tags$em("factor(am)"), " as a two-level factor variable (", tags$em("am=0"), " for manual and '", tags$em("am=1"), "' for automatic transmission); 
        ", tags$em("wt"), " measuring a car's weight in units of 1000 lbs; ", tags$em("hp"), " measuring a car's horse power in units hp; 
        and ", tags$em("factor(am)*wt"), " as interaction term between variables ", tags$em("am"), " and ", tags$em("wt"), ".
        "),
      
      p("Separate linear regression fits are conducted for a comparison of ", tags$em("mpg"), " as outcome and ", tags$em("factor(am)"), " 
        and ", tags$em("wt"), " as predictors and ", tags$em("factor(am)*wt"), " as interaction term and a second comparison with ", tags$em("mpg"), " 
        and ", tags$em("factor(am)"), " and ", tags$em("hp"), " as outcome and predictor variables, respectively. Automatic transmission 
        (", tags$em("am=1"), ") appears favorable for higher gas mileage 
        when weights are smaller while for increased weights a cross-over happens where manual transmission (", tags$em("am=0"), ") 
        starts yielding higher mpg values. This crossover happens at a weight of around 3000 lbs.
        "),
      
      p("In the interactive plot,", tags$em("Figure 1"), "below, 
        two histograms are shown to demonstrate the distribution of 
        mpg values in the data set for the two groups", tags$em("automatic transmission (", tags$em("am=1"), ")."),  
        "and", tags$em("manual transmission (", tags$em("am=0"), ")."), "The bin size in both histograms 
        may be adjusted interactively 
        with the slide bar shown to the left.
      "),
      
      br(), 

      h2("Histogram plot:", align = "center"),
      plotOutput("distPlot"),
 
      br(),
      
      textOutput("figure1"),
      
      br(),
      
      textOutput("text"),
      
      br(), 
      
      span("A general decrease of miles per gallon can be observed with an increase in horse powers, as expected which is independent 
        of the selected group, i.e. whether automatic ", tags$em("am=1"), " or manual transmission ", tags$em("am=0"), " is being assumed. 
        An average downward shift of about 5 miles per gallon in mpg can be seen when switching 
        from  ", tags$em("am=1"), " to ", tags$em("am=0."), "In the other comparison, separate linear fits occur with different 
        slopes for the two groups ", tags$em("am=1"), " and ", tags$em("am=0"), " when ", tags$em("mpg"), " is fitted versus em(wt). 
        This differentiation is due to a relevant interaction term ", tags$em("factor(am) wt"), ".", style = "color:blue"),

      h2("Appendix:", align = "center"),

      p("
        The Appendix summarizes the findings of the data analysis on the ", tags$em("mtcars"), " data set in a series of different plots:
      "),
      
      p("
        - Figure 2:
        The data points for the two regression models are highlighted in red for 
        group ", tags$em("am=0"), " (manual)' and in black for ", tags$em("am=1"), " (automatic). The left-hand plot shows 
        two linear regression fits with different slopes with mpg as outcome versus wt 
        as predictor (black and red lines) due to the included interaction term, factor(am) wt. 
        The right-hand plot shows two linear regression lines with equal slope when fitting mpg 
        versus hp. An overall shift of average mpg values occurs for automatic versus manual 
        transmission at constant hp values.
      "),
      
      p("
        - Figure 3: 
        shows the distribution of normalized residuals 
        for each of the above two regression models. Normalization is done with respect 
        to the standard deviation as computed assuming a t-student distribution due to 
        the small sample size (only 32 vehicle brands compared). 
      "),
      
      p("
        - Figure 4: 
        These two plots compare residual values (y-axis) with 
        fitted values (x-axis) of each model. A clear separation of the 
        data of the two groups ", tags$em("am=0"), " and ", tags$em("am=1"), " can be seen towards lower 
        and higher mpg values, respectively. 
      "),
    
      img(src = "plot_lm_mpg_hp_hist.png", height = 500, width = 500),
      textOutput("figure2"),
      
      br(),
      
      img(src = "plot_resid_mpg_hp.png", height = 500, width = 500),  
      textOutput("figure3"),
      
      br(),
      
      img(src = "plot_resid_mpg_hp_hist.png", height = 500, width = 500),   
      textOutput("figure4")
    )
  )
))