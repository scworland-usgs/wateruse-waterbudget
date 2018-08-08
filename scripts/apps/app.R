library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

ui <- fluidPage(
  
  titlePanel("Dynamic Histogram"),
  
  sliderInput(inputId = "slider_num", 
              label = "Choose number of bins", 
              value=50, 
              min=10,
              max=100),
  
  selectInput(inputId = "bincolor",
              label = "Choose a bin color",
              choices = c("red", "blue", "green", "black"),
              selected = "black"),
  
  sliderInput(inputId = "value_num",
              label = "Choose number of random samples",
              value = 500,
              min = 200,
              max = 1000),
  
  textInput(inputId = "plot_title",
            label = "Type title for plot",
            value = "Histograms of two probability distributions"),
  
  br(),
  
  actionButton(inputId = "new_samples",
               label = "Generate plot with new samples"),
  
  br(),
  br(),
  
  plotOutput("histogram"),
  
  dataTableOutput('tab', width="90%")
)

server <- function(input, output){
  
  # data <- reactive({data.frame(normal=round(rnorm(input$value_num),3),
  #                              exponential=round(rexp(input$value_num),3))})
  
  data <- eventReactive(input$new_samples, {data.frame(normal=round(rnorm(input$value_num),3),
                                                       exponential=round(rexp(input$value_num),3))})
  
  
  output$histogram <- renderPlot({
    ggplot(gather(data(),key,value)) +
      geom_histogram(aes(value),
                     bins=input$slider_num, 
                     fill=input$bincolor,
                     color="white") +
      facet_wrap(~key, ncol=2) +
      ggtitle(isolate({input$plot_title})) +
      theme_bw()
  })
  
  output$tab <- DT::renderDataTable({
    data()
  })
}

shinyApp(ui, server)