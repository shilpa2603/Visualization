# Load libraries
install.packages("ggthemes")
library(shiny)
library(tidyverse)
library(ggthemes)
# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes_string(x =input$continuous_variable)) +
        geom_histogram(bins = input$bins) +  # histogram geom
        labels(x =input$continuous_variable,  y = "Frequency", title = "Histogram") +  # labels
        facet_wrap(~prediction) # facet by prediction
      theme_minimal()
    }
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot()+  # boxplot geom
        coord_flip() +  # flip coordinates
        labels(y = input$continuous_variable, title = "Boxplot") +  # labels
        facet_wrap(~prediction)    # facet by prediction
      theme_fivethirtyeight()
    }
    
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x =input$categorical_variable)) +
      labs(x = input$categorical_variable, y = "Count", title = "Bar Chart") +  # labels
      theme_minimal()
    theme(axis.title.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")    # modify theme to change text angle and legend position
    
    if (input$is_stacked) {
      p +geom_bar(aes(fill = prediction), position = "stack")  # add bar geom and use prediction as fill
    }
    else{
      p + geom_bar(aes(fill = input$categorical_variable), position = "dodge") + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~prediction)  # facet by prediction
    }
  })
  
})
