#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

App_data <- read_csv("PS_7.csv")

# Define UI for application that plots features of movies 
ui <- fluidPage(
  
  # Application title
  titlePanel("Polling Margin of Error by Party"),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      selectInput(inputId = "demographic", 
                  label = "Demographic",
                  choices = c("Democrat" = "Dem_Error", "Republican" = "Rep_Error", "Undecided" = "Und_Error"), 
                  selected = "Dem_Error"),
      

      #create checkbox for user input of states
      checkboxGroupInput(inputId = "selected_type",
                         label = "Select State(s):",
                         choices = c("AZ","CA","CO","FL","GA","IL","IA","KS","KY","ME","MI","MN","NJ","NM","NY","NC","OH","PA","TX","UT","VA","WA","WV"),
                         selected = "CA")
      
    ),
    
    
    # Outputs
    
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      p("Choose from the States in the side panel to investigate how the predicted republican advantage results differed from the actual results"),
      p("You can also flip the axis for another view, if you want")
      
    )
    
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  #filter data based on user selection
  states_subset <- reactive({
    req(input$selected_type)
    filter(App_data, state %in% input$selected_type) 
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    
    #Create visualization using ggplot 
    ggplot(data = states_subset(), aes_string(x = "total", y = input$demographic, color = "state_dist")) +
      geom_point(size = 3, alpha = 0.8)  +
      geom_abline() +
      labs(color = "District") +
      ggtitle("Margin of Error vs Voter Turnout by District")
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)