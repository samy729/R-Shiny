# dataset from 'https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009'

# Create an interactive app that would allow a user to generate a scatterplot by choosing 
# two input variables, as above.  Additionally, allow a user to hover over data points in the 
# scatterplot and see information about that point underneath the scatterplot. 

library(stats)
library(tidyverse)
library(shiny)

# Reads csv file 
wine <- read.csv('winequalityred.csv')

# Renames column names
colnames(wine)[colnames(wine)=="fixed.acidity"] <- "fixed_acidity"
colnames(wine)[colnames(wine)=="volatile.acidity"] <- "volatile_acidity"
colnames(wine)[colnames(wine)=="citric.acid"] <- "citric_acid"
colnames(wine)[colnames(wine)=="residual.sugar"] <- "residual_sugar"
colnames(wine)[colnames(wine)=="free.sulfur.dioxide"] <- "free_sulfur_dioxide"
colnames(wine)[colnames(wine)=="total.sulfur.dioxide"] <- "total_sulfur_dioxide"

vars <- c("fixed acidity"          = "fixed_acidity",
          "volatile acidity"       = "volatile_acidity",
          "citric acid"            = "citric_acid",
          "residual sugar"         = "residual_sugar",
          "chlorides"              =  "chlorides",
          "free sulfur dioxide"    =  "free_sulfur_dioxide",
          "total sulfur dioxide"   =  "total_sulfur_dioxide",
          "density"                =  "density",
          "pH"                     =  "pH",
          "sulphates"              =  "sulphates",
          "alcohol"                =  "alcohol",
          "quality"                = "quality")

# Define UI for application that plots features of wine 
ui <- fluidPage(
  
  br(),
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs:
    sidebarPanel(
      
      # Text instructions
      HTML(paste("Select variables on the x and y axis:")),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis",
                  choices = vars, 
                  selected = "alcohol"),
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis",
                  choices = vars, 
                  selected = "pH")
      
    ),
    # Outputs:
    mainPanel(
      # Show scatterplot with brushing capability
      plotOutput(outputId = "scatterplot", hover = "plot_hover"),
      # Show data table
      dataTableOutput(outputId = "wine_detail"),
      br()
    )
  )
)

# Define server function 
server <- function(input, output) {
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    p <- ggplot(data = wine, aes_string(x = input$x, y = input$y)) +
      geom_point()
    p + ggtitle("Scatter plot b/w Wine Characteristics") + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Create data table for the hovering data points
  output$wine_detail <- DT::renderDataTable({
    nearPoints(wine, input$plot_hover) %>% 
      select(fixed_acidity, volatile_acidity, citric_acid, residual_sugar, chlorides,
             free_sulfur_dioxide, total_sulfur_dioxide, density, pH, sulphates, alcohol, quality)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)  