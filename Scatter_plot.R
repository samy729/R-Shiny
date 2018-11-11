# dataset from 'https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009'

# Create an app that would allow a user to generate a scatterplot in the mainPanel after 
# choosing two input variables from two dropdown menus in sidebarPanel.  Also, add a box 
# below the scatterplot that prints the correlation between the two variables.  (e.g., a 
# user interested in the correlation between density and pH can input those values in the 
# sidebarPanel and receive the above information in the mainPanel.)

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
      plotOutput(outputId = "scatterplot"), textOutput(outputId = "correlation"))
 )
)

# Define server function 
server <- function(input, output) {
  
  # Create scatterplot 
  output$scatterplot <- renderPlot({
   p <- ggplot(data = wine, aes_string(x = input$x, y = input$y)) +
      geom_point()
   p + ggtitle("Scatter plot b/w Wine Characteristics") + theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  # Display correlation
  output$correlation <- renderText({
    cr <- round(cor(wine[, input$x], wine[, input$y], use = "pairwise"), 3)
    paste0("Correlation Coefficient = ", cr, " (Rounded to three decimal places)")
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)  