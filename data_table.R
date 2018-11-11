# dataset from 'https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009'

# Create an app that allows users to choose a random number of wines and display relevant 
# information about each of the wines in a tabular format (e.g., input 30 in the sidebarPanel 
# and see information on 30 wines in the mainPanel).  You can decide which variable to show on 
# the table in the main panel.

library(shiny)
library(dplyr)
library(DT)

# Reads csv file 
wine <- read.csv('winequalityred.csv')

# Renames column names
colnames(wine)[colnames(wine)=="fixed.acidity"] <- "fixed_acidity"
colnames(wine)[colnames(wine)=="volatile.acidity"] <- "volatile_acidity"
colnames(wine)[colnames(wine)=="citric.acid"] <- "citric_acid"
colnames(wine)[colnames(wine)=="residual.sugar"] <- "residual_sugar"
colnames(wine)[colnames(wine)=="free.sulfur.dioxide"] <- "free_sulfur_dioxide"
colnames(wine)[colnames(wine)=="total.sulfur.dioxide"] <- "total_sulfur_dioxide"

# Counts total number of records
n_total <- nrow(wine)

# Define UI for application that plots features of wine
ui <- fluidPage(
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Input:
    sidebarPanel(
      # Text instructions
      HTML(paste("Enter number of wine records to display b/w 1 and", n_total)),
      
      # Numeric input for sample size
      numericInput(inputId = "n",
                   label = "Records",
                   value = 20,
                   min = 1, max = n_total,
                   step = 1)
    ),
    
    # Output: Show data table
    mainPanel(
      DT::dataTableOutput(outputId = "WineQuality_table")
    )
  )
)

# Define server function required to create the table
server <- function(input, output) {
  
 # Create data table
  output$WineQuality_table <- DT::renderDataTable({req(input$n)

    wine_sample <- wine %>% sample_n(input$n) %>% select(fixed_acidity:quality)
      DT::datatable(data = wine_sample,
                    options = list(pageLength = 15),
                    rownames = FALSE)
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
