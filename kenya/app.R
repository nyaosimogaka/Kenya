library(shiny)
library(shiny.fluent)
library(dplyr)

#Read Data
df <- read.csv("/home/nyaosi/Desktop/Kenya/2019CENSUS.csv")

county_choices <- unique(df$County)
county_choices <- lapply(county_choices, function(county) {
  list(key = county, text = county)
})

subcounty_choices <- unique(df$Subcounty)
subcounty_choices <- lapply(subcounty_choices, function(subcounty) {
  list(key = subcounty, text = subcounty)
})

filters <- Stack(
  tokens = list(childrenGap = 10),
  Dropdown.shinyInput(
    inputId = "county",
    label = "Select County",
    options = county_choices,
    value = county_choices[[1]]
  ),
  Dropdown.shinyInput(
    inputId = "subcounty",
    label = "Select Sub County",
    options = subcounty_choices
  )
)

# Define UI for application
ui <- fluentPage(
  filters,
  uiOutput("analysis")
)

# Initialize reactiveValues outside the server function
counties <- reactiveValues(df = NULL)
subcounties <- reactiveValues(df = NULL)

# Define server logic 
server <- function(input, output) {
  observe({
    # Reactive data frame for County selection
    counties$df <- df[df$County == input$county, ]
  })
  
  observe({
    # Update subcounty dropdown options based on selected county
    options <- unique(counties$df$Subcounty)
    dropdown_options <- lapply(options, function(subcounti) {
      list(key = subcounti, text = subcounti)
    })
    
    # Pass session explicitly
    updateDropdown.shinyInput(session = getDefaultReactiveDomain(), "subcounti", choices = dropdown_options)
  })
  
  observe({
    # Reactive data frame for match selection
    subcounties$df <- df[df$County == input$county & df$Subcounty == input$subcounti, ]
  })
  
  output$analysis <- renderUI({
    items_list <- DetailsList(items = subcounties$df)
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", "Event Details", block = TRUE),
      div(style = "max-height: 500px; overflow: auto", items_list)
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
