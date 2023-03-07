library(tidyverse)
library(shiny)

# Title Page
## Contains general information about the dataset.
## No interactivity or sidebar is needed
## Use at least two html text formatting markers, such as strong() or em()

# Plot Page
## At least two widgets that change visual output (type of plot, for example)
## Use sidebar or something more complex
## Main panel displays the data
## At least one widget allows the user to modify values displayed on screen
## At least one widget allows the user to change only the visual, data untouched
## Must be appropriately labeled
## Must have textual output, such as number of non-missing data values, or
##   overall average value

# Table Page
## Use sidebar or something more complex
## Main panel contains table
## At least two widgets that change visual output
## At least one widget should allow for modification of which data is displayed
## Must include textual output, such as number of non-missing data values, or
##   overall average value

# Other
## github repo must have a readme with the following:
### Brief explanation of data
### What widgets and panels are doing
### Link to shinyapps for the project

riders <- read_delim("data/seattle_transit_data.csv")

agencies <- unique(riders$Agency)
modes <- unique(riders$Mode)

server <- function(input, output) {
  
  ridersData <- reactive({
    req(input$agencySelect, input$modeSelect)
    ridersTemp <- riders %>% 
      filter(Agency %in% input$agencySelect, Mode %in% input$modeSelect) %>% 
      group_by(Agency)
  })
  
  output$mainPlot <- renderUI ({
    p <- mainPanel(plotOutput(input$selectPlot))
    p
  })
  
  output$ridershipPlot <- renderPlot({
    g <- ggplot(ridersData(), aes(x = Agency, y = Riders, fill = Mode)) +
      geom_col(position = "dodge")
    g
  })
}

ui <- navbarPage("Seattle Transit Ridership",
  tabPanel("Home"),
  tabPanel("Plot",
    titlePanel("Transit Data Plot"),
  
    sidebarLayout(
      sidebarPanel(
        selectInput("selectPlot",
                    label = "Select Plot Type",
                    choices = list("ridershipPlot" = 1, "Plot 2" = 2),
                    selected = 1),
        checkboxGroupInput("agencySelect",
                           label = "Agencies",
                           choices = agencies,
                           selected = agencies),
        checkboxGroupInput("modeSelect",
                           label ="Modes",
                           choices = modes,
                           selected = modes)
      ),
      uiOutput("mainPlot")
    )
  ),
  tabPanel("Table")
)

shinyApp(ui, server)