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

riders <- riders %>% 
  filter(Riders != 0)

agencies <- unique(riders$Agency)
modes <- unique(riders$Mode)

ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("agencySelect",
                           label = "Agencies",
                           choices = agencies,
                           selected = agencies),
        checkboxGroupInput("modeSelect",
                           label ="Modes",
                           choices = modes,
                           selected = modes)
      ),
      mainPanel(plotOutput("ridershipPlot")
    )
  )
)

server <- function(input, output) {
  userSelected <- reactive(df(agencySelect, modeSelect))
  
  ridersPlot <- riders %>% 
    filter(????????????????????????????????????????????????????????????) %>% 
    group_by(Agency)
  
  output$ridershipPlot <- renderPlot({
    ggplot(ridersPlot, aes(x = input$agencySelect, y = Riders, fill = input$modeSelect)) +
    geom_col(position = "dodge")
  })
}

shinyApp(ui = ui, server = server)