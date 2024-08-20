#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
fdata = read.csv("Crash_Data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    month_name = factor(month.abb[month], levels = month.abb)
  )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fatalities"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "year_selection", 
                        label = "Select year:", 
                        choices = 1989:2021, 
                        selected = 2019,
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("fatalitiesPlot"),
           verbatimTextOutput("chisq_test")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    final_data = reactive({
      fdata %>% 
        filter(year %in% input$year_selection) %>% 
        group_by(month_name) %>% 
        count()
    })
 
    output$fatalitiesPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      final_data() %>% 
        ggplot(aes(x = month_name, y = n)) + 
        geom_bar(stat = 'identity') + 
        #geom_col() 
        labs(title = paste(input$year_selection, collapse = ", "))
    })
    
    output$chisq_test = renderPrint({
       final_data() %>% 
        pull(n) %>% 
        chisq.test()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
