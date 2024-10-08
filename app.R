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
library(palmerpenguins)
library(DT)
library(gt)

numeric_choices = c("body_mass_g", "flipper_length_mm", "bill_depth_mm", "bill_length_mm")
categorical_choices = c("species", "island", "sex", "year")

penguins = penguins %>% 
  mutate(
    year = factor(year)
  )
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Penguins!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "var_xaxis", 
                        label = "X axis variable", 
                        choices = numeric_choices, 
                        selected = "body_mass_g"),
            selectInput(inputId = "var_yaxis", 
                        label = "Y axis variable", 
                        choices = numeric_choices, 
                        selected = "flipper_length_mm"),
            selectInput(inputId = "var_col", 
                        label = "Colouring variable", 
                        choices = categorical_choices, 
                        selected = "species"),
            selectInput(inputId = "var_facet", 
                        label = "Faceting variable", 
                        choices = categorical_choices, 
                        selected = "island"),
            checkboxInput(inputId = "checkbox_line_of_fit", label = 'Enable line of best fit', value = T)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(title = 'Main Plot', 
              plotOutput("distPlot"), gt::gt_output(outputId = 'corr_table')
            ),
            tabPanel(title = 'Penguins Table', DT::DTOutput(outputId = 'table_simple'))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
      penguins %>%  drop_na(sex) %>% ggplot() + 
        aes(x = .data[[input$var_xaxis]], # could also use aes_string() instead of .data[[]]
            y = .data[[input$var_yaxis]],
            colour = .data[[input$var_col]]) +
        geom_point() +
        facet_grid(cols = vars(.data[[input$var_facet]])) + 
        {if(input$checkbox_line_of_fit == T) geom_smooth(method = 'lm', se = F)}
    })
    
    output$table_simple <- renderDT({
      penguins
    })
    
    output$corr_table <- render_gt({
      cor_table = penguins %>%
        group_by(.data[[input$var_facet]]) %>% 
        summarise(r = cor(.data[[input$var_xaxis]], .data[[input$var_yaxis]], use = "pairwise.complete.obs"))
    })
} 

# Run the application 
shinyApp(ui = ui, server = server)
