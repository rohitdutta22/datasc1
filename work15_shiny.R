#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Gapminder Data"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Text for providing a caption ----
            # Note: Changes made to the caption in the textInput control
            # are updated in the output area immediately as you type
        
                   checkboxGroupInput("continent", 
                                      h3("Choose which continent"), 
                                      choices = levels(gapminder$continent),
                                      selected = c("Asia","Americas")),
            
            # Input: Selector for choosing dataset ----
            
            sliderInput("year", "Selected Years",
                        min = 1952, max = 2007,
                        value = c(1976,2000)),
            
            # Input: Numeric entry for number of obs to view ----
            selectInput("country", h3("Select Country"), 
                        choices = levels(gapminder$country), 
                        selected = "France"),
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            

            
            #plot 
            plotOutput("points"),
            
            ##another plot
            plotOutput("lines")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$points <- renderPlot({
        cont <- input$continent
        y <- input$year
        dat <- subset(gapminder, gapminder$continent %in% cont & gapminder$year >= y[1] & gapminder$year <= y[2])
        
        ggplot(dat, aes(x = gdpPercap, y = lifeExp, size = pop, colour = continent)) +
            geom_point(show.legend = TRUE, alpha = 0.7) +
            scale_color_viridis_d() +
            scale_size(range = c(2, 12)) +
            scale_x_log10() +
            labs(x = "GDP per capita", y = "Life expectancy")
    
    })
    
    output$lines <- renderPlot({
        
        cont <- input$continent
        y <- input$year
        coun <- input$country
        dat <- subset(gapminder, gapminder$continent %in% cont & gapminder$year >= y[1] & gapminder$year <= y[2])
        ggplot(dat,
               aes(year, lifeExp, group = country, color = continent)) +
            geom_line() +
            geom_line(color = (gapminder$country == coun))
            labs(x = "Year", y = "Life Expectancy")
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
