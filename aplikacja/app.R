
library(shiny)

# Define UI ----

ui <- fluidPage(
    titlePanel("Wskaznik H i Entropia census"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
               information from the 2010 US Census."),
            
            selectInput("year", 
                        label = "Choose a year",
                        choices = c("1990", 
                                    "2000",
                                    "2010", 
                                    "2020"),
                        selected = "1990"),
            
            selectInput("index", 
                        label = "Choose an index",
                        choices = c("Entrophy",
                                    "Index of dissimilarity", 
                                    "The information theory index H"),
                        selected = "Percent White"),
            
            selectInput("var", 
                        label = "Choose a variable to display",
                        choices = c("Percent White", 
                                    "Percent Black",
                                    "Percent Hispanic", 
                                    "Percent Asian"),
                        selected = "Percent White"),
            
        ),
        
        mainPanel(
            textOutput("selected_var")
        )
    )
)
# Define server logic ----
server <- function(input, output) {
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
