
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
# wczytanie danych
shp_block_1990 <- st_read("dane/counties_shp/census_shp/shp_block_1990.gpkg")
shp_block_2000 <- st_read("dane/counties_shp/census_shp/shp_block_2000.gpkg")
shp_block_2010 <- st_read("dane/counties_shp/census_shp/shp_block_2010.gpkg")
shp_block_2020 <- st_read("dane/counties_shp/census_shp/shp_block_2020.gpkg")

shp_grp_blocks_1990 <- st_read("dane/counties_shp/census_shp/shp_grp_blocks_1990.gpkg")
shp_grp_blocks_2000 <- st_read("dane/counties_shp/census_shp/shp_grp_blocks_2000.gpkg")
shp_grp_blocks_2010 <- st_read("dane/counties_shp/census_shp/shp_grp_blocks_2010.gpkg")
shp_grp_blocks_2020 <- st_read("dane/counties_shp/census_shp/shp_grp_blocks_2020.gpkg")

shp_tract_1990 <- st_read("dane/counties_shp/census_shp/shp_tract_1990.gpkg")
shp_tract_2000 <- st_read("dane/counties_shp/census_shp/shp_tract_2000.gpkg")
shp_tract_2010 <- st_read("dane/counties_shp/census_shp/shp_tract_2010.gpkg")
shp_tract_2020 <- st_read("dane/counties_shp/census_shp/shp_tract_2020.gpkg")


blocks <- list(shp_block_1990, shp_block_2000, shp_block_2010, shp_block_2020)
grp_blocks <- list(shp_grp_blocks_1990, shp_grp_blocks_2000, shp_grp_blocks_2010, shp_grp_blocks_2020)
tracts <- list(shp_tract_1990, shp_tract_2000, shp_tract_2010, shp_tract_2020)

obiekty <- list(blocks = blocks, grp_blocks =  grp_blocks, tracts = tracts)
# wskazniki
shp_ind_D_1990 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_1990.gpkg")
shp_ind_D_2000 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_2000.gpkg")
shp_ind_D_2010 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_2010.gpkg")
shp_ind_D_2020 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_2020.gpkg")

shp_ind_ent_1990 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_1990.gpkg")
shp_ind_ent_2000 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_2000.gpkg")
shp_ind_ent_2010 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_2010.gpkg")
shp_ind_ent_2020 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_2020.gpkg")

shp_ind_H_1990 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_1990.gpkg")
shp_ind_H_2000 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_2000.gpkg")
shp_ind_H_2010 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_2010.gpkg")
shp_ind_H_2020 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_2020.gpkg")


# Define UI ----

ui <- navbarPage("Racial diversity",
                 tabPanel("Interactive map"),
                 tabPanel("Data frames"),
                 tabPanel("Author"),
                 
                 shinyjs::useShinyjs(),
                 
                 sidebarLayout(
                     
                     sidebarPanel(
                         tabsetPanel(id = "tabset",
                                     checkboxInput("aggr_button", "All indicators for individual years", value = 1),
                             
                             
                             selectInput("unit", 
                                         label = "Choose an aggregation unit",
                                         choices = c("blocks", 
                                                     "grp_blocks",
                                                     "tracts"),
                                         selected = "blocks"),
                             
                             selectInput("year", 
                                         label = "Choose a year",
                                         choices = c("1990", 
                                                     "2000",
                                                     "2010", 
                                                     "2020"),
                                         selected = "1990")),
                         tabsetPanel(id = "tabset2",
                                     checkboxInput("indicator_button", "Selected indicator for individual aggregation units", value = 0),
                             
                             selectInput("index", 
                                         label = "Choose an index",
                                         choices = c("Entrophy",
                                                     "Index of dissimilarity", 
                                                     "The information theory index H"),
                                         selected = "Entrophy")),
                         actionButton("run", "Dawaj kurwa")
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Map", tmapOutput("map")), 
                             tabPanel("Plot", verbatimTextOutput("plot")), 
                             tabPanel("Table", tableOutput("table"))
                         )
                     )
                 )
)

# Define server logic ----
server <- function(input, output,session) {
   
    shinyjs::onclick("advanced2",
                     shinyjs::aggr_button(id = "advanced2", anim = TRUE))
    
    observeEvent(input$aggr_button, {
        if(input$aggr_button == 1){
            shinyjs::disable(id = "index")
            shinyjs::enable(id = "unit")
            shinyjs::enable(id = "year")
            updateCheckboxInput(
                session =  session,
                inputId = "indicator_button", 
                value = FALSE
                )
        } 
        else if(input$indicator_button == 1) {
            shinyjs::enable(id = "index")
            shinyjs::disable(id = "unit")
            shinyjs::disable(id = "year")
            updateCheckboxInput(
                session =  session,
                inputId = "aggr_button", 
                value = FALSE
            )
        }
    })
    
    warstwa <- eventReactive(input$run, {
        
        plik <- switch(input$unit, 
                       "blocks" = obiekty[[1]],
                       "grp_blocks" = obiekty[[2]],
                       "tracts" = obiekty[[3]])
        
        switch(input$year, 
               "1990" = plik[[1]],
               "2000" = plik[[2]],
               "2010" = plik[[3]],
               "2020" = plik[[4]])
    })
    tmap_mode("view")
    output$map <- renderTmap({
        warstwa <- warstwa()
        tm_shape(warstwa) + tm_fill(col = "Entropia_std", 
                                    id = "NAMELSAD",
                                    popup.vars = c("Entropia: " = "Entropia", "Entropia std: " = "Entropia_std", 
                                                   "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                                                   "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                                                   "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")) + tm_borders()
        
        
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
