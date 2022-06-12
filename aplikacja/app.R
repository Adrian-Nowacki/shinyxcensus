
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(shinydashboard)
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
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with 
               information from the 2010 US Census."),
            
            
            selectInput("unit", 
                        label = "Choose an aggregation unit",
                        choices = c("blocks", 
                                    "groups of blocks",
                                    "tracts"),
                        selected = "blocks"),
            
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
            actionButton("action", "Show")
            
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
server <- function(input, output) {
    tmap_mode("view")
    output$map <- renderTmap({
        tm_shape(shp_tract_2020) + tm_fill(col = "Entropia_std", 
                                           id = "NAMELSAD",
                                           popup.vars = c("Entropia: " = "Entropia", "Entropia std: " = "Entropia_std", 
                                                          "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                                                          "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                                                          "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")) + tm_borders()
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
