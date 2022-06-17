
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(plotly)
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

aggr_objects <- list(blocks = blocks, grp_blocks =  grp_blocks, tracts = tracts)
# wskazniki
shp_ind_ent_1990 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_1990.gpkg")
shp_ind_ent_2000 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_2000.gpkg")
shp_ind_ent_2010 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_2010.gpkg")
shp_ind_ent_2020 <- st_read("dane/counties_shp/indexes_shp/shp_ind_ent_2020.gpkg")

shp_ind_D_1990 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_1990.gpkg")
shp_ind_D_2000 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_2000.gpkg")
shp_ind_D_2010 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_2010.gpkg")
shp_ind_D_2020 <- st_read("dane/counties_shp/indexes_shp/shp_ind_D_2020.gpkg")

shp_ind_H_1990 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_1990.gpkg")
shp_ind_H_2000 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_2000.gpkg")
shp_ind_H_2010 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_2010.gpkg")
shp_ind_H_2020 <- st_read("dane/counties_shp/indexes_shp/shp_ind_H_2020.gpkg")

entrophy <- list(shp_ind_ent_1990, shp_ind_ent_2000, shp_ind_ent_2010, shp_ind_ent_2020)
d_index <- list(shp_ind_D_1990, shp_ind_D_2000, shp_ind_D_2010, shp_ind_D_2020)
h_index <- list(shp_ind_H_1990 , shp_ind_H_2000 , shp_ind_H_2010 , shp_ind_H_2020 )

ind_objects <- list(entrophy = entrophy, d_index = d_index, h_index = h_index)
# Define UI ----

ui <- navbarPage(id = "navbar",
                 "Racial diversity",
                 tabPanel("Interactive map",
                          sidebarLayout(
                              
                              sidebarPanel(id = "sidebar",
                                           tabsetPanel(id = "tabset",
                                                       checkboxInput("aggr_button", "All indicators for individual years", value = 1),
                                                       
                                                       
                                                       selectInput("unit", 
                                                                   label = "Choose an aggregation unit",
                                                                   choices = c("Blocks", 
                                                                               "Groups of blocks",
                                                                               "Tracts"),
                                                                   selected = "blocks"),
                                                       
                                                       
                                                       checkboxInput("indicator_button", "Selected indicator for individual aggregation units", value = 0),
                                                       
                                                       selectInput("index", 
                                                                   label = "Choose an index",
                                                                   choices = c("Entropy",
                                                                               "Index of dissimilarity", 
                                                                               "The information theory index H"),
                                                                   selected = "Entrophy")),
                                           
                                           tabsetPanel(id = "tabset2",
                                                       selectInput("year", 
                                                                   label = "Choose a year",
                                                                   choices = c("1990", 
                                                                               "2000",
                                                                               "2010", 
                                                                               "2020"),
                                                                   selected = "1990")),
                                           actionButton("run", "Lets goooooooooo")
                              ),
                              
                              mainPanel(id = "mainpanel",
                                        tabsetPanel(
                                            tabPanel("Map", tmapOutput("map", height = "600px")), 
                                            tabPanel("Plot", plotlyOutput("plot",  height = "600px")), 
                                            tabPanel("Table", DT::dataTableOutput("table"))
                                        )
                              )
                          )
                 ),
                 tabPanel("Data frames",
                          sidebarLayout(
                              sidebarPanel(id = "dt_sidebar",
                                           selectInput("dataset", "Choose a dataset:", 
                                                       choices = c("Rock", "Pressure", "Cars")),
                                           radioButtons("filetype", "File type:",
                                                        choices = c("csv", "shp", "gpkg")),
                                           downloadButton('downloadData', 'Download')
                              ),
                              mainPanel(id = "dt_mainpanel",
                                        tableOutput("aaa")
                              )
                          )
                 ),
                 tabPanel("Author",
                          fluidRow(
                              column(2,
                                     img(src='github_logo.png', width = "200px", height = "200px", align = "center")),
                              column(10, 
                                     h2("Application visualizing the values of racial segregation indicators for individual aggregation units sizes"),
                              )
                          )
                          
                          
                 ),
                 
                 setBackgroundColor(
                     color =  "#252525"
                 ),
                 
                 tags$head(tags$style(
                     HTML('
                         #sidebar {
                            background-color: #353535;
                            width:440px;
                         }
                        #navbar {
                            background-color: #252525;
                        }
                        #mainpanel {
                        margin-left:-50px;
                        width: 1050px;
                        }
                        #dt_sidebar {
                        background-color: #353535;
                        color: #dddddd;
                        }
                
                        body, label, input, button, select { 
                          font-family: "Trebuchet MS";
                          color: #dddddd;
                        }')
                 )),
                 
                 shinyjs::useShinyjs()
                 
)


# Define server logic ----
server <- function(input, output,session) {
    
    observeEvent(input$aggr_button, {
        if(input$aggr_button == 1){
            shinyjs::disable(id = "index")
            shinyjs::enable(id = "unit")
            updateCheckboxInput(
                inputId = "indicator_button", 
                value = FALSE
            )} 
        else {
            shinyjs::enable(id = "index")
            shinyjs::disable(id = "unit")
            updateCheckboxInput(
                inputId = "aggr_button",
                value = FALSE
            )}
    })
    
    observeEvent(input$indicator_button, {
        if(input$indicator_button == 0){
            shinyjs::disable(id = "index")
            shinyjs::enable(id = "unit")
            updateCheckboxInput(
                inputId = "indicator_button", 
                value = FALSE
            )} 
        else {
            shinyjs::enable(id = "index")
            shinyjs::disable(id = "unit")
            updateCheckboxInput(
                inputId = "aggr_button",
                value = FALSE
            )}
    })
    observeEvent({input$aggr_button
        input$indicator_button}, {
            if(input$aggr_button == 0 & input$indicator_button ==0){
                shinyjs::disable(id = "index")
                shinyjs::disable(id = "unit")}
        })
    
    warstwa <- eventReactive(input$run, {
        
        aggr <- switch(input$unit, 
                       "Blocks" = aggr_objects[[1]],
                       "Groups of blocks" = aggr_objects[[2]],
                       "Tracts" = aggr_objects[[3]])
        
        indexes <- switch(input$index, 
                          "Entropy" = ind_objects[[1]],
                          "Index of dissimilarity" = ind_objects[[2]],
                          "The information theory index H" = ind_objects[[3]])
        
        #function(){
        if (input$aggr_button == 1){
            switch(input$year, 
                   "1990" = aggr[[1]],
                   "2000" = aggr[[2]],
                   "2010" = aggr[[3]],
                   "2020" = aggr[[4]])
        }
        else if (input$indicator_button == 1){
            switch(input$year, 
                   "1990" = indexes[[1]],
                   "2000" = indexes[[2]],
                   "2010" = indexes[[3]],
                   "2020" = indexes[[4]])
        }
        #}
    })
    tmap_mode("view")
    
    output$map <- renderTmap({
        warstwa <- warstwa() 
        tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + tm_fill(col = "H", 
                                                                           id = "NAMELSAD",
                                                                           palette = "YlGn",
                                                                           popup.vars = c("Nazwa: " = "NAMELSAD", "Entropia std: " = "Entropia_std", 
                                                                                          "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                                                                                          "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                                                                                          "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")) + tm_borders()
        
        
        
    })
    
    output$plot <- renderPlotly({
        warstwa <- warstwa() 
        g <- ggplot(warstwa, aes(warstwa$H)) + geom_histogram(bins = 80) + theme(
            panel.grid.major.y = element_blank(),
            plot.background = element_rect(fill = "#252525"),
            panel.background = element_rect(fill = "#252525"), 
            axis.title = element_text(size = 15,
                                      color = "#dddddd"), 
            plot.title = element_text(size = 18,
                                      color = "#dddddd",
                                      vjust = 2,
                                      hjust = 0.5), 
            legend.background = element_rect(color = "#222222", 
                                             fill = "#777777"),  
            legend.title = element_text(size = 13),
            legend.text = element_text(size = 12),
            axis.text = element_text(size = 12, 
                                     color = "#dddddd"))
        ggplotly(g)
    }) 
    # output$table <- renderDataTable(
    #     warstwa <- as.data.frame(warstwa()),
    #     warstwa <- warstwa[, 3:5],
    #     warstwa,
    #     #warstwa1 <- warstwa %>% st_drop_geometry(),
    #     options = list(pageLength = 15))
    output$table <- DT::renderDataTable(DT::datatable(warstwa(), options = list(
        rownames = FALSE,
        pageLength = 12,
        autoWidth = TRUE,
        columnDefs = list(list(visible=FALSE, targets= c(2:5, 8:10, 20))),
        lengthMenu = c(6, 12, 18))))
}


# Run the app ----
shinyApp(ui = ui, server = server)
