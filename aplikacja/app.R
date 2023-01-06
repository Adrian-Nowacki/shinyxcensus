
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(plotly)
library(leaflet)
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
                                                       selectInput("variable_unit", 
                                                                   label = "Choose a variable",
                                                                   choices = c("Entropia", 
                                                                               "Entropia std",
                                                                               "H", 
                                                                               "D_wb",
                                                                               "D_wa", 
                                                                               "D_wl",
                                                                               "D_bl", 
                                                                               "D_ba",
                                                                               "D_la"),
                                                                   selected = "Entropia"),
                                                       
                                                       
                                                       checkboxInput("indicator_button", "Selected indicator for individual aggregation units", value = 0),
                                                       
                                                       selectInput("index", 
                                                                   label = "Choose an index",
                                                                   choices = c("Entropy",
                                                                               "Index of dissimilarity", 
                                                                               "The information theory index H"),
                                                                   selected = "Entrophy"),
                                                       selectInput("variable_index", 
                                                                   label = "Choose an aggregation unit and variable",
                                                                   choices = c("Entropy    |ALL AGGREGATION UNITS", 
                                                                               "Entropy std    |ALL AGGREGATION UNITS"),
                                                                   selected = "Entropy")),
                                           
                                           tabsetPanel(id = "tabset2",
                                                       selectInput("year", 
                                                                   label = "Choose a year",
                                                                   choices = c("1990", 
                                                                               "2000",
                                                                               "2010", 
                                                                               "2020"),
                                                                   selected = "1990")),
                                           actionButton("run", "Run")
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
                                                       choices = c("Blocks - 1990", "Blocks - 2000", "Blocks - 2010", "Blocks - 2020",
                                                                   "Groups of blocks - 1990", "Groups of blocks - 2000", "Groups of blocks - 2010", "Groups of blocks - 2020",
                                                                   "Tracts - 1990", "Tracts - 2000", "Tracts - 2010", "Tracts - 2020")),
                                           radioButtons("filetype", "File type:",
                                                        choices = c(".csv", ".shp", ".gpkg")),
                                           downloadButton('download_csv', 'Download .csv'),
                                           downloadButton('download_gpkg', 'Download .gpkg')
                              ),
                              mainPanel(id = "dt_mainpanel",
                                        tableOutput("table_dt")
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
                        color: #bbbbbb;
                        }
                        #dt_sidebar {
                        background-color: #353535;
                        color: #dddddd;
                        }
                        #download_gpkg {
                        margin-left:160px;
                        }
                        #table {
                        color:#dddddd;
                        }
                        #table th{
                        color:#dddddd;
                        background-color:rgba(70, 70, 70, 0.9);
                        border-right:1px solid #dddddd;
                        }
                        #table tr:nth-child(2n+1){
                        background-color:#dddddd;
                        color:#222222;
                        }
                        #table a{
                        color:#dddddd!important;
                        }
                        
                        
                
                        body, label, input, button, select { 
                          font-family: "Lucida Console";
                          color: #dddddd;
                          letter-spacing: -0.3px;
                        }')
                 )),
                 
                 shinyjs::useShinyjs()
                 
)


# Define server logic ----
server <- function(input, output,session) {
    
    observeEvent(input$aggr_button, {
        if(input$aggr_button == 1){
            shinyjs::disable(id = "index")
            shinyjs::disable(id = "variable_index")
            shinyjs::enable(id = "unit")
            shinyjs::enable(id = "variable_unit")
            updateCheckboxInput(
                inputId = "indicator_button", 
                value = FALSE
            )} 
        else {
            shinyjs::enable(id = "index")
            shinyjs::enable(id = "variable_index")
            shinyjs::disable(id = "unit")
            shinyjs::disable(id = "variable_unit")
            updateCheckboxInput(
                inputId = "aggr_button",
                value = FALSE
            )}
    })
    
    observeEvent(input$indicator_button, {
        if(input$indicator_button == 0){
            shinyjs::disable(id = "index")
            shinyjs::disable(id = "variable_index")
            shinyjs::enable(id = "unit")
            shinyjs::enable(id = "variable_unit")
            updateCheckboxInput(
                inputId = "indicator_button", 
                value = FALSE
            )} 
        else {
            shinyjs::enable(id = "index")
            shinyjs::enable(id = "variable_index")
            shinyjs::disable(id = "unit")
            shinyjs::disable(id = "variable_unit")
            updateCheckboxInput(
                inputId = "aggr_button",
                value = FALSE
            )}
    })
    observeEvent({input$aggr_button
        input$indicator_button}, {
            if(input$aggr_button == 0 & input$indicator_button ==0){
                shinyjs::disable(id = "index")
                shinyjs::disable(id = "variable_index")
                shinyjs::disable(id = "unit")
                shinyjs::disable(id = "variable_unit")}
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
    
    observeEvent(input$index, {
      
    if (input$indicator_button == 1){
        if (input$index == "Entropy"){
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("Entropy    |ALL AGGREGATION UNITS",
                                        "Entropy std    |ALL AGGREGATION UNITS"),
                            selected = "Entropy    |ALL AGGREGATION UNITS"
          )}
        else if (input$index == "Index of dissimilarity"){
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("white-black    |BLOCKS",
                                        "white-asian    |BLOCKS",
                                        "white-latin    |BLOCKS",
                                        "black-latin    |BLOCKS",
                                        "black-asian    |BLOCKS",
                                        "latin-asian    |BLOCKS",
                                        "white-black    |GROUP OF BLOCKS",
                                        "white-asian    |GROUP OF BLOCKS",
                                        "white-latin    |GROUP OF BLOCKS",
                                        "black-latin    |GROUP OF BLOCKS",
                                        "black-asian    |GROUP OF BLOCKS",
                                        "latin-asian    |GROUP OF BLOCKS",
                                        "white-black    |TRACT",
                                        "white-asian    |TRACT",
                                        "white-latin    |TRACT",
                                        "black-latin    |TRACT",
                                        "black-asian    |TRACT",
                                        "latin-asian    |TRACT"),
                            selected = "white-black    |BLOCKS"
          )
        }
        else if (input$index == "The information theory index H"){
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("H    |BLOCKS",
                                        "H    |GROUP OF BLOCKS",
                                        "H    |TRACTS"),
                            selected = "H    |BLOCKS"
          )
        }
    }
    })
    
         output$map <- renderTmap({
           if (input$aggr_button == 1){
             
             tmap_mode("view")
             warstwa <- warstwa()
             variable_unit <- as.character(input$variable_unit)
             popup <- c("Entropia: " = "Entropia", "Entropia std: " = "Entropia_std", 
                        "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                        "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                        "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")
             
               tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                 tm_fill(col = variable_unit, 
                         palette = "YlGn",
                         id = "NAMELSAD",
                         popup.vars = popup) + tm_borders()
            
             
             }
        #   
        #   if (input$aggr_button == 1){
        #   tmap_mode("view")
        #   popup <- paste0("<strong>County: </strong>", 
        #                         warstwa()$NAMELSAD, 
        #                         "<br><strong> Entrophy std: </strong>",
        #                   warstwa()$Entropia_std,
        #                         "<br><strong> H index: </strong>",
        #                   warstwa()$H,
        #                         "<br><strong> D (white-black): </strong>",
        #                   warstwa()$D_wb,
        #                   "<br><strong> D (white-asian): </strong>",
        #                   warstwa()$D_wa,
        #                   "<br><strong> D (white-latin): </strong>",
        #                   warstwa()$D_wl,
        #                   "<br><strong> D (black-latin): </strong>",
        #                   warstwa()$D_bl,
        #                   "<br><strong> D (black-asian): </strong>",
        #                   warstwa()$D_ba,
        #                   "<br><strong> D (latin-asian): </strong>",
        #                   warstwa()$D_la)
        #   # text = paste0("Nazwa: " = "NAMELSAD", "Entropia std: " = "Entropia_std", 
        #   #          "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
        #   #          "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
        #   #          "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")
        #   warstwa <- warstwa()
        #   variable_unit <- as.character(input$variable_unit)
        #   pal <- colorNumeric(
        #     palette = "YlGn",
        #     domain = warstwa[[variable_unit]])
        #   pal2 <- colorNumeric(
        #     palette = "YlGn",
        #     domain = warstwa[[variable_unit]])
        #   map <- leaflet(data = warstwa()) %>%
        #     setView(lat = 50, lng = -120, zoom = 3.2) %>%
        #     addTiles() %>%
        #     
        #     addPolygons(fillColor = ~pal(warstwa[[variable_unit]]),
        #                 popup = popup,
        #                 group = "H",
        #                 color = "#222222",
        #                 fillOpacity = 0.8,
        #                 weight = 0.5) %>%
        #     addLegend(pal = pal, 
        #               values = ~warstwa[[variable_unit]], 
        #               opacity = 1, 
        #               title = variable_unit) %>%
        #     
        #     # addPolygons(fillColor = ~pal2(warstwa$Entropia_std),
        #     #             popup = popup,
        #     #             group = "Entropia_std",
        #     #             color = "#222222",
        #     #             fillOpacity = 0.8,
        #     #             weight = 0.5) %>%
        #     # addLegend(pal = pal2, values = ~Entropia_std, opacity = 1) %>%
        #   addLayersControl(
        #     baseGroups = "OSM (default)",
        #     overlayGroups = c("Entropia_std", "H"),
        #     options = layersControlOptions(collapsed = FALSE))
        #   map 
          
          
         # a <- tm_shape(warstwa()) + tm_polygons(col = c("Entropia_std", "H"),
         #                                        palette = "YlGn",
         #                                        popup.vars = text) + tm_facets(as.layers = TRUE) + tm_view(set.view = c(-120, 50, 3.2))
         # 
         # tmap_leaflet(a,
          #             mode = "view",
          #             in.shiny = TRUE)
          
          
          else if (input$indicator_button == 1){
            if (input$index == "Entropy"){
              
              
              tmap_mode("view")
              warstwa <- warstwa()
              var <- switch(input$variable_index, 
                                       "Entropy    |ALL AGGREGATION UNITS" = "Entropy",
                                       "Entropy std    |ALL AGGREGATION UNITS" = "Entropy_std")
              popup <- c("Entropy: " = "Entropy", "Entropy std: " = "Entropy_std")
              tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                tm_fill(col = var, 
                        palette = "YlGn",
                        id = "NAMELSAD",
                        popup.vars = popup) + tm_borders()
            }
            
            
            else if (input$index == "Index of dissimilarity"){
              tmap_mode("view")
              warstwa <- warstwa()
              var <- switch(input$variable_index, 
                            "white-black    |BLOCKS" = "D_wb_block",
                            "white-asian    |BLOCKS" = "D_wa_block",
                            "white-latin    |BLOCKS" = "D_wl_block",
                            "black-latin    |BLOCKS" = "D_bl_block",
                            "black-asian    |BLOCKS" = "D_ba_block",
                            "latin-asian    |BLOCKS" = "D_la_block",
                            "white-black    |GROUP OF BLOCKS" = "D_wb_group_blocks",
                            "white-asian    |GROUP OF BLOCKS" = "D_wa_group_blocks",
                            "white-latin    |GROUP OF BLOCKS" = "D_wl_group_blocks",
                            "black-latin    |GROUP OF BLOCKS" = "D_bl_group_blocks",
                            "black-asian    |GROUP OF BLOCKS" = "D_ba_group_blocks",
                            "latin-asian    |GROUP OF BLOCKS" = "D_la_group_blocks",
                            "white-black    |TRACT" = "D_wb_tract",
                            "white-asian    |TRACT" = "D_wa_tract",
                            "white-latin    |TRACT" = "D_wl_tract",
                            "black-latin    |TRACT" = "D_bl_tract",
                            "black-asian    |TRACT" = "D_ba_tract",
                            "latin-asian    |TRACT" = "D_la_tract")
              popup <- c("white-black    |BLOCKS: " = "D_wb_block",
                         "white-asian    |BLOCKS: " = "D_wa_block",
                         "white-latin    |BLOCKS: " = "D_wl_block",
                         "black-latin    |BLOCKS: " = "D_bl_block",
                         "black-asian    |BLOCKS: " = "D_ba_block",
                         "latin-asian    |BLOCKS: " = "D_la_block",
                         "white-black    |GROUP OF BLOCKS: " = "D_wb_group_blocks",
                         "white-asian    |GROUP OF BLOCKS: " = "D_wa_group_blocks",
                         "white-latin    |GROUP OF BLOCKS: " = "D_wl_group_blocks",
                         "black-latin    |GROUP OF BLOCKS: " = "D_bl_group_blocks",
                         "black-asian    |GROUP OF BLOCKS: " = "D_ba_group_blocks",
                         "latin-asian    |GROUP OF BLOCKS: " = "D_la_group_blocks",
                         "white-black    |TRACT: " = "D_wb_tract",
                         "white-asian    |TRACT: " = "D_wa_tract",
                         "white-latin    |TRACT: " = "D_wl_tract",
                         "black-latin    |TRACT: " = "D_bl_tract",
                         "black-asian    |TRACT: " = "D_ba_tract",
                         "latin-asian    |TRACT: " = "D_la_tract")
              
              tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                tm_fill(col = var, 
                        palette = "YlGn",
                        id = "NAMELSAD",
                        popup.vars = popup) + tm_borders()
            }
            
            
            else if (input$index == "The information theory index H"){
              tmap_mode("view")
              warstwa <- warstwa()
              var <- switch(input$variable_index, 
                            "H    |BLOCKS" = "H_block",
                            "H    |GROUP OF BLOCKS" = "H_group_blocks",
                            "H    |TRACTS" = "H_tracts")
              popup <- c("Entropy: " = "Entropy", "Entropy std: " = "Entropy_std")
              
              tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                tm_fill(col = var, 
                        palette = "YlGn",
                        id = "NAMELSAD",
                        popup.vars = popup) + tm_borders()
            
            }
            }
        })
        
          
    
    output$plot <- renderPlotly({
        warstwa <- warstwa() 
        variable_unit <- as.character(input$index)
        
        g <- ggplot(warstwa, aes(warstwa[[variable_unit]])) + geom_histogram(bins = 80, fill = '#367d59') + theme(
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
        ggplotly(g)%>% config(displayModeBar = F)
    }) 
    
    output$table <- DT::renderDataTable(DT::datatable(warstwa(), options = list(
        rownames = FALSE,
        pageLength = 12,
        autoWidth = TRUE,
        columnDefs = list(list(visible=FALSE, targets= c(1, 2:5, 8:10, 20))),
        lengthMenu = c(6, 12, 18))))
    
    
    datasetInput <- reactive({
            switch(input$dataset,
           "Blocks - 1990" = shp_block_1990, "Blocks - 2000" = shp_block_2000, "Blocks - 2010" = shp_block_2010, "Blocks - 2020" = shp_block_2020,
           "Groups of blocks - 1990" = shp_grp_blocks_1990, "Groups of blocks - 2000" = shp_grp_blocks_2000, "Groups of blocks - 2010" = shp_grp_blocks_2010, "Groups of blocks - 2020" = shp_grp_blocks_2020,
           "Tracts - 1990" = shp_tract_1990, "Tracts - 2000" = shp_tract_2000, "Tracts - 2010" = shp_tract_2010, "Tracts - 2020" = shp_tract_2020)
    #dataset <- dataset %>% st_drop_geometry()
    })
    
    output$download_csv <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            datasetInput_1 <- datasetInput()
            datasetInput_1 <- datasetInput_1 %>% st_drop_geometry()
            write.csv(datasetInput_1, file, row.names = FALSE)
        }
    )
    
    output$download_gpkg <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".gpkg", sep = "")
        },
        content = function(file) {
            datasetInput <- datasetInput()
            st_write(datasetInput, file)
        }
    )
    
    
}


# Run the app ----
shinyApp(ui = ui, server = server)
