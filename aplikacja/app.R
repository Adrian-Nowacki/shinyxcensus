
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
                                                                               "Entropia_std",
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
                                            tabPanel("Interactive map", tmapOutput("map", height = "600px")), 
                                            tabPanel("Statistics", plotlyOutput("plot",  height = "600px")), 
                                            tabPanel("Table", DT::dataTableOutput("table"))
                                        )
                              )
                          )
                 ),
                 tabPanel("Download data",
                          sidebarLayout(
                              sidebarPanel(id = "dt_sidebar",
                                           checkboxInput("dataset_button_1", "Dataset divided into aggregation units:", value = 1),
                                           
                                           selectInput("dataset_1", "Choose a dataset:", 
                                                       choices = c("Blocks - 1990", "Blocks - 2000", "Blocks - 2010", "Blocks - 2020",
                                                                   "Groups of blocks - 1990", "Groups of blocks - 2000", "Groups of blocks - 2010", "Groups of blocks - 2020",
                                                                   "Tracts - 1990", "Tracts - 2000", "Tracts - 2010", "Tracts - 2020")),
                                           
                                           checkboxInput("dataset_button_2", "Dataset divided into individual indicators:", value = 0),
                                           
                                           selectInput("dataset_2", "Choose a dataset:", 
                                                       choices = c("Entropy - 1990", "Entropy - 2000", "Entropy - 2010", "Entropy - 2020",
                                                                   "Index of dissimilarity - 1990", "Index of dissimilarity - 2000",
                                                                   "Index of dissimilarity - 2010", "Index of dissimilarity - 2020",
                                                                   "The information theory index H - 1990", "The information theory index H - 2000", 
                                                                   "The information theory index H - 2010", "The information theory index H - 2020")),
                                           actionButton("choose", "Choose"),
                                           
                                           radioButtons("filetype", "File type:",
                                                        choices = c(".csv", ".shp", ".gpkg")),
                                           downloadButton('download_csv', 'Download .csv'),
                                           downloadButton('download_gpkg', 'Download .gpkg')
                              ),
                              mainPanel(id = "dt_mainpanel",
                                        tabsetPanel(
                                          tabPanel("Table", DT::dataTableOutput("table_dt", height = "600px"))
                                        )
                              )
                          )
                 ),
                 tabPanel("About",
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
                            background-color: #454545;
                            width:360px;
                            height:740px!important;
                            border:none!important;
                            border-right:1px solid #eeeeee!important;
                         }
                         .row {
                            margin-left:-30px!important;
                         }
                         .well {
                         margin-bottom:0px!important;
                         }
                         body {
                            overflow:hidden;
                         }
                        .navbar-default {
                            background-color:#477676!important;
                            color:#eeeeee!important;
                        }
                        .navbar-brand {
                            color:#eeeeee!important;
                            font-weight:bold!important;
                        }
                        .navbar .navbar-nav {
                            float: right;
                            background-color:#477676!important;
                        }
                        .navbar {
                            margin-bottom:0px!important;
                        }
                        .navbar-nav>li>a {
                            background-color:#477676!important;
                            color:#eeeeee!important;
                        }
                        .navbar-nav li a:hover, .navbar-nav > .active > a {
                            color: #eeeeee!important;
                            background-color:#3a5959 !important;
                            background-image: none !important;
                        }
                        #mainpanel {
                            margin-left:-100px;
                            width: 1050px;
                            color: #bbbbbb;
                        }
                        #dt_mainpanel{
                            margin-left:-100px;
                            width: 1050px;
                            color: #bbbbbb;
                        }
                        #dt_sidebar {
                            background-color: #454545;
                            width:360px;
                            height:740px!important;
                            border:none!important;
                            border-right:1px solid #eeeeee!important;
                        }
                        
                        #table {
                            color:#dddddd;
                        }
                        #table th{
                            color:#dddddd;
                            background-color:#477676;
                            border-right:1px solid #dddddd;
                        }
                        #table tr:nth-child(2n+1){
                            background-color:#cfe2e2;
                            color:#222222;
                        }
                        #table a{
                            color:#dddddd!important;
                            font-size: 0.4;
                        }
                        #table_dt{
                            color:#dddddd;
                        }
                        #table_dt th{
                            color:#dddddd;
                            background-color:#477676;
                            border-right:1px solid #dddddd;
                        }
                        #table_dt tr:nth-child(2n+1){
                            background-color:#cfe2e2;
                            color:#222222;
                        }
                        #table_dt a{
                            color:#dddddd!important;
                            font-size: 0.4;
                        }
                        
                        
                
                        body, label, input, button, select { 
                            color: #dddddd;
                            letter-spacing: -0.2px;
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
    
    
    
    ## conditions for buttons downloading datasets:
    observeEvent(input$dataset_button_1, {
      if(input$dataset_button_1 == 1){
        shinyjs::disable(id = "dataset_2")
        shinyjs::enable(id = "dataset_1")
        updateCheckboxInput(
          inputId = "dataset_button_2", 
          value = FALSE
        )} 
      else {
        shinyjs::enable(id = "dataset_2")
        shinyjs::disable(id = "dataset_1")
        updateCheckboxInput(
          inputId = "dataset_button_1",
          value = FALSE
        )}
    })
    
    observeEvent(input$dataset_button_2, {
      if(input$dataset_button_2 == 0){
        shinyjs::disable(id = "dataset_2")
        shinyjs::enable(id = "dataset_1")
        updateCheckboxInput(
          inputId = "dataset_button_2", 
          value = FALSE
        )} 
      else {
        shinyjs::enable(id = "dataset_2")
        shinyjs::disable(id = "dataset_1")
        updateCheckboxInput(
          inputId = "dataset_button_1",
          value = FALSE
        )}
    })
    observeEvent({input$dataset_button_1
      input$dataset_button_2}, {
        if(input$dataset_button_1 == 0 & input$dataset_button_2 ==0){
          shinyjs::disable(id = "dataset_2")
          shinyjs::disable(id = "dataset_1")}
      })
    
    
    
    warstwa <- eventReactive(input$run, {
      input$run
        aggr <- isolate(switch(input$unit, 
                       "Blocks" = aggr_objects[[1]],
                       "Groups of blocks" = aggr_objects[[2]],
                       "Tracts" = aggr_objects[[3]]))
        
        indexes <- isolate(switch(input$index, 
                          "Entropy" = ind_objects[[1]],
                          "Index of dissimilarity" = ind_objects[[2]],
                          "The information theory index H" = ind_objects[[3]]))
        
        #function(){
        if (input$aggr_button == 1){
          input$run
          isolate(switch(input$year, 
                   "1990" = aggr[[1]],
                   "2000" = aggr[[2]],
                   "2010" = aggr[[3]],
                   "2020" = aggr[[4]]))
        }
        else if (input$indicator_button == 1){
          input$run
            isolate(switch(input$year, 
                   "1990" = indexes[[1]],
                   "2000" = indexes[[2]],
                   "2010" = indexes[[3]],
                   "2020" = indexes[[4]]))
        }
        #}
    })
    
    observeEvent(input$index, {
    if (input$indicator_button == 1){
      
        if (input$index == "Entropy"){
          freezeReactiveValue(input, "variable_index")
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("Entropy    |ALL AGGREGATION UNITS",
                                        "Entropy std    |ALL AGGREGATION UNITS"),
                            selected = "Entropy    |ALL AGGREGATION UNITS"
          )}
        else if (input$index == "Index of dissimilarity"){
          freezeReactiveValue(input, "variable_index")
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
          freezeReactiveValue(input, "variable_index")
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
           if (isolate(input$aggr_button) == 1){
             input$run
             tmap_mode("view")
             warstwa <- warstwa()
             
             variable_unit <- isolate(input$variable_unit)
             
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
          
          else if (isolate(input$indicator_button) == 1){
            
            if (isolate(input$index) == "Entropy"){
              input$run
              
              tmap_mode("view")
              warstwa <- warstwa()
              var <- isolate(switch(input$variable_index, 
                                       "Entropy    |ALL AGGREGATION UNITS" = "Entropy",
                                       "Entropy std    |ALL AGGREGATION UNITS" = "Entropy_std"))
              popup <- c("Entropy: " = "Entropy", "Entropy std: " = "Entropy_std")
              
             
              tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                tm_fill(col = var, 
                        palette = "YlGn",
                        id = "NAMELSAD",
                        popup.vars = popup) + tm_borders()
            }
            
            
            else if (isolate(input$index) == "Index of dissimilarity"){
              input$run
              tmap_mode("view")
              warstwa <- warstwa()
              var <- isolate(switch(input$variable_index, 
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
                            "latin-asian    |TRACT" = "D_la_tract"))
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
            
            
            else if (isolate(input$index) == "The information theory index H"){
              input$run
              tmap_mode("view")
              warstwa <- warstwa()
              var <- isolate(switch(input$variable_index, 
                            "H    |BLOCKS" = "H_block",
                            "H    |GROUP OF BLOCKS" = "H_group_blocks",
                            "H    |TRACTS" = "H_tract"))
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
      
      plot_theme <- theme(
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
      
      warstwa <- warstwa() 
      
      if (isolate(input$aggr_button) == 1){
        
        var <- input$variable_unit
        
        g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
          plot_theme + labs(x = var, y = "Count", title = var)
        ggplotly(g)%>% config(displayModeBar = F)
        
      }
      else if (isolate(input$indicator_button) == 1){
        if (isolate(input$index) == "Entropy"){
          
          var <- isolate(switch(input$variable_index, 
                                "Entropy    |ALL AGGREGATION UNITS" = "Entropy",
                                "Entropy std    |ALL AGGREGATION UNITS" = "Entropy_std"))
          
          g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
            plot_theme + labs(x = var, y = "Count", title = var)
          ggplotly(g)%>% config(displayModeBar = F)
        }
        
        
        else if (isolate(input$index) == "Index of dissimilarity"){
          var <- isolate(switch(input$variable_index, 
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
                                "latin-asian    |TRACT" = "D_la_tract"))
          
          g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
            plot_theme + labs(x = var, y = "Count", title = var)
          ggplotly(g)%>% config(displayModeBar = F)
        }
        
        
        else if (isolate(input$index) == "The information theory index H"){
          
          var <- isolate(switch(input$variable_index, 
                                "H    |BLOCKS" = "H_block",
                                "H    |GROUP OF BLOCKS" = "H_group_blocks",
                                "H    |TRACTS" = "H_tract"))
          
          g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
            plot_theme + labs(x = var, y = "Count", title = var)
          ggplotly(g)%>% config(displayModeBar = F)

        }
      }
        }) 
    
    
    output$table <- DT::renderDataTable(DT::datatable(warstwa(), options = list(
        rownames = FALSE,
        pageLength = 12,
        autoWidth = TRUE,
        columnDefs = list(list(visible=FALSE, targets= c(1, 2:5, 8:10, 20))),
        lengthMenu = c(6, 12, 18))))
    
    
    datasetInput <- eventReactive(input$choose,{
           if (input$dataset_button_1 == 1){
            switch(input$dataset_1,
           "Blocks - 1990" = shp_block_1990, "Blocks - 2000" = shp_block_2000, "Blocks - 2010" = shp_block_2010, "Blocks - 2020" = shp_block_2020,
           "Groups of blocks - 1990" = shp_grp_blocks_1990, "Groups of blocks - 2000" = shp_grp_blocks_2000, "Groups of blocks - 2010" = shp_grp_blocks_2010, "Groups of blocks - 2020" = shp_grp_blocks_2020,
           "Tracts - 1990" = shp_tract_1990, "Tracts - 2000" = shp_tract_2000, "Tracts - 2010" = shp_tract_2010, "Tracts - 2020" = shp_tract_2020)
    #dataset <- dataset %>% st_drop_geometry()
      }
       else if (input$dataset_button_2 == 1){
        switch(input$dataset_2,
        "Entropy - 1990" = shp_ind_ent_1990, "Entropy - 2000" = shp_ind_ent_2000, "Entropy - 2010" = shp_ind_ent_2010, "Entropy - 2020" = shp_ind_ent_2020,
        "Index of dissimilarity - 1990"  = shp_ind_D_1990, "Index of dissimilarity - 2000" = shp_ind_D_2000,
        "Index of dissimilarity - 2010" = shp_ind_D_2010, "Index of dissimilarity - 2020" = shp_ind_D_2020,
        "The information theory index H - 1990" = shp_ind_H_1990, "The information theory index H - 2000" = shp_ind_H_2000, 
        "The information theory index H - 2010" = shp_ind_H_2010, "The information theory index H - 2020" = shp_ind_H_2020)
      }
      
    })
    output$download_csv <- downloadHandler(
      
        filename = function() {
          if (input$dataset_button_1 == 1){
            paste0(input$dataset_1, ".csv", sep = "")
          }
          else if (input$dataset_button_2 == 1){
            paste0(input$dataset_2, ".csv", sep = "")
          }
        },
        content = function(file) {
            datasetInput <- datasetInput() %>% st_drop_geometry()
            write.csv(datasetInput, file, row.names = FALSE)
        }
    )
    
    output$download_gpkg <- downloadHandler(
        filename = function() {
          if (input$dataset_button_1 == 1){
            paste0(input$dataset_1, ".gpkg", sep = "")
          }
         else if (input$dataset_button_2 == 1){
           paste0(input$dataset_2, ".gpkg", sep = "")
         }
        },
        content = function(file) {
            st_write(datasetInput(), file)
        }
    )
     dataset <- eventReactive(input$choose,{
       datasetInput() %>% st_drop_geometry()
     })
    
    output$table_dt <- DT::renderDataTable(DT::datatable(dataset(), options = list(
      rownames = FALSE,
      pageLength = 12,
      scrollX = TRUE,
      #autoWidth = TRUE,
     # columnDefs = list(list(visible=FALSE, targets= c(1, 2:5, 8:10, 20))),
      lengthMenu = c(6, 12))))
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        setView(lng = -98.583, lat = 39.833, zoom = 4)
      
      
    })
}


# Run the app ----
shinyApp(ui = ui, server = server)
