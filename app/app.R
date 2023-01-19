
library(webshot)
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
shp_block_1990 <- st_read("../counties_shp/low_res/census_shp/shp_block_1990.gpkg")
shp_block_2000 <- st_read("../counties_shp/low_res/census_shp/shp_block_2000.gpkg")
shp_block_2010 <- st_read("../counties_shp/low_res/census_shp/shp_block_2010.gpkg")
shp_block_2020 <- st_read("../counties_shp/low_res/census_shp/shp_block_2020.gpkg")

shp_grp_blocks_1990 <- st_read("../counties_shp/low_res/census_shp/shp_grp_blocks_1990.gpkg")
shp_grp_blocks_2000 <- st_read("../counties_shp/low_res/census_shp/shp_grp_blocks_2000.gpkg")
shp_grp_blocks_2010 <- st_read("../counties_shp/low_res/census_shp/shp_grp_blocks_2010.gpkg")
shp_grp_blocks_2020 <- st_read("../counties_shp/low_res/census_shp/shp_grp_blocks_2020.gpkg")

shp_tract_1990 <- st_read("../counties_shp/low_res/census_shp/shp_tract_1990.gpkg")
shp_tract_2000 <- st_read("../counties_shp/low_res/census_shp/shp_tract_2000.gpkg")
shp_tract_2010 <- st_read("../counties_shp/low_res/census_shp/shp_tract_2010.gpkg")
shp_tract_2020 <- st_read("../counties_shp/low_res/census_shp/shp_tract_2020.gpkg")


blocks <- list(shp_block_1990, shp_block_2000, shp_block_2010, shp_block_2020)
grp_blocks <- list(shp_grp_blocks_1990, shp_grp_blocks_2000, shp_grp_blocks_2010, shp_grp_blocks_2020)
tracts <- list(shp_tract_1990, shp_tract_2000, shp_tract_2010, shp_tract_2020)

aggr_objects <- list(blocks = blocks, grp_blocks =  grp_blocks, tracts = tracts)
# wskazniki
shp_ind_ent_1990 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_ent_1990.gpkg")
shp_ind_ent_2000 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_ent_2000.gpkg")
shp_ind_ent_2010 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_ent_2010.gpkg")
shp_ind_ent_2020 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_ent_2020.gpkg")

shp_ind_D_1990 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_D_1990.gpkg")
shp_ind_D_2000 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_D_2000.gpkg")
shp_ind_D_2010 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_D_2010.gpkg")
shp_ind_D_2020 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_D_2020.gpkg")

shp_ind_H_1990 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_H_1990.gpkg")
shp_ind_H_2000 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_H_2000.gpkg")
shp_ind_H_2010 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_H_2010.gpkg")
shp_ind_H_2020 <- st_read("../counties_shp/low_res/indexes_shp/shp_ind_H_2020.gpkg")

entrophy <- list(shp_ind_ent_1990, shp_ind_ent_2000, shp_ind_ent_2010, shp_ind_ent_2020)
d_index <- list(shp_ind_D_1990, shp_ind_D_2000, shp_ind_D_2010, shp_ind_D_2020)
h_index <- list(shp_ind_H_1990 , shp_ind_H_2000 , shp_ind_H_2010 , shp_ind_H_2020 )

ind_objects <- list(entrophy = entrophy, d_index = d_index, h_index = h_index)
# Define UI ----



ui <- navbarPage(id = "navbar",
                   title=div(img(src='logo_5.png', width = "32px", height = "32px"), "🇸‌🇭‌🇮‌🇳‌🇾‌🇽‌🇨‌🇪‌🇳‌🇸‌🇺‌🇸‌", style = "margin-top:-2px;font-weight:bold!important; text-shadow: #222 1px 1px 1px;"),
                 #"Racial diversity App",
                 tabPanel("Home",
                          fluidRow(style = "
                          background: 
                            radial-gradient(ellipse at top, #477676, transparent),
                            radial-gradient(ellipse at bottom, #2a3c3c, transparent);",
                                   
                                   column(2),
                                   column(8,
                                          img(src='logo_5.png', width = "150px", height = "150px", align = "center", style = "margin-top:150px;"),
                                          style = "height:800px!important; text-align:center; font-family: Trebuchet MS; color: #DDD;",
                                          h2("🆂🅷🅸🅽🆈🆇🅲🅴🅽🆂🆄🆂",
                                             style = "font-weight:bold!important; text-shadow: #222 1px 1px 1px;"),
                                          h4("Application visualizing the values of racial-ethnic segregation and differentiation indicators for individual sizes of aggregation units", 
                                             style = "text-shadow: #222 1px 1px 1px; margin-top:50px;"
                                          ),
                                          
                                   ),
                                   column(2)
                          )
                          
                          
                 ),
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
                                                                   choices = c("Entropy", 
                                                                               "Entropy std",
                                                                               "The information theory index H", 
                                                                               "Index of dissimilarity (white-black)",
                                                                               "Index of dissimilarity (white-asian)", 
                                                                               "Index of dissimilarity (white-latin)",
                                                                               "Index of dissimilarity (black-latin)", 
                                                                               "Index of dissimilarity (black-asian)",
                                                                               "Index of dissimilarity (latin-asian)"),
                                                                   selected = "Entropy"),
                                                       
                                                       
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
                                        tabsetPanel(id = "tabsetpanel",
                                            tabPanel("Interactive map", tmapOutput("map", height = "600px"),
                                                     fluidRow(id = "instruction_1",
                                                              style = "margin-top: -570px; font-size:16px; color:#ffffff",
                                                              h3("On the left, there is a side panel divided into two sections:"),
                                                              p("- the first one allows you to select a layer covering", span("all indicators for the selected aggregation unit", style = "font-weight:bold; color:#5f8484")),
                                                              p("- the second one allows you to select", span("one indicator for all aggregation units", style = "font-weight:bold; color:#5f8484")),
                                                              br(),
                                                              p("1. Select the indicator and year you are interested in"),
                                                              p("2. Click 'Run'"), 
                                                              br(),
                                                              p("For more information on aggregation units and indices of racial differentiation and racial segregation, see the 'About' tab", style = "color:#cccccc; font-size:12px"))),
                                            tabPanel("Statistics", 
                                                     fluidRow(style = "margin-top: 30px; text-size: 18px;text-align: center; color: #eeeeee; background-color:#444444;",
                                                              column(4,
                                                                     textOutput("text_mean"),
                                                                     textOutput("text_sd")),
                                                              column(4,
                                                                     textOutput("text_min"),
                                                                     textOutput("text_max")),
                                                              column(4,
                                                                     textOutput("text_median"),
                                                                     textOutput("text_quantile"))),
                                                     plotlyOutput("plot", height = "480px", width = "770px"),
                                                     # fluidRow(id = "instruction_2",
                                                     #          style = "margin-top: -445px; font-size:16px; color:#ffffff",
                                                     #          h3("On the left, there is a side panel divided into two sections:"),
                                                     #          p("- the first one allows you to select a layer covering", span("all indicators for the selected aggregation unit", style = "font-weight:bold; color:#5f8484")),
                                                     #          p("- the second one allows you to select", span("one indicator for all aggregation units", style = "font-weight:bold; color:#5f8484")),
                                                     #          br(),
                                                     #          p("1. Select the indicator and year you are interested in"),
                                                     #          p("2. Click 'Run'"), 
                                                     #          br(),
                                                     #          p("For more information on aggregation units and indices of racial differentiation and racial segregation, see the 'About' tab", style = "color:#cccccc; font-size:12px")),
                                                     downloadButton('save_png', 'Save as .png',
                                                                    style = "visibility:hidden"))
                                            
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
                                           div(style = "clear:both;"),
                                           downloadButton('download_csv', 'Download .csv'),
                                           downloadButton('download_gpkg', 'Download .gpkg')
                              ),
                              mainPanel(id = "dt_mainpanel",
                                        tabsetPanel(
                                          tabPanel("Table", DT::dataTableOutput("table_dt", height = "600px"),
                                                   fluidRow(id = "instruction_3",
                                                            style = "margin-top: -570px; font-size:16px; color:#ffffff",
                                                            h3("On the left, there is a side panel divided into two datasets:"),
                                                            p("- the first one allows you to show and download a table covering", span("all indicators for the selected aggregation unit", style = "font-weight:bold; color:#5f8484")),
                                                            p("- the second one allows you to show and download a table covering", span("one indicator for all aggregation units", style = "font-weight:bold; color:#5f8484")),
                                                            br(),
                                                            p("1. Select the layer you are interested in"),
                                                            p("2. Click 'Show'"), 
                                                            br(),
                                                            ))
                                        )
                              )
                          )
                 ),
                 tabPanel("About",
                          fluidRow(style = "
                          background: 
                            radial-gradient(ellipse at top, #477676, transparent),
                            radial-gradient(ellipse at bottom, #2a3c3c, transparent);",
                                   column(1),
                                   column(10,
                                     style = "margin-top:30px;height:1200px!important; font-size:16px; text-align:justify; font-family: Trebuchet MS; color: #DDD;",
                                     p("The aim of the work was to create and make available a database containing segregation 
                                       indicators calculated for each county in the USA for the years: 1990, 2000, 2010 and 2020."),
                                     br(),
                                     p("These indicators were calculated for 3 levels of aggregation: census blocks, groups of blocks
                                       and census areas. The application allows you to view county spatial data combined with the resulting 
                                       tabular data. It enables visualization of data in the form of an interactive map, interactive charts 
                                       and a table  containing all calculated indicator values for each county. The application also allows you 
                                       to download shared data in tabular and spatial form."),
                                     br(), br(),
                                     p("1. Measures of racial differentiation", style = "color:#ffffff; font-weight:bold;"),
                                     p("These measures are used to determine the level of racial and ethnic heterogeneity of the population structure in the analyzed area. 
                                       The values of racial differentiation measures are independent of the adopted division into spatial units and have the same value for all 
                                       aggregation units in a given year. The most commonly used measures are:"),
                                         column(1),
                                         column(11,
                                         p("• ", span("Entropy", style = "font-weight:bold; color:#111111;"), " - informs about the level of racial-ethnic mixing of the population structure in the study area. Its maximum value depends on the number of racial and ethnic groups inhabiting a given area - the more groups considered, the higher the maximum value of entropy. Due to the reclassification of data to 6 categories, the calculated measure assumes the highest possible value of 1.79. The lower the entropy index, the greater the dominance of a single racial-ethnic group - the higher it is, the more equal the share of all groups."),
                                         p("• ", span("Standardized entropy", style = "font-weight:bold; color:#111111;"), "– a measure used for comparative purposes, with an adopted unified range of values ​​from 0 to 1, which is obtained as a result of dividing by the maximum entropy value."),
                                         br(),br()
                                         ),
                                     p("2. Indicators of racial segregation", style = "color:#ffffff; font-weight:bold;"),
                                     p("These measures measure the extent to which two or more groups live separately from each other in different parts of the city (Massey & Denton, 1988). They describe the level of spatial distribution of particular races and ethnic groups in a given geographical area. The values ​​of segregation indicators - in contrast to racial differentiation indicators - depend on the adopted division into aggregation units and require dividing the area into smaller component areas. Commonly used indicators of racial segregation are:"),
                                         column(1),
                                         column(11,
                                                p("• ", span("Index of dissimilarity", style = "font-weight:bold; color:#111111;"), "– used to measure the integration or relative separation of two groups within neighborhoods of a single city or metropolitan area, as well as within individual constituent census areas. In the work, the index was presented in relation to counties and calculated on the basis of the blocks, groups of blocks and census areas included in it. The index takes values ​​from 0 to 1, where 0 means complete integration (even distribution) of both groups, and 1 – complete segregation, i.e. a given spatial unit is inhabited by only one group. It tells you the percentage of the group that would need to move between units to equalize the size of the other group (i.e. both subgroups would be fully integrated)."),
                                                p("• ", span("Information theory index H", style = "font-weight:bold; color:#111111;"), "measures the difference between the entropy (racial diversity) of the entire area and the entropy of individual census areas in relation to the diversity of the entire area (Fig. 2). Its range is from 0 to 1, where 0 means the same racial and ethnic structure of the census area as compared to the entire area, i.e. full integration of all groups - while the value of 1 indicates that the census area is inhabited by one racial and ethnic group (full segregation)."),
                                         br(),br()
                                                ),
                                     tags$a(img(src='github-sign.png', width = "60px", height = "60px", align = "center", style = "margin-left: 47.5%; 
                                         margin-top:100px; margin-bottom:25px;"), href="https://github.com/Adrian-Nowacki/praca-inzynierska"),
                                     p("Adrian Nowacki © 2022",
                                       style = "font-size:16px; text-align:center;"),
                                     
                              ),
                                   column(1),
                          ),
                          
                          
                 ),
                 
                 setBackgroundColor(
                     color =  "#252525"
                 ),
                 
                 tags$head(tags$style(
                     HTML('
                         #sidebar {
                            background-color: #2f5151;
                            width:360px;
                            height:740px!important;
                            border:none!important;
                            border-right:1px solid #eeeeee!important;
                            color:#ffffff!important;
                         }
                         .leaflet-popup-content-wrapper{
                            font-family: Lucida Console;
                         }
                         
                         .row {
                            margin-left:-30px!important;
                         }
                         .well {
                         margin-bottom:0px!important;
                         }
                         body {
                            
                            radial-gradient(ellipse at top, #477676, transparent),
                            radial-gradient(ellipse at bottom, #2a3c3c, transparent);
                         }
                         #plot{
                         margin-left:auto;
                         margin-right:auto;
                         margin-top:50px;
                         }
                         
                        .navbar-default {
                            background-color:#477676!important;
                            color:#ffffff!important;
                        }
                        .navbar-brand {
                            color:#ffffff!important;
                            font-weight:bold!important;
                            font-size:20px!important;
                            line-height: 30px!important;
                        }
                        .navbar .navbar-nav {
                            float: right;
                            background-color:#477676!important;
                        }
                        .navbar {
                            margin-bottom:0px!important;
                            height: 60px!important;
                        }
                        .navbar-nav>li>a {
                            background-color:#477676!important;
                            color:#eeeeee!important;
                            height: 59px!important;
                            line-height: 30px!important;
                        }
                        .navbar-nav li a:hover, .navbar-nav > .active > a {
                            color: #eeeeee!important;
                            background-color:#3a5959 !important;
                            background-image: none !important;
                        }
                        #mainpanel {
                            margin-left:-100px;
                            margin-top:20px;
                            width: 1100px;
                            color: #bbbbbb;
                        }
                        #tabsetpanel a {
                            color:#ffffff;
                            background-color:#444444;
                        }
                        #tabsetpanel a:hover,  #tabsetpanel a:active, #tabsetpanel a:focus{
                            color:#ffffff;
                            background-color:#777777;
                        }
                       
                        #dt_mainpanel{
                            margin-left:-100px;
                            width: 1050px;
                            color: #bbbbbb;
                        }
                        #dt_sidebar {
                            background-color: #2f5151;
                            width:360px;
                            color:#ffffff!important;
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
                        #choose {
                        float:left;
                        margin-bottom:40px;
                        }
                        #download_csv {
                        float:left;
                        }
                        #download_gpkg {
                        float:right;
                        }

                        body, label, input, button, select { 
                            
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
             
             var <- isolate(switch(input$variable_unit, 
                                   "Entropy" = "Entropia", 
                                   "Entropy std" = "Entropia_std",
                                   "The information theory index H" = "H", 
                                   "Index of dissimilarity (white-black)" = "D_wb",
                                   "Index of dissimilarity (white-asian)" = "D_wa", 
                                   "Index of dissimilarity (white-latin)" = "D_wl",
                                   "Index of dissimilarity (black-latin)" = "D_bl", 
                                   "Index of dissimilarity (black-asian)" = "D_ba",
                                   "Index of dissimilarity (latin-asian)" = "D_la"))
             
             popup <- c("Entropia: " = "Entropia", "Entropia std: " = "Entropia_std", 
                        "H: " = "H", "D (white-black)" = "D_wb", "D (white-asian)" = "D_wa", 
                        "D (white-latin)" = "D_wl", "D (black-latin)" = "D_bl", 
                        "D (black-asian)" = "D_ba", "D (latin-asian)" = "D_la")
             
               tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                 tm_fill(col = var, 
                         palette = "YlGn",
                         id = "NAME",
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
                        id = "NAME",
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
                        id = "<span style = NAME",
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
              popup <- c("H    |BLOCKS" = "H_block",
                         "H    |GROUP OF BLOCKS" = "H_group_blocks",
                         "H    |TRACTS" = "H_tract")
              
              
              tm_shape(warstwa) + tm_view(set.view = c(-120, 50, 3.2)) + 
                tm_fill(col = var, 
                        palette = "YlGn",
                        id = "NAME",
                        popup.vars = popup) + tm_borders()
              
            
            }
            }
        })
        
          
    
   # output$plot <- renderPlotly({
      plot <- reactive({
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
        
        
        var <- isolate(switch(input$variable_unit, 
                              "Entropy" = "Entropia", 
                              "Entropy std" = "Entropia_std",
                              "The information theory index H" = "H", 
                              "Index of dissimilarity (white-black)" = "D_wb",
                              "Index of dissimilarity (white-asian)" = "D_wa", 
                              "Index of dissimilarity (white-latin)" = "D_wl",
                              "Index of dissimilarity (black-latin)" = "D_bl", 
                              "Index of dissimilarity (black-asian)" = "D_ba",
                              "Index of dissimilarity (latin-asian)" = "D_la"))
        
        output$text_mean <- renderText({paste0("mean: ", round(mean(warstwa[[var]]), 2)) })
        output$text_sd <- renderText({paste0("sd: ", round(sd(warstwa[[var]]), 2)) })
        output$text_min <- renderText({paste0("min.: ", round(min(warstwa[[var]]), 2)) })
        output$text_max <- renderText({paste0("max.: ", round(max(warstwa[[var]]), 2)) })
        output$text_median <- renderText({paste0("median: ", round(median(warstwa[[var]]), 2)) })
        output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(warstwa[[var]], probs = 0.75), 2)) })
        
        g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
          plot_theme + labs(x = var, y = "Count", title = var)
        ggplotly(g)%>% config(displayModeBar = F)
        
        
        
      }
      else if (isolate(input$indicator_button) == 1){
        if (isolate(input$index) == "Entropy"){
          
          var <- isolate(switch(input$variable_index, 
                                "Entropy    |ALL AGGREGATION UNITS" = "Entropy",
                                "Entropy std    |ALL AGGREGATION UNITS" = "Entropy_std"))
          
          output$text_mean <- renderText({paste0("mean: ", round(mean(warstwa[[var]]), 2)) })
          output$text_sd <- renderText({paste0("sd: ", round(sd(warstwa[[var]]), 2)) })
          output$text_min <- renderText({paste0("min.: ", round(min(warstwa[[var]]), 2)) })
          output$text_max <- renderText({paste0("max.: ", round(max(warstwa[[var]]), 2)) })
          output$text_median <- renderText({paste0("median: ", round(median(warstwa[[var]]), 2)) })
          output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(warstwa[[var]], probs = 0.75), 2)) })
          
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
          
          output$text_mean <- renderText({paste0("mean: ", round(mean(warstwa[[var]]), 2)) })
          output$text_sd <- renderText({paste0("sd: ", round(sd(warstwa[[var]]), 2)) })
          output$text_min <- renderText({paste0("min.: ", round(min(warstwa[[var]]), 2)) })
          output$text_max <- renderText({paste0("max.: ", round(max(warstwa[[var]]), 2)) })
          output$text_median <- renderText({paste0("median: ", round(median(warstwa[[var]]), 2)) })
          output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(warstwa[[var]], probs = 0.75), 2)) })
          
          g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
            plot_theme + labs(x = var, y = "Count", title = var)
          ggplotly(g)%>% config(displayModeBar = F)
        }
        
        
        else if (isolate(input$index) == "The information theory index H"){
          
          var <- isolate(switch(input$variable_index, 
                                "H    |BLOCKS" = "H_block",
                                "H    |GROUP OF BLOCKS" = "H_group_blocks",
                                "H    |TRACTS" = "H_tract"))
          
          output$text_mean <- renderText({paste0("mean: ", round(mean(warstwa[[var]]), 2)) })
          output$text_sd <- renderText({paste0("sd: ", round(sd(warstwa[[var]]), 2)) })
          output$text_min <- renderText({paste0("min.: ", round(min(warstwa[[var]]), 2)) })
          output$text_max <- renderText({paste0("max.: ", round(max(warstwa[[var]]), 2)) })
          output$text_median <- renderText({paste0("median: ", round(median(warstwa[[var]]), 2)) })
          output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(warstwa[[var]], probs = 0.75), 2)) })
          
          g <- ggplot(warstwa, aes(warstwa[[var]])) + geom_histogram(bins = 80, fill = '#367d59') + 
            plot_theme + labs(x = var, y = "Count", title = var)
          ggplotly(g)%>% config(displayModeBar = F)

        }
      }
        }) 
      
      output$plot <- renderPlotly({
        print(plot())
      })
      
    table_dataset_1<- eventReactive(input$run,{
      warstwa() %>% st_drop_geometry()
    })
    
    # output$table <- DT::renderDataTable(DT::datatable(table_dataset_1(), options = list(
    #     rownames = FALSE,
    #     pageLength = 12,
    #    # autoWidth = TRUE,
    #     scrollX = TRUE,
    #     #columnDefs = list(list(visible=FALSE, targets= c(1, 2:5, 8:10, 20))),
    #     lengthMenu = c(6, 12))))
    
    
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
    
    output$save_png<- downloadHandler(
      filename = function() {
        if (isolate(input$aggr_button) == 1){
          paste0(input$variable_unit, "_", input$year, ".png", sep = "")
        }
        else if (isolate(input$indicator_button) == 1){
          paste0(input$variable_index, "_", input$year, ".png", sep = "")
        }
      },
      content = function(file) {
        #ggsave(file, plot = plot(), device = "png")
        export(plot(), file=file)
      }
    )
     table_dataset_2 <- eventReactive(input$choose,{
       datasetInput() %>% st_drop_geometry()
     })
    
    output$table_dt <- DT::renderDataTable(DT::datatable(table_dataset_2(), options = list(
      rownames = FALSE,
      pageLength = 12,
      scrollX = TRUE,
      #autoWidth = TRUE,
     # columnDefs = list(list(visible=FALSE, targets= c(1, 2:5, 8:10, 20))),
      lengthMenu = c(6, 12))))
    observeEvent(input$run,{hide("instruction_1")})
    observeEvent(input$run,{hide("instruction_2")})
    observeEvent(input$run,{hide("instruction_3")})
    observeEvent(input$run,
                  runjs('document.getElementById("save_png").style.visibility = "visible";')
                 )
                 
}


# Run the app ----
shinyApp(ui, server)
#runApp()
