
#wczytanie pakietów
library(shiny)
library(dplyr)
library(sf)
library(tmap)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(plotly)
# wczytanie danych obejmujących wszystkie wskaźniki dla wybranej jednostki agregacji
shp_block_1990 <- st_read("data/census_shp/shp_block_1990.gpkg")
shp_block_2000 <- st_read("data/census_shp/shp_block_2000.gpkg")
shp_block_2010 <- st_read("data/census_shp/shp_block_2010.gpkg")
shp_block_2020 <- st_read("data/census_shp/shp_block_2020.gpkg")

shp_grp_blocks_1990 <- st_read("data/census_shp/shp_grp_blocks_1990.gpkg")
shp_grp_blocks_2000 <- st_read("data/census_shp/shp_grp_blocks_2000.gpkg")
shp_grp_blocks_2010 <- st_read("data/census_shp/shp_grp_blocks_2010.gpkg")
shp_grp_blocks_2020 <- st_read("data/census_shp/shp_grp_blocks_2020.gpkg")

shp_tract_1990 <- st_read("data/census_shp/shp_tract_1990.gpkg")
shp_tract_2000 <- st_read("data/census_shp/shp_tract_2000.gpkg")
shp_tract_2010 <- st_read("data/census_shp/shp_tract_2010.gpkg")
shp_tract_2020 <- st_read("data/census_shp/shp_tract_2020.gpkg")

# utworzenie list z obiektami
blocks <- list(shp_block_1990, shp_block_2000, shp_block_2010, shp_block_2020)
grp_blocks <- list(shp_grp_blocks_1990, shp_grp_blocks_2000, shp_grp_blocks_2010, shp_grp_blocks_2020)
tracts <- list(shp_tract_1990, shp_tract_2000, shp_tract_2010, shp_tract_2020)

aggr_objects <- list(blocks = blocks, grp_blocks =  grp_blocks, tracts = tracts)

# wczytanie danych obejmujących pojedyncze wskaźniki dla wszystkich jednostek agregacji
shp_ind_ent_1990 <- st_read("data/indexes_shp/shp_ind_ent_1990.gpkg")
shp_ind_ent_2000 <- st_read("data/indexes_shp/shp_ind_ent_2000.gpkg")
shp_ind_ent_2010 <- st_read("data/indexes_shp/shp_ind_ent_2010.gpkg")
shp_ind_ent_2020 <- st_read("data/indexes_shp/shp_ind_ent_2020.gpkg")

shp_ind_D_1990 <- st_read("data/indexes_shp/shp_ind_D_1990.gpkg")
shp_ind_D_2000 <- st_read("data/indexes_shp/shp_ind_D_2000.gpkg")
shp_ind_D_2010 <- st_read("data/indexes_shp/shp_ind_D_2010.gpkg")
shp_ind_D_2020 <- st_read("data/indexes_shp/shp_ind_D_2020.gpkg")

shp_ind_H_1990 <- st_read("data/indexes_shp/shp_ind_H_1990.gpkg")
shp_ind_H_2000 <- st_read("data/indexes_shp/shp_ind_H_2000.gpkg")
shp_ind_H_2010 <- st_read("data/indexes_shp/shp_ind_H_2010.gpkg")
shp_ind_H_2020 <- st_read("data/indexes_shp/shp_ind_H_2020.gpkg")

#utworzenie list z obiektami
entrophy <- list(shp_ind_ent_1990, shp_ind_ent_2000, shp_ind_ent_2010, shp_ind_ent_2020)
d_index <- list(shp_ind_D_1990, shp_ind_D_2000, shp_ind_D_2010, shp_ind_D_2020)
h_index <- list(shp_ind_H_1990 , shp_ind_H_2000 , shp_ind_H_2010 , shp_ind_H_2020 )

ind_objects <- list(entrophy = entrophy, d_index = d_index, h_index = h_index)


# zdefinowanie wyglądu oraz struktury aplikacji
ui <- navbarPage(id = "navbar",
                 title = div(img(src='logo_5.png', width = "32px", height = "32px"), "shinyxcensus", #tutył aplikacji w navibarze
                             style = "margin-top:-2px;font-weight:bold!important; text-shadow: #222 1px 1px 1px; font-family: Candara; font-size:18px"),
                
                 
                 windowTitle = HTML("shinyxcensus</title><link rel='icon' type='png' href='logo_5.png'>"), #tytuł aplikacji na zakładce
                  
                 # zdefiniowanie głównych zakładek 
                 tabPanel("Home",
                          fluidRow(style = "
                          background: 
                            radial-gradient(ellipse at top, #477676, transparent),
                            radial-gradient(ellipse at bottom, #2a3c3c, transparent);
                            height:94vh;
                            overflow-y:hidden!important;",
                                   
                                   column(2),
                                   column(8,
                                          img(src='logo_5.png', width = "150px", height = "150px", align = "center", style = "margin-top:150px;"),
                                          style = "height:94vh!important; text-align:center; font-family: Trebuchet MS; color: #DDD;",
                                          h2("🆂🅷🅸🅽🆈🆇🅲🅴🅽🆂🆄🆂",
                                             style = "font-weight:bold!important; text-shadow: #222 1px 1px 1px;"),
                                          h4("App for comparing racial segregation and divirsity measures for different sizes of aggregation units", 
                                             style = "text-shadow: #222 1px 1px 1px; margin-top:50px;"
                                          ),
                                   ),
                                   column(2)
                          )
                 ),
                 
                 # zdefiniowanie głównych zakładek - 2. Mapa interaktywna
                 tabPanel("Interactive map",
                          sidebarLayout(
                            # panel wyboru warstwy do wizualizacji
                              sidebarPanel(id = "sidebar",
                                           tabsetPanel(id = "tabset",
                                                       # wybór pliku zawierającego wszystkie wskaźniki dla jednej jednostki agregacji
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
                                                       
                                                       # wybór pliku zawierającego jeden wskaźnik dla wszystkich jednostek agregacji
                                                       selectInput("index", 
                                                                   label = "Choose an index",
                                                                   choices = c("Entropy",
                                                                               "Index of dissimilarity", 
                                                                               "The information theory index H"),
                                                                   selected = "Entrophy"),
                                                       selectInput("variable_index", 
                                                                   label = "Choose an aggregation unit and variable",
                                                                   choices = c("Entropy |ALL AGGREGATION UNITS", 
                                                                               "Entropy std |ALL AGGREGATION UNITS"),
                                                                   selected = "Entropy")),
                                           
                                           # wybór roku
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
                              
                              # panel wizualizacji mapy interaktywnej
                              mainPanel(id = "mainpanel",
                                        tabsetPanel(id = "tabsetpanel",
                                            
                                              tabPanel("Interactive map", 
                                                       tmapOutput("map", height = "600px"), # panel z mapą interaktywną
                                                     fluidRow(id = "instruction_1", # instrukcja użycia, gdy mapa jest nieaktywna
                                                              style = "margin-top: -570px; font-size:16px; color:#ffffff",
                                                              h3("Side panel is divided into two sections:"),
                                                              p("- the first one allows you to select a layer covering", span("all indicators for the selected aggregation unit", style = "font-weight:bold; color:#5f8484")),
                                                              p("- the second one allows you to select", span("one indicator for all aggregation units", style = "font-weight:bold; color:#5f8484")),
                                                              br(),
                                                              p("1. Select the indicator and year you are interested in"),
                                                              p("2. Click 'Run'"), 
                                                              br(),
                                                              p("For more information on aggregation units and indices of racial differentiation and racial segregation, see the 'About' tab", style = "color:#cccccc; font-size:12px"))),
                                           
                                             tabPanel("Statistics", # panel ze statystykami oraz histogramem
                                                     fluidRow(style = "margin-top: 30px; font-size: 16px;text-align: center; color: #eeeeee; background-color:#444444;",
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
                                                                    style = "visibility:hidden")),
                                            tabPanel("Scatter plot", # panel z wykresem rozrzutu
                                                     fluidRow(
                                                       column(8,style = "margin-top: 25px; font-size: 18px;text-align: center; color: #eeeeee;",
                                                              p("compare the value of the selected indicator with another aggregation unit for the same year:")
                                                              ),
                                                       column(2,
                                                              selectInput("scatter_input", 
                                                                                 label = "",
                                                                                 choices = c("block",
                                                                                             "group_blocks",
                                                                                             "tract"),
                                                                                 selected = "tract")
                                                            ),
                                                       column(2)),
                                                     
                                                     plotlyOutput("scatter_plot", height = "480px", width = "770px"),
                                                     downloadButton('save_scatter_png', 'Save as .png',
                                                                    style = "visibility:hidden"))
                                                )
                                          )
                                    )
                              ),
                 
                 # zdefiniowanie głównych zakładek - 3. Pobieranie danych
                 tabPanel("Download data",
                          sidebarLayout(
                              sidebarPanel(id = "dt_sidebar", # panel z wyborem warstwy do wyświetlenia oraz pobrania
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
                                           actionButton("show", "Show"),
                                           div(style = "clear:both;"),
                                           downloadButton('download_csv', 'Download .csv'),
                                           downloadButton('download_gpkg', 'Download .gpkg')
                              ),
                              mainPanel(id = "dt_mainpanel", # panel główny ukazujący podgląd tabeli
                                        tabsetPanel(id = "tabsetpanel_dt",
                                          tabPanel("Table", DT::dataTableOutput("table_dt", height = "600px"),
                                                   fluidRow(id = "instruction_3",
                                                            style = "margin-top: -570px; font-size:16px; color:#ffffff",
                                                            h3("Side panel is divided into two datasets:"),
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
                 
                 # zdefiniowanie głównych zakładek - 4. O projekcie
                 tabPanel("About",
                          fluidRow(style = "
                          background: 
                            radial-gradient(ellipse at top, #477676, transparent),
                            radial-gradient(ellipse at bottom, #2a3c3c, transparent);",
                                   column(1),
                                   column(10,
                                     style = "margin-top:30px;height:1300px!important; font-size:16px; text-align:justify; font-family: Trebuchet MS; color: #DDD;",
                                     p("The aim of the app was to visualize and share a database containing racial segregation and diversity metrics calculated for each county in the USA for the years: 1990, 2000, 2010 and 2020."),
                                     br(),
                                     p("These metrics were calculated for 3 levels of aggregation units: census blocks, blocks groups and census trants. The app allows to view county spatial data combined with the resulting tabular data. It enables visualization of data in the form of an interactive map, interactive charts and a table. This app also allows to download resulted data in tabular and spatial form."),
                                     br(), br(),
                                     p("1. Measures of racial diversity", style = "color:#ffffff; font-weight:bold;"),
                                     p("These measures are used to determine the level of ethnoracial heterogeneity of the population in the analyzed area. The values of racial diversity measures are independent of the adopted division into spatial units and have the same value for all aggregation units in a given year. The most commonly used measures are:"),
                                         column(1),
                                         column(11,
                                         p("• ", span("Entropy", style = "font-weight:bold; color:#111111;"), " - informs about the level of ethnoracial mixing of the population in the study area. Its maximum value depends on the number of ethnoracial groups lving in a given area (the more groups considered, the higher the maximum value of entropy). The app uses data reclassified to 6 categories, that gives the highest possible value of 1.79. The lowest value of entropy corresponds to the area dominated by one ethnoracial group. The highest value of entropy corresponds to equal share of all groups. "),
                                         p("• ", span("Standardized entropy", style = "font-weight:bold; color:#111111;"), "– a measure used for comparative purposes, with range of values ​​from 0 to 1, which is obtained as a result of dividing entropy by the maximum entropy value."),
                                         br(),br()
                                         ),
                                     p("2. Measures of racial segregation", style = "color:#ffffff; font-weight:bold;"),
                                     p("These metrics measure the extent to which two or more groups live separately from each other in different parts of the city (Massey & Denton, 1988). They describe the level of spatial distribution of particular ethnoracial groups in a given geographical area. Calculating segregation metrics require dividing the area into smaller component areas. The values ​​of segregation metrics - in contrast to racial diversity metrics - depend on the adopted division into aggregation units. Commonly used racial segregation metrics are:"),
                                         column(1),
                                         column(11,
                                                p("• ", span("Index of dissimilarity (D)", style = "font-weight:bold; color:#111111;"), "– measures the integration or relative separation of two groups within neighborhoods of a single area (city, metropolitan area, county). This app presents the value of D in the county and were calculated dividing county into blocks, blocks group and census tracts. The index of dissimilarity vary ​from 0 to 1, where 0 means complete integration (even distribution) of both groups, and 1 – complete segregation, i.e. a given spatial unit is inhabited by only one group. Index of dissimilarity is interpreted in terms of the percentage of the group that would need to move between units to equalize the size of the other group (i.e. both subgroups would be fully integrated)."),
                                                p("• ", span("Information theory index (H)", style = "font-weight:bold; color:#111111;"), "- measures the difference between the entropy (racial diversity) of the entire area and the entropy of individual census areas in relation to the diversity of the entire area. Its range is from 0 to 1, where 0 means the same ethoracial composition of the census area as compared to the entire area, i.e. full integration of all groups - while the value of 1 indicates that the census area is inhabited by one ethnoracial group (full segregation)."),
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
                 
                 # ustawienie stylu dla poszczególnych elementów aplikacji
                 tags$head(tags$style(
                     HTML('
                         #sidebar {
                              background-color: #2f5151;
                              width:20vw;
                              min-width:250px!important;
                              height:94vh;
                              border:none!important;
                              border-right:1px solid #eeeeee!important;
                              color:#ffffff!important;
                         }
                         .leaflet-popup-content-wrapper{
                              font-family: Lucida Console;
                              background-color:#477676;
                              color:#ffffff;
                              border:0.5px solid #222222;
                              max-height:250px;
                         }
                         .leaflet-popup-content{
                              max-height:200px;
                         }
                         #map{
                              border:1px solid #ffffff;
                         }
                         .leaflet-popup-tip{
                              background-color:#477676;
                              border:0.5px solid #222222;
                         }
                         .leaflet-popup-content-wrapper tr td:nth-child(2n+1){
                              color:#cccccc!important;
                              padding-right:10px!important;
                         }
                         .leaflet-popup-content-wrapper thead{
                              font-size:18px!important;
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
                              font-size:calc(12px + 0.1vw);
                         }
                         #plot{
                              margin-left:auto;
                              margin-right:auto;
                              margin-top:30px;
                         }
                         #scatter_plot{
                              margin-top: 30px; 
                              margin-left:auto;
                              margin-right:auto;
                              text-align: center; 
                         }
                         #scatter_input{
                              text-align:center;
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
                              margin-top:20px;
                              min-width:900px!important;
                              width: 70vw;
                              color: #bbbbbb;
                              margin-left:-100px;
                         }
                         #dt_mainpanel{
                              margin-top:20px;
                              width: 1100px;
                              color: #bbbbbb;
                         }
                         #tabsetpanel a, #tabsetpanel_dt a{
                              color:#ffffff;
                              background-color:#444444;
                         }
                         #tabsetpanel a:hover,  #tabsetpanel a:active, #tabsetpanel a:focus, 
                         #tabsetpanel_dt a:hover, #tabsetpanel_dt a:active, #tabsetpanel_dt a:focus{
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
                              width:20vw;
                              min-width:250px!important;
                              color:#ffffff!important;
                              height:94vh;
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
                              color:#dddddd!important;
                         }
                         #table_dt label{
                              color:#dddddd!important;
                         }
                         .dataTables_info{
                              color:#dddddd!important;
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
                         #show {
                              float:left;
                              margin-bottom:40px;
                         }
                         #download_csv {
                              float:left;
                              width:45%;
                              font-size:calc(9px + 0.1vw);
                         }
                         #download_gpkg {
                              float:right;
                              width:45%;
                              font-size:calc(9px + 0.1vw);
                         }
                         body, label, input, button, select { 
                              letter-spacing: -0.2px;
                         }
                         
                        
                         @media screen and (max-width: 600px){
                               
                               #sidebar {
                                     width:100vw;
                                     border-right:none!important;
                                     border-bottom:1px solid #eeeeee!important;
                                     height:100%;
                               }
                               #dt_sidebar {
                                     width:100vw;
                                     border-right:none!important;
                                     border-bottom:1px solid #eeeeee!important;
                                     height:30em!important;
                               }
                               #mainpanel, #dt_mainpanel {
                                     width:100vw!important;
                                     margin-left:8px!important;
                                     min-width:0!important;
                                      overflow-x: scroll!important;
                               }
                               .nav {
                                     margin-bottom:10px!important;
                               }
                               #instruction_1, #instruction_3{
                                     width:100vw!important;
                                     padding-left:25px;
                                     padding-right:15px;
                                     overflow-x: hidden!important; 
                               }
                               .col-sm-8 {
                                     padding-right:0px!important;
                               }
                               .leaflet-control{
                                     margin:0px!important;
                               }
                               .legend, .leaflet-control-zoom, .leaflet-control-layers {
                                     transform:scale(0.8);
                               }
                               .leaflet-popup {
                                     font-size: 85%;
                               }
                               #scatter_plot, #plot {
                                     overflow-x: scroll!important;
                                     width:150vw!important;
                                     height:calc(150vw/1.6)!important;
                               }
                              
                         }
                          '
                          )
                 )),
                 shinyjs::useShinyjs(), collapsible = TRUE
)


# zdefiniowanie działania aplikacji
server <- function(input, output,session) {
  
    ### utworzenie wydarzenia obserwacyjnego, aktywującego lub dezaktywującego 
    ### sekcję według warunków wyboru kategorii wskaźników
    observeEvent(input$aggr_button, {
        if(input$aggr_button == 1){
            shinyjs::disable(id = "index")
            shinyjs::disable(id = "variable_index")
            shinyjs::disable(id = "scatter_input") 
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
            shinyjs::disable(id = "scatter_input") 
            updateCheckboxInput(
                inputId = "aggr_button",
                value = FALSE
            )}
    })
    observeEvent(input$indicator_button, {
        if(input$indicator_button == 0){
            shinyjs::disable(id = "index")
            shinyjs::disable(id = "variable_index")
            shinyjs::disable(id = "scatter_input") 
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
            shinyjs::disable(id = "scatter_input") 
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
                shinyjs::disable(id = "variable_unit")
                shinyjs::disable(id = "scatter_input") }
        })
    
    
    
    
    ### utworzenie wydarzenia obserwacyjnego, aktywującego lub dezaktywującego 
    ### sekcję według warunków wyboru zbiorów danych do pobrania
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
    
    
    
    
    
    
    ##### WARSTWA WYJŚCIOWA
    ####### utworzenie obiektu reaktywnego - warstwy wyjściowej wyborów poszczególnych opcji
    layer <- eventReactive(input$run, {
      input$run
        aggr <- isolate(switch(input$unit, # utworzenie obiektu zawierającego wybraną listę wybranych obiektów według jednostki agregacji
                       "Blocks" = aggr_objects[[1]],
                       "Groups of blocks" = aggr_objects[[2]],
                       "Tracts" = aggr_objects[[3]]))
        
        indexes <- isolate(switch(input$index, # utworzenie obiektu zawierającego wybraną listę wybranych obiektów według wskaźnika
                          "Entropy" = ind_objects[[1]],
                          "Index of dissimilarity" = ind_objects[[2]],
                          "The information theory index H" = ind_objects[[3]]))
        # warunki wyboru roku pliku wyjściowego według zaznaczonej opcji (według jednostki agregacji lub wskaźnika)
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
    })
    # utworzenie wydarzenia obserwującego wybór poszczególnych wskaźników z listy i zamieniającego elementy listy według wyboru danej miary
    observeEvent(input$index, {
    if (input$indicator_button == 1){
        if (input$index == "Entropy"){
          freezeReactiveValue(input, "variable_index")
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("Entropy |ALL AGGREGATION UNITS",
                                        "Entropy std |ALL AGGREGATION UNITS"),
                            selected = "Entropy |ALL AGGREGATION UNITS"
          )}
        else if (input$index == "Index of dissimilarity"){
          freezeReactiveValue(input, "variable_index")
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("white-black |BLOCKS",
                                        "white-asian |BLOCKS",
                                        "white-latin |BLOCKS",
                                        "black-latin |BLOCKS",
                                        "black-asian |BLOCKS",
                                        "latin-asian |BLOCKS",
                                        "white-black |GROUP OF BLOCKS",
                                        "white-asian |GROUP OF BLOCKS",
                                        "white-latin |GROUP OF BLOCKS",
                                        "black-latin |GROUP OF BLOCKS",
                                        "black-asian |GROUP OF BLOCKS",
                                        "latin-asian |GROUP OF BLOCKS",
                                        "white-black |TRACT",
                                        "white-asian |TRACT",
                                        "white-latin |TRACT",
                                        "black-latin |TRACT",
                                        "black-asian |TRACT",
                                        "latin-asian |TRACT"),
                            selected = "white-black |BLOCKS"
          )
        }
        else if (input$index == "The information theory index H"){
          freezeReactiveValue(input, "variable_index")
          updateSelectInput(session, "variable_index",
                            label = "Choose an aggregation unit and variable",
                            choices = c("H |BLOCKS",
                                        "H |GROUP OF BLOCKS",
                                        "H |TRACTS"),
                            selected = "H |BLOCKS"
                            )
                          }}
      
    })
    
    
    
    
    
   
    
    ######### RENDEROWANIE MAPY INTERAKTYWNEJ
         output$map <- renderTmap({
           palette <- c("#f8fcc1", "#74b496", "#2f5151")
           ### opcje mapy przy wyborze pliku ze wszystkimi wskaźnikami dla jednej jednostki agregacji
           if (isolate(input$aggr_button) == 1){ 
             input$run
             tmap_mode("view")
             layer <- layer()
             var <- isolate(switch(input$variable_unit, # obiekt podmieniający wybór z listy wskaźników na nazwę kolumny
                                   "Entropy" = "Entropy", 
                                   "Entropy std" = "Entropy_std",
                                   "The information theory index H" = "H", 
                                   "Index of dissimilarity (white-black)" = "D_wb",
                                   "Index of dissimilarity (white-asian)" = "D_wa", 
                                   "Index of dissimilarity (white-latin)" = "D_wl",
                                   "Index of dissimilarity (black-latin)" = "D_bl", 
                                   "Index of dissimilarity (black-asian)" = "D_ba",
                                   "Index of dissimilarity (latin-asian)" = "D_la"))
             
             popup <- c("Entropy: " = "Entropy", "Entropy std: " = "Entropy_std", # zmienne wyświetlane w popupie
                        "H: " = "H", "D (white-black): " = "D_wb", "D (white-asian): " = "D_wa", 
                        "D (white-latin): " = "D_wl", "D (black-latin): " = "D_bl", 
                        "D (black-asian): " = "D_ba", "D (latin-asian): " = "D_la")
             
                tm_shape(layer) + tm_view(set.view = c(-95, 40, 4.5)) + # określenie parametrów mapy interaktywnej
                 tm_fill(title = isolate(paste0(input$variable_unit, br(), " in ", input$year, " | ", input$unit)),
                         col = var, 
                         palette = palette,
                         id = "NAME",
                         popup.vars = popup,
                         popup.format=list(digits=2)) + tm_view(leaflet.options = c(zoomSnap=0.01)) +  tm_borders() + tm_basemap(c("Carto Dark" = "CartoDB.DarkMatter",
                                                                                    "OpenStreetMap" = "OpenStreetMap",
                                                                                    "Esri" = "Esri.WorldGrayCanvas"))
     }
          
          ### opcje mapy przy wyborze pliku z jednym wskaźnikiem dla wszystkich jednostek agregacji 
          # Entropia
          else if (isolate(input$indicator_button) == 1){
            if (isolate(input$index) == "Entropy"){
              input$run
              tmap_mode("view")
              layer <- layer()
              var <- isolate(switch(input$variable_index, 
                                       "Entropy |ALL AGGREGATION UNITS" = "Entropy",
                                       "Entropy std |ALL AGGREGATION UNITS" = "Entropy_std"))
              popup <- c("Entropy: " = "Entropy", "Entropy std: " = "Entropy_std")
              
              tm_shape(layer) + tm_view(set.view = c(-95, 40, 4.5)) + 
                tm_fill(title = isolate(paste0(input$variable_index, br(), " in ", input$year)),
                        col = var, 
                        palette = palette,
                        id = "NAME",
                        popup.vars = popup,
                        popup.format=list(digits=2)) + tm_view(leaflet.options = c(zoomSnap=0.01)) + tm_borders() + tm_basemap(c("Carto Dark" = "CartoDB.DarkMatter",
                                                                                   "OpenStreetMap" = "OpenStreetMap",
                                                                                   "Esri" = "Esri.WorldGrayCanvas"))
            }
            
            # Wskaźnik niepodobieństwa D
            else if (isolate(input$index) == "Index of dissimilarity"){
              input$run
              tmap_mode("view")
              layer <- layer()
              var <- isolate(switch(input$variable_index, 
                            "white-black |BLOCKS" = "D_wb_block",
                            "white-asian |BLOCKS" = "D_wa_block",
                            "white-latin |BLOCKS" = "D_wl_block",
                            "black-latin |BLOCKS" = "D_bl_block",
                            "black-asian |BLOCKS" = "D_ba_block",
                            "latin-asian |BLOCKS" = "D_la_block",
                            "white-black |GROUP OF BLOCKS" = "D_wb_group_blocks",
                            "white-asian |GROUP OF BLOCKS" = "D_wa_group_blocks",
                            "white-latin |GROUP OF BLOCKS" = "D_wl_group_blocks",
                            "black-latin |GROUP OF BLOCKS" = "D_bl_group_blocks",
                            "black-asian |GROUP OF BLOCKS" = "D_ba_group_blocks",
                            "latin-asian |GROUP OF BLOCKS" = "D_la_group_blocks",
                            "white-black |TRACT" = "D_wb_tract",
                            "white-asian |TRACT" = "D_wa_tract",
                            "white-latin |TRACT" = "D_wl_tract",
                            "black-latin |TRACT" = "D_bl_tract",
                            "black-asian |TRACT" = "D_ba_tract",
                            "latin-asian |TRACT" = "D_la_tract"))
              popup <- c("white-black |BLOCKS: " = "D_wb_block",
                         "white-asian |BLOCKS: " = "D_wa_block",
                         "white-latin |BLOCKS: " = "D_wl_block",
                         "black-latin |BLOCKS: " = "D_bl_block",
                         "black-asian |BLOCKS: " = "D_ba_block",
                         "latin-asian |BLOCKS: " = "D_la_block",
                         "white-black |GROUP OF BLOCKS: " = "D_wb_group_blocks",
                         "white-asian |GROUP OF BLOCKS: " = "D_wa_group_blocks",
                         "white-latin |GROUP OF BLOCKS: " = "D_wl_group_blocks",
                         "black-latin |GROUP OF BLOCKS: " = "D_bl_group_blocks",
                         "black-asian |GROUP OF BLOCKS: " = "D_ba_group_blocks",
                         "latin-asian |GROUP OF BLOCKS: " = "D_la_group_blocks",
                         "white-black |TRACT: " = "D_wb_tract",
                         "white-asian |TRACT: " = "D_wa_tract",
                         "white-latin |TRACT: " = "D_wl_tract",
                         "black-latin |TRACT: " = "D_bl_tract",
                         "black-asian |TRACT: " = "D_ba_tract",
                         "latin-asian |TRACT: " = "D_la_tract")
              
              tm_shape(layer) + tm_view(set.view = c(-95, 40, 4.5)) + 
                tm_fill(title = isolate(paste0(input$index, br(), input$variable_index, br(), " in ", input$year)),
                        col = var, 
                        palette = palette,
                        id = "NAME",
                        popup.vars = popup,
                        popup.format=list(digits=2)) + tm_view(leaflet.options = c(zoomSnap=0.01)) + tm_borders() + tm_basemap(c("Carto Dark" = "CartoDB.DarkMatter",
                                                                                   "OpenStreetMap" = "OpenStreetMap",
                                                                                   "Esri" = "Esri.WorldGrayCanvas"))
            }
            
            # Wskaźnik teorii informacji H
            else if (isolate(input$index) == "The information theory index H"){
              input$run
              tmap_mode("view")
              layer <- layer()
              var <- isolate(switch(input$variable_index, 
                            "H |BLOCKS" = "H_block",
                            "H |GROUP OF BLOCKS" = "H_group_blocks",
                            "H |TRACTS" = "H_tract"))
              popup <- c("H |BLOCKS: " = "H_block",
                         "H |GROUP OF BLOCKS: " = "H_group_blocks",
                         "H |TRACTS: " = "H_tract")
              
              tm_shape(layer) + tm_view(set.view = c(-95, 40, 4.5)) +
                tm_fill(title = isolate(paste0(input$variable_index, br(), " in ", input$year)),
                        col = var, 
                        palette = palette,
                        id = "NAME",
                        popup.vars = popup,
                        popup.format=list(digits=2)) + tm_view(leaflet.options = c(zoomSnap=0.01)) + tm_borders() + tm_basemap(c("Carto Dark" = "CartoDB.DarkMatter",
                                                                                  "OpenStreetMap" = "OpenStreetMap",
                                                                                  "Esri" = "Esri.WorldGrayCanvas")) 
                }
              }
        })
        
          
    
         
         
         
         
         
   ##### UTWORZENIE OBIEKTU REAKTYWNEGO W CELU WYGENEROWANIA HISTOGRAMU
      plot <- reactive({
      plot_theme <- theme( # ustawienie stylu każdegow wykresu
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
      
      layer <- layer() 
      
      
      ### opcje histogramu przy wyborze pliku ze wszystkimi wskaźnikami dla jednej jednostki agregacji
      if (isolate(input$aggr_button) == 1){ 
        var <- isolate(switch(input$variable_unit, # obiekt podmieniający wybór z listy wskaźników na nazwę kolumny
                              "Entropy" = "Entropy", 
                              "Entropy std" = "Entropy_std",
                              "The information theory index H" = "H", 
                              "Index of dissimilarity (white-black)" = "D_wb",
                              "Index of dissimilarity (white-asian)" = "D_wa", 
                              "Index of dissimilarity (white-latin)" = "D_wl",
                              "Index of dissimilarity (black-latin)" = "D_bl", 
                              "Index of dissimilarity (black-asian)" = "D_ba",
                              "Index of dissimilarity (latin-asian)" = "D_la"))
        # pola ze statystykami
        output$text_mean <- renderText({paste0("mean: ", round(mean(layer[[var]], na.rm = TRUE), 2)) })
        output$text_sd <- renderText({paste0("sd: ", round(sd(layer[[var]], na.rm = TRUE), 2)) })
        output$text_min <- renderText({paste0("min.: ", round(min(layer[[var]], na.rm = TRUE), 2)) })
        output$text_max <- renderText({paste0("max.: ", round(max(layer[[var]], na.rm = TRUE), 2)) })
        output$text_median <- renderText({paste0("median: ", round(median(layer[[var]], na.rm = TRUE), 2)) })
        output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(layer[[var]], probs = 0.75, na.rm = TRUE), 2)) })
        
        index <- layer[[var]] # warstwa do wyświetlenia
        g <- ggplot(layer(), aes(index)) + # obiekt zawierający wykres
             geom_histogram(bins = 80, fill = '#477676') + 
             plot_theme + labs(x = var, 
                            y = "Count", 
                            title = isolate(paste0(input$variable_unit, " in ", input$year)))
        
        ggplotly(g) %>% config(displayModeBar = F) # ustawienie interaktywności wykresu
      }
      
      
      ### opcje histogramu przy wyborze pliku z jednym wskaźnikiem dla wszystkich jednostek agregacji 
        # Entropia
      else if (isolate(input$indicator_button) == 1){
        if (isolate(input$index) == "Entropy"){
          
          var <- isolate(switch(input$variable_index, 
                                "Entropy |ALL AGGREGATION UNITS" = "Entropy",
                                "Entropy std |ALL AGGREGATION UNITS" = "Entropy_std"))
          
          output$text_mean <- renderText({paste0("mean: ", round(mean(layer[[var]], na.rm = TRUE), 2)) })
          output$text_sd <- renderText({paste0("sd: ", round(sd(layer[[var]], na.rm = TRUE), 2)) })
          output$text_min <- renderText({paste0("min.: ", round(min(layer[[var]], na.rm = TRUE), 2)) })
          output$text_max <- renderText({paste0("max.: ", round(max(layer[[var]], na.rm = TRUE), 2)) })
          output$text_median <- renderText({paste0("median: ", round(median(layer[[var]], na.rm = TRUE), 2)) })
          output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(layer[[var]], probs = 0.75, na.rm = TRUE), 2)) })
          
          index <- layer[[var]]
          g <- ggplot(layer, aes(index)) + geom_histogram(bins = 80, fill = '#477676') + 
            plot_theme + labs(x = var, 
                              y = "Count", 
                              title = isolate(paste0(input$variable_index, " in ", input$year)))
          
          ggplotly(g)%>% config(displayModeBar = F)
        }
        
        # Wskaźnik niepodobieństwa D
        else if (isolate(input$index) == "Index of dissimilarity"){
          var <- isolate(switch(input$variable_index, 
                                "white-black |BLOCKS" = "D_wb_block",
                                "white-asian |BLOCKS" = "D_wa_block",
                                "white-latin |BLOCKS" = "D_wl_block",
                                "black-latin |BLOCKS" = "D_bl_block",
                                "black-asian |BLOCKS" = "D_ba_block",
                                "latin-asian |BLOCKS" = "D_la_block",
                                "white-black |GROUP OF BLOCKS" = "D_wb_group_blocks",
                                "white-asian |GROUP OF BLOCKS" = "D_wa_group_blocks",
                                "white-latin |GROUP OF BLOCKS" = "D_wl_group_blocks",
                                "black-latin |GROUP OF BLOCKS" = "D_bl_group_blocks",
                                "black-asian |GROUP OF BLOCKS" = "D_ba_group_blocks",
                                "latin-asian |GROUP OF BLOCKS" = "D_la_group_blocks",
                                "white-black |TRACT" = "D_wb_tract",
                                "white-asian |TRACT" = "D_wa_tract",
                                "white-latin |TRACT" = "D_wl_tract",
                                "black-latin |TRACT" = "D_bl_tract",
                                "black-asian |TRACT" = "D_ba_tract",
                                "latin-asian |TRACT" = "D_la_tract"))
          
          output$text_mean <- renderText({paste0("mean: ", round(mean(layer[[var]], na.rm = TRUE), 2)) })
          output$text_sd <- renderText({paste0("sd: ", round(sd(layer[[var]], na.rm = TRUE), 2)) })
          output$text_min <- renderText({paste0("min.: ", round(min(layer[[var]], na.rm = TRUE), 2)) })
          output$text_max <- renderText({paste0("max.: ", round(max(layer[[var]], na.rm = TRUE), 2)) })
          output$text_median <- renderText({paste0("median: ", round(median(layer[[var]], na.rm = TRUE), 2)) })
          output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(layer[[var]], probs = 0.75, na.rm = TRUE), 2)) })
          
          index <- layer[[var]]
          g <- ggplot(layer, aes(index)) + geom_histogram(bins = 80, fill = '#477676') + 
            plot_theme + labs(x = var, 
                              y = "Count", 
                              title = isolate(paste0(input$variable_index, " in ", input$year)))
          
          ggplotly(g)%>% config(displayModeBar = F)
        }
        
        # Wskaźnik teorii informacji H
        else if (isolate(input$index) == "The information theory index H"){
          
          var <- isolate(switch(input$variable_index, 
                                "H |BLOCKS" = "H_block",
                                "H |GROUP OF BLOCKS" = "H_group_blocks",
                                "H |TRACTS" = "H_tract"))
          
          output$text_mean <- renderText({paste0("mean: ", round(mean(layer[[var]], na.rm = TRUE), 2)) })
          output$text_sd <- renderText({paste0("sd: ", round(sd(layer[[var]], na.rm = TRUE), 2)) })
          output$text_min <- renderText({paste0("min.: ", round(min(layer[[var]], na.rm = TRUE), 2)) })
          output$text_max <- renderText({paste0("max.: ", round(max(layer[[var]], na.rm = TRUE), 2)) })
          output$text_median <- renderText({paste0("median: ", round(median(layer[[var]], na.rm = TRUE), 2)) })
          output$text_quantile <- renderText({paste0("quantile (0.75): ", round(quantile(layer[[var]], probs = 0.75, na.rm = TRUE), 2)) })
          
          index <- layer[[var]]
          g <- ggplot(layer, aes(index)) + geom_histogram(bins = 80, fill = '#477676') + 
            plot_theme + labs(x = var, 
                              y = "Count", 
                              title = isolate(paste0(input$variable_index, " in ", input$year)))
          
          ggplotly(g)%>% config(displayModeBar = F)
        }}
      }) 
      
      ###### RENDEROWANIE HISTOGRAMU
      output$plot <- renderPlotly({
        print(plot())
      })
      
      
      
      ##### UTWORZENIE OBIEKTU REAKTYWNEGO W CELU WYGENEROWANIA WYKRESU ROZRZUTU
      scatter_plot <- reactive({
        plot_theme <- theme( # ustawienie stylu
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
        
        layer <- layer() 
        if (isolate(input$aggr_button) == 1){ 
          shinyjs::disable(id = "scatter_input") 
        }
        
        else if (isolate(input$indicator_button) == 1){ 
          ### opcje histogramu przy wyborze pliku ze wszystkimi wskaźnikami dla jednej jednostki agregacji
            if (isolate(input$index) == "Entropy"){
              shinyjs::disable(id = "scatter_input") # dezaktywacja wyboru ze względu na identyczne wartości entropii w każdej jednostce agregacji
               }
        
            
            ### opcje histogramu przy wyborze pliku z jednym wskaźnikiem dla wszystkich jednostek agregacji
              # Wskaźnik niepodobieństwa D
            else if (isolate(input$index) == "Index of dissimilarity"){
              shinyjs::enable(id = "scatter_input")
              
              var <- isolate(switch(input$variable_index, # obiekt podmieniający wybór z listy wskaźników na nazwę kolumny
                                    "white-black |BLOCKS" = "D_wb_block",
                                    "white-asian |BLOCKS" = "D_wa_block",
                                    "white-latin |BLOCKS" = "D_wl_block",
                                    "black-latin |BLOCKS" = "D_bl_block",
                                    "black-asian |BLOCKS" = "D_ba_block",
                                    "latin-asian |BLOCKS" = "D_la_block",
                                    "white-black |GROUP OF BLOCKS" = "D_wb_group_blocks",
                                    "white-asian |GROUP OF BLOCKS" = "D_wa_group_blocks",
                                    "white-latin |GROUP OF BLOCKS" = "D_wl_group_blocks",
                                    "black-latin |GROUP OF BLOCKS" = "D_bl_group_blocks",
                                    "black-asian |GROUP OF BLOCKS" = "D_ba_group_blocks",
                                    "latin-asian |GROUP OF BLOCKS" = "D_la_group_blocks",
                                    "white-black |TRACT" = "D_wb_tract",
                                    "white-asian |TRACT" = "D_wa_tract",
                                    "white-latin |TRACT" = "D_wl_tract",
                                    "black-latin |TRACT" = "D_bl_tract",
                                    "black-asian |TRACT" = "D_ba_tract",
                                    "latin-asian |TRACT" = "D_la_tract"))
            # przypisanie plików z nazwami kolumn w celu odpowiedniego podstawienia do wykresu
            sub <- substr(var, 1, 5)
            x <- layer[[var]]
            y_sub <- paste0(sub, input$scatter_input)
            y <- layer[[y_sub]]
            g <- ggplot(layer, aes(x=x, y=y)) + geom_smooth(color = "#c88484") + plot_theme + geom_bin2d(bins = 40) + # opcje wykresu rozrzutu
              scale_fill_gradient(low = "#2f5151", high = "#dfecec") + labs(x = var, 
                                                                            y = y_sub, 
                                                                            title = isolate(paste0("Comparison of ", input$index, " in ", input$year)))
            ggplotly(g)%>% config(displayModeBar = F) 
            }
            
            
            # Wskaźnik teorii informacji H
            else if (isolate(input$index) == "The information theory index H"){
              shinyjs::enable(id = "scatter_input")
              
              var <- isolate(switch(input$variable_index, 
                                    "H |BLOCKS" = "H_block",
                                    "H |GROUP OF BLOCKS" = "H_group_blocks",
                                    "H |TRACTS" = "H_tract"))
            
              sub <- substr(var, 1, 2)
              x <- layer[[var]]
              y_sub <- paste0(sub, input$scatter_input)
              y <- layer[[y_sub]]
              
              g <- ggplot(layer, aes(x=x, y=y)) + geom_smooth(color = "#c88484") + plot_theme + geom_bin2d(bins = 40) + 
                scale_fill_gradient(low = "#2f5151", high = "#dfecec") + labs(x = var, 
                                                                              y = y_sub, 
                                                                              title = isolate(paste0("comparison of ", input$index, " in ", input$year)))
              ggplotly(g)%>% config(displayModeBar = F) 
            }
        }
      })
      
      
      
      # RENDEROWANIE WYKRESU ROZRZUTU
      output$scatter_plot <- renderPlotly({
        print(scatter_plot())
      })
      
      
      
  
  ### utworzenie obiektu reaktywnego, który zostanie wyświetlony w tabeli oraz pobrany  
    datasetInput <- eventReactive(input$show,{
           if (input$dataset_button_1 == 1){ # wybór pliku z pierwszej sekcji w zależności od zaznaczenia
            switch(input$dataset_1,
           "Blocks - 1990" = shp_block_1990, "Blocks - 2000" = shp_block_2000, "Blocks - 2010" = shp_block_2010, "Blocks - 2020" = shp_block_2020,
           "Groups of blocks - 1990" = shp_grp_blocks_1990, "Groups of blocks - 2000" = shp_grp_blocks_2000, "Groups of blocks - 2010" = shp_grp_blocks_2010, "Groups of blocks - 2020" = shp_grp_blocks_2020,
           "Tracts - 1990" = shp_tract_1990, "Tracts - 2000" = shp_tract_2000, "Tracts - 2010" = shp_tract_2010, "Tracts - 2020" = shp_tract_2020)
           #dataset <- dataset %>% st_drop_geometry()
           }
      
       else if (input$dataset_button_2 == 1){ # wybór pliku z drugiej sekcji w zależności od zaznaczenia
        switch(input$dataset_2,
        "Entropy - 1990" = shp_ind_ent_1990, "Entropy - 2000" = shp_ind_ent_2000, "Entropy - 2010" = shp_ind_ent_2010, "Entropy - 2020" = shp_ind_ent_2020,
        "Index of dissimilarity - 1990"  = shp_ind_D_1990, "Index of dissimilarity - 2000" = shp_ind_D_2000,
        "Index of dissimilarity - 2010" = shp_ind_D_2010, "Index of dissimilarity - 2020" = shp_ind_D_2020,
        "The information theory index H - 1990" = shp_ind_H_1990, "The information theory index H - 2000" = shp_ind_H_2000, 
        "The information theory index H - 2010" = shp_ind_H_2010, "The information theory index H - 2020" = shp_ind_H_2020)
      }
    })
    
    ## porzucenie geometrii wybranego pliku w celu poprawnego wyświetlenia w tabeli
    table_dataset_2 <- eventReactive(input$show,{
      datasetInput() %>% st_drop_geometry()
    })
    
    ## wyrenderowanie tabeli
    output$table_dt <- DT::renderDataTable(DT::datatable(table_dataset_2(), options = list(
      rownames = FALSE,
      pageLength = 12,
      scrollX = TRUE,
      lengthMenu = c(6, 12))
      ))
    
    
    
    
    
    
    ###  przypisanie opcji pobrania tabeli jako .csv
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
    
    
    
    ###  przypisanie opcji pobrania tabeli jako .gpkg
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
    
    
    
    ###  przypisanie opcji pobrania histogramu jako .png
    output$save_png<- downloadHandler(
      filename = function() {
        if (isolate(input$aggr_button) == 1){
          paste0(input$variable_unit, "_", input$year, "_hist", ".png", sep = "")
        }
        else if (isolate(input$indicator_button) == 1){
          paste0(input$index, "_", input$year, "_hist", ".png", sep = "")
        }
      },
      content = function(file) {
        export(plot(), file=file)
      }
    )
    
    
    
    ###  przypisanie opcji pobrania wykresu rozrzutu jako .png
    output$save_scatter_png<- downloadHandler(
      filename = function() {
          paste0(input$index, "_", input$year, "_scatter", ".png", sep = "")
      },
      content = function(file) {
        export(scatter_plot(), file=file)
      }
    )

    
    ## opcje ukrycia instrukcji po wyrenderowaniu mapy i wykresów
    observeEvent(input$run,{hide("instruction_1")})
    observeEvent(input$run,{hide("instruction_2")})
    observeEvent(input$show,{hide("instruction_3")})
    observeEvent(input$run,
                  runjs('document.getElementById("save_png").style.visibility = "visible";')
                 )
    observeEvent(input$run,{
      if (isolate(input$aggr_button) == 1){ 
        runjs('document.getElementById("save_scatter_png").style.visibility = "hidden";')
      }
      
      else if (isolate(input$indicator_button) == 1){ 
        if (isolate(input$index) == "Entropy"){
          runjs('document.getElementById("save_scatter_png").style.visibility = "hidden";')
        }
        else if (isolate(input$index) == "Index of dissimilarity"){
          runjs('document.getElementById("save_scatter_png").style.visibility = "visible";')
        }
        else if (isolate(input$index) == "The information theory index H"){
          runjs('document.getElementById("save_scatter_png").style.visibility = "visible";')
        }
      }
    })
                 
}


# uruchomienie aplikacji
shinyApp(ui, server)

