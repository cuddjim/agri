library(shiny); library(shiny.i18n)
library(leaflet); library(plotly)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  uiOutput('page_content')
  
)

translator <- Translator$new(translation_json_path = "translation")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # select and translate language
  tr <- reactive({
    
    selected <- 'en'#input$selected_language
    
    if (length(selected) > 0 && selected == TRUE) {
      translator$set_translation_language('en')
    }
    
    else {
      
      translator$set_translation_language('fr')
      
    }
    
    translator
    
  })
  
  farm_snd_table <- reactive({
    
    
    selected_geo = input$province_farm
    selected_crop = input$crop_farm
    
    farm_snd %>% spread(ref_date, value) %>% 
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>% 
      filter(geo == selected_geo, type_of_crop == selected_crop)
    
  })

  output$farm_data_table <- renderDataTable({
    
    datatable(farm_snd_table(), extensions = c('Buttons','FixedColumns'),
              options = list(buttons = c('copy','csv'),
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 4),
                             dom = 'Btpl',
                             lengthMenu = list(c(8, 16, 24, 72,-1),c('8','16','24','72','All')), 
                             pageLength = 24,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#03085B', 'color': '#fff'});",
                               "}")))
    
  })
  
  farm_snd_graph <- reactive({


    selected_geo = input$province_farm_graph
    selected_crop_1 = input$crop_farm_graph_1
    selected_crop_2 = input$crop_farm_graph_2
    selected_crops = c(selected_crop_1,selected_crop_2)
    snd_choice = input$snd_graph

    farm_snd %>% spread(ref_date, value) %>%
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'))) %>%
      filter(geo == selected_geo, type_of_crop %in% selected_crops, farm_supply_and_disposition_of_grains == snd_choice)

  })

  output$farm_data_graph <- renderPlotly({

    plot_ly() %>% 
      add_trace(data = farm_snd_graph(), type = 'scatter',mode = 'markers') %>% 
      add_trace(data = farm_snd_graph(),type = 'scatter',mode = 'markers')

  })
  
  output$page_content <- renderUI({
    
    navbarPage(title = div(span(img(src = "wheat2.png"),
                                "Crops and grains in Canada",
                                style = "position: relative; top: 80%; transform: translateY(10%);")),
                 
               tabPanel(

                 tr()$t('Crop Production'),

                 tabsetPanel(

                   type='tabs',
 
                   tabPanel(

                     'Graphs',

                     mainPanel(
                       fluidRow(
                         plotlyOutput("line1", height = '550px')
                       ),
                       width=8
                     )

                   )

                 )
               ),
               
               tabPanel(
                 
                 tr()$t('Farm supply and disposition of grains'),
                 
                 tabsetPanel(
                   
                   type = 'tabs',
                   
                   tabPanel(
                     
                     'Map',
                     
                     sidebarPanel(
                       selectizeInput("map_commodity", label = 'Select Fuel Type',
                                      choices = c('a','b'),
                                      selected = 'a'),
                       sliderInput("year", label = 'Select Year:',2005, 2018, value=2018,
                                   step=1,
                                   animate=TRUE,
                                   sep = ""),
                       width=2
                     ),
                     
                     mainPanel(
                       leafletOutput("plot",height=600),
                       width=10
                     )
                     
                   ),
                   
                   
                   tabPanel(
                     
                     'Data table',
                     
                     sidebarPanel(
                       selectizeInput("province_farm", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("crop_farm", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         dataTableOutput('farm_data_table')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Graphs',
                     
                     sidebarPanel(
                       selectizeInput("province_farm_graph", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("crop_farm_graph_1", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("crop_farm_graph_2", label = 'Select Crop',
                                     choices = unique(farm_snd$type_of_crop), selected='Canola'),
                       selectizeInput('snd_graph', label = 'Select variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected = 'Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("farm_data_graph", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Notes',
                     
                     mainPanel(
                       dataTableOutput("storytable"),
                       width=12
                     )
                     
                 )
               )
               )
    )
    
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)