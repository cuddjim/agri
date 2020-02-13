
library(shiny); library(shiny.i18n)
library(leaflet); library(plotly)
library(DT); library(shinyWidgets)

sad_map = readOGR('CropsSADRegions_2017_Gen/CropsSADRegions_2017_Gen.shp',stringsAsFactors = FALSE)
sad_map@data$PRSADReg=as.numeric(sad_map@data$PRSADReg)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(HTML("
      .navbar .navbar-nav {float: right; 
                  color: #ff3368; 
                  font-size: 20px; 
                  background-color: #FFFF00 ; } 
                  .navbar.navbar-default.navbar-static-top{ color: #ff3368; 
                  font-size: 20px; 
                  background-color: #FFFF00 ;}
                  .navbar .navbar-header {float: left; } 
                  .navbar-default .navbar-brand { color: #ff3368; 
                  font-size: 38px; 
                  background-color: #FFFF00 ;} 
                  ")),
  tags$script("$(\"input:radio[name='selected_language'][value='fr']\").parent().css('background-color', '#FFFFFF');"),
  uiOutput('select_language'),
  uiOutput('page_content')
  
)

translator <- Translator$new(translation_json_path = "translation")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # select and translate language
  tr <- reactive({
    
    selected <- input$selected_language
    
    if (length(selected) > 0 && selected == TRUE) {
      translator$set_translation_language('en')
    }
    
    else {
      
      translator$set_translation_language('fr')
      
    }
    
    translator
    
  })
  
  
  ## crop production tab

  # map
  cp_map_reactive <- reactive({

    # min_year = min(input$year); max_year = max(input$year)
    # farm_map_var = input$farm_map_var
    # min_emissions = str_c(farm_map_var,'_emission_',min_year); max_emissions = str_c(farm_map_var,'_emission_',max_year)
    # min_outputs = str_c(farm_map_var,'_output_',min_year); max_outputs = str_c(farm_map_var,'_output_',max_year)
    #
    # prov_map@data %<>%
    #   mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
    #          outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>%
    #   mutate(scaled_outputs = log(1+outputs)^2.5) %>%
    #   mutate(mean_output = mean(scaled_outputs,na.rm=TRUE)) %>%
    #   mutate(outputs_1=23*scaled_outputs/mean_output)

    sad_map

  })

  output$cp_map <- renderLeaflet({

    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 2,
                                     attributionControl=FALSE)) %>%
      setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
      addProviderTiles(providers$Wikimedia,
                       options = providerTileOptions(opacity = 0.8)) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = cp_map_reactive(),
                  fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), PRSADReg, 5)(PRSADReg),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4)

  })

  # data table
  cp_table_reactive <- reactive({

    cp_prov_table = input$cp_prov_table
    cp_crop_table = input$cp_crop_table
    cp_var_table = input$cp_var_table

    farm_snd %>% spread(ref_date, value) %>%
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>%
      filter(
        geo == cp_prov_table,
        type_of_crop == cp_crop_table,
        farm_supply_and_disposition_of_grains == cp_var_table) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=farm_supply_and_disposition_of_grains)

  })

  output$cp_table <- renderDataTable({

    datatable(cp_table_reactive(), extensions = c('Buttons','FixedColumns'),
              options = list(buttons = c('copy','csv'),
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 4),
                             dom = 'Bfrtip',
                             lengthMenu = list(c(8, 16, 24, 72,-1),c('8','16','24','72','All')),
                             pageLength = 24,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#03085B', 'color': '#fff'});",
                               "}")))

  })

  # line chart
  cp_plot_reactive_1 <- reactive({

    cp_plot_geo = input$cp_prov_plot
    cp_plot_crop_1 = input$cp_crop_plot_1
    cp_plot_var = input$cp_var_plot

    farm_snd %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == cp_plot_geo,type_of_crop == cp_plot_crop_1,
             farm_supply_and_disposition_of_grains == cp_plot_var)

  })

  cp_plot_reactive_2 <- reactive({

    cp_plot_geo = input$cp_prov_plot
    cp_plot_crop_2 = input$cp_crop_plot_2
    cp_plot_var = input$cp_var_plot

    farm_snd %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == cp_plot_geo,type_of_crop == cp_plot_crop_2,
             farm_supply_and_disposition_of_grains == cp_plot_var)

  })

  output$cp_plot <- renderPlotly({

    plot_ly() %>%
      add_trace(data = cp_plot_reactive_1(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', 
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes')) %>%
      add_trace(data = cp_plot_reactive_2(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines',
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'))

  })
  
  
  ## farm supply and disposition tab
  
  # map
  farm_map_reactive <- reactive({
    
    # min_year = min(input$year); max_year = max(input$year)
    # farm_map_var = input$farm_map_var
    # min_emissions = str_c(farm_map_var,'_emission_',min_year); max_emissions = str_c(farm_map_var,'_emission_',max_year)
    # min_outputs = str_c(farm_map_var,'_output_',min_year); max_outputs = str_c(farm_map_var,'_output_',max_year)
    # 
    # prov_map@data %<>% 
    #   mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
    #          outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>% 
    #   mutate(scaled_outputs = log(1+outputs)^2.5) %>%
    #   mutate(mean_output = mean(scaled_outputs,na.rm=TRUE)) %>%
    #   mutate(outputs_1=23*scaled_outputs/mean_output)
    
    sad_map
    
  })
  
  output$farm_map <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 2,
                                     attributionControl=FALSE)) %>%
      setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
      addProviderTiles(providers$Wikimedia,
                       options = providerTileOptions(opacity = 0.8)) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = farm_map_reactive(),
                  fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), PRSADReg, 5)(PRSADReg),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4)
    
  })
  
  # data table
  farm_table_reactive <- reactive({
    
    farm_prov_table = input$farm_prov_table
    farm_crop_table = input$farm_crop_table
    farm_var_table = input$farm_var_table
    
    farm_snd %>% spread(ref_date, value) %>% 
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>% 
      filter(
        geo == farm_prov_table, 
        type_of_crop == farm_crop_table,
        farm_supply_and_disposition_of_grains == farm_var_table) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=farm_supply_and_disposition_of_grains)
    
  })
  
  output$farm_table <- renderDataTable({
    
    datatable(farm_table_reactive(), extensions = c('Buttons','FixedColumns'),
              options = list(buttons = c('copy','csv'),
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 4),
                             dom = 'Btpl',
                             lengthMenu = list(c(8, 16, 24, 72,-1),c('8','16','24','72','All')), 
                             pageLength = 24,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#03085B', 'color': '#fff'});",
                               "}")))
    
  })
  
  # line chart
  farm_plot_reactive_1 <- reactive({
   
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_1 = input$farm_crop_plot_1
    farm_plot_var = input$farm_var_plot
    
    farm_snd %>% filter(grepl('-07',ref_date)) %>% 
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>% 
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>% 
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_1,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  farm_plot_reactive_2 <- reactive({
    
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_2 = input$farm_crop_plot_2
    farm_plot_var = input$farm_var_plot
    
    farm_snd %>% filter(grepl('-07',ref_date)) %>% 
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>% 
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>% 
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_2,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  output$farm_plot <- renderPlotly({
    
    plot_ly() %>% 
      add_trace(data = farm_plot_reactive_1(), 
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines',
                hoverinfo = 'text', text = ~paste(format(round(value, 0), 
                                                         big.mark = ',',
                                                         scientific = F),'tonnes')) %>% 
      add_trace(data = farm_plot_reactive_2(), 
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines',
                hoverinfo = 'text', text = ~paste(format(round(value, 0), 
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'))
    
  }) 
  
  
  ## national supply and disposition tab
  
  # data table
  can_table_reactive <- reactive({
    
    can_prov_table = input$can_prov_table
    can_crop_table = input$can_crop_table
    can_var_table = input$can_var_table
    
    farm_snd %>% spread(ref_date, value) %>%
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>%
      filter(
        geo == can_prov_table,
        type_of_crop == can_crop_table,
        farm_supply_and_disposition_of_grains == can_var_table) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=farm_supply_and_disposition_of_grains)
    
  })
  
  output$can_table <- renderDataTable({
    
    datatable(can_table_reactive(), extensions = c('Buttons','FixedColumns'),
              options = list(buttons = c('copy','csv'),
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 4),
                             dom = 'Btpl',
                             lengthMenu = list(c(8, 16, 24, 72,-1),c('8','16','24','72','All')),
                             pageLength = 24,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#03085B', 'color': '#fff'});",
                               "}")))
    
  })
  
  # line chart
  can_plot_reactive_1 <- reactive({
    
    can_crop_plot_1 = input$can_crop_plot_1
    can_var_plot = input$can_var_plot
    
    farm_snd %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(type_of_crop == can_crop_plot_1,
             farm_supply_and_disposition_of_grains == can_var_plot)
    
  })
  
  can_plot_reactive_2 <- reactive({
    
    can_crop_plot_2 = input$can_crop_plot_2
    can_var_plot = input$can_var_plot
    
    farm_snd %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(type_of_crop == can_crop_plot_2,
             farm_supply_and_disposition_of_grains == can_var_plot)
    
  })
  
  output$can_plot <- renderPlotly({
    
    plot_ly() %>%
      add_trace(data = can_plot_reactive_1(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', 
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes')) %>%
      add_trace(data = can_plot_reactive_2(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines',
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'))
    
  })
  
  
  ## dynamic user interface section
  
  # select language
  output$select_language <- renderUI({
    
    fluidPage(
      
      tabPanel(
        
        'Language',
        
        column(switchInput(
          inputId = "selected_language",
          value = TRUE,
          onLabel = 'FR',
          offLabel = 'EN',
          onStatus = '#000000',
          offStatus = '#FFFFFF',
          labelWidth='auto',
          handleWidth='auto',
          size='normal',
          width='100px'
        ), width=4,offset=10),
        icon = NULL
        
      )
    )
    
  })
  
  # all else
  output$page_content <- renderUI({
    
    navbarPage(title = div(span(img(src = "wheat2.png"),
                                "Crops and grains in Canada",
                                style = "position: relative; top: 80%; transform: translateY(10%);")),
                 
               tabPanel(
                 
                 tr()$t('Crop Production'),
                 
                 tabsetPanel(
                   
                   type = 'tabs',
                   
                   tabPanel(
                     
                     'Map',
                     
                     sidebarPanel(
                       selectizeInput("cp_map_var", label = 'Select Fuel Type',
                                      choices = c('a','b'),
                                      selected = 'a'),
                       sliderInput("cp_map_year", label = 'Select Year:',2005, 2018, value=2018,
                                   step=1,
                                   animate=TRUE,
                                   sep = ""),
                       width=2
                     ),
                     
                     mainPanel(
                       leafletOutput("cp_map",height=600),
                       width=10
                     )
                     
                   ),
                   
                   
                   tabPanel(
                     
                     'Data table',
                     
                     sidebarPanel(
                       selectizeInput("cp_prov_table", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("cp_crop_table", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("cp_var_table", label = 'Select Variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected='Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         dataTableOutput('cp_table')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Graphs',
                     
                     sidebarPanel(
                       selectizeInput("cp_prov_plot", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("cp_crop_plot_1", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("cp_crop_plot_2", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='Canola'),
                       selectizeInput('cp_var_plot', label = 'Select variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected = 'Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("cp_plot", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Notes',
                     
                     mainPanel(
                       dataTableOutput("cp_notes"),
                       width=12
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
                       selectizeInput("farm_map_var", label = 'Select Fuel Type',
                                      choices = c('a','b'),
                                      selected = 'a'),
                       sliderInput("farm_map_year", label = 'Select Year:',2005, 2018, value=2018,
                                   step=1,
                                   animate=TRUE,
                                   sep = ""),
                       width=2
                     ),
                     
                     mainPanel(
                       leafletOutput("farm_map",height=600),
                       width=10
                     )
                     
                   ),
                   
                   
                   tabPanel(
                     
                     'Data table',
                     
                     sidebarPanel(
                       selectizeInput("farm_prov_table", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("farm_crop_table", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("farm_var_table", label = 'Select Variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected='Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         dataTableOutput('farm_table')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Graphs',
                     
                     sidebarPanel(
                       selectizeInput("farm_prov_plot", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("farm_crop_plot_1", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("farm_crop_plot_2", label = 'Select Crop',
                                     choices = unique(farm_snd$type_of_crop), selected='Canola'),
                       selectizeInput('farm_var_plot', label = 'Select variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected = 'Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("farm_plot", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Notes',
                     
                     mainPanel(
                       dataTableOutput("farm_notes"),
                       width=12
                     )
                     
                 )
               )
               ),
               
               tabPanel(
                 
                 tr()$t('Canada supply and disposition of grains'),
                 
                 tabsetPanel(
                   
                   type = 'tabs',
                   
                   
                   tabPanel(
                     
                     'Data table',
                     
                     sidebarPanel(
                       selectizeInput("can_prov_table", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("can_crop_table", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("can_var_table", label = 'Select Variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected='Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         dataTableOutput('can_table')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Graphs',
                     
                     sidebarPanel(
                       selectizeInput("can_prov_plot", label = 'Select Province',
                                      choices = unique(farm_snd$geo), selected='Canada'),
                       selectizeInput("can_crop_plot_1", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='All wheat'),
                       selectizeInput("can_crop_plot_2", label = 'Select Crop',
                                      choices = unique(farm_snd$type_of_crop), selected='Canola'),
                       selectizeInput('can_var_plot', label = 'Select variable',
                                      choices = unique(farm_snd$farm_supply_and_disposition_of_grains), selected = 'Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("can_plot", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Notes',
                     
                     mainPanel(
                       dataTableOutput("can_notes"),
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