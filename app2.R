
library(shiny); library(shiny.i18n)
library(leaflet); library(plotly)
library(DT); library(shinyWidgets)

sad_map = readOGR('CropsSADRegions_2017_Gen/CropsSADRegions_2017_Gen.shp',stringsAsFactors = FALSE)
sad_map@data$PRSADReg=as.numeric(sad_map@data$PRSADReg)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
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
  
  map_reactive <- reactive({
    
    # min_year = min(input$year); max_year = max(input$year)
    # map_commodity = input$map_commodity
    # min_emissions = str_c(map_commodity,'_emission_',min_year); max_emissions = str_c(map_commodity,'_emission_',max_year)
    # min_outputs = str_c(map_commodity,'_output_',min_year); max_outputs = str_c(map_commodity,'_output_',max_year)
    # 
    # prov_map@data %<>% 
    #   mutate(emissions=round(rowMeans(select(.,min_emissions:max_emissions),na.rm=TRUE),0),
    #          outputs=round(rowMeans(select(.,min_outputs:max_outputs),na.rm=TRUE),0)) %>% 
    #   mutate(scaled_outputs = log(1+outputs)^2.5) %>%
    #   mutate(mean_output = mean(scaled_outputs,na.rm=TRUE)) %>%
    #   mutate(outputs_1=23*scaled_outputs/mean_output)
    
    sad_map
    
  })
  
  output$plot <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 2,
                                     attributionControl=FALSE)) %>%
      setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
      addProviderTiles(providers$Wikimedia,
                       options = providerTileOptions(opacity = 0.8)) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = map_reactive(),
                  fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), PRSADReg, 5)(PRSADReg),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4)
    
  })
  
  farm_snd_table <- reactive({
    
    selected_geo = input$province_farm
    selected_crop = input$crop_farm
    selected_variable = input$var_farm
    
    list_of_sets[['can_snd']] %>% spread(ref_date, value) %>% 
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>% 
      filter(
        geo == selected_geo, 
        type_of_crop == selected_crop,
        farm_supply_and_disposition_of_grains == selected_variable) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=farm_supply_and_disposition_of_grains)
    
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
                               "$(this.api().table().header()).css({'font-size': '12px', 'background-color': '#03085B', 'color': '#fff'});",
                               "}")))
    
  })
  
  farm_plot_reactive_1 <- reactive({
    
    
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_1 = input$farm_prov_plot_1
    farm_plot_var = input$farm_var_plot
    
    list_of_sets[['can_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_1,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  farm_plot_reactive_2 <- reactive({
    
    
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_2 = input$farm_prov_plot_2
    farm_plot_var = input$farm_var_plot
    
    list_of_sets[['can_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_2,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  farm_plot_reactive_3 <- reactive({
    
    
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_3 = input$farm_prov_plot_3
    farm_plot_var = input$farm_var_plot
    
    list_of_sets[['can_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_3,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  
  
  output$farm_plot <- renderPlotly({
    
    xaxis <- list(title = "",
                  showline = TRUE,
                  showgrid = FALSE,
                  showticklabels = TRUE,
                  linecolor = 'rgb(204, 204, 204)',
                  linewidth = 2,
                  autotick = FALSE,
                  ticks = 'outside',
                  tickcolor = 'rgb(204, 204, 204)',
                  tickwidth = 2,
                  ticklen = 5,
                  tickfont = list(family = 'Arial',
                                  size = 12,
                                  color = 'rgb(82, 82, 82)'))
    
    yaxis <- list(title = "",
                  showgrid = FALSE,
                  zeroline = FALSE,
                  showline = FALSE,
                  showticklabels = FALSE)
    
    margin <- list(autoexpand = T,
                   l = 100,
                   r = 100,
                   t = 110)
    
    farm_plot_reactive_1_ann_1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = farm_plot_reactive_1()$value[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste(format(farm_plot_reactive_1()$value[1],big.mark = ','),'tonnes'),
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    farm_plot_reactive_1_ann_2 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.95,
      y = farm_plot_reactive_1()$value[nrow(farm_plot_reactive_1())],
      xanchor = 'left',
      yanchor = 'middle',
      text = ~paste(format(farm_plot_reactive_1()$value[nrow(farm_plot_reactive_1())],big.mark = ','),'tonnes'),
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    farm_plot_reactive_2_ann_1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = farm_plot_reactive_2()$value[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste(format(farm_plot_reactive_2()$value[1],big.mark = ','),'tonnes'),
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    farm_plot_reactive_2_ann_2 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.95,
      y = farm_plot_reactive_2()$value[nrow(farm_plot_reactive_2())],
      xanchor = 'left',
      yanchor = 'middle',
      text = ~paste(format(farm_plot_reactive_2()$value[nrow(farm_plot_reactive_2())],big.mark = ','),'tonnes'),
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    farm_plot_reactive_3_ann_1 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.05,
      y = farm_plot_reactive_3()$value[1],
      xanchor = 'right',
      yanchor = 'middle',
      text = ~paste(format(farm_plot_reactive_3()$value[1],big.mark = ','),'tonnes'),
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    farm_plot_reactive_3_ann_2 <- list(
      xref = 'paper',
      yref = 'y',
      x = 0.95,
      y = farm_plot_reactive_3()$value[nrow(farm_plot_reactive_3())],
      xanchor = 'left',
      yanchor = 'middle',
      text = ~paste(format(farm_plot_reactive_3()$value[nrow(farm_plot_reactive_3())],big.mark = ','),'tonnes'),
      font = list(family = 'Arial',
                  size = 16,
                  color = 'rgba(67,67,67,1)'),
      showarrow = FALSE)
    
    plot_ly() %>%
      add_trace(data = farm_plot_reactive_1(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', name = farm_plot_crop_1,
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'),
                line = list(color = 'rgba(67,67,67,1)', width = 5)) %>%
      add_trace(data = farm_plot_reactive_2(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', name = farm_plot_crop_2,
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'),
                line = list(color = 'rgba(49,130,189,1)', width = 5)) %>% 
      add_trace(data = farm_plot_reactive_3(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', name = farm_plot_crop_3,
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'),
                line = list(color = 'rgba(25,180,126,1)', width = 5)) %>% 
      add_trace(x = ~c(farm_plot_reactive_1()$crop_year[1], farm_plot_reactive_1()$crop_year[nrow(farm_plot_reactive_1())]), y = ~c(farm_plot_reactive_1()$value[1], farm_plot_reactive_1()$value[nrow(farm_plot_reactive_1())]), 
                hoverinfo = 'text', text = ~paste(format(farm_plot_crop_1)),type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', size = 10), showlegend = FALSE) %>%
      add_trace(x = ~c(farm_plot_reactive_2()$crop_year[1], farm_plot_reactive_2()$crop_year[nrow(farm_plot_reactive_2())]), y = ~c(farm_plot_reactive_2()$value[1], farm_plot_reactive_2()$value[nrow(farm_plot_reactive_2())]), 
                hoverinfo = 'text', text = ~paste(format(farm_plot_crop_2)),type = 'scatter', mode = 'markers', marker = list(color = 'rgba(49,130,189, 1)', size = 10), showlegend = FALSE) %>% 
      add_trace(x = ~c(farm_plot_reactive_3()$crop_year[1], farm_plot_reactive_3()$crop_year[nrow(farm_plot_reactive_3())]), y = ~c(farm_plot_reactive_3()$value[1], farm_plot_reactive_3()$value[nrow(farm_plot_reactive_3())]), 
                hoverinfo = 'text', text = ~paste(format(farm_plot_crop_3)),type = 'scatter', mode = 'markers', marker = list(color = 'rgba(25,180,126,1)', size = 10), showlegend = FALSE) %>% 
      layout(title = paste0('<b>Comparing ',input$farm_prov_plot_1,', ',input$farm_prov_plot_2,' and ',input$farm_prov_plot_3,' ',input$farm_var_plot,' in ',input$farm_prov_plot,'</b>'), xaxis = xaxis, yaxis = yaxis, margin = margin,
             autosize = T,
             showlegend = T,
             annotations = farm_plot_reactive_1_ann_1) %>%
      layout(annotations = farm_plot_reactive_2_ann_1) %>%
      layout(annotations = farm_plot_reactive_3_ann_1) %>% 
      layout(annotations = farm_plot_reactive_1_ann_2) %>%
      layout(annotations = farm_plot_reactive_2_ann_2) %>% 
      layout(annotations = farm_plot_reactive_3_ann_2)
      
    
  })
  
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
                                      choices = unique(list_of_sets[['can_snd']]$geo), selected='Canada'),
                       selectizeInput("crop_farm", label = 'Select Crop',
                                      choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='All wheat'),
                       selectizeInput("var_farm", label = 'Select Variable',
                                      choices = unique(list_of_sets[['can_snd']]$farm_supply_and_disposition_of_grains), selected='Production'),
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
                       selectizeInput("farm_prov_plot", label = 'Select Province',
                                      choices = unique(list_of_sets[['can_snd']]$geo), selected='Canada'),
                       selectizeInput("farm_prov_plot_1", label = 'Select Crop',
                                      choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='All wheat'),
                       selectizeInput("farm_prov_plot_2", label = 'Select Crop',
                                     choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='Canola'),
                       selectizeInput("farm_prov_plot_3", label = 'Select Crop',
                                      choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='Barley'),
                       selectizeInput('farm_var_plot', label = 'Select variable',
                                      choices = unique(list_of_sets[['can_snd']]$farm_supply_and_disposition_of_grains), selected = 'Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("farm_plot", height = '550px')
                       ),
                       width=9
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