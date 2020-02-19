
library(shiny); library(shiny.i18n)
library(leaflet); library(plotly)
library(DT); library(shinyWidgets)
library(shinythemes)

# 1 create plots for tabs 1 and 3
# add footnotes to tabs 1,2 and 3
# add translation for tabs + snd names, etc
# add centroids to map, add snd variable to select


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  # tags$style(HTML("
  #     .navbar .navbar-nav {float: right; 
  #                 color: #ff3368; 
  #                 font-size: 20px; 
  #                 background-color: #FFFF00 ; } 
  #                 .navbar.navbar-default.navbar-static-top{ color: #ff3368; 
  #                 font-size: 20px; 
  #                 background-color: #FFFF00 ;}
  #                 .navbar .navbar-header {float: left; } 
  #                 .navbar-default .navbar-brand { color: #ff3368; 
  #                 font-size: 38px; 
  #                 background-color: #FFFF00 ;} 
  #                 ")),
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
    
    cp_year = input$cp_map_year
    cp_map_crop = input$cp_map_crop
    map_disp_1 = str_c(cp_map_crop,'_production_',cp_year)
    map_disp_2 = str_c(cp_map_crop,'_harvested_area_',cp_year)
    
    sad_map@data %<>% 
      left_join(list_of_sets[['grain_area']] %>%
                  unite(variable,c('type_of_crop','harvest_disposition','ref_date')) %>%
                  select(variable,value,sad_code,geo) %>%
                  spread(variable,value) %>%
                  mutate(sad_code = as.character(sad_code)), by=c('PRSADReg'='sad_code')
      ) %>%
      rename(disp_1={{map_disp_1}}) %>%
      mutate(disp_1=replace_na(disp_1,0))
    
    sad_map
    
    
  })
  
  output$cp_map <- renderLeaflet({
    
    color_pal <- colorNumeric(palette = "RdYlBu", domain = cp_map_reactive()$disp_1, reverse = FALSE)
    
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 2,
                                     attributionControl=FALSE)) %>%
      setView(lng = -98.4, lat = 58.2, zoom = 4) %>%
      addProviderTiles(providers$Wikimedia,
                       options = providerTileOptions(opacity = 0.8)) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = cp_map_reactive(),
                  fillColor = ~colorBin(c("RdYlBu"), disp_1, 5)(disp_1),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4) %>%
      addLegend(data = cp_map_reactive(), "bottomleft", pal = color_pal, values = ~disp_1,
                title = "Production",
                opacity = 0.8)
    
  })

  # data table
  cp_table_reactive <- reactive({

    cp_prov_table = input$cp_prov_table
    cp_crop_table = input$cp_crop_table
    cp_var_table = input$cp_var_table

    list_of_sets[['grain_area']] %>% spread(ref_date, value) %>%
      select(c('geo','type_of_crop','harvest_disposition',ends_with('07'),ends_with('03'),ends_with('12'))) %>%
      filter(
        geo == cp_prov_table,
        type_of_crop == cp_crop_table,
        harvest_disposition %in% cp_var_table) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=harvest_disposition) %>%
      mutate(
        Variable = toTitleCase(gsub('_',' ',Variable)),
        Crop = toTitleCase(gsub('_',' ',Crop))
      )

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

    list_of_sets[['grain_area']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','harvest_disposition','value')) %>%
      filter(geo == cp_plot_geo,type_of_crop == cp_plot_crop_1,
             harvest_disposition == cp_plot_var)

  })

  cp_plot_reactive_2 <- reactive({

    cp_plot_geo = input$cp_prov_plot
    cp_plot_crop_2 = input$cp_crop_plot_2
    cp_plot_var = input$cp_var_plot

    list_of_sets[['grain_area']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','harvest_disposition','value')) %>%
      filter(geo == cp_plot_geo,type_of_crop == cp_plot_crop_2,
             harvest_disposition == cp_plot_var)

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
    
    min_year = min(input$farm_map_year); max_year = max(input$farm_map_year)
    farm_map_crop = input$farm_map_crop
    min_map_snd_1 = str_c(farm_map_crop,'_production_',min_year,'-07'); max_map_snd_1 = str_c(farm_map_crop,'_production_',max_year,'-07')
    min_map_snd_2 = str_c(farm_map_crop,'_deliveries_',min_year,'-07'); max_map_snd_2 = str_c(farm_map_crop,'_deliveries_',max_year,'-07')
    
    prov_map@data %<>% 
      left_join(list_of_sets[['farm_snd']] %>%
                  unite(variable,c('type_of_crop','farm_supply_and_disposition_of_grains','ref_date')) %>%
                  select(variable,value,geo) %>%
                  spread(variable,value) %>%
                  select(geo,ends_with('07')), by=c('NAME'='geo')
      ) %>%
      mutate(
        snd_1=round(rowMeans(select(.,min_map_snd_1:max_map_snd_1),na.rm=TRUE),0),
        snd_2=round(rowMeans(select(.,min_map_snd_2:max_map_snd_2),na.rm=TRUE),0)
        )
    
    prov_map
    
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
                  fillColor = ~colorBin(c("#E1F5C4","#EDE574","#F9D423","#FC913A","#FF4E50"), snd_1, 5)(snd_1),
                  color = "#BDBDC3",
                  fillOpacity = 0.7,
                  weight = 4) %>%
      addLegend(data = farm_map_reactive(), "bottomleft", pal = color_pal, values = ~disp_1,
                title = "Production",
                opacity = 0.8)
    
  })
  
  # data table
  farm_table_reactive <- reactive({
    
    farm_prov_table = input$farm_prov_table
    farm_crop_table = input$farm_crop_table
    # farm_var_table = input$farm_var_table
    
    list_of_sets[['farm_snd']] %>% spread(ref_date, value) %>% 
      select(c('geo','type_of_crop','farm_supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>% 
      filter(
        geo %in% farm_prov_table, 
        type_of_crop == farm_crop_table) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=farm_supply_and_disposition_of_grains) %>%
      mutate(
        Variable = toTitleCase(gsub('_',' ',Variable)),
        Crop = toTitleCase(gsub('_',' ',Crop))
        )
    
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
    
    list_of_sets[['farm_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_1,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  farm_plot_reactive_2 <- reactive({
    
    
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_2 = input$farm_crop_plot_2
    farm_plot_var = input$farm_var_plot
    
    list_of_sets[['farm_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','farm_supply_and_disposition_of_grains','value')) %>%
      filter(geo == farm_plot_geo,type_of_crop == farm_plot_crop_2,
             farm_supply_and_disposition_of_grains == farm_plot_var)
    
  })
  
  farm_plot_reactive_3 <- reactive({
    
    
    farm_plot_geo = input$farm_prov_plot
    farm_plot_crop_3 = input$farm_crop_plot_3
    farm_plot_var = input$farm_var_plot
    
    list_of_sets[['farm_snd']] %>% filter(grepl('-07',ref_date)) %>%
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
    
    margin <- list(autoexpand = F,
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
      text = ~paste(format_lang(farm_plot_reactive_1()$value[1],input$selected_language),'tonnes'),
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
      text = ~paste(format_lang(farm_plot_reactive_1()$value[nrow(farm_plot_reactive_1())],input$selected_language),'tonnes'),
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
                type = 'scatter',mode = 'lines', name = input$farm_crop_plot_1,
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'),
                line = list(color = 'rgba(67,67,67,1)', width = 5)) %>%
      add_trace(data = farm_plot_reactive_2(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', name = input$farm_crop_plot_2,
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'),
                line = list(color = 'rgba(49,130,189,1)', width = 5)) %>% 
      add_trace(data = farm_plot_reactive_3(),
                x = ~crop_year,y = ~value,
                type = 'scatter',mode = 'lines', name = input$farm_crop_plot_3,
                hoverinfo = 'text', text = ~paste(format(round(value, 0),
                                                         big.mark = ',',
                                                         scientific = F),'tonnes'),
                line = list(color = 'rgba(25,180,126,1)', width = 5)) %>% 
      add_trace(x = ~c(farm_plot_reactive_1()$crop_year[1], farm_plot_reactive_1()$crop_year[nrow(farm_plot_reactive_1())]), y = ~c(farm_plot_reactive_1()$value[1], farm_plot_reactive_1()$value[nrow(farm_plot_reactive_1())]), 
                hoverinfo = 'text', text = ~paste(format(input$farm_crop_plot_1)),
                type = 'scatter', mode = 'markers', marker = list(color = 'rgba(67,67,67,1)', size = 10), showlegend = FALSE) %>%
      add_trace(x = ~c(farm_plot_reactive_2()$crop_year[1], farm_plot_reactive_2()$crop_year[nrow(farm_plot_reactive_2())]), y = ~c(farm_plot_reactive_2()$value[1], farm_plot_reactive_2()$value[nrow(farm_plot_reactive_2())]), 
                hoverinfo = 'text', text = ~paste(format(input$farm_crop_plot_2)),
                type = 'scatter', mode = 'markers', marker = list(color = 'rgba(49,130,189, 1)', size = 10), showlegend = FALSE) %>% 
      add_trace(x = ~c(farm_plot_reactive_3()$crop_year[1], farm_plot_reactive_3()$crop_year[nrow(farm_plot_reactive_3())]), y = ~c(farm_plot_reactive_3()$value[1], farm_plot_reactive_3()$value[nrow(farm_plot_reactive_3())]), 
                hoverinfo = 'text', text = ~paste(format(input$farm_crop_plot_3)),
                type = 'scatter', mode = 'markers', marker = list(color = 'rgba(25,180,126,1)', size = 10), showlegend = FALSE) %>% 
      layout(title = paste0('<b>Comparing ',toTitleCase(input$farm_crop_plot_1),', ',toTitleCase(input$farm_crop_plot_2),' and ',toTitleCase(input$farm_crop_plot_3),' ',toTitleCase(gsub('_',' ',input$farm_var_plot)),' in ',input$farm_prov_plot,'</b>'), 
             xaxis = xaxis, yaxis = yaxis, margin = margin,
             autosize = T,
             showlegend = T, legend=list(x=min(farm_plot_reactive_3()$crop_year),y=max(farm_plot_reactive_3()$value,farm_plot_reactive_2()$value,farm_plot_reactive_1()$value)),
             annotations = farm_plot_reactive_1_ann_1) %>%
      layout(annotations = farm_plot_reactive_2_ann_1) %>%
      layout(annotations = farm_plot_reactive_3_ann_1) %>% 
      layout(annotations = farm_plot_reactive_1_ann_2) %>%
      layout(annotations = farm_plot_reactive_2_ann_2) %>% 
      layout(annotations = farm_plot_reactive_3_ann_2)
    
    
  })
  
  
  ## national supply and disposition tab
  
  # data table
  can_table_reactive <- reactive({
    
    can_prov_table = input$can_prov_table
    can_crop_table = input$can_crop_table
    can_var_table = input$can_var_table
    
    list_of_sets[['can_snd']] %>% spread(ref_date, value) %>%
      select(c('geo','type_of_crop','supply_and_disposition_of_grains',ends_with('07'),ends_with('03'),ends_with('12'))) %>%
      filter(
        geo == can_prov_table,
        type_of_crop == can_crop_table,
        supply_and_disposition_of_grains %in% can_var_table) %>%
      rename(Province=geo,Crop=type_of_crop,Variable=supply_and_disposition_of_grains)
    
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
    
    list_of_sets[['can_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','supply_and_disposition_of_grains','value')) %>%
      filter(type_of_crop == can_crop_plot_1,
             supply_and_disposition_of_grains == can_var_plot)
    
  })
  
  can_plot_reactive_2 <- reactive({
    
    can_crop_plot_2 = input$can_crop_plot_2
    can_var_plot = input$can_var_plot
    
    list_of_sets[['can_snd']] %>% filter(grepl('-07',ref_date)) %>%
      mutate(crop_year = as.numeric(str_sub(ref_date,1,4))) %>%
      select(c('crop_year','geo','type_of_crop','supply_and_disposition_of_grains','value')) %>%
      filter(type_of_crop == can_crop_plot_2,
             supply_and_disposition_of_grains == can_var_plot)
    
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
        
        tr()$t('Language'),
        
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
                                tr()$t("Crops and grains in Canada"),
                                style = "position: relative; top: 80%; transform: translateY(10%);")),
                 
               tabPanel(
                 
                 tr()$t('Crop Production'),
                 
                 tabsetPanel(
                   
                   type = 'tabs',
                   
                   tabPanel(
                     
                     'Map',
                     
                     sidebarPanel(
                       selectizeInput("cp_map_crop", label = tr()$t('Select Fuel Type'),
                                      choices = setNames(grain_area_crop,grain_area_crop_names),
                                      selected = 'wheat'),
                       sliderInput("cp_map_year", label = tr()$t('Select Year:'),2005, 2018, value=2018,
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
                     
                     tr()$t('Data Table'),
                     
                     sidebarPanel(
                       selectizeInput("cp_prov_table", label = tr()$t('Select Province'),
                                      choices = unique(list_of_sets[['grain_area']]$geo), selected='Canada'),
                       selectizeInput("cp_crop_table", label = tr()$t('Select Crop'),
                                      choices = setNames(grain_area_crop,grain_area_crop_names), selected='All wheat'),
                       selectizeInput("cp_var_table", label = tr()$t('Select Variable'),
                                      choices = setNames(grain_area_disp,grain_area_disp_names), 
                                      multiple=TRUE, selected=c('production','seeded_area','harvested_area','average_yield')),
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
                     
                     tr()$t('Graphs'),
                     
                     sidebarPanel(
                       selectizeInput("cp_prov_plot", label = tr()$t('Select Province'),
                                      choices = unique(list_of_sets[['grain_area']]$geo), selected='Canada'),
                       selectizeInput("cp_crop_plot_1", label = tr()$t('Select Crop'),
                                      choices = setNames(grain_area_crop,grain_area_crop_names), selected='Wheat'),
                       selectizeInput("cp_crop_plot_2", label = tr()$t('Select Crop'),
                                      choices = setNames(grain_area_crop,grain_area_crop_names), selected='Canola'),
                       selectizeInput('cp_var_plot', label = tr()$t('Select variable'),
                                      choices = setNames(grain_area_disp,grain_area_disp_names), selected = 'Production'),
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
                       selectizeInput("farm_map_crop", label = tr()$t('Select Fuel Type'),
                                      choices = tr()$t(setNames(farm_snd_crop,farm_snd_crop_names)),
                                      selected = 'barley'),
                       sliderInput("farm_map_year", label = tr()$t('Select Year:'),2005, 2018, value=2018,
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
                       selectizeInput("farm_prov_table", label = tr()$t('Select Province'),
                                      choices = unique(list_of_sets[['farm_snd']]$geo), multiple = TRUE, selected='Canada'),
                       selectizeInput("farm_crop_table", label = tr()$t('Select Crop'),
                                      choices = setNames(farm_snd_crop,farm_snd_crop_names), selected='All wheat'),
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
                       selectizeInput("farm_prov_plot", label = tr()$t('Select Province'),
                                      choices = unique(list_of_sets[['farm_snd']]$geo), selected='Canada'),
                       selectizeInput("farm_crop_plot_1", label = tr()$t('Select Crop'),
                                      choices = setNames(farm_snd_crop,farm_snd_crop_names), selected='wheat'),
                       selectizeInput("farm_crop_plot_2", label = tr()$t('Select Crop'),
                                     choices = setNames(farm_snd_crop,farm_snd_crop_names), selected='canola'),
                       selectizeInput("farm_crop_plot_3", label = tr()$t('Select Crop'),
                                      choices = setNames(farm_snd_crop,farm_snd_crop_names), selected='barley'),
                       selectizeInput('farm_var_plot', label = tr()$t('Select Variable'),
                                      choices = setNames(farm_snd_disp,farm_snd_disp_names), selected = 'Production'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("farm_plot", height = '450px')
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
                       selectizeInput("can_prov_table", label = tr()$t('Select Province'),
                                      choices = unique(list_of_sets[['can_snd']]$geo), selected='Canada'),
                       selectizeInput("can_crop_table", label = tr()$t('Select Crop'),
                                      choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='All wheat'),
                       selectizeInput("can_var_table", label = tr()$t('Select Variable'),
                                      choices = unique(list_of_sets[['can_snd']]$supply_and_disposition_of_grains), multiple = T, selected=c('Total supplies','Production','Total beginning stocks','Imports',
                                                                                                                                             'Total disposition','Total exports','Total ending stocks')),
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
                       selectizeInput("can_prov_plot", label = tr()$t('Select Province'),
                                      choices = unique(list_of_sets[['can_snd']]$geo), selected='Canada'),
                       selectizeInput("can_crop_plot_1", label = tr()$t('Select Crop'),
                                      choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='All wheat'),
                       selectizeInput("can_crop_plot_2", label = tr()$t('Select Crop'),
                                      choices = unique(list_of_sets[['can_snd']]$type_of_crop), selected='Canola'),
                       selectizeInput('can_var_plot', label = tr()$t('Select Variable'),
                                      choices = unique(list_of_sets[['can_snd']]$supply_and_disposition_of_grains), selected = 'Production'),
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