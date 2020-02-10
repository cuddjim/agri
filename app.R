library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  uiOutput('page_content')#,
  # uiOutput('page_content_1')
  
)

translator <- Translator$new(translation_json_path = "translation")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # select and translate language
  tr <- reactive({
    
    selected <- 'fr'#input$selected_language
    
    if (length(selected) > 0 && selected == TRUE) {
      translator$set_translation_language('en')
    }
    
    else {
      
      translator$set_translation_language('fr')
      
    }
    
    translator
    
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
                     
                     'Bubble chart',
                     
                     sidebarPanel(
                       selectizeInput("province", label = 'Select Province',
                                      choices = c('a','b'), selected='a'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("bubble", height = '650px')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Line chart',
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("line", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Data stories',
                     
                     mainPanel(
                       dataTableOutput("storytable"),
                       width=12
                     )
                     
                   )
                   
                 )
               ),
               tabPanel(
                 
                 tr()$t('Farm supply and disposition of grains'),
                 
                 tabsetPanel(
                   
                   type='tabs',
                   
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
                     
                     'Bubble chart',
                     
                     sidebarPanel(
                       selectizeInput("province", label = 'Select Province',
                                      choices = c('a','b'), selected='a'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("bubble", height = '650px')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Line chart',
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("line", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Data stories',
                     
                     mainPanel(
                       dataTableOutput("storytable"),
                       width=12
                     )
                     
                   )
                   
                 )
               ),
               tabPanel(
                 
                 tr()$t('Supply and disposition of grains in Canada'),
                 
                 tabsetPanel(
                   
                   type='tabs',
                   
                   
                   tabPanel(
                     
                     'Bubble chart',
                     
                     sidebarPanel(
                       selectizeInput("province", label = 'Select Province',
                                      choices = c('a','b'), selected='a'),
                       width=3
                     ),
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("bubble", height = '650px')
                       ),
                       width=8
                     )
                     
                   ),
                   
                   tabPanel(
                     
                     'Line chart',
                     
                     mainPanel(
                       fluidRow(
                         plotlyOutput("line", height = '550px')
                       ),
                       width=8
                     )
                     
                   ),
                   tabPanel(
                     
                     'Data stories',
                     
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