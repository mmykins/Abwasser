library(shiny)
library(plotly)
library(datasets)



ui <- shinyUI(fluidPage(
    titlePanel("Column Plot"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # added interface for uploading data from
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        
        tabPanel("First Type",
                 pageWithSidebar(
                     headerPanel('My First Plot'),
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded
                         selectInput('xcol', 'X Variable', ""),
                         selectInput('ycol', 'Y Variable', "", selected = ""),
                         p("Select desired X parameter"),
                         sliderInput('wave.adjuster',
                                     "Wavelength",
                                     min = min.x,
                                     max = max.x, 
                                     value = c(min.x, max.x),
                                     step=1)
                     ),
                     
                     mainPanel(
                         plotlyOutput('MyPlot', height = "900px")
                     )
                 )
        )
        
    )
)
)

server <- shinyServer(function(input, output, session) {
    # added "session" because updateSelectInput requires it
    
    
    data <- reactive({ 
        req(input$file1) ## ?req #  require that the input is available
        inFile <- input$file1 
    
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                       quote = input$quote)
        
        
        # Update inputs (you could create an observer with both updateSel...)
        # You can also constraint your choices. If you wanted select only numeric
        # variables you could set "choices = sapply(df, is.numeric)"
        # It depends on what do you want to do later on.
        
        updateSelectInput(session, inputId = 'xcol', label = 'Select desired X parameter',
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'ycol', label = 'Select desired Y parameter',
                          choices = names(df), selected = names(df)[2])
        
        low.x <- input$wave.adjuster[1]
        high.x <- input$wave.adjuster[2]
        
        #observeEvent(input$sliderInput,
                     
            #{updateSliderInput(session, inputId = 'xcol')
            #}
       # )
                
        min.x = min(df$xcol)
        max.x = max(df$xcol)
        
        return(df)
    })
        
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlotly({
        
        # build graph with ggplot syntax
        p <- ggplot(data(), aes_string(x = input$xcol, y = input$ycol, color = "id")) + 
            geom_point()
            
        
        ggplotly(p) %>% 
            layout(height = input$plotHeight, autosize=TRUE)
        
    })
})

shinyApp(ui, server)
