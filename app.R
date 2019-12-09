##################

#This is an application designed to help the Abwasser team quick graph and represent data to determine the effectiveness of patented 
#pulse emission technology for water sterilization 

#Author: Michael Mykins 

# The Author acknowledges recommendations by Dr. Andrew Steen on Code style and syntax 


library(shiny)
library(plotly)
library(datasets)

#min.x = 500 # set the minimum wavelength to 500
#max.x = 750 # set the maximum wavelength to 750

ui <- shinyUI(fluidPage(
    titlePanel("Abwasser Fluorescent Intensity Application"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Uploading Files"),
                 sidebarLayout(
                     sidebarPanel(
                         
                         fileInput('file1', 'Choose CSV File', # file input can accept text and csv files separated by comma, semicolon or tab
                                   accept = c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         
                         # design interface came with help from the rstudio tutorial for file uploading 
                         # http://shiny.rstudio.com/gallery/file-upload.html
                         tags$br(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator', # give the user the option to separate their data based on the file extension 
                                      c(Comma = ',',
                                        Semicolon = ';',
                                        Tab = '\t'),
                                      ','),
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        
        tabPanel("Plots for file upload",
                 pageWithSidebar(
                     headerPanel(''),
                     sidebarPanel(
                         
                         # "Empty inputs" - they will be updated after the data is uploaded by the user
                         selectInput('column1', 'X Variable', ""),
                         selectInput('column2', 'Y Variable', "", selected = ""),
                         # sliderInput('wave.adjuster',
                         #             "Wavelength",
                         #             min = min.x,
                         #             max = max.x, 
                         #             value = c(min.x, max.x),
                         #             step=1)
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
    
    
    data <- reactive({ # everytime a new file is uploaded, run through the code again
        req(input$file1) ## req requires that the input is available
        inFile <- input$file1 # inputted file is saved an object that will be read as a dataframe 
    
        df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
        
        
        # Update inputs 
        
        updateSelectInput(session, inputId = 'column1', label = 'Select desired X axis', # monitor the user input so that x and y axis can be changed
                          choices = names(df), selected = names(df))
        updateSelectInput(session, inputId = 'column2', label = 'Select desired Y axis',
                          choices = names(df), selected = names(df)[2])
    
        # 
        # 
        # low.x <- input$wave.adjuster[1]
        # high.x <- input$wave.adjuster[2]
        # 
        # observeEvent(input$sliderInput,
        #              
        #     {updateSliderInput(session, inputId = 'xcol')
        #     }
        # )
                
        #min.x = min(df$xcol)
        #max.x = max(df$xcol)
        
       df # return the data frame 
    })
        
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlotly({
        
        # build graph with ggplot syntax
        p <- ggplot(data(), aes_string(x = input$column1, y = input$column2, color = "id")) + # make a graph showing the desired paramaters as a point plot of X and Y 
            geom_point()
            
        
        ggplotly(p) %>% 
            layout(height = input$plotHeight, autosize=TRUE)
        
    })
})

shinyApp(ui, server)
