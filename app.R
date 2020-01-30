library(shiny)
library(dygraphs)
library(DBI)
library(stringr)
library(xts)
library(rhandsontable)

source('config.R')
source('appFunctions.R')

ui <- fluidPage(

    headerPanel(fluidPage(HTML("<img src=logos/CSIRO.gif style='vertical-align: top;'>&nbsp;&nbsp; Townsville Field Support and Instrumentation Group Sensor Network"))),

    sidebarLayout(
        sidebarPanel(width = 3,
            wellPanel(
                
                selectInput("pickStation", "Station Name", choices=NULL),
                selectInput("pickPlatform", "Platform", choices=NULL),
                selectInput("pickDataStream", "Data Stream", choices=NULL),
                dateRangeInput('dateRange',
                               label = 'Date range input: yyyy-mm-dd',
                               start = as.Date(defaultStartDate), end = Sys.Date()),
                actionButton("buttonGetData", "Get Data"), HTML('<br>,<br>'),
                downloadLink('downloadData', 'Download Data')
            )
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Chart", 
                        htmlOutput("errMsgBox", inline = T),
                        dygraphOutput("sensorChart1", width = "850", height = "300px")
            ), tabPanel("Table",
                        rHandsontableOutput("dataStreamTable"))
          )
        )
    )
)


server <- function(input, output, session) {
    
    RV <- reactiveValues()
    RV$currentTS <- NULL
    RV$availableStations <- NULL
    RV$availablePlatforms <- NULL
    RV$availableDataStreams <- NULL
    RV$error <- NULL
    
    
################  update the pick lists  ##########################    
    observe({
        RV$availableStations <- getStationsList()
        updateSelectInput(session, "pickStation", choices =  RV$availableStations)
    })
    
    observe({

        req(input$pickStation)
        RV$availablePlatforms <- getPlatformList(input$pickStation)
        updateSelectInput(session, "pickPlatform", choices =  RV$availablePlatforms)
    })
    
    observe({
      print('Datastream')
        req(input$pickStation, input$pickPlatform)
        RV$availableDataStreams <- getDataStreamList(input$pickStation, input$pickPlatform)
        updateSelectInput(session, "pickDataStream",choices =  RV$availableDataStreams)
    })
    
    
################  Get the Data ####################################
    observeEvent(input$buttonGetData, {
        req(input$pickDataStream)
        req(input$pickPlatform)
        req(input$pickDataStream)
        
        isolate({
        stn <- input$pickStation
        platF <- input$pickPlatform 

           ts <- getDataStreamValues(stn, platF, input$pickDataStream, input$dateRange[1], input$dateRange[2])
           if(nrow(ts) > 0){
               RV$currentTS <- ts
               RV$error <- NULL
           }else{
               RV$currentTS <- NULL
               RV$error <- 'No data available in the specified date range'
           }
        })
    })
    
      
    ################## Render the Chart  ##################
    output$sensorChart1 <- renderDygraph({
        
        if(!is.null(RV$currentTS)){
            
            isolate({
                maxVal <- max(RV$currentTS)
                dygraph(RV$currentTS ,  main = "")%>%
                    dyAxis("y", label = RV$currentSiteInfo$DataType, valueRange = c(0, maxVal)) %>%
                    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
                    dyRangeSelector()
            })
        }
    })
    
    #################  Eroor message text  ###########################
    output$errMsgBox <- renderText({
      paste0('<h4><span style="color: #ff0000;">', RV$error, '</span></h4>')
      
    })

    ##################  Render the data table ################## 
    output$dataStreamTable = renderRHandsontable({
      
      req(RV$currentTS)
      
      if(nrow(RV$currentTS) > 0){
        df <- data.frame(date=index(RV$currentTS), coredata(RV$currentTS))
        rhandsontable(df,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F, width = 450) 
      }else{
        return(NULL)
      }
    })
    
    ##################  Download the TS data   #####################
    
    output$downloadData <- downloadHandler(
        filename = function() {
          paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          df <- data.frame(date=index(RV$currentTS), coredata(RV$currentTS))
          write.csv(df, con, row.names = F)
        }
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
