library(shiny)
library(dygraphs)
library(DBI)
library(stringr)
library(xts)
library(rhandsontable)
library(shinyWidgets)
library(plyr)

source('config.R')
source('appFunctions.R')

ui <- fluidPage(

  
  tags$head(
    ### icon for web server version
    #tags$link( rel="icon", type="image/png", href="layers.png", sizes="32x32" ),
    ### icon for standalone version
    # tags$link( rel="icon", type="image/png", href="http://localhost:1984/layers.png", sizes="32x32" ),
    # tags$head(tags$script(src = "message-handler.js")),
    tags$title("FSIG")
    #HTML("<img src=logos/CSIRO.gif style='vertical-align: top;'>&nbsp;&nbsp; Townsville Field Support and Instrumentation Group Sensor Network")
    
  ),
    headerPanel(HTML("<img src=logos/CSIRO.gif style='vertical-align: top;'>&nbsp;&nbsp; Townsville Field Support and Instrumentation Group Sensor Network")),

   
    sidebarLayout(
        sidebarPanel(width = 3,
            wellPanel(
              fluidRow( column(1, actionLink("show", "Login"))), 
              fluidRow( column(1, htmlOutput("loginStatus"), HTML('<br>'))),
                selectInput("pickStation", "Station Name", choices=NULL),
                selectInput("pickPlatform", "Platform", choices=NULL),
               # selectInput("pickDataStream", "Data Stream", choices=NULL, multiple=T),
              pickerInput(
                inputId = "pickDataStream", 
                label = "Data Stream/s", 
                choices = NULL, 
                options = list(
                  `actions-box` = TRUE, 
                  size = 8,
                  `selected-text-format` = "count > 3"
                ), 
                multiple = TRUE
              ),
                dateRangeInput('dateRange',
                               label = 'Date range input: yyyy-mm-dd',
                               start = Sys.Date()-numDaysToRetrieve, end = Sys.Date()),
              HTML('<br><br>'),
                actionButton("buttonGetData", "Get Data"), HTML('<br><br>'),
                downloadLink('downloadData', 'Download Data')
            )
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Chart", 
                        htmlOutput("errMsgBox", inline = T),
                        dygraphOutput("sensorChart1", width = "850", height = "500px") 
            ), tabPanel("Table",
                        rHandsontableOutput("dataStreamTable"))
          )
        )
    )
)


server <- function(input, output, session) {
  
  loginResult <- F 
  
    RV <- reactiveValues()
    RV$currentTS <- NULL
    RV$availableStations <- NULL
    RV$availablePlatforms <- NULL
    RV$availableDataStreams <- NULL
    RV$error <- NULL
    RV$Authenticated <- F
    RV$Usr <- NULL
 
    RV$Authenticated <- T

    #### Login to system   #########################
    
    adsLogin <- function(usr, pwd ){
      
      if(usr == uiName){
        if(pwd == uiPwd){
          return(T)
        }else{
          return(F)
        }
      }else{
        return(F)
      }
    }
    
    
    # Display information about selected data
    output$loginStatus <- renderText({
      if (!is.null(RV$Usr)){
        paste0(RV$Usr)
      }
    })
    
    Login <- function(failed = FALSE) {
      modalDialog(title = "FSIG Login", size = 's',
                  textInput("usrID", "User Name", placeholder = 'Demo'),
                  passwordInput("usrPwd", "Password", placeholder = 'Not Telling'),
                  HTML(paste0('For Information about obtaining a login contact <a href=mailto:', adminEmail, '?Subject=SMIPS&nbsp;Access>', adminName, '</a>')), 
                  tags$a(href=paste0("mailto:", adminEmail, "?Subject=Hello%20again", adminName)),
                  if (failed)
                    div(tags$b("Login failed", style = "color: red;")),
                  
                  footer = tagList(
                    modalButton("Cancel"),
                    actionButton("ok", "Login")
                  )
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$show, {
      showModal(Login())
    })
    
    observeEvent(input$ok, {
      
      if (!is.null(input$usrID) && nzchar(input$usrID) && !is.null(input$usrPwd) && nzchar(input$usrPwd) ) {
        
        loginResult <- adsLogin(usr = input$usrID, pwd = input$usrPwd )
        if(loginResult){
          
          RV$Usr <- input$usrID
          RV$Authenticated <- T
         
          removeModal()
          
        }else{
          showModal(Login(failed = TRUE))
        }
        
      } else {
        showModal(Login(failed = TRUE))
      }
    })
    
    
    
    
    
    
    
################  update the pick lists  ##########################    
    observe({
      req( RV$Authenticated)
    
      if( RV$Authenticated){
        RV$availableStations <- getStationsList()
        updateSelectInput(session, "pickStation", choices =  RV$availableStations)
      }
    })
    
    observe({

        req(input$pickStation)
        RV$availablePlatforms <- getPlatformList(input$pickStation)
        updateSelectInput(session, "pickPlatform", choices =  RV$availablePlatforms)
    })
    
    observe({

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


        tss <- vector("list", length(input$pickDataStream) )
        
        for (i in 1:length(input$pickDataStream)) {
          #print(i)
          fld <- input$pickDataStream[i]
          print(fld)
          df <- getDataStreamValues(stn, platF, fld, input$dateRange[1], input$dateRange[2])
          #print(str[ts])
          tss[[i]] <- df
        }
        
        noNulltss <- compact(tss)
       
        #print(head(allTs))

        
           # ts <- getDataStreamValues(stn, platF, input$pickDataStream, input$dateRange[1], input$dateRange[2])
           if(length(noNulltss) > 0){
             allTs <- do.call(merge, noNulltss)
               RV$currentTS <- allTs
               RV$error <- NULL
           }else{
               RV$currentTS <- NULL
               RV$error <- 'No data available in the specified date range'
           }
        
        #print(indexTZ(tss))
        #print(str(RV$currentTS))
        })
    })
    
      
    ################## Render the Chart  ##################
    output$sensorChart1 <- renderDygraph({
        
        if(!is.null(RV$currentTS)){
            
          
            isolate({
               # maxVal <- max(RV$currentTS)
              
              print(indexTZ(RV$currentTS))
              
                dygraph(RV$currentTS ,  main = "")%>%
                    #dyAxis("y", label = RV$currentSiteInfo$DataType, valueRange = c(0, maxVal)) %>%
                   # dyAxis("y", label = RV$currentSiteInfo$DataType) %>%
                    dyLegend(labelsSeparateLines = T) %>%
                    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = T, titleHeight = 26) %>%
                    dyRangeSelector()
            })
        }
    })
    
    #################  Error message text  ###########################
    output$errMsgBox <- renderText({
      paste0('<h4><span style="color: #ff0000;">', RV$error, '</span></h4>')
      
    })

    ##################  Render the data table ################## 
    output$dataStreamTable = renderRHandsontable({
      
      req(RV$currentTS)
      
      if(nrow(RV$currentTS) > 0){
        df <- data.frame(date=index(RV$currentTS), coredata(RV$currentTS))
        rhandsontable(df,   manualColumnResize = T, readOnly = TRUE, rowHeaders = F, width = 1450) 
      }else{
        return(NULL)
      }
    })
    
    ##################  Download the TS data   #####################
    
    output$downloadData <- downloadHandler(
        filename = function() {
         # colName <- getColumnNamefromIDs(input$pickStation,input$pickPlatform,input$pickDataStream)
          tabName <- getTableNamefromIDs(input$pickStation,input$pickPlatform)
          stationName <- getStationNamefromIDs(input$pickStation)
          colnames <- paste0(input$pickDataStream, collapse = "_")
          paste('FSIG-', stationName, '-', tabName, '-', colnames, '.csv', sep='')
        },
        content = function(con) {

          df <- data.frame(DateTime=index(RV$currentTS), coredata(RV$currentTS),row.names=NULL)
          write.csv(df, con, row.names = F)
        }
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
