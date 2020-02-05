# getCon <- function(){
#   
#   con <-  dbConnect(odbc::odbc(),
#                     Driver = 'ODBC Driver 13 for SQL Server',
#                     Server = 'mssql-prod12.it.csiro.au',
#                     Database = "TSV_LNDB",
#                     trusted_connection = 'yes',
#                     port = 1433
#   )
#   return(con)
# }

getCon <- function(){
  
  con <-  dbConnect(odbc::odbc(),
                    Driver = 'ODBC Driver 13 for SQL Server',
                    Server = dbServer,
                    Database = dbDatabase,
                    trusted_connection = 'no',
                    UID = dbUser,
                    PWD = dbPwd,
                    port = 1433
  )
  return(con)
}

runQuery <- function(sql){
  conn <- getCon()
  d <- dbGetQuery(conn, sql)
  dbDisconnect(conn)
  return(d)
}


getStations <- function(){
  conn <- getCon()
  d <- dbGetQuery(conn, 'SELECT * FROM LNDBStationMeta')
  dbDisconnect(conn)
  return(d$lnStationName)
}

getStationsList <- function(){
  conn <- getCon()
  d <- dbGetQuery(conn, 'SELECT stationID, lnStationName FROM LNDBStationMeta order by lnStationName')
  dbDisconnect(conn)
  stationList<- as.list(d$stationID)
  names(stationList) <- d$lnStationName
  return(stationList)
}

getPlatformList <- function(stationID){
  conn <- getCon()
  sql <- paste0("SELECT tableID, LNDBStationMeta_stationID, lnTableName, dbTableName FROM LNDBTableMeta where LNDBStationMeta_stationID = ", stationID, " order by lnTableName")

  d <- dbGetQuery(conn, sql)
  dbDisconnect(conn)
  platformList<- as.list(d$tableID)
  names(platformList) <- d$lnTableName
  return(platformList)
}

getDataStreamList <- function(stationID, platformID){
  conn <- getCon()
  sql <- paste0("SELECT columnID, LNDBStationMeta_stationID, LNDBTableMeta_tableID, lnColumnName, dbColumnName, process, units FROM LNDBColumnMeta 
                where LNDBStationMeta_stationID = ", stationID, ' and LNDBTableMeta_tableID = ', platformID , " order by lnColumnName")

  d <- dbGetQuery(conn, sql)

  dbDisconnect(conn)
  
  rows_to_keep = which(!d$dbColumnName %in% c('RecNum', 'TmStamp'))
  d2 <- d[rows_to_keep,]
  
  datastreamList<- as.list(d2$dbColumnName)
  names(datastreamList) <- d2$lnColumnName
  
  return(datastreamList[-c(1, 2)])
}

getDataStreamValues <- function(stationID, platformID, dataStreamID, startDate, endDate){
  conn <- getCon()

  colName <- getColumnNamefromIDs(stationID, platformID, dataStreamID)
  tabName <- getTableNamefromIDs(stationID, platformID)
  # get the timeseries
  sql <- paste0("SELECT TmStamp, ", colName, " FROM [", tabName,"] WHERE TmStamp >='", startDate, "' and TmStamp <='",endDate ,"'")
  res <- runQuery(sql)
  dbDisconnect(conn)
  
  print(paste0('nrows = ', nrow(res)))
  
  
  if(nrow(res) != 0){
  d <- as.POSIXct(str_trim(res[,1]) , format = "%Y-%m-%d %H:%M:%S")
  #ts <- xts(x=res[,2], unique = FALSE, order.by=d, tzone =  Sys.getenv("TZ"))
  ts <- xts(x=res[,2], unique = FALSE, order.by=d)
  names(ts) <- c(colName)
  
  #indexTZ(ts)<- Sys.timezone()
  indexTZ(ts)<- "Australia/Brisbane"

  return(ts)
  }else{
    return(NULL)
  }
  
  #print(head(ts))

 
}

getStationNamefromIDs<- function(stationID){
  conn <- getCon()
  sql <- paste0('SELECT stationID, lnStationName FROM LNDBStationMeta where stationID = ', stationID)
  d <- dbGetQuery(conn, sql) 
  dbDisconnect(conn)
  stationName<- d$lnStationName
  return(stationName)
}


getTableNamefromIDs<- function(stationID, platformID){
  conn <- getCon()
  sql <- paste0("SELECT tableID, LNDBStationMeta_stationID, lnTableName, dbTableName FROM LNDBTableMeta where LNDBStationMeta_stationID = ", stationID ,' and tableID = ',platformID)
  d <- dbGetQuery(conn, sql)
  dbDisconnect(conn)
  tabName <- d$dbTableName
  return(tabName)
}

getColumnNamefromIDs<- function(stationID, platformID, dataStreamID){
  
  conn <- getCon()
  # get the DB column name
  sql <- paste0("SELECT columnID, LNDBStationMeta_stationID, LNDBTableMeta_tableID, lnColumnName, dbColumnName, process, units FROM LNDBColumnMeta 
                  where LNDBStationMeta_stationID = ", stationID, " and LNDBTableMeta_tableID = ", platformID, " and lnColumnName =  '", dataStreamID, "'")
  d <- dbGetQuery(conn, sql)
  colName <- d$dbColumnName
  return(colName)
}
