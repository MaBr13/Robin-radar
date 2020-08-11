########## CUSTOM FUNCTIONS ########## 
## Jens van Erp
## List of Custom functions


# GET TIDES 
getTides <- function(tidesFile){
  ## Jens van Erp
  ## 17 March 2020
  ## Function to retrieve the high/low tide moments from the raw xml file
  ## XML's of specifick stations can be downloaded from https://getij.rws.nl/
  
  # Load in the data
  tidesXML <- xmlTreeParse(file = tidesFile)
  
  # COnvert XML to a list
  tidesList <- xmlSApply(xmlRoot(tidesXML),
                         function(x) xmlSApply(x, xmlToList))
  
  #Convert list into a dt
  tidesDb <- as.data.table(tidesList$values)
  # Transpose
  tidesDb <- data.table(t(tidesDb))               
  
  #Give the columns the right names
  setnames(tidesDb, c("V1","V2","V3"),c("timestamp","tide","waterheight"))
  
  # All values are still sepperate lists, so unlist them
  tidesDb[,tide:=unlist(tide)]
  tidesDb[,waterheight:=unlist(waterheight)]
  
  # Make waterheight a double
  tidesDb[,waterheight:=as.double(waterheight)]
  
  # For some reason a number of the timestamps is not a list with a single value but a branched list. 
  # Unlisting would create a vector larger than the number of rows and would crash R. 
  # To circumvent this, extract the right item of those lists if the timestamp is not a character
  # Extract the timestamps
  stamps <- tidesDb$timestamp
  
  # Initialize a vector for the new timestamps
  newStamps = vector()
  
  # Loop over the timestamps, check if the stamp is a character of list, and append accordingly
  for (i in 1:length(stamps)){
    if (class(stamps[[i]])=="character") {
      newStamps <- append(newStamps,stamps[[i]])
    } else {
      newStamps <- append(newStamps,stamps[[i]]$text)
    }
  }
  
  # Replace the old timstamp list with the new character vector
  tidesDb[,timestamp:=newStamps]
  
  # Create datetime class of timestamp
  tidesDb[,timestamp:=ymd_hm(timestamp)]
  
  # Return the data.table
  return(tidesDb)
}


# GET RAIN FROM TRACKS 
getRainFromTracks <-  function(trackDb, radarName, startTime, nrDays,trackThres,flockThres,bufferSize, saveFile){
  ## Jens van Erp
  ## 16 January 2020
  ## Function to get rain from a RR track database using rough filtering parameters
  ## Note that the input database should be vertical tracks only, with the proper altitude filter in place
  
  # Initialize first minute
  firstMinute <- ymd_hms(startTime)
  # Initialize table of minutes
  allMinutes <- data.table(timeStamp=ymd_hms(), 
                           trackCount=double(), 
                           flockCount=double())
  
  trackDb[,timestamp_start := ymd_hms(timestamp_start)]
  
  # Calculate the floored minute of every track
  trackDb[,flooredMinute_start := floor_date(timestamp_start, unit="minute")]
  
  # Loop over the minutes in the acquired database, this can take a long time
  for (curMinute in 0:(nrDays*24*60-1)){
    # Create an time interval of the current minute
    startMinute <- update(firstMinute, minute = curMinute)
    
    minuteTracks <- trackDb[flooredMinute_start == startMinute,]
    
    nTracks <- nrow(minuteTracks)
    nFlocks <- nrow(minuteTracks[classification_id == "FLOCK"])
    
    allMinutes <- rbind(allMinutes, list(startMinute,nTracks,nFlocks))
  }
  
  ################# Processing Data #################
  # Calculate the ratio of flocks/tracks 
  allMinutes[,flockRatio:=flockCount/trackCount]
  
  # Extract times with a high amount of tracks and high ratio of flocks
  rainMinutes <- allMinutes[trackCount>=trackThres,][flockRatio>=flockThres,]$timeStamp
  # If required, add a buffer to those times
  if (bufferSize>0) {
    rainMinutes <- addRainBuffer(rainMinutes,bufferSize)
  }
  
  ################# Saving Data #################
  if (saveFile) {
    # Save rainminutes as RDS file (easy to handle for R)############
    endTime <- firstMinute+days(nrDays-1)
    
    fileTitle = paste("rainMinutes",radarName,date(startTime),date(endTime), sep="_") 
    saveRDS(rainMinutes,file = fileTitle)
    
    # Put all setting variables in a .txt and save it
    # Open file
    fileTitle = paste("rainMinutes",radarName,date(startTime),date(endTime),"settings", sep="_")
    setFile <- file(paste(fileTitle,".txt",sep = ""))
    
    # Initilize text to write
    headText <- "Settings for the identically  named rain minutes database."
    trackTxt <- "Settings for track filtering"
    altFilterTxt <- "AltitudeFilter was not applied"
    rainTxt <- "Settings for rain filtering"
    tpmTxt <- paste("Minimum tracks per minute = ",trackThres, sep = "")
    fpmTxt <- paste("Minimum ratio of flock tracks per minute = ",flockThres, sep = "")
    buffSetTxt <- paste("Buffer minutes = ",bufferSize, sep = "")
    
    
    # Write text to file and close file
    writeLines(c(headText,trackTxt,altFilterTxt,
                 "",rainTxt,tpmTxt,fpmTxt,buffSetTxt), setFile)
    close(setFile)
  }
  return(rainMinutes)
}

## GET RAIN 
getRain <- function(radarName,startTime,nrDays,altThres,trackThres,flockThres,bufferSize, saveFile){
  ## Jens van Erp
  ## 16 January 2020
  ## Function to get rain for a certain time period and radar (queries database)
  
  ################# Retrieve Data #################
  # Connect to the database
  dbCon <- ame(radarName, 'robin')
  
  # Initialize database
  allMinutes <- data.table(timeStamp=ymd_hms(), trackCount=double(), flockCount=double())
  
  firstMinute <- ymd_hms(paste(startTime,'00:00:00', sep = ' '))
  
  
  for (curMinute in 0:(60*24*nrDays)-1){
    # Retreive curthe current minute from start_time and set the end_time with it
    thisMinute <- update(firstMinute, minute = curMinute)
    endMinute <- update(firstMinute, minute = curMinute+1) - dseconds(1)
    
    # Query the current minute
    nTracks <- dbGetQuery(dbCon, paste("SELECT count(*) FROM track",
                                       " WHERE tracktype = 'RaEl'", 
                                       " AND timestamp_start BETWEEN '",thisMinute, "' AND '", endMinute,"'",
                                       " AND ((st_z(ST_StartPoint(trajectory)) + st_z(ST_EndPoint(trajectory)))/2) > ",
                                       altThres, sep = ''))
    nFlocks <- dbGetQuery(dbCon, paste("SELECT count(*) FROM track as t",
                                       " JOIN classification as c on (t.classification_id = c.id)",
                                       " WHERE tracktype = 'RaEl' AND classification = 'FLOCK'",
                                       " AND timestamp_start BETWEEN '",thisMinute, "' AND '", endMinute,"'",
                                       " AND ((st_z(ST_StartPoint(trajectory)) + st_z(ST_EndPoint(trajectory)))/2) > ",
                                       altThres, sep = ''))
    
    # Add row to data.frame
    allMinutes <- rbind(allMinutes,list(thisMinute,nTracks[1,"count"],nFlocks[1,"count"]))
  }
  
  #Disconnect from database
  dbDisconnect(dbCon)
  
  ################# Processing Data #################
  # Calculate the ratio of flocks/tracks 
  allMinutes[,flockRatio:=flockCount/trackCount]
  
  # Extract times with a high amount of tracks and high ratio of flocks
  rainMinutes <- allMinutes[trackCount>=trackThres,][flockRatio>=flockThres,]$timeStamp
  # If required, add a buffer to those times
  if (bufferSize>0) {
    rainMinutes <- addRainBuffer(rainMinutes,bufferSize)
  }
  
  ################# Saving Data #################
  if (saveFile) {
    # Save rainminutes as RDS file (easy to handle for R)############
    endTime <- firstMinute+days(nrDays-1)
    
    fileTitle = paste("rain",radarName,startTime,endTime, sep="_") 
    saveRDS(rainMinutes,file = fileTitle)
    
    # Put all setting variables in a .txt and save it
    # Open file
    fileTitle = paste("rain",radarName,startTime,endTime,"settings", sep="_")
    setFile <- file(paste(fileTitle,".txt",sep = ""))
    
    # Initilize text to write
    headText <- "Settings for the identically  named rain minutes database."
    trackTxt <- "Settings for track filtering"
    altFilterTxt <- paste("AltitudeFilter = ",altThres, sep = "")
    rainTxt <- "Settings for rain filtering"
    tpmTxt <- paste("Minimum tracks per minute = ",trackThres, sep = "")
    fpmTxt <- paste("Minimum ratio of flock tracks per minute = ",flockThres, sep = "")
    buffSetTxt <- paste("Buffer minutes = ",bufferSize, sep = "")
    
    
    # Write text to file and close file
    writeLines(c(headText,trackTxt,altFilterTxt,
                 "",rainTxt,tpmTxt,fpmTxt,buffSetTxt), setFile)
    close(setFile)
  }
  return(rainMinutes)
}

## ADD RAIN BUFFER 
addRainBuffer <- function (rainMinutes,buffer){
  ## Jens van Erp
  ## 16 January 2020
  ## Adds a buffer to rainminutes provided
  
  # Loop over the number of minutes to buffer
  for (buff in 1:buffer) {
    # Make a vector of minutes before and after the rain minutes
    plusMinutes <- rainMinutes+minutes(1)
    minMinutes <- rainMinutes-minutes(1)
    
    #Add those to the original minutes, keep the unique minutes and sort them
    rainMinutes <- append(rainMinutes,plusMinutes) %>%
      append(minMinutes) %>%
      unique() %>%
      sort()
    # This pipeline transforms the times to the current timezone, rework back to UTC
    rainMinutes <- with_tz(rainMinutes, tzone = "UTC")
  }
  
  return(rainMinutes)
}


## GET MASKS
getMasks <- function(radarName,startTime,nrDays,saveFile){
  ## Jens van Erp
  ## 19 February 2020
  ## This script is used to pull the masking data of a radar from the database
  
  # Connect to the database
  dbCon <- ame(radarName, 'robin')
  
  # Find the radar_ID's of the current horizontal and vertical radar
  hradarQuery <- "SELECT r.id
                FROM radartype as rt
                JOIN radar as r ON (rt.id = r.radartype_id)
                WHERE rt.radar_type = 'HORIZONTAL'
                ORDER BY id DESC LIMIT 1"
  idHor <- dbGetQuery(dbCon, hradarQuery)[[1]]
  
  vradarQuery <- "SELECT r.id
                FROM radartype as rt
                JOIN radar as r ON (rt.id = r.radartype_id)
                WHERE rt.radar_type = 'VERTICAL'
                ORDER BY id DESC LIMIT 1"
  idVer <- dbGetQuery(dbCon, vradarQuery)[[1]]
  
  # Initialize endtime
  endTime <- ymd_hms(paste(startTime,'23:59:59', sep = ' '))+days(nrDays-1)
  
  # Query the database for the average aplied mask per minute ::character varying
  # Horitontal radar
  horQuery <- paste("SELECT date_trunc('minute',timestamp) as timestamp_minute,
                    avg(variantmask_percentage) as variantMask, 
                    avg(landmask_percentage) as landMask, 
                    avg(rain_percentage) as rain
                    FROM image as i
                    JOIN ip_metainfo ON (i.id = image_id)
                    WHERE timestamp BETWEEN '",startTime, "' AND '", endTime,"'
                    AND i.radar_id = ",idHor,"
                    GROUP BY timestamp_minute",
                    sep = '')
  masksHor <- dbGetQuery(dbCon, horQuery)
  
  # Vertical radar
  verQuery <- paste("SELECT date_trunc('minute',timestamp) as timestamp_minute,
                    avg(variantmask_percentage) as variantMask, 
                    avg(landmask_percentage) as landMask, 
                    avg(rain_percentage) as rain
                    FROM image as i
                    JOIN ip_metainfo ON (i.id = image_id)
                    WHERE timestamp BETWEEN '",startTime, "' AND '", endTime,"'
                    AND i.radar_id = ",idVer,"
                    GROUP BY timestamp_minute",
                    sep = '')
  masksVer <- dbGetQuery(dbCon, verQuery)
  
  # Put both results in a list
  maskList <- list(masksHor,masksVer)
  
  # Disconnect from database
  dbDisconnect(dbCon)
  
  if (saveFile){
    fileTitle = paste("masks",radarName,date(startTime),date(endTime), sep="_") 
    saveRDS(maskList,file = fileTitle)
    
    # Put all setting variables in a .txt and save it
    # Open file
    fileTitle = paste("masks",radarName,date(startTime),date(endTime),"settings", sep="_")
    setFile <- file(paste(fileTitle,".txt",sep = ""))
    
    # Initilize text to write
    headText <- "Settings for the identically named mask database."
    radarTxt <- "Settings of sampled radar"
    horTxt <- paste("Horizontal radar id = ",idHor, sep = "")
    verTxt <- paste("Vertical radar id = ",idVer, sep = "")
    queryTxt <- "Queries used on database:"
    
    
    
    # Write text to file and close file
    writeLines(c(headText,radarTxt,horTxt,verTxt,
                 "",queryTxt,horQuery,"",verQuery), setFile)
    close(setFile)
  }
  
  return(maskList)
}

## GET DAILY MEAN 
getDailyMean <- function(countDb){
  ## Jens van Erp
  ## 1 March 2020
  ## Function to create mean daily overviews of track-count data (both quantitative and normalized)
  
  # Remove rows that have no counts (radar most probably off/too much clutter)
  # Note that this is an assumption!
  countDb <- countDb[total_count>0,]
  
  # As we want patterns create normalisations of each day over the maximum of that day
  # Gather all days
  allDays <- countDb[,time] %>%
    date() %>%
    unique()
  
  # Initialize row of normalized counts
  normTotal <- double()
  
  # Loop over days
  for (curDay in allDays){
    # Gather data of the current day
    thisDay <- countDb[date(time) == curDay,]
    # Retrieve the maximum count of that day
    maxCount <- max(thisDay$total_count)
    # Calculate normalized counts
    thisNormTotal <- thisDay$total_count/maxCount
    # Append data
    normTotal <- append(normTotal, thisNormTotal)
  }
  
  countDb[,totalNorm:=normTotal]  
  
  # Create database for total counts
  hrTotal <- data.table(hour = double(),
                        avCount = double(),
                        stdCount = double(),
                        avCountNorm = double(),
                        stdCountNorm = double())
  
  # Loop over hours
  for (curHour in 0:23){
    # Get counts of current hour
    thisHour <- countDb[hour(time) == curHour,]
    avAll <- thisHour[, lapply(.SD, mean, na.rm=TRUE)]
    stdAll <- thisHour[, lapply(.SD, sd, na.rm=TRUE)]
    
    # Bind the counts to the databases
    hrTotal <- rbind(hrTotal,list(curHour,avAll$total_count,stdAll$total_count,avAll$totalNorm,stdAll$totalNorm))
    
  }
  # Return overview and figures
  return(hrTotal)
}



################ DB CONNECT FUNCTIONS ################ 

# SET CREDENTIALS 
set_ame_credentials <- function(username){
  if("keyring" %in% rownames(installed.packages()) == FALSE) {install.packages("keyring")}
  
  require(keyring)
  service='ame'
  key_set(service=service, username = username)
}

# LOAD YOUR CREDENTIALS FOR CONNECTING TO THE DATABASE 
ame_credentials <- function(){
  # set key first if you don't have it in your keyring 
  require(keyring)
  service <- 'ame'
  user <- keyring::key_list(service)$username[1]
  password <- keyring::key_get(service=service, username = user)
  credentials <- list(user,password)
  names(credentials) <-c('user','password')
  return(credentials)
}

# CONNECT TO THE DATABASE OF CHOICE 
ame <- function(database=c('borssele','gemini','luchtmacht2','luchtmacht1','rws01','staticeemshaven','ehdk','eheh','ehgr','ehlw','ehvk','ehwo','eecology'), 
                server=c('robin','eecology')){
  ## Johannes De Groeve
  ## 25 March 2020
  ## Function that allows you to easily connect to the database with passwords stored in the keychain 
  
  require(RPostgreSQL);require(rpostgis);require(sf)
  credentials <- ame_credentials()
  if(server=='robin'){host<-'robin1.e-ecology.nl'}
  if(server=='eecology'){host<-'db.e-ecology.sara.nl'}
  con <- dbConnect("PostgreSQL", 
                   dbname = database, 
                   host = host, 
                   user = credentials$user, 
                   password = credentials$password)
  return(con)
}

robin <- function(){
  ## Johannes De Groeve
  ## 25 March 2020
  ## Function that lists all the available robin databases 
  robin <- data.frame(radarlocation=c('Borssele','Gemini','TBD','Eindhoven','Luchterduinen','Eemshaven','De Kooi','Eindhoven','Gilzen-Rijen','Leeuwarden','Volkel','Woensdrecht'),
                      database=c('borssele','gemini','luchtmacht2','luchtmacht1','rws01','staticeemshaven','ehdk','eheh','ehgr','ehlw','ehvk','ehwo'),
                      stringsAsFactors = FALSE)
  return(robin)
}



################ OLD FUNCTIONS ################ 

## PACKAGE INSTALLER
packageInstaller <- function(){
  ## Jens van Erp
  ## 19 February 2020
  ## Function to install a number of useful packages for my project
  
  # Make a list of package to check
  required_packages <- c(
    "circular","cowplot","data.table","dplyr","ggplot2","installr","lubridate","maps","mapproj","mapdata",
    "plotrix", "pryr","rgeos","rlang","rpostgis","RPostgreSQL","tidyverse")
  # Verify which packages have to be installed
  new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
  # Install packages
  if(length(new_packages)) 
    install.packages(new_packages)
}


## GET SHOWERS 
getShowers <- function(rainminutes){
  ## Jens van Erp
  ## 16 January 2020
  ## Extract "rain showers" by looping over the times of a database of minute-by-minute events of rain and check for subsequent minutes
  
  showers <-  data.table(timestart=ymd_hms(),timeend=ymd_hms())
  if (nrow(rainminutes) == 0) { # Catch zero rainminutes case
    return(showers)
  }
  rain_nr <- 1
  for (i in 1:nrow(rainminutes)){
    if (i == 1) { #If first minute: set as start of first rainshower
      shower_start <- rainminutes[i,time]
      if (nrow(rainminutes) == 1) { # Catch a case where there is only 1 rainminute, so 1 shower
        shower_end <- rainminutes[i,time]
        showers <- rbind(showers,list(shower_start,shower_end))
        return(showers)
      }
      next
    }
    else if (i == nrow(rainminutes)){ #If last minute: check if its "own rainshowerser" as end of rainshower and break
      if (minute(rainminutes[i,time]) == (minute(rainminutes[i-1,time])+1)){ # Not its own, end of last rainshower
        shower_end <- rainminutes[i,time]
        showers <- rbind(showers,list(shower_start,shower_end))
        break
      }
      else {; # Its own, end last shower and make extra shower for this minute
        shower_end <- rainminutes[i-1,time]
        showers <- rbind(showers,list(shower_start,shower_end))
        
        shower_start <- rainminutes[i,time]
        shower_end <- rainminutes[i,time]
        showers <- rbind(showers,list(shower_start,shower_end))
      }
      
    }
    else if (minute(rainminutes[i,time]) == (minute(rainminutes[i-1,time])+1)){ #If subsequent rainminute: continue
      next
    }
    else { #If not: end old rainshower and start new
      shower_end <- rainminutes[i-1,time]
      showers <- rbind(showers,list(shower_start,shower_end))
      shower_start <- rainminutes[i,time]
    }
  }
  return(showers)
}


## KNIT SHOWERS 
knitShowers <- function(showers,buffer){
  ## Jens van Erp
  ## 16 January 2020
  ## Function to knit rainshowers based on a given buffer (in minutes)
  
  if (nrow(showers) == 0) { # Catch zero showers case
    return(showers)
  }
  showers[,timestart:= timestart-buffer*60]
  showers[,timeend:= timeend+buffer*60]
  
  if (nrow(showers) == 1 ) { # Catch one shower case
    return(showers)
  }
  # Because of this, rainshowers might now overlap with eachother. Loop over the database and merge any showers that overlap
  last_time <- showers[nrow(showers),timeend]
  for (i in 1:nrow(showers)){
    while (showers[i,timeend] >= showers[i+1,timestart]){ #Keep merging as long as the next shower overlaps
      showers[i,"timeend"] <- showers[i+1,"timeend"]
      showers <- showers[!i+1,] #Delete the shower we merged
      if (showers[i,timeend]== last_time){ # Return once end is reached (earlier than loop)
        return(showers)
      }
    }
    if (showers[i+1,timeend] == last_time){ # In case last shower is "on its own" return showers before while loop in next iteration looks for [i+1] and finds an error
      return(showers)
    }
  }
}