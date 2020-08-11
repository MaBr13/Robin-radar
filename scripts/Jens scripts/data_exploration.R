############################################
## Data_exploration.R
## Jens van Erp
## 16 January 2020
## This script can be run to request an overview of the Horizontal data for all offshore RR radars.
## Though technically it allows you to view any time period I advise to keep to max. 1~2 weeks
############################################
# ToDo
# Time bins are still ofset by 30 minutes (12-13h bin plotted in middle of 12h)
# Density plots?
# Rose/Density to giff?
# Ground speed plots?

################# store password in keychain ################# 
# First time only
set_ame_credentials('jens_vanerp')

################# Initialization ################# 

## User input
# Select data to explore
startDate <- '2020-03-23' # Date from which to qeury the data
nrDays <- 7            # Number of days you want to explore from startDate
robin() ## check the list of radars 
radarName <- 'rws01'      # Which radar: Luchterduinen =    'rws01' 
#                     Borssele Alpha =   'borssele'
#                     Gemini =           'gemini'
extraFileText <- ''       # Addition to the name of the files to be saved (if querying the same data/radar)  

# Turn rough filters on or off
# Filtering applies some boundaries on the tracks extracted.
filters <- TRUE
rainFilter <- TRUE

# Pre-downloaded datasets (way faster than querying)
# Leave empty (NULL) query new data ()
trackFile <- NULL#"data/filteredTracks_LD_Mar19toDec19.csv"
rainFile <- NULL#"data/rainMinutes_rws01_Mar19toDec19"
maskFile <- NULL#"data/masks_rws01_2019-03-01_2019-12-31"
tidesFile <- NULL#"data/tides_IJmuiden_NAP_GMT+1_2019.xml"

## Statics (other variables needed for script, no need to change) 
# Table columns to query
# For a more detailed description of the different colums, see documentation
# Simply add or remove '#' to change
dataColumns <- paste('t.id',                    # ID of track
                     'timestamp_start',         # Timestamp of start of track (
                     'timestamp_end',           # Timestamp of end of track  
                     'assignable_properties',   # Vector of track properties
                     'c.classification',        # Classification (from classification table!)
                     'rho_diff',                # Striaght line distance travelled
                     'phi_diff',                # Bearing of straight line distance travelled
                     'theta_diff',              # Elevation of straight line distance travelled
                     'tracktype',               # Shows which radar was used for estimating the track
                     #'trajectory',              # Geometric polyline of track plot positions (will result in large db)
                     #'trajectory_time',         # Timestamps of geometric polyline of track plot
                     #'avg_mass',                # Average object mass
                     'avg_corrected_mass',      # Average object mass, correced for distance
                     #'avg_peak_mass',           # Maximum estimated object mass
                     'avg_corrected_peak_mass', # Maximum estimated object mass, correced for distance
                     'distance_travelled',      # Distance travelled along the track path (geo polyline)
                     'airspeed',                # Average airspeed of the object
                     'score',                   # Score of the tracking algorithm for the plot 
                     'nr_of_plots',             # Number of plots in the track
                     sep= ', ')  

# Filter settings + justification
filtPlots <- 30        # [#] Minimum number of plots in a track
filtMinRange <-  800      # [m] Minimum range from radar of track 
filtMaxRange <-  3000     # [m] Maximum range from radar of track
filtRho <- 200            # [m] Minimum straight-line traveled distance
filtAlt <- 25             # [m] Minimum height for quering vertical tracks for the rainfilter
filtTrackThres <- 50      # [#] Minimum amount of tracks/minute to be considered rain 
filtFlockThres <- 0.1     # [ratio] Minimum ratio of flocks/tracks to be considered rain
filtBufferTime <- 2       # [min] Buffer in minutes around showers to account for

################# Load Data #################
# Initiaze the time window we want to retreive data
startTime <- ymd_hms(paste(startDate,"00:00:00", sep=" "))
endTime <- startTime + days(nrDays) - seconds(1)

# Connect to the database
dbCon <- ame(radarName, 'robin')


# Get the Horizontal radar coordinates from a small query
radarQuery <- "SELECT r.id,
r.location_name,
ST_X((ST_DumpPoints(r.position)).geom),
ST_Y((ST_DumpPoints(r.position)).geom),
ST_Z((ST_DumpPoints(r.position)).geom)
FROM radartype as rt
JOIN radar as r ON (rt.id = r.radartype_id)
WHERE rt.radar_type = 'HORIZONTAL'
ORDER BY id DESC LIMIT 1"
radarData <- data.table(dbGetQuery(dbCon, radarQuery))

# Extract x and y coordinates
radarCoords <- paste(toString(radarData$st_x),toString(radarData$st_y),sep = ' ')

# Check if a filename is given to load in data locally, if not, create a query and load the data.
if (is.null(rainFile)){
  # Create the datbase queries
  dbQuery <- paste("SELECT ", dataColumns, " FROM track as t",
                   " JOIN classification as c on (t.classification_id = c.id)",
                   " WHERE tracktype != 'RaEl'",
                   " AND c.classification IN ('FLOCK', 'LARGE_BIRD', 'MEDIUM_BIRD', 'SMALL_BIRD')",
                   " AND timestamp_start BETWEEN '",startTime, "' AND '", endTime,"'",
                   sep = '')
  
  if (filters) {
    dbQuery <- paste(dbQuery,
                     " AND nr_of_plots >= ",filtPlots,
                     " AND rho_diff >= ",filtRho,
                     " AND ST_MaxDistance(st_transform(ST_GeomFromText('POINT(",radarCoords,")',4326), 3035), st_transform(trajectory, 3035)) >= ",filtMinRange,
                     " AND ST_Distance(st_transform(ST_GeomFromText('POINT(",radarCoords,")',4326), 3035), st_transform(trajectory, 3035)) <= ",filtMaxRange,
                     sep = '')
  }
  
  # Query the database for tracks
  trackDb <- data.table(dbGetQuery(dbCon, dbQuery))
} else {
  # Else load in the data locally , cut out timestmaps we are not interested in. 
  rawDb <- fread(trackFile)
  trackDb <- rawDb
  rm(rawDb)
  trackDb[,timestamp_start := ymd_hms(timestamp_start)]
  trackDb[,timestamp_end := ymd_hms(timestamp_end)]
}

# Disconnect from database
dbDisconnect(dbCon)

# Load in rain minutes
# Again if a filename is given load the data locally, otherwise query through function getRain
if (rainFilter) {
  if (is.null(rainFile)){
    rainMinutes <- getRain(radarName,startTime,nrDays,filtAlt, filtTrackThres,filtFlockThres, filtBufferTime, FALSE)
  } else {
    rawRain <- readRDS(rainFile)
    rainMinutes <- rawRain[rawRain>=(startTime-days(1)) & rawRain<(endTime-days(1))]
    rm(rawRain)
  }
}

# Load in masking data
# And again, either from a local file or query it through function getMasks
if (is.null(maskFile)){
  maskData <- getMasks(radarName,startTime,nrDays,FALSE)
  horMasks <- maskData[[1]]
}else{
  rawMasks <- readRDS(maskFile)
  horMasks <- rawMasks[[1]]
  horMasks <- horMasks[timestamp_minute>=(startTime-days(1)) & timestamp_minute<(endTime-days(1)),]
}

# Load in tides data, can only happen from file
if (!is.null(tidesFile)){
  rawTides <- getTides(tidesFile)
  tidesDb <- rawTides[timestamp>=(startTime) & timestamp<(endTime),]
  rm(rawTides)
}

################# Data pre-processing #################
# Set time of the retrieved timestamps to UTC, as your machine thinks it retrieved local time
trackDb[, timestamp_start := force_tz(trackDb[,timestamp_start], 'UTC')]

# Next make a circular-data column from this heading
trackDb[,circ_rad := circular(phi_diff, type = "angles", units = "radians", zero = 0, rotation = "clock")]

# Tag rain based on rainMinutes
trackDb[,rain:=FALSE]
if (rainFilter) {
  trackDb[floor_date(timestamp_start,unit="minute") %in% rainMinutes,rain:=TRUE]
}

################# Analysis #################
## Create new databases
# Initialize db for the number of bird tracks per hour for each bird class
hourlyStats <- data.table(time=ymd_hms(),
                          flock_count = integer(),
                          large_count = integer(),
                          medium_count = integer(),
                          small_count = integer(),
                          total_count = integer(),
                          rain_count = integer(),
                          meanHeading = double(),
                          rhoMeanHeading = double())

# Create a new column with floored hours
trackDb[,flooredHour_start := floor_date(timestamp_start, unit="hour")]


# Loop over the number of hours within the given number of days
for (curHour in 0:(24*nrDays-1)){
  ## Sample the tracks for this hour
  # Create an time interval of the hour we want 
  startHour <- update(startTime, hour = curHour)
  
  # Extract the tracks of this hour
  hourTracks <- trackDb[flooredHour_start == startHour,]
  
  # Retreive the counts of the different birds
  flocks <- nrow(hourTracks[classification == "FLOCK" & !rain,])
  larges <- nrow(hourTracks[classification == "LARGE_BIRD" & !rain,])
  mediums <- nrow(hourTracks[classification == "MEDIUM_BIRD" & !rain,])
  smalls <- nrow(hourTracks[classification == "SMALL_BIRD" & !rain,])
  totals <- flocks+larges+mediums+smalls
  
  # Do the same for rain if the filter is active
  if (rainFilter) {
    rains <- nrow(hourTracks[(rain)])
  } else {
    rains <- NA
  }
  
  
  # Retreive the average heading vector per hour (mean direction and rho of mean direction) 
  # Don't use raintracks, and fill in empty if there are no tracks this hour
  if (nrow(hourTracks) == 0){
    curMeanHeading <- 0
    curRhoHeading <- 0
  } else {
    # Calculate mean heading 
    curMeanHeading <- mean(hourTracks[rain==FALSE, circ_rad])[[1]]
    
    #  Calculate rho
    curRhoHeading <- rho.circular(hourTracks[rain==FALSE, circ_rad])
  }
  
  # Bind all the data as a new row to the hourlyStats
  hourlyStats <- rbind(hourlyStats, list(startHour,flocks,larges,mediums,smalls,
                                         totals,rains,curMeanHeading,curRhoHeading))
}
# Remove temp variables
rm(startHour,hourTracks,flocks,larges,mediums,smalls,totals,rains,curMeanHeading,curRhoHeading)

# Get daily mean of the counts
meanDay <- getDailyMean(hourlyStats)

# Problem with rhoHeading is that if there is a single observation it wil receive a rho of 1
# Therefore, also calculate a normalized rho, weighted by the number of tracks that hour
# Get the hourly max over the whole database (set is as variable as we need it more often later)
maxHourly <- max(hourlyStats$total_count)
# Based on the hourly normalized counts, get the normalized rho
hourlyStats[,normRho := rhoMeanHeading*(total_count/maxHourly)]


# Now we will create a table with daily stats
# Get the days on our dataset
allDays <- hourlyStats[,time] %>%
  date() %>%
  unique()

# Initialize database by summing total counts and rain counts from hourlyStats
dailyStats <- hourlyStats[,.(total_count=sum(total_count), 
                             rain_count=sum(rain_count)),
                          by=date(time)]

# Unfortunately, heading and rho has to be recalculated over the trackDb
dailyHeading <- data.table(time=ymd(),
                           meanHeading=double(),
                           rhoMeanHeading=double())
# Loop over the days and gather averages
for (dayNum in 1:length(allDays)){
  curDay <- ymd(allDays[dayNum])
  # Get tracks of this day
  dayTracks <- trackDb[date(timestamp_start) == curDay,]
  
  if (nrow(dayTracks[tracktype!="RaEl",]) == 0){
    dailyHeading <- rbind(dailyHeading, list(curDay,0,0))
  } else {
    # Calculate mean heading of these tracks
    curMeanHeading <- mean(dayTracks[rain==FALSE,circ_rad])[[1]]
    # Calculate rho of mean heading of these tracks
    curRhoHeading <- rho.circular(dayTracks[rain==FALSE,circ_rad])
    # Bind the data
    dailyHeading <- rbind(dailyHeading, list(curDay,curMeanHeading,curRhoHeading))
  }
}
# Remove temp variables
rm(curDay,dayTracks,curMeanHeading,curRhoHeading)

# Add headings to the dailyStats table 
dailyStats <- dailyStats[dailyHeading, on = .(date=time)]
# Calculate normalized rho (similar to hourlyStats)
dailyStats[,normRho:= rhoMeanHeading*(total_count/max(total_count))]
rm(dailyHeading)

# Extract sun-angle for every observed at the mid point of that hour
sunLight <- getSunlightPosition(hourlyStats$time+minutes(30), lat=radarData$st_y, lon=radarData$st_x)
# Add relevant information to hourlyStats
hourlyStats[,sunAlt:=sunLight$altitude]
rm(sunLight)

################# Visualization #################
# As it is set now, the script creates two plots, one with counts and one with directions. 
# Code to save these plots together as a figure is here, remove comments to make that work

# Create a roseplot for flight directions for queried period

figDirHist <- ggplot(trackDb, aes(x=circ_rad)) +
  geom_histogram(breaks=seq(-pi,pi,2*pi/24),col='white',fill="cyan4") + #Make a historgram
  coord_polar(start = -pi) + # and turn in it into a circle
  scale_x_continuous("", breaks = c(-pi,-0.75*pi,-0.5*pi,-0.25*pi,0,0.25*pi,0.5*pi,0.75*pi),labels = c("S","SW","W","NW","N","NE","E","SE"))+ # Create compas labels
  theme_minimal()+ #the two next code pices are just for styling your plot
  ggtitle("Track heading")+
  theme(plot.title = element_text(size = 20,face = "bold"), axis.title.y = element_blank(),axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16,face = "bold",colour="black"), panel.grid.minor  =element_blank(),
        panel.grid.major=element_line(colour = "cyan4"))
figDirHist

# Loop over days and create a roseplot per day to show daily trends, print them immediately
for (dayNum in 82:length(allDays)){
  curDay <- ymd(allDays[dayNum])
  # Get tracks of this day
  dayTracks <- trackDb[date(timestamp_start) == curDay,]
  
  if (nrow(dayTracks) != 0){ 
    figDirHist <- ggplot(dayTracks, aes(x=circ_rad)) +
      geom_histogram(breaks=seq(-pi,pi,2*pi/24),col='white',fill="cyan4") + #Make a historgram
      coord_polar(start = -pi) + # and turn in it into a circle
      scale_x_continuous("", breaks = c(-pi,-0.75*pi,-0.5*pi,-0.25*pi,0,0.25*pi,0.5*pi,0.75*pi),labels = c("S","SW","W","NW","N","NE","E","SE"))+ # Create compas labels
      theme_minimal()+ #the two next code pices are just for styling your plot
      ggtitle("Track heading")+
      theme(plot.title = element_text(size = 20,face = "bold"), axis.title.y = element_blank(),axis.text.y = element_blank(),
            axis.text.x = element_text(size = 16,face = "bold",colour="black"), panel.grid.minor  =element_blank(),
            panel.grid.major=element_line(colour = "cyan4"))
    
    pngTitle = paste("trackdirection_roseplot",curDay, sep="_")
    png(paste(pngTitle,".png",sep = ""), width = 640, height = 640, units = "px")
    
    # Put the two plots together in a grid and save the grid to the png-file
    print(figDirHist)
    dev.off()
  }
}


# Plot an overview of the number of tracks per hour with the sun-angle
# For making the histogram in ggplot, we cast the hourlyStats to a countDb
countDb <- melt(hourlyStats,
                id.vars = "time",
                measure.vars = c("flock_count","large_count","medium_count","small_count","rain_count"),
                variable.name = "classification",
                value.name = "count")
countDb[classification=="flock_count", classification:="Flock"]
countDb[classification=="large_count", classification:="Large"]
countDb[classification=="medium_count", classification:="Medium"]
countDb[classification=="small_count", classification:="Small"]
countDb[classification=="rain_count", classification:="Rain"]
setorder(countDb,time)
# Set colours for different classes
if (rainFilter) {
  fillValue = c("Flock"="#660066","Large"="#CC0000","Medium"="#FF6600","Small"="#FFCC00","Rain"="#666666")
} else {
  fillValue = c("Flock"="#660066","Large"="#CC0000","Medium"="#FF6600","Small"="#FFCC00")
}

# Plot histogram with sun-angle
figCountHist <- ggplot()+
  geom_col(data=countDb, aes(x=time,y=count,fill=classification))+
  scale_fill_manual(name="Counts", values=fillValue)+
  geom_line(data=hourlyStats, 
            aes(x=time,y=sunAlt*0.75*maxHourly-min(sunAlt)*0.75*maxHourly,color=sunAlt),
            size = 1,
            show.legend=FALSE)+
  scale_color_continuous(low="black", high="yellow")+
  labs(y = "Count (#)", x = "Time", title = "Bird counts per hour")+
  scale_x_datetime(date_labels = "%e %b", date_breaks = "1 day")+
  theme(plot.title = element_text(size = 30,face = "bold"),
        legend.title = element_text(size = 22,face = "bold"),
        legend.text = element_text(size = 18,face = "bold"),
        axis.title = element_text(size = 22,face = "bold"),
        axis.text = element_text(size = 18,face = "bold"))
figCountHist

ggplot()+
  geom_col(data=countDb, aes(x=time,y=count,fill=classification))+
  scale_fill_manual(name="Counts", values=fillValue)+
  geom_line(data=tidesDb, 
            aes(x=timestamp,y=waterheight),
            color="blue",
            size = 1,
            show.legend=FALSE)+
  labs(y = "Count (#)", x = "Time", title = "Bird counts per hour")+
  scale_x_datetime(date_labels = "%e %b", date_breaks = "1 day")+
  theme(plot.title = element_text(size = 30,face = "bold"),
        legend.title = element_text(size = 22,face = "bold"),
        legend.text = element_text(size = 18,face = "bold"),
        axis.title = element_text(size = 22,face = "bold"),
        axis.text = element_text(size = 18,face = "bold"))

# Plot the absolute mean daily count overview
figMeanDay <- ggplot(data = meanDay, aes(x=hour))+
  geom_col(aes(y=avCount))+
  geom_errorbar(aes(ymin=avCount-stdCount,ymax=avCount+stdCount))+
  scale_x_continuous(breaks=seq(-0.5,23.5,3),labels = c("00:00","03:00","06:00","09:00","12:00",
                                                        "15:00","18:00","21:00","24:00"))+
  labs(y = "Total bird count (#)", x = "Time (hour of day)",
       title = "Average absolute bird counts per hour")+
  theme(plot.title = element_text(size = 20,face = "bold"),
        axis.title = element_text(size = 18,face = "bold"),
        axis.text = element_text(size = 16,face = "bold"))
figMeanDay

# Plot the normalized mean daily count overview
figMeanDayNorm <- ggplot(data = meanDay, aes(x=hour))+
  geom_col(aes(y=avCountNorm))+
  geom_errorbar(aes(ymin=avCountNorm-stdCountNorm,ymax=avCountNorm+stdCountNorm))+
  scale_x_continuous(breaks=seq(-0.5,23.5,3),labels = c("00:00","03:00","06:00","09:00","12:00",
                                                        "15:00","18:00","21:00","24:00"))+
  labs(y = "Normalized bird count (#)", x = "Time (hour of day)",
       title = "Average normalized bird counts per hour")+
  theme(plot.title = element_text(size = 20,face = "bold"),
        axis.title = element_text(size = 18,face = "bold"),
        axis.text = element_text(size = 16,face = "bold"))
figMeanDayNorm

# Plot the masking prevalence throughout the period
figMasks <- ggplot(data = horMasks, aes(x=timestamp_minute))+
  geom_line(aes(y=smooth(variantmask)), colour= "brown")+
  geom_line(aes(y=smooth(landmask)), colour= "green")+
  geom_line(aes(y=smooth(rain)), colour= "blue")+
  ylim(c(0,1))+
  labs(y = "Filter ratio", x = "Time", title = "Masking activity per minute")+
  scale_x_datetime(date_labels = "%e %b", date_breaks = "1 day")+
  theme(plot.title = element_text(size = 30,face = "bold"),
        legend.title = element_text(size = 22,face = "bold"),
        legend.text = element_text(size = 18,face = "bold"),
        axis.title = element_text(size = 22,face = "bold"),
        axis.text = element_text(size = 18,face = "bold"))
figMasks

# Plot daily counts plus masks 
figCountMask <- ggplot()+
  geom_col(data=countDb, aes(x=time,y=count,fill=classification))+
  geom_line(data=horMasks, aes(x=timestamp_minute,y=smooth(variantmask)*maxHourly), colour= "brown")+
  geom_line(data=horMasks, aes(x=timestamp_minute,y=smooth(landmask)*maxHourly), colour= "green")+
  geom_line(data=horMasks, aes(x=timestamp_minute, y=smooth(rain)*maxHourly), colour= "blue")+
  scale_fill_manual(name="Counts",
                    values=fillValue)+
  labs(y = "Count (#)", x = "Time", title = "Bird counts per hour")+
  scale_x_datetime(date_labels = "%e %b", date_breaks = "1 day")+
  theme(plot.title = element_text(size = 30,face = "bold"),
        legend.title = element_text(size = 22,face = "bold"),
        legend.text = element_text(size = 18,face = "bold"),
        axis.title = element_text(size = 22,face = "bold"),
        axis.text = element_text(size = 18,face = "bold"))
figCountMask

# Create a feather plot of the hourly headings. Very bare-bones function but best I got atm.
feather.plot(hourlyStats$normRho,hourlyStats$meanHeading,fp.type = "m", xlab="Time (hours from start)")

# Create a feather plot of the daily headings. Very bare-bones function but best I got atm.
feather.plot(dailyStats$normRho,dailyStats$meanHeading,fp.type = "m", xlab="Time (day)")

################# Data saving #################
# Open a png file to save figures to
pngTitle = paste(radarName,startDate,toString(date(endTime)+1),"counts", sep="_")
png(paste(pngTitle,".png",sep = ""), width = 2*640, height = 640, units = "px")

# Put the two plots together in a grid and save the grid to the png-file
pgrid <- plot_grid(figCountHist,figDirHist, nrow = 1, ncol = 2, rel_widths = c(3,1), align = "h")
print(pgrid)
dev.off()

# Open a png file to save the figures to
pngTitle = paste(radarName,startDate,toString(date(endTime)+1),"counts&masks", sep="_")
png(paste(pngTitle,".png",sep = ""), width = 2*640, height = 640, units = "px")

# Put the two plots together in a grid and save the grid to the png-file
pgrid <- plot_grid(figCountMask,figDirHist, nrow = 1, ncol = 2, rel_widths = c(3,1), align = "h")
print(pgrid)
dev.off()

# Put all setting variables in a .txt and save it
# Open file
fileTitle = paste(radarName,startDate,toString(date(endTime)+1),"settings", sep="_")
setFile <- file(paste(fileTitle,".txt",sep = ""))

# Initilize text to write
headText <- "Settings for the identically  named overview figure."
queryText <- "Query used on database:"
filtText <- "Filters off or on?"
trackFilterTxt <- paste("TrackFilter = ",filters, sep = "")
rainFilterTxt <- paste("RainFilter = ",rainFilter, sep = "")
trackTxt <- "Settings for track filtering"
plotsTxt <- paste("Minimum number of plots per track = ",filtPlots, sep = "")
minRangeTxt <- paste("Minimum range = ",filtMinRange, sep = "")
maxRangeTxt <- paste("Maximum range = ",filtMaxRange, sep = "")
rhoTxt <- paste("Minimum coverered straigh-line distance = ",filtRho, sep = "")
rainTxt <- "Settings for rain filtering"
if (is.null(rainFile)){
  altTxt <- paste("Minimum altitude for tracks = ",filtAlt, sep = "")
  tpmTxt <- paste("Minimum tracks per minute = ",filtTrackThres, sep = "")
  fpmTxt <- paste("Minimum ratio of flock tracks per minute = ",filtFlockThres, sep = "")
  rainTxtTot <- paste(altTxt,tpmTxt,fpmTxt, sep = "\n")
} else {
  rainTxtTot <- paste("Rain file used:","rainFile",sep = "\n")
}

totCount <- paste("Total tracks =",nrow(trackDb), sep = "")
flockCount <- paste("Flock tracks = ",nrow(trackDb[classification == "FLOCK"]), sep = "")
largeCount <- paste("Large tracks = ",nrow(trackDb[classification == "LARGE_BIRD"]), sep = "")
mediumCount <- paste("Medium tracks = ",nrow(trackDb[classification == "MEDIUM_BIRD"]), sep = "")
smallCount <- paste("Small tracks = ",nrow(trackDb[classification == "SMALL_BIRD"]), sep = "")


# Write text to file and close file
writeLines(c(headText,queryText,dbQuery,
             "",filtText,trackFilterTxt,rainFilterTxt,
             "",trackTxt,plotsTxt,minRangeTxt,maxRangeTxt,rhoTxt,
             "",rainTxt,rainTxtTot,
             "",totCount,flockCount,largeCount,mediumCount,smallCount), setFile)
close(setFile)