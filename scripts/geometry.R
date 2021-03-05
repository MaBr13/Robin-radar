library(lubridate)
library(DBI)
library(sf)
library(RPostgreSQL)
library(suncalc)
library(dplyr)

geometry1 <- read.csv("C:/Users/mbradar/Documents/Robin/data/geometry_Oct2020.csv",sep=";")

zs <- split(tt_filtered,nrow(tt_filtered))
lst <- lapply(split(tt_filtered['timestamp_start','distance','trajectory'], tt_filtered$timestamp_start), transform) 


vert <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/autumn2019vert.csv", sep=";")
plot_ly(x=vert$lon,y=vert$lat,z=vert$altitude, type="scatter3d", mode="markers",color = vert$classification)

zs <- list()


zs <- tt_filtered[1:10] %>%
  group_by(track_id)%>%
  summarise(timestamp_start=timestamp_start,distance=distance,x=st_coordinates(tt_filtered$trajectory)[1],
          y=st_coordinates(tt_filtered$trajectory)[2],z=st_coordinates(tt_filtered$trajectory)[3])


vert <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert.csv",sep=";")
vert$timestamph <- as.POSIXct(vert$timestamph,tz='UTC')
vert <- subset(vert,datex>='2019-09-30 17:00:00' & datex<='2019-11-01 07:00:00')
vert$timestamp <- as.POSIXct(vert$timestamp,tz='UTC')
colnames(vert)[colnames(vert)==c("timestamp_start")] <- c("timestamp")
clutter <- read.csv("C:/Users/mbradar/Documents/Robin/data/Clutter/Luchterduinen/clutter_autumn2019_L.csv",sep=";")
clutterV <- subset(clutter,radar_id==1 | radar_id==4 | radar_id==7 | radar_id==8 | radar_id==10 | radar_id==12)
colnames(clutterV)[colnames(clutterV)==c("timestamp1")] <- c("timestamp")
clutterV$timestamp <- as.POSIXct(clutterV$timestamp,tz='UTC')
hoursA19 <- seq(ymd_hms('2019-09-30 17:00:00'),ymd_hms('2019-11-01 07:00:00'), by="1 min")
hoursA19 <- as.data.frame(hoursA19)
colnames(hoursA19)[colnames(hoursA19)==c("hoursA19")] <- c("timestamp")
hoursA19$date <- date(hoursA19$timestamp)
sun<- getSunlightTimes(date(hoursA19$timestamp),52.25,4,keep = c("sunrise","sunset"),tz="UTC")
sun <- dplyr::distinct(sun)
hoursA19 <- left_join(hoursA19,sun,by=c('date'))
rm(list=c("sun"))
hoursA19$light <- with(hoursA19,ifelse(hour(hoursA19$timestamp)>=hour(sunset),"evening",
                                     ifelse(hour(hoursA19$timestamp)<=hour(sunrise),"night","day")))
hoursA19$night <- with(hoursA19,ifelse(hoursA19$light=="evening",hoursA19$date,
                                     ifelse(hoursA19$light=="night",hoursA19$date-lubridate::days(1),0)))
hoursA19 <- subset(hoursA19,!night==0)
hoursA19$night <- zoo::as.Date(hoursA19$night,tz="UTC")

rain <- read.csv("c:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2019.csv",sep=';')

colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
rain$timestamph <-as.POSIXct(ymd_h(paste(as.Date(rain$timestamp), hour(rain$timestamp),sep= ' '))) 
rain <- subset(rain,mean<500)

data <- left_join(hoursA19,clutterV,by="timestamp")
data <- left_join(data,rain, by='timestamp')
data <- 


birds_n <- subset(vert,night=='2019-10-01 - 2019-10-02')
clut_n <- subset(data,night=='2019-10-01')

fig1 <- ggplot(data=birds_n, aes(x=timestamp)) +
  geom_bar(mapping = aes(x = timestamp, y = ..count..,colour=birds_n$classification, fill=birds_n$classification), stat = "count",show.legend = T) +
  scale_fill_manual(values = c("magenta","red","orange","yellow"), name="", drop=F)+
  scale_colour_manual(values  = c("magenta","red","orange","yellow"), name="", drop=F)+
  theme_bw()+
  theme(axis.title.y = element_text(size=12), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=10), axis.text.x=element_text(angle=90), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  #scale_x_datetime(date_breaks="min", date_labels="%Y-%m-%d",
   #                limits=c(as.POSIXct("2019-02-15 17:00:00"), as.POSIXct("2020-12-01 07:00:00")))+
  scale_y_continuous(name = "Counts")

fig1 <- ggplotly(fig1,dynamicTicks = TRUE)

fig2 <- plot_ly(clutterV) 
fig2 <- fig2 %>% add_lines(data=clut_N, name="rain_V", x = ~timestamp, y = ~r_mask*100,
                             line = list(color="darkslateblue",width=0.5),showlegend=TRUE)

fig3 <- plot_ly(rain) 
fig3 <- fig3 %>% add_lines(data=clut_n, name="rain_KNMI", x = ~timestamp, y = ~mean,
                           line = list(color="royalblue4",width=0.5),showlegend=TRUE)

rainfig <-  subplot(fig1,fig3,fig2,nrows = 3, shareX = TRUE) 
rainfig %>% rangeslider(start=as.POSIXct("2019-10-01 18:00:00"),end=as.POSIXct("2019-10-01 21:00:00"), borderwidth = 0.2) %>%
  layout(yaxis=list(title="Count", tickfont = list(size=8), titlefont=list(size=14,face="bold")),          
         yaxis2=list(title="Rain (mm)",range=c(0,100), tickfont = list(size=8), titlefont=list(size=14,face="bold")),
         yaxis3=list(title="Percentage",range=c(0,100), tickfont = list(size=8), titlefont=list(size=14,face="bold")),
         titleX = TRUE, titleY = TRUE) 


geometry <- list()
database <- "rws01"
con <- dbConnect("PostgreSQL", 
                 dbname=database, # database name  
                 host='robin1.e-ecology.nl', 
                 user= "maja_bradaric",# username
                 password="55nebitno992@") # password


timestamps <- c('2019-10-01 23:00:00','2019-10-20 17:00:00','2019-10-28 03:00:00','2020-10-06 00:00:00',
                '2020-10-15 07:00:00')

for(k in 1:length(timestamps)){
  
 geometry[[k]] <- st_read(con,query = paste0("select track.id as track_id,track.tracktype, st_x(trackestimate.position) as x,
                         st_y(trackestimate.position) as y, st_z(trackestimate.position) as z
                         from public.track
                         INNER JOIN public.trackestimate 
                         ON track.id=trackestimate.track_id
                         where track.timestamp_start BETWEEN '", as.character(timestamps[k]) , "' AND '", as.character(as.POSIXct(timestamps[k])+hours(1)) ,"' 
                         AND track.tracktype in ('RaAzEl','RaEl')"))
}

vert <- read.csv("C:/Users/mbradar/Documents/Robin/data/Birds/Luchterduinen/allseasons_2019-2020_vert&mixed_filtered.csv",sep=";")
vert <- vert[,c(1,5)]
vert$class <- with(vert, ifelse(classification=="SMALL_BIRD",1,
                                ifelse(classification=="MEDIUM_BIRD",2,
                                       ifelse(classification=="LARGE_BIRD",3,4))))

for (k in 1:length(geometry)){
  geometry[[k]] <- left_join(geometry[[k]],vert,by="track_id")
}

for (k in 1:length(geometry)){
  geometry[[k]] <- subset(geometry[[k]],z>=0 & z<=1500)
}

geometry1 <- do.call("rbind",geometry)

library(plotly)
fig <- plot_ly(geometry[[1]], x = ~x, y = ~y, z = ~z, split=~track_id, type = 'scatter3d', mode = 'lines',
               opacity = 1, line = list(width = 6, reverscale = FALSE))

