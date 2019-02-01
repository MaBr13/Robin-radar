db.file <-"MyRobinData" #provide the name of the database or file for ODBC driver
library(RODBC) 
Rdb <- odbcConnect(db.file)#establish connection with database


MTR <- sqlQuery(Rdb, "SELECT id,timestamp_end as timestamp,ST_Force2D(trajectory) as trajectory,airspeed,
                extract(year from timestamp_end) as year,
                extract (month from timestamp_end) as month, 
                extract (day from timestamp_end) as day,
                extract (hour from timestamp_end) as hour,
                transect
                from public.track,public.MTR
                WHERE exists (
                SELECT * FROM unnest(assignable_properties) n
                WHERE n =  'STRAIGHT' ) 
                AND airspeed between '8' and '13'
                AND timestamp_end between '2018-11-01 00:00:00' and '2018-12-01 00:00:00'				
                AND ST_Intersects(trajectory, transect)
                ")

#########THE OLD VERSION##############

#MTR <- sqlQuery(Rdb, "select id,timestamp_end as timestamp,classification_id,
#ST_X(ST_StartPoint(trajectory)) as long1, ST_Y(ST_StartPoint(trajectory)) as lat1,
#ST_X(ST_EndPoint(trajectory)) as long2, ST_Y(ST_EndPoint(trajectory)) as lat2,
#extract(year from timestamp_end) as year,
#extract (month from timestamp_end) as month,
#extract (day from timestamp_end) as day,
#extract (hour from timestamp_end) as hour
#from public.track
#where classification_id in (8)
#and timestamp_end between '2018-11-01 00:00:00' and '2018-12-01 00:00:00'
#order by timestamp_end")


#transect further from the radar
#transect_1 <- c(4.204446,52.422253)
#transect_2 <- c(4.213685,52.415270)


#transect closer to the radar
#transect_1 <- c(4.190928,52.431858)
#transect_2 <- c(4.200196,52.424890)

#check which tracks intersect with the transect (first or second)

#require(IceCast)
#for (k in 1:nrow(MTR1)){
#  MTR1[k,12] <- checkIntersect(transect_1,transect_2,
#                                     c(MTR1[k,]$long1,MTR1[k,]$lat1),c(MTR1[k,]$long2,MTR1[k,]$lat2))
#}


#colnames(test)[c(7)] <- c("intersect")

#subset only tracks that intersect with the transect
#mtrtrue <- subset(test,intersect==TRUE,select = id:intersect)


####THE NEW VERSION#############

setwd("C:/Users/mbradar/Documents/Robin/data/MTRs")
Sept <- read.csv("September_2018_v2.csv",sep=',')
Oct <- read.csv("October_2018_v2.csv",sep=',')
Nov <- read.csv("November_2018_v2.csv",sep=',')


library(lubridate)
library(dplyr)

Sept$timestamp <- paste(Sept$year, Sept$month, Sept$day,Sept$hour, sep=" ") %>% ymd_h() %>% as.POSIXct()
Oct$timestamp <- paste(Oct$year, Oct$month, Oct$day,Oct$hour, sep=" ") %>% ymd_h() %>% as.POSIXct()
Nov$timestamp <- paste(Nov$year, Nov$month, Nov$day,Nov$hour, sep=" ") %>% ymd_h() %>% as.POSIXct()

#create a table that summarizes number of tracks per day
library(dplyr)
trackcount2 <- 
  Nov %>%
    group_by(timestamp) %>%
    summarise(Nr.tracks=length(id))

#fill in the missing times in the table
library(tidyverse)
trackcount2 <- trackcount2 %>%
  complete(timestamp = seq(timestamp[1], timestamp[710], by = "1 hour"),
           fill = list(Val1 = 0, Val2 = 0))

#create timestamps

library(lubridate)
Sys.setenv(TZ="UTC")
Sys.setlocale(category = "LC_ALL", locale = "English_United Kingdom.1252")#set the time on your computer to match
trackcount2$date <- as.Date(trackcount2$timestamp) 

#fill in the NA values of number of tracks as zeroes

trackcount2[is.na(trackcount2)] <- 0

#plotting

library(ggplot2)
library(scales)

 
p3 <- ggplot(trackcount2,aes(timestamp,Nr.tracks)) +
  geom_point(color="goldenrod3")+
  geom_line(size=1,color='goldenrod3')+
  ggtitle("Nov 2018") +
  theme(axis.title.y = element_text(size=18), legend.text=element_text(size=12), 
        legend.title=element_text(size=16, face="bold"), legend.position = "bottom",
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14, angle = 90), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Year") + ylab("MTR (#/km/h)")+ylim(0,1000)+
  scale_x_datetime(breaks="days",labels = date_format("%Y-%m-%d"))

library(ggpubr)
library(grid)  



migrants <- ggarrange(p1, p2,p3, ncol=1, nrow=3) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
png(paste0("C:/Users/mbradar/Documents/Robin/","MTR_2018_2",".png"),height=9,width=14,unit="in",res=500)
annotate_figure(migrants,top = textGrob("MTRs Luchterduinen",gp=gpar(fontsize=24,fontface="bold")))
dev.off()
