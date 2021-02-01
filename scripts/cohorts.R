filtMinRange <-  800      # [m] Minimum range from radar of track 
filtMaxRange <-  3000  
radarQuery <- "SELECT r.id,
r.location_name,
ST_X((ST_DumpPoints(r.position)).geom),
ST_Y((ST_DumpPoints(r.position)).geom),
ST_Z((ST_DumpPoints(r.position)).geom)
FROM radartype as rt
JOIN radar as r ON (rt.id = r.radartype_id)
WHERE rt.radar_type = 'HORIZONTAL'
ORDER BY id DESC LIMIT 1"
radarData <- data.table(dbGetQuery(con, radarQuery))

# Extract x and y coordinates
radarCoords <- paste(toString(radarData$st_x),toString(radarData$st_y),sep = ' ')


track <- dbGetQuery(con, "select id,timestamp_start,phi_diff,
                        FROM public.track WHERE 
                        timestamp_start BETWEEN '2020-02-15 00:00:00' AND '2020-06-01 00:00:00'
                        and extract(hour from timestamp_start) in (19,20,21,22,23,0,1,2,3,4,5,6,7)
                        AND classification_id BETWEEN 4 AND 15
                        AND tracktype in ('RaAz', 'RaAzEl')
                        AND nr_of_plots >= 30
                        AND rho_diff >= 200")
                        

track$dir_rad <- circular(track$phi_diff, type = "angles", units = "radians", zero = 0, rotation = "clock")
track$dir_deg <- track$phi_diff * 180/pi
track$dir_deg  <- ifelse(track$dir_deg <0, 360+track$dir_deg , track$dir_deg )
track$dir_deg <- circular(track$dir_deg, units = "degrees",modulo ="2pi", template="geographic")
track$Date <- date(track$timestamp_start)
#autumn
track_s1 <- subset(track,dir_deg>=255 & dir_deg<=285)
track_s2 <- subset(track,dir_deg>=180 & dir_deg<235)

#spring
track_s1 <- subset(track,dir_deg>=75 & dir_deg<=105)
track_s2 <- subset(track,dir_deg>=0 & dir_deg<65)

windows(8,5)
ggplot(track_s1, aes(Date)) +
  geom_bar(stat="count",col="black",fill="black") +
  ggtitle("Number of tracks flying towards E Spring 2020 Gemini") +
  theme_bw()+
  theme(axis.title.y = element_text(size=16), legend.text=element_text(size=9), 
        legend.title=element_text(size=12, face="bold"), legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y=element_text(size=14), axis.text.x=element_text(size=14), plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title.x = element_blank(), plot.title = element_text(size = 18, face = "bold"))+
  xlab("Day") + ylab("Number of track") +
scale_x_date(date_breaks="months", date_labels="%m", limits=c(as.Date("2020-02-15"), as.Date("2020-06-01")))
