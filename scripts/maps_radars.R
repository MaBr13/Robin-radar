library(ggmap)
library(ggplot2)
library(gridExtra)
library(ggpubr)


Boosted <- c(10.0468,54.0043)
Borkum <- c(6.7482,53.5640)
Chenies <- c(-0.53,51.69)
De_Bilt <- c(5.1783,52.1017)
Den_Helder <- c(4.7899,52.9533)
Emden <- c(7.0237,53.3387)
Herwijnen <- c(5.13797,51.83708)
Ingham <- c(-0.55,53.335)
Jabbeke <- c(3.0641,51.1919)
Rostock <- c(12.058,54.1757)
Thurnham <- c(0.6064,51.2942)
Zaventum <- c(4.4579,50.9054)
Widemount <- c(5.5044,49.9135)
K14 <- c(3.37,53.16)
Luchterduinen <- c(4.10,52.25)
OWEZ <- c(4.38,52.6)
Borselle_Alfa <- c(3,51.35)
Gemini <- c (5.57,54.02)
Thornton <- c(2.92,51.54)

weather_r <- data.frame(rbind(Boosted,Borkum,Chenies,De_Bilt,Den_Helder,Emden,Herwijnen,
                   Ingham,Jabbeke,Rostock,Thurnham,Zaventum,Widemount))
weather_r <- 
  weather_r %>%
  rename(
    Longitude=X1,
    Latitude=X2
  )

weather_r$rad.type <- as.factor(rep(1,nrow(weather_r)))
weather_r$rad.name <- as.character(row.names(weather_r))

bird.r <- as.data.frame(rbind(Luchterduinen,K14,OWEZ,Borselle_Alfa,Gemini,Thornton))
bird.r <- 
  bird.r %>%
  rename(
    Longitude=V1,
    Latitude=V2
  )
bird.r$rad.type <- as.factor(rep(2,nrow(bird.r)))
bird.r$rad.name <- as.character(row.names(bird.r))

radars <- rbind(weather_r,bird.r)
radars$col <- ifelse(weather_r$rad.type==1, radars$col==1,radars$col==2)

NSea <- get_map(location = c(lon=4.7, lat=54.5110 ), 
                maptype = "terrain-background",  source = "stamen", color="bw", zoom=5) #add a map
S <- ggmap(NSea)

windows(15,15)
S+geom_point(data=radars, aes_string(radars$Longitude,radars$Latitude,colour=radars$rad.type),size=5) + 
  scale_color_manual(name="Type",values = c("cyan4","yellow"),labels=c("Weather radar","Bird radar"))+
  #geom_text(aes(label=weather_r$rad.name),hjust=0, vjust=0)
 # scale_colour_manual("rad.name",values=c("darkmagenta", "dodgerblue4", "aquamarine3", "goldenrod3", "firebrick1"))+
  ylab("Latitude")+
  xlab  ("Longitude")+
  scale_x_continuous(limits = c(-1.2, 14.0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(49.0, 56.0), expand = c(0, 0))+
  theme(legend.position = "top")
  



