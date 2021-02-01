feb <- read.csv("C:/Users/mbradar/Documents/Robin/data/feb2019vert&mix.csv",sep=";")
mar <- read.csv("C:/Users/mbradar/Documents/Robin/data/march2019vert&mix.csv",sep=";")
aprv <- read.csv("C:/Users/mbradar/Documents/Robin/data/april2019vert.csv",sep=";")
aprm <- read.csv("C:/Users/mbradar/Documents/Robin/data/april2019mixed.csv",sep=";")
may <- read.csv("C:/Users/mbradar/Documents/Robin/data/may_2019mix&vert.csv",sep=";")

spring <- do.call("rbind", list(feb,mar,aprv,aprm,may))
spring2 <- subset(spring,datex>="2019-02-15")


write.table(spring2,"C:/Users/mbradar/Documents/Robin/data/spring2019mixed&vert.csv",sep=";")

aug <- read.csv("C:/Users/mbradar/Documents/Robin/data/aug_2019mixedandvert.csv",sep=";")
aug1 <- subset(aug, select = -c(nr_of_plots))
mix <-  read.csv("C:/Users/mbradar/Documents/Robin/data/autumn2019mixedandvert.csv",sep=";")

autumn <- do.call("rbind",list(aug1,mix))
write.table(autumn,"C:/Users/mbradar/Documents/Robin/data/autumn_2019mixedandvert.csv",sep=";")

feb <- read.csv("C:/Users/mbradar/Documents/Robin/data/feb_2020_mixed&vert.csv",sep=";")
mar <- read.csv("C:/Users/mbradar/Documents/Robin/data/march_2020_mixed&vert.csv",sep=";")
apr <- read.csv("C:/Users/mbradar/Documents/Robin/data/april_2020_mixed&vert.csv",sep=";")
may <- read.csv("C:/Users/mbradar/Documents/Robin/data/may_2020_mixed&vert.csv",sep=";")
aug <- read.csv("C:/Users/mbradar/Documents/Robin/data/aug_2020_mixed&vert.csv",sep=";")
sep <- read.csv("C:/Users/mbradar/Documents/Robin/data/sep_2020_mixed&vert.csv",sep=";")
oct <- read.csv("C:/Users/mbradar/Documents/Robin/data/oct_2020_mixed&vert.csv",sep=";")
nov <- read.csv("C:/Users/mbradar/Documents/Robin/data/nov_2020mixed&vert.csv",sep=";")

spring <- do.call("rbind",list(feb,mar,apr,may))
Autumn <- do.call("rbind",list(aug,sep,oct,nov))

write.table(spring,"C:/Users/mbradar/Documents/Robin/data/spring_2020_mixed&vert.csv",sep=';')
write.table(Autumn,"C:/Users/mbradar/Documents/Robin/data/Autumn_2020_mixed&vert.csv",sep=';')

all <- check2 %>%
  group_by(night) %>%
  summarise(vid=sum(count,na.rm = TRUE))

windows(30,15)

vid1 <- ggplot(all,aes(x=night,y=vid))+
  geom_line()+
  scale_x_date(breaks="hours", date_labels="%y-%m-%d",
               limits=c(as.Date("2020-02-15 00:00:00"), as.Date("2020-05-31 00:00:05")),expand = c(0,0))+
theme(panel.background = element_rect(fill='white'),axis.text.x = element_text(angle = 90))
ggsave("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/vid_spring2020.png",vid1,dpi=500)
dev.off()
vid3 <- ggplot(all,aes(x=night,y=vid))+
  geom_line()+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",
               limits=c(as.Date("2020-08-15"), as.Date("2020-11-30")),expand = c(0,0))+
  theme(panel.background = element_rect(fill='white'),axis.text.x = element_text(angle = 90))
ggsave("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/vid_autumn2020.png",vid3,dpi=500)

one <- subset(check2,night=="2020-10-16")
one <- one %>%
  group_by(time)%>%
  summarise(vid=sum(count,na.rm = TRUE))
windows(30,15)
vid2 <- ggplot(one,aes(x=time,y=vid))+
  geom_line()+
  scale_x_datetime(breaks="hours",limits=c(as_datetime(one$time[1]), as_datetime(max(one$time))),expand = c(0,0))+
  theme(panel.background = element_rect(fill='white'),axis.text.x = element_text(angle = 90))
ggsave("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/vid_20201016.png",vid2,dpi=500)
dev.off()
#SPRING
#load radar data
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring2019mixed&vert.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamp_start <- as.POSIXct(ymd_h(paste(date(vert$timestamp_start), vert$hourx,sep= ' ')))
#load rain data and filter rain minutes
rain <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_spring2019.csv", sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)
#AUTUMN
#load bird data
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/Autumn_2020_mixed&vert.csv",sep=";")
vert <- subset(mix,tracktype=='RaEl')
vert <- subset(vert,altitude>=0)
vert <- subset(vert,distance>=500 & distance<=1500 | distance<=-500 & distance>=-1500)
vert$timestamp <- as.POSIXct(ymd_hm(paste(date(vert$timestamp_start), vert$hourx,minute(vert$timestamp_start), sep= ' ')))
vert$timestamp_start <- as.POSIXct(ymd_h(paste(date(vert$timestamp_start), vert$hourx,sep= ' ')))
#load rain data and filter rain minutes
rain <- read.table("C:/Users/mbradar/Documents/Rain-Johannes/rain_autumn2020.csv", sep=";")
colnames(rain)[colnames(rain)==c("timestampx")] <- c("timestamp")
rain$timestamp <- as.POSIXct(rain$timestamp, tz="UTC")
vert <- left_join(vert,rain,by=c("timestamp"))
vert$mean<- na_interpolation(vert$mean, option ="linear")
vert <- subset(vert,vert$mean==0)

dens <- ggplot(spring19,aes(y=altitude))+
  geom_density(col="green",orientation="y",weight=2)+
  geom_density(data=autumn19,aes(y=altitude),col="orange",orientation="y",weight=2,show.legend=TRUE)+
  geom_density(data=spring20,aes(y=altitude),col="green",orientation="y",linetype="dashed",weight=5)+
  geom_density(data=autumn20,aes(y=altitude),col="orange",orientation="y",linetype="dashed",weight=2)+
  theme_bw()+
  ylim(0,1500)
 # theme(panel.background = element_rect(fill='white'))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/alt_dens_all.png"),dens,dpi=500)

spring <- rbind(spring19,spring20)
autumn20 <- autumn20[ , -which(names(autumn20) %in% c("nr_of_plots"))]
autumn <- rbind(autumn19,autumn20)

dens <- ggplot(spring,aes(y=altitude))+
  geom_density(col="green",orientation="y",weight=2)+
  geom_density(data=autumn,aes(y=altitude),col="orange",orientation="y",weight=2,show.legend=TRUE)+
  theme_bw()+
  ylim(0,1500)
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/alt_dens_full.png"),dens,dpi=500)

altdif <- subset(vert,altitude_layer>=0 & altitude_layer<500 | 
                   altitude_layer>=500 & altitude_layer<1000 | 
                   altitude_layer>=1000 & altitude_layer<1500)
altdif$height <- with(altdif,ifelse(altdif$altitude_layer>=0 & altdif$altitude_layer<=200,"low",
                             ifelse(altdif$altitude_layer>=500 & altdif$altitude_layer<=700,"medium","high")))
altdif$height = factor(altdif$height, levels=c("high","medium","low"), labels=c("high","medium","low"))

windows(30,15)
alts <- ggplot(altdif, aes(x = night, y = x)) + 
  geom_line(aes(color = height)) + 
  geom_hline(yintercept = 500,col="red")+
  facet_grid(height ~ ., scales = "free_y") + 
  ylab("count")+
  ylim(0,5500)+
  scale_x_date(breaks="days", date_labels="%y-%m-%d",
               limits=c(as.Date("2020-08-15"), as.Date("2020-11-30")),expand = c(0,0))+
  theme(panel.background = element_rect(fill='white'),axis.text.x = element_text(angle = 90),
        legend.position = "none")
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/altdif_Aaa.png"),alts,dpi=500)
dev.off()

windows(30,15)
perc1 <- ggplot(altdif, aes(x= night,  group=height)) + 
  geom_bar(aes(y = ..prop.., fill=height, stat="count")) +
  #geom_text(aes( label = scales::percent(..prop..),
  #               y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") +
  facet_grid(height ~ ., scales = "free_y") + 
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill='white')
  ) +
  scale_y_continuous(limits=c(0,.3),labels = scales::percent)
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/weather&alt/Luchterduinen/altdif_perc_adj_S.png"),perc1,dpi=500)  
dev.off()

ggplot(altdif, aes(night, fill = height)) +
  geom_bar(position = "fill")

ggplot(altdif, aes(x = night, y = x, fill = height)) + 
  geom_bar(stat = "identity")
