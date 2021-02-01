data<-data.frame(LATITUDE=runif(100,1,100),LONGITUDE=runif(100,1,100));

## Add the latitudes and longitudes between which each observation is located
library(tidyverse)
library(lubridate)
library(ggplot2)
library(raster)
library(circular)
library(ggquiver)
library(rWind)

vert <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring2019vert.csv",sep=";")
mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/autumn2019mixedandvert.csv",sep=";")
mix <- rbind(vert,mix)
mix$dir_rad <- circular(mix$phi_diff, type = "angles", units = "radians", zero = 0, rotation = "clock")
mix$dir_deg <- mix$phi_diff * 180/pi
mix$dir_deg  <- ifelse(mix$dir_deg <0, 360+mix$dir_deg , mix$dir_deg )
mix$dir_deg <- circular(mix$dir_deg, units = "degrees",modulo ="2pi", template="geographic")
mix$delta_t <- difftime(mix$timestamp_end,mix$timestamp_start,units = "secs")
mix$delta_t <- as.numeric(mix$delta_t)
mix$groundspeed <- mix$distance_travelled/mix$delta_t

oneday <- subset(mix, night=="2019-10-28 - 2019-10-31")
mix <- read.csv("C:/Users/mbradar/Documents/Robin/Hans.csv",sep = ";")
oneday <- subset(oneday,altitude>=0)
oneday$dir_deg <- ifelse(oneday$tracktype=='RaEl',oneday$dir_deg==NA, oneday$dir_deg)
oneday$dir_deg <- circular(oneday$dir_deg,units = "degrees",modulo ="2pi",template="geographic")
oneday$timestamp_start <- as.POSIXct(oneday$timestamp_start)
class(oneday$timestamp_start)
oneday$tc <- cut(oneday$timestamp_start, breaks = "15 min")
oneday$tc <- as.POSIXct(oneday$tc)
pts <- oneday[,c('tc','altitude_layer',"dir_deg","groundspeed")]

## You can substitute any number of breaks you want. Or, a vector of fixed cutpoints
## LATgrid and LONgrid are going to be factors. With ugly level names.

pts <- oneday[,c('tc','altitude_layer')]
## Create a single factor that gives the lat,long of each observation. 
pts$IDgrid<-with(pts,interaction(tc,altitude_layer))

## Now, create another factor based on the above one, with shorter IDs and no empty levels
pts$IDNgrid<-factor(pts$IDgrid)
levels(pts$IDNgrid)<-seq_along(levels(pts$IDNgrid))

## If you want total grid-cell count repeated for each observation falling into that grid cell, do this:
pts$count<- ave(pts,pts$IDNgrid,FUN=length);
## You could have also used data$LONGITUDE, doesn't matter in this case

## If you want just a table of counts at each grid-cell, do this:
tab <- aggregate(pts$tc,pts[,c('tc','altitude_layer','IDNgrid')],FUN=length)
dir <- aggregate(pts$dir_deg,pts[,c('tc','altitude_layer','IDNgrid')], 
                 function(x) mean.circular(x,units = "degrees",modulo ="2pi", 
                                           template="geographic",na.rm=T))
gsp <- aggregate(pts$groundspeed, pts[,c('tc','altitude_layer','IDNgrid')], FUN=mean)
znj <- left_join(tab,dir,by=c("tc","altitude_layer", "IDNgrid"))
tab <- left_join(znj,gsp,by=c("tc","altitude_layer", "IDNgrid"))
names(tab)[c(4,5,6)]<-paste(c("count","dir","gsp"))
tab$dir <- circular(tab$dir,units = "degrees",modulo ="2pi",template="geographic")
tab$u <- ds2uv(tab$dir,tab$gsp)[,1]
tab$v <- ds2uv(tab$dir,tab$gsp)[,2]

tab.keep = unique(tab$u)[seq(2, length(unique(tab$u)), 5)]


windows(30,15)
s <- ggplot(data = tab,
              aes(x = tc, y = altitude_layer, fill = count)) +
  geom_tile() +
  geom_quiver(aes(x = tc, y = altitude_layer,u=u,v=v),col="black",vecsize=1.5,size = 0.1,center = FALSE)+
  theme(
    #legend.position = c(.95, .95),
    legend.justification = c("right"),
    #legend.title = element_text('density'),
    legend.box.just = "right",
    legend.background=element_blank(),    
   #legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(color = "black",size=6),
    axis.text.x = element_text(angle = 90),
    legend.title = element_text(color = "black", size = 10)
  ) +
  xlab('time (15 min)') + 
  ylab('altitude (m)') +
  theme(panel.background = element_rect(fill='white'))+
  scale_fill_gradientn('nr.tracks', limits=c(0, 700),
                      breaks = seq(0,700,30),  
                      colours = c("yellow","#fd8d3c","#bd0026","darkblue"))+
  guides(fill = guide_colourbar(barwidth = 0.5, barheight = 10))
ggsave(filename=paste0("C:/Users/mbradar/Documents/Robin/20201014.png"),s,dpi=500)

ggplot(tab[1:10,]) +
  geom_segment(aes(x = tc,
                   y = 0,
                   xend = tc+ lubridate::dseconds (gsp * 1 * -cos((90-dir) / 360 * 2 * pi)),
                   yend = gsp * 1 * -sin((90-dir) / 360 * 2 * pi),
                   col = factor(tc)
  ),
  arrow = arrow(length = unit(0.5, "cm")) ) +
  geom_point(aes(tc, 0), size = 1) +
  ylim(0,100)+
 # coord_fixed(3600) +
  theme(legend.position = "none")

## I included the LATgrid and LONgrid vectors so there would be some 
## sort of descriptive reference accompanying the anonymous numbers in IDNgrid,
## but only IDNgrid is actually necessary

## If you want a really minimalist table, you could do this:
table(data$IDNgrid);