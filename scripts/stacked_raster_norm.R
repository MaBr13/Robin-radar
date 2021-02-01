mix <- read.csv("C:/Users/mbradar/Documents/Robin/data/autumn2019mixedandvert.csv",sep=";")
mix <- rbind(vert,mix)
mix$dir_rad <- circular(mix$phi_diff, type = "angles", units = "radians", zero = 0, rotation = "clock")
mix$dir_deg <- mix$phi_diff * 180/pi
mix$dir_deg  <- ifelse(mix$dir_deg <0, 360+mix$dir_deg , mix$dir_deg )
mix$dir_deg <- circular(mix$dir_deg, units = "degrees",modulo ="2pi", template="geographic")
mix$delta_t <- difftime(mix$timestamp_end,mix$timestamp_start,units = "secs")
mix$delta_t <- as.numeric(mix$delta_t)
mix$groundspeed <- mix$distance_travelled/mix$delta_t
matx <- subset(mix,altitude>=0)
matx$dir_deg <- ifelse(matx$tracktype=='RaEl',matx$dir_deg==NA, matx$dir_deg)
matx$dir_deg <- circular(matx$dir_deg,units = "degrees",modulo ="2pi",template="geographic")
matx$timestamp_start <- as.POSIXct(matx$timestamp_start)
class(matx$timestamp_start)
matx$tc <- cut(matx$timestamp_start, breaks = "15 min")
matx$tc <- as.POSIXct(matx$tc)
pts <- matx[,c('tc','altitude_layer',"dir_deg","groundspeed","night")]
check <- split(pts,pts$night)

for(k in 1:length(check)){
  check[[k]]$IDgrid <- with(check[[k]],interaction(tc,altitude_layer))
  check[[k]]$IDNgrid<-factor(check[[k]]$IDgrid)
  levels(check[[k]]$IDNgrid)<-seq_along(levels(check[[k]]$IDNgrid))
}
tab <- list()
dir <- list()
gsp <- list()

for(k in 1:length(check)){
  tab[[k]] <- aggregate(check[[k]]$tc,check[[k]][,c('tc','altitude_layer','IDNgrid')],FUN=length)
  dir[[k]] <- aggregate(check[[k]]$dir_deg,check[[k]][,c('tc','altitude_layer','IDNgrid')], 
                   function(x) mean.circular(x,units = "degrees",modulo ="2pi", 
                                             template="geographic",na.rm=T))
  gsp[[k]] <- aggregate(check[[k]]$groundspeed, check[[k]][,c('tc','altitude_layer','IDNgrid')], FUN=mean)
}

znj <- list()

for(k in 1:length(tab)){
  znj[[k]] <- left_join(tab[[k]],dir[[k]],by=c("tc","altitude_layer", "IDNgrid"))
  tab[[k]] <- left_join(znj[[k]],gsp[[k]],by=c("tc","altitude_layer", "IDNgrid"))
  names(tab[[k]])[c(4,5,6)]<-paste(c("count","dir","gsp"))
  tab[[k]]$dir <- circular(tab[[k]]$dir,units = "degrees",modulo ="2pi",template="geographic")
  tab[[k]]$u <- ds2uv(tab[[k]]$dir,tab[[k]]$gsp)[,1]
  tab[[k]]$v <- ds2uv(tab[[k]]$dir,tab[[k]]$gsp)[,2]
  
}

rm(list = c("check","dir","gsp", "pts","znj"))

normalize_stack <- function (x) {
  ls <- list()
  for (i in 1:dim(x)[87]) {
    ls[[i]] <- scale(subset(x, i))
  }
  return(stack(ls))
}

normalize_stack(tab)

  