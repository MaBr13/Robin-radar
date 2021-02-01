library(viridis)
library(ggplot2)
library(sf)
library(DBI)
library(RPostgreSQL)
library(raster)
library(beepr)

Sys.setenv(TZ='UTC')

# QUERY TO EXTRACT VERTICAL RADAR INFORMATION 
rr_vertical_query <- function(schema, day = TRUE, interv = c('2018-11-01 00:00:00','2018-11-30 00:00:00') ){
  # schema = specify the schema where the track data is stored
  # day = if day is true than all data of a specific day is used. Otherwise the function uses data during the night (one hour after sunset and one hour before sunrise)
  if(day){
    tt <- st_read(con, query = paste0("WITH z AS (
	WITH y AS (
		WITH x AS (
  	WITH radar AS (
        SELECT *, timestamp start_time, lead(timestamp) over (order by timestamp) end_time 
				FROM radar JOIN radartype on (radartype_id = radartype.id) WHERE radar_type = 'VERTICAL'
				)
  			SELECT position, start_time, CASE WHEN end_time is null then now() else end_time END end_time, timestamp FROM radar
			)
			SELECT track.id track_id, st_transform(position,3035) radar_location,
			st_centroid(st_transform(trajectory,3035)),
			CASE WHEN st_y(st_centroid(st_transform(trajectory,3035))) > st_y(st_transform(position,3035)) THEN 
			st_distance(st_transform(position,3035),st_centroid(st_transform(trajectory,3035))) ELSE 
			- st_distance(st_transform(position,3035),st_centroid(st_transform(trajectory,3035))) END distance,
			st_z((st_dumppoints(st_transform(trajectory,3035))).geom),
			track.*, 
		  rise_set_time(st_y(position),st_x(position),TRUE,timestamp_start) sunrise,
			rise_set_time(st_y(position),st_x(position),FALSE,timestamp_start) sunset
			FROM x, ",schema,".track 
			WHERE timestamp_start BETWEEN start_time AND end_time --AND avg_corrected_mass between 29.40 and 70.80 
			AND tracktype  in ('RaEl') AND	
			timestamp_start BETWEEN '", interv[1] , "' AND '", interv[2] ,"' 
		)
	SELECT track_id,
	avg(st_z) altitude,
	stddev(st_z) altitude_sd, 
	radar_location, 
	st_centroid,
	distance, 
	classification, 
	timestamp_start, 
	nr_of_plots,
	timestamp_start::date datex, 
	extract(hour from timestamp_start) hourx,
	floor((avg(st_z))/100)*100 altitude_layer, 
	sunrise,
	sunset, 
	CASE WHEN timestamp_start between sunrise and sunset THEN 'day' 
		WHEN timestamp_start < sunrise THEN 'night' 
		WHEN timestamp_start > sunset THEN 'evening' END sunriseset,
	avg_corrected_mass,
  phi_diff,
  timestamp_end,
  tracktype,
	FROM y JOIN classification cc on (cc.id=y.classification_id) 
	GROUP BY track_id, radar_location, st_centroid, distance, classification, phi_diff, tracktype,
  timestamp_end, 	nr_of_plots,avg_corrected_mass,timestamp_start,timestamp_start::date, 
  extract(hour from timestamp_start),sunrise, sunset, sunriseset
	ORDER BY y.timestamp_start
	)
SELECT *, 
CASE WHEN sunriseset = 'evening' THEN (datex || ' - ' || (datex::date + interval '1 day')::date)
WHEN sunriseset = 'night' THEN ((datex - interval '1 day')::date || ' - ' || datex::date) END night
FROM z 
WHERE
classification in ('SMALL_BIRD','MEDIUM_BIRD','LARGE_BIRD','FLOCK') 
ORDER BY timestamp_start;"))
  } else { 
    tt <- st_read(con, query = paste0("WITH z AS (
	WITH y AS (
		WITH x AS (
  			WITH radar AS (
        SELECT *, timestamp start_time, lead(timestamp) over (order by timestamp) end_time 
				FROM radar JOIN radartype on (radartype_id = radartype.id) WHERE radar_type = 'VERTICAL'
				)
  			  SELECT position, start_time, CASE WHEN end_time is null then now() else end_time END end_time, timestamp FROM radar
			)
			SELECT track.id track_id,st_transform(position,3035) radar_location, 
			st_centroid(st_transform(trajectory,3035)),
			CASE WHEN st_y(st_centroid(st_transform(trajectory,3035))) > st_y(st_transform(position,3035)) THEN 
			st_distance(st_transform(position,3035),st_centroid(st_transform(trajectory,3035))) ELSE 
			- st_distance(st_transform(position,3035),st_centroid(st_transform(trajectory,3035))) END distance,
			st_z((st_dumppoints(st_transform(trajectory,3035))).geom),
			track.*, 
			rise_set_time(st_y(position),st_x(position),TRUE,timestamp_start) sunrise,
			rise_set_time(st_y(position),st_x(position),FALSE,timestamp_start) sunset
			FROM x, ",schema,".track 
			WHERE timestamp_start BETWEEN start_time AND end_time --AND avg_corrected_mass between 29.40 and 70.80 
			AND tracktype  in ('RaEl') AND 
			timestamp_start BETWEEN '", interv[1] , "' AND '", interv[2] ,"' 
		)
	SELECT track_id,
	avg(st_z) altitude,
	stddev(st_z) altitude_sd, 
	radar_location, 
	st_centroid, 
	distance, 
	classification, 
	timestamp_start,
	nr_of_plots,
	timestamp_start::date datex, 
	extract(hour from timestamp_start) hourx,
	floor((avg(st_z))/100)*100 altitude_layer, 
	sunrise,
	sunset, 
	CASE WHEN timestamp_start between sunrise and sunset THEN 'day' 
		WHEN timestamp_start < sunrise THEN 'night' 
		WHEN timestamp_start > sunset THEN 'evening' END sunriseset,
	avg_corrected_mass,
  phi_diff,
  timestamp_end,
  tracktype
	FROM y JOIN classification cc on (cc.id=y.classification_id) 
	GROUP BY track_id,radar_location,st_centroid, distance, classification, phi_diff,tracktype, 
  timestamp_end,nr_of_plots,avg_corrected_mass,timestamp_start,timestamp_start::date,
  extract(hour from timestamp_start),sunrise, sunset, sunriseset
	ORDER BY y.timestamp_start
	)
SELECT *, 
CASE WHEN sunriseset = 'evening' THEN (datex || ' - ' || (datex::date + interval '1 day')::date)
WHEN sunriseset = 'night' THEN ((datex - interval '1 day')::date || ' - ' || datex::date) END night
FROM z 
WHERE sunriseset in ('night','evening') AND 
classification in ('SMALL_BIRD','MEDIUM_BIRD','LARGE_BIRD','FLOCK') AND
(timestamp_start < sunrise - interval '1 hour' OR timestamp_start > sunset + interval '1 hour') 
ORDER BY timestamp_start;"))
  }
  
  
  
  return(tt)  
}



rr_vertical_density <- function(data=tt, res=50,limits=NULL,vertical_range=c(0,4000),horizontal_range=c(-2000,2000),dir='/Users/jedgroev/ownCloud/Compsup (Projectfolder)/PLAATJES/VERTICAL_RADAR/EHEH/') {
  # data = table imported with the function rr_vertical_query
  # res = resolution of interest in m 
  # limits = limits of the color scale values 
  # vertical range = altitude range which you would like to make calculations for
  # horizontal range = distance from the radar location you would like to make calculations for
  # dir = location where to store the resulting figures 
  xmn <- horizontal_range[1]; xmx <- horizontal_range[2]; ymn <- vertical_range[1]; ymx <- vertical_range[2]
  resolution <- res
  r <- raster(ncol=abs(xmn - xmx)/resolution,nrow=ymn+ymx/resolution, xmx=xmx, xmn=xmn,ymx=ymx,ymn=ymn)
  #pts = data.frame(x=runif(10,xmn,xmx), y=runif(10,ymn,ymx))
  pts <- data[,c('distance','altitude')]
  pts$radar_location <- NULL
  
  pointcount = function(r, pts){
    # make a raster of zeroes like the input
    r2 = r
    r2[] = 0
    # get the cell index for each point and make a table:
    counts = table(cellFromXY(r,pts))
    # fill in the raster with the counts from the cell index:
    r2[as.numeric(names(counts))] = counts
    return(r2)
  }
  
  r_density = pointcount(r, pts)
  
  r_density_df <- as.data.frame(r_density, xy=TRUE)
  colnames(r_density_df) <- c('x','y','Density')
  gg_r_dns <- ggplot() +
    #theme_void() +
    geom_raster(data = r_density_df, aes(x = x, y = y, fill = Density)) + 
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      #legend.title = element_text('density'),
      legend.box.just = "right",
      legend.background=element_blank(),    
      legend.margin = margin(6, 6, 6, 6),
      legend.text = element_text(color = "white",),
      legend.title = element_text(color = "white", size = 10)
    ) +
    xlab('Distance (m)') + 
    ylab(NULL) +
    theme(panel.background = element_rect(fill='white')) 
  if(is.null(limits)){
    gg_r_dns <- gg_r_dns + scale_fill_viridis()
  } else {
    gg_r_dns <- gg_r_dns + scale_fill_viridis(limits=limits)
  }
  
  #scale_fill_gradientn(colours=c("#FFFFFFFF","#0000FFFF","#FF0000FF"))
  #+
  #facet_wrap(~ variable,ncol = ncol) +
  #scale_fill_gradientn(colours = colours, limits=c(min(limits),max(limits))) 
  ag_dis <- aggregate(r_density_df$Density, by=list(Category=r_density_df$x), FUN=sum)
  ag_alt <- aggregate(r_density_df$Density, by=list(Category=r_density_df$y), FUN=sum)
  
  gg_l_dis <- ggplot() + 
    geom_line(data=ag_dis, aes(x=Category, y=x)) + 
    xlim(c(xmn,xmx)) +
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank()) + 
    theme(panel.background = element_rect(fill='white'))  + 
    ylab(NULL) + 
    geom_vline(xintercept=0, linetype="dashed", color = "grey") + 
    geom_vline(xintercept=500, linetype="dashed", color = "grey") + 
    geom_vline(xintercept=-500, linetype="dashed", color = "grey") 
  
  
  gg_l_alt <- ggplot() + 
    geom_line(data=ag_alt, aes(x=Category, y=x)) + 
    xlim(c(ymn,ymx)) + 
    coord_flip() + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())+ 
    theme(panel.background = element_rect(fill='white')) +
    ylab('birds (n)') + 
    #geom_vline(xintercept=1000, linetype="dashed", color = "grey") + 
    #geom_vline(xintercept=750, linetype="dashed", color = "grey") + 
    #geom_vline(xintercept=500, linetype="dashed", color = "grey") + 
    geom_vline(xintercept=250, linetype="dashed", color = "grey") 
  
  # total 
  gg_p_dns <- ggplot() + geom_point(data=data, 
                                    fill=NA,
                                    aes(x=distance,y=altitude,
                                        colour=altitude_layer),
                                    shape=20,
                                    size=2,
                                    alpha = 1/10) + 
    xlim(c(xmn,xmx)) + 
    ylim(c(ymn,ymx)) + 
    theme(legend.position = "none") + 
    theme(panel.background = element_rect(fill='white')) + 
    xlab('Distance (m)') + 
    ylab('Altitude (m)')
  
  gg_h_dis <- ggplot() + 
    geom_histogram(data=data, aes(distance),binwidth=resolution) + 
    xlim(c(xmn,xmx)) + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank()) + 
    theme(panel.background = element_rect(fill='white')) + 
    ylab('birds (n)') + 
    geom_vline(xintercept=0, linetype="dashed", color = "grey") + 
    geom_vline(xintercept=500, linetype="dashed", color = "grey") + 
    geom_vline(xintercept=-500, linetype="dashed", color = "grey") 
  
  
  gg_h_alt <- ggplot() + 
    geom_histogram(data=data, aes(altitude),binwidth=resolution) + 
    xlim(c(ymn,ymx)) + 
    coord_flip() + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank()) + 
    theme(panel.background = element_rect(fill='white')) + 
    ylab('birds (n)') + 
    #geom_vline(xintercept=1000, linetype="dashed", color = "grey") + 
    #geom_vline(xintercept=750, linetype="dashed", color = "grey") + 
    #geom_vline(xintercept=500, linetype="dashed", color = "grey") + 
    geom_vline(xintercept=250, linetype="dashed", color = "grey") 
  
  empty <- ggplot()+geom_point(aes(1,1), colour="white")+
    theme(axis.ticks=element_blank(), 
          panel.background=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),           
          axis.title.x=element_blank(), axis.title.y=element_blank())
  
  library(gridExtra)
  library(grid)
  period <- paste0(min(data$datex), ' to ', max(data$datex))
  g<-grid.arrange(gg_h_dis, empty, gg_l_dis, empty,
                  gg_p_dns, gg_h_alt, gg_r_dns, gg_l_alt,
                  ncol=4, nrow=2, 
                  widths=c(4, 1, 4, 1), 
                  heights=c(1, 4),
                  top = textGrob(paste0(period,' (',resolution,'*',resolution,')'),gp=gpar(fontsize=20,font=3)))
  if (is.null(vertical_range)){
    title <- paste0(gsub(' ', '', gsub('-','',period)),'_',resolution,'x',resolution)
    
  } else {
    title <- paste0(gsub(' ', '', gsub('-','',period)),'_',resolution,'x',resolution,'_',vertical_range[1],'_',vertical_range[2])
  }
  ggsave(paste0(dir,title,'.png'),g,width=12,height=6)
}


# SCRIPT 

# set the database of interest 
database <-'gemini' # SET PARAMETER 
# set directory where you want to save the output (here I chose the home directory)
dir <- '~/Robin/plots/radar_beam/combination'
# connect to the database 
con <- dbConnect("PostgreSQL", 
                    dbname=database, # database name  
                    host='robin1.e-ecology.nl', 
                    user= "maja_bradaric",# username
                    password="55nebitno992@") # password


############## only night tracks ##############

# IMPORT DATA 
tts <- rr_vertical_query(schema='public', # select the schema
                        day=FALSE, # if day is false only tracks during the night of the specified interval are imported
                        interv=c('2018-11-01 00:00:00','2018-12-01 00:00:00')) 
beep()
# FILTER DATA - filter tracks to keep only birds (removes mainly insects)
tt_filtered <- subset(tts, avg_corrected_mass >= 29.40 & avg_corrected_mass <= 70.80)
library()
# DENSITY PLOTS 

# without limits of the color range (limits automatically set)
rr_vertical_density(data=tt_filtered, 
                    res=50,
                    limits=NULL,
                    vertical_range=c(0,4000),
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))

# specify the color range manually (limits argument)
rr_vertical_density(data=tt_filtered, 
                    res=50,
                    limits=c(1,20000), # set the limits of the color scale (1 so that all cells with 0 tracks are grey)
                    vertical_range=c(0,4000),
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))

# only interested in the lower layer? 
rr_vertical_density(data=tt_filtered, 
                    res=50,
                    limits=c(1,20000), 
                    vertical_range=c(0,500), # changed the maximum altitude
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))
# you can also adjust the resolution 
rr_vertical_density(data=tt_filtered, 
                    res=10, # set the resolution to 10x10m
                    limits=c(1,2000), 
                    vertical_range=c(0,500),
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))

############## full day tracks ##############
# IMPORT DATA 
tt_day <- rr_vertical_query(schema='public', # select the schema
                            day=TRUE, # if day is true all tracks during the specified interv are imported
                            interv=c('2018-11-09 00:00:00','2018-11-10 00:00:00')) 
# FILTER DATA - filter tracks to keep only birds (removes mainly insects)
tt_day_filtered <- subset(tt_day, avg_corrected_mass >= 29.40 & avg_corrected_mass <= 70.80)


# DENSITY PLOTS 
 
# without limits of the color range (limits automatically set)
rr_vertical_density(data=tt_day_filtered, 
                    res=50,
                    limits=NULL,
                    vertical_range=c(0,4000),
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))

# specify the color range manually (limits argument)
rr_vertical_density(data=tt_day_filtered, 
                    res=50,
                    limits=c(1,20000), # set the limits of the color scale (1 so that all cells with 0 tracks are grey)
                    vertical_range=c(0,4000),
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))

# only interested in the lower altitude layer? 
rr_vertical_density(data=tt_day_filtered, 
                    res=50,
                    limits=c(1,20000), 
                    vertical_range=c(0,500), # changed the maximum altitude
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))
# you can also adjust the resolution 
rr_vertical_density(data=tt_day_filtered, 
                    res=10, # set the resolution to 10x10m
                    limits=c(1,2000),
                    vertical_range=c(0,500),
                    horizontal_range=c(-2000,2000),
                    dir=paste0(dir,toupper(database)))







