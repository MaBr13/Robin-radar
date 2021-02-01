### Directional components of flight
##Short summary

## A function to calculate the speed of movement between consecutive points ## this is your groundspeed
pt2pt.speed <- function(distance, duration){
  return(distance/duration)
}

#calculate u and v components from wind direction, flight direction or heading and wind speed, 
#groundspeed or airspeed
library(rWind)

ds2uv(d, s) #d is your direction in degrees and s is your accompanying speed 

#calculate wind speed (m/s) from u and v components (this also applies to groundspeed and airspeed, just use
# u and v components of that)

windspeedms <-  sqrt(yourdata$u_wind^2 + yourdata$v_wind^2) # square root from a sum of squares of your u and v components
 
#calculate wind direction (radians, directions that wind blows to)
  
winddir <- atan2(yourdata$u_wind, yourdata$v_wind)

#CALCULATE HEADING FROM WIND DIRECTION AND FLIGHT DIRECTION

#first calcuate sinus and cosinus of your flight direction and wind direction (have to be in radians)
# if it is easier for you have separate columns in your data for all measurements in rad and deg, since
# you might need them later in your calculations

winddir <- winddir*(pi/180) #conversion to radians
flight_direction <- flight_direction*(pi/180)

sflight<- sin(yourdata$flight_direction)#calculate sinus and cosinus of track and wind direction
cflight <- cos(yourdata$flight_direction)
swind <- sin(yourdata$winddir)
cwind <- cos(yourdata$winddir)

#use formulas from the Shamoun-Baranes et al. 2007 to calculate xa and ya components that define
#vector of heading and airspeed

xa <- (yourdata$groundspeedms*sflight)-(yourdata$windspeedms*swind)
ya <- (yourdata$groundspeedms*cflight)-(yourdata$windspeedms*cwind)
  
heading<- atan2(xa,ya) #calculate your heading in radians
airspeedms<-sqrt((xa^2)+(ya^2)) #calculate your airspeed (m/s) 

heading <- heading*(180/pi)#formula for conversion back to angles
winddir <- winddirR*(180/pi)
flight_direction <- flight_direction*(180/pi)

#when you convert data from radians to degrees, they tend to be on 180 modulo (range between +180 and -180)
#since you want them to be on a 360 modulo (range between 0 and 360) you have to do this step for all your
#directional measurements

heading <- ifelse(heading<0, 360+heading, heading)
winddir <- ifelse(winddir<0, 360+winddir, winddir)
flight_direction <- ifelse(flight_direction<0, 360+flight_direction, flight_direction)

##CALCULATE WIND ASSISTANCE BASED ON KEMP ET AL 2012

#assuming full wind drift

wind_assistance <- NCEP.Tailwind (yourdata$u_wind,yourdata$v_wind,yourdata$heading,yourdata$airspeed)

#the result is a small table, your wind assistance is the first column, to just extract that use this

wind_assistance <- NCEP.Tailwind (yourdata$u_wind,yourdata$v_wind,yourdata$heading,yourdata$airspeed)[,1]
  