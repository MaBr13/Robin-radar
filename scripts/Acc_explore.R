library(RODBC)
db_file <- "gps"
dbc <- odbcConnect(db_file)

acc.p.month <- sqlQuery(db,
         "select a.device_info_serial, extract(month from a.date_time) as month, extract(year from a.date_time) as year, count(a.z_acceleration)
          from gps.ee_acceleration_limited as a
          join gps.ee_track_session_limited as s on a.device_info_serial = s.device_info_serial
          where a. date_time >= s.start_date
          group by a.device_info_serial, month, year",  as.is = c(2))

acc.p.day <- sqlQuery(dbc,
                                     "select a.device_info_serial, extract(month from a.date_time) as month, extract(year from a.date_time) as year, 
extract(day from a.date_time) as day,count(a.z_acceleration)
          from gps.ee_acceleration_limited as a
          join gps.ee_track_session_limited as s on a.device_info_serial = s.device_info_serial
          where a. date_time >= s.start_date
          group by a.device_info_serial, day, month, year",  as.is = c(2))

close(dbc)

saveRDS(acc.p.month, "acc.p.month.180518.RDS")
saveRDS(acc.p.day, "acc.p.day.180518.RDS")

head(acc.p.month)

library(dplyr)
acc.p.month$month <- factor(acc.p.month$month, levels = c(6:12, 1:5))

acc.p.month <- acc.p.month %>% group_by(device_info_serial) %>%
  mutate(birdyear = ifelse(month %in% 6:12, year, year - 1) - min(year) + 1)

acc.p.month %>% 
  group_by(device_info_serial, birdyear) %>% 
  summarise(n.month = n()) %>% 
  filter(n.month >= 9) %>%  ##Which birds have data in most months
  arrange(desc(n.month)) %>%
  print(n=25)

acc.p.month %>% 
  filter(!(month %in% 5:7)) %>% ##Check for month outside of breeding period with at least 1 acc measurement
  group_by(device_info_serial, birdyear) %>% 
  summarise(n.month = n()) %>% 
  filter(n.month >= 9) %>% ##Look for birds with complete nonbreeding data
  arrange(n.month) %>%
  print(n=25)

acc.p.month %>% filter(device_info_serial ==5027) %>% arrange(birdyear, month) ##ACC stopped mid april
