##Connect to e-ecology

db.file <-"MyRobinData" #provide the name of the database or file for ODBC driver
library(RODBC) 
Rdb <- odbcConnect(db.file)#establish connection with database
tables <- sqlTables(Rdb)#view the table names
tracks <- sqlColumns(Rdb, "track")#view specific columns of the table


trackestimate <- sqlQuery(Rdb, "select * from public.trackestimate where timestamp between '2018-10-14 19:00:00' and '2018-10-15 07:00:00' 
                                and airspeed<28
                                order by timestamp")

track <- sqlQuery(Rdb, "select * from public.track where timestamp_start between '2018-10-14 19:00:00' and '2018-10-15 07:00:00' 
                        and classification_id=5
                        order by timestamp_start")

track <-  sqlQuery(Rdb, "select * from public.track limit 100")
trackestimate <- sqlQuery(Rdb, "select * from public.trackestimate limit 100")
metainfo <- sqlQuery(Rdb, "select * from public.tracking_metainfo")
weather <-  sqlQuery(Rdb, "select * from public.weather limit 100 offset 5")
class <- sqlQuery(Rdb, "select * from public.classification limit 100 offset 5")
image <- sqlQuery(Rdb, "select * from public.image limit 100 offset 5")
classification <- sqlQuery(Rdb,"select * from public.classification limit 100")

altitude <-  sqlQuery(Rdb, "select id, timestamp,position, ST_X(position), ST_Y(position), ST_Z(position)
                           from trackestimate limit 1000 offset 3000") #to extract altitude info
