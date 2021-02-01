library(ggpubr)

birds <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring_2020_br.csv",sep=";")
weather <- read.csv("C:/Users/mbradar/Documents/Robin/data/spring_2020_wr.csv",sep=";")
rownames(weather) <- NULL
joined <- left_join(birds,weather,by=c("time","altitude_layer"))
my_data1 <-  joined %>%
  group_by(altitude_layer)%>%
  summarise(count=sum(count,na.rm = T), wind_ass=mean(wind_ass),temp=mean(temp),rh=mean(r),
            crwc=mean(crwc),cc=mean(cc))


#wind_assistance
ggscatter(my_data1, x = "wind_ass", y = "count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "wind_ass", ylab = "count")
#temp 
ggscatter(my_data1, x = "temp", y = "count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "temp", ylab = "count")
#cc
ggscatter(my_data1, x = "cc", y = "count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "cc", ylab = "count")
#crwc
ggscatter(my_data1, x = "crwc", y = "count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "crwc", ylab = "count")
#rh
ggscatter(my_data1, x = "rh", y = "count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "rh", ylab = "count")

my_data2 <- joined %>%
  group_by(night.x,altitude_layer) %>%
  summarise(count=sum(count,na.rm = T), wind_ass=mean(wind_ass),temp=mean(temp),rh=mean(r),
            crwc=mean(crwc),cc=mean(cc))

alt_count <- my_data2 %>%
  group_by(night.x) %>%
  slice(which.max(count))

alt_temp <- my_data2 %>%
  group_by(night.x) %>%
  slice(which.max(temp))

alt_wind <- my_data2 %>%
  group_by(night.x) %>%
  slice(which.max(wind_ass))

alt_rh <- my_data2 %>%
  group_by(night.x) %>%
  slice(which.min(rh))

alt_cc <- my_data2 %>%
  group_by(night.x) %>%
  slice(which.min(cc))

alt_crwc <- my_data2 %>%
  group_by(night.x) %>%
  slice(which.min(crwc))



my_data3 <- Reduce(function(...) merge(..., by="night.x",all.x=TRUE), list(alt_count, alt_temp, alt_wind,alt_crwc,alt_rh,alt_cc))
my_data4 <- my_data3[,c(1:2,9,16,23,30,37)]
colnames(my_data4)[colnames(my_data4)==c("night.x","altitude_layer.x","altitude_layer.y","altitude_layer.x.1",
                                         "altitude_layer.y.1","altitude_layer.x.2","altitude_layer.y.2")] <-
                                       c("night","alt_cnt","alt_t","alt_w","alt_cw","alt_rh","alt_cc")
#wind
ggscatter(my_data4, x = "alt_w", y = "alt_cnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "alt_w", ylab = "alt_count")
#temperature
ggscatter(my_data4, x = "alt_t", y = "alt_cnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "alt_t", ylab = "alt_count")
#cc
ggscatter(my_data4, x = "alt_cc", y = "alt_cnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "alt_cc", ylab = "alt_count")
#humidity
ggscatter(my_data4, x = "alt_rh", y = "alt_cnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "alt_rh", ylab = "alt_count")
#crwc
ggscatter(my_data4, x = "alt_cw", y = "alt_cnt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "alt_cw", ylab = "alt_count")
