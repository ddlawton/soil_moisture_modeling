########################
# Sandhills Soil Moisture
#  Inflitration event modeling
#   December 31,2021
#    Douglas Lawton
#######################

# The goal is to create a dataframe that includes about 20 hours of data around each rainfall event.
# The rainfall events days are to be determined by you. The specific time of the rainfall events 
# are to be determined by the max soil moisture reading time stamp.

# The final dataframe should at least have the following columns:

# row ID column
# Site column
# Treatment column
# Block column
# Event column (just a factored integer specifying with event the specific row corresponds to)
# Date column
# Time difference column


rm(list=ls())

library(tidyverse) # for all your data management and visualizing needs
library(lubridate) # To help manage the date columns
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(data.table) # for the command 'fread' which is a faster version of read.csv


# load in the data (I have already merged all the sensor data into one csv)

dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% mutate(
  Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year))

dat_2020 <- dat %>% filter(Year == 2020) %>% filter(Block != 2) # get rid of block 2 in year 2020

dat <- dat %>% filter(Year==2021) %>% rbind(dat_2020) %>% #recombined the data (there might be a better way of doing this)
  mutate(year_DOY = paste0(Year,"_",DOY))

# Now determine what specific days you would like to look at
# Here I just use the ones you gave me


infilevents <- as_tibble(fread("data/raw/Infiltration and Evaporation Events - Inflitration events - R friendly format.csv")) %>%
  mutate(Year = factor(Year), Site = factor(Site),Rain_class = factor(`Rain class`), Sensor_issues = factor(`Sensor Issues?`)) %>%
  select(1:4,7,8) %>% separate(Hour,into=c("start","end"))  %>% 
  mutate(start = factor(start), end =factor(end)) %>% rowid_to_column(var="event") %>%
  mutate(event = factor(event), DOY= yday(Day), year_DOY = paste(Year,DOY,sep="_"))

# Now we need to filter out the sensor data for the specific dates you identified.
# Dont try to filter using the Date column as since there is a time (0-24h) associated with each date, filtering
# wont work properly (e.g. if you filter for "2020-07-24" this will return "2020-07-25 00:00:00" only)
# Instead use a hack: combined the Year and DOY columns into one (i've already done this for the dat and infilevents datafame)


days <- infilevents$year_DOY

days_rainfall <- infilevents %>% select(year_DOY,Rain_class) #create a list of the factor levels you want to filter



max_points <- dat %>% 
  filter(year_DOY %in% days) %>% 
  group_by(Year,Site,Block,Trt,Date.x) %>%
  summarize(X5 = mean(X5), X15 = mean(X15), X30 = mean(X30),
            TWS = mean(TWS), year_DOY = first(year_DOY)) %>%
  group_by(Year,Site,Block,Trt,year_DOY) %>% 
  mutate(X5_max = max(X5,na.rm=TRUE),  #friendly reminder, if there are NAs in the data, a lot of base commands wont work by default. use na.rm=TRUE to fix it
         X5_timepoint = case_when(X5 == X5_max ~ Date.x)) %>% 
  select(1:4,year_DOY,ends_with("max"),ends_with('_timepoint')) %>%
        drop_na(X5_timepoint) %>% ungroup() %>%
  group_by(Year,Site,Block,Trt,year_DOY) %>%
  mutate(mean_Max_timepoint = mean(X5_timepoint),
         lower_bound_timepoint =  mean_Max_timepoint - hours(5),
         upper_bound_timepoint =  mean_Max_timepoint + hours(5))  %>%
  select(lower_bound_timepoint,mean_Max_timepoint,upper_bound_timepoint) %>%
  left_join(days_rainfall,by="year_DOY")


dat2 <- dat  %>% # %in% tells the filter() command to find all factor levels that match the list
  group_by(Year,Site,Block,Trt,Date.x) %>%
  summarize(X5 = mean(X5), year_DOY = first(year_DOY)) %>% ungroup() %>%
  group_by(Year,Site,Block,Trt,year_DOY) %>%
  left_join(max_points,by=c("Year","Site","Block","Trt","year_DOY")) %>%
  filter(between(Date.x,lower_bound_timepoint,upper_bound_timepoint)) %>% drop_na(mean_Max_timepoint) %>%
  mutate(time_diff = difftime(Date.x,mean_Max_timepoint,units="hours")) 


ggplot(dat2,aes(x=time_diff,y=X5)) + geom_smooth(aes(color=Trt),method = "gam", formula = y ~ s(x, bs = "tp", k = 50))  + 
  facet_grid(Rain_class~Site)         

view(dat2 %>% group_by(year_DOY,Max_time_point))

# Important!!! All depths will reach peak soil moisture at different time points. Consider creating a column to see the
# temporal lag between soil moisture peak at the depths. 







#####
# Old code
# Abandon hope ye who enter
###



event1 %>% group_by(Year,Site,Block,Trt) %>%
  summarize(n=n(),max_x5 = max(X5))

event1_2 <- event1 %>% filter(X5 == 0.168) %>%
  mutate(Date.future = Date.x + hours(1),
         Date.past = Date.x - hours(1)) %>% 
  select(Date.x, Date.future,Date.past)

infilevents <- as_tibble(fread("data/raw/Infiltration and Evaporation Events - Inflitration events - R friendly format.csv")) %>%
  mutate(Year = factor(Year), Site = factor(Site),Rain_class = factor(`Rain class`), Sensor_issues = factor(`Sensor Issues?`)) %>%
  select(1:4,7,8) %>% separate(Hour,into=c("start","end"))  %>% 
  mutate(start = factor(start), end =factor(end)) %>% rowid_to_column(var="event") %>%
  mutate(event = factor(event), DOY= yday(Day), year_DOY = paste(Year,DOY,sep="_"))

event1 <- dat %>%
  filter(DOY == 183, Year == 2021, Site == "dryland") %>%
  mutate(event = "2021-07-02",Rain_class = "Heavy",Sensor_issues = "NA",
         start = 14,end=14)

event2 <- dat %>%
  filter(DOY == 208, Year == 2021, Site == "dryland") %>%
  mutate(event = "2021-07-27",Rain_class = "Heavy",Sensor_issues = "NA",
         start = 18,end=19)

event3 <- dat %>%
  filter(DOY == 183, Year == 2021, Site == "dryland") %>%
  mutate(event = "2021-07-02",Rain_class = "Moderate",Sensor_issues = "NA",
         start = 7,end=7)


event4 <- dat %>%
  filter(DOY == 183, Year == 2021, Site == "dryland") %>%
  mutate(event = "2021-07-18",Rain_class = "Moderate",Sensor_issues = "NA",
         start = 19,end=22)

event5 <- dat %>%
  filter(DOY == 183, Year == 2021, Site == "dryland") %>%
  mutate(event = "2021-07-02",Rain_class = "Heavy",Sensor_issues = "missing 1NC, 30 cm",
         start = 14,end=14)

event6 <- dat %>%
  filter(DOY == 208, Year == 2021, Site == "dryland") %>%
  mutate(event = "2021-07-27",Rain_class = "Heavy",Sensor_issues = "missing 3NC, 15 cm",
         start = 18,end=19)

event7 <- dat %>%
  filter(DOY == 179, Year == 2021, Site == "irrigated") %>%
  mutate(event = "2021-06-28",Rain_class = "Moderate",Sensor_issues = "missing 1NC, 30 cm",
         start = 10,end=11)

event8 <- dat %>%
  filter(DOY == 183, Year == 2021, Site == "irrigated") %>%
  mutate(event = "2021-07-02",Rain_class = "Moderate",Sensor_issues = "missing 1NC, 30 cm",
         start = 7,end=7)


event9 <- dat %>%
  filter(DOY == 199, Year == 2021, Site == "irrigated") %>%
  mutate(event = "2021-07-18",Rain_class = "Moderate",Sensor_issues = "Missing 3NC, 15 cm",
         start = 19,end=22)

event10 <- dat %>%
  filter(DOY == 148, Year == 2021, Site == "irrigated") %>%
  mutate(event = "2021-05-28",Rain_class = "Moderate",Sensor_issues = "Missing 3NC, 15 cm",
         start = 19,end=22)

event10 <- dat %>%
  filter(DOY == 209, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-07-27",Rain_class = "Moderate",Sensor_issues = "NA",
         start = 16,end=16)

event11 <- dat %>%
  filter(DOY == 209, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-07-27",Rain_class = "Heavy",Sensor_issues = "1C, missing 15 cm",
         start = 11,end=11)

event12 <- dat %>%
  filter(DOY == 272, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-09-28",Rain_class = "Heavy",Sensor_issues = "3NC, missing 30 cm",
         start = 11,end=11)

event13 <- dat %>%
  filter(DOY == 172, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-06-20",Rain_class = "Low",Sensor_issues = "3NC, missing 15, 30 cm",
         start = 0,end=7)

event14 <- dat %>%
  filter(DOY == 211, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-07-29",Rain_class = "Low",Sensor_issues = "1C, missing 15 cm",
         start = 2,end=5)

event15 <- dat %>%
  filter(DOY == 238, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-08-25",Rain_class = "Low",Sensor_issues = "4NC, missing 30 cm",
         start = 9,end=10)

event16 <- dat %>%
  filter(DOY == 220, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-08-07",Rain_class = "Moderate",Sensor_issues = "NA",
         start = 23,end=23)

event17 <- dat %>%
  filter(DOY == 155, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-06-03",Rain_class = "Moderate",Sensor_issues = "NA",
         start = 17,end=17)

event18 <- dat %>%
  filter(DOY == 162, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-06-10",Rain_class = "Moderate",Sensor_issues = "NA",
         start = 19,end=20)

event19 <- dat %>%
  filter(DOY == 242, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-08-29",Rain_class = "Low",Sensor_issues = "3NC and 4NC, missing 30 cm",
         start = 9,end=11)

event20 <- dat %>%
  filter(DOY == 163, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-06-11",Rain_class = "Heavy",Sensor_issues = "NA",
         start = 3,end=4)

event21 <- dat %>%
  filter(DOY == 187, Year == 2020, Site == "irrigated") %>%
  mutate(event = "2020-07-05",Rain_class = "Low",Sensor_issues = "3C, missing 30 cm",
         start = 16,end=20)

event1 %>% group_by(Site,Block, Trt) %>% filter(Hour==5) %>%
  summarize(n=n())

events <- rbind(event1,event2,event3,event4,event5,event6,event7,event8,event9,event10,event11,event12,event13,event14,event15,
                event16,event17,event18,event19,event20,event21) %>%
  mutate(event = factor(event),
         event_hours_start = Hour - start,
         event_hours_end = (Hour - end) - (end - start))


event1 %>% group_by(Year,Site,Block,Trt) %>%
  summarize(n=n(),max_x5 = max(X5))

event1_2 <- event1 %>% filter(X5 == 0.168) %>%
  mutate(Date.future = Date.x + hours(10),
         Date.past = Date.x - hours(10)) %>% 
  select(Date.x, Date.future,Date.past)

event1_3 <- event1 %>% filter(between(Date.x, event1_2$Date.past,event1_2$Date.future)) %>%
  mutate(time_diff = difftime(Date.x,event1_2$Date.x,units="hours"))
summary(event1_3)

ggplot(event14,aes(x=Hour,y=X30,color=Block,linetype=Trt)) + geom_smooth(method="gam") + geom_point() + 
  geom_vline(aes(xintercept = start)) + geom_vline(aes(xintercept = end)) + geom_vline(aes(xintercept = end)) 



ggplot(event1,aes(x=Hour,y=X5,color=Block)) + geom_smooth() +# geom_point() + 
  geom_vline(aes(xintercept = start)) + geom_vline(aes(xintercept = end)) +
  facet_grid(Site~Rain_class)


events %>% filter(Rain_class == "Heavy") %>%
  mutate(Trt = factor(Trt),Site = factor(Site), event = factor(event)) %>%
  ggplot(aes(x=Hour,y=X5,group=Trt,color=(Trt))) + geom_smooth() + geom_point() + facet_wrap(~Site + event)

    
events %>% filter(Rain_class == "Moderate") %>%
  mutate(Trt = factor(Trt),Site = factor(Site), event = factor(event)) %>%
  ggplot(aes(x=Hour,y=X5,group=Trt,color=(Trt))) + geom_smooth() + geom_point() + facet_wrap(~Site + event)
    
    

year_DOYs <-unique(infilevents$year_DOY)

dat %>% filter(Year == "2021") %>% rbind(dat_2020) %>%
  mutate(year_DOY=paste(Year,DOY,sep="_")) %>%
  filter(year_DOY == "2020_187")


dat_2020 <- dat %>% filter(Year == "2020") %>% filter(Block != "2")


flevels <- c("low","moderate","heavy")




filter_dat <- dat %>% filter(Year == "2021") %>% rbind(dat_2020) %>%
  mutate(year_DOY=paste(Year,DOY,sep="_")) %>%
  filter(year_DOY %in% year_DOYs) %>% droplevels() %>% 
  left_join(infilevents,by="year_DOY") %>%
  select(1:22,25:29) %>% 
  mutate(date.numeric = as.numeric(Date.x),Rain_class = factor(Rain_class,levels=flevels))


events <- unique(filter_dat$year_DOYs) 



tail(filter_dat %>% group_by(event) %>%
       summarize(n=n()))

plots <- list()
for (i in year_DOY){
  filter_dat_temp <- filter_dat %>% filter(year_DOY == i) %>% droplevels() 
  title <- unique(filter_dat_temp$Day)
  plots[[i]] <- filter_dat_temp %>%
    ggplot(aes(x=Hour,y=X5)) + #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) + 
    geom_vline(aes(xintercept = as.numeric(as.character(start)))) +
    geom_vline(aes(xintercept = as.numeric(as.character(end)))) +
    geom_point(aes(color=Block)) + 
    geom_smooth(aes(color=Block),method = "gam", formula = y ~ s(x, bs = "cr", k = 15)) +
    facet_grid(Site.x ~ Rain_class + Trt,scales="free") +
    ggtitle(title)
}


plots[16]

lapply(1:26, function(i)
  ggsave(filename=paste0("your_path",i,".jpeg"), plot=plotlist[[x]]))

as.numeric(as.character((filter_dat %>% filter(event == 3))$start))





ggplot(filter_dat,aes(x=Hour,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) + 
  geom_vline(aes(xintercept = as.integer(start))) +
  geom_vline(aes(xintercept = as.integer(end))) +
  facet_grid(Site.x ~ Rain_class + event,scales="free")

