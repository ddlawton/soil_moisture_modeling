library(tidyverse) # for all your data management and visualizing needs
library(mgcv) # the most population GAM package
library(gratia) # just some nice model diagnostics and results plotting 
library(lubridate) # To help manage the date columns
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(data.table) # for the command 'fread' which is a faster version of read.csv
library(fuzzyjoin)


dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% mutate(
  Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year))

dat_2020 <- dat %>% filter(Year == 2020) %>% filter(Block != 2) # get rid of block 2 in year 2020

dat <- dat %>% filter(Year==2021) %>% rbind(dat_2020) %>% #recombined the data (there might be a better way of doing this)
  mutate(year_DOY = paste0(Year,"_",DOY))

# Now determine what specific days you would like to look at
# Here I just use the ones you gave me

view(as_tibble(fread("data/raw/Infiltration and Evaporation Events - Inflitration events - R friendly format.csv")))
infilevents <- as_tibble(fread("data/raw/Infiltration and Evaporation Events - Inflitration events - R friendly format.csv")) %>%
  mutate(Year = factor(Year), Site = factor(Site),Rain_class = factor(`Rain class`), Sensor_issues = factor(`Sensor Issues?`)) %>%
  select(1:4,7,8) %>% separate(Hour,into=c("start","end"))  %>% 
  mutate(start = factor(start), end =factor(end)) %>% rowid_to_column(var="event") %>%
  mutate(event = factor(event), DOY= yday(Day), year_DOY = paste(Year,DOY,sep="_"),
         end = as.factor(ifelse(is.na(end), paste(start),paste(end))),
         rain_start  = parse_date_time(paste0(Day," ",start,":00:00"),orders = "Ymd HMS"),
         rain_end  = parse_date_time(paste0(Day," ",end,":00:00"),orders = "Ymd HMS"),
         Time_start = parse_date_time(paste0(Day," ",start,":00:00"),orders = "Ymd HMS") - hours(5),
         Time_end = parse_date_time(paste0(Day," ",end,":00:00"),orders = "Ymd HMS") + hours(5)) %>% droplevels() %>%
  select(event,Rain_class,Sensor_issues,rain_start,rain_end,Time_start,Time_end)



dat2.5 <- fuzzy_inner_join(
  dat, infilevents,
  by = c(
    "Date.x" = "Time_start",
    "Date.x" = "Time_end"
  ),
  match_fun = list(`>=`, `<=`)
) 

dat3 <- dat2.5 %>% drop_na(event)


dat3 %>% filter(event=="1") %>% filter(Site == "dryland") %>% filter(Block == "1")  %>% filter(Trt == "C") %>% 
  filter(Date.x == parse_date_time("2021-07-02 09:00:00",orders = "Ymd HMS"))



%>% filter(Date.x == parse_date_time("2020-05-28 11:00:00",orders = "Ymd HMS"))

dat3 %>% count(event,Boot,Trt) %>% pivot_wider(names_from = "Boot",values_from = n)


dat4 <- dat3 %>% group_by(event, Site, Block, Trt) %>% 
  mutate(X5_max = max(X5,na.rm=TRUE),  
         X5_timepoint = case_when(X5 == X5_max ~ Date.x))



dat5 <- dat4 %>% group_by(event, Site, Block, Trt) %>% mutate(saturation = min(X5_timepoint,na.rm=TRUE),
                                                              diff_time = difftime(Date.x,saturation, units="hours")) %>% 
  mutate(diff_time = as.numeric(diff_time, units="hours")) %>% 
  filter(between(diff_time,-5,0))
  
ggplot((dat5),aes(x=diff_time,y=X5)) + geom_smooth()

ggplot((dat5 %>% filter(diff_time > -5)),aes(x=diff_time,y=X5,color=Block,linetype=Trt)) +geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 15),se=FALSE) +
  facet_grid(Site ~ Rain_class) 

ggplot(dat6, aes(x=diff_time,y=X5,color=Year)) + geom_smooth()


dat6 <- dat5 %>% filter(diff_time > -5) %>% drop_na(X5)
summary(dat6)







mod <- bam(X5 ~ te(diff_time,by=Rain_class,bs='ad') + 
             te(diff_time,Site,bs='fs') + 
             te(diff_time,Trt,bs='fs') + 
             s(Block,bs="re"),select=TRUE, dat=dat6)
summary(mod)
plot(mod)
gratia::draw(mod)
summary(dat5)

dat7 <- add_partial_residuals(dat6,mod)
names(dat7)
dat7 %>% select(32:36,Rain_class) %>%
  ggplot(aes(x=diff_time,y=`te(diff_time,Rain_class)`,color=Rain_class)) + geom_smooth()




ggplot(dat6,aes(x=X5)) + geom_histogram()



dat4.5 <- dat4 %>% group_by(event, Site, Block, Trt) %>% summarise(min = min(X5_timepoint,na.rm=TRUE), max = max(X5_timepoint,na.rm=TRUE)) %>%
  mutate(diff = case_when(
    min == max ~ 0,
    min != max ~ 1
  )) %>% filter(diff == "1") %>% mutate(diff_time = difftime(max,min,units="hours")) %>% arrange(diff_time)










dat5 <- dat3 %>% group_by(event) %>% summarize(
  min = min(Date.x),max = max(Date.x)
)

dat5 %>% left_join(infilevents,by="event")





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





dat2[infilevents, on = .(Date_numeric >= Time_start_numeric, Date_numeric <= Time_end_numeric)]

setDT(infilevents)[setDT(dat), 
             on = .(event, Date.x >= Time_start, Date.x <= Time_end), 
             event := event][]


dat2 <- dat %>% 
  mutate(dummy=TRUE) %>%
  left_join(infilevents %>% mutate(dummy=TRUE)) %>%
  filter(Date.x >= Time_start, Date.x < Time_end) %>%
  select(-dummy)


test <- dat2 %>% select(Date.x,Time_start,Time_end) %>% mutate(
  between = case_when(
    Date.x>=Time_start & Date.x<=Time_end ~ 1,
    TRUE ~ 0
  )) 






dat %>%
  mutate(event = case_when(
    Date.x >= infilevents$Time_start & Date.x <= infilevents$Time_end ~ infilevents$event
  ))


fuzzy_left_join( dat, infilevents,by = c("Date.x" = "Time_start","Date.x" = "Time_end"),match_fun = list(`>=`, `<=`))


