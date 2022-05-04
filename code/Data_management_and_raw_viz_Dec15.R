########################
# Sandhills Soil Moisture
#  GAM modeling
#   December 15,2021
#    Douglas Lawton
#######################

rm(list=ls()) # in case you need to clear your working space

#libraries

library(tidyverse) # for all your data management and visualizing needs
library(mgcv) # the most population GAM package
library(gratia) # just some nice model diagnostics and results plotting 
library(lubridate) # To help manage the date columns
library(ggridges) # adds ridgeline plots to the ggplot environment
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(readxl) # able to read in excel format

# load in data
# the data was given to me in three separate CSVs. Lets combine it all 
# The final data set will have the following variables:


# TWS (total water storage)
# Block (randomized block)
# Trt (Treatment -- cover or no cover)
# Site (irrigated or dryland)
# Date (Date and time)
# DOY (day of year)
# Hour (Hour of day)
# Year (year -- 2020 or 2021)
# MOD (Minute of day)
# Depth (The three depths)
# Moisture (Soil moisture)


sand20 <- as_tibble(read.csv("data/raw/Sandhills20.csv")) %>%
  mutate(Time = parse_date_time(Time,orders="mdy HM"), 
         Block = factor(Block),
         Trt = factor(Trt),
         Site = factor("irrigated"))

irr21 <- as_tibble(read.csv("data/raw/Irrigated21.csv")) %>%
  mutate(Time = parse_date_time(Time,orders="mdy HM"),
         Block = factor(Block),
         Trt = factor(Trt),
         Site = factor("irrigated"))

dry21 <- as_tibble(read.csv("data/raw/Dryland21.csv")) %>%
  mutate(Time = parse_date_time(Time,orders="mdy HM"),
         Block = factor(Block),
         Trt = factor(Trt),
         Site = factor("dryland"))

combined <- sand20 %>% rbind(irr21,dry21) %>%
  mutate(Date = Time, DOY = yday(Time), 
         Hour = hour(Date), Year = (year(Date)),
         MOD = hour(Date)*60 + minute(Date)) %>% 
  select(!Time) %>%
  mutate(Boot = factor(Boot)) 

combined2 %>% group_by(Boot) %>% filter(Year == "2021") %>% summarize(min = min(Date.x), mean= mean(Date.x), max = max(Date.x))

# I am combining some basic weather station data to the dataset

JACK <- as_tibble(read.csv("data/raw/Weather_station/QV2YC4V9_1.csv")) %>%
  rbind(read.csv("data/raw/Weather_station/QV2YC4V9_2.csv")) %>%
  mutate(Date = parse_date_time(Date.Time..Eastern.,orders="ymd hms"), 
         Air_temp = as.numeric(Top.of.the.Hour.Air.Temperature..C.),
         Precip = as.numeric(Total.Precipitation..mm.),
         Solar = as.numeric(Average.Solar.Radiation..W.m2.)) %>%
  dplyr::select(5:8) %>% mutate(Year = (year(Date)),DOY = (yday(Date)),Hour = (hour(Date))) %>%
  filter(Precip < 500) # theres a weird reading here

dat_logger <- read_excel("data/raw/Sandhills exp rain data.xlsx",sheet=1) %>% 
  rbind(read_excel("data/raw/Sandhills exp rain data.xlsx",sheet=3)) %>%
  mutate(Year = year(Date), DOY = yday(Date)) %>%
  rename(logger_mm = mm,logger_rainclass = rainclass)

classlevel <- c("none","low","moderate","heavy")

combined2 <- combined %>% 
  left_join(JACK,by=c("Year","DOY","Hour")) %>% 
  left_join(dat_logger,by=c("Year","DOY","Hour")) %>% select(!Date.y) %>%
  mutate(
    logger_mm = replace_na(logger_mm,0),
    logger_rainclass = replace_na(logger_rainclass,"none"),
    logger_rainclass = factor(logger_rainclass,levels=classlevel)
  )

levels(factor(combined2$logger_rainclass))

ggplot(combined2,aes(x=logger_rainclass,y=logger_mm)) + geom_boxplot()

write.csv(combined2,file="data/processed/combined_Dec282021.csv")

# Now lets do so data visualization
# First off, some basic visualization of the dependent variable by the various categorical variables
# there are some extreme values (soil moisture ~ 0.60) as such i have *logged* the data.

Depth <- ggplot(combined, aes(x = Moisture, y = Depth)) +
  geom_density_ridges() +
  xlab("Soil Moisture") +
  labs(title = 'Soil moisture by soil depth') +
  theme_pubr() + scale_x_continuous(trans='log10') 

Site <- ggplot(combined, aes(x = Moisture, y = Site)) +
  geom_density_ridges_gradient() +
  xlab("Soil Moisture") +
  labs(title = 'Soil moisture by site') +
  theme_pubr() + scale_x_continuous(trans='log10') 

Trt <- ggplot(combined, aes(x = Moisture, y = Trt)) +
  geom_density_ridges_gradient() +
  xlab("Soil Moisture") +
  ylab("Treatment") +
  labs(title = 'Soil moisture by treatment')  +
  theme_pubr() + scale_x_continuous(trans='log10') 

Hour <- ggplot(combined, aes(x = Moisture, y = factor(Hour))) +
  geom_density_ridges_gradient() +
  xlab("Soil Moisture") +
  labs(title = 'Soil moisture by hour of day') +
  theme_pubr() + scale_x_continuous(trans='log10') 

Year <- ggplot(combined, aes(x = Moisture, y = factor(Year))) +
  geom_density_ridges_gradient() +
  xlab("Soil Moisture") +
  labs(title = 'Soil moisture by year') +
  theme_pubr() + scale_x_continuous(trans='log10') 

hist <- Depth + Site + Trt + Hour + Year

#ggsave(hist,file="output/figures/ridgeline_plots/logged_ridgelines.png",width=15,height=10,units='in',dpi=300)



# Now lets vizualize how soil moisture is inlfuenced by the various independent variables
# I have chose to really look at two time component within day (minute of day) and day of year.

## Overall daily and seasonal graphing

DOY <- ggplot((combined),aes(x=DOY,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Day of Year") +
  scale_y_continuous(labels = paste(seq(0, 0.2, by = 0.05)), 
                     breaks = seq(0, 0.2, by = 0.05), 
                     limits = c(0, 0.7)) +
    coord_cartesian(ylim=c(0,.2))

MOD <- ggplot((combined),aes(x=MOD,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Minute of Day") +
  scale_y_continuous(labels = paste(seq(0, 0.2, by = 0.05)), 
                     breaks = seq(0, 0.2, by = 0.05), 
                     limits = c(0, 0.7)) +
  coord_cartesian(ylim=c(0,.2))

DOYxMOD <- DOY / MOD

ggsave(DOYxMOD,file="output/figures/Raw_data_smooths/DOYxMOD.png",width=5,height=10,units='in',dpi=300)


## Overall daily and seasonal graphing by site

DOY_SITE <- ggplot((combined %>% sample_n(size=50000)),aes(x=DOY,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Day of Year") + facet_wrap(~Site) +
  scale_y_continuous(labels = paste(seq(0, 0.15, by = 0.05)), 
                     breaks = seq(0, 0.15, by = 0.05), 
                     limits = c(0, 0.7)) +
  coord_cartesian(ylim=c(0,.15))


MOD_SITE <- ggplot((combined %>% sample_n(size=50000)),aes(x=MOD,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Minute of Day") + facet_wrap(~Site) +
  scale_y_continuous(labels = paste(seq(0, 0.15, by = 0.05)), 
                     breaks = seq(0, 0.15, by = 0.05), 
                     limits = c(0, 0.7)) +
  coord_cartesian(ylim=c(0,.15))

DOY_SITExMOD_SITE <- DOY_SITE / MOD_SITE

ggsave(DOY_SITExMOD_SITE,file="output/figures/Raw_data_smooths/DOY_SITExMOD_SITE.png",width=0,height=10,units='in',dpi=300)


## Overall daily and seasonal graphing by Treatment


DOY_Trt <- ggplot((combined %>% filter(between(DOY, 150,160))),aes(x=DOY,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Day of Year") + facet_wrap(~Trt) +
  scale_y_continuous(labels = paste(seq(0, 0.15, by = 0.05)), 
                     breaks = seq(0, 0.15, by = 0.05), 
                     limits = c(0, 0.7)) +
  coord_cartesian(ylim=c(0,.15))


MOD_Trt <- ggplot((combined %>% sample_n(size=50000)),aes(x=Hour,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Minute of Day") + facet_wrap(~Trt) +
  scale_y_continuous(labels = paste(seq(0, 0.15, by = 0.05)), 
                     breaks = seq(0, 0.15, by = 0.05), 
                     limits = c(0, 0.7)) +
  coord_cartesian(ylim=c(0,.15))

DOY_TrtxMOD_Trt <- DOY_Trt / MOD_Trt

ggsave(DOY_TrtxMOD_Trt,file="output/figures/Raw_data_smooths/DOY_TrtxMOD_Trt.png",width=10,height=10,units='in',dpi=300)


# Two dimensional viz

 

ggplot((combined),aes(y=MOD,x=DOY,z=(Moisture))) + stat_summary_hex() + theme_pubr() +
  ylab("Minute of Day") + xlab("Day of Year") + facet_grid(Depth~Site + Trt) +
  scale_fill_viridis()
  
  
ggplot((combined %>% sample_n(size=10000)), aes(x=MOD,y=Moisture)) + geom_point()  





## Filtering out for just dryland

dryland <- combined %>% filter(Site == "dryland")



## Overall daily and seasonal graphing by Treatment
summary(dryland$Date)

DOY_Trt <- ggplot((dryland %>% filter(between(DOY,155,170))),aes(x=Date,y=Moisture,color=Depth)) + geom_smooth() + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  ylab("Soil Moisture") + xlab("Day of Year") + facet_wrap(~Trt)
DOY_Trt


MOD_dat <- dryland %>% filter(Date > "2021-08-14" & Date <"2021-08-15")  %>% filter(Block == 1) %>% filter(between(MOD,0,1400)) %>% filter() %>% mutate(date_numeric = as.numeric(Date), Depth = factor(Depth))

MOD_Trt <- ggplot((dryland %>% sample_n(size=25000)),aes(x=DOY,y=Moisture,color=Trt))+ 
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) + geom_point(size=0.1) + geom_smooth(method="gam") +
  ylab("Soil Moisture") + xlab("Date")  + theme_pubr() 
MOD_Trt

DOY_TrtxMOD_Trt <- DOY_Trt / MOD_Trt
DOY_TrtxMOD_Trt
ggsave(DOY_TrtxMOD_Trt,file="output/figures/Raw_data_smooths/DOY_TrtxMOD_Trt.png",width=10,height=10,units='in',dpi=300)

library(mgcv)

mod <- gam(Moisture ~ s(date_numeric,by=Trt,k=15) + s(date_numeric,Depth,k=15,bs="fs"),data=MOD_dat,select=TRUE)
summary(mod)
gratia::draw(mod)
k.check(mod)

ggplot(combined, aes(x=MOD,y=Moisture,color=Depth)) + geom_smooth(method = "loess")
