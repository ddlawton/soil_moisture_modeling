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
  mutate(year_DOY = paste0(Year,"_",DOY))  %>% drop_na(TWS)

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

infilevents %>% filter(event == 4 | event == 9)

dat2.5 <- fuzzy_inner_join(
  dat, infilevents,
  by = c(
    "Date.x" = "Time_start",
    "Date.x" = "Time_end"
  ),
  match_fun = list(`>=`, `<=`)
) 

dat3 <- dat2.5 %>% drop_na(event) %>% select(!Date) %>%
  rename(ID = "V1", Date = "Date.x") %>% select(ID,X5,X15,X30,TWS,Block,Trt,Date,Year,event,Time_start,Time_end,Rain_class,Site)



min_maxs <-dat3 %>% pivot_longer(cols=c(2:5), names_to = "Soil_variable", values_to = "soil_variable_value") %>%
  group_by(event, Site, Block, Trt,Soil_variable) %>%
  mutate(variable_max = max(soil_variable_value,na.rm=TRUE),
         variable_min = min(soil_variable_value,na.rm=TRUE),
         variable_time_max = case_when(soil_variable_value == variable_max ~ Date),
         variable_time_min = case_when(soil_variable_value == variable_min ~ Date)) %>%
  summarize(across(c(variable_time_max,variable_time_min), ~ first(na.omit(.))),
            variable_max = first(variable_max),
            variable_min = first(variable_min))



dat4 <- dat3 %>% pivot_longer(
  cols=c(2:5), names_to = "Soil_variable", values_to = "soil_variable_value") %>% 
  left_join(min_maxs,by=c("event", "Site", "Block", "Trt","Soil_variable")) %>%
  group_by(event, Site, Block, Trt,Soil_variable) %>%
  mutate(difftime =  as.numeric(difftime(Date,variable_time_max,units="hours")),
         waterdiff = variable_max - variable_min) %>% 
  filter(between(difftime,-5,0))


write.csv(dat4,file="data/processed/infiltration_modeling_data_Jan142022.csv")
