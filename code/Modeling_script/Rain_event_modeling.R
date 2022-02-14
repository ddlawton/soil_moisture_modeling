########################
# Sandhills Soil Moisture
#  Rainevent exploration and
#   Modeling
#   December 28,2021
#    Douglas Lawton
#######################

rm(list=ls())


library(tidyverse) # for all your data management and visualizing needs
library(mgcv) # the most population GAM package
library(gratia) # just some nice model diagnostics and results plotting 
library(lubridate) # To help manage the date columns
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(data.table) # for the command 'fread' which is a faster version of read.csv


dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% mutate(
  Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year))


dat %>% group_by(Boot) %>% filter(Year == "2021") %>% summarize(min = min(Date.x), mean= mean(Date.x), max = max(Date.x))
dat %>% group_by(Boot) %>% filter(Year == "2020") %>% summarize(min = min(Date.x), mean= mean(Date.x), max = max(Date.x))

irrigated <- dat %>% filter(Site == "irrigated") %>% filter(Year == "2021") %>% select(logger_mm,DOY,Hour)
dryland <- dat %>% filter(Site == "dryland") %>% filter(Year == "2021") %>% select(logger_mm,DOY,Hour)

view(irrigated %>% left_join(dryland,by=c("DOY","Hour")) %>% mutate(dif = logger_mm.x -logger_mm.y))

test <- (dat %>% filter(Year == "2021") %>% mutate(DOY = yday(Date.x)) %>% group_by(DOY) %>% mutate(daiy_log = sum(logger_mm),water_dif = daiy_log - Precip))



P1 <- dat %>% filter(Year == "2020") %>%
ggplot(aes(x=Date.x,y=logger_mm,colour=Boot,group=Boot)) + geom_line() + ggtitle("Year 2020") +
  theme_pubr()

P2 <- dat %>% filter(Year == "2021") %>%
ggplot(aes(x=Date.x,y=logger_mm,colour=Boot,group=Boot)) + geom_line() + ggtitle("Year 2021") +
  theme_pubr()

P3 <- dat %>% filter(Year == "2020") %>%
  ggplot(aes(x=Date.x,y=Precip,colour=Boot,group=Boot)) + geom_line() + ggtitle("Year 2020") +
  theme_pubr()

P4 <- dat %>% filter(Year == "2021") %>%
  ggplot(aes(x=Date.x,y=Precip,colour=Boot,group=Boot)) + geom_line() + ggtitle("Year 2021") +
  theme_pubr()


(P1 + P2) / (P3 + P4)



pre_dat <- dat %>% filter(Year == "2021") %>%
  filter(between(DOY,139,235))



ggplot(pre_dat,aes(x=Date.x,y=TWS,color=Site)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50))



post_dat <- dat %>% filter(Boot == "Post") %>% filter(Year == "2020") %>%
  filter(between(DOY,244,255))

pre_dat_long <- dat %>% filter(Boot == "Pre") %>% filter(Year == "2020") %>%
  filter(between(DOY,150,160)) %>% pivot_longer()

post_dat_long <- dat %>% filter(Boot == "Post") %>% filter(Year == "2020") %>%
  filter(between(DOY,221,225))


summary(post_da)


P5 <- pre_dat %>% filter(Block != "2") %>% filter(Trt == "NC") %>%
  ggplot(aes(x=Date.x,y=X5,color=Block))  +  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 100)) + 
  coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Cover") # + geom_hline(yintercept = 3.5, color = "orange")


P5 <- pre_dat %>% filter(Trt == "C") %>%
ggplot(aes(x=Date.x,y=TWS)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 100)) + 
  coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Cover")  # + ylim(0,.4)

P6 <- pre_dat %>% filter(Trt == "NC") %>%
  ggplot(aes(x=Date.x,y=TWS)) + geom_point() + geom_smooth(method = "gam", 
formula = y ~ s(x, bs = "tp", k = 100)) + coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Non-Cover") # + ylim(0,.4)

P7 <- post_dat %>% filter(Trt == "C") %>%
  ggplot(aes(x=Date.x,y=TWS)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 100)) + 
  coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Cover")   #+ ylim(0,.4)

P8 <- post_dat %>% filter(Trt == "NC") %>%
  ggplot(aes(x=Date.x,y=TWS)) + geom_point() + geom_smooth(method = "gam", 
                                                                      formula = y ~ s(x, bs = "tp", k = 100)) + 
  coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Non-Cover")  #+ ylim(0,.4)

P5 + P6 + P7 + P8


P5 <- pre_dat %>% filter(Trt == "C") %>%
  ggplot(aes(x=Date.x,y=X15,color=Block)) + geom_point() + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 100)) + 
  coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Cover") 

P6 <- pre_dat %>% filter(Trt == "NC") %>%
  ggplot(aes(x=Date.x,y=X15,color=Block)) + geom_point() + geom_smooth(method = "gam", 
                                                                      formula = y ~ s(x, bs = "tp", k = 100)) + coord_cartesian(ylim=c(0,NA)) + theme_pubr() + ggtitle("Non-Cover") 

P5 + P6  

pre_dat %>% pivot_longer(cols=starts_with("X"),names_to = "Depth",values_to = "Soil_moisture") %>%
  ggplot(aes(x=Date.x,y=Soil_moisture,color=Depth)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 100)) + 
  coord_cartesian(ylim=c(0,NA)) + theme_pubr() 


model_dat <- pre_dat %>% 
  mutate(Date_numeric = as.numeric(Date.x)) %>% drop_na(Date_numeric,Trt,TWS,X5,X15,X30,Block)

model_dat_long_cover <- pre_dat %>% pivot_longer(cols=starts_with("X"),names_to = "Depth",values_to = "Soil_moisture") %>%
  mutate(Depth = factor(Depth), Date_numeric = as.numeric(Date.x)) %>% filter(Trt == "C")

model_dat_long_no_cover <- pre_dat %>% pivot_longer(cols=starts_with("X"),names_to = "Depth",values_to = "Soil_moisture") %>%
  mutate(Depth = factor(Depth), Date_numeric = as.numeric(Date.x)) %>% filter(Trt == "NC")



mod_long_c <- bam(Soil_moisture ~ s(Date_numeric,by=Depth,bs="gp",k=100) + s(Block,bs="re"),
                select=TRUE,data=model_dat_long_cover,family=scat())

mod_long_c_fs <- bam(Soil_moisture ~ s(Date_numeric,Depth,bs="fs",k=100) + s(Block,bs="re"),
                select=TRUE,data=model_dat_long_cover,family=scat())

mod_long_nc <- bam(Soil_moisture ~ s(Date_numeric,by=Depth,bs="gp",k=100) + s(Block,bs="re"),
                  select=TRUE,data=model_dat_long_no_cover,family=scat())

mod_long_nc_fs <- bam(Soil_moisture ~ s(Date_numeric,Depth,bs="fs",k=100) + s(Block,bs="re"),
                     select=TRUE,data=model_dat_long_no_cover,family=scat())



draw(mod_long_c)
draw(mod_long_nc)

mod <- bam(TWS ~ s(Date_numeric,by=Trt,bs="gp",k=100) + s(Block,bs="re"),
           select=TRUE,data=model_dat,family=scat())

X5_mod <- bam(X5 ~ s(Date_numeric,by=Trt,bs="gp",k=100) + s(Block,bs="re"),
              select=TRUE,data=model_dat,family=scat())

X15_mod <- bam(X15 ~ s(Date_numeric,by=Trt,bs="gp",k=100) + s(Block,bs="re"),
              select=TRUE,data=model_dat,family=scat())

X30_mod <- bam(X30 ~ s(Date_numeric,by=Trt,bs="gp",k=100) + s(Block,bs="re"),
              select=TRUE,data=model_dat,family=scat())

model_dat2 <- add_partial_residuals(model_dat,X5_mod) %>%
  rename(X5_Date_TrtC = `s(Date_numeric):TrtC`,X5_Date_TrtNC = `s(Date_numeric):TrtNC`)

model_dat3 <-add_partial_residuals(model_dat2,X15_mod) %>%
  rename(X15_Date_TrtC = `s(Date_numeric):TrtC`,X15_Date_TrtNC = `s(Date_numeric):TrtNC`) 

model_dat4 <-add_partial_residuals(model_dat3,X30_mod) %>%
  rename(X30_Date_TrtC = `s(Date_numeric):TrtC`,X30_Date_TrtNC = `s(Date_numeric):TrtNC`)  %>%
  select(2:21,starts_with("X"))


draw(X5_mod)
draw(X15_mod)
draw(X30_mod)
draw(mod)


ggplot(model_dat2,aes(x=Date_numeric,y=`s(Date_numeric):TrtC`)) + geom_smooth()

model_dat4 %>% pivot_longer(cols=c(starts_with("X5_")),names_to = "Cover",values_to = "pred") %>%
  ggplot(aes(x=Date_numeric,y=pred,color=Cover)) + geom_smooth()




summary(mod)
draw(mod)
appraise(mod)
acf(resid(mod), lag.max = 36, main = "ACF")
pacf(resid(mod), lag.max = 36, main = "pACF")

post_dat <- dat %>% filter(Boot == "Post")  %>% filter(Year == "2020") %>%
  filter(between(DOY,206,210))


max(pre_dat$logger_mm)
