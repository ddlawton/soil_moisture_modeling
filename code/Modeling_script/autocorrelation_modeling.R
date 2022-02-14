#########################
# Sandhills Soil Moisture
#  Global Modeling
#   January 24,2021
#    Douglas Lawton
#######################

rm(list=ls())


library(tidyverse) # for all your data management and visualizing needs
library(mgcv) # the most population GAM package
library(gratia) # just some nice model diagnostics and results plotting 
library(lubridate) # To help manage the date columns
library(DHARMa)
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(data.table) # for the command 'fread' which is a faster version of read.csv
library(visreg)

dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% 
  mutate(Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year),
         Date_numeric = as.numeric(Date.x)) %>%
  select(TWS,V1,Block,Trt,Boot,MOD,Date.x,Date_numeric,Year,Site) %>% rename("ID" = V1, Date = "Date.x") %>% drop_na(TWS)

dat_2020 <- dat %>% filter(Year == "2020") %>% filter(Block != 2) %>% mutate(Doy = yday(Date))



## AR(0)
m1 <- gamm(TWS ~ te(Date_numeric,Trt,bs="fs",k=500) +
             te(Date_numeric,Block,bs="fs",k=250)+
             te(MOD,Trt,bs="fs",k=100) +
             te(MOD,Block,bs="fs",k=100),
           select=TRUE, data = dat_2020)



## AR(1)
 m1 <- gamm(TWS ~ te(Date_numeric,Trt,bs="fs",k=500) +
              te(Date_numeric,Block,bs="fs",k=250)+
              te(MOD,Trt,bs="fs",k=100) +
              te(MOD,Block,bs="fs",k=100),
            select=TRUE, data = dat_2020, 
            correlation = corARMA(form = ~ 1|Doy, p = 1))
 
saveRDS(m1,file="output/modle_AR1.rds") 
 
 
## AR(2)
m2 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),
            data = dat_2020, correlation = corARMA(form = ~ 1|Doy, p = 2))

saveRDS(m2,file="output/modle_AR2.rds") 
 
## AR(3)
m3 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),
            data = dat_2020, correlation = corARMA(form = ~ 1|Doy, p = 3))

saveRDS(m3,file="output/modle_AR3.rds")

## AR(4)
m4 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),
           data = dat_2020, correlation = corARMA(form = ~ 1|Doy, p = 4))

saveRDS(m4,file="output/modle_AR4.rds")


## AR(5)
m5 <- gamm(Temperature ~ s(nMonth, bs = "cc", k = 12) + s(Time, k = 20),
           data = dat_2020, correlation = corARMA(form = ~ 1|Doy, p = 5))


saveRDS(m5,file="output/modle_AR5.rds")

## AR(0)
m0 <- gamm(TWS ~ te(Date_numeric,Trt,bs="fs",k=500) +
             te(Date_numeric,Block,bs="fs",k=250)+
             te(MOD,Trt,bs="fs",k=100) +
             te(MOD,Block,bs="fs",k=100),
           select=TRUE, data = dat_2020)


saveRDS(m0,file="output/modle_AR0.rds")