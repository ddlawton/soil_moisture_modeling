#########
# Averaging data
#  to daily
#  and filtering out just for drylands
#  2022
#########

rm(list=ls())

#main modeling and diagnostic packages
library(mgcv)
library(gratia)
library(DHARMa)


#Supporting packages

library(tidyverse) # for all your data management and visualizing needs
library(lubridate) # To help manage the date columns
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(data.table) # for the command 'fread' which is a faster version of read.csv
library(ggridges)
library(ggpubr)

# Functions

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}


#loading in data and doing some quick exploratory stuff


dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% 
  mutate(Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year),
         Date_numeric = as.numeric(Date.x)) %>%
  select(TWS,V1,Block,Trt,Boot,MOD,Date.x,Date_numeric,Year,Site) %>% rename("ID" = V1, Date = "Date.x") %>% drop_na(TWS)


ridgelines <- dat %>% 
  filter(Year == "2021" & Site == "dryland") %>%
  ggplot(aes(y=Trt,x=TWS)) +
  geom_density_ridges() +
  facet_wrap(~Block,scales="free") +
  theme_pubr()

dat_2021_dry <- dat %>% filter(Year == "2021" & Site == "dryland") %>%
  mutate(DOY = yday(Date)) %>%
  group_by(Block,Trt,DOY) %>%
  summarize(std = stderr(TWS), mean = mean(TWS))

ridgelines <- dat_2021_dry %>%
  pivot_longer(cols=3:5,names_to = "variable",values_to = "value") %>%
  ggplot(aes(y=Trt,x=value)) +
  geom_density_ridges() +
  facet_wrap(variable~Block,scales="free") +
  theme_pubr()
ridgelines

ggplot(dat_2021_dry,aes(x=std,y=mean))+ geom_point(shape=21,alpha=0.8)  + geom_smooth(size=1.25,method="gam") 

summary(dat_2021_dry)


#Quick modeling



mod_by_2021_dry_gauss <- bam(mean ~ te(DOY,by=Trt,bs="tp",k=50) +
                               te(DOY,by=Block,bs="tp",k=50),
                             select=TRUE,data=dat_2021_dry,family=Gamma())


mod_fs_2021_dry_gauss <- bam(mean ~ te(DOY,Trt,bs="fs",m=1,k=50) +
                               te(DOY,Block,bs="fs",m=1,k=50),
                             select=TRUE,data=dat_2021_dry,family=Gamma())

summary(mod_by_2021_dry_gauss)
summary(mod_fs_2021_dry_gauss)

k.check(mod_by_2021_dry_gauss)

AIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC)

BIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss) %>%
  mutate(deltaBIC = BIC - min(BIC)) %>%
  arrange(deltaBIC)

resids <- simulateResiduals(mod_fs_2021_dry_gauss)
plot(resids)
