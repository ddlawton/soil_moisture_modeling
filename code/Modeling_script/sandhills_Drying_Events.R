########################
# Sandhills Soil Moisture
#  Drying event modeling
#   January 14, 2021
#    Douglas Lawton
#######################
rm(list=ls())


library(tidyverse) # for all your data management and visualizing needs
library(mgcv) # the most popular GAM package
library(gratia) # just some nice model diagnostics and results plotting 
library(lubridate) # To help manage the date columns
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(viridis) # some nice colors
library(data.table) # for the command 'fread' which is a faster version of read.csv
library(foreach)
library(DHARMa)

dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% mutate(
  Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year)) 


dryevents <- as_tibble(fread("data/raw/Drying_events.csv")) %>%
  mutate(Study = factor(Study), Year = factor(Year),ndays= difftime(End,`Start (day of event)`)) %>%
  rename(Start_date = `Start (day of event)`) %>% rowid_to_column(var="event")


event1 <- dat %>% filter(
  between(Date.x,"2020-05-29","2020-06-02")) %>%
  mutate(event = "2020-05-29 : 2020-06-02") %>%
  filter(Block != 2) %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))


event2 <- dat %>% filter(
  between(Date.x,"2020-06-05","2020-06-07")) %>%
  mutate(event = "2020-06-05 : 2020-06-07") %>%
  filter(Block != 2) %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event3 <- dat %>% filter(
  between(Date.x,"2020-06-05","2020-06-07")) %>%
  mutate(event = "2020-06-30 : 2020-07-04") %>%
  filter(Block != 2) %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event4 <- dat %>% filter(
  between(Date.x,"2020-07-29","2020-08-02")) %>%
  mutate(event = "2020-07-29 : 2020-08-02") %>%
  filter(Block != 2) %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Post")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event5 <- dat %>% filter(
  between(Date.x,"2020-09-02","2020-09-07")) %>%
  mutate(event = "2020-09-02 : 2020-09-07") %>%
  filter(Block != 2) %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Post")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event6 <- dat %>% filter(
  between(Date.x,"2021-05-23","2021-06-02")) %>%
  mutate(event = "2021-05-23 : 2021-06-02") %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event7 <- dat %>% filter(
  between(Date.x,"2021-06-11","2021-06-16")) %>%
  mutate(event = "2021-06-11 : 2021-06-16") %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event8 <- dat %>% filter(
  between(Date.x,"2021-06-23","2021-06-27")) %>%
  mutate(event = "2021-06-23 : 2021-06-27") %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event9 <- dat %>% filter(
  between(Date.x,"2021-08-20","2021-08-27")) %>%
  mutate(event = "2021-08-20 : 2021-08-27") %>%
  filter(Site == "irrigated")%>%
  filter(Boot == "Post")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event10 <- dat %>% filter(
  between(Date.x,"2021-06-11","2021-06-19")) %>%
  mutate(event = "2021-06-11 : 2021-06-19") %>%
  filter(Site == "dryland")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event11 <- dat %>% filter(
  between(Date.x,"2021-06-23","2021-07-01")) %>%
  mutate(event = "2021-06-23 : 2021-07-01") %>%
  filter(Site == "dryland")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event12 <- dat %>% filter(
  between(Date.x,"2021-07-04","2021-07-06")) %>%
  mutate(event = "2021-07-04 : 2021-07-06") %>%
  filter(Site == "dryland")%>%
  filter(Boot == "Pre")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event13 <- dat %>% filter(
  between(Date.x,"2021-08-11","2021-08-13")) %>%
  mutate(event = "2021-08-11 : 2021-07-13") %>%
  filter(Site == "dryland")%>%
  filter(Boot == "Post")  %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))

event14 <- dat %>% filter(
  between(Date.x,"2021-08-20","2021-08-27")) %>%
  mutate(event = "2021-08-20 : 2021-08-27") %>%
  filter(Site == "dryland")%>%
  filter(Boot == "Post") %>% mutate(days_after = as.numeric(difftime(Date.x,min(Date.x),units="days")))


events <- rbind(event1,event2,event3,event4,event5,event6,event7,event8,event9,event10,event11,event12,event13,event14) %>%
  mutate(event=factor(event)) %>% drop_na(TWS) %>% filter(days_after <=5) %>%
  filter(event != "2020-05-29 : 2020-06-02")


ggplot((events ),aes(x=days_after,y=X15,color=event,linetype=Trt)) + geom_smooth() + facet_wrap(~Site)


X5 <- ggplot(events,aes(days_after,y=X5))  +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50),aes(color=Boot,linetype=Trt)) + 
  facet_wrap(~Site + event,scales="free") +
  theme_pubr() + coord_cartesian(ylim=c(0,NA)) + ggtitle("5 cm")

X15 <- ggplot(events,aes(days_after,y=X15))  +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50),aes(color=Boot,linetype=Trt)) + 
  facet_wrap(~Site + event,scales="free") +
  theme_pubr() + coord_cartesian(ylim=c(0,NA))  + ggtitle("15 cm")


X30 <- ggplot(events,aes(days_after,y=X30))  +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50),aes(color=Boot,linetype=Trt)) + 
  facet_wrap(~Site + event,scales="free") +
  theme_pubr() + coord_cartesian(ylim=c(0,NA)) + ggtitle("30 cm")

TWS <- ggplot(events,aes(days_after,y=TWS))  +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50),aes(color=Boot,linetype=Trt)) + 
  facet_wrap(~Site + event,scales="free") +
  theme_pubr() + coord_cartesian(ylim=c(0,NA)) + ggtitle("TWS")


ggsave(X5,file="output/figures/drying_events/X5.png",width=10,height=10,units='in',dpi=300)
ggsave(X15,file="output/figures/drying_events/X15.png",width=10,height=10,units='in',dpi=300)
ggsave(X30,file="output/figures/drying_events/X30.png",width=10,height=10,units='in',dpi=300)
ggsave(TWS,file="output/figures/drying_events/TWS.png",width=10,height=10,units='in',dpi=300)





X5_mod <- bam(
  X5 ~ te(days_after,Site,Trt,bs=c("tp","re","re"),k=20) + s(Block,bs="re") + s(event,bs="re"), family=betar(),select=TRUE,
  data=events,discrete = TRUE,nthreads = 10)

X15_mod <- bam(
  X15 ~ te(days_after,Site,Trt,bs=c("tp","re","re"),k=20) + s(Block,bs="re") + s(event,bs="re"), family=betar(),select=TRUE,
  data=events,discrete = TRUE,nthreads = 10)

X30_mod <- bam(
  X30 ~ te(days_after,Site,Trt,bs=c("tp","re","re"),k=20) + s(Block,bs="re") + s(event,bs="re"), family=betar(),select=TRUE,
  data=events,discrete = TRUE,nthreads = 10)

TWS_mod <- bam(
  TWS ~ te(days_after,Site,Trt,bs=c("tp","re","re"),k=20) + s(Block,bs="re") + s(event,bs="re"),select=TRUE,
  data=events,discrete = TRUE,nthreads = 10)


summary(X5_mod)
summary(X15_mod)
summary(X30_mod)
summary(TWS_mod)




resid5 <- simulateResiduals(X5_mod)
plot(resid5)


resid15 <- simulateResiduals(X15_mod)
plot(resid15)

resid30 <- simulateResiduals(X30_mod)
plot(resid30)

residTWS <- simulateResiduals(TWS_mod)
plot(residTWS)



events2 <- add_partial_residuals(data=events,model=X5_mod)
events3 <- add_partial_residuals(data=events,model=X15_mod)
events4 <- add_partial_residuals(data=events,model=X30_mod)
events5 <- add_partial_residuals(data=events,model=TWS_mod)


x5_model_plots <- ggplot(events2,aes(x=days_after,y=`te(days_after,Site,Trt)`,color=Trt)) + geom_smooth() + facet_wrap(~Site) + ggpubr::theme_pubr() + 
  xlab("Days after rain event") +
  ylab("Soil Moisture 5 cm") + 
  scale_color_manual(values=c("#1b9e77","#d95f02"))

x15_model_plots <-  ggplot(events3,aes(x=days_after,y=`te(days_after,Site,Trt)`,color=Trt)) + geom_smooth() + facet_wrap(~Site) + ggpubr::theme_pubr() +
  xlab("Days after rain event")+
  ylab("Soil Moisture 15 cm")  + 
  scale_color_manual(values=c("#1b9e77","#d95f02"))

x30_model_plots <- ggplot(events4,aes(x=days_after,y=`te(days_after,Site,Trt)`,color=Trt)) + geom_smooth() + facet_wrap(~Site) + ggpubr::theme_pubr() + 
  xlab("Days after rain event")+
  ylab("Soil Moisture 30 cm")  + 
  scale_color_manual(values=c("#1b9e77","#d95f02"))

TWS_model_plots <- ggplot(events5,aes(x=days_after,y=`te(days_after,Site,Trt)`,color=Trt)) + geom_smooth() + facet_wrap(~Site) + ggpubr::theme_pubr() + 
  xlab("Days after rain event") +
  ylab("Total Water Storage")  + 
  scale_color_manual(values=c("#1b9e77","#d95f02"))



ggsave(x5_model_plots,file="output/figures/drying_events/x5_modeled_results.png",width=10,height=10,units='in',dpi=300)
ggsave(x15_model_plots,file="output/figures/drying_events/x15_modeled_results.png",width=10,height=10,units='in',dpi=300)
ggsave(x30_model_plots,file="output/figures/drying_events/x30_modeled_results.png",width=10,height=10,units='in',dpi=300)
ggsave(TWS_model_plots,file="output/figures/drying_events/TWS_modeled_results.png",width=10,height=10,units='in',dpi=300)





