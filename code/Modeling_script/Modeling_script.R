########################
# Sandhills Soil Moisture
#  Data Modeling
#   December 15,2021
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



mod <- bam(TWS  ~
             s(DOY,by=Site,k=15) +
             s(DOY,by=Trt,k=15)+
             s(DOY,by=Year,k=15)+
             s(MOD,by=Site,k=15)+
             s(MOD,by=Trt,k=15)+
             s(MOD,by=Year,k=15),
select=TRUE,data=dat,discrete=TRUE,nthreads = 15)

summary(mod)
draw(mod)

mod2 <- bam(TWS  ~
             s(DOY,by=Site,k=15) +
             s(DOY,by=Trt,k=15)+
             s(DOY,by=Year,k=15)+
             s(MOD,by=Site,k=15)+
             s(MOD,by=Trt,k=15),
             s(MOD,by=Year,k=15),
           select=TRUE,data=dat,discrete=TRUE,nthreads = 15)

summary(mod2)
draw(mod2)
appraise(mod3)

drylands <- dat %>% filter(Site == "dryland") %>% mutate(date_numeric = as.numeric(Date)) %>% drop_na()


mod3 <- bam(Moisture   ~
              te(DOY,Depth,k=15,bs="fs") + te(MOD,Depth,k=15,bs='fs') + s(Block,bs="re"),
            select=TRUE,data=(drylands),discrete=TRUE,nthreads = 15,family=scat())


mod4 <- bam(Moisture   ~
              te(DOY,MOD,Depth,bs=c("tp","tp","re")),
            select=TRUE,data=(drylands),discrete=TRUE,nthreads = 15,family=scat())



summary(mod3)
draw(mod3)
appraise(mod3)
k.check(mod)

drylands2 <- add_partial_residuals(drylands, mod3) 

drylands2$pred <- predict(mod3,newdata=drylands2,type="response")


ggplot(drylands2,aes(x=DOY,y=`te(DOY,Depth)`,color=Depth)) + geom_smooth() + theme_pubr() 
ggplot(drylands2,aes(x=DOY,y=pred,color=Depth)) + geom_smooth() + theme_pubr()


summary(drylands2$Date)






mod3 <- bam(Moisture   ~
              te(DOY,Depth,k=15,bs="fs") + te(MOD,Depth,k=15,bs='fs') + s(Block,bs="re"),
            select=TRUE,data=(drylands),discrete=TRUE,nthreads = 15,family=scat())



pdat <- with(drylands, expand.grid(Block = levels(Block), 
                                    Depth = levels(Depth), 
                                    MOD = seq(min(MOD), max(MOD), length = 10),
                                    DOY = seq(min(DOY), max(DOY), length = 10))
)


pdat <- transform(pdat, pred = predict(mod3, newdata = pdat, type = "response"))

ggplot(pdat,aes(x=DOY,y=pred,color=Depth)) + geom_smooth(method="gam") + geom_point()












