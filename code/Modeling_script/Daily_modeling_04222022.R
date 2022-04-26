#########
# Averaging data
#  to daily
#  and filtering out just for drylands
#  2022
#########

rm(list=ls())

#main modeling and diagnostic packages
library(mgcv)
library(glmmTMB)
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
library(MetBrewer)
library(emmeans)
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
  summarize(std = stderr(TWS), mean = mean(TWS)) %>%
  mutate(block_trt = paste0(Block,Trt))

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



mod_by_2021_dry_gauss <- bam(mean ~ te(DOY,by=Trt,bs="tp",k=100) +
                               te(DOY,by=Block,bs="tp",k=100),
                             select=TRUE,data=dat_2021_dry,family=scat(),
                             discrete=TRUE,nthreads = 23)


#playing with autocorrelation
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

m0 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=50) +
                                       te(DOY,Block,bs="fs",m=2,k=50),data=dat_2021_dry)


m1 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=50) +
                               te(DOY,Block,bs="fs",m=2,k=50),
                               data=dat_2021_dry, correlation = corCAR1(value = 0.6,form = ~ DOY|block_trt),
                               control = ctrl)


k.check(m1$gam)

m2 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
             te(DOY,Block,bs="fs",m=2,k=25),
           data=dat_2021_dry, correlation = corARMA(p = 2,q=1,form = ~ DOY|block_trt),
           control = ctrl)

m2 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
                                te(DOY,Block,bs="fs",m=2,k=25),
                              data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 2),
                              control = ctrl)

m3 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
                                    te(DOY,Block,bs="fs",m=2,k=25),
                                  data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 3),
                                  control = ctrl)

m4 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
                                    te(DOY,Block,bs="fs",m=2,k=25),
                                  data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 4),
                                  control = ctrl)

m5 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
                                    te(DOY,Block,bs="fs",m=2,k=25),
                                  data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 5),
                                  control = ctrl)

m6 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
             te(DOY,Block,bs="fs",m=2,k=25),
           data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 6),
           control = ctrl)

m7 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
             te(DOY,Block,bs="fs",m=2,k=25),
           data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 7),
           control = ctrl)

m7 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
             te(DOY,Block,bs="fs",m=2,k=25),
           data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 7),
           control = ctrl)


m_est <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=25) +
             te(DOY,Block,bs="fs",m=2,k=25),
           data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY),
           control = ctrl)


layout(matrix(1:2, ncol = 2))
acf(resid(m0$lme, type = "normalized"))
pacf(resid(m0$lme, type = "normalized"))
layout(1)

acf(resid(m1$lme, type = "normalized"))
pacf(resid(m1$lme, type = "normalized"))

acf(resid(m2$lme, type = "normalized"))
pacf(resid(m2$lme, type = "normalized"))

acf(resid(m3$lme, type = "normalized"))
pacf(resid(m3$lme, type = "normalized"))

acf(resid(m4$lme, type = "normalized"))
pacf(resid(m4$lme, type = "normalized"))

acf(resid(m5$lme, type = "normalized"))
pacf(resid(m5$lme, type = "normalized"))

acf(resid(m6$lme, type = "normalized"))
pacf(resid(m6$lme, type = "normalized"))

acf(resid(m7$lme, type = "normalized"))
pacf(resid(m7$lme, type = "normalized"))
layout(1)

anova(m0$lme, m1$lme, m2$lme, m3$lme,m4$lme,m5$lme,m6$lme,m7$lme)



?bam
summary(mod_by_2021_dry_gauss)
summary(m3$gam)

k.check(mod_fs_2021_dry_gauss)

AIC(m0$lme,m1$lme,m2$lme,m3$lme,m4$lme,m5$lme,m6$lme,m7$lme) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC)

BIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss) %>%
  mutate(deltaBIC = BIC - min(BIC)) %>%
  arrange(deltaBIC)

resids <- simulateResiduals(m1$lme)
plot(m2$lme)
(m3$gam)






m0_est <- gratia::smooth_estimates(m0) %>%
  mutate(mod = "m0",est = coef(m0$gam)[1] + est)
m1_est <- gratia::smooth_estimates(m1)%>%
  mutate(mod = "m1",est = coef(m1$gam)[1] + est)
m2_est <- gratia::smooth_estimates(m2)%>%
  mutate(mod = "m2",est = coef(m2$gam)[1] + est)
m3_est <- gratia::smooth_estimates(m3)%>%
  mutate(mod = "m3",est = coef(m3$gam)[1] + est)
m4_est <- gratia::smooth_estimates(m4)%>%
  mutate(mod = "m4",est = coef(m4$gam)[1] + est)
m5_est <- gratia::smooth_estimates(m5)%>%
  mutate(mod = "m5",est = coef(m5$gam)[1] + est)
m6_est <- gratia::smooth_estimates(m6)%>%
  mutate(mod = "m6",est = coef(m6$gam)[1] + est)
m7_est <- gratia::smooth_estimates(m7)%>%
  mutate(mod = "m7",est = coef(m7$gam)[1] + est)

results <- m0_est %>% rbind(m1_est) %>%#,m2_est,m3_est,m4_est,m5_est,m6_est,m7_est) %>%
  filter(smooth == "te(DOY,Trt)") %>%
  select(est,se,DOY,Trt,mod)


date_trt <- results %>% 
  filter(mod %in% c("m0","m1")) %>%
  ggplot(aes(x=DOY,group=mod)) + #geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=mod),alpha=.1) +
  geom_line(aes(y = est, color=mod),size=1.1) + theme_pubr() +
  #geom_point(data=lm_dat,aes(x=DOY,y=mean,color=Trt)) +
  #geom_smooth(data=lm_dat,aes(x=DOY,y=mean,color=Trt)) +
  ggtitle("date by treatment smooth") + xlab("Date") +
  facet_wrap(~Trt) +
  scale_color_met_d(name="Demuth") +
  scale_fill_met_d(name="Demuth")
date_trt


results <- m0_est %>% rbind(m1_est,m2_est,m3_est,m4_est,m5_est,m6_est,m7_est) %>%
  filter(smooth == "te(DOY,Block)") %>%
  select(est,se,DOY,Block,mod)

date_block <- results %>% 
  filter(mod %in% c("m0","m2")) %>%
  ggplot(aes(x=DOY,group=mod)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=mod),alpha=.1) +
  geom_line(aes(y = est, color=mod),size=1.1) + theme_pubr() + 
  ggtitle("date by block smooth")+ xlab("Date") +
  facet_wrap(~Block) +
  scale_color_met_d(name="Demuth") +
  scale_fill_met_d(name="Demuth")
#date_block

date_trt + date_block


#Okay m2 seems to be the best of the evils here lets play around

m0_round2 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=100) +
             te(DOY,Block,bs="fs",m=2,k=100),data=dat_2021_dry)


m3_round2 <- gamm(mean ~ te(DOY,Trt,bs="fs",m=2,k=100) +
             te(DOY,Block,bs="fs",m=2,k=100),
           data=dat_2021_dry, correlation = corARMA(form = ~ 1|DOY, p = 3),
           control = ctrl)

summary(m0_round2$gam)
summary(m3_round2$gam)


m0_est_round2 <- gratia::smooth_estimates(m0_round2) %>%
  mutate(mod = "m0",est = coef(m0_round2$gam)[1] + est)

m3_est_round2 <- gratia::smooth_estimates(m3_round2)%>%
  mutate(mod = "m3",est = coef(m2_round2$gam)[1] + est)

results <- m0_est_round2 %>% rbind(m2_est_round2,m3_est_round2) %>%
  filter(smooth == "te(DOY,Trt)") %>%
  select(est,se,DOY,Trt,mod)


date_trt <- results %>% 
  filter(mod %in% c("m0","m2","m3")) %>%
  ggplot(aes(x=DOY,group=mod)) + #geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=mod),alpha=.1) +
  geom_line(aes(y = est, color=mod),size=1.1) + theme_pubr() +
  ggtitle("date by treatment smooth") + xlab("Date") +
  facet_wrap(~Trt+mod,scales="free") +
  scale_color_met_d(name="Demuth") +
  scale_fill_met_d(name="Demuth")
date_trt


results <- m0_est_round2 %>% rbind(m2_est_round2) %>%
  filter(smooth == "te(DOY,Block)") %>%
  select(est,se,DOY,Block,mod)

date_block <- results %>% 
  filter(mod %in% c("m0","m2")) %>%
  ggplot(aes(x=DOY,group=mod)) + geom_ribbon(aes(ymin=est-se, ymax=est+se,fill=mod),alpha=.1) +
  geom_line(aes(y = est, color=mod),size=1.1) + theme_pubr() + 
  ggtitle("date by block smooth")+ xlab("Date") +
  facet_wrap(~Block) +
  scale_color_met_d(name="Demuth") +
  scale_fill_met_d(name="Demuth")
#date_block

date_trt + date_block

#okay not sure about that, lets look at the linear shittt

######https://bodowinter.com/tutorial/bw_LME_tutorial2.pdf

test <- results %>% 
  filter(mod %in% c("m0","m2"))

         
         
         
lm_dat %>%
  group_by(DOY,Trt) %>%
  summarize(mean = mean(mean), std=mean(std)) %>%
  ggplot(aes(x=DOY,y=mean,color=Trt)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=mean - std, ymax = mean + std),size=2) +
  geom_line()



lm_dat <- dat_2021_dry %>%
  mutate(doy_fact = factor(DOY))

test <- lm_dat %>%
  group_by(Block,doy_fact) %>%
  summarize(count = length(doy_fact)) %>%
  pivot_wider(names_from = "Block", values_from = count)

lm_dat$numfactor <- numFactor(lm_dat$DOY)

parseNumLevels(levels(numFactor(lm_dat$DOY)))

levels(lm_dat$doy_fact)
?glmmTMB
m1 <- glmmTMB(mean ~ Trt + DOY + Trt*DOY + (1|Block), data=dat_2021_dry,
              control = glmmTMBControl(parallel = 10))

m2 <- glmmTMB(mean ~ Trt + DOY + Trt*DOY + (1|Block) + corCAR1(value=0.6,form=~DOY|block_trt), data=lm_dat,
              control = glmmTMBControl(parallel = 10))

m3 <- glmmTMB(mean ~ Trt + DOY + Trt*DOY + (1|Block) + toep(numfactor + 0 | Block), data=lm_dat, dispformula=~0)

m4 <- glmmTMB(mean ~ Trt + DOY + Trt*DOY + (1|Block) + ou(numfactor + 0 | Block),data=lm_dat,family=tweedie(),
              control = glmmTMBControl(parallel = 10))

AIC(m1,m2) %>%
  mutate(delta = AIC - min(AIC)) %>%
  arrange(delta)

BIC(m1,m2) %>%
  mutate(delta = BIC - min(BIC)) %>%
  arrange(delta)


plot(m2)

data <- multcomp::cld(emmeans(m2, "Trt")) %>% as_tibble()

data %>%
  ggplot(aes(x=Trt,y=emmean)) +
  geom_point() +
  geom_errorbar(aes(ymin=emmean - SE,ymax = emmean +SE))



summary(m1)
summary(m2)



TukeyHSD(m2)

performance::check_model(m1)
resid <- DHARMa::simulateResiduals(m1)
plot(resid)

acf(resid(m1))
acf(resid(m2))
acf(resid(m4))

pacf(resid(m1))
pacf(resid(m2))



##model attempts

library("nlme")
library("lme4")

m1 <- lmer(mean ~ Trt + DOY + Trt*DOY + (1|Block), data=dat_2021_dry) #structure if random intercepts

m2 <- lme(mean ~ Trt|DOY, data=dat_2021_dry, random = ~ 1|Block, correlation = corAR1(form = 1|Block), method="REML")

m2b <- lme(mean ~ Trt|DOY, data=dat_2021_dry, random = ~ 1|Block, correlation = corAR1(), method="REML")

m3 <- lme(mean ~ Trt + DOY + DOY*Trt, data=dat_2021_dry, random = ~ DOY|Block)



citation("lme4")



