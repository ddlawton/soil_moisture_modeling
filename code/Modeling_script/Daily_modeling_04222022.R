#########
# Averaging data
#  to daily means
#  and filtering out just for drylands
#  2022
#########

rm(list=ls())

#main modeling and diagnostic packages
library(mgcv)
library(gratia)
library(lme4)
library(performance)

#Supporting packages

library(tidyverse) # for all your data management and visualizing needs
library(lubridate) # To help manage the date columns
library(patchwork) # another ggplot helper package. allows to combined plots together
library(ggpubr) # just some pleasing themes for ggplot
library(data.table) # for the command 'fread' which is a faster version of read.csv
library(ggridges) #density plots
library(MetBrewer) # some fun colors inspired by paintings in the met
library(emmeans) #estimated marginal means

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



#GAMM modeling with a Continuous AR(1) Correlation Structure
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")

m0 <- gamm(mean ~ 
             te(DOY,Trt,bs="fs",m=2,k=50) +
             te(DOY,Block,bs="fs",m=2,k=50), select=TRUE,
           data=dat_2021_dry,
           control = ctrl)
acf(resid(m0$lme,type='normalized')) 
pacf(resid(m0$lme,type='normalized'))


m1 <- gamm(mean ~ 
             te(DOY,Trt,bs= c('tp',"re"),m=2,k=50) +
             te(DOY,Block,bs= c('tp',"re"),m=2,k=50), select=TRUE,
           data=dat_2021_dry, correlation = corCAR1(value = 0.2,form = ~ DOY|block_trt),
           control = ctrl)


acf(resid(m1$lme,type='normalized'))
pacf(resid(m1$lme,type='normalized'))

m2 <- gamm(mean ~ 
             te(DOY,Trt,bs="fs",m=2,k=50) +
             te(DOY,Block,bs="fs",m=2,k=50),
           data=dat_2021_dry, correlation = corCAR1(value = 0.4,form = ~ DOY|block_trt), select=TRUE,
           control = ctrl)
acf(resid(m2$lme,type='normalized')) 
pacf(resid(m2$lme,type='normalized'))

m3 <- gamm(mean ~ 
             te(DOY,Trt,bs= c('gp',"re"),m=2,k=50) +
             te(DOY,Block,bs="fs",m=2,k=50),
           data=dat_2021_dry, correlation = corCAR1(value = 0.6,form = ~ DOY|block_trt),
           control = ctrl)
acf(resid(m3$lme,type='normalized'))
pacf(resid(m3$lme,type='normalized'))



#alright there is still SOME temporal autocorrelation issues...but certainly better than not adding the correlation structure

anova(m3$lme,m0$lme, m1$lme, m2$lme)

AIC(m3$lme,m0$lme, m1$lme, m2$lme) %>%
  mutate(delta = AIC - min(AIC)) %>%
  arrange(delta)

BIC(m3$lme,m0$lme, m1$lme, m2$lme) %>%
  mutate(delta = BIC - min(BIC)) %>%
  arrange(delta)

appraise(m2$gam)
summary(m2$gam)
k.check(m2$gam) #Making sure that there are enough knots to capture all the wiggles (yes that the official term)


#Now plotting to see results


results_m0 <- gratia::smooth_estimates(m0)

results_m0$adj_est <- results_m0$est + coef(m0$gam)[1] #This is how I adjust to TWS units


results_m2 <- gratia::smooth_estimates(m2)

results_m2$adj_est <- results_m2$est + coef(m2$gam)[1] #This is how I adjust to TWS units

DOYxBlock_m0 <- results_m0 %>% # not sigificant in the model
  filter(smooth == "te(DOY,Block)") %>%
  ggplot(aes(x=DOY)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.2) +
  geom_line(aes(y = adj_est,color=Block),size=1) + theme_pubr() +
  ggtitle("Day of year by block smooth") + xlab("Day of year") +
  scale_color_met_d(name="Demuth") +
  scale_fill_met_d(name="Demuth")

DOYxTrt_m0 <- results_m0 %>%
  mutate(Trt = case_when(
    Trt == "C" ~ "Cover",
    Trt == "NC" ~ "Noncover")) %>%
  filter(smooth == "te(DOY,Trt)") %>%
  ggplot(aes(x=DOY,)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.2) +
  geom_line(aes(y = adj_est,color=Trt),size=1) + theme_pubr() +
  xlab("Day of year") +
  ylab("Modeled TWS") +
  scale_color_met_d(name="Veronese") +
  scale_fill_met_d(name="Veronese") +
  theme(legend.title=element_blank())


DOYxBlock_m1 <- results_m2 %>% # not sigificant in the model
  filter(smooth == "te(DOY,Block)") %>%
  ggplot(aes(x=DOY)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.2) +
  geom_line(aes(y = adj_est,color=Block),size=1) + theme_pubr() +
  ggtitle("Day of year by block smooth") + xlab("Day of year") +
  scale_color_met_d(name="Demuth") +
  scale_fill_met_d(name="Demuth")

DOYxTrt_m1 <- results_m2 %>%
  mutate(Trt = case_when(
    Trt == "C" ~ "Cover",
    Trt == "NC" ~ "Noncover")) %>%
  filter(smooth == "te(DOY,Trt)") %>%
  ggplot(aes(x=DOY,)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.2) +
  geom_line(aes(y = adj_est,color=Trt),size=1) + theme_pubr() +
  xlab("Day of year") +
  ylab("Modeled TWS") +
  scale_color_met_d(name="Veronese") +
  scale_fill_met_d(name="Veronese") +
  theme(legend.title=element_blank())




#comparing a model with the correlational structure and without

m0_results <- (DOYxBlock_m0) / (DOYxTrt_m0)

m2_results <- (DOYxBlock_m1) / (DOYxTrt_m1) # I nean the models dont change THAT much....but whatever


#Now the traditional approach

#making the basic line plot showing DOY by treatment

Trt<-dat_2021_dry %>%
  group_by(Trt,DOY) %>%
  summarize(se = stderr(mean),mean = mean(mean))
summary(Trt)


line_graph <- dat_2021_dry %>%
  mutate(Trt = case_when(
    Trt == "C" ~ "Cover",
    Trt == "NC" ~ "Noncover")) %>%
  group_by(Trt,DOY) %>%
  summarize(se = stderr(mean),mean = mean(mean)) %>%
  ggplot(aes(x=DOY)) +
  geom_point(aes(y=mean,color=Trt),size=0.9) +
  geom_linerange(aes(ymin=mean-se,ymax= mean+se,color=Trt),size=0.5,alpha=0.5)+
  geom_line(aes(y = mean,x=DOY,color=Trt),size=0.25) +
  scale_color_met_d(name="Veronese") +
  xlab("Day of year") +
  ylab("Averaged TWS") +
  ggpubr::theme_pubr() +
  theme(legend.position="none")


dat_2021_dry %>% #When all averaged together....there really isnt a significant difference. The time component is important!
  ggplot(aes(x=Trt,y=mean)) +
  geom_boxplot() +
  geom_jitter(pch=21,size=0.75) +
  ggpubr::theme_pubr() +
  ylab("TWS") +
  xlab("Treatment")


dat_2021_dry %>%   #It is pretty clear that there are certain DOYs where there is a separation of the two covers....
  mutate(DOY = factor(DOY)) %>%
  ggplot(aes(x=DOY,y=mean,color=Trt)) +
  geom_boxplot() +
  geom_jitter(pch=21,size=0.75) +
  ggpubr::theme_pubr() +
  ylab("TWS") +
  xlab("Day of Year")  +
  scale_color_met_d(name="Veronese") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))




dat_2021_dry <- dat_2021_dry %>%
  mutate(block_trt = factor(block_trt),
         DOY_fact = factor(DOY))

lme_simple_fit <- lme(mean~ Trt,random=~1 + DOY | block_trt,data=dat_2021_dry,correlation=corCAR1(value=0.2,form=~DOY|block_trt)) # This is the exact same correlational structure as in the GAMs

performance::check_model(lme_simple_fit) #It could definitely be better....but I think it gets the point across.


plot(lme_simple_fit)
acf(resid(lme_simple_fit,type="normalized"))
pacf(resid(lme_simple_fit,type="normalized"))

summary(lme_simple_fit)

MuMIn::r.squaredGLMM(lme_simple_fit) #just to get that R-square
summary(m1$gam)$r.sq

emeans <- emmeans(lme_simple_fit, pairwise ~ Trt)
emeans_df <- emeans$emmeans %>% as.data.frame()

LME_emmeans <- ggplot(emeans_df,aes(x=Trt,y=emmean)) + geom_point() +
  geom_errorbar(aes(ymin = emmean - SE,ymax=emmean+SE)) +
  scale_y_continuous(limits=c(0,NA)) +
  theme_pubr() + ylab("Modeled TWS")  + 
  scale_x_discrete(labels = c('Cover','Noncover')) +
  xlab("")

plots <- (LME_emmeans + plot_spacer()) / line_graph / DOYxTrt + 
  plot_annotation(tag_levels = 'A') +  theme(legend.position = "bottom")

ggsave(plots,file="output/Figure_1.png",height = 7.5,width=5,units="in",dpi=600)

