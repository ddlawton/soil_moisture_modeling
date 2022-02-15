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

dat_2020 <- dat %>% filter(Year == "2020") %>% filter(Block != 2)
dat_2021_irr <- dat %>% filter(Year == "2021" & Site == "irrigated")
dat_2021_dry <- dat %>% filter(Year == "2021" & Site == "dryland")

# 2020 year modeling -- Irrigated
#####




seasonal <- ggplot(dat_2020,aes(x=Date,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  theme_pubr() + coord_cartesian(ylim=c(0,NA))

MOD <- ggplot(dat_2020,aes(x=MOD,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 50)) +
  theme_pubr() 

block_trt <- ggplot(dat_2020,aes(x=Date,y=TWS)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  theme_pubr() + coord_cartesian(ylim=c(0,NA)) + facet_grid(Trt~Block) + geom_rug(sides="b")





mod_by_2020_irr_gauss <- bam(TWS ~ te(Date_numeric,by=Trt,bs="tp",k=500) +
                               te(Date_numeric,by=Block,bs="tp",k=250)+
                               te(MOD,by=Trt,bs="tp",k=100) +
                               te(MOD,by=Block,bs="tp",k=100),
                             select=TRUE,data=dat_2020,discrete = TRUE,nthreads = 23)


mod_fs_2020_irr_gauss <- bam(TWS ~ te(Date_numeric,Trt,bs="fs",m=1,k=500) +
                               te(Date_numeric,Block,bs="fs",m=1,k=250)+
                               te(MOD,Trt,bs="fs",k=100) +
                               te(MOD,Block,bs="fs",k=100),
                             select=TRUE,data=dat_2020,discrete = TRUE,nthreads = 23)




AIC(mod_by_2020_irr_gauss,mod_fs_2020_irr_gauss)
BIC(mod_by_2020_irr_gauss,mod_fs_2020_irr_gauss)


summary(mod_fs_2020_irr_gauss)
k.check(mod_fs_2020_irr_gauss)
concurvity(mod_fs_2020_irr_gauss)


simresid <- simulateResiduals(mod_fs_2020_dry_gauss)
plot(simresid)


draw(mod_fs_2020_irr_gauss)

b0 <- coef(mod_fs_2020_irr_gauss)[1]

test <- gratia::smooth_estimates(mod_fs_2020_irr_gauss)

test$adj_est <- test$est + b0

date_trt <- test %>% filter(smooth == "te(Date_numeric,Trt)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("date by treatment smooth") + xlab("Date")

date_block <- test %>% filter(smooth == "te(Date_numeric,Block)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr() +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("date by block smooth")+ xlab("Date")

mod_trt <- test %>% filter(smooth == "te(MOD,Trt)") %>%
  ggplot(aes(x=(MOD/60),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr()  +
  scale_color_manual(values = c("#1b9e77","#d95f02"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("Minute of day by treatment smooth") + xlab("Hour of Day") +
  xlim(0,24)


mod_block <- test %>% filter(smooth == "te(MOD,Block)") %>%
  ggplot(aes(x=(MOD/60),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr()  +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("Minute of day by block smooth") + xlab("Hour of Day")

irrigated_2020 <- date_trt/date_block/(mod_trt+mod_block)

ggsave(irrigated_2020,file="output/figures/global_smooth_irrigated_2020/all_plots_together.png",width=10,height=20,unit="in",dpi=600)
ggsave(date_trt,file="output/figures/global_smooth_irrigated_2020/date_treatment_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(date_block,file="output/figures/global_smooth_irrigated_2020/date_block_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(mod_trt,file="output/figures/global_smooth_irrigated_2020/mod_trt_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(mod_block,file="output/figures/global_smooth_irrigated_2020/mod_block_smooth.png",width=10,height=5,unit="in",dpi=600)


#####


# 2021 year modeling -- Irrigated
#####


seasonal <- ggplot(dat_2021_irr,aes(x=Date,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  theme_pubr() + coord_cartesian(ylim=c(0,NA))

MOD <- ggplot(dat_2021_irr,aes(x=MOD,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 50)) +
  theme_pubr() 

block_trt <- ggplot(dat_2021_irr,aes(x=Date,y=TWS)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  theme_pubr() + coord_cartesian(ylim=c(0,NA)) + facet_grid(Trt~Block) + geom_rug(sides="b")



mod_by_2021_irr_gauss <- bam(TWS ~ te(Date_numeric,by=Trt,bs="tp",k=500) +
                               te(Date_numeric,by=Block,bs="tp",k=250)+
                               te(MOD,by=Trt,bs="tp",k=100) +
                               te(MOD,by=Block,bs="tp",k=100),
                             select=TRUE,data=dat_2021_irr,discrete = TRUE,nthreads = 23)


mod_fs_2021_irr_gauss <- bam(TWS ~ te(Date_numeric,Trt,bs="fs",m=1,k=500) +
                               te(Date_numeric,Block,bs="fs",m=1,k=250)+
                               te(MOD,Trt,bs="fs",k=100) +
                               te(MOD,Block,bs="fs",k=100),
                             select=TRUE,data=dat_2021_irr,discrete = TRUE,nthreads = 23)




AIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss)
BIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss)


summary(mod_fs_2021_irr_gauss_blocktrt)
k.check(mod_fs_2021_irr_gauss)

summary(mod_fs_2021_dry_gauss)


simresid <- simulateResiduals(mod_fs_2021_dry_gauss)
plot(simresid)


draw(mod_fs_2021_irr_gauss)

b0 <- coef(mod_fs_2021_irr_gauss)[1]

test <- gratia::smooth_estimates(mod_fs_2021_irr_gauss)

test$adj_est <- test$est + b0

date_trt <- test %>% filter(smooth == "te(Date_numeric,Trt)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("date by treatment smooth") + xlab("Date")

date_block <- test %>% filter(smooth == "te(Date_numeric,Block)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr() +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("date by block smooth")+ xlab("Date")

mod_trt <- test %>% filter(smooth == "te(MOD,Trt)") %>%
  ggplot(aes(x=(MOD/60),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr()  +
  scale_color_manual(values = c("#1b9e77","#d95f02"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("Minute of day by treatment smooth") + xlab("Hour of Day") +
  xlim(0,24)


mod_block <- test %>% filter(smooth == "te(MOD,Block)") %>%
  ggplot(aes(x=(MOD/60),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr()  +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("Minute of day by block smooth") + xlab("Hour of Day")

irrigated_2021 <- date_trt/date_block/(mod_trt+mod_block)

ggsave(irrigated_2021,file="output/figures/global_smooth_irrigated_2021/all_plots_together.png",width=10,height=20,unit="in",dpi=600)
ggsave(date_trt,file="output/figures/global_smooth_irrigated_2021/date_treatment_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(date_block,file="output/figures/global_smooth_irrigated_2021/date_block_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(mod_trt,file="output/figures/global_smooth_irrigated_2021/mod_trt_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(mod_block,file="output/figures/global_smooth_irrigated_2021/mod_block_smooth.png",width=10,height=5,unit="in",dpi=600)



#####



# 2021 year modeling -- Dryland
#####


seasonal <- ggplot(dat_2021_dry,aes(x=Date,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  theme_pubr() + coord_cartesian(ylim=c(0,NA))

MOD <- ggplot(dat_2021_dry,aes(x=MOD,y=TWS,color=Trt)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc", k = 50)) +
  theme_pubr() 

block_trt <- ggplot(dat_2021_dry,aes(x=Date,y=TWS)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp", k = 50)) +
  theme_pubr() + coord_cartesian(ylim=c(0,NA)) + facet_grid(Trt~Block) + geom_rug(sides="b")




mod_by_2021_dry_gauss <- bam(TWS ~ te(Date_numeric,by=Trt,bs="tp",k=500) +
                               te(Date_numeric,by=Block,bs="tp",k=250)+
                               te(MOD,by=Trt,bs="tp",k=100) +
                               te(MOD,by=Block,bs="tp",k=100),
                             select=TRUE,data=dat_2021_dry,discrete = TRUE,nthreads = 23)


mod_fs_2021_dry_gauss <- bam(TWS ~ te(Date_numeric,Trt,bs="fs",m=1,k=500) +
                               te(Date_numeric,Block,bs="fs",m=1,k=250)+
                               te(MOD,Trt,bs="fs",k=100) +
                               te(MOD,Block,bs="fs",k=100),
                             select=TRUE,data=dat_2021_dry,discrete = TRUE,nthreads = 23)




AIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss)
BIC(mod_by_2021_dry_gauss,mod_fs_2021_dry_gauss)


summary(mod_fs_2021_irr_gauss_blocktrt)
k.check(mod_fs_2021_irr_gauss)

summary(mod_fs_2021_dry_gauss)


simresid <- simulateResiduals(mod_fs_2021_dry_gauss)
plot(simresid)


draw(mod_fs_2021_irr_gauss)

b0 <- coef(mod_fs_2021_dry_gauss)[1]

test <- gratia::smooth_estimates(mod_fs_2021_dry_gauss)

test$adj_est <- test$est + b0

date_trt <- test %>% filter(smooth == "te(Date_numeric,Trt)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("date by treatment smooth") + xlab("Date")

date_block <- test %>% filter(smooth == "te(Date_numeric,Block)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr() +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("date by block smooth")+ xlab("Date")

mod_trt <- test %>% filter(smooth == "te(MOD,Trt)") %>%
  ggplot(aes(x=(MOD/60),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr()  +
  scale_color_manual(values = c("#1b9e77","#d95f02"))+
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("Minute of day by treatment smooth") + xlab("Hour of Day") +
  xlim(0,24)


mod_block <- test %>% filter(smooth == "te(MOD,Block)") %>%
  ggplot(aes(x=(MOD/60),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr()  +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("Minute of day by block smooth") + xlab("Hour of Day")

dryland_2021 <- date_trt/date_block/(mod_trt+mod_block)


ggsave(dryland_2021,file="output/figures/global_smooth_dryland_2021/all_plots_together.png",width=10,height=20,unit="in",dpi=600)
ggsave(date_trt,file="output/figures/global_smooth_dryland_2021/date_treatment_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(date_block,file="output/figures/global_smooth_dryland_2021/date_block_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(mod_trt,file="output/figures/global_smooth_dryland_2021/mod_trt_smooth.png",width=10,height=5,unit="in",dpi=600)
ggsave(mod_block,file="output/figures/global_smooth_dryland_2021/mod_block_smooth.png",width=10,height=5,unit="in",dpi=600)





summary(mod_fs_2021_irr_gauss)
summary(mod_fs_2021_irr)

AIC(mod_2021_irr,mod_fs_2021_Ga)

draw(mod_fs_2021_irr_gauss)
plot(mod_fs_2021_irr_scat)


simresid <- simulateResiduals(mod_fs_2021_irr_gauss)
plot(simresid)

res = recalculateResiduals(simresid, group = dat_2021_dry$Date_numeric)
testTemporalAutocorrelation(res, time = unique(dat_2021_dry$Date_numeric))

#####