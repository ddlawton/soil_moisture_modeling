library(tidyverse)
library(data.table)
library(lubridate)

rm(list=ls())

dat <- as_tibble(fread("data/processed/combined_Dec282021.csv")) %>% mutate(
  Site = factor(Site), Trt = factor(Trt), Block = factor(Block), Boot = factor(Boot), Year = factor(Year)) %>%
  select(TWS,Date.x,Trt,Block,Site,Year) %>% drop_na(TWS)

dat2 <- dat %>% group_by(Site,Block) %>%
  pivot_wider(names_from = Trt,values_from = TWS,values_fn = mean) %>%
  mutate(TWS_diff = C - NC) %>% drop_na(C,NC)

dat2 %>% filter(Year=="2020" & Site == "irrigated") %>%
  ggplot(aes(x=Date.x,y=TWS_diff,color = Block)) + geom_smooth() + geom_hline(yintercept = 0) + ggpubr::theme_pubr()



dat %>% filter(Site == "irrigated") %>% filter(Block == "1") %>% 
  filter(between(Date.x,parse_date_time("2021-05-19 12:30:00",orders="Ymd HMS"),
                 parse_date_time("2021-05-19 13:50:00",orders="Ymd HMS")) 

