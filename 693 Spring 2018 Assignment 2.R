# load libraries
library(tidyverse)
library(nycflights13)


# dataset for assignment

flights = flights


# cancelled flights per day

cancelled = flights %>% 
  group_by(month,day) %>% 
  summarise(cancel=sum(is.na(air_time))/length(air_time))


# scatterplot of cancelled flights per day

ggplot(cancelled,aes(x=day,y=cancel,color=month)) +
  geom_point()


# average delay to look for relationship with cancelled flights

delays = flights %>% 
  group_by(month,day) %>% 
  summarise(delay=mean(dep_delay,na.rm=T))


# anova to determine if cancellations is related to delays

anova(lm(cancelled$cancel~delays$delay))


# look at carriers with worst delays

flights %>% 
  group_by(carrier) %>% 
  summarise(delay=mean(dep_delay,na.rm=T)) %>% 
  arrange(desc(delay))


# disentanglement=engage

flights %>% 
  group_by(carrier,dest) %>% 
  summarise(n())


# number of flights before first delay of greater than 1 hour




# plane with worst on-time record (on average)

flights %>% 
  group_by(tailnum) %>% 
  summarise(delay=mean(dep_delay,na.rm=T)) %>% 
  arrange(desc(delay))


# time of day to avoid delays (by hour)

flights %>% 
  group_by(hour) %>% 
  summarise(delay=mean(dep_delay,na.rm=T)) %>% 
  arrange(delay)


# total minutes of delay for each destination

flights %>% 
  group_by(dest) %>% 
  summarise(delay=sum(dep_delay,na.rm=T))


# proportion of total delay for each flight



# suspiciously fast flights

flights %>% 
  group_by(origin,dest) %>% 
  mutate(min_atime=min(air_time)) %>% 
  mutate(fast=air_time/min_atime) %>% 
  select(carrier,flight,origin,dest,distance,air_time,fast) %>% 
  arrange(fast)


# most delayed flights in the air

flights %>% 
  mutate(air_delay=arr_delay-dep_delay) %>% 
  select(carrier,flight,origin,dest,air_delay) %>% 
  arrange(desc(air_delay))






