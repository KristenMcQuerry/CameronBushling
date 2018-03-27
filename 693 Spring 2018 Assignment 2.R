# load libraries
library(tidyverse)
library(nycflights13)

# dataset for assignment
flights = flights

# cancelled flights per day
cancelled = flights %>% 
  group_by(month,day) %>% 
  summarise(cancel=sum(is.na(air_time)),c.prop=sum(is.na(air_time))/length(air_time),
            a.delay=mean(dep_delay,na.rm=T))

# scatterplot of cancelled flights per day
ggplot(cancelled,aes(x=day,y=cancel,color=month)) +
  geom_point()

# anova to determine if cancellations is related to delays
anova(lm(cancelled$c.prop~cancelled$a.delay))

## finished with number 3

# look at carriers with worst delays
flights %>% 
  group_by(carrier) %>% 
  summarise(a.delay=mean(dep_delay,na.rm=T)) %>% 
  arrange(desc(a.delay))

# number of flights before first delay of greater than 1 hour
flights %>% 
  arrange(tailnum,month,day) %>% 
  select(tailnum,month,day,dep_delay)

# plane with worst on-time record (on average)
flights %>% 
  group_by(tailnum) %>% 
  summarise(a.delay=mean(dep_delay,na.rm=T)) %>% 
  arrange(desc(a.delay))

## finished with number 6

# time of day to avoid delays (by hour)
flights %>% 
  group_by(hour) %>% 
  summarise(a.delay=mean(dep_delay,na.rm=T)) %>% 
  arrange(a.delay)

## finished with number 7

# total minutes of delay for each destination
flights %>% 
  group_by(dest) %>% 
  summarise(t.delay=sum(dep_delay,na.rm=T))

# proportion of total delay for each flight
flights %>% 
  group_by(dest) %>% 
  mutate(t.delay=sum(dep_delay,na.rm=T)) %>% 
  mutate(delay.prop=dep_delay/t.delay) %>% 
  select(dest,dep_delay,t.delay,delay.prop)

## finished with number 8

# suspiciously fast flights
flights %>% 
  group_by(origin,dest) %>% 
  mutate(avg_atime=mean(air_time,na.rm=T)) %>% 
  mutate(fast=air_time/avg_atime) %>% 
  select(carrier,flight,origin,dest,distance,air_time,avg_atime,fast) %>% 
  arrange(fast)

# most delayed flights in the air
flights %>% 
  mutate(air_delay=arr_delay-dep_delay,air.delay.dist=air_delay/distance) %>% 
  select(carrier,flight,origin,dest,air_delay,distance,air.delay.dist) %>% 
  arrange(desc(air.delay.dist))

## finished with number 9




