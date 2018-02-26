# load libraries
library(tidyverse)
library(nycflights13)

# dataset for assignment
data = flights

# A
flights %>% filter(arr_delay>=120)

# B
flights %>% filter(dest==c("IAH","HOU"))

# C
flights %>% filter(carrier==c("UA","AA","DL"))

# D
flights %>% filter(month %in% c(7,8,9))

# E
flights %>% filter(arr_delay>120) %>% filter(dep_delay<=0)

# F
flights %>% filter(dep_delay>=60) %>% filter(dep_delay-arr_delay>30)

# G
flights %>% filter(dep_time<=600)

# H
flights %>% arrange(desc(dep_delay))
flights %>% arrange(dep_delay)

# I
flights %>% arrange(desc(distance/air_time))

# J
flights %>% arrange(desc(air_time))
flights %>% arrange(air_time)
# OR
flights %>% arrange(desc(distance))
flights %>% arrange(distance)



