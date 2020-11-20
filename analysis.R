# load neccesary packages
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

# load Vera Institude data
incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# cleaning `incarceration` df `
incarceration_data <- incarceration %>%
  select(
  year, fips, state, county_name,
  total_pop_15to64, female_pop_15to64, male_pop_15to64, 
  black_pop_15to64, white_pop_15to64, 
  
  total_jail_pop, female_jail_pop, male_jail_pop, black_jail_pop, 
  white_jail_pop, aapi_jail_pop, latinx_jail_pop, native_jail_pop,
  
  total_prison_pop, female_prison_pop, male_prison_pop, black_prison_pop,
  white_prison_pop, black_female_prison_pop, black_male_prison_pop, 
  white_female_prison_pop, white_male_prison_pop, 
  
  total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate, 
  black_jail_pop_rate, white_jail_pop_rate, 
  
  total_prison_pop_rate, female_prison_pop_rate, male_prison_pop_rate,
  black_prison_pop_rate, white_prison_pop_rate)

# subsetting `incarceration` df

#  Summary Information
# At least 5 values calculated from data; ie. av of var in all counties, year; 
# where var is highest/lowest; how much var changed over the years; focus on by race

summary <- incarceration_data %>%
  select(year, state, county_name, black_prison_pop, white_prison_pop,
         black_jail_pop, white_jail_pop) %>%
  na.omit() 


# focus on black prison population ------

highest_black_prison_pop <- summary %>%
  filter(black_prison_pop == max(black_prison_pop)) %>%
  select(year, county_name, black_prison_pop)

lowest_black_prison_pop <- summary %>%
  filter(black_prison_pop > 0) %>%
  filter(black_prison_pop == min(black_prison_pop)) %>%
  select(year, county_name, black_prison_pop)

years_av_black_prison_pop <- summary %>%
  select(year, state, county_name, black_prison_pop) %>%
  group_by(year, state, county_name) %>%
  summarize(mean_rate = mean(black_prison_pop))

# prison ratio black white 
prison_pop_ratio <- incarceration_data %>%
  select(year, fips, county_name, black_prison_pop_rate, white_prison_pop_rate, total_prison_pop_rate) %>%
  mutate(black_white_ratio = black_prison_pop_rate/white_prison_pop_rate)
  
# how many counties in 2016 had black white ratio > 1 
# aka more blacks than whites in prison population
high_prison_pop_ratio <- prison_pop_ratio %>%
  filter(black_white_ratio > 1) %>%
  filter(year == 2016)

highest_prison_pop_ratio <- high_prison_pop_ratio %>%
  filter(black_white_ratio == max(black_white_ratio)) %>%
  
  

# 

av_prison_rate <- summary %>%
  na.omit(av_prison_pop_rate$total_prison_pop_rate) %>%
  group_by(year, state) %>%
  summarize(mean_rate = mean(total_prison_pop_rate, na.rm = TRUE))

highest_prison_rate_county <- summary %>%
  na.omit(av_prison_pop_rate$total_prison_pop_rate) %>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate))
  
  
av_prison_rate_county <- summary %>%
  na.omit(av_prison_pop_rate$total_prison_pop_rate) %>%
  group_by(year, state) %>%
  summarize(mean_rate = mean(total_prison_pop_rate, na.rm = TRUE))



highest_prison_rate_state <- av_prison_rate %>%
  filter(mean_rate == max(mean_rate)) %>%
  pull()

  


  
test <- summary %>%
  filter(year == 1970 & state == "GA") %>%
  select(total_prison_pop_rate)

mean(test$total_prison_pop_rate, na.rm = T)

# washington specific -------------------------------------
# chart over time

over_time <- incarceration_data %>%
  mutate(black_prop = black_prison_pop/total_prison_pop) %>%
  mutate(white_prop = white_prison_pop/total_prison_pop) %>%
  select(year, county_name, state, black_prop, white_prop) %>%
  na.omit()

over_time_top_state <- over_time %>%
  mutate(bw_ratio = black_prop/white_prop) %>%
  group_by(year, state) %>%
  summarize(max_bw_ratio = max(mean(round(bw_ratio))))

# filter to states with black prop >  


  
wa_incarceration <- incarceration_data %>%
  filter(state == "WA") %>%
  select(year, fips, county_name, black_prison_pop, white_prison_pop, 
         black_jail_pop, white_jail_pop, total_prison_pop, total_jail_pop)

wa_prison_years <- wa_incarceration %>%
  select(year, fips, county_name, black_prison_pop, white_prison_pop,
         total_prison_pop) %>%
  mutate(black_prop = black_prison_pop/total_prison_pop) %>%
  mutate(white_prop = white_prison_pop/total_prison_pop)


# washington incarceration data
# total_pop, adult incar; 

# var descrips
# jail: awaiting trail/not pled guilty/misdemeanor sentence
# prison: convicted of felony; run by state
# jail pop count: av. daily #; state prison, local jails
# prison pop: ppl sentenced to state jail authority; from county 
# fips: county ID code
# aapi_pop: asian american/pacific islander
# region: census region
# total_jail_from_prison: jail pop count, state prison sys
# total_jail_from_other_jail: '' local
# total_jail_from_fed: fed jail pop count (ICE, etc)
# total_jail_from_ice: jail pop count, ICE

# female and male prison pop: summarize 
# race and prison/jail pop: summarize, use soc225 assign 
# focus on state incareration
# how many states reduced total pris/jail incarceration rates
# how many states increased ppl in jail; while reducing prison
# what states have increased prison/jail populations 

