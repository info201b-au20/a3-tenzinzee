# load neccesary packages
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)

# load Vera Institude 
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# cleaning `incarceration` df `
incarceration <- incarceration_data %>%
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

# ----------------------------------------------
# ########### Trends over Time Chart ###########
# ----------------------------------------------

# clean and filter incarceration_data 
prison_year_pop <- incarceration_data %>%
  select(year, state, county_name, white_prison_pop, 
         black_prison_pop, native_prison_pop, 
         latinx_prison_pop, aapi_prison_pop, total_pop) %>%
  na.omit()

# summarising total sum of prison_pop by race
prison_year_pop <- prison_year_pop %>%
  filter(state == "CA" | state == "NY") %>%
  group_by(year, state) %>%
  summarise(Black_pop = sum(black_prison_pop), 
            White_pop = sum(white_prison_pop),
            Native_pop = sum(native_prison_pop),
            Latinx_pop = sum(latinx_prison_pop),
            Aapi_pop = sum(aapi_prison_pop))

# mutating data to categorical and num cols 
prison_year_pop <- prison_year_pop %>%
  pivot_longer(cols = contains("pop"), 
               names_to = "Race",
               values_to = "Population") %>%
  filter(!Population == 0) 

# plot population vs year, categorized by race, faceted by state
prison_year_pop_plot <- prison_year_pop %>%
  ggplot(aes(
    x = year, y = Population, 
    color = Race)) + 
  geom_point() + 
  stat_smooth(method = 'loess', span = 0.3, size = 0.3) + 
  facet_wrap(~state)
#geom_smooth(method = "lm", alpha = .2) + 

# ---------------------------------------------
# ############# 2 Variable Chart  #############
# ---------------------------------------------

# clean and filter incarceration_data 
prison_rate_prop <- incarceration_data %>%
  select(year, state, fips, county_name, total_prison_adm_rate, 
         white_prison_adm_rate, black_prison_adm_rate, 
         white_prison_pop_rate, black_prison_pop_rate) %>%
  filter(year == 2016) %>%
  na.omit()

# summarising total av of pop, adm rates by race
prison_rate_prop <- prison_rate_prop %>%
  group_by(state, county_name) %>%
  summarise(Black_adm_rate = mean(black_prison_adm_rate),
            Black_pop_rate = mean(black_prison_pop_rate)) %>%
  arrange(desc(Black_adm_rate)) %>%
  filter(state == "WA" |state ==  "NY" |state ==  "FL" |state ==  "CA" |state ==  "GA")

# plot prop by pop
prison_rate_prop_plot <- prison_rate_prop %>%
  ggplot(aes(x = Black_adm_rate,
             y = Black_pop_rate, 
             color = state)) + 
  geom_point() 

# ----------------------------------------------
# ########### Summary Information ###########
# ----------------------------------------------
max_black_pris_pop_cali <- prison_year_pop %>%
  filter(Race == "Black_pop") %>% 
  ungroup() %>%
  filter(Population == max(Population)) 

max_black_pris_pop <- incarceration_data %>%
  select(year, state, county_name, black_prison_pop, white_prison_pop) %>%
  filter(!black_prison_pop == 0) %>%
  filter(black_prison_pop == max(black_prison_pop)) 

max_bw_ratio <- incarceration_data %>%
  select(year, state, county_name, black_prison_pop, white_prison_pop) %>%
  na.omit() %>%
  mutate(bw_ratio = black_prison_pop/white_prison_pop) %>%
  filter(bw_ratio == max(bw_ratio))
  

# ---------------------------------------------
# ################# County Map  ###############
# ---------------------------------------------

# prison population by race
# black prison pop / total prison pop OR total state pop
prison_race_prop <- incarceration %>%
  select(year, state, county_name, total_prison_pop,
         black_prison_pop, white_prison_pop) 

# WIDE; create prop cols
prison_race_prop <- prison_race_prop %>%
  filter(state == "NY") %>%
  mutate(prop_black = black_prison_pop/total_prison_pop) %>%
  mutate(prop_white = white_prison_pop/total_prison_pop) %>%
  select(year, county_name, prop_black, prop_white) 
# note: prison pop 

# LONG; create categorical & num cols
prison_race_prop <- prison_race_prop %>%
  group_by(year) %>%
  pivot_longer(cols = contains("prop"), 
               names_to = "Race",
               values_to = "Proportion")

# plot race prop by years 
prison_race_prop_plot <- prison_race_prop %>%
  ggplot(aes(x = year, y = Proportion, 
             color = Race)) + # mapping race to color
  geom_point(alpha = .5) + # adding transparency
  facet_wrap(~Race) # faceting by race 

# ----------------

# jail_race_prop <- incarceration %>%
#   filter(state == "FL") %>%
#   group_by(year, county_name) %>%
#   mutate(prop_black = black_jail_pop/total_pop) %>%
#   mutate(prop_white = total_pop - (black_jail_pop/total_pop)) %>%
#   select(year, county_name, prop_black, prop_white) 
# # note: prison pop 
# 
# # LONG; create categorical & num cols
# jail_race_prop <- jail_race_prop %>%
#   pivot_longer(cols = contains("prop"), 
#                names_to = "Race",
#                values_to = "Proportion")
# 
# # plot race prop by years 
# jail_race_prop_plot <- jail_race_prop %>%
#   ggplot(aes(x = year, y = Proportion, 
#              color = Race)) + # mapping race to color
#   geom_point(alpha = .5) + # adding transparency
#   facet_wrap(~Race) + 
#   geom_smooth(method = "lm") + 
#   xlim(1980, 2016)
# 
# # ---------------------------------------------------------------------------------
# # Variable comparison chart 
# # ---------------------------------------------------------------------------------
# 
# # clean and filter incarceration_data 
# prison_year_rate <- incarceration_data %>%
#   select(year, state, fips, county_name, total_prison_adm_rate, 
#          white_prison_adm_rate, black_prison_adm_rate, 
#          white_prison_pop_rate, black_prison_pop_rate) %>%
#   filter(year == 2016) %>%
#   na.omit()
# 
# # summarising total sum of pop, adm rates by race
# prison_year_rate <- prison_year_rate %>%
#   group_by(state, county_name) %>%
#   summarise(Black_adm_rate = mean(black_prison_adm_rate),
#             Black_pop_rate = mean(black_prison_pop_rate)) %>%
#   arrange(desc(Black_adm_rate)) %>%
#   filter(state == "WA" |state ==  "NY" |state ==  "FL" |state ==  "CA" |state ==  "GA")
# 
# # plot population by year, categorized by race, faceted by state
# prison_year_rate_plot <- prison_year_rate %>%
#   ggplot(aes(
#     x = Black_adm_rate, y = Black_pop_rate, 
#     color = state)) + # mapping race to color
#   geom_point() + 
#   geom_smooth(lm)
# 
#             
#             
# # manipulate to categorical and num cols for plot
# prison_year_rate <- prison_year_rate %>%
#   pivot_longer(cols = contains("pop"), 
#                names_to = "Race",
#                values_to = "Population")
# 
# # plot population by year, categorized by race, faceted by state
# prison_year_rate_plot <- prison_year_rate %>%
#   ggplot(aes(
#     x = year, y = Population, 
#     color = Race)) + # mapping race to color
#   geom_point() + 
#   geom_line() + #alpha = .5) + # adding transparency
#   facet_wrap(~state) # faceting by race 
# # ---------------------------------------------------------------------------------
# 
# 
# # black prison pop / total prison pop OR total state pop
# prison_race_prop <- incarceration %>%
#   select(year, state, county_name, total_prison_pop,
#          black_prison_pop, white_prison_pop) 
# 
# # WIDE; create prop cols
# prison_race_prop <- prison_race_prop %>%
#   filter(state == "NY") %>%
#   mutate(prop_black = black_prison_pop/total_prison_pop) %>%
#   mutate(prop_white = white_prison_pop/total_prison_pop) %>%
#   select(year, county_name, prop_black, prop_white) 
# # note: prison pop 
# 
# # LONG; create categorical & num cols
# prison_race_prop <- prison_race_prop %>%
#   group_by(year) %>%
#   pivot_longer(cols = contains("prop"), 
#                names_to = "Race",
#                values_to = "Proportion")
# 
# # plot race prop by years 
# prison_race_prop_plot <- prison_race_prop %>%
#   ggplot(aes(x = year, y = Proportion, 
#              color = Race)) + # mapping race to color
#   geom_point(alpha = .5) + # adding transparency
#   facet_wrap(~Race) # faceting by race 
# 
# 
# 
# # prison_race_prop = wide
# # make long (by face and proportion, factor = year)
# # then ggplot, facet wrap
#   
# 
# 
# schools_wide <-
#   schools %>%
#   select(`School Name`, `Economic Need Index`, 
#          `Percent Asian`, `Percent Black`, 
#          `Percent Hispanic`, `Percent White`)
# 
# 
# 
# 
# 
# 
# 
# # subsetting `incarceration` df
# 
# #  Summary Information
# # At least 5 values calculated from data; ie. av of var in all counties, year; 
# # where var is highest/lowest; how much var changed over the years; focus on by race
# 
# summary <- incarceration_data %>%
#   select(year, state, county_name, black_prison_pop, white_prison_pop,
#          black_jail_pop, white_jail_pop) %>%
#   na.omit() 
# 
# 
# # focus on black prison population ------
# 
# highest_black_prison_pop <- summary %>%
#   filter(black_prison_pop == max(black_prison_pop)) %>%
#   select(year, county_name, black_prison_pop)
# 
# lowest_black_prison_pop <- summary %>%
#   filter(black_prison_pop > 0) %>%
#   filter(black_prison_pop == min(black_prison_pop)) %>%
#   select(year, county_name, black_prison_pop)
# 
# years_av_black_prison_pop <- summary %>%
#   select(year, state, county_name, black_prison_pop) %>%
#   group_by(year, state, county_name) %>%
#   summarize(mean_rate = mean(black_prison_pop))
# 
# # prison ratio black white 
# prison_pop_ratio <- incarceration_data %>%
#   select(year, fips, county_name, black_prison_pop_rate, white_prison_pop_rate, total_prison_pop_rate) %>%
#   mutate(black_white_ratio = black_prison_pop_rate/white_prison_pop_rate)
#   
# # how many counties in 2016 had black white ratio > 1 
# # aka more blacks than whites in prison population
# high_prison_pop_ratio <- prison_pop_ratio %>%
#   filter(black_white_ratio > 1) %>%
#   filter(year == 2016)
# 
# highest_prison_pop_ratio <- high_prison_pop_ratio %>%
#   filter(black_white_ratio == max(black_white_ratio)) %>%
#   
#   
# 
# # 
# 
# av_prison_rate <- summary %>%
#   na.omit(av_prison_pop_rate$total_prison_pop_rate) %>%
#   group_by(year, state) %>%
#   summarize(mean_rate = mean(total_prison_pop_rate, na.rm = TRUE))
# 
# highest_prison_rate_county <- summary %>%
#   na.omit(av_prison_pop_rate$total_prison_pop_rate) %>%
#   filter(total_prison_pop_rate == max(total_prison_pop_rate))
#   
#   
# av_prison_rate_county <- summary %>%
#   na.omit(av_prison_pop_rate$total_prison_pop_rate) %>%
#   group_by(year, state) %>%
#   summarize(mean_rate = mean(total_prison_pop_rate, na.rm = TRUE))
# 
# 
# 
# highest_prison_rate_state <- av_prison_rate %>%
#   filter(mean_rate == max(mean_rate)) %>%
#   pull()
# 
#   
# 
# 
#   
# test <- summary %>%
#   filter(year == 1970 & state == "GA") %>%
#   select(total_prison_pop_rate)
# 
# mean(test$total_prison_pop_rate, na.rm = T)
# 
# # washington specific -------------------------------------
# # chart over time
# 
# over_time <- incarceration_data %>%
#   mutate(black_prop = black_prison_pop/total_prison_pop) %>%
#   mutate(white_prop = white_prison_pop/total_prison_pop) %>%
#   select(year, county_name, state, black_prop, white_prop) %>%
#   na.omit()
# 
# over_time_top_state <- over_time %>%
#   mutate(bw_ratio = black_prop/white_prop) %>%
#   group_by(year, state) %>%
#   summarize(max_bw_ratio = max(mean(round(bw_ratio))))
# 
# # filter to states with black prop >  
# 
# 
#   
# wa_incarceration <- incarceration_data %>%
#   filter(state == "WA") %>%
#   select(year, fips, county_name, black_prison_pop, white_prison_pop, 
#          black_jail_pop, white_jail_pop, total_prison_pop, total_jail_pop)
# 
# wa_prison_years <- wa_incarceration %>%
#   select(year, fips, county_name, black_prison_pop, white_prison_pop,
#          total_prison_pop) %>%
#   mutate(black_prop = black_prison_pop/total_prison_pop) %>%
#   mutate(white_prop = white_prison_pop/total_prison_pop)
# 
# 
# # washington incarceration data
# # total_pop, adult incar; 
# 
# # var descrips
# # jail: awaiting trail/not pled guilty/misdemeanor sentence
# # prison: convicted of felony; run by state
# # jail pop count: av. daily #; state prison, local jails
# # prison pop: ppl sentenced to state jail authority; from county 
# # fips: county ID code
# # aapi_pop: asian american/pacific islander
# # region: census region
# # total_jail_from_prison: jail pop count, state prison sys
# # total_jail_from_other_jail: '' local
# # total_jail_from_fed: fed jail pop count (ICE, etc)
# # total_jail_from_ice: jail pop count, ICE
# 
# # female and male prison pop: summarize 
# # race and prison/jail pop: summarize, use soc225 assign 
# # focus on state incareration
# # how many states reduced total pris/jail incarceration rates
# # how many states increased ppl in jail; while reducing prison
# # what states have increased prison/jail populations 
# 
# 
# # ---------------------NOTES--------------------------
# # find # of counties in each state; no deaths
# prop_no_deaths <- counties %>%
#   group_by(state) %>%
#   filter(deaths == 0) %>%
#   filter(date == max(date)) %>%
#   filter(!str_detect(county, "Unknown")) %>%
#   summarize(zero_count = n())
# 
# # find # of counties in each state 
# counties_total <- counties %>%
#   group_by(state) %>%
#   filter(date == max(date)) %>%
#   filter(!str_detect(county, "Unknown")) %>%
#   summarize(total_count = n())
# 
# # merge both as df
# prop_no_deaths <- merge(
#   x = prop_no_deaths, y = counties_total, 
#   by = "state", all = TRUE)
# 
# # take out NA's
# prop_no_deaths <- na.omit(prop_no_deaths)
# 
# # now calculate prop as new col
# prop_no_deaths <- prop_no_deaths %>%
#   mutate(prop = zero_count/total_count) %>%
#   select(state, prop)
# 
# # What proportion of counties in Washington have had zero deaths? 
# # # `wa_prop_no_deaths`
# # wa_prop_no_deaths <- prop_no_deaths %>%
# #   filter(state == "Washington") %>%
# #   pull(prop) 
# 
# 
# 
# 
# race_data <- county_level %>%
#   select(black_jail_pop_rate, white_jail_pop_rate, total_jail_pop_rate, 
#          total_jail_pop_rate, year, fips, state, county_name) %>%
#   mutate(black_white_ratio = black_jail_pop_rate / white_jail_pop_rate)  %>%
#   filter(year == 2018)
# 
# 
# county_shapes <- map_data("country") %>%
#   unite(polyname, region, subregion, sep = ",") %>%
#   left_join(county.fips, by = "polyname")
# 
# map_data <- county_shapes %>%
#   left_join(race_data, by = "fips") %>%
#   filter(state == "WA")
# ggtitle("Black and white ratio rate in 2018")
# 
# blank_theme <- theme_bw() +
#   theme(
#     axis.line = element_blank(), # remove axis lines
#     axis.text = element_blank(), # remove axis labels
#     axis.ticks = element_blank(), # remove axis ticks
#     axis.title = element_blank(), # remove axis titles
#     plot.background = element_blank(), # remove gray background
#     panel.grid.major = element_blank(), # remove major grid lines
#     panel.grid.minor = element_blank(), # remove minor grid lines
#     panel.border = element_blank() # remove border around plot
#   )
# 
# ggplot(county_shapes) +
#   geom_polygon(
#     mapping = aes(x = long, y = lat, fill = black_white_ratio),
#     color = "white", 
#     size = .1
#   ) +
#   coord_map() +
#   blank_theme
# 
