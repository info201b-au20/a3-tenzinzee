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
  
  total_prison_pop, native_prison_pop, aapi_prison_pop, black_prison_pop,
  white_prison_pop, latinx_prison_pop, black_female_prison_pop, 
  white_female_prison_pop,  
  
  total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate, 
  black_jail_pop_rate, white_jail_pop_rate, 
  
  total_prison_pop_rate, female_prison_pop_rate, male_prison_pop_rate,
  black_prison_pop_rate, white_prison_pop_rate)

# ----------------------------------------------
# ########### Trends over Time Chart ###########
# ----------------------------------------------

# clean and filter incarceration_data 
prison_year_pop <- incarceration %>%
  select(year, state, county_name, white_prison_pop, 
         black_prison_pop, native_prison_pop, 
         latinx_prison_pop, aapi_prison_pop) %>%
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
  ggplot(aes(x = year, y = Population, color = Race)) + 
  geom_point() + 
  facet_wrap(~state)

prison_year_pop_plot <- prison_year_pop_plot + 
  stat_smooth(method = 'loess', span = 0.3, size = 0.3) + 
  labs(title = "Prison Population by Race Time Trend ")

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

prison_rate_prop_plot <- prison_rate_prop_plot + 
  labs(title = "Black Admission Rate vs Population Rate", subtitle = "Comparing prison rates in 2016") +
  xlab("Black Prison Admission Rate") + 
  ylab("Black Prison Population Rate")

# ---------------------------------------------
# ################# County Map  ###############
# ---------------------------------------------

# filtering incarceration data to 2016 prison admission data   
prison_adm <- incarceration_data %>% 
  select(year, fips, state, county_name, black_prison_adm_rate, white_prison_adm_rate) %>%
  mutate(bw_ratio = black_prison_adm_rate/white_prison_adm_rate) %>% 
  filter(year == 2016) 

# loading county shapefile with fips
prison_map <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname") %>%
  left_join(prison_adm, by="fips") 

# cleaning map template for mapping
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

# mapping black_white_ratio on the county map
prison_adm_map <- ggplot(prison_map) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = bw_ratio),
    color = "grey", size = .2) + 
  coord_map() +
  blank_theme +
  scale_fill_continuous(limits=c(0,max(prison_map$bw_ratio)), na.value="white", low="blue", high="red") +
  ggtitle("Black Prison Admission Rate > White Prison Admission Rate") + 
  labs(subtitle = "2016")

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

max_adm_rate <- prison_adm %>%
  na.omit() %>%
  filter(white_prison_adm_rate == 0) %>%
  select(state)

