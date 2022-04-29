# library and clean working directory 
rm(list = ls())
getwd()
library(tidyverse)
library(readr)
library(timetk)
library(lubridate)

df <- read_csv("raw/pdp.csv") %>%
  filter(device_type %in% c('Mobile','Desktop'))


df <- df %>%
  mutate(rtr_type = if_else(
    pts_source=='rtr' & brand_id=='843b07c3-49b0-432d-82af-d2bfdffcb6a8',
    'Free',
    if_else(
      pts_source=='rtr' & brand_id!='843b07c3-49b0-432d-82af-d2bfdffcb6a8',
      'Paid',NULL
)))
     
head(df)

           
           
           

################################################################################
# 1. Show the number of RTR available in the system over time
################################################################################

rtr_available_overtime <- df %>%
  #select(date, inv_offer_id, pts_source,offer_typ,etyp) %>%
  filter(offer_typ == 'survey', etyp == 'CRDIMPR') %>%
  group_by(date) %>%
  mutate(rtr_available = n_distinct(inv_offer_id)) %>%
  distinct() %>% ungroup()

rtr_available_overtime  %>% 
  plot_time_series(date, 
                   rtr_available,
                   #.interactive = FALSE,
                   .smooth = F,
                   .x_lab = "",
                   .y_lab = "",
                   .title = "RTR available in the system over time",
                  )

rtr_available_overtime  %>%
  ggplot(aes(date, rtr_available)) +
  geom_line() +
  ggtitle('# RTR available in the system') + 
  ylab('')
################################################################################
# 2. Ratio of overall earned points to available points.
################################################################################
rtr_seen_per_day <-   df %>%
  filter(offer_typ == 'survey', etyp == 'CRDIMPR') %>%
  select(date, inv_offer_id) 

rtr_worth_pts <- df %>%
  group_by(inv_offer_id) %>%
  summarise(inv_pts = max(inv_pts)) 

available_pts <- rtr_seen_per_day %>%
  left_join(rtr_worth_pts) %>%
  group_by(date) %>%
  summarise(rtr_available_pts = sum(inv_pts))

rtr_earned_pts <- df %>%
  filter(etyp == 'CRDERN', pts_source == 'rtr') %>%
  group_by(date) %>%
  summarise(rtr_earned_pts = sum(inv_pts))

ratio <- available_pts %>%
  left_join(rtr_earned_pts)


ratio  %>%
  gather(key = "variable", value = "value", -date) %>%
  ggplot(aes(date, value, group = variable, color = variable)) +
  geom_line()
  

## check 
df %>%
filter(etyp %in% c('CRDLK' , 'CRDDLK'),
       ext_user_id =='70cd4eae-8730-4cb0-aa84-d8a9e0665fb3', 
       date == '2022-04-19') %>%
  select(ext_user_id, date, etyp, inv_pts) %>% view()  


################################################################################
# 3.0 as a corollary we need to show the number of overall pieces of content 
# available at any one time.

# I do not think pdp events show seen contents by user instead
#  we only see contents consumed by users(CRDLK, CRDDLK, CRDDTL, 
#  CRDSPNT, CRDBM, CRDCLK)
################################################################################
contents_available_overtime <- df %>%
  filter(pts_source_ == 'news' , etyp == 'CRDIMPR') %>%
  group_by(date) %>%
  mutate(contents_available = n_distinct(inv_offer_id)) %>%
  distinct()
################################################################################
# 3.1 - show the ratio of paywalls accessed relative to the overall number of articles.
################################################################################

overall_articles <- 
df %>%
  filter(pts_source_ =='articles') %>%
  group_by(date) %>%
  summarise(overall_articles = n_distinct(inv_offer_id)) %>%
  distinct()

paywalls_accessed <- df %>%   
  filter(etyp == 'CRDSPNT') %>%
  group_by(date, inv_pts) %>%
  summarise(paywalls = n()) %>%
  select(-inv_pts) %>%
  distinct()

# join 
ratio_paywall_overall_articles <- paywalls_accessed %>%
  left_join(overall_articles) %>%
  mutate(ratio = round(
    (paywalls / overall_articles),
    2))
  
ratio_paywall_overall_articles  %>% 
  ggplot(aes(date, ratio)) +
  geom_line()


ratio_paywall_overall_articles  %>% 
  select(-ratio) %>%
  gather(key = "variable", value = "value", -date) %>%
  ggplot(aes(date, value, group = variable, color = variable)) +
  geom_line()
################################################################################
# 4.0 we need to show the overall ratio of earn to spend at any given time
################################################################################

earn <- df %>%
  #filter(etyp %in% c('CRDERN', 'CRDLK' , 'CRDDLK')) %>%
  filter(etyp == 'CRDERN', pts_source == 'rtr') %>%
  group_by(date) %>%
  summarise(earn = sum(inv_pts)) %>%
  distinct()

# join earn with spend
spend <-  df %>%   
  filter(etyp == 'CRDSPNT') %>%
  group_by(date) %>%
  summarise(spend = sum(inv_pts)) %>%
  distinct()

ratio_earn_to_spend <- earn %>%
  left_join(spend) %>%
  mutate(ratio  = round(
    (earn / spend),
    2),
    diff_adj = (earn - spend)/ earn)


ratio_earn_to_spend  %>% 
  ggplot(aes(date, diff_adj)) +
  geom_line()


ratio_earn_to_spend  %>% 
  select(-ratio) %>%
  gather(key = "variable", value = "value", -date) %>%
  ggplot(aes(date, value, group = variable, color = variable)) +
  geom_line()
