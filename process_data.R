
# Install libraries

library(tidyverse)
library(dplyr)
library(stringr)
library(fs)
library(reshape2)
library(knitr)
library(lubridate)
library(scales)
library(foreign)
library(janitor)
library(data.table)

#  load in Results data

results <- read_csv("mt_2_results.csv") %>% mutate(state_dist = paste(str_to_lower(state), district, sep = "")) %>% 
  mutate(total = dem_votes + rep_votes + other_votes, dem = dem_votes/total, rep = rep_votes/total, other = other_votes/total)

# load in upshot data

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")

my_list <- dir_ls("2018-live-poll-results-master/data/")

# Combine all upshot data into one dataframe

predictions <- map_dfr(my_list, read_csv, .id = "source") 

# Create variables for state, district and state_district combo and filter for wave 3

pred_wave_3 <- predictions %>%  mutate(wave = str_sub(source, -5,-5), state = str_sub(source, 51,52), district = str_sub(source,53,54), district = as.numeric(district), state_dist = str_extract(source, "[a-zA-Z]{2}[0-9]{2}")) %>% 
  filter(wave == 3)

# Clean errors in response variable

pred_wave_3$response[pred_wave_3$response==3] <- "Und"
pred_wave_3$response[pred_wave_3$response==4] <- "Und"
pred_wave_3$response[pred_wave_3$response==5] <- "Und"
pred_wave_3$response[pred_wave_3$response==6] <- "Und"

# collapse upshot data into district level

 pred_district <- pred_wave_3 %>% 
   select(response, final_weight, state_dist, wave) %>% 
   filter(!is.na(final_weight)) %>% 
   group_by(response, state_dist) %>% 
   tally(wt = final_weight) %>% 
   group_by(state_dist) %>% 
   spread(key = response, value = n) %>% 
   filter(!is.na(state_dist)) %>% 
   group_by(state_dist) %>% 
   mutate(total = Dem + Rep + Und, Dem = Dem/total, Rep = Rep/total, Und = Und/total) 
   
 
 
 
 #Final dataframe combining both results and predictions for wave 3 polling
 
 final <- left_join(results, pred_district, by = "state_dist") %>% 
   filter(!is.na(Dem)) %>% 
   transmute( state = state, district = district, state_dist = state_dist, Dem_Pred = Dem, Rep_Pred = Rep, Und_Pred = Und, Dem_Act = dem, Rep_Act = rep, Und_Act = other, Winner = win_name, total = total.x) %>% 
   mutate(Dem_Pred = Dem_Pred *100, Rep_Pred = Rep_Pred *100, Und_Pred = Und_Pred *100, Dem_Act = Dem_Act*100, Rep_Act = Rep_Act*100, Und_Act = Und_Act *100 ) %>% 
   mutate(Dem_Error = Dem_Act - Dem_Pred, Rep_Error = Rep_Act - Rep_Pred, Und_Error = Und_Act - Und_Pred)
 
 #write CSV of data for App 
 
 project_directory <- "/Users/charlesflood/Documents/" 
 
 write.csv( final ,
                       file = file.path( project_directory ,
                                          "PS_7.csv" ) )
 
 
  



