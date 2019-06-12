## This script runs through the data in the /stages , /person and /household tables
## it sources scripts to do the routing
## It outputs data to the /routing_results folder, and the /day_tables folders.

## Issues / To do
## 1) The data that is put back into the day table does not bring through which London Underground line was used,
## or bus number etc. So the 'line' column is never updated. Need to think about a better way to do this.
## 2) At the moment only the 'car' routing works. The others need fixing.
## 3) The function which chooses which routing api needs to be cleverer in terms of whether in London or not.

rm(list=ls())

library(tidyverse)
library(sf)
library(lubridate)
library(googleway)
library(ggmap)
library(gepaf)
library(data.table)


people <- list.files('people/')
print(length(people))


for (i in 1:length(people)) {
  
  ## Prep work
  person    <- read_csv(paste0('people/',people[i]), col_types = cols())
  
  ppid      <- substr(people[1],1,nchar(people[1])-4)
  
  household <- read_csv(paste0('household/', substr(people[i], 1,8), '.csv'), col_types = cols()) %>%
              mutate(date = ymd(htdate))
  
  # extracing for convenience
  date      <- household$date
  
  # Get the stages in as a list of spatial dataframes
  stages <- list()
  
  stage_files <- list.files('stages/', pattern = ppid, full.names = T)
  
      for (j in 1:length(stage_files)) {
        
        # Import a function to fix the times column
        source('functions/format_ltds_datetimes.R')
        
        stages[[j]]                   <- read_csv(stage_files[j], col_types = cols()) %>%
                                          st_as_sf(coords = c('stage_start_easting', 'stage_start_northing')) %>%
                                          st_set_crs(27700) %>%
                                          mutate(stage_start_time = format_ltds_datetimes(stage_start_time, date),
                                                 stage_end_time   = format_ltds_datetimes(stage_end_time, date),
                                                 start            = geometry) %>%
                                          st_set_geometry('start') %>%
                                          select(-geometry)
        
        # Give the sptail data frame a second geom column (non-standard behaviour)
        stages[[j]]$end             <- st_sfc(st_point(cbind(stages[[j]]$stage_end_easting, stages[[j]]$stage_end_northing)))
        stages[[j]]$stage_end_easting  <- NULL
        stages[[j]]$stage_end_northing <- NULL
        
        rm(format_ltds_datetimes)
        
      }
  rm(j)
  # End of prep
  
  ## Create a blank day for this person from 04:00 to 03:59
  
  day_table <- tibble(ppid = ppid,
                      date_time = seq(from = as.POSIXct(household$date) + 14400,
                                      by   = 'min',
                                      length.out = 1440),
                      x    = NA,
                      y    = NA,
                      mode = NA,
                      line = NA)
  
  ## Fill in the locations before a journey is made
  
  day_table[day_table$date_time < stages[[1]]$stage_start_time,'x']               <- st_coordinates(stages[[1]])[1]
  day_table[day_table$date_time < stages[[1]]$stage_start_time,'y']               <- st_coordinates(stages[[1]])[2]
  day_table[day_table$date_time < stages[[1]]$stage_start_time,'mode']            <- 'indoor'
  
  # Fill in the locations after the last journey
  
  day_table[day_table$date_time > stages[[length(stages)]]$stage_end_time,'x']    <- st_coordinates(st_set_geometry(stages[[length(stages)]], 'end'))[1]
  day_table[day_table$date_time > stages[[length(stages)]]$stage_end_time,'y']   <- st_coordinates(st_set_geometry(stages[[length(stages)]], 'end'))[2]
  day_table[day_table$date_time > stages[[length(stages)]]$stage_end_time,'mode'] <- 'indoor'
  
  ## Now a loop here which will route each of the stages and save them to a folder
  
  for (k in 1:length(stages)) {
    
      source('functions/choose_routing_api.R')
      

      
      start_lat  <- st_coordinates(st_transform(stages[[k]], 4326))[2]
      start_lon  <- st_coordinates(st_transform(stages[[k]], 4326))[1]
      end_lat    <- st_coordinates(st_transform(st_set_crs(st_set_geometry(stages[[k]], 'end'),27700),4326))[2]
      end_lon    <- st_coordinates(st_transform(st_set_crs(st_set_geometry(stages[[k]], 'end'),27700),4326))[1]
      stage_mode <- stages[[k]]$stage_mode
      ssid       <- as.character(stages[[k]]$stage_id)
  
      choose_routing_api(stage_mode, start_lat, start_lon, end_lat, end_lon)
      
      save(route, file= paste0('routing_results/', ssid, '.Rdata'))
      
      rm(start_lat, start_lon, end_lat, end_lon, stage_mode, ssid, route)
      
  }
  
  ## Now import each route, turn it into a line, and then split the line by the number of minute the journey is. Then store than into the day_table
  
  for (k in 1:length(stages)) {
  
    load(paste0('routing_results/', as.character(stages[[k]]$stage_id), '.Rdata'))
    
    route <- route %>%
              st_as_sf(coords = c('x', 'y'), crs = 4326) %>%
              st_transform(27700) %>%
              st_coordinates() %>%
              st_linestring() %>%
              st_line_sample(stages[[k]]$stage_duration, type='regular') %>%
              st_cast("POINT") %>%
              st_coordinates()
    
    day_table[day_table$date_time >= stages[[k]]$stage_start_time &
                day_table$date_time < stages[[k]]$stage_end_time, c('x', 'y')] <- route 
    
    day_table[day_table$date_time >= stages[[k]]$stage_start_time &
                day_table$date_time < stages[[k]]$stage_end_time, c('mode')]   <- stages[[k]]$stage_mode
    
  
  }
  
  ## Tidy up the missing data in the day table
  
  day_table[is.na(day_table$mode),'mode'] <- 'indoor'
  day_table <- day_table %>% 
                fill(x, .direction = 'down') %>%
                fill(y, .direction = 'down')
  
  ## Write the day table out to file before cleaning up and moving onto the next person
  
  save(day_table, file = paste0('day_tables/', ppid, '.Rdata'))
  
  rm(day_table, household, person, route, stages, date, i, k, people, ppid, stage_files, choose_routing_api, decodeLine)
  
}
