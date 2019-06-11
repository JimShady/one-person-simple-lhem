rm(list=ls())

library(tidyverse)
library(sf)
library(lubridate)

people <- list.files('people/')
print(length(people))


for (i in 1:length(people)) {
  
  ## Prep work
  person    <- read_csv(paste0('people/',people[i]))
  
  ppid      <- substr(people[1],1,nchar(people[1])-4)
  
  household <- read_csv(paste0('household/', substr(people[i], 1,8), '.csv')) %>%
              mutate(date = ymd(htdate))
  
  # extracing for convenience
  date      <- household$date
  
  # Get the stages in as a list of spatial dataframes
  stages <- list()
  
  stage_files <- list.files('stages/', pattern = ppid, full.names = T)
  
      for (j in 1:length(stage_files)) {
        
        # Import a function to fix the times column
        source('functions/format_ltds_datetimes.R')
        
        stages[[j]]                   <- read_csv(stage_files[j]) %>%
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
        
        print(j)
        
        rm(format_ltds_datetimes)
        
        }
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
  
  ## Now a loop here which will route each of the stages
  
  
  
}




## Route each of the routes that need doing, getting the linestring and any info about which tube, or bus etc if applicable



## Store these in a folder as ssid.Rdata (Aswell as insert to the blank day above)



## Identify which CMAQ files going to want to access



## Import CMAQ files and sort out as a geo-referenced raster



## Extract concentrations from the raster by location, hour(?), month(?) etc. store as new 'raw_exposure' column in table



## Depending on the transport mode used (for now) apply microenvironment I/O factors 



## When between transport, apply I/O factor for indoors



## Write the final dataframe to file, as <ppid>.Rdata


