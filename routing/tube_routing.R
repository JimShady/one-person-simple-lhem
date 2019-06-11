url <- paste("https://api.tfl.gov.uk/Journey/JourneyResults/", start_lat, ",", start_lon, "/to/", end_lat, ",", end_lon,
             "?time=1000&journeyPreference=LeastTime&mode=tube&app_id=", tfl_app_id, "&app_key=", tfl_app_key ,sep="")

json_data <- fromJSON(RCurl::getURL(url), simplify = FALSE)

if (json_data$`$type` == 'Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities') {
  
  temp_results_frame <- data.frame(unique_id = numeric(),
                                   lat = numeric(),
                                   lon = numeric(),
                                   mode = character(),
                                   line = character(),
                                   stringsAsFactors = FALSE)
  
  for (r in 1:length(json_data$journeys[[1]]$legs)) {
    
    line        <-  json_data$journeys[[1]]$legs[[r]]$routeOptions[[1]]$name
    linestring  <-  json_data$journeys[[1]]$legs[[r]]$path$lineString
    linestring  <- gsub(" ", "", linestring, fixed=TRUE)
    linestring  <- gsub("[", "", linestring, fixed = TRUE)
    linestring  <- gsub("]", "", linestring, fixed = TRUE)
    linestring  <- unlist(strsplit(linestring, split = ","))
    
    per_leg_results <- data.frame(unique_id = numeric(),
                                  lat = numeric(),
                                  lon = numeric(),
                                  mode = character(),
                                  line = character(),
                                  stringsAsFactors = FALSE)
    
    l <- 2
    m <- 1
    for (k in 1:(length(linestring)/2)){
      per_leg_results[k,] <- c(as.numeric(i), as.numeric(linestring[m]), as.numeric(linestring[l]), mode, line)
      l <- l+2
      m <- m+2
    }
    
    temp_results_frame <- rbindlist(list(temp_results_frame, per_leg_results), use.names=TRUE)
    rm(per_leg_results)
    temp_results_frame$unique_id <-    as.numeric(temp_results_frame$unique_id)
    temp_results_frame$lat       <-    as.numeric(temp_results_frame$lat)
    temp_results_frame$lon       <-    as.numeric(temp_results_frame$lon)
    
    
  }
  temp_results_frame[temp_results_frame$line == "","line"] <- NA
  temp_results_frame$line <- fill_missing_info(temp_results_frame$line)
  results <- rbindlist(list(results, temp_results_frame), use.names=TRUE)
  print(paste("At", Sys.time(), "routing unique journey id", i, "was completed using TfL tube mode"))
  rm(temp_results_frame, k, l, m, r, line, linestring, url, json_data)
  unique_journeys[i,]$status <- 'tfl'
  } else
{ print(paste("At", Sys.time(), "routing unique journey id", i, "was NOT completed using TfL tube mode, the error id will be logged"))
  error_ids[j,] <- i
  j <- j+1
}