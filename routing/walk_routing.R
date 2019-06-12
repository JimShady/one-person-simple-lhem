source('../tfl_api_credentials.R')

url <- paste("https://api.tfl.gov.uk/Journey/JourneyResults/", start_lat, ",", start_lon, "/to/", end_lat, ",", end_lon,
             "?journeyPreference=LeastTime&mode=walking&app_id=", tfl_app_id, "&app_key=", tfl_app_key, sep="")

received <- RCurl::getURL(url)

temp <- try(fromJSON(received, simplify = FALSE))

if (class(temp) != 'try-error') {

json_data <- fromJSON(received, simplify = FALSE)

if (json_data$`$type` == 'Tfl.Api.Presentation.Entities.JourneyPlanner.ItineraryResult, Tfl.Api.Presentation.Entities') {
  
  linestring <- json_data$journeys[[1]]$legs[[1]]$path$lineString
  linestring <- gsub(" ", "", linestring, fixed=TRUE)
  linestring <- gsub("[", "", linestring, fixed = TRUE)
  linestring <- gsub("]", "", linestring, fixed = TRUE)
  linestring <- unlist(strsplit(linestring, split = ","))
  linestring <- as.numeric(linestring)
  temp_results_frame <- data.frame(unique_id = numeric(),
                                   lat = numeric(),
                                   lon = numeric(),
                                   mode = character(),
                                   line = character(),
                                   stringsAsFactors = FALSE)
  l <- 2
  m <- 1
  for (k in 1:(length(linestring)/2)){
    temp_results_frame[k,] <- c(as.numeric(i), as.numeric(linestring[m]), as.numeric(linestring[l]), mode, NA)
    l <- l+2
    m <- m+2
  }
  temp_results_frame$unique_id <- as.numeric(as.character(temp_results_frame$unique_id))
  temp_results_frame$lon <- as.numeric(as.character(temp_results_frame$lon))
  temp_results_frame$lat <- as.numeric(as.character(temp_results_frame$lat))
  temp_results_frame$mode <- as.character(temp_results_frame$mode)
  temp_results_frame$line <- as.character(temp_results_frame$line)
  results <- rbindlist(list(results, temp_results_frame), use.names=TRUE)
  print(paste("At", Sys.time(), "routing unique journey id", i, "was completed using TfL walking mode"))
  rm(json_data, temp, temp_results_frame, linestring, url, l, m, k)
  unique_journeys[i,]$status <- 'tfl'
} else
{ print(paste("At", Sys.time(), "routing unique journey id", i, "was NOT completed using TfL walking mode, the error id will be logged"))
  error_ids[j,] <- i
  j <- j+1
}  } else {
 print(paste("At", Sys.time(), "routing unique journey id", i, "was NOT completed using TfL walking mode, the error id will be logged"))
    error_ids[j,] <- i
    j <- j+1
}

Sys.sleep(0.3)
rm(tfl_app_id, tfl_app_key)
