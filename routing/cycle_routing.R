url <- paste("http://www.cyclestreets.net/api/journey.json?key=", cycle_streets_api_key, "&itinerarypoints=", start_lon, ",", start_lat,
             "|", end_lon, ",", end_lat, "&plan=balanced&segments=0&reporterrors=1", sep="")

json_data <- fromJSON(RCurl::getURL(url), simplify = FALSE)

if (is.null(json_data$marker) == FALSE) {
  temp_results_frame <- data.frame(unique_id = numeric(),
                                   lat = numeric(),
                                   lon = numeric(),
                                   mode = character(),
                                   line = character(),
                                   stringsAsFactors = FALSE)
  
  linestring <- json_data$marker$'@attributes'$coordinates
  linestring <- unlist(strsplit(linestring, split = " "))
  
  for (k in 1:length(linestring)) {
    temp_results_frame[k,] <- list(as.numeric(i),
                                   as.numeric(unlist(strsplit(linestring[k], split=","))[2]),
                                   as.numeric(unlist(strsplit(linestring[k], split=","))[1]),
                                   mode,
                                   NA) }
  
  results <- rbindlist(list(results, temp_results_frame), use.names=TRUE)
  rm(temp_results_frame, k, linestring, url, json_data)
  print(paste("At", Sys.time(), "routing unique journey id", i, "was completed using cyclestreets"))
  unique_journeys[i,]$status <- 'cyclestreets'
} else { print(paste("At", Sys.time(), "routing unique journey id", i, "was NOT completed using cyclestreets mode, the error id will be logged"))
  error_ids[j,] <- i
  j <- j+1 }