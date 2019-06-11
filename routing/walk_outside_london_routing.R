json_data <- google_directions(origin = paste(start_lat, ",", start_lon, sep="") ,
                  destination = paste(end_lat, ",", end_lon, sep=""),
                  key = google_api_key,
                  mode = "walking",
                  simplify = T)

if (json_data$status == 'OK') {
  
  temp_results_frame <- data.frame(unique_id = i,
                                   lat = decodePolyline(json_data$routes$overview_polyline$points)$lat,
                                   lon = decodePolyline(json_data$routes$overview_polyline$points)$lon,
                                   mode = 'Walk',
                                   line = NA,
                                   stringsAsFactors = FALSE)
  
  results <- rbindlist(list(results, temp_results_frame), use.names=TRUE)
  
  rm(temp_results_frame, json_data)
  print(paste("At", Sys.time(), "routing unique journey id", i, "was completed using google walking mode"))
  unique_journeys[i,]$status <- 'google'
  Sys.sleep(1) # limit of 2500 a day .... so this keeps it roughly in touch while cathcing up.
  
} else {
  print(paste("At", Sys.time(), "routing unique journey id", i, "was NOT completed using google walking mode, the error id will be logged"))
  error_ids[j,] <- i
  j <- j+1
}