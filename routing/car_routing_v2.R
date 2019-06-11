print(getwd())

source('../google_api_key.R')
source('functions/decodeline.R')

json_data <- google_directions(origin = paste(start_lat, ",", start_lon, sep="") ,
                               destination = paste(end_lat, ",", end_lon, sep=""),
                               key = google_api_key,
                               mode = "driving",
                               simplify = T)

if (json_data$status == 'OK') {
  
  route <- data.frame( x = decodePolyline(json_data$routes$overview_polyline$points)$lon,
                       y = decodePolyline(json_data$routes$overview_polyline$points)$lat,
                       mode = 'car',
                       line = NA,
                       stringsAsFactors = FALSE)
  
  rm(json_data)
  
} else {
  print('failed')
}