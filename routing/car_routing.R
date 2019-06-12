mapzen_api <- 'mapzen-tY12PF1'

## https://mapzen.com/documentation/mobility/turn-by-turn/api-reference/

if (nchar(start_lat) < 5) { start_lat <- paste(start_lat, '100', sep="") }
if (nchar(start_lon) < 5 | round(start_lon,2) == 0.08) { start_lon <- as.numeric(paste(round(start_lon,2), '100', sep="")) }
if (nchar(end_lat) < 5) { end_lat <- paste(end_lat, '100', sep="") }
if (nchar(end_lon) < 5 | round(end_lon,2) == 0.08) { end_lon <- as.numeric(paste(round(end_lon,2), '100', sep="")) }

url <- paste('http://valhalla.mapzen.com/route?json={"locations":[{"lat":',
                start_lat,
                ',"lon":',
                start_lon,
                '},{"lat":',
                end_lat, 
                ',"lon":',
                end_lon,
                '}],"costing":"auto"}&api_key=mapzen-tY12PF1',
                sep="")

received <- URLencode(url)

temp <- try(fromJSON(received, simplify = FALSE))

if (class(temp) != 'try-error') {

json_data <- fromJSON(received, simplify = FALSE)

if (json_data$trip$status_message == "Found route between points") {
  print(paste("At", Sys.time(), "routing unique journey id", i, "was completed using mapzen"))
  temp_results_frame <- decodeLine(json_data$trip$legs[[1]]$shape)
  temp_results_frame$unique_id <- as.numeric(as.character(temp_results_frame$unique_id))
  temp_results_frame$lon <- as.numeric(as.character(temp_results_frame$lon))
  temp_results_frame$lat <- as.numeric(as.character(temp_results_frame$lat))
  temp_results_frame$mode <- as.character(temp_results_frame$mode)
  temp_results_frame$line <- as.character(temp_results_frame$line)
  results <- rbindlist(list(results, temp_results_frame))
  rm(url, received, temp, json_data, temp_results_frame, mapzen_api)
  unique_journeys[i,]$status <- 'mapzen'
} else {
  print(paste("At", Sys.time(), "routing unique journey id ", i, "was NOT completed using mapzen, the error id will be logged"))
  error_ids[j,] <- i
  j <- j+1
}
} else
{
  rm(temp)
  print(paste("At", Sys.time(), "routing unique journey id ", i, "was NOT completed using mapzen, the error id will be logged"))
  error_ids[j,] <- i
  j <- j+1
}

Sys.sleep(0.5)
rm(mapzen_api)