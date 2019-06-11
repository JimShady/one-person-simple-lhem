format_ltds_datetimes <- function(unformatted_time, survey_date) {
  
  if (nchar(unformatted_time) == 4) {  

    formatted_date <- paste0(survey_date, " ",
                            paste0(substr(unformatted_time,1,2), ":",
                                    substr(unformatted_time,nchar(unformatted_time)-1,nchar(unformatted_time)))
                                  )}
  
  if (nchar(unformatted_time) == 3 & unformatted_time >= 400) {

    formatted_date <- paste0(survey_date, " ", 
                              paste0("0",substr(unformatted_time,1,1),  ":",
                                      substr(unformatted_time,nchar(unformatted_time)-1,nchar(unformatted_time)))
                                  )}
  
  if (nchar(unformatted_time) == 3 & unformatted_time < 400) {  
 
    formatted_date <- paste0(survey_date + 1, " ",
                              paste0("0", substr(unformatted_time,1,1), ":", 
                                      substr(unformatted_time,nchar(unformatted_time)-1,nchar(unformatted_time)))
                                  )}
  
  if (nchar(unformatted_time) == 2) {  
   
    formatted_date <- paste0(survey_date + 1, " ",
                              paste0("00", ":",
                                      substr(unformatted_time,nchar(unformatted_time)-1,nchar(unformatted_time)))
                                  )}
  
  if (nchar(unformatted_time) == 1) {  
  
    formatted_date <- paste0(survey_date + 1, " ", "00", ":0",
                            unformatted_time)
                                    }
  
  return(formatted_date)

}


