choose_routing_api <- function(stage_mode, start_lat, start_lon, end_lat, end_lon) {
  
  if (stage_mode == 'car')    { source('routing/car_routing_v2.R')}
  if (stage_mode == 'walk')   { source('routing/walk_routing.R')}
  if (stage_mode == 'tube')   { source('routing/tube_routing.R')}
  if (stage_mode == 'bus')    { source('routing/bus_routing.R')}
  if (stage_mode == 'train')  { source('routing/train_routing.R')}
  if (stage_mode == 'cycle')  { source('routing/cycle_routing.R')}
  
  ## This bit needs work to know if it's outside London or not.
  if (stage_mode == 'walk')   { source('routing/walk_outside_london_routing.R')}
  
 # return(results)
  
  
}