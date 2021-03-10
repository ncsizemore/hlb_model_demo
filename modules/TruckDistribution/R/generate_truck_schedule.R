.generate_truck_schedule <- function(removed_citrus, harvest_season, sim){
  # returns the dates of next year's harvest season, and schedule for the days on which
  # trucks will run in the current season in the form of a list which contains, for each harvest
  # location, the days after the start of the current season that trucks will run (eg. 4 indicates a truck
  # running on the 4th day after the start of the season)
  
  # dates are sampled from the trapezoidal distribution specified by the season dates
  #browser()
  harvest_locs_above_thresh <- which(sim$maps$citrus_harvesting_locations[] > 10)

  # temporary reversion:
  # citrus <- readRDS("legacy/crops_04.rds")
  # agged <- velox::velox(citrus)
  # agged$aggregate(factor = c(8,8), aggtype = 'sum')
  # agged <- agged$as.RasterLayer()
  # harvest_locs_above_thresh <- which(agged[] > 10)

  trucks_needed <- 40 * sim$maps$citrus_harvesting_locations * P(sim, "TruckDistribution", "trucks_per_10_acres")/10
  
  truck_schedule <- map(seq_along(harvest_locs_above_thresh), 
                        ~ sort(as.integer(
                          rtrapezoid(n = as.integer(trucks_needed[harvest_locs_above_thresh[.x]]), 
                                     min=0, 
                                     mode1 = as.numeric(harvest_season[2] - harvest_season[1]), 
                                     mode2 = as.numeric(harvest_season[3] - harvest_season[1]), 
                                     max = as.numeric(harvest_season[4] - harvest_season[1]), 
                                     n1 = 2, n3 = 2, alpha = 1)
                          ))
                        )
  
  return(truck_schedule)
  
}

