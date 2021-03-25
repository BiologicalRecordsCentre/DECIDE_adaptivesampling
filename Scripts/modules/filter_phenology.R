# Return a list of the species that are flying at the moment
#
# _Input_: Location; taxa; date (defaults to current date)
# 
# _Output_: List of species currently flying; full dataframe with lat/long coordinates
# 
# _Metadata_: Date used; taxa used; location



# load data frame of 50% flight periods
fly_df <- read.csv('Data/taxonomy/25_75_flight_periods_inverts.csv')

head(fly_df)

library(lubridate) # possible to do it without lubridate, but it's nicer with


####   I don't like that this function takes lat_lon and the other (filter_distance) takes lon_lat   ####


filter_phenology <- function(date = Sys.Date(), # needs to be in format "YYYY-MM-DD", default is current day
                             taxa_list = 'any', # must be one or multiple of 'butterfly', 'moth', 'orthoptera'
                             location = c(-1.110557, 51.602436)){ # can be a 1 latitude number, a  lat_lon (although only looks at latitude), or NA
  
  taxa_list <- tolower(taxa_list)
  
  if(!all(taxa_list %in% c('butterfly', 'moth', 'orthoptera'))){
    
    stop("'taxa values are limited to:'butterfly', 'moth', 'orthoptera'")
    
  }
  
  # convert system date to julian date
  jd_date <- yday(ymd(date))
  
  # Have option not to include location, so can see all species flying
  # at a given time of year
  if(!is.na(location[2])){
    
    # get the latitude as the distribution will vary more by latitude than longitude
    # will need to change this because latitude will affect when a species is seen
    # could also include longitude quite easily here - is this necessary?
    rnd_lat <- round(location[2])
    
    # get a list of species that are active at the time of using the function
    # and at the latitude of the location
    fly_sp_list <- fly_df$species[fly_df$lwr <= jd_date & 
                                    fly_df$upr >= jd_date &
                                    fly_df$lat == rnd_lat]
    
  } else if(is.na(location[2])) {
    
    fly_sp_list <- fly_df$species[fly_df$lwr < jd_date & 
                                    fly_df$upr > jd_date]
    
    # if no latitude specified, return all latitudes
    rnd_lat <- unique(fly_df$lat)
    
    
  }
  
  
  # subset the data frame and manipulate into output format
  (out_df <- fly_df[fly_df$species %in% fly_sp_list,] %>%  
      dplyr::select(taxa, species, lat, lwr, upr) %>% 
      filter(lat %in% rnd_lat &
               taxa %in% taxa_list) %>% 
      mutate(flight_period_lower = as_date(lwr, origin = dmy(paste0("1/1/", year(date)))),
             flight_period_upper = as_date(upr, origin = dmy(paste0("1/1/", year(date))))) %>% 
      rename(latitude = lat,
             day_lower_flight_period = lwr,
             day_upper_flight_period = upr))
  
  
  # return a list specifying the date used, the list of species and the data
  # frame with the flight periods (in Julian Day) and latitude values for 
  # each of the species
  return(list(date=date, species = fly_sp_list, full_df = out_df))
  
}


####    Testing    ####
filter_phenology(taxa_list = "moth",
                 location = c(-2, 60))

