# Return a list of the species that are flying at the moment
#
# _Input_: Location; taxa; date (defaults to current date)
# 
# _Output_: List of species currently flying; full dataframe with lat/long coordinates
# 
# _Metadata_: Date used; taxa used; location



# data frame of the flight period loaded and explained below the function
# head(fly_df)
# 
# library(lubridate) # possible to do it without lubridate, but it's nicer with


####   I don't like that this function takes lat_lon and the other (filter_distance) takes lon_lat   ####


filter_phenology <- function(date = Sys.Date(), # needs to be in format "YYYY-MM-DD", default is current day
                             taxa = 'any', # must be one of 'butterflies', 'moths', 'orthoptera'
                             lat_long = c(51.602436, -1.110557)){ # can be a 1 latitude number, a  lat_lon (although only looks at latitude), or NA
  
  taxa <- tolower(taxa)
  
  if(!all(taxa %in% c('butterflies', 'moths', 'orthoptera'))){
    
    stop("'taxa values are limited to:'butterflies', 'moths', 'orthoptera'")
    
  }
  
  # convert system date to julian date
  jd_date <- yday(ymd(date))
  
  # Have option not to include location, so can see all species flying
  # at a given time of year
  if(!is.na(lat_long[1])){
    
    # get the latitude as the distribution will vary more by latitude than longitude
    # will need to change this because latitude will affect when a species is seen
    # could also include longitude quite easily here - is this necessary?
    lat <- lat_long[1]
    
    # get a list of species that are active at the time of using the function
    # and at the latitude of the location
    fly_sp_list <- fly_df$species[fly_df$lwr < jd_date & 
                                    fly_df$upr > jd_date &
                                    fly_df$lwr_lat < lat &
                                    fly_df$upr_lat > lat]
    
  } else if(is.na(lat_long[1])) {
    fly_sp_list <- fly_df$species[fly_df$lwr < jd_date & 
                                    fly_df$upr > jd_date]
  }
  
  
  out_df <- fly_df[fly_df$species %in% fly_sp_list,]
  colnames(out_df) <- c("species", "start", "end", "lower_lat", "upper_lat", "taxa")
  
  # get taxa of interest
  out_df <- out_df[out_df$taxa == taxa,]
  
  # return a list specifying the date used, the list of species and the data
  # frame with the flight periods (in Julian Day) and latitude values for 
  # each of the species
  return(list(date=date, species = fly_sp_list, full_df = out_df))
  
  # # return the species that are flying today
  # # start and end are given as Julian day
  # return(data.frame(species = 'Polyommatus bellargus', 
  #                   start = 135,
  #                   end = 273))
  
}

filter_phenology(taxa = 'moths',
                 lat_long = NA)




# couldn't be bothered to load files through the object store so just creating it within R (from my computer)
# These are the day-flying moths and their flight periods
# this is the 5% quantile and the 95% quantile
species = c(
  'Adscita geryon',
  'Adscita statices',
  'Anarta melanopa',
  'Anarta myrtilli',
  'Archiearis parthenias',
  'Chiasmia clathrata',
  'Ematurga atomaria',
  'Epirrhoe tristata',
  'Euclidia glyphica',
  'Euclidia mi',
  'Glacies coracina',
  'Heliothis maritima',
  'Heliothis viriplaca',
  'Hemaris fuciformis',
  'Hemaris tityus',
  'Idaea muricata',
  'Jordanita globulariae',
  'Lycia lapponaria',
  'Lycia zonaria',
  'Macaria brunneata',
  'Macaria carbonaria',
  'Minoa murinata',
  'Odezia atrata',
  'Orgyia antiqua',
  'Orgyia recens',
  'Panemeria tenebrata',
  'Parasemia plantaginis',
  'Perconia strigillaria',
  'Perizoma albulata',
  'Photedes captiuncula',
  'Phytometra viridaria',
  'Pseudopanthera macularia',
  'Rheumaptera hastata',
  'Scotopteryx bipunctaria',
  'Siona lineata',
  'Tyria jacobaeae',
  'Tyta luctuosa',
  'Zygaena exulans',
  'Zygaena filipendulae',
  'Zygaena lonicerae',
  'Zygaena loti',
  'Zygaena purpuralis',
  'Zygaena trifolii')

lwr = c(
  140,
  144,
  133,
  136,
  73,
  142,
  116,
  153.5,
  128,
  126,
  155,
  183.85,
  153,
  124.1,
  121,
  168,
  153,
  84.6,
  83,
  151,
  115.3,
  129.1,
  153,
  176,
  137.7,
  124,
  138.4,
  145,
  137.9,
  161.7,
  122.4,
  121.1,
  139,
  183,
  152,
  134,
  132,
  186,
  164,
  157,
  154,
  163,
  148.5)

upr = c(
  186.8,
  204,
  172.4,
  249,
  112,
  245,
  186,
  223,
  178,
  174,
  196,
  228.8,
  230.65,
  207.8,
  193,
  208,
  178,
  130,
  182,
  197.2,
  143,
  220,
  202.15,
  285,
  198.25,
  166,
  206.8,
  185.25,
  209,
  212.2,
  219,
  168.9,
  231,
  239.55,
  175.7,
  209,
  221,
  186,
  224,
  209,
  189,
  192,
  211)

lwr_lat = c(
  50.9408378188731,
  50.9683886826588,
  56.4335189132171,
  50.672301172322,
  50.6011561857289,
  51.2572798676663,
  50.7938048101375,
  51.6985977962609,
  50.6269154727518,
  50.7069561377728,
  56.5442504311769,
  50.8403482234039,
  52.0353611162405,
  50.7328019920851,
  50.6249750956207,
  50.6595405713966,
  50.84114,
  56.1346850086241,
  53.9972300174766,
  50.6255639590124,
  57.0420337191318,
  50.9420289312162,
  51.2396945212773,
  50.7563606609805,
  52.7387108610979,
  50.7242471585334,
  50.797251708325,
  50.5976231757224,
  51.00236,
  53.0707457415988,
  50.5924117566129,
  50.3542705541458,
  51.062,
  50.5418082232196,
  51.1549524064296,
  50.711254,
  50.5194636761311,
  56.986697,
  50.6269326587562,
  50.955147728802,
  56.3588771798669,
  56.3497496606852,
  50.34135)

upr_lat = c(
  54.3042594999384,
  55.0114560032112,
  57.8297716617348,
  56.3981119197686,
  57.1166431566198,
  55.8602876975855,
  57.4872681191911,
  58.1855017916415,
  52.99207,
  54.9799983770893,
  57.6782834902708,
  51.0817133140729,
  52.9864006097614,
  53.25109,
  57.5858619136954,
  54.2311641978566,
  51.1448833021021,
  57.5846906026929,
  57.6262768344017,
  57.227400813914,
  57.3919880089379,
  52.353406,
  57.1078296466888,
  54.6778540784086,
  53.6315501911738,
  54.2000300445849,
  57.9415508279472,
  54.8722925,
  57.3849831707582,
  54.4872523463969,
  56.7298805884908,
  56.1638200690199,
  58.158384,
  52.5236854669436,
  51.2258557742978,
  54.6293196615766,
  52.634661404929,
  56.991533,
  56.4375,
  55.2787269018799,
  56.6329782400316,
  56.8725412446927,
  52.8627944507519)


fly_df <- data.frame(species, lwr, upr, lwr_lat, upr_lat, taxa = 'moths')


