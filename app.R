# Flexible Facility Locator Tool 
# Kara Todd, Freyja Brandel-Tanis, & Daniel Arias
# Takes set of vacant property candidates for new bus depot and identifies which candidate
# would minimize total deadhead travel time for a typical weekday schedule.

# install any missing packages
list.of.packages <- c("sf", "dodgr", "stringr", "dplyr", "leaflet", "RColorBrewer", "osmdata", "chron", "shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# load libraries
library(sf)
library(dodgr)
library(stringr)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(osmdata)
library(chron)
library(shiny)


##### UI #####
options(shiny.maxRequestSize = 50*1024^2)
ui <- fluidPage(
  titlePanel("Bus Depot Locator"),
  
  helpText(h5('Please note: The application can take 20-45 minutes to run.', br()), style = "color:black"),
  
  helpText(h5("Enter the average hourly bus operator wage for your agency.", br(), 
              "If nothing is entered, this value will default to $15/hr"), style = "color:black"),
  textInput(inputId = "wage.input",
            label = "Hourly driver wage:", value = "", 
            placeholder = "16.38"), 
  
  helpText(h5("Enter the per mile operating cost for your agency.", br(), 
              "If nothing is entered, this value will default to $9/mi"), style = "color:black"),
  textInput(inputId = "op_cost.input",
            label = "Per mile operating cost:", value = "",
            placeholder = "8.45"),
  
  
  # GTFS files
  helpText(br(), h4("Upload the GTFS files for your agency below. All files should have .txt extensions."), style = "color:black"),
  fileInput(inputId = "cal.input", label = "Upload GTFS calendar.txt file:", buttonLabel = "Browse..."),
  fileInput(inputId = "trips.input", label = "Upload GTFS trips.txt file:", buttonLabel = "Browse..."),
  fileInput(inputId = "routes.input", label = "Upload GTFS routes.txt file:", buttonLabel = "Browse..."),
  fileInput(inputId = "times.input", label = "Upload GTFS stop_times.txt file:", buttonLabel = "Browse..."),
  fileInput(inputId = "stops.input", label = "Upload GTFS stops.txt file:", buttonLabel = "Browse..."),
  
  # Depot data
  helpText(br(), h4("Finally, upload the .csv files with the coordinates of your existing depots and the vacant properties you want to consider."),
           style = "color:black"),
  helpText(h5("Upload the data on your existing depots. The file should have at least the following columns:", br(), 
              "1. A column with the ID #'s of your depots", br(),
              "2. A column titled 'lat' with latitude coordinates of your depots", br(), 
              "3. A column titled 'lon' with longitude coordinates of your depots"),
           style = "color:Black"),
  fileInput(inputId = "garages.input", label = "", buttonLabel = "Existing Garages..."),
  
  helpText(h5("Upload the data on the vacant properties you're considering. The file should have at least the following columns:", br(), 
              "1. A column with the name or address of the properties", br(),
              "2. A column with the ID #'s of the properties", br(),
              "3. A column titled 'lat' with latitude coordinates of the properties", br(), 
              "4. A column titled 'lon' with longitude coordinates of the properties"),
           style = "color:Black"),
  fileInput(inputId = "vac.input", label = "", 
            buttonLabel = "Vacant Properties..."),
  
  helpText(h4("Some residential roads might be necessary to reach first and last stops, but their widths and intersection characteristic may be difficult for full-size buses to navigate. Choose whether you would like to include them in your model."),
           style = "color:black"),
  helpText(h5(em("Removing residential roads may produce a model with slightly reduced accuracy. However, including residential roads may assume bus paths that are not physically usable. Running the model twice will provide a balance between these limitations.")),
           style = "color:black"),
  radioButtons(
    inputId = "resRoad",
    label = "Include Residential Roads?",
    choices = c(
      "Yes" = "y",
      "No" = "n"
      ),
    inline = T,
    selected = "n"
  ),
  
  actionButton(inputId = "submit", label = "Submit"),
  helpText(br()),
  
  
  # Outputs
  textOutput("winner_name.output"),
  textOutput("dh_savings.output"),
  textOutput("dh_cost_savings.output"),
  
  # output winning depot and vehicle matches
  textOutput("download_message.output"),
  uiOutput("downloadDataShow"),
  
  helpText(br()),
  
  # output results of other properties that were considered
  textOutput("download_message2.output"),
  uiOutput("downloadDataShow2"),
  
  helpText(br())
  
)


##### SERVER #####
server <- function(input, output){
  observeEvent(input$submit, { # only run when user clicks submit
    
    vac_prop <- input$vac.input
    garages <- input$garages.input
    calendar <- input$cal.input
    trips <- input$trips.input
    routes <- input$routes.input
    times <- input$times.input
    stops <- input$stops.input
    resChoice <- input$resRoad
    
    
    # return NULL if any file inputs are missing
    if(is.null(input$vac.input)) return(NULL)
    if(is.null(input$garages.input)) return(NULL)
    if(is.null(input$cal.input)) return(NULL)
    if(is.null(input$trips.input)) return(NULL)
    if(is.null(input$routes.input)) return(NULL)
    if(is.null(input$times.input)) return(NULL)
    if(is.null(input$stops.input)) return(NULL)
    
    
    # read in user inputs
    # network data
    vac_prop <- read.csv(vac_prop$datapath)
    garages <- read.csv(garages$datapath)
    
    
    # GTFS
    calendar <- read.delim(calendar$datapath, sep = ",")
    trips <- read.delim(trips$datapath, sep = "\t") 
    routes <- read.delim(routes$datapath, sep = "\t", stringsAsFactors = FALSE)
    times <- read.delim(times$datapath, sep = ",", stringsAsFactors = FALSE)
    stops <- read.delim(stops$datapath, sep = "\t", stringsAsFactors = FALSE)

    
    # cost inputs
    if(input$wage.input == ""){
      hourly_wage <- 16.38
      cat(file = stderr(), "Using default driver wage of $15/hr \n")                       # TRACING PROGRESS
    }else{
      hourly_wage <- as.numeric(input$wage.input)
    }
    if(input$op_cost.input == ""){
      op_cost_mi <- 8.45
      cat(file = stderr(), "Using default operating cost of $9/mi \n")                       # TRACING PROGRESS
    }else{
      op_cost_mi <- as.numeric(input$op_cost.input)
    }

    ##### clean & format data #####
    # eliminate rail routes                                                           
    rail_ids <- routes$route_id[which(!grepl("\\d", routes$route_short_name))]
    trips <- trips[ ! trips$route_id %in% rail_ids, ]
    
    # only want to calculate deadhead for typical weekday
    weekday_code <- as.numeric(calendar$service_id[which(calendar$wednesday == 1)])
    trips <- trips[trips$service_id == weekday_code , ]
    
    # join routes <> trips <> stop times <> stops
    bus_routes <- routes %>% filter(!route_id %in% rail_ids) # drop train & streetcar routes
    
    gtfs <- inner_join(bus_routes, trips, by = "route_id")
    gtfs <- inner_join(gtfs, times, by = "trip_id")
    gtfs <- inner_join(gtfs, stops, by = "stop_id")
    
    trip_first_last <- gtfs %>% group_by(route_id, trip_id) %>% summarize(first_stop = stop_id[which.min(stop_sequence)],
                                                                          last_stop = stop_id[which.max(stop_sequence)],
                                                                          first_lat = stop_lat[which.min(stop_sequence)],
                                                                          first_lon = stop_lon[which.min(stop_sequence)],
                                                                          last_lat = stop_lat[which.max(stop_sequence)],
                                                                          last_lon = stop_lon[which.max(stop_sequence)])
    
    
    first_stops <- trip_first_last %>% select(c("route_id", "trip_id", "first_stop", "first_lat", "first_lon"))
    colnames(first_stops) <- c("route_id", "trip_id", "stop_id", "lat", "lon")
    last_stops <- trip_first_last %>%  select(c("route_id", "trip_id", "last_stop", "last_lat", "last_lon"))
    colnames(last_stops) <- c("route_id", "trip_id", "stop_id", "lat", "lon")
    fl_stops <- rbind(first_stops, last_stops)
    fl_stops <- fl_stops %>% ungroup() %>%  select(c("stop_id", "lat", "lon"))
    fl_stops <- unique(fl_stops)
    
    #create spatial data for first and last stops
    fl_stop_locs <- st_as_sf(fl_stops, coords = c("lon", "lat"), crs = 4326)
    
    # vacant properties / depot candidates
    names(vac_prop)[1] <- "address"
    vac_prop$address <- as.character(vac_prop$address)
    
    
    ##### set up network #####
    box <- st_bbox(fl_stop_locs)
    # box <- matrix(c(box[1], box[3], box[2], box[4]), 2, 2)

    lon_min <- box[1]
    lon_max <- box[3]
    lat_min <- box[2]
    lat_max <- box[4]
    garage_lon_min <- min(garages$lon)
    garage_lon_max <- max(garages$lon)
    garage_lat_min <- min(garages$lat)
    garage_lat_max <- max(garages$lat)
    vac_lon_min <- min(vac_prop$lon)
    vac_lon_max <- max(vac_prop$lon)
    vac_lat_min <- min(vac_prop$lat)
    vac_lat_max <- max(vac_prop$lat)
    
    lon_min <- min(lon_min, garage_lon_min, vac_lon_min)
    lon_max <- max(lon_max, garage_lon_max, vac_lon_max)
    lat_min <- min(lat_min, garage_lat_min, vac_lat_min)
    lat_max <- max(lat_max, garage_lat_max, vac_lat_max)
    box_full <- matrix(c(lon_min, lon_max, lat_min, lat_max), 2, 2)
    
    withProgress(message = "Building street network", detail = "This may take a while...", value = 0, {
      setProgress(value = 0.3)
      system.time( #1179 seconds / ~20 mins
        streets <- dodgr_streetnet(bbox=box_full)
      )
      setProgress(value = 1)
    })

    cat(file = stderr(), "Network complete \n")                       # TRACING PROGRESS  
      
    if(resChoice=="n"){
    # remove residential streets to improve processing time
    streets$type_code <- ifelse((
      streets$highway == "motorway_link" | streets$highway == "tertiary_link" |
        streets$highway == "unclassified" | streets$highway == "primary" |
        streets$highway == "tertiary" | streets$highway == "motorway" | 
        streets$highway == "secondary" | streets$highway == "trunk_link" |
        streets$highway == "secondary_link" | streets$highway == "trunk" |
        streets$highway == "primary_link" | streets$highway == "road" | 
        streets$highway == "corridor") &
        !is.na(streets$highway), 1, 0)
    cat(file = stderr(), "Building network without residential roads \n")
    }else if (resChoice=="y"){
                                     streets$type_code <- ifelse((
                                       streets$highway == "motorway_link" | streets$highway == "tertiary_link" |
                                         streets$highway == "unclassified" | streets$highway == "primary" |
                                         streets$highway == "tertiary" | streets$highway == "motorway" |
                                         streets$highway == "secondary" | streets$highway == "trunk_link" |
                                         streets$highway == "secondary_link" | streets$highway == "trunk" |
                                         streets$highway == "primary_link" | streets$highway == "road" |
                                         streets$highway == "corridor" | streets$highway == "residential") &
                                         !is.na(streets$highway), 1, 0)
                                     
                                     cat(file = stderr(), "Building network with residential roads \n")
                                     }
    
    
    streets_fil <- streets[streets$type_code == 1, ]
    names(st_geometry(streets_fil)) = NULL
    
    # delete unnecessary variables in network
    streets_fil <- streets_fil %>% select(c("osm_id", "name", "highway", "width", "width.lanes", "geometry"))
    
    # apply weights to OSM network
    # distance in meters, time in seconds
      system.time( #38 seconds
        net <- weight_streetnet(streets_fil, wt_profile = "psv")
      )
      
    cat(file = stderr(), "Placing properties in the network \n")                            # TRACING PROGRESS  
      
    # properties are not vertices of the network, so we match them to the closest point
    verts <- dodgr_vertices(net)
    
    vac_prop$vert <- match_points_to_graph(verts, vac_prop[, c("lon", "lat")], connected = TRUE) 
    vac_prop$vert <- verts$id[vac_prop$vert]
    
    garages$vert <- match_points_to_graph(verts, garages[, c("lat", "lon")], connected = TRUE)
    garages$vert <- verts$id[garages$vert]
    
    cat(file = stderr(), "Properties are matched to network vertices \n")                       # TRACING PROGRESS
    
    # match first/last stops to vertices
    fl_stops$vert <- match_points_to_graph(verts, fl_stops[, c("lon", "lat")], connected = TRUE)
    fl_stops$vert <- verts$id[fl_stops$vert]
    fl_stops$vert[fl_stops$stop_id == 902145] <- fl_stops$vert[fl_stops$stop_id == 902144] # manually fixing North Lindbergh stop
    
    cat(file = stderr(), "Stops are matched to network vertices \n")                            # TRACING PROGRESS
    
    ##### create OD travel time & distance matrices #####
    net$from <- net$from_id
    net$to <- net$to_id
    
    # travel times & distances from existing garages to all first/last stops
    tt_garages <- dodgr_times(net, from = garages$vert, to = fl_stops$vert)
    tt_garages <- tt_garages/60
    rownames(tt_garages) <- garages$id #using ID as the garage identifier
    colnames(tt_garages) <- fl_stops$stop_id #using stop ID as the stop identifier
    
    dist_garages <- dodgr_distances(net, from = garages$vert, to = fl_stops$vert, shortest = FALSE)
    dist_garages <- dist_garages/1609
    rownames(dist_garages) <- garages$id
    colnames(dist_garages) <- fl_stops$stop_id
    
    # travel times & distances from vacant properties to all first/last stops
    tt_vac <- dodgr_times(net, from = vac_prop$vert, to = fl_stops$vert)
    tt_vac <- tt_vac/60 # convert to minutes
    rownames(tt_vac) <- vac_prop$id
    colnames(tt_vac) <- fl_stops$stop_id
    
    dist_vac <- dodgr_distances(net, from = vac_prop$vert, to = fl_stops$vert, shortest = FALSE)
    dist_vac <- dist_vac/1609 # convert to miles
    rownames(dist_vac) <- vac_prop$id
    colnames(dist_vac) <- fl_stops$stop_id
    
    
    cat(file = stderr(), "OD matrices complete \n")                            # TRACING PROGRESS
    
    ##### estimate number of deadhead trips to each route start/end#####
    # join trip start times to trip IDs
    trip_times <- left_join(trips, times[times$stop_sequence == 1,], by = "trip_id")
    trip_times <- trip_times[trip_times$service_id == weekday_code , ]
    trip_times$arrival_time <- as.character(trip_times$arrival_time)
    
    # recode trips times after midnight (last trip of day at 2:05 AM)
    trip_times$arrival_time <- ifelse(grepl("24:\\d\\d:\\d\\d",trip_times$arrival_time), gsub("24:", "00:", trip_times$arrival_time),
                                      ifelse(grepl("25:\\d\\d:\\d\\d",trip_times$arrival_time), gsub("25:", "01:", trip_times$arrival_time),
                                             ifelse(grepl("26:\\d\\d:\\d\\d",trip_times$arrival_time), gsub("26:", "02:", trip_times$arrival_time),
                                                    trip_times$arrival_time)))
    trip_times$arrival_time <- as.times(trip_times$arrival_time)
    
    # identify which (if any) vehicles make midday deadhead trips
    trip_times <- trip_times %>% group_by(block_id)
    trip_times <- trip_times %>% arrange(arrival_time, .by_group = TRUE)
    
    deadhead <- trip_times %>% group_by(block_id) %>%  
      summarize(first_trip = min(arrival_time[which(arrival_time > "03:00:00")]),
                # check if last trip occurred after midnight
                last_trip = as.times(ifelse(length(which(arrival_time < "03:00:00")) == 0,
                                            max(arrival_time),
                                            max(arrival_time[which(arrival_time < "03:00:00")]))),
                first_id = trip_id[which(arrival_time > "03:00:00")][which.min(arrival_time[which(arrival_time > "03:00:00")])],
                last_id =  trip_id[which(arrival_time == last_trip)],
                midday_dh = ifelse((length(which(arrival_time > "11:00:00" & arrival_time < "15:00:00")) == 0) & 
                                     (first_trip < "11:00:00" & last_trip > "15:00:00"), TRUE, FALSE)) 
    
    # duplicate rows with midday deadhead and identify relevant routes
    midday_dh_ids <- deadhead$block_id[which(deadhead$midday_dh == TRUE)]
    
    if(length(midday_dh_ids) != 0){
      deadhead2 <- trip_times[ trip_times$block_id %in% midday_dh_ids, ] %>% group_by(block_id) %>% 
        summarize(first_trip = min(arrival_time[which(arrival_time > "15:00:00")]), #second outbound deadhead trip
                  last_trip = max(arrival_time[which(arrival_time < "11:00:00")]),  #first inbound deadhead trip
                  first_id = trip_id[which(arrival_time == first_trip)],
                  last_id = trip_id[which(arrival_time == last_trip)],
                  midday_dh = TRUE)
      
      deadhead <- rbind(deadhead, deadhead2)
    }
    
    # add first and last stop id's based on trip_id
    deadhead <- left_join(deadhead, first_stops, by = c("first_id" = "trip_id"))
    deadhead$first_rt_id <- deadhead$route_id
    deadhead$first_stop_id <- deadhead$stop_id
    deadhead <- deadhead %>% select(-c(route_id, stop_id, lat, lon))
    
    deadhead <- left_join(deadhead, last_stops, by = c("last_id" = "trip_id"))
    deadhead$last_rt_id <- deadhead$route_id
    deadhead$last_stop_id <- deadhead$stop_id
    deadhead <- deadhead %>% select(-c(route_id, stop_id, lat, lon))
    
    cat(file = stderr(), "Deadhead data assembled \n")                            # TRACING PROGRESS
    
    ##### calculate deadhead travel times for each vehicle #####
    # transpose travel time and distance matrices
    tt_garages <- t(tt_garages)
    tt_vac <- t(tt_vac)
    dist_garages <- t(dist_garages)
    dist_vac <- t(dist_vac)
    
    # join travel times to stop first and last stops
    tt_garages <- as.data.frame(tt_garages)
    tt_garages$stop_id <- as.numeric(rownames(tt_garages))
    
    tt_vac <- as.data.frame(tt_vac)
    tt_vac$stop_id <- as.numeric(rownames(tt_vac))
    
    
    # create 4 OD travel time matrices -- 2 pull-out (from garages & candidate sites), 2 pull-in (from garages & candidate sites)
    ## pull-out deadhead times
    dh_garages_out <- inner_join(deadhead, tt_garages, by = c("first_stop_id" = "stop_id"))
    dh_vac_out <- inner_join(deadhead, tt_vac, by = c("first_stop_id" = "stop_id"))
    
    ## pull-in deadhead times
    dh_garages_in <- inner_join(deadhead, tt_garages, by = c("last_stop_id" = "stop_id"))
    dh_vac_in <- inner_join(deadhead, tt_vac, by = c("last_stop_id" = "stop_id"))
    
    
    ### calculate current deadhead time ###                                           
    find_assignment <- function(df){
      search_cols <- which(grepl("\\d", colnames(df)))
      search <- df[,search_cols]
      match <- apply(search, 1, which.min) 
      match <- match + min(search_cols) - 1
      assignment <- colnames(df)[match]
      assignment
    }
    
    # add pull-out and pull-in times for round trip deadhead
    dh_garages_round <- dh_garages_out
    dh_garages_round[ ,which(grepl("\\d", colnames(dh_garages_round)))] <- dh_garages_round[ ,which(grepl("\\d", colnames(dh_garages_round)))] +
      dh_garages_in[ ,which(grepl("\\d", colnames(dh_garages_in)))]
    
    # identify garage with min total deadhead time and match vehicle to it
    dh_garages_round <- na.omit(dh_garages_round)
    dh_garages_round$min <- do.call(pmin, dh_garages_round[ ,which(grepl("\\d", colnames(dh_garages_round)))])
    dh_garages_round$depot_match <- find_assignment(dh_garages_round)
    
    # add up to find current total deadhead
    current_dh_time <- sum(dh_garages_round$min, na.rm = TRUE)
    current_dh_time <- current_dh_time/60 #convert to hours
    
    
    ### compare current deadhead time to time to each vacant property ###
    # add pull-out and pull-in times for round trip deadhead
    dh_vac_round <- dh_vac_out
    dh_vac_round[ ,which(grepl("\\d", colnames(dh_vac_round)))] <- dh_vac_round[ ,which(grepl("\\d", colnames(dh_vac_round)))] +
      dh_vac_in[ ,which(grepl("\\d", colnames(dh_vac_in)))]
    
    # create data frame to compare each vacant property to existing scenario
    withProgress(message = "Choosing the best depot location...", value = 0, {
      
      dh_comp <- dh_vac_round
      dh_comp <- na.omit(dh_comp)
      dh_comp$depot_match <- paste0("g", dh_garages_round$depot_match)
      vac_cols <- which(grepl("\\d", colnames(dh_comp)))
      
      setProgress(value = 1/15)
      
      for (i in min(vac_cols) : max(vac_cols)){
        # assign trip to depot candidate if TT < current TT
        dh_comp$temp <- ifelse(dh_comp[,i] < dh_garages_round$min, paste0("v", colnames(dh_comp)[i]), dh_comp$depot_match)
        # store matches for each candidate site scenario
        new_name <- paste0("match_v", colnames(dh_comp)[i])
        dh_comp <- rename(dh_comp, !! new_name := temp)
        # update travel time for that candidate site as needed
        indices <- which(dh_comp[,i] > dh_garages_round$min)
        dh_comp[indices, i] <- dh_garages_round$min[indices]
        rm(new_name)
        
        incProgress(amount = 1/15)
      }
    })
    
    ##### identify winning property and calculate savings #####
    # total the pull-out times under each candidate site scenario
    dh_sums <- colSums(dh_comp[, vac_cols])
    dh_sums <- dh_sums/60 #change to hours
    
    # create dataframe with all property ID's and the total deadhead that would result
    props_considered <- vac_prop
    props_considered$total_dh_hrs <- dh_sums
    
    # find which vacant property minimizes deadhead time
    winner <- which.min(dh_sums)
    winner <- ifelse(min(dh_sums) < current_dh_time, winner, "None")
    new_dh_time <- min(dh_sums) 
    
    cat(file = stderr(), "Winning property identified \n")                            # TRACING PROGRESS
    
    # calculate current DH distance
    # create 4 OD DISTANCE matrices -- 2 pull-out (from garages & candidate sites), 2 pull-in (from garages & candidate sites)
    dist_garages <- as.data.frame(dist_garages)
    dist_garages$stop_id <- as.numeric(rownames(dist_garages))
    
    dist_vac <- as.data.frame(dist_vac)
    dist_vac$stop_id <- as.numeric(rownames(dist_vac))
    
    ## pull-out deadhead distances
    dh_dist_garages_out <- inner_join(deadhead, dist_garages, by = c("first_stop_id" = "stop_id"))
    dh_dist_vac_out <- inner_join(deadhead, dist_vac, by = c("first_stop_id" = "stop_id"))
    
    ## pull-in deadhead distances
    dh_dist_garages_in <- inner_join(deadhead, dist_garages, by = c("last_stop_id" = "stop_id"))
    dh_dist_vac_in <- inner_join(deadhead, dist_vac, by = c("last_stop_id" = "stop_id"))
    
    ## round trip distances
    dh_dist_garages_round <- dh_dist_garages_out
    dh_dist_garages_round[ ,which(grepl("\\d", colnames(dh_dist_garages_round)))] <- dh_dist_garages_round[ ,which(grepl("\\d", colnames(dh_dist_garages_round)))] +
      dh_dist_garages_in[ ,which(grepl("\\d", colnames(dh_dist_garages_in)))]
    
    dh_dist_vac_round <- dh_dist_vac_out
    dh_dist_vac_round[ ,which(grepl("\\d", colnames(dh_dist_vac_round)))] <- dh_dist_vac_round[ ,which(grepl("\\d", colnames(dh_dist_vac_round)))] +
      dh_dist_vac_in[ ,which(grepl("\\d", colnames(dh_dist_vac_in)))]
    
    # find round trip distance to current garage match
    dh_dist_garages_round$depot_match <- dh_garages_round$depot_match
    dh_dist_garages_round$match_dist <- NA
    
    withProgress(message = "Calculating distance...", value = 0, {
      system.time( 
        for(i in 1:nrow(dh_dist_garages_round)){
          match <- match(dh_dist_garages_round$depot_match, colnames(dh_dist_garages_round))
          dh_dist_garages_round$match_dist[i] <- dh_dist_garages_round[i, match[i]] 
          
          incProgress(1/600)
        }
      )
    }) 
    
    
    dh_dist_garages_round$match_dist <- as.numeric(dh_dist_garages_round$match_dist)
    current_dh_dist <- sum(dh_dist_garages_round$match_dist)
    
    
    # calculate round trip distance to final matches
    winner_data <- dh_comp[, c(which(colnames(dh_comp) == winner), which(colnames(dh_comp) == paste0("match_v", winner)))]
    names(winner_data) <- c("final_times", "final_matches")
    
    dh_dist_final <- cbind(dh_dist_garages_round, winner_data)
    dh_dist_final <- dh_dist_final %>% select(-c(depot_match, match_dist, final_times))
    dh_dist_final <- cbind(dh_dist_final, dh_dist_vac_round[, colnames(dh_dist_vac_round)==winner])
    dh_dist_final$final_matches <- gsub("\\D", "", dh_dist_final$final_matches)
    dh_dist_final$final_dist <- NA
    
    system.time( 
      for(i in 1:nrow(dh_dist_final)){
        match <- match(dh_dist_final$final_matches, colnames(dh_dist_final))
        dh_dist_final$final_dist[i] <- dh_dist_final[i, match[i]] 
      }
    )
    
    dh_dist_final$final_dist <- as.numeric(dh_dist_final$final_dist)
    new_dh_dist <- sum(dh_dist_final$final_dist)
    
    cat(file = stderr(), "Final distances calculated \n")                            # TRACING PROGRESS
    
    ##### print final output #####
    current_matches <- cbind(dh_comp[,1:10], dh_comp$depot_match)
    names(current_matches)[11] <- "depot_match"
    
    final_match_data <- cbind(dh_comp[, 1:10], dh_garages_round$depot_match, dh_garages_round$min, dh_dist_garages_round$match_dist,
                              winner_data$final_matches)
    colnames(final_match_data)[1:14] <- c("block_id", "first_trip_time", "last_trip_time", 
                                          "first_trip_id", "last_trip_id", "midday_dh",
                                          "first_rt_id", "first_stop_id", "last_rt_id", "last_stop_id", 
                                          "existing_depot_match", "existing_dh_mins", "existing_dh_miles", "final_match")
                                           
    
    final_match_data$reassigned <- ifelse(paste0("g", final_match_data$existing_depot_match) == final_match_data$final_match, FALSE, TRUE)
    
    final_match_data <- cbind(final_match_data, winner_data$final_times, dh_dist_final$final_dist)
    colnames(final_match_data)[16:17] <- c("final_dh_mins", "final_dh_miles")
    
    dh_savings_time <- round(current_dh_time - new_dh_time, 2) 
    dh_savings_dist <- round(current_dh_dist - new_dh_dist, 2)
    
    wage_savings <- round(hourly_wage*dh_savings_time, 2)
    op_cost_savings <- round(op_cost_mi*dh_savings_dist, 2)
    total_cost_savings <- wage_savings+op_cost_savings
    
    winner_name <- vac_prop[winner,1]
    
    cat(file = stderr(), "Printing outputs \n")                             # TRACING PROGRESS
    
    # attach to output dataframe
    output$winner_name.output <- renderText(paste0("The new depot should be located at: ", winner_name))
    output$dh_savings.output <- renderText(paste0("This would save ~", dh_savings_time, " hours or ", dh_savings_dist,
                                                  " miles of deadhead travel per weekday."))
    
    output$dh_cost_savings.output <- renderText(paste0("This translates to saving about $", wage_savings, " in wage costs and $", 
                                                       op_cost_savings, " in operating costs per weekday."))
    
    output$downloadData <- downloadHandler(filename = "Final Bus_Depot Matches.csv", 
                                           content = function(file) {
                                             write.csv(final_match_data, file)
                                           })
    output$download_message.output <- renderText("You can download the winning bus-depot output data here:")
    
    output$downloadDataShow <- renderUI({
      req(final_match_data)
      downloadButton("downloadData")
    }) 
    
    output$downloadData2 <- downloadHandler(filename = "Properties Considered.csv", 
                                           content = function(file) {
                                             write.csv(props_considered, file)
                                           })
    
    output$download_message2.output <- renderText("You can download the other properties' results here:")
    
    output$downloadDataShow2 <- renderUI({
      req(props_considered)
      downloadButton("downloadData2")
    }) 
    
  })
} 


shinyApp(ui = ui, server = server)


