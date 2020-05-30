# todo: extract bike information
# <place uid="11464983" lat="50.94160996778235" lng="6.972284317016602" name="Charles-de-Gaulle Platz" spot="1" number="4856" bikes="1" booked_bikes="0" bike_racks="12" free_racks="11" terminal_type="stele" bike_numbers="22901" bike_types="{"17":1}" place_type="14" rack_locks="1">
#   <bike number="22901" bike_type="17" lock_types="fork_lock" active="1" state="ok" electric_lock="1" boardcomputer="27270" pedelec_battery=""/>
#     </place>

#'@author christian  bitter
#'@name bike_position
#'@title Cologne City - Bike stations
#'@description Returns the next bike station information
#'@param as_spatial boolean indicating if the return value should be an sf or not (default True)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes describing the
#'next bike station and bikes at the station if any.
#'@examples
#'sp_sf <- bike_position()
#'@export
bike_position <- function(as_spatial = T) {
  url <- "http://api.nextbike.net/maps/nextbike-live.xml?city=14";
  r <- httr::GET(url = url);
  # json_content <- rOCologneKVB::base_request(url = url, rq_type = "xml");
  content <- httr::content(r, encoding = "UTF-8", as = "text");

  pg <- xml2::read_xml(content);
  # todo turn into apply
  recs <- xml2::xml_find_all(pg, "//place");
  f <- recs[[1]]
  # for each location we can have a number of bikes

  station_attrib <- c("uid", "lat", "lng", "name", "spot", "number", "bikes", "bike",
                      "booked_bikes", "bike_racks", "free_racks", "terminal_type", "bike_numbers", "bike_types", "place_type");
  m_station <- matrix(NA_character_, nrow = length(recs), ncol = length(station_attrib));
  colnames(m_station) <- station_attrib;

  bike_attribs <- c("station_uuid", "number", "bike_type", "lock_types", "active", "state", "electric_lock", "boardcomputer", "pedelec_battery");
  m_bikes      <- matrix(NA_character_, nrow = length(recs), ncol = length(bike_attribs));
  colnames(m_bikes) <- bike_attribs;
  no <- 1;
  for (i in 1:length(recs)) {
    n <- recs[i];
    np <- xml2::xml_attrs(n)[[1]];

    for (nnp in names(np)) {
      ix_nnp <- which(nnp == station_attrib);
      m_station[i, ix_nnp] <- np[nnp];
    }

    station_uuid  <- np["uid"];
    bikes <- xml2::xml_children(n);
    if (length(bikes) > 0) {
      # cat("Processing Bikes for Station: ", station_uuid, "\r\n");
      for (j in 1:length(bikes)) {
        b <- bikes[j];
        ba <- xml2::xml_attrs(b)[[1]];
        m_bikes[no, ] <- c(station_uuid, ba);
        no <- no + 1;

        if (no == nrow(m_bikes)) {
          b_temp <- matrix(NA_character_, nrow = length(recs), ncol = length(bike_attribs));
          colnames(b_temp) <- bike_attribs;
          m_bikes <- rbind(m_bikes, b_temp);
        }
      }
    }
  }
  # harmonize the columns ... bikes = bike
  m_bikes     <- m_bikes[1:(no - 1), ];
  bikes_df    <- dplyr::as_tibble(m_bikes) %>%
    dplyr::mutate(number = as.integer(number),
                  active = if_else(active == 1, T, F),
                  electric_lock = if_else(electric_lock == 1, T, F)) %>%
    dplyr::rename(bike_number = "number");

  stations_df <- dplyr::as_tibble(m_station);
  stations_df <- stations_df %>%
    dplyr::mutate(
      lat = as.numeric(lat),
      lng = as.numeric(lng),
      number = as.integer(number),
      bikes = as.integer(bikes),
      bike  = as.integer(bike),
      booked_bikes = as.integer(booked_bikes),
      bike_racks = as.integer(bike_racks),
      free_racks = as.integer(free_racks)
    );

  data_df <- dplyr::right_join(bikes_df, stations_df, by = c("station_uuid" = "uid"));

  .data   <- data_df;
  if (as_spatial) {
    data_sf  <- sf::st_as_sf(.data, coords = c("lat", "lng"))
    sf::st_crs(data_sf) <- 4326;
    .data <- sf::st_transform(data_sf, crs = 4326);
  }

  return(.data);
}


#'@author christian  bitter
#'@name bike_station
#'@title Cologne City - Bike stations
#'@description Returns the next bike station information
#'@param as_spatial boolean indicating if the return value should be an sf or not (default True)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes describing the
#'next bike station, such as name, spot, number, bikes, booked_bikes, bike_racks, free_racks,
#'terminal_type bike_numbers and bike_types.
#'@examples
#'sp_sf <- bike_station()
#'@export
bike_station <- function(as_spatial = T) {
  url <- "http://api.nextbike.net/maps/nextbike-official.xml?city=14";
  r <- httr::GET(url = url);
  # json_content <- rOCologneKVB::base_request(url = url, rq_type = "xml");
  content <- httr::content(r, encoding = "UTF-8", as = "text");

  pg <- xml2::read_xml(content);
  epsg_code <- 4326;
  # get all the <record>s
  data_df <- NULL;
  # todo turn into apply
  recs <- xml2::xml_find_all(pg, "//place")

  for (i in 1:length(recs)) {
    n <- recs[i];
    np <- xml2::xml_attrs(n)[[1]];
    if (is.null(data_df)) {
      data_df <- dplyr::bind_rows(np);
    } else {
      data_df <- dplyr::bind_rows(data_df, np);
    }
  }

  .data   <- data_df;
  if (as_spatial) {
    data_sf  <- sf::st_as_sf(.data, coords = c("lat", "lng"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- sf::st_transform(data_sf, crs = 4326);
  }

  return(.data);
}
