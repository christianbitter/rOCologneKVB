#'@author christian bitter
#'@name lift
#'@title KVB - Lift
#'@description returns the lift information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'lift_sf <- lift()
#'@export
lift <- function() {
  url <- "https://data.webservice-kvb.koeln/service/opendata/aufzuege/json";

  json_content <- base_request(url = url);

  # now into a spatial structure
  data_df <- json_content$features;
  coords  <- data_df$geometry$coordinates;
  coords  <- unlist(coords);
  coords_m<- matrix(data = coords, ncol = 2, byrow = T);
  data_df$x <- coords_m[, 1];
  data_df$y <- coords_m[, 2];
  data_df$geom_type <- data_df$geometry$type;
  data_df$Kennung <- data_df$properties$Kennung;
  data_df$Bezeichnung <- data_df$properties$Bezeichnung;
  data_df$Haltestellenbereich <- data_df$properties$Haltestellenbereich;
  data_df$Info <- data_df$properties$Info;

  data_df <- data_df %>%
    dplyr::select(-geometry, -properties) %>%
    dplyr::mutate(Haltestellenbereich = as.numeric(Haltestellenbereich));

  epsg_code <- 4326;
  data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
  sf::st_crs(data_sf) <- epsg_code;

  return(data_sf);
}

#'@author christian bitter
#'@name lift_incident
#'@title KVB - Lift
#'@description returns the lift information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'lift_incident_sf <- lift_incident()
#'@export
lift_incident <- function(){
  url <- "https://data.webservice-kvb.koeln/service/opendata/aufzugsstoerung/json";

  json_content <- base_request(url = url);
  data_df      <- json_content$features;
  coords  <- data_df$geometry$coordinates;
  coords  <- unlist(coords);
  coords_m<- matrix(data = coords, ncol = 2, byrow = T);
  data_df$x <- coords_m[, 1];
  data_df$y <- coords_m[, 2];
  data_df$geom_type <- data_df$geometry$type;
  data_df$Kennung <- data_df$properties$Kennung;
  data_df$Bezeichnung <- data_df$properties$Bezeichnung;
  data_df$Haltestellenbereich <- data_df$properties$Haltestellenbereich;
  data_df$Info <- data_df$properties$Info;
  data_df$time <- lubridate::ymd_hms(data_df$properties$timestamp);

  data_df <- data_df %>%
    dplyr::select(-geometry, -properties) %>%
    dplyr::mutate(Haltestellenbereich = as.numeric(Haltestellenbereich));

  epsg_code <- 4326;
  data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
  sf::st_crs(data_sf) <- epsg_code;

  return(data_sf);
}
