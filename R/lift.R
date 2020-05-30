#'@author christian bitter
#'@name lift
#'@title KVB - Lift
#'@description returns the lift information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@param as_spatial should the data be returned as an sf object
#'@examples
#'lift_sf <- lift()
#'@export
lift <- function(as_spatial = F) {
  url <- "https://data.webservice-kvb.koeln/service/opendata/aufzuege/json";

  json_content <- base_request(url = url, rq_type = "json");

  # now into a spatial structure
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  data_df <- data_df %>%
    dplyr::mutate(Haltestellenbereich = as.numeric(Haltestellenbereich));

  .data   <- data_df;
  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}

#'@author christian bitter
#'@name lift_incident
#'@title KVB - Lift
#'@description returns the lift incident information from the KVB Open Data Portal
#'@param as_spatial should the data be returned as an sf object
#'@return an sf object of the provided data (EPSG 4326) or a data frame
#'@examples
#'lift_incident_sf <- lift_incident()
#'@export
lift_incident <- function(as_spatial){
  url <- "https://data.webservice-kvb.koeln/service/opendata/aufzugsstoerung/json";

  json_content <- base_request(url = url, rq_type = "json");
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  data_df <- data_df %>%
    dplyr::mutate(time = lubridate::ymd_hms(data_df$timestamp),
                  Haltestellenbereich = as.numeric(Haltestellenbereich));

  .data   <- data_df;
  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}
