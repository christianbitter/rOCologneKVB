#'@author christian bitter
#'@name escalator
#'@title KVB - Escalator
#'@description returns the escalator information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@param as_spatial should data be returned as sf (default False)
#'@examples
#'escalator_sf <- escalator()
#'@export
escalator <- function(as_spatial = F) {
  url <- "https://data.webservice-kvb.koeln/service/opendata/fahrtreppen/json";

  json_content <- base_request(url = url, rq_type = "json");

  # now into a spatial structure
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  data_df <- data_df %>%
    dplyr::mutate(Haltestellenbereich = as.numeric(Haltestellenbereich));

  .data <- data_df;

  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}

#'@author christian bitter
#'@name escalator
#'@title KVB - Escalator
#'@param as_spatial should data be returned as sf (default False)
#'@description returns the escalator incident information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'escalator_incident_sf <- escalator_incident()
#'@export
escalator_incident <- function(as_spatial = F) {
  url <- "https://data.webservice-kvb.koeln/service/opendata/fahrtreppenstoerung/json";

  json_content <- base_request(url = url, rq_type = "json");

  # now into a spatial structure
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  data_df <- data_df %>%
    dplyr::mutate(time = lubridate::ymd_hms(data_df$timestamp),
                  Haltestellenbereich = as.numeric(Haltestellenbereich));

  .data <- data_df;

  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}
