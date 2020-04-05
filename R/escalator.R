#'@author christian bitter
#'@name escalator
#'@title KVB - Escalator
#'@description returns the escalator information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'escalator_sf <- escalator()
#'@export
escalator <- function(){
  url <- "https://data.webservice-kvb.koeln/service/opendata/fahrtreppen/json";

  json_content <- base_request(url = url);

  # now into a spatial structure
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  data_df <- data_df %>%
    dplyr::mutate(Haltestellenbereich = as.numeric(Haltestellenbereich));

  epsg_code <- 4326;
  data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
  sf::st_crs(data_sf) <- epsg_code;

  return(data_sf);
}

#'@author christian bitter
#'@name escalator
#'@title KVB - Escalator
#'@description returns the escalator incident information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'escalator_incident_sf <- escalator_incident()
#'@export
escalator_incident <- function(){
  url <- "https://data.webservice-kvb.koeln/service/opendata/fahrtreppenstoerung/json";

  json_content <- base_request(url = url);

  # now into a spatial structure
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  data_df <- data_df %>%
    dplyr::mutate(time = lubridate::ymd_hms(data_df$timestamp),
                  Haltestellenbereich = as.numeric(Haltestellenbereich));

  epsg_code <- 4326;
  data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
  sf::st_crs(data_sf) <- epsg_code;

  return(data_sf);
}
