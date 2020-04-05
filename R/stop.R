#'@author christian bitter
#'@name stops
#'@title KVB - Lift
#'@description Returns the Stop Information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'stop_df <- stop()
#'@export
stop <- function() {
  url <- "https://data.webservice-kvb.koeln/service/opendata/haltestellen/json";

  json_content <- base_request(url = url);
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  epsg_code <- 4326;
  data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
  sf::st_crs(data_sf) <- epsg_code;

  return(data_sf);
}
