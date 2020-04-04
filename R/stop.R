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

  data_df$ASS <- data_df$properties$ASS;
  data_df$Name <- data_df$properties$Name;
  data_df$Kurzname <- data_df$properties$Kurzname;
  data_df$Betriebsbereich <- data_df$properties$Betriebsbereich;
  data_df$Linien <- data_df$properties$Linien;

  data_df <- data_df %>%
    dplyr::select(-geometry, -properties);

  epsg_code <- 4326;
  data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
  sf::st_crs(data_sf) <- epsg_code;

  return(data_sf);
}
