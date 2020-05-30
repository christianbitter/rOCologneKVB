#'@author christian bitter
#'@name service_point
#'@title KVB - Service Points
#'@description Returns the service point Information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@param as_spatial should the data be returned as a spatial object
#'@examples
#'sp_sf <- service_point()
#'@export
service_point <- function(as_spatial = F) {
  url <- "https://data.webservice-kvb.koeln/service/opendata/verkaufsorte/json";

  json_content <- base_request(url = url, rq_type = "json");
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df);

  .data <- data_df;

  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}
