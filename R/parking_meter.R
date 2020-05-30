#'@author christian  bitter
#'@name parking_meter
#'@title Cologne City - Bike stations
#'@description Returns the cologne city biking meters
#'@param as_spatial boolean indicating if the return value should be an sf or not (default True)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes describing the
#'next bike station and bikes at the station if any.
#'@examples
#'sp_sf <- parking_meter()
#'@export
parking_meter <- function(as_spatial = T) {
  url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Parkscheinautomaten_66/MapServer/0/query?text=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&objectIds=&where=id%20is%20not%20null&time=&returnCountOnly=false&returnIdsOnly=false&returnGeometry=true&maxAllowableOffset=&outSR=4326&outFields=%2A&f=json";

  json_content <- base_request(url = url, rq_type = "json");
  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df, properties_attrib_name = "attributes");

  .data   <- data_df;

  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data    <- data_sf;
  }

  return(.data);
}
