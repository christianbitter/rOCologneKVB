#'@author christian  bitter
#'@name child_care
#'@title Cologne City - Public Facilities
#'@description Returns Cologne's schools (https://open.nrw/dataset/kindertagesstaetten-koeln-k)
#'@param as_spatial boolean indicating if the return value should be an sf or not (default True)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes describing the
#'school if any.
#'@examples
#'sp_sf <- child_care()
#'@export
child_care <- function(as_spatial = F){
  url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Stadtplanthemen/MapServer/9/query?geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&objectIds=&where=objectid%20is%20not%20null&time=&returnCountOnly=false&returnIdsOnly=false&returnGeometry=true&maxAllowableOffset=&outSR=4326&outFields=%2A&f=json";
  json_content <- base_request(url = url, rq_type = "json");

  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df, properties_attrib_name = "attributes");

  .data   <- data_df;

  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data_df, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}

#'@author christian  bitter
#'@name school
#'@title Cologne City - Public Facilities
#'@description Returns Cologne's schools (https://open.nrw/dataset/schulen-koeln-k)
#'@param as_spatial boolean indicating if the return value should be an sf or not (default True)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes describing the
#'school if any.
#'@examples
#'sp_sf <- school()
#'@export
school <- function(as_spatial = F){
  url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Stadtplanthemen/MapServer/6/query?text=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&objectIds=&where=objectid+is+not+null&time=&returnCountOnly=false&returnIdsOnly=false&returnGeometry=true&maxAllowableOffset=&outSR=4326&outFields=*&f=json";
  json_content <- base_request(url = url, rq_type = "json");

  data_df <- json_content$features;
  data_df <- extract_geom(data_df);
  data_df <- extract_prop(data_df, properties_attrib_name = "attributes");

  .data   <- data_df;

  if (as_spatial) {
    epsg_code <- 4326;
    data_sf  <- sf::st_as_sf(.data_df, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- data_sf;
  }

  return(.data);
}
