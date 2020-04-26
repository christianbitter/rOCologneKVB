traffic <- function(){
  url <- "https://www.stadt-koeln.de/externe-dienste/open-data/traffic.php";
  json_content <- base_request(url = url);
  epsg_code    <- json_content$spatialReference$wkid;
  data_df <- json_content$features;
  data_df <- rOCologneKVB::extract_prop(data_df, properties_attrib_name = "attributes");
  # p <- data_df$geometry$paths;
  # p1 <- p[]
  # data_df$geometry <- NULL;
  # data_sf  <- sf::st_as_sfc(data_df, agr = "geometry", crs = epsg_code);

  # we have to walk through all line strings ... and construct them


  # TODO: prep
  return(json_content);
}

#'@author christian bitter
#'@name car_park
#'@title Cologne City - car park occupancy
#'@description Returns the current status of car parks around cologne. (see: http://www.stadt-koeln.de/externe-dienste/open-data/parking-ts.php)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes
#'IDENTIFIER: identifier of the car park (e.g., PH01 Cologne Central Station)
#'KAPAZITAET: number of available parking spots ( -1 = no data available, service interruption).
#'TENDENZ: -1 => not available 0 => number of parking spots constant 1 => less spots 2 => more parking places
#'@examples
#'sp_sf <- car_park()
#'@export
car_park <- function(as_spatial = T){
  url <- "https://www.stadt-koeln.de/externe-dienste/open-data/parking-ts.php";
  json_content <- base_request(url = url);
  epsg_code    <- json_content$spatialReference$wkid;
  data_df <- json_content$features;
  data_df <- rOCologneKVB::extract_geom(data_df, pull_from_geometry = F);
  data_df <- rOCologneKVB::extract_prop(data_df, properties_attrib_name = "attributes");
  .data   <- data_df;
  if (as_spatial) {
    data_sf  <- sf::st_as_sf(data_df, coords = c("x", "y"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- sf::st_transform(data_sf, crs = 4326);
  }

  return(.data);
}
