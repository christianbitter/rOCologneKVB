#'@author christian bitter
#'@name lift
#'@title Open Data KVB
#'@description returns the lift information from the KVB Open Data Portal
#'@return an sf object of the provided data (EPSG 4326).
#'@examples
#'lift()
#'@export
lift <- function() {
  url <- "https://data.webservice-kvb.koeln/service/opendata/aufzuege/json";

  r <- httr::GET(url = url);

  if (httr::status_code(r) != 200) {
    stop(sprintf("lift - failed with\r\n%s", str(http_status(r))));
  }

  content <- httr::content(r, encoding = "UTF-8", as = "text");
  json_content <- jsonlite::fromJSON(content);

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
