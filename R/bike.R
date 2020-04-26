# todo: extract bike information
# <place uid="11464983" lat="50.94160996778235" lng="6.972284317016602" name="Charles-de-Gaulle Platz" spot="1" number="4856" bikes="1" booked_bikes="0" bike_racks="12" free_racks="11" terminal_type="stele" bike_numbers="22901" bike_types="{"17":1}" place_type="14" rack_locks="1">
#   <bike number="22901" bike_type="17" lock_types="fork_lock" active="1" state="ok" electric_lock="1" boardcomputer="27270" pedelec_battery=""/>
#     </place>


#'@author christian  bitter
#'@name bike_station
#'@title Cologne City - Bike stations
#'@description Returns the next bike station information
#'@param as_spatial boolean indicating if the return value should be an sf or not (default True)
#'@return an sf object of the provided data (EPSG 4326). Containing attributes describing the
#'next bike station, such as name, spot, number, bikes, booked_bikes, bike_racks, free_racks,
#'terminal_type bike_numbers and bike_types.
#'@examples
#'sp_sf <- bike_station()
#'@export
bike_station <- function(as_spatial = T) {
  url <- "http://api.nextbike.net/maps/nextbike-official.xml?city=14";
  r <- httr::GET(url = url);
  # json_content <- rOCologneKVB::base_request(url = url, rq_type = "xml");
  content <- httr::content(r, encoding = "UTF-8", as = "text");

  pg <- xml2::read_xml(content);
  epsg_code <- 4326;
  # get all the <record>s
  data_df <- NULL;
  # todo turn into apply
  recs <- xml2::xml_find_all(pg, "//place")

  for (i in 1:length(recs)) {
    n <- recs[i];
    np <- xml2::xml_attrs(n)[[1]];
    if (is.null(data_df)) {
      data_df <- dplyr::bind_rows(np);
    } else {
      data_df <- dplyr::bind_rows(data_df, np);
    }
  }

  .data   <- data_df;
  if (as_spatial) {
    data_sf  <- sf::st_as_sf(data_df, coords = c("lat", "lng"))
    sf::st_crs(data_sf) <- epsg_code;
    .data <- sf::st_transform(data_sf, crs = 4326);
  }

  return(.data);
}
