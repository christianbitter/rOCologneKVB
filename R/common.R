base_request <- function(url, ...) {

  verbose <- F;
  execute <- T;

  .args  <- list(...);
  .nargs <- names(.args);

  if ("verbose" %in% .nargs) verbose <- .args[["verbose"]];
  if ("execute" %in% .nargs) execute <- .args[["execute"]];

  json_content <- NULL;

  if (verbose) cat(base_request, "\r\n");

  if (execute) {
    r <- httr::GET(url = url);

    if (httr::status_code(r) != 200) {
      stop(sprintf("lift - failed with\r\n%s", str(httr::http_status(r))));
    }

    content <- httr::content(r, encoding = "UTF-8", as = "text");
    json_content <- jsonlite::fromJSON(content);
  }

  return(json_content);
}

extract_geom <- function(data_df, pull_from_geometry = T) {
  if (missing(data_df)) stop("data missing");
  if (nrow(data_df) < 1) stop("No data");

  if (pull_from_geometry) {
    if (!("geometry" %in% names(data_df))) stop("no geometry attribute in data");

    coords   <- data_df$geometry$coordinates;
    coords   <- unlist(coords);
    coords_m <- matrix(data = coords, ncol = 2, byrow = T);
    data_df$x <- coords_m[, 1];
    data_df$y <- coords_m[, 2];
    data_df$geom_type <- data_df$geometry$type;

  } else {
    data_df$x <- data_df$geometry$x;
    data_df$y <- data_df$geometry$y;
  }
  data_df$geometry <- NULL;
  return(data_df);
}

extract_prop <- function(data_df, properties_attrib_name = "properties") {
  if (missing(data_df)) stop("data missing");
  if (nrow(data_df) < 1) stop("No data");
  if (is.null(properties_attrib_name) || nchar(properties_attrib_name) < 1) stop("properties attribute name not specified");
  if (!(properties_attrib_name %in% names(data_df))) stop("no properties attribute in data");

  ni                 <- names(data_df[[properties_attrib_name]]);
  di                 <- data.frame(data_df[[properties_attrib_name]][1:length(ni)]);
  data_df            <- cbind(data_df, di);
  data_df[[properties_attrib_name]] <- NULL;
  return(data_df);
}

#'@title Common Routines
#'@name cologne
#'@description returns the shape file of the boundary of the City of Cologne
#'@return sf object representing the City's boundary.
#'@author Christian Bitter
#'@export
cologne <- function(){
  a_fp <- system.file("data/Cologne_-_Boundary/Cologne_-_Boundary.shp", package = "rOCologneKVB");
  a_sf <- sf::st_read(a_fp);
  return(a_sf);
}
