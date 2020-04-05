base_request <- function(url, ...) {

  verbose <- F;
  execute <- T;

  .args <- list(...);
  .nargs<- names(.args);

  if ("verbose" %in% .nargs) verbose <- .args[["verbose"]];
  if ("execute" %in% .nargs) execute <- .args[["execute"]];

  json_content <- NULL;

  if (verbose) cat(base_request, "\r\n");

  if (execute) {
    r <- httr::GET(url = url);

    if (httr::status_code(r) != 200) {
      stop(sprintf("lift - failed with\r\n%s", str(http_status(r))));
    }

    content <- httr::content(r, encoding = "UTF-8", as = "text");
    json_content <- jsonlite::fromJSON(content);
  }

  return(json_content);
}

extract_geom <- function(data_df) {
  if (missing(data_df)) stop("data missing");
  if (nrow(data_df) < 1) stop("No data");
  if (!("geometry" %in% names(data_df))) stop("no geometry attribute in data");

  coords  <- data_df$geometry$coordinates;
  coords  <- unlist(coords);
  coords_m<- matrix(data = coords, ncol = 2, byrow = T);
  data_df$x <- coords_m[, 1];
  data_df$y <- coords_m[, 2];
  data_df$geom_type <- data_df$geometry$type;

  data_df$geometry <- NULL;

  return(data_df);
}

extract_prop <- function(data_df) {
  if (missing(data_df)) stop("data missing");
  if (nrow(data_df) < 1) stop("No data");
  if (!("properties" %in% names(data_df))) stop("no properties attribute in data");

  ni                 <- names(data_df$properties);
  di                 <- data.frame(data_df$properties[1:length(ni)]);
  data_df            <- cbind(data_df, di);
  data_df$properties <- NULL;
  return(data_df);
}
