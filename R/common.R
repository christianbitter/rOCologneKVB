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
