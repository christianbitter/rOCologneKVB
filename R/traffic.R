traffic <- function(){
  url <- "https://www.stadt-koeln.de/externe-dienste/open-data/traffic.php";
  json_content <- base_request(url = url);
  # TODO: prep
  return(json_content);
}
