# https://geoportal.stadt-koeln.de/arcgis/rest/services
#https://geoportal.stadt-koeln.de/arcgis/rest/services/Stadtplanthemen/MapServer/

#https://open.nrw/dataset/kindertagesstaetten-koeln-k
child_care <- function(){
  url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Stadtplanthemen/MapServer/9/query?geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&objectIds=&where=objectid%20is%20not%20null&time=&returnCountOnly=false&returnIdsOnly=false&returnGeometry=true&maxAllowableOffset=&outSR=4326&outFields=%2A&f=json";
  json_content <- base_request(url = url);
  # TODO: prep
  return(json_content);
}

# https://open.nrw/dataset/schulen-koeln-k
school <- function(){
  url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Stadtplanthemen/MapServer/6/query?text=&geometry=&geometryType=esriGeometryPoint&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&objectIds=&where=objectid+is+not+null&time=&returnCountOnly=false&returnIdsOnly=false&returnGeometry=true&maxAllowableOffset=&outSR=4326&outFields=*&f=json";
  json_content <- base_request(url = url);
  # TODO: prep
  return(json_content);
}
