library("leaflet")


ax <- readRDS("data/accidents.rds")

input <- list()
input$color <- "none"
title <- input$color

col <- switch(input$color,
  "Severity"    = list(var="severity", type="factor"),
  "Casualties"  = list(var="no_casualt", type="int"),
  "Time"        = list(var="a_time_hr", type="int"),
  "Vehicles"    = list(var="no_vehicle", type="int"),
  "Speed limit" = list(var="speed_limi", type="int"),
  list(var="none", type="none"))
message(col)

col_fn <- function(col){
  if(col$type != "none"){
    if(col$type == "int") {
      return(colorNumeric("Set1", domain=ax[[col$var]]))
    } else {
      return(colorFactor("Set1", domain=ax[[col$var]]))
    }} else return( function(...) "black" )
}

map <- leaflet(ax) %>% addTiles()

pal <- colorNumeric("Set1", ax$speed_limi)

if(col[[1]] == "none"){
  map %>%
    addCircleMarkers(data=ax, ~long, ~lat, radius=8, fillOpacity=.3,
      color=NA, fillColor = ~col_fn(col)(ax[[col$var]])) 
  } else {
    map %>%
      addCircleMarkers(data=ax, ~long, ~lat, radius=8, fillOpacity=.3,
        color=NA, fillColor = ~col_fn(col)(ax[[col$var]])) %>%
      addLegend("bottomleft", pal=col_fn(col), values=ax[[col$var]], title=title,
        layerId="legend")
}
