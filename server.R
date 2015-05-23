library("dplyr")
library("DT")
library("ggplot2")
library("htmltools")
library("leaflet")
library("RColorBrewer")
library("shiny")
library("zoo")

accidents <- readRDS("data/accidents.rds")
accidents <- accidents[sample(1:nrow(accidents), 2000),]

accident_desc <- function(row)
  with(as.list(row), paste0("<b>",
    format.Date(a_date, "%a %d %B %Y"), ":</b> A ", 
    tolower(severity), " collision in a ", speed_limi, 
    " MPH zone, involving ", no_vehicle, " vechicle(s) with ",
    no_casualt, " casualtie(s). Weather was ", tolower(weather)))

strs <- apply(accidents, 1, accident_desc)
names(strs) <- NULL

# summary plot munging
d2 <- accidents %>% group_by(as.factor(ym)) %>%
  summarise(n=n())
colnames(d2) <- c("ym", "n")

# colour paletters
pal <- brewer.pal(9, "Set1")
cont_pal <- colorRampPalette(brewer.pal(9, "RdBu"))(24)

# clean table for dt
clean <- accidents[,c(3:5,6:7, 17, 22, 23, 24, 25, 35)]
rownames(clean) <- NULL
colnames(clean) <- c("Severity", "No. vehicles", 
  "No. casualties", "Date", "Day", "Speed limit",
  "Light conditions", "Weather conditions", 
  "Road conditions", "Special conditions", "Postcode")

shinyServer(function(input, output, session) {

  getData <- reactive({
    subset(accidents, a_date >= input$dates[[1]] & a_date <= input$dates[[2]])
  })
  
  getAlpha <- reactive({
    message("alpha changed : ", input$alpha)
    input$alpha
  })
  
  legend <- reactive({
    proxy <- leafletProxy("mymap", session, data=accidents)
    if(input$color == "Speed limit"){
      message("triggered")
      pal <- colorFactor(palette="Set1", domain=factor(accidents$speed_limi))
      l <- proxy %>% 
        addLegend("bottomleft", pal=pal, values=~speed_limi, 
          labFormat=labelFormat(suffix=" mph"), title="Speed limit")
      return(l)
    } else {
      return(proxy)
    }
  })
  
  output$mymap <- renderLeaflet({
    ax <- getData()
    
#     fillv <- if(input$color == "None") "black" else 
#       if(input$color == "Severity") pal[as.factor(ax$severity)] else
#         if(input$color == "Casualties") pal[as.factor(ax$no_casualt)] else
#           if(input$color == "Time") cont_pal[ax$a_time_hr] else
#             if(input$color == "Vehicles") pal[as.factor(ax$no_vehicle)] else
#               pal[as.factor(ax$speed_limi)]
    
      l <- leaflet(data=ax) %>% 
        addTiles(urlTemplate="http://openmapsurfer.uni-hd.de/tiles/roadsg/x={x}&y={y}&z={z}") %>%
        addTiles('http://{s}.tile.openstreetmap.se/hydda/roads_and_labels/{z}/{x}/{y}.png', 
          attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        setView(lng=-3.19, lat=55.95, zoom=13) #%>%
        #addCircleMarkers(~long, ~lat, radius=~(no_vehicle+.8)**1.5, fillOpacity=input$alpha,
         # color=NA, popup=strs, weight=2, fillColor = fillv)
      
    
      session$sendCustomMessage(type = "map_done", "done")
      
      l
  })
  
#   observe({
#     ax <- getData()
#     alpha <- getAlpha()
#     leafletProxy("mymap", session, data=ax) %>%
#       addCircleMarkers(~long, ~lat, radius=~(no_vehicle+.8)**1.5, fillOpacity=alpha,
#        color=NA, popup=strs, fillColor = "black")
#   })
    
  observe({
    ax <- getData()
    title <- input$color
    
    col <- switch(input$color,
      "Severity"    = list(var="severity", type="factor"),
      "Casualties"  = list(var="no_casualt", type="int"),
      "Time"        = list(var="a_time_hr", type="int"),
      "Vehicles"    = list(var="no_vehicle", type="int"),
      "Speed limit" = list(var="speed_limi", type="int"),
      list(var="speed_limi", type="int"))
    message(col)
    
    col_fn <- function(col){
      if(col$type != "none"){
        if(col$type == "int") {
          return(colorNumeric("Set1", domain=ax[[col$var]]))
        } else {
          return(colorFactor("Set1", domain=ax[[col$var]]))
        }} else return( function() "black" )
    }
    
    leafletProxy("mymap", session, data=ax) %>%
      clearMarkers() %>%
      addCircleMarkers(~long, ~lat, radius=~1+(no_vehicle**2), fillOpacity=getAlpha(),
        color=NA, popup=strs, fillColor = ~col_fn(col)(ax[[col$var]])) %>%
      addLegend("bottomleft", pal=col_fn(col), values=ax[[col$var]], title=title)
  })
  
  output$monthTotals <- renderPlot({
    print(ggplot(d2, aes(x=zoo::as.Date(zoo::as.yearmon(ym)), y=n)) + 
      geom_area() + theme_minimal() + 
      labs(x="", y="Recorded collisions\nper month") +
      scale_y_continuous(expand=c(0,0)))
  })

  output$table <- DT::renderDataTable({
    DT::datatable(clean, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))
  })
  
})
