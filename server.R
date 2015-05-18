library("dplyr")
library("DT")
library("ggplot2")
library("htmltools")
library("leaflet")
library("RColorBrewer")
library("shiny")
library("stringr")
library("zoo")

accidents <- readRDS("data/accidents.rds")
accidents <- accidents[sample(1:nrow(accidents), 2000),]

accident_desc <- function(row)
  with(as.list(row), paste0(strong(
    format.Date(a_date, "%a %d %B %Y"), ": "), "A ", 
    tolower(severity), " collision in a ", speed_limi, 
    " MPH zone, involving ", no_vehicle, " vechicle(s) with ",
    no_casualt, " casualtie(s). Weather was ", tolower(weather)))

strs <- apply(accidents, 1, accident_desc)
strs <- str_wrap(strs, width=10) 

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

  output$mymap <- renderLeaflet({
    fillv <- if(input$color == "None") "black" else 
      if(input$color == "Severity") pal[as.factor(accidents$severity)] else
        if(input$color == "Casualties") pal[as.factor(accidents$no_casualt)] else
          if(input$color == "Time") cont_pal[accidents$a_time_hr] else
            if(input$color == "Vehicles") pal[as.factor(accidents$no_vehicle)] else
              pal[as.factor(accidents$speed_limi)]
    
      l <- leaflet(data=accidents) %>% 
        addTiles(urlTemplate="http://openmapsurfer.uni-hd.de/tiles/roadsg/x={x}&y={y}&z={z}") %>%
        addTiles('http://{s}.tile.openstreetmap.se/hydda/roads_and_labels/{z}/{x}/{y}.png', 
          attribution='&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>%
        setView(lng=-3.19, lat=55.95, zoom=13) %>%
        addCircleMarkers(~long, ~lat, radius=~(no_vehicle+.8)**1.5, fillOpacity=input$alpha,
          color=NA, popup=strs, weight=2, fillColor = fillv)
      
      session$sendCustomMessage(type = "map_done", "done")
      
      l
  })
  
  output$monthTotals <- renderPlot({
    ggplot(d2, aes(x=zoo::as.Date(zoo::as.yearmon(ym)), y=n)) + 
      geom_area() + theme_minimal() + 
      labs(x="", y="Recorded collisions\nper month") +
      scale_y_continuous(expand=c(0,0))
  })

  output$table <- DT::renderDataTable({
    DT::datatable(clean, filter = 'top', options = list(
      pageLength = 10, autoWidth = TRUE))
  })
  
})
