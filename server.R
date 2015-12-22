library("dplyr")
library("DT")
library("ggplot2")
library("htmltools")
library("leaflet")
#library("rcharts")
library("shiny")

accidents <- readRDS("data/accidents.rds")
#accidents <- accidents[sample(1:nrow(accidents), 3000),]

accident_desc <- function(row)
  with(as.list(row), paste0("<b>",
    format.Date(a_date, "%a %d %B %Y"), ":</b> A ", 
    tolower(severity), " collision in a ", speed_limi, 
    " MPH zone, involving ", no_vehicle, " vechicle(s) with ",
    no_casualt, " casualtie(s). Weather was ", tolower(weather)))

strs <- apply(accidents, 1, accident_desc)
names(strs) <- NULL

accidents$text <- strs

# summary plot munging
d2 <- accidents %>% group_by(as.factor(ym)) %>%
  summarise(n=n())
colnames(d2) <- c("ym", "n")

# clean table for dt
clean <- accidents[,c(3:5,6:7, 17, 22, 23, 24, 25, 35)]
rownames(clean) <- NULL
colnames(clean) <- c("Severity", "No. vehicles", 
  "No. casualties", "Date", "Day", "Speed limit",
  "Light conditions", "Weather conditions", 
  "Road conditions", "Special conditions", "Postcode")

map_choice <- function(inp){
  switch(inp,
    "Severity"    = list(var="severity", type="factor"),
    "Casualties"  = list(var="no_casualt", type="int"),
    "Time"        = list(var="a_time_hr", type="int"),
    "Vehicles"    = list(var="no_vehicle", type="int"),
    "Speed limit" = list(var="speed_limi", type="int"),
    list(var="none", type="none"))
}

legend_hdr <- function(inp){
  switch(inp,
    "Severity"    = "Severity",
    "Casualties"  = "Number of casualties",
    "Time"        = "Time (24h)",
    "Vehicles"    = "Number of vehicles",
    "Speed limit" = "Speed limit (mph)",
    "")
}

monthly <- accidents %>% group_by(a_date_yr, a_date_mon) %>% tally()
monthly$a_date_mon <- factor(monthly$a_date_mon, levels=month.name, ordered=T)
monthly$a_date_yr <- factor(paste0("20",monthly$a_date_yr), levels=2013:2010)

shinyServer(function(input, output, session) {
  
  getData <- reactive({
    subset(accidents, a_date >= input$dates[[1]] & a_date <= input$dates[[2]])
  })
  
  getAlpha <- reactive({
    #message("alpha changed : ", input$alpha)
    input$alpha
  })
  
#   getZoom <- reactive({
#     input$mymap_zoom
#   })
  
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
    # build base map on load
    ax <- getData()
        
    l <- leaflet(data=ax) %>% 
      addProviderTiles("CartoDB.Positron", tileOptions(minZoom=10, maxZoom=17))  %>%
      setView(lng=-3.19, lat=55.95, zoom=13) %>%
      setMaxBounds(lng1=-5, lat1=55, lng2=-2,  lat2=57)
    
    # stop spinner    
    session$sendCustomMessage(type = "map_done", "done")
    
    l
  })
  
  observe({
    # modify map of changed input i.e. colour by
    ax <- getData()
    title <- legend_hdr(input$color)

    updateRadioButtons(session, "color_mob", selected=input$color)
    
    col <- map_choice(input$color)
    scale <- map_choice(input$scale)
        
    pal <- c("#A52278", "#993086", "#8C3C97", 
      "#6D328A", "#4E2B81", "#3B264B", "#180B11", "#000000")
    
    col_fn <- function(col){
      if(col$type != "none"){
        if(col$type == "int") {
          #return(colorNumeric("RdGy", domain=ax[[col$var]]))
          return(colorNumeric(pal, domain=ax[[col$var]]))
        } else {
          return(colorFactor("Set1", domain=ax[[col$var]]))
        }} else return( function(...) "black" )
    }
    
    # TODO ::
    # as zoom increases, point sizes decrease,
    # instead, increase points with zoom size
    # zoom <- getZoom()
    # message(zoom)
    
    if(col$var == "none"){
      l <- leafletProxy("mymap", session, data=ax) %>%
        addCircleMarkers(~long, ~lat, 
          radius=~input$base+(eval(parse(text=scale$var))**1.5), 
          fillOpacity=getAlpha(),
          color=NA, popup=~text, fillColor = "black",
          layerId=paste0("p", 1:nrow(ax))) %>%
        removeControl(layerId="legend") 
  
    } else {
      
    l <- leafletProxy("mymap", session, data=ax) %>%
      addCircleMarkers(~long, ~lat, 
        radius=~input$base+(eval(parse(text=scale$var))**1.5), 
        fillOpacity=getAlpha(),
        color=NA, popup=~text, fillColor = ~col_fn(col)(ax[[col$var]]),
        layerId=paste0("p", 1:nrow(ax))) %>%
      addLegend("bottomleft", pal=col_fn(col), values=ax[[col$var]], 
        title=title, layerId="legend")
    }
  })
  
  # -- Map mobile controls back to main panel -- #
  observe({
    updateSelectInput(session, "color", selected=input$color_mob)
  })
  
  
  output$monthTotals <- renderPlot({
    d2 <- getData() %>% group_by(as.factor(ym)) %>%
      summarise(n=n())
    colnames(d2) <- c("ym", "n")
    
    print(ggplot(d2, aes(x=zoo::as.Date(zoo::as.yearmon(ym)), y=n)) + 
        geom_area() + theme_minimal() + 
        labs(x=NULL, y="Incidents\nper month") +
        scale_y_continuous(expand=c(0,0)))
  })
  
  output$month_waffle <- renderPlot({
    print(ggplot(monthly, aes(x=a_date_mon, y=a_date_yr, fill=n)) +
      geom_tile(col="white") + scale_fill_continuous(low="grey80", high="grey10") +
      theme_minimal() + scale_x_discrete(expand=c(0,0), 
        labels=substr(month.abb, 1, 1)) +
      labs(x=NULL, y=NULL, fill="Incidents") +
      theme(axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_text(hjust=0),
        axis.text.x=element_text(vjust=0)))
  })
  
  
  output$involving <- renderPlot({
    types <- getData() %>% group_by(acc_type) %>% tally()
    
    types$lab <- factor(with(types, ifelse(acc_type == "M.V.N.P.", "Multiple vehicles",
      ifelse(acc_type == "S.V.N.P.", "Single vehicle",
        ifelse(acc_type == "T.V.N.P.", "Two vehicles", as.character(acc_type))))))
    
    types$lab <- factor(types$lab, levels=levels(types$lab)[c(5:6, 3, 1:2, 4)])
    
    print(ggplot(types, aes(x=lab, y=n/sum(n))) +
        geom_bar(stat="identity", position=position_stack()) +
        theme_minimal() + labs(y="Percent collisions involved", x=NULL) +
        scale_y_continuous(expand=c(0,0), labels=scales::percent) +
        theme(axis.text.y=element_text(hjust=1)) + #, colour = "white")) +
        coord_flip())
  })
  
  
  
  output$table <- DT::renderDataTable({
    action <- dataTableAjax(session, clean)
    DT::datatable(clean, filter = 'top', server=T, options = list(
      pageLength = 10, autoWidth = TRUE, ajax=list(url=action)))
  })
  
})
