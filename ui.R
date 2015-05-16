library("dplyr")
library("DT")
library("shiny")
library("leaflet")


shinyUI(navbarPage("Blackspot", id="nav",
  tabPanel("Map",
    div(class="outer",
      
      tags$head(
          includeCSS("styles.css")
      ),
      
      leafletOutput("mymap", width="100%", height="100%"),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 120, left = "auto", right = 20, bottom = "auto",
        width = 400, height = "auto",
        
        h2("Edinburgh traffic collisions"),
        hr(),
        
        h3("Controls"),
        
        #selectInput("color", "Color", vars),
        #selectInput("size", "Size", vars, selected = "adultpop"),
        
        dateRangeInput('dates',
          label = 'Occurred between:',
          start = as.Date("2010-01-01"), end = as.Date("2013-07-01")),
        
        selectInput("color", "Color by:", 
          choices=c("None", "Severity", "Casualties", "Time", "Vehicles")),
        
        sliderInput("alpha", "Opacity:",
          min=0, max=1, value=.3),
        
        h3("Summary plots"),
        plotOutput("monthTotals", height = 150)
        #plotOutput("scatterCollegeIncome", height = 250)
      ),
      
      tags$div(id="cite",
        'Traffic collision data from Edinburgh County Council via', 
        tags$a("Edinburgh Open Data", href="http://www.edinburghopendata.info/dataset/vehicle-collisions"),
        ". Shiny code adapted from",
        tags$a("Superzip", href="https://github.com/jcheng5/superzip"), "by Joe Cheng.")
    )
  ), tabPanel("Table", DT::dataTableOutput("table"))
)
)