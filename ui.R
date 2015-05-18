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
        draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
        width = 380, height = "auto",
        
        h2(),
        p(strong("Blackspot"), " shows vechicle collisions in and around",
          "the city of Edinburgh, UK. Data is made available by ",
          "Edinburgh County Council via", 
          a("Edinburgh Open Data.", 
            href="http://www.edinburghopendata.info/dataset/vehicle-collisions")),

        hr(),
        
        h4("Controls"),
        
        #selectInput("color", "Color", vars),
        #selectInput("size", "Size", vars, selected = "adultpop"),
        
        dateRangeInput('dates',
          label = 'Occurred between:',
          start = as.Date("2010-01-01"), end = as.Date("2013-07-01")),
        
        selectInput("color", "Color by:", 
          choices=c("None", "Severity", "Casualties", "Time", "Vehicles")),
        
        sliderInput("alpha", "Opacity:",
          min=0, max=1, value=.3),
        
        hr(),
        h4("Summary plots"),
        plotOutput("monthTotals", height = "120px"),
        
        hr(),
        p("Under active development by ", 
        a("@benjaminlmoore", href="http://twitter.com/benjaminlmoore"),
        HTML("&mdash;"), "code available on ",
        a("github", href="http://github.com/blmoore/blackspot"),
          "(original Shiny code adapted from",
        a("Superzip", href="https://github.com/jcheng5/superzip"), 
          "by Joe Cheng).")
        
      ),
      
      tags$div(id="cite",
        a("@benjaminlmoore", href="http://twitter.com/benjaminlmoore"))
    )
  ), tabPanel("Table", DT::dataTableOutput("table"))
)
)