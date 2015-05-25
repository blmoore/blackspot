library("dplyr")
library("DT")
library("shiny")
library("leaflet")


shinyUI(navbarPage("Blackspot", id="nav", collapsible=T,
  tabPanel("Map",
    div(class="outer",
      
      tags$head(
          includeScript("analytics.js"),
          includeCSS("styles.css"),
          includeScript("spin.min.js")
      ),
      
      leafletOutput("mymap", width="100%", height="100%"),
      tags$script("
var spinner = new Spinner().spin();
$( 'div#mymap' ).append(spinner.el);"),
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
        width = 360, height = "auto",
        
        h2(),
        p(strong("Blackspot"), " shows vechicle collisions in and around",
          "the city of Edinburgh, UK. Data is made available by ",
          "Edinburgh County Council via", 
          a("Edinburgh Open Data.", 
            href="http://www.edinburghopendata.info/dataset/vehicle-collisions")),

        hr(),
        
        h4("Controls"),
        
        dateRangeInput('dates',
          label = 'Occurred between:',
          start = as.Date("2010-01-01"), end = as.Date("2013-07-01")),
        
        selectInput("color", "Colour by:", 
          choices=c("None", "Severity", "Casualties", "Time", "Vehicles", "Speed limit")),
        
        sliderInput("alpha", "Opacity:",
          min=0, max=1, value=.4, step=.025, ticks=T),
        
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
          "by Joe Cheng)."),
      
      tags$script('
  Shiny.addCustomMessageHandler("map_done",
        function(s) {
          spinner.stop();
          $( "div#mymap" ).remove(spinner);
        });')
        
      )#,
      
     # tags$div(id="cite",
      #  a("@benjaminlmoore", href="http://twitter.com/benjaminlmoore"))
    )
  ), tabPanel("Table", DT::dataTableOutput("table"))
)
)