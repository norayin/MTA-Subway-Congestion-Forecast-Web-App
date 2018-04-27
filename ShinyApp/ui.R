library(shiny)
library(DT)
library(leaflet)
library(shinythemes)

navbarPage("MTA CONGESTION PREDICTION",theme = shinytheme("flatly"),
          
#  tags$head(tags$style(
#    HTML('
#         #sidebar {
#         background-color: #dec4de;
#         }
#         ')
#  )),
  
  tabPanel("Map", 
           
     sidebarLayout(
       sidebarPanel(id="sidebar",
         p(strong(span("Search Congestion Level", style = "font-size:20pt"))),
         p(em(span("When and where do you want to take the MTA subway?", style = "color:grey;font-size:16pt"))),
         br(),
         uiOutput("selectMonth"),
         uiOutput("selectDay"),
         uiOutput("selectHour"),
         uiOutput("selectStation"),
         br(),
         actionButton("recalc", "Zoom to Selected Station",icon("refresh"), 
                      style="color: #fff; background-color: #2c3e50; border-color: #2e6da4")
       ),
       mainPanel(
         br(),
         uiOutput("leaf"),
         #leafletOutput("mymap"),
         hr()
       )
     )
   ),
              
       tabPanel("Result Table", 
               dataTableOutput('table'),
               hr()
               )
  )

