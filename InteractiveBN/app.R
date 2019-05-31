#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(highcharter)

# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 4,
# 
#         fluidRow(
#           column(12,
#                  wellPanel(
#             textInput("BAR", label = h5("Biochar application rate "), value = ""),
#             textInput("NAR", label = h5("Nitrogen application rate"), value = ""),
#             textInput("Lat", label = h5("latitude "), value = "")
#           ))),
fluidRow(column(6,      h4("Soil"),
                fluidRow(column(
                  12,
                  wellPanel(
                    textInput("SpH", label = h5("Soil pH"), value = ""),
                    textInput("CEC", label = h5("CEC"), value = ""),
                    textInput("SOC", label = h5("SOC"), value = ""),
                    sliderInput(
                      "Sand",
                      label = h5("Sand"),
                      min = 0,
                      max = 100,
                      value = 50
                    ),
                    sliderInput(
                      "Silt",
                      label = h5("Silt"),
                      min = 0,
                      max = 100,
                      value = 30
                    ),
                    sliderInput(
                      "Clay",
                      label = h5("Clay"),
                      min = 0,
                      value = 20,
                      max = 100
                    )
                    
                    
                    
                  )
                ))),
         column(6,        h4("Biochar"),
                fluidRow(column(
                  12, wellPanel(
                    textInput("BpH", label = h5("Biochar pH "), value = ""),
                    textInput(
                      "HT",
                      label = h5("Highest temperature "),
                      value = ""
                    ),
                    textInput("BC", label = h5("Biochar C "), value = ""),
                    textInput("BCN", label = h5(" Biochar C/N"), value = "")
                  ),
                  h4("Add to chart"),
                  textInput("seriesname", label = h5(""), value = "Series name"),
                  actionButton("addev", label = "Add", class="btn btn-success btn-block"),br(),
                  actionButton("resetbtn", label = "Reset !", class="btn btn-danger btn-block")
                ))))


        # fluidRow( column(12, wellPanel(
        #     selectInput("txts", label = h5("Texture"), 
        #                 choices = list(" " = 1, "Light" = "Light", "Medium" = "Medium","Heavy"="Heavy"), 
        #                 selected = 1),
        #     selectInput("fds", label = h5("Feedstock"), 
        #                 choices = list(" " = 1, "Nonwood" = "Nonwood", "Wood" = "Wood","Manure"="Manure"), 
        #                 selected = 1), 
        #     selectInput("prs", label = h5("Pyrolysis rate"), 
        #                 choices = list(" " = 1, "fast" = "fast", "slow" = "slow"), 
        #                 selected = 1)
        #   )))

      ),
      
      # Show a plot of the generated distribution
      mainPanel(width = 8,

        fluidRow(
          column(12,wellPanel( highchartOutput("hcontainer",height = "auto"))),
          column(12,wellPanel( 
            fluidRow(highchartOutput("compplot",height = "auto")))
            
          ))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

