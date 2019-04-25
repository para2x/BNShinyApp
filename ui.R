#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(shinythemes)
library(visNetwork) 
library(leaflet)
library(highcharter)

shinyUI(navbarPage(windowTitle = "Bayesian networks & Biochar",theme = shinytheme("united"), h4("BN and Biochar"),
                    
                   tabPanel(h4("  Introduction  "),
                            tags$head(HTML( " <title>BN& Biochar</title>"),
                              tags$style( HTML( ".modal-backdrop {z-index: 0;}"  )
                              )
                            ), 
                            fluidRow(
                              column(2),
                              column(8,   includeHTML(rmarkdown::render("intro.Rmd"))),
                              column(2)
                            )
                   ),
                   tabPanel(h4("  Network  "),
                            fluidRow(
                       
                              column(12,  visNetworkOutput("network", height = "850px"))
                        
                            )
                   ),
                   
               tabPanel(h4(" Training dataset  "),
                        
                        fluidRow(
                          column(2),
                          column(8),
                          column(2)
                        )
                        
                        ),
                   
                   
                tabPanel(h4("Query"),
                               fluidRow(
                                 
                                 
                                 column(2,
                                      actionButton("action", label = " 1- Build the network", class="btn btn-success") 
                                        ),
                                 column(2, tags$div(class="alert alert-dismissible alert-info",
                                   h5("",textOutput("nump"))  
                                 )),
                                 
                                 column(2, tags$div(class="alert alert-dismissible alert-info",
                                                    h5(textOutput("logli"))  
                                                    ) ),
                                 
                                 column(2, tags$div(class="alert alert-dismissible alert-info",
                                   h5("",textOutput("mRR")) 
                                 )),
                                 
                                 column(2, tags$div(class="alert alert-dismissible alert-info",
                                   h5("",textOutput("varRR")) 
                                 )),
                                 
                                 column(2, tags$div(class="alert alert-dismissible alert-info",
                                   h5("",textOutput("pRR"))
                                 ))
                                 
                                 
                                      
                                                  ),

                                                  fluidRow(
                                                    column(2, wellPanel(
                                                      textInput("BAR", label = h5("Biochar application rate "), value = ""),
                                                      textInput("NAR", label = h5("Nitrogen application rate"), value = ""),
                                                      textInput("Lat", label = h5("latitude "), value = "")
                                                    )),
                                                    column(2, wellPanel(
                                                      textInput("BpH", label = h5("Biochar pH "), value = ""),
                                                      textInput("HT", label = h5("Highest temperature "), value = ""),
                                                      textInput("BC", label = h5("Biochar C "), value = "")
                                                    
                                                      
                                                    )),
                                                    column(2, wellPanel(
                                                      textInput("BN", label = h5("Biohar N"), value = ""),
                                                      textInput("BA", label = h5(" Biochar ash"), value = ""),
                                                      textInput("BCN", label = h5(" Biochar C/N"), value = "")
                                                  
                                                      
                                                    )),
                                                    column(2, wellPanel(
                                                      textInput("SpH", label = h5("Soil pH"), value = ""),
                                                      textInput("CEC", label = h5("CEC"), value = ""),
                                                      textInput("SOC", label = h5("SOC"), value = "")
                                                     
                                                      
                                                      
                                                    )),
                                                    column(2, wellPanel(
                                                      textInput("Sand", label = h5("Sand"), value = ""),
                                                      textInput("Silt", label = h5("Silt"), value = ""),
                                                      textInput("Clay", label = h5("Clay"), value = "")
                                                      
                                                      
                                                    )),
                                                    column(2, wellPanel(
                                                       selectInput("txts", label = h5("Texture"), 
                                                                  choices = list(" " = 1, "Light" = "Light", "Medium" = "Medium","Heavy"="Heavy"), 
                                                                  selected = 1),
                                                          selectInput("fds", label = h5("Feedstock"), 
                                                                  choices = list(" " = 1, "Nonwood" = "Nonwood", "Wood" = "Wood","Manure"="Manure"), 
                                                                  selected = 1), 
                                                       selectInput("prs", label = h5("Pyrolysis rate"), 
                                                                  choices = list(" " = 1, "fast" = "fast", "slow" = "slow"), 
                                                                  selected = 1)
                                                    ))
                                                    ),
                                                  fluidRow(
                                                    column(6,wellPanel( highchartOutput("hcontainer",height = "500px"))),
                                                    column(6,wellPanel( 
                                                      fluidRow(column(4,textInput("seriesname", label = h5(""), value = "Series name")),
                                                               column(3,br(),actionButton("addev", label = "Add to the comparison", class="btn btn-success")),
                                                               column(3,br(),actionButton("resetbtn", label = "Reset !", class="btn btn-danger"))
                                                               
                                                                      ),
                                                      fluidRow(highchartOutput("compplot",height = "430px")))
                                                     
                                                      ))
                              ),# end about
               tabPanel(h4(" Biochar selector  "),    
                        fluidRow(               
                          column(8, fluidRow( 
                            column(3, wellPanel(
                              textInput("BARMAP", label = h5("Biochar application rate "), value = ""),
                              textInput("NARMAP", label = h5("Nitrogen application rate"), value = ""),
                              selectInput("txts", label = h5("Texture"), 
                                          choices = list(" " = 1, "Light" = "Light", "Medium" = "Medium","Heavy"="Heavy"), 
                                          selected = 1)
                            )),
                            column(3, wellPanel(
                              textInput("BpHMAP", label = h5("Biochar pH "), value = ""),
                              textInput("HTMAP", label = h5("Highest temperature "), value = ""),
                              textInput("BCMAP", label = h5("Biochar C "), value = "")
                              
                              
                            )),
                            column(3, wellPanel(
                              textInput("BNMAP", label = h5("Biohar N"), value = ""),
                              textInput("BAMAP", label = h5(" Biochar ash"), value = ""),
                              textInput("BCNMAP", label = h5(" Biochar C/N"), value = "")
                            )),
                            column(3, wellPanel(
                              selectInput("fdsMAP", label = h5("Feedstock"), 
                                          choices = list(" " = 1, "Nonwood" = "Nonwood", "Wood" = "Wood","Manure"="Manure"), 
                                          selected = 1), 
                              selectInput("prsMAP", label = h5("Pyrolysis rate"), 
                                          choices = list(" " = 1, "fast" = "fast", "slow" = "slow"), 
                                          selected = 1),
                              textInput("BBOX", label = h5(" Bounding box"), value = "")
                            ))
                            )),
                          column(4, HTML( " <div class=\"panel panel-default\">
                                          <div class=\"panel-heading\">Instruction</div>
                                          <div class=\"panel-body\">
                                          <b>
                                          <h5>1-Build the network</h5>
                                          <h5>2-Select the bounding box just by going to your area of interset</h5>
                                          <h5>3-Enter your biochar properties</h5>
                                          <h5>4-Push the calculate button</h5>
                                          </b>
                                          "),
                                 fluidRow(column(4,actionButton("Mapnetbtn", label = "Build the network", class="btn btn-success")),
                                          column(4,actionButton("Mapstartbtn", label = "Calculate", class="btn btn-success"))
                                 ),
                                 HTML( " </div>
                                          </div>"))
                          ),
                        fluidRow(
                          column(12, wellPanel( leafletOutput("mymap",width = "100%",height = "600px")))
                          
                        )
               ),
                   tabPanel(h4(" API  "),    fluidRow(
                     column(2),
                     column(8,   includeHTML(rmarkdown::render("API.Rmd"))),
                     column(2)
                   ) ),
               
               tabPanel(h4(" About  "),    fluidRow(
                     column(2),
                     column(8,   includeHTML(rmarkdown::render("about.Rmd"))),
                     column(2)
                   ) )
))








