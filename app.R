#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
suppressPackageStartupMessages({
  library(shiny)
  library(highcharter)
  library(rJava)
  library(dplyr)
})

source('UtilFuncs.R')$value
source('bayesserver.R')$value
options(shiny.trace = TRUE)
# Define UI for application that draws a histogram
ui <- fluidPage(
  br(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(width = 1,
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
                  6,
                  wellPanel(
                    textInput("SpH", label = h5("Soil pH"), value = ""),
                    textInput("CEC", label = h5("CEC (cmolc/kg)"), value = ""),
                    textInput("SOC", label = h5("SOC (%) "), value = ""),
                    sliderInput(
                      "Sand",
                      label = h5("Sand (%) "),
                      min = 0,
                      max = 100,
                      value = 50
                    ),
                    sliderInput(
                      "Silt",
                      label = h5("Silt (%) "),
                      min = 0,
                      max = 100,
                      value = 30
                    ),
                    sliderInput(
                      "Clay",
                      label = h5("Clay (%) "),
                      min = 0,
                      value = 20,
                      max = 100
                    )
                    
                    
                    
                  )
                ),
                column(6,        h4("Biochar"),
                       fluidRow(column(
                         12, wellPanel(
                           textInput("BpH", label = h5("Biochar pH "), value = ""),
                           textInput(
                             "HT",
                             label = h5("Highest temperature  (C) "),
                             value = ""
                           ),
                           textInput("BC", label = h5("Biochar C  (%) "), value = ""),
                           textInput("BCN", label = h5(" Biochar C/N"), value = "")
                         ),
                         h4("Add to chart"),
                         textInput("seriesname", label = h5(""), value = "Series name"),
                         actionButton("addev", label = "Predict !", class="btn btn-success btn-block"),br(),
                         actionButton("resetbtn", label = "Reset", class="btn btn-danger btn-block")
                       )))))
)


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
      mainPanel(width = 11,

        fluidRow(
          column(12,
            fluidRow(highchartOutput("compplot",height = "600px"))
            
          ))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues(networkg=NULL,
                           compdf=data.frame(mRR=numeric(0),varRR=numeric(0),PRR=numeric(0),name=character(0)),
                           lRRvar=NULL,lmRR=NULL,lPRR=NULL)
  
  values$networkG<-buildnet()
  
  observeEvent(input$addev,{
       
    #Check the inputs
    if(
       is.na(as.numeric(input$BCN))&is.na(as.numeric(input$SpH)) & is.na(as.numeric(input$CEC)) & is.na(as.numeric(input$SOC)) &
       is.na(as.numeric(input$BpH))&is.na(as.numeric(input$HT))&
       is.na(as.numeric(input$BC))&
       is.null(values$networkg))
      return(NULL)
    
    
     tryCatch({

      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,{
                   
                     #.... some operation here
                     incProgress(1 / 15) # As we move forward with our calc let's increase this
                     res<-quary.BN( values$networkG,
                                    as.numeric(input$Sand)/100, as.numeric(input$Silt)/100, as.numeric(input$Clay)/100, as.numeric(input$SOC),
                                    as.numeric(input$SpH),  as.numeric(input$CEC),  NA,  NA,
                                    NA,  as.numeric(input$BpH),  as.numeric(input$HT),   as.numeric(input$BC),
                                    NA,   NA,   as.numeric(input$BCN),"1", "1","1"
                     )

                     ################ reprintin the result
                     ### keeping the vakues in the global for the chart
                     values$lmRR=round(res[[1]],2)
                     values$lRRvar=round(res[[2]],2)
                     values$lPRR=round(pnorm(0,res[[1]],sqrt(res[[2]]),lower.tail =F),2)
                     
                 
                     values$compdf<-rbind(values$compdf,
                                          data.frame(`Mean Response`=values$lmRR,
                                                     `Variance`=values$lRRvar,
                                                     `Probability`=values$lPRR,
                                                     name=input$seriesname))
                     
                     output$compplot<-renderHighchart({
                       hc<-highchart() %>% 
                         hc_chart(type = "column") %>% 
                         hc_title(text = "Comparison chart") %>% 
                         hc_xAxis(categories = names(values$compdf),
                                  labels = list(style = list(fontSize = "15px")))%>% 
                         
                         hc_yAxis(max=1, lineWidth=0,labels = list(style = list(fontSize = "15px")),
                                   title = list(text = "value",style = list(fontSize = "15px")))%>%
                         hc_exporting(enabled = TRUE) # enable exporting option
                       
                       for(i in 1:nrow(values$compdf)){
                         
                         hc<-hc %>% 
                           hc_add_series(data = as.numeric(values$compdf[i,1:3]),name = values$compdf[i,4])
                       }
                       hc %>%
                         hc_add_theme(hc_theme_elementary(yAxis = list(gridLineColor ="#9a9d9e",title = list(style = list(fontSize=17)),
                                                                       labels = list(style = list(fontSize=17))),
                                                          xAxis = list(gridLineColor ="#9a9d9e",title = list(style = list(fontSize=17)),
                                                                       labels = list(style = list(fontSize=17)))
                         ))
                       
                     })
                     
                     output$mRR<-renderPrint({
                       cat("RR mean = ", values$lmRR)
                     })
                     output$varRR<-renderPrint({
                       cat("RR variance = ", values$lRRvar)
                     })
                     output$pRR<-renderPrint({
                       cat("P( RR>0 | ... ) = ",values$lPRR)
                     })
                   })
      #Signaling the success of the operation
    
    },
    error = function(e) {
      showModal(modalDialog(
        title = "Error",
        conditionMessage(e)
      ))

    })

  }) 
  
  #Reset button
  observeEvent(input$resetbtn,{
    values$compdf<-NULL
    output$compplot<-renderHighchart({
      hc<-highchart() 
      hc
      
    })
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

