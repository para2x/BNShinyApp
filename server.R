#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(RCurl)
library(XML)
library(shiny)
library(visNetwork)
library(rgdal)
library(rJava)

library(plyr)
library(dplyr)
source('bayesserver.R')
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
values <- reactiveValues(networkg=NULL,
                         compdf=data.frame(mRR=numeric(0),varRR=numeric(0),PRR=numeric(0),name=character(0)),
                         lRRvar=NULL,lmRR=NULL,lPRR=NULL)

##################################################
################# MAP PANEL
#######################################################
observeEvent(input$Mapnetbtn, {
  ################# setting up the network
  values$networkG<-buildnet()
  output$logli<-renderPrint({
    cat("Logliklihood = -35289.7")
  })
  #number of param
  output$nump<-renderPrint({
    cat("Num. parameters =",ParameterCounter$getParameterCount(values$networkG))
  })
  
})
########################Leaflet
output$mymap <- renderLeaflet({
     leaflet() %>% 
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("Esri.WorldImagery", group = "World Imagery")%>% 
    addProviderTiles("Stamen.TonerLines",  options = providerTileOptions(opacity = 0.35), group = "World Imagery" ) %>%
    addProviderTiles("Stamen.TonerLabels", group = "World Imagery")%>%
    setView(lng = -92.0589, lat = 42.3601, zoom = 5)%>% 
    addLayersControl(
      baseGroups = c("OSM (default)","World Imagery"),
      overlayGroups = c("Probability"),
      options = layersControlOptions(collapsed = FALSE)
    )
       
   })
############# start
  observe({
    if(is.null(input$mymap_bounds))
      return(NULL)
    #if we still need a point
    if(input$mymap_zoom>=15){
      new.point<-input$mymap_bounds
      updateTextInput(session, "BBOX",value = paste0(round(new.point$north,3),",",round(new.point$east,3),",",round(new.point$south,3),
                                                     ",",round(new.point$west,3)))
    }
  })

############# calculate
observeEvent(input$Mapstartbtn, {
  if(is.null(input$BBOX))
    return(NULL)
  
  withProgress(message = 'Calculating ...', value = 0, {
    
  new.point<-input$mymap_bounds
  bbxstr<-paste0(new.point$west,",",new.point$south," ",new.point$east,",",new.point$north)
  incProgress(0.1, detail = paste("Downloading the soil layers")) 
  ##downling the desire bbox
  fileName <- "tempf.gml"
  downstr<-paste0("http://sdmdataaccess.sc.egov.usda.gov/Spatial/SDMWM.wfs?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetFeature&TYPENAME=MapunitPoly&FILTER=<Filter><BBOX><PropertyName>Geometry</PropertyName><Box%20srsName=%27EPSG:4326%27><coordinates>",
         bbxstr,"</coordinates></Box></BBOX></Filter>&SRSNAME=EPSG:3857&OUTPUTFORMAT=GML2")
  download.file(downstr, fileName)
  ###
  soilfile <- readOGR(dsn = fileName, 
                      layer =  ogrListLayers(fileName)[1],disambiguateFIDs=TRUE,require_geomType="wkbPolygon")
  #projecting the map
  soilfile <- spTransform(soilfile, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

  incProgress(0.4, detail = paste("Downloading the soil parameters")) 
  ################################ retieving the soil parameters
  mio<-(ldply(lapply(soilfile$mukey,function(x){soildataret(x)}), rbind))
  mio<-mio[mio$depth<=30,]

  mio$mRR<-NA
  mio$pRR<-NA
  incProgress(0.4, detail = paste("Calculating the probibility")) 
  for(i in 1:nrow(mio)){

    mrr.tmp<-with(mio,
                  quary.BN(values$networkG,c(sandtotal_r[i]/100),
                   c(silttotal_r[i]/100), c(claytotal_r[i]/100), c(om_r[i]),
                   c(ph1to1h2o_r[i]), c(cec7_r[i]), NA,as.numeric(input$BARMAP),
                   as.numeric(input$NARMAP),  as.numeric(input$BpHMAP),  as.numeric(input$HTMAP),   as.numeric(input$BCMAP),
                   as.numeric(input$BNMAP),   as.numeric(input$BAMAP),   as.numeric(input$BCNMAP),input$prsMAP, input$fdsMAP,
                   "1"))
    mio$mRR[i]<-mrr.tmp[[1]]
    mio$pRR[i]<-pnorm(0,mrr.tmp[[1]],sqrt(mrr.tmp[[2]]),lower.tail = F)
  }
  incProgress(0.75, detail = paste("Aggregating")) 
  aggr<-mio%>%group_by(mukey)%>%select(mRR,pRR)%>%summarise_each(funs(mean))
  mrg<-merge(aggr,soilfile@data, by.x="mukey", by.y="mukey")


  incProgress(0.85, detail = paste("Updating the map")) 
  ##################################################
    ### updating the map
  soilfile@data$mRR<-mrg$mRR
  soilfile@data$pRR<-mrg$pRR
  pal <- colorNumeric(
    palette = "RdYlBu",
    domain = soilfile$pRR
  )
  
  leafletProxy("mymap", data = soilfile) %>%
    clearShapes() %>%
    addPolygons(data=soilfile,stroke = FALSE, fillOpacity = 0.3, smoothFactor = 0.5,
                color = ~pal(pRR), group = "Probability") %>%
    addLegend("bottomright", pal = pal, values = soilfile$pRR,
              title = "Probability",    opacity = 1
    )
  ########################################
  })#progress bar
})
##################################################
################# END MAP PANEL
#######################################################


##################################################
################# QUERY PANEL
#######################################################
observeEvent(input$resetbtn,{
  
  if(nrow(values$compdf)==0)
    return(NULL)
  
  values$compdf<-values$compdf[-(1:nrow( values$compdf)),]
    
})
############################## add to the comparison button
observeEvent(input$addev,{
    values$compdf<-rbind(values$compdf,data.frame(mRR=values$lmRR,varRR=values$lRRvar,PRR=values$lPRR,name=input$seriesname))
     output$compplot<-renderHighchart({
  hc<-highchart() %>% 
    hc_chart(type = "column",backgroundColor = NULL) %>% 
    hc_title(text = "Comparison chart") %>% 
    hc_xAxis(categories = names(values$compdf),
             labels = list(style = list(fontSize = "15px")))%>% 
    
    hc_yAxis( lineWidth=0,labels = list(style = list(fontSize = "15px")),
              title = list(text = "value",style = list(fontSize = "15px")))%>%
    hc_exporting(enabled = TRUE) # enable exporting option
  
  for(i in 1:nrow(values$compdf)){
   
    hc<-hc %>% 
      hc_add_series(data = as.numeric(values$compdf[i,1:3]),name = values$compdf[i,4])
  }
  hc

})
 
 })
########################################
################ Building the network 
observeEvent(input$action, {
    ################# setting up the network
  values$networkG<-buildnet()
    output$logli<-renderPrint({
      cat("Logliklihood = -35289.7")
    })
    #number of param
    output$nump<-renderPrint({
      cat("Num. parameters =",ParameterCounter$getParameterCount(values$networkG))
    })

  })
################ showing the chart in single
observe({
  if(is.na(as.numeric(input$Sand)) & is.na(as.numeric(input$Silt)) & is.na(as.numeric(input$Clay)) &
   is.na(as.numeric(input$BCN))&is.na(as.numeric(input$SpH)) & is.na(as.numeric(input$CEC)) & is.na(as.numeric(input$SOC)) &
   is.na(as.numeric(input$BAR))&is.na(as.numeric(input$NAR))&is.na(as.numeric(input$Lat))&is.na(as.numeric(input$BpH))&is.na(as.numeric(input$HT))&
   is.na(as.numeric(input$BC))&is.na(as.numeric(input$BN))&is.na(as.numeric(input$BA))&
   (input$prs=="1")& (input$fds=="1")& (input$txts=="1") & is.null(values$networkg))
  return(NULL)

  res<-quary.BN( values$networkG,
           as.numeric(input$Sand), as.numeric(input$Silt), as.numeric(input$Clay), as.numeric(input$SOC),
           as.numeric(input$SpH),  as.numeric(input$CEC),  as.numeric(input$Lat),  as.numeric(input$BAR),
           as.numeric(input$NAR),  as.numeric(input$BpH),  as.numeric(input$HT),   as.numeric(input$BC),
           as.numeric(input$BN),   as.numeric(input$BA),   as.numeric(input$BCN),input$prs, input$fds,
           input$txts
           )
  #################### plotting the density function
 output$hcontainer <- renderHighchart({
    x <- round(seq(res[[1]]-2.5, res[[1]]+2.5, length.out = 1000),2)
    dens<-round(dnorm(x,res[[1]],sqrt(res[[2]])),2)
    
    yprob<-c(rep(0,min(which(x>0))),dens[min(which(x>0)):length(dens)])
    
    hc <- highchart()%>% 
      hc_chart(backgroundColor = NULL) %>% 
      hc_yAxis(title = list(text = "value",style = list(fontSize = "15px")),
               labels = list(style = list(fontSize = "15px")))%>%
      hc_add_series(name = "Probability of positive ",data=yprob,type="areaspline") %>%
      hc_xAxis(categories=x,
               labels = list(style = list(fontSize = "15px")),
               plotLines = list(
                 list(label = list(text = "No biochar effect",style = list(fontSize = "13px")),
                      color = "#FF0000",
                      zIndex=1,
                      width = 4,
                      value = min(which(x>0))))) %>% 
      hc_add_series(name = "RR density", data =dens,type="line")
    
    
    hc
    
  })
  ################ reprintin the result
 ### keeping the vakues in the global for the chart
 values$lmRR=round(res[[1]],2)
 values$lRRvar=round(res[[2]],2)
 values$lPRR=round(pnorm(0,res[[1]],sqrt(res[[2]]),lower.tail =F),2)
 
  output$mRR<-renderPrint({
    cat("RR mean = ", values$lmRR)
    })
  output$varRR<-renderPrint({
    cat("RR variance = ", values$lRRvar)
  })
  output$pRR<-renderPrint({
    cat("P( RR>0 | ... ) = ",values$lPRR)
  })
}) ## end observe - witing for the input
##################################################
################# END QUERY PANEL
#######################################################


##################################################
#################  OTHERS
#######################################################
############# showing the network  
output$network <- renderVisNetwork({
    nodes <- read.csv("NODES.csv", header=T, as.is=T)
    links <- read.csv("EDGES.csv", header=T, as.is=T)

    nodes$shape <- "dot"  
    nodes$shadow <- TRUE # Nodes will drop shadow
    nodes$title <- nodes$Variable # Text on click
    nodes$label <- nodes$Variable
    nodes$borderWidth <- 3 # Node border width

    
    # We can set the color for several elements of the nodes:
    # "background" changes the node color, "border" changes the frame color;
    # "highlight" sets the color on click, "hover" sets the color on mouseover.
    
    nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
    nodes$color.border <- "black"
    nodes$color.highlight.background <- "orange"
    nodes$color.highlight.border <- "darkred"
    

    # Below we change some of the visual properties of the edges:
    
    #links$width <- 1+links$weight/8 # line width
    links$color <- "gray"    # line color  
    links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
    links$smooth <- TRUE    # should the edges be curved?
    links$shadow <- TRUE    # edge shadow
    
    visNetwork(nodes, links) %>%
      visInteraction(dragView = TRUE,navigationButtons = TRUE)%>%
      visPhysics(solver = "forceAtlas2Based", 
                 forceAtlas2Based = list(gravitationalConstant = -60))%>%
      #visIgraphLayout(,type = "square")%>%
      visLayout(randomSeed = 2069)%>%
      visEdges(width=2)%>% visNodes(font=list(size=20))%>%
      visOptions(selectedBy = "Type",highlightNearest = list(enabled = T, degree = 1, hover = T)) 
  })
  
})


####### utility functions
buildnet<-function(){
    network <- new(Network, "Demo")
    #### importing the network
    network$loadFromString(readLines("networkdata.txt", n = -1))
    return(network)
}

quary.BN<-function(network,sandv=NA,siltv=NA,clayv=NA,socv=NA,soilpHv=NA,cecv=NA,latv=NA,
                   BARv=NA,NARv=NA,BpHv=NA,HTv=NA, BCv=NA, BNv=NA, BAv=NA, BCNv=NA,
                   prv="1",feedstockv="1",textv="1"){
  allvars<-network$getVariables()
  # use the factory design pattern to create the necessary inference related objects
  #factory <- new(LikelihoodSamplingInferenceFactory)
  inference <- new(LikelihoodSamplingInference,network)
  #queryOptions <- factory$createQueryOptions()
  #queryOutput <- factory$createQueryOutput()
  queryOptions<-new(LikelihoodSamplingQueryOptions)
  queryOptions$setSeed(new(Integer, "123"))
  queryOptions$setSampleCount(new(Integer, "123"))
  queryOutput<-new(LikelihoodSamplingQueryOutput)
  # Add some queries
  queryRR <- new(CLGaussian,  allvars$get("RR", TRUE))
  queryDistributions <- inference$getQueryDistributions()
  queryDistributions$add(queryRR)
  
  # Set some evidence
  evidence <- inference$getEvidence()
  ####################### SAND
  if(!is.na(sandv)) {
    evidence$set(allvars$get("sand", TRUE), new(Double, sandv))
  }
  ####################### SILT
  if(!is.na(siltv)){ 
    evidence$set(allvars$get("silt", TRUE), new(Double, siltv))
  }
  ####################### clay
  if(!is.na(clayv)){ 
    evidence$set(allvars$get("clay", TRUE), new(Double, clayv))
  }
  ####################### SOC
  if(!is.na(socv)) {
    evidence$set(allvars$get("Soil.Organic.Carbon", TRUE), new(Double, socv))
  }
  ####################### SoilpH
  if(!is.na(soilpHv)){
    evidence$set(allvars$get("Soil_PH", TRUE), new(Double, soilpHv))
  }
  ####################### CEC
  if(!is.na(cecv)) {
    evidence$set(allvars$get("CEC", TRUE), new(Double, cecv))
  }
  ############################LAT
  if(!is.na(latv)) 
   evidence$set(allvars$get("latitude", TRUE), new(Double, latv))
  ##########################
  if(!is.na(BARv)) 
    evidence$set(allvars$get("BApplication", TRUE), new(Double, BARv))
  ##########################
  if(!is.na(NARv)) 
    evidence$set(allvars$get("NAP", TRUE), new(Double, NARv))
  ##########################
  if(!is.na(BpHv)) 
    evidence$set(allvars$get("biochar_pH", TRUE), new(Double, BpHv))
  ##########################
  if(!is.na(HTv)) 
    evidence$set(allvars$get("highestT", TRUE), new(Double, HTv))
  ##########################
  if(!is.na(BCv)) 
    evidence$set(allvars$get("Biochar.carbon", TRUE), new(Double, BCv))
  ##########################
  if(!is.na(BNv)) 
    evidence$set(allvars$get("Biochar.Nitrogen", TRUE), new(Double, BNv))
  ##########################
  if(!is.na(BAv)) 
    evidence$set(allvars$get("Biochar.Ash", TRUE), new(Double, BAv))
  ########################## biochar C/N
  if(!is.na(BCNv)) 
    evidence$set(allvars$get("Biochar.C.N", TRUE), new(Double, BCNv))
  ########################## croptype
  #if(croptv!="1") 
   # evidence$setState(allvars$get("croptype", TRUE)$getStates()$get(croptv, TRUE))
  ########################## Pyrolysis rate
  if(prv!="1") 
    evidence$setState(allvars$get("PyrolysisType", TRUE)$getStates()$get(prv, TRUE))
  ########################## Feedstock
  if(feedstockv!="1") 
    evidence$setState(allvars$get("feedstock", TRUE)$getStates()$get(feedstockv, TRUE))
  ########################## Feedstock
  if(textv!="1") 
    evidence$setState(allvars$get("texture", TRUE)$getStates()$get(textv, TRUE))
  ######################
  # Execute the query
  inference$query(queryOptions, queryOutput) # note that this can raise an exception (see help for details)
  
  # Read the results of the query
  #print(queryRR$getMean(RRType))
  #print(queryRR$getVariance(RRType))
  return(list(queryRR$getMean( allvars$get("RR", TRUE)),queryRR$getVariance( allvars$get("RR", TRUE))))
}
soildataret<-function(mukey=2747727){
  
  ######### Reteiv soil 
  headerFields =
    c(Accept = "text/xml",
      Accept = "multipart/*",
      'Content-Type' = "text/xml; charset=utf-8",
      SOAPAction = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx/RunQuery")
  
  body = paste('<?xml version="1.0" encoding="utf-8"?>
               <soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
               <soap:Body>
               <RunQuery xmlns="http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx">
               <Query>
               SELECT mapunit.mukey, mapunit.muname,component.cokey,component.mukey,chorizon.cec7_r,
               chorizon.sandtotal_r,chorizon.silttotal_r,chorizon.claytotal_r,chorizon.om_r,chorizon.hzdept_r,
               chorizon.ph1to1h2o_r,chorizon.cokey,chorizon.chkey from mapunit 
               join component on mapunit.mukey=component.mukey
               join chorizon on component.cokey=chorizon.cokey
               where mapunit.mukey=', mukey,';
               </Query>
               </RunQuery>
               </soap:Body>
               </soap:Envelope>')
  reader = basicTextGatherer()
  out<-curlPerform(url = "http://SDMDataAccess.nrcs.usda.gov/Tabular/SDMTabularService.asmx",
                   httpheader = headerFields,  postfields = body,
                   writefunction = reader$update
  )
  
 # xml_data <-read_xml(reader$value())
  xml_doc<-xmlTreeParse(reader$value())
  xmltop = xmlRoot(xml_doc)
  tablesxml<-(xmltop[[1]]["RunQueryResponse"][[1]]["RunQueryResult"][[1]]["diffgram"][[1]]["NewDataSet"][[1]])

  tables<-getNodeSet(tablesxml,"//Table")
 # xmlValue(xmlChildren(tables[[1]])$chkey)
  ##### All datatables below newdataset
  df<-data.frame(
    mukey=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$mukey)  })),
    cokey=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$cokey)  })),
    chkey=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$chkey)  })),
    muname=(sapply(tables,function(x){ xmlValue(xmlChildren(x)$muname)  })),
    cec7_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$cec7_r)  })),
    sandtotal_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$sandtotal_r)  })),
    silttotal_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$silttotal_r)  })),
    claytotal_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$claytotal_r)  })),
    om_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$om_r)  })),
    ph1to1h2o_r=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$ph1to1h2o_r)  })),
    depth=as.numeric(sapply(tables,function(x){ xmlValue(xmlChildren(x)$hzdept_r)  }))
  )
return(df)
}