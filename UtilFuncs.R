buildnet<-function(){
  network <- new(Network, "Demo")
  #### importing the network
  network$loadFromString(readLines("../networkdata.txt", n = -1))
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
  return(list(queryRR$getMean( allvars$get("RR", TRUE)), queryRR$getVariance( allvars$get("RR", TRUE))))
}