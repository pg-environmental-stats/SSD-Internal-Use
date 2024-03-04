print("Running LCX analysis")
rvs$AnalysisComplete <- 0
confLevel <- 0.95
#input conf level?  Old tools have, here not doing that
testData <- rvs$finalDF
testData$logDose <- log10(testData$doses)
doseSpacing <- median(diff(testData$logDose[testData$doses>0]),na.rm = TRUE)
testData$logDose[testData$doses==0] <- min(testData$logDose[testData$doses>0],na.rm = TRUE)-1.5*doseSpacing
testData$yVals <- with(testData,responses/sizes)

print(testData)
print(input$modelType)
if(input$modelType=="abbott")predictionModel <<- function(params,logdoses,ECx=0.5){
  #DGT0 <- as.numeric(is.finite(logdoses))
  #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
  #p0 goes to zero when BG parm goes large negative, and goes to 1 when BG parm goes large positive
  #could also recenter this so when BG parm is 0, BG prob is 0.05 (BG prob = 0.5 doesn't make any sense)
  #unlikely to make much of a difference, if any.  Most trouble is associated with interval endpoints
  #exactly on dose values (likelihood cliffs)
  p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
  phat <- p0 + (1-p0)/(1+exp(-qlogis(ECx)-(params["beta"])*(logdoses-params["ldXX"])))
  phat
}
if(input$modelType=="lcx")predictionModel <<- function(params,logdoses,ECx=0.5){
  #DGT0 <- as.numeric(is.finite(logdoses))
  #parameterize background parameter as a logit s.t. no bounds are necessary on the bg rate!
  #p0 goes to zero when BG parm goes large negative, and goes to 1 when BG parm goes large positive
  #could also recenter this so when BG parm is 0, BG prob is 0.05 (BG prob = 0.5 doesn't make any sense)
  #unlikely to make much of a difference, if any.  Most trouble is associated with interval endpoints
  #exactly on dose values (likelihood cliffs)
  #p0 <- exp(params["BG"])/(1. + exp(params["BG"]))
  phat <- 1/(1+exp(-qlogis(ECx)-(params["beta"])*(logdoses-params["ldXX"])))
  phat
}

results <- logisticLCx(
  inputData = testData,
  ECxPercent = input$ECXvalue * 100,
  #ECxPercent = 0.5,
  #confLevelPct = input$confidenceCI,
  confLevelPct = confLevel * 100,
  modelType = input$modelType)
results["x"] <- results["x"]/100
print(results)

### logisticLCx() has code that tries to decide whether to try PLL
### but here, do PLL whenever abbott correction is implemented
### in most cases, the std model is used so in long run this should
### not have major impact on timing
if(input$modelType=="abbott" & 
   length(which(regexpr("PLL",names(results))>0))==0){
  tryPLL <- logistic.abbott.PLL(inputFile=testData,
                              ECx.targets=input$ECXvalue * 100,
                              confidenceCI=confLevel * 100,
                              quietTF=TRUE,
                              modelSTR="abbott")
  print("PLL for abbott")
  print(tryPLL)
}
if(length(FIEhits <- which(regexpr("FIE",names(results))>0))>0){
  CIbounds <- results[FIEhits]
}
print("check 1")
#If PLL interval is present, it was done for a reason -- so use it
if(length(PLLhits <- which(regexpr("PLL",names(results))>0))>0){
  CIbounds <- results[PLLhits]
}

if(input$modelType=="abbott" & length(PLLhits)==0){
  ###in this case, check try PLL
  CI.PLL <- tryPLL[which(regexpr("PLL",names(tryPLL))>0)]
  if(diff(CI.PLL)>diff(CIbounds))CIbounds <- CI.PLL
}

modelSTR <- toupper(input$modelType)
modelRange <- 1
BGrate <- 0

input2plot <- testData
if(input$modelType=="lcx"){
  MLE.Parms <- tail(results,2)
  input2plot <- subset(testData,doses>0)
}
if(input$modelType=="abbott")MLE.Parms <- tail(results,3)
print(MLE.Parms)
if(input$modelType=="abbott"){
  BGparm <- MLE.Parms[which(regexpr("BG",names(MLE.Parms))>0)]
  BGrate <- exp(BGparm)/(1+exp(BGparm))
  modelRange <- 1-BGrate
}

if(modelSTR=="LCX")modelSTR <- "STD"
resultsDF <- data.frame(method="Logistic",modelType=modelSTR,as.data.frame(rbind(c(results[1:2],CIbounds,NA,confLevel))))
print(resultsDF)
names(resultsDF) <- c("Method","ModelType","p","ECp","LowerCL","UpperCL","Trim","ConfLevel")
print(resultsDF)
output$resultsTableSK <- NULL
resultsDF.SK <- NULL
if(input$ECXvalue==0.5){
  resultsSK <- spearmanKarberORIG(subset(testData,doses>0),doPlot = FALSE)
  print(resultsSK)
  resultsDF.SK <- data.frame(method="TSK",modelType="Nonparametric",as.data.frame(rbind(c(0.5,resultsSK[1:4],0.95))))
  names(resultsDF.SK) <- c("Method","ModelType","p","ECp","LowerCL","UpperCL","Trim","ConfLevel")
  #output$resultsTableSK <- shiny::renderTable(expr={resultsDF.SK},bordered = TRUE,na = "N/A")
}
finalTable <- rbind(resultsDF,resultsDF.SK)
print(finalTable)

pdf(file = "LCxplotOutput.pdf",width = input$figW,height = input$figH)
newLims <- 10^((range(input2plot$logDose,na.rm = TRUE))-c(0.1,0)*diff((range(input2plot$logDose,na.rm = TRUE))))
useFIT <- TRUE
xlimsFORCE <- newLims
yMaxFORCE <- NULL
source("LCxplotCode.R",local = TRUE)
dev.off()


print("Writing XLSX file")
source("write2xlsxLCx.R",local = TRUE)
print("LCx analyses complete")
print("LCx analyses complete")
print("LCx analyses complete")

rvs$AnalysisComplete <- 1
