### speciesTF = FALSE is used to override listing species on the right margin.
### this is useful for using this same code to set up other plots

print("Running LCx plot code")
print(input2plot)
cexLAB <- input$labelSize # size of xlab and ylab
cexAXIS <- input$axisSize  # tick annotations size on axis
cexLWD <- input$lineSize  # line width
cexPCH <- ifelse(cexLWD<1,1,cexLWD*.75)  # plot symbol size
xlims <- range(input2plot$logDose)
### just in case, must cover range...
xlims <- 10^xlims
if(useFIT){
  if(all(is.finite(CIbounds)))xlims <- range(c(xlims,CIbounds))
}
if(!is.null(xlimsFORCE)) xlims <- xlimsFORCE
### the par() setup allows for species names
print("Margins")
print(c(input$figH*.15, input$figW*.15, 0, 0)+0.1)
par(mai=c(input$figH*.15, input$figW*.15, 0, 0)+0.1,omi=rep(0,4))
print(c(xlims=xlims))
plotSetupGeneric(inputDF=input2plot,
                 yRange=c(0,1),
                 xRange=xlims,
                 cexLAB=cexLAB,cexAXIS=cexAXIS,cexLWD=cexLWD,
                 plotType=c("CDF","PDF")[1],
                 logscaleTF=TRUE,
                 ptColor="darkgray",
                 xlabSTR=paste0(input$xLab," (",input$units,")"),
                 ylabSTR="Probability")
if(useFIT){
  xVals <- seq(log10(xlims[1]),log10(xlims[2]),length=1000)
  lines(y=predictionModel(params=MLE.Parms,logdoses=xVals,ECx=0.5),x=10^xVals,col=lineColors[1],lwd=cexLWD*1.7)
  if(input$doGrays){
    lines(y=predictionModel(params=MLE.Parms,logdoses=xVals,ECx=0.5),x=10^xVals,col=lineColors[3],lwd=cexLWD,lty=2)
  }
}
if(!useFIT & FALSE){
  #use the nonparametric version -- only makes sense for SSD
  xVals <- quantile(input2plot$responses,probs = seq(0.0001,0.9999,length=10000),type = 8)
  yVals <- seq(0.0001,0.9999,length=10000)
  lines(y=yVals,x=xVals,col=lineColors[1],lwd=cexLWD*1.7)
}
#if(any(input2plot$species=="ADD ONE")){
#  with(subset(input2plot,species=="ADD ONE"),{
#    points(x=responses,y=yVals,cex=cexPCH*sqrt(2),pch=18)
#  })
#}

  with(input2plot,{
    points(x=10^logDose,y=yVals,cex=cexPCH,col="black",pch=pchOpens[1])
    points(x=10^logDose,y=yVals,cex=cexPCH,col=colorList[1],pch=pchSolids[1])
  })


#hcX <- 10^(qlogis(HC.primary,LOO.results.logis[[1]][[1]][5],LOO.results.logis[[1]][[1]][6]))
#abline(h=inputList$ECXvalue.SSD,lty=3)
if(useFIT){
  ECp.value <- resultsDF[1,"ECp"]
  abline(v=ECp.value)
  lines(y=resultsDF[1,"p"]*c(1,1),x=CIbounds,col="red",lwd=3,lend="butt")
}
