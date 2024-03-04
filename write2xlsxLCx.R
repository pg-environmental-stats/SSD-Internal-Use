wb <- openxlsx::createWorkbook()
addWorksheet(wb = wb, sheetName = "Data Listing", gridLines = FALSE)

# Output the raw data
unitSTR <- paste0("(",input$units,")")
writeData(wb = wb,sheet = 1,x = testData[,c("doses","responses","sizes")],
          startCol = 1,startRow = 3,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = data.frame(X1="Conc",X2="Resp Count",X3="Group Size"),
          startCol = 1,startRow = 1,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = data.frame(X1=unitSTR),
          startCol = 1,startRow = 2,colNames = FALSE)


headerStyle <- createStyle(fontSize = 20,textDecoration = "bold")

getPlacesFMT <- function(x){
  x <- abs(na.omit(x))
  if(all(x==0))return("0")
  if(all(x==round(x)))return("0")
  x <- x[x>0]
  #print(x)
  # the large subtract value, the more decimal places will be printed
  places2print <- floor(min(log10(x)))-2
  #print(places2print)
  places2print <- ifelse(places2print<0,yes = abs(places2print),no = 0)
  #print(places2print)
  if(places2print<=0)return("0")
  if(places2print >0)return(paste0("0.",paste0(rep("0",places2print),collapse = "")))
}
#checking
#getPlacesFMT(testData$responses)
#getPlacesFMT(testData$responses/1000)
#getPlacesFMT(testData$responses*1000)
print("Writing LCx XLSX file")
print(testData)
numFMT <- openxlsx::createStyle(fontSize=18,numFmt = getPlacesFMT(testData$doses))
print(c(doseFMT=getPlacesFMT(testData$doses)))
### Format the numeric values (decimal places) and italics for species
addStyle(wb = wb,sheet = 1,style = numFMT,rows = 2+(1:nrow(testData)),
         cols = rep(1,nrow(testData)))
numFMT <- openxlsx::createStyle(fontSize=18,numFmt = getPlacesFMT(testData$responses))
print(c(respFMT=getPlacesFMT(testData$responses)))
addStyle(wb = wb,sheet = 1,style = numFMT,rows = 2+(1:nrow(testData)),
         cols = rep(2,nrow(testData)))
numFMT <- openxlsx::createStyle(fontSize=18,numFmt = getPlacesFMT(testData$sizes))
print(c(sizeFMT=getPlacesFMT(testData$sizes)))
addStyle(wb = wb,sheet = 1,style = numFMT,rows = 2+(1:nrow(testData)),
         cols = rep(3,nrow(testData)))

### Start col for results
SC.res <- 6
RES.DF <- as.data.frame(finalTable)
#Write the actual results starting in row 3, header added next
#8 columns with params, 6 without
RES.DF.noparms <- RES.DF
writeData(wb = wb,sheet = 1,x = RES.DF.noparms,startCol = SC.res,startRow = 3,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("Method","Model Type","p","ECp","LowerCL","UpperCL","Trim Fract","Confidence Level"))),
          startCol = SC.res,startRow = 1,colNames = FALSE)
writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("","","",unitSTR,unitSTR,unitSTR,"",""))),
          startCol = SC.res,startRow = 2,colNames = FALSE)

#params to right, for completeness
writeData(wb = wb,sheet = 1,x = rbind(MLE.Parms),startCol = SC.res+10,startRow = 3,colNames = FALSE)
if(input$modelType=="lcx"){
  writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("Parameters:","Beta","ECp"))),
          startCol = SC.res+9,startRow = 1,colNames = FALSE)
}
if(input$modelType=="abbott"){
  writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("Parameters:","BG","Beta","ECp"))),
            startCol = SC.res+9,startRow = 1,colNames = FALSE)
}
if(FALSE)writeData(wb = wb,sheet = 1,x = as.data.frame(rbind(c("(log scale)","(log scale)"))),
          startCol = SC.res+10,startRow = 2,colNames = FALSE)

### format numeric output
print(RES.DF.noparms)
for(i in SC.res + 2:(ncol(RES.DF.noparms)-1)){
  print(c(numFMT=getPlacesFMT(na.omit(RES.DF.noparms[,i-(SC.res-1)]))))
  colStyle <- createStyle(
    fontSize = 18,
    numFmt = getPlacesFMT(na.omit(RES.DF.noparms[,i-(SC.res-1)]))
  )
  addStyle(wb = wb,sheet = 1,style = colStyle,rows = 2+(1:nrow(RES.DF.noparms)),cols = i)
}
print(MLE.Parms)
for(i in 1:length(MLE.Parms)){
  colStyle <- createStyle(
    fontSize = 18,
    numFmt = getPlacesFMT(MLE.Parms[i])
  )
  addStyle(wb = wb,sheet = 1,style = colStyle,rows = 3,cols = i+SC.res+10-1)
}

# row label of results
addStyle(wb = wb,sheet = 1,style = createStyle(fontSize=18),
         rows = 2+(1:nrow(RES.DF)),cols = SC.res+c(0,1),gridExpand = TRUE,stack = TRUE)
# all first row is bold/size
addStyle(wb = wb,sheet = 1,style = headerStyle,rows=1,cols = 1:20,stack = TRUE)
# right-align numeric headers
addStyle(wb = wb,sheet = 1,style = createStyle(halign = "right"),rows=1,cols = c(1:3,(SC.res+2):20),stack = TRUE)
# right-align units in 2nd row
addStyle(wb = wb,sheet = 1,style = createStyle(fontSize=18,halign = "right"),rows=2,cols = 1:20,stack = TRUE)

setColWidths(
  wb = wb,
  sheet = 1,
  cols = 1:20,
  widths = "auto")
#the p column needs extra width from auto sizing
setColWidths(
  wb = wb,
  sheet = 1,
  cols = SC.res+2,
  widths = 10)





saveWorkbook(wb = wb,file = "LCxoutput.xlsx",overwrite = TRUE)
