
library(xlsx)
setwd("~/R Project_Clothes Manufacturing Outlet")
attributeClotheOrg = read.xlsx('Attribute DataSet.xlsx',header = TRUE,sheetName = 'Sheet1')
head(attributeClotheOrg)


attributeClothe= na.omit(attributeClotheOrg)
model = glm(Recommendation ~ Style + Price + Rating+Size+Season+NeckLine+SleeveLength+waiseline+Material+FabricType+Decoration+Pattern.Type,data = attributeClothe)
summary(model)


y = attributeClothe$Recommendation
result <- predict(model,attributeClothe[,2:13],type = "response") 

library(ROCR) 
rocrPredic <- prediction(result,y) 
rocrPerf  <- performance(rocrPredic,"tpr","fpr") 
plot(rocrPerf,colorize=TRUE, print.cutoffs.at=seq(0.1,by=0.1)) 
train_PredSurvived <- ifelse(result > 0.4,1,0) 
table(predicted=train_PredSurvived,actualdata=y)

#library(caret)
#confusionMatrix(train_PredSurvived,y)

totalSales = read.csv('Total Sales.csv',header = TRUE)

library(forecast)
matrixTotalSales <- as.matrix(totalSales[,-1]) 
numericVector <-  as.numeric(as.vector(matrixTotalSales)) 
timeseries <- ts(numericVector, start = 1,frequency = 5) 
fit <- auto.arima(timeseries) 
summary(fit)

forecast(fit,5)
plot(forecast(fit,5))


indTotalSales <- read.csv("Dress Sales_Remove Duplicate ID.csv") 
indTotalSales[is.na(indTotalSales)] <- 0
head(indTotalSales)

dataIndTotalSales = indTotalSales[,-1]

i = 0 
n = nrow(dataIndTotalSales) 
tmp = 0 

while(i < n) {   
  matrixIndTotalSales <- as.matrix(dataIndTotalSales[i+1,])   
  numMatrixIndTotalSales <- as.numeric(as.vector(matrixIndTotalSales))   
  timeseries <- ts(numMatrixIndTotalSales, start = 1,frequency = 5)   
  fit <- auto.arima(timeseries)   
  predictedValue <-  as.data.frame(forecast(fit,3))   
  productwithpredictedvalue <- data.frame(dressid=indTotalSales[i+1,1],forecast1stday=predictedValue$`Point Forecast`[1],
                                          forecast2ndday=predictedValue$`Point Forecast`[2],
                                          forecast3rdday=predictedValue$`Point Forecast`[3])
 
  tmp <- rbind(tmp,productwithpredictedvalue)  
  i <- i+1   
  print(tmp) 
} 

View(tmp) 

dressSalesWTotalSales = read.csv('Dress Sales with Total Sales.csv')
attributeClotheOrg = read.csv('Attribute DataSet.csv')
indDressWTotalSales=cbind(attributeClotheOrg, Total.Sales=dressSalesWTotalSales$Total.sales)
head(indDressWTotalSales)

TestForStyle <- aov(Total.Sales ~ Style, data = indDressWTotalSales)
summary(TestForStyle)

TestForSeason <- aov(Total.Sales ~ Season, data = indDressWTotalSales)
summary(TestForSeason)

TestForMaterial <- aov(Total.Sales ~ Material, data = indDressWTotalSales)
summary(TestForMaterial)

lmModel=lm(Total.Sales ~ Style+Season+Material, data = indDressWTotalSales)
summary(lmModel)

comStyPrModel <- lm(Total.Sales ~ Style + Price, data = indDressWTotalSales)
summary(comStyPrModel) 



modeluseAll <- lm(Total.Sales~.-Dress_ID,data = indDressWTotalSales) 
summary(modeluseAll) 


cor.test(Total.Sales~Rating,data= indDressWTotalSales)






