#Setting working directory
setwd("D:/Project - 600/finalData")

#loading data with the selected columns
finalData <- read.csv("finalDataset.csv")
View(finalData)

str(finalData)
summary(finalData)

#Saving the missing values of vacant building as test data for final prediction
finalTestData <- finalData[which(finalData$VacantBuilding == ''),]
finalData <- finalData[-(which(finalData$VacantBuilding == '')),]

#replacing all the empty rows of open violation with zero
finalData$Num.Open.Violations[which(is.na(finalData$Num.Open.Violations))] <- 0

#Taking backup of cleaned data 
backup <- finalData
#finalData <- backup

#converting data into categories 
finalData$WaterService <- as.character(finalData$WaterService)
finalData$WaterService[which(finalData$WaterService == '')] <- "unknown"
finalData$WaterService <- as.factor(finalData$WaterService)


finalData$VacantBuilding <- as.character(finalData$VacantBuilding)
finalData$VacantBuilding[which(finalData$VacantBuilding == 1)] <- "Y"
finalData$VacantBuilding <- as.factor(finalData$VacantBuilding)

is.na(finalData$YearBuilt) <- finalData$YearBuilt == 0 
hist(finalData$YearBuilt)


finalData$YearBuilt <- as.character(finalData$YearBuilt)
finalData$YearBuilt[which(finalData$YearBuilt <= 1900)] <- "Very Old"
finalData$YearBuilt[which(finalData$YearBuilt > 1900 & finalData$YearBuilt <= 1975)] <- "Old"
finalData$YearBuilt[which(finalData$YearBuilt > 1975 & finalData$YearBuilt <= 2017 )] <- "New"
finalData$YearBuilt <- as.factor(finalData$YearBuilt)

summary(finalData)


finalData$ZIP <- as.character(finalData$ZIP)
finalData$ZIP[which(startsWith(finalData$ZIP,"132"))] <- "Syracuse"
finalData$ZIP[which(startsWith(finalData$ZIP,"1"))] <- "Outside Syr, In NY"
finalData$ZIP[which(startsWith(finalData$ZIP,"0") | startsWith(finalData$ZIP,"2") | startsWith(finalData$ZIP,"3")  | startsWith(finalData$ZIP,"4") | startsWith(finalData$ZIP,"5") | startsWith(finalData$ZIP,"6") | startsWith(finalData$ZIP,"7") | startsWith(finalData$ZIP,"8") | startsWith(finalData$ZIP,"9"))] <- "Outside NY"
finalData$ZIP <- as.factor(finalData$ZIP)

#Remove unrequired values present in ZIP column using excel
#write.csv(finalData,"backup.csv")
#finalData <- read.csv("backup.csv")
#finalData <- finalData[,-1]

finalData$AssessedValue <-  ifelse(finalData$AssessedValue <= 75000, 'Cheap',
                                   ifelse(finalData$AssessedValue > 75000 & finalData$AssessedValue <= 2000000, 'Moderate','Expensive'))
finalData$AssessedValue <- as.factor(finalData$AssessedValue)

#write.csv(finalData,"Cleaned_Data.csv")

########################################################################
#loading clean data file

finalData <- read.csv("Cleaned_Data.csv")

#Treating NA values

#checking how much % of data is NAs
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(finalData,2,pMiss) #applying to columns

#Census columns have 15% or more of missing data
library(mice)
md.pattern(finalData) #shows the pattern of missing data

library(VIM)
aggr_plot <- aggr(finalData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(finalData), cex.axis=.3,cex.numbers=0.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

library(DMwR)
temp <- knnImputation(finalData,3)
apply(temp,2,pMiss)
View(temp)

finalData <- temp
clean_backup <- finalData
#finalData <- clean_backup


################################
#Logit Model
################################

ldata <- finalData

ldata$VacantBuilding <- ifelse(ldata$VacantBuilding == "Y",1,0)

lm1 <- glm(formula = VacantBuilding ~ LandUse+ Total.Taxes.Owed + Num.Open.Violations + AssessedValue + ZIP + WaterService + YearBuilt + Total_crimes + Occupied_Pr + Vacant_Pr + H_.1.3.P + H.4..P + Owner_Occupied, data = ldata, family = binomial)
summary(lm1)

#selecting a model by taking only the significant predictors
lm2 <- glm(formula = VacantBuilding ~ LandUse + Num.Open.Violations + AssessedValue + ZIP + WaterService + YearBuilt, data = ldata, family = binomial)
summary(lm2)

lm3 <- glm(formula = VacantBuilding ~ Num.Open.Violations + AssessedValue + ZIP + WaterService + YearBuilt, data = ldata, family = binomial)
summary(lm3)

#Based on AIc criteria lm2 is better than lm3. Hence we select lm2 as our model

# Chi-square ( Log-likehood Ratio test)

anova(lm1, lm2, test ="Chisq") # p is not less than 0.05, hence we cannot reject H0 and reduced model is a better fit

#predicting on complete data

predict <- predict(lm2, type='response')
predict <- ifelse(predict < 0.5,0,1)
View(predict)

# Creating a confusion matrix

compTable1 <- data.frame(ldata$VacantBuilding,predict)
colnames(compTable1) <- c('Actual','Predicted')
table(compTable1)
#Accuracy = 0.973

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, ldata$VacantBuilding)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#Using Training and Testing Data
randIndex <- sample(1:dim(ldata)[1])

train_cutpoint2_3 <- floor((2*dim(ldata)[1])/3)
trainData <- ldata[randIndex[1:train_cutpoint2_3],]
View(testData)

testCutpoint <- dim(ldata)[1]-(train_cutpoint2_3+1)
testData <- ldata[randIndex[train_cutpoint2_3+1:testCutpoint],]


lm2 <- glm(formula = VacantBuilding ~ LandUse + Num.Open.Violations + AssessedValue + ZIP + WaterService + YearBuilt, data = trainData, family = binomial)
summary(lm2)

predict_test <- predict(lm2, testData, type='response')
predict_test <- ifelse(predict_test < 0.5,0,1)
View(predict_test)

# Creating a confusion matrix
compTable1 <- data.frame(testData$VacantBuilding,predict_test)
colnames(compTable1) <- c('Actual','Predicted')
table(compTable1)

#Accuracy = 0.973

#ROCR Curve
ROCRpred <- prediction(predict_test, testData$VacantBuilding)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#fitted values

fit <- lm2$fitted.values 
hist(fit)

###########################################################
#Random Forest
###########################################################

library(randomForest)
library(caret)
str(finalData)
View(finalData)
finalData <- finalData[,-9]

#Training and Testing Data
randIndex <- sample(1:dim(finalData)[1])

train_cutpoint2_3 <- floor((2*dim(finalData)[1])/3)
train <- finalData[randIndex[1:train_cutpoint2_3],]

testCutpoint <- dim(finalData)[1]-(train_cutpoint2_3+1)
test <- finalData[randIndex[train_cutpoint2_3+1:testCutpoint],]

rf <- randomForest(VacantBuilding ~ LandUse + Total.Taxes.Owed + Num.Open.Violations + AssessedValue + ZIP + WaterService + YearBuilt + Total_crimes + Occupied_Pr + Vacant_Pr + H_.1.3.P + H.4..P + Owner_Occupied ,data = finalData, na.action = na.exclude)
rf

plot(rf)

#From the graph we see that the error is same from 150 trees. We can choose any number of trees after that. We chose 200


rf <- randomForest(VacantBuilding ~ LandUse + Total.Taxes.Owed + Num.Open.Violations + AssessedValue + ZIP + WaterService + YearBuilt + Total_crimes + Occupied_Pr + Vacant_Pr + H_.1.3.P + H.4..P + Owner_Occupied ,ntree = 200,data = finalData, na.action = na.exclude)
rf

#Prediction on complete data
predict_rf <- predict(rf,finalData)
confusionMatrix(predict_rf,finalData$VacantBuilding)

#Accuracy = 0.997

#Tuning the RF model to avoid over fitting
t <- tuneRF(finalData[,-5],finalData[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            improve = 0.05)

#Here we see that OOB - out of bag error is least at mtry = 3, hence we select that for our model 

#Number of nodes in each tree
hist(treesize(rf),main = "No of nodes in each tree",col = "green")

#Prediction on test data
rf <- randomForest(VacantBuilding~., data = train, mtry = 3,ntree = 200 ,importance = TRUE, proximity = TRUE)
rf

predict_rf <- predict(rf,test)
confusionMatrix(predict_rf,test$VacantBuilding)
#Accuracy = 0.976

vif <- importance(rf)
varImp(rf)
varImpPlot(rf)
varImpPlot(rf,sort = T, n.var = 5, main = "Top 5 variables")
varUsed(rf)

par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(4, 2), xpd=NA)

#partial dependencies
partialPlot(rf,train,WaterService)
partialPlot(rf,train,Num.Open.Violations)
partialPlot(rf,train,LandUse)
partialPlot(rf,train,Total.Taxes.Owed)
partialPlot(rf,train,Total_crimes)
partialPlot(rf,train,ZIP)
partialPlot(rf,train,YearBuilt)
partialPlot(rf,train,AssessedValue)

#When building is vacant
partialPlot(rf,train,WaterService,"Y")
partialPlot(rf,train,Num.Open.Violations,"Y")
partialPlot(rf,train,LandUse,"Y")
partialPlot(rf,train,Total.Taxes.Owed,"Y")
partialPlot(rf,train,Total_crimes,"Y")
partialPlot(rf,train,ZIP,"Y")
partialPlot(rf,train,YearBuilt,"Y")
partialPlot(rf,train,AssessedValue,"Y")

dev.off()

#variable influence barplot
bp <- barplot(t(vif/sum(vif)),las = 2)

###########################################################
#Apriori Rules
###########################################################

library(arules)
library(arulesViz)
apriori.data <- clean_backup
str(apriori.data)

#new_data <- as(trialdf1,"transactions")
unique(apriori.data$Total.Taxes.Owed)
apriori.data$Total.Taxes.Owed <-  ifelse(apriori.data$Total.Taxes.Owed <= 1000, 'Low',ifelse(apriori.data$Total.Taxes.Owed > 1000 & apriori.data$Total.Taxes.Owed <= 10000, 'Medium','High'))
apriori.data$AssessedValue <-  ifelse(apriori.data$AssessedValue <= 75000, 'Low',ifelse(apriori.data$AssessedValue > 75000 & apriori.data$AssessedValue <= 2000000, 'Medium','High'))
apriori.data$Total_crimes <- ifelse(apriori.data$Total_crimes <= 5, 'Low',ifelse(apriori.data$Total_crimes > 20,'High','Medium'))
table(apriori.data$Total_crimes)

apriori.data$ZIP <- as.character(apriori.data$ZIP)
apriori.data$ZIP[which(startsWith(apriori.data$ZIP,"132"))] <- "Syracuse"
apriori.data$ZIP[which(startsWith(apriori.data$ZIP,"1"))] <- "Outside Syr, In NY"
apriori.data$ZIP[which(startsWith(apriori.data$ZIP,"0") | startsWith(apriori.data$ZIP,"2") | startsWith(apriori.data$ZIP,"3")  | startsWith(apriori.data$ZIP,"4") | startsWith(apriori.data$ZIP,"5") | startsWith(apriori.data$ZIP,"6") | startsWith(apriori.data$ZIP,"7") | startsWith(apriori.data$ZIP,"8") | startsWith(apriori.data$ZIP,"9"))] <- "Outside NY"
apriori.data$ZIP <- as.factor(apriori.data$ZIP)

### Apriori ####

# Converting all the varibales into factors to run apriori function
for(i in 1:ncol(apriori.data))
{
  apriori.data[,i] <- as.factor(apriori.data[,i])
}

#aprioriDf <- na.omit(aprioriDf)
#View(aprioriDf)

ruleset <- apriori(apriori.data, parameter = list(support=0.01,confidence=0.3, target='rules'),
                   appearance = list(default='lhs',rhs=('VacantBuilding=Y')))

inspect(ruleset)

importantRules<-ruleset[quality(ruleset)$lift>16]
importantRules
inspect(importantRules)


ruleset2 <- apriori(apriori.data, parameter = list(support=0.5,confidence=0.5, target='rules'),
                    appearance = list(default='lhs',rhs=('VacantBuilding=N')))

inspect(ruleset2)

importantRules2<-ruleset2[quality(ruleset2)$lift>1.0484]
importantRules2
inspect(importantRules2)


#plot(ruleset, method="graph");

plot(importantRules, method="graph", control=list(type="items"));

aprioriDfTransaction <- as(apriori.data, 'transactions')
itemFrequencyPlot(aprioriDfTransaction, support=0.015,col='cyan')


###########################################################
#Regression Model
###########################################################

#using uncategorized data for regression
finalData <- backup

library(DMwR)
temp <- knnImputation(finalData,3)
apply(temp,2,pMiss)

finalData <- temp

# Running a Regression model
str(finalData)
View(finalData)
summary(finalData)
unique(finalData$LandUse)

regression.data <- finalData
str(regression.data)
summary(regression.data)

# Preparing variables for regression. Converting significant variables into numeric and normalizing them.
normalize <- function(x)
{
  (x-min(x))/max(x)-min(x)
}

# Total Taxes Owed

regression.data$Total.Taxes.Owed <-  ifelse(regression.data$Total.Taxes.Owed <= 1000, 1,ifelse(regression.data$Total.Taxes.Owed > 1000 & regression.data$Total.Taxes.Owed <= 10000, 2,3))

hist(regression.data$Total.Taxes.Owed)

unique(regression.data$Total.Taxes.Owed)
summary(regression.data)

# Num.Open Violations

hist(regression.data$Num.Open.Violations)
regression.data$Num.Open.Violations <-  ifelse(regression.data$Num.Open.Violations < 1 , 1,ifelse(regression.data$Num.Open.Violations >= 1 & regression.data$Num.Open.Violations <= 5, 2,3))
View(regression.data)

# Assessed Value

regression.data$AssessedValue <-  ifelse(regression.data$AssessedValue <= 75000, 1,ifelse(regression.data$AssessedValue > 75000 & regression.data$AssessedValue <= 2000000, 2,3))
hist(regression.data$AssessedValue)

# Year Built

hist(regression.data$YearBuilt)
regression.data$YearBuilt <- ifelse(regression.data$YearBuilt <=1900,1,ifelse(regression.data$YearBuilt > 1900 & regression.data$YearBuilt<=1975,2,3))

# Total_crimes

hist(regression.data$Total_crimes)
unique(regression.data$Total_crimes)

# Vacant Building


# Running the regression Model

str(regression.data)
summary(regression.data)

regression.Model.1 <- lm(Vacant_Pr ~ Total.Taxes.Owed + Num.Open.Violations + AssessedValue +
                           YearBuilt + Total_crimes  + H_.1.3.P +
                           H.4..P + Owner_Occupied,data = regression.data)
summary(regression.Model.1)

regression.Model.2 <- lm(Occupied_Pr ~ Total.Taxes.Owed + Num.Open.Violations + AssessedValue +
                           YearBuilt + Total_crimes  + H_.1.3.P +
                           H.4..P + Owner_Occupied,data = regression.data)
summary(regression.Model.2)

# Visualizing the results for Y= Vancant_Pr

temp <- regression.data[,c("Vacant_Pr",'Total.Taxes.Owed',"Num.Open.Violations",
                           'AssessedValue','YearBuilt',"Total_crimes","H_.1.3.P","H.4..P","Owner_Occupied")]
#View(temp)
tempMelt <- melt(temp, id='Vacant_Pr')
#View(tempMelt)

library(ggplot2)
#Heat Map 
ggplot(tempMelt, aes(x=Vacant_Pr,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

# Scatter Plot
ggplot(regression.data, aes(x=YearBuilt, y=Vacant_Pr, size=H_.1.3.P, col=H.4..P,
                            group=Owner_Occupied))+geom_jitter(width = 0.5, height = 1) + geom_abline()


# Visualizing the results for Y = Occupied_Pr

temp1 <- regression.data[,c("Occupied_Pr",'Total.Taxes.Owed',"Num.Open.Violations",
                            'AssessedValue','YearBuilt',"Total_crimes","H_.1.3.P","H.4..P","Owner_Occupied")]
#View(temp)
tempMelt1 <- melt(temp1, id='Occupied_Pr')
#View(tempMelt)

#Heat Map 
ggplot(tempMelt1, aes(x=Occupied_Pr,y=variable, fill=value)) + geom_tile() + scale_fill_gradient(low="yellow", high = "orange")

# Scatter Plot
ggplot(regression.data, aes(x=YearBuilt, y=Occupied_Pr, size=H_.1.3.P, col=H.4..P,
                            group=Owner_Occupied))+geom_jitter(width = 0.5, height = 1) + geom_abline()


