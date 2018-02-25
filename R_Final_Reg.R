

##stepwise regression shows there are 

library(caret)
library(pls)
library(e1071)
library(MASS)
library(car)
library(rpart)
##read gp1

datagp1<- read.csv(file.choose())
##dropping 1st index column
datagp1<-datagp1[,!names(datagp1) %in% c('X','X.1')]
head(datagp1)

summary(datagp1)##summary shows no null values



library(corrplot)

corrplot(cor(datagp1), method="circle",shade.col=NA, tl.col = "black", tl.srt=45)

##data splitting


###create training and test data set
###set 75 percent of rows for training and rest for test
bound<-floor(0.75*nrow(datagp1))
data.train <- datagp1[1:bound, ]            
data.test <- datagp1[(bound+1):nrow(datagp1), ] 
nrow(data.test)
nrow(data.train)
dataTrainX<-data.train
dataTestX<-data.test

##reg$rowno <- 1:dim(reg)[1]

##train <- subset(reg,rowno <= dim(reg)[1]*0.7)

##test <- subset(reg,rowno > dim(reg)[1]*0.7)


cor.mtest(datagp1)

stepwise<- lm(medv ~ .,data = dataTrainX)

summary(stepwise)

dataTrainX$pred <- predict(stepwise, newdata = dataTrainX)
dataTestX$pred <- predict(stepwise, newdata = dataTestX)
rmse_fun()
##r squared error - 4.6
##rmse_fun()
##[1] 4.57337 8.25498

##rm,age,dis,tax,ptratio,black,lstat are the significant factors identified using summary

##validating the same by using those factors and checking whether the same is available for the combinations

stepwise_imp<- lm(medv ~ rm+age+dis+tax+ptratio+black+lstat,data = dataTrainX)

summary(stepwise_imp)

dataTrainX$pred <- predict(stepwise_imp, newdata = dataTrainX)
dataTestX$pred <- predict(stepwise_imp, newdata = dataTestX)
rmse_fun()
##r squared error - 5.0
##rmse_fun()
##[1] 4.966165 6.220548

##from summary the factor black  seems to be less important or significant

stepwise_final <- lm(medv ~ rm+age+dis+ptratio,data = dataTrainX)

summary(stepwise_final)

##r squared error - 5.45

dataTrainX$pred <- predict(stepwise_final, newdata = dataTrainX)
dataTestX$pred <- predict(stepwise_final, newdata = dataTestX)
rmse_fun = function() { return(c(mean( (dataTrainX$pred - dataTrainX$medv)^2, na.rm = TRUE ) ^0.5, mean( (dataTestX$pred - dataTestX$medv)^2, na.rm = TRUE ) ^0.5))}

rmse_fun()

##train error 5.41,test error - 8.41

##all factors are having 3* ratings in the model.

step(lm(medv ~.,data = dataTrainX),direction = "forward")

pairs(~ rm+age+dis+ptratio,data = dataTrainX)


##stepwise regression

model1 <- lm(medv ~ 1,data = dataTrainX)
model2 <- lm(medv ~ .,data = dataTrainX)
step(model1 , scope = list(lower = model1,upper = model2),direction = "forward")
step(model2,scope = list(lower = model2,upper = model1),direction = "backward")
##AIC values are minimum when the model has all the available factors,this seems to be less reliable.

###do a scatterplot matrix to  see linearity
library(lattice)
splom(datagp1)
##interpretation seems to be difficult with this kind of graph.
nrow(datagp1)
head(datagp1)

head(dataTrainX)


###check skewness 
skewness(dataTrainX$rad) ##3.06472
skewness(dataTrainX$age)## - 0.27
skewness(dataTrainX$ptratio) ## - 0.36
skewness(dataTrainX$dis)##0.68


histogram(dataTrainX$rad)
histogram(dataTrainX$age)
##Since there are skewness in the data we are transforming using Box Cox method of preprocessing

###apply box cox transformation
boxcox<-preProcess(dataTrainX,method ="BoxCox") 
dataTrainXtrans<-predict(boxcox,dataTrainX)
head(dataTrainXtrans)


hist(dataTrainXtrans$rad)
skewness(dataTrainXtrans$rad) ## Skewness is removed now ## skew val - 0.49
hist(dataTrainX$crim) ##crim before transformation

hist(dataTrainXtrans$crim)## crim after transformation

datatestXtrans<-predict(boxcox,dataTestX)
head(datatestXtrans)
hist(datatestXtrans$medv)
hist(dataTestX$medv)

###create training data
trainingData<-dataTrainXtrans
trainingData<-dataTrainX

##testingData <- datatestXtrans
##testingData <- dataTestX
##rm(testingData)
##head(trainingData)

rm(trainingData)
###fit the model-OLS
model<-lm(medv~.,data=trainingData)
summary(model)
defaultSummary(model)
par(mfrow=c(1,1))
plot(model)

###predict values
pred1<-predict(model,datatestXtrans)
###create obs,pred data frame
df<-data.frame(obs=datatestXtrans$medv,pred=pred1)
df
defaultSummary(df)
##higher RMSE should select appropriate varaibles to build model.


###cross-validation
ctrl<-trainControl(method="cv",n=10)
set.seed(100)
tmp<-subset(dataTrainXtrans,select =-medv)
head(tmp)
modcv<-train(x=tmp,y=dataTrainXtrans$medv,method="lm",trControl =ctrl)
print(modcv)
##rmse - 0.02

##r-sq - 0.78

train_control <- trainControl(method="boot", number=100)
# train the model
model_rprt <- train(x=tmp,y=dataTrainXtrans$medv,method="rpart",trControl =train_control)
print(model_rprt)
##cp value is 0.06,with RMSE - 0.0296               
##r - sq - 0.59

###check for multicollinearality
library(car)
vif(model) ##nox and rad has score greater than 4 therefore it is not good to include those in the model.
###vif levels shows collinearity in the dataset
###use ridge regression to check for estimate variation
library(MASS)

ridge<-lm.ridge(medv~.,data=trainingData,lambda =seq(0,150,by=12))
plot(ridge)
summary(ridge)
###ridge estimates and plot suggest multicollinarity exists
##but it does not cause significant change in the 
###estimate values.To estimate parameters by reducing correlation, a PCA is done


###pca analysis 
pca<-datagp1
###standardize independent variables
x<-subset(pca,select=-medv)
head(x)
x<-scale(x)
###center the dependent variable
y<-pca$medv
y<-scale(y,scale =F)
###do pca on indepenedent variables
comp<-princomp(x,cor =F,scores =T)
comp
plot(comp)
biplot(comp,scale = 0)

plot(comp,type = 'l')
summary(comp)
###three principal components explain 95% of the total variance
comp$scores
comp$loadings

pcr<-pcr(medv~.,data=trainingData,validation="CV")
summary(pcr)
###choose nine components for prediction
xpcr=subset(datatestXtrans,select=-medv)
pcrpred<-predict(pcr,xpcr,ncomp =3)

pcrdf<-data.frame(obs=datatestXtrans$medv,pred=pcrpred)
###find rmse
rmsepcr<-sqrt(mean((pcrdf$obs-pcrdf$medv.9.comps)^2))
###rmse is reduced to  0.87

###pls regression is a better variation of PCR.
##It accounts for the variation in response when selecting weights
###use pls package, plsr function

plsFit<-plsr(medv~.,data=trainingData,validation="CV")
###predict first five MEDV values using 1 and 2 components
pls.pred<-predict(plsFit,datatestXtrans[1:5,],ncomp=1:3)
summary(plsFit)
validationplot(plsFit,val.type ="RMSEP")

pls.RMSEP<-RMSEP(plsFit,estimate="CV")
plot(pls.RMSEP,main="RMSEP PLS",xlab="Components")
min<-which.min(pls.RMSEP$val)
min
summary(pls.RMSEP)

points(min,min(pls.RMSEP$val),pch=1,col="red")
plot(plsFit, ncomp=9, asp=1, line=True)

###use 9 components
pls.pred2<-predict(plsFit,datatestXtrans,ncomp=9)
pls.eval<-data.frame(obs=datatestXtrans$medv,pred=pls.pred2[,1,1])
summary(pls.eval)
##rpart model

model2<-rpart(medv ~ .,data=trainingData)
summary(model2)

rpart.plot(model2)


###predict values
pred<-predict(model2 , datatestXtrans,ncomp= 9)
###create obs,pred data frame
df2<-data.frame(obs = datatestXtrans$medv,pred=pred)
defaultSummary(pls.eval)
df2
defaultSummary(df2)



