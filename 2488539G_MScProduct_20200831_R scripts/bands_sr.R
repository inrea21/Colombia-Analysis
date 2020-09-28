MyData <- read.csv(file="D:/uni/project MSc/Patia/mosaic/Patiadataorig.csv", header=TRUE, sep=",")

# Save an object to a file
saveRDS(MyData, file = "Patiadataorig.rds")

# Restore the object
readRDS(file = "Patiadataorig.rds")

library(randomForest)
library(caret)
library(lattice)
library(ggplot2)
library(doFuture)

### Using multiple cores 
registerDoFuture()

plan(multiprocess, workers = availableCores() - 1)


###### Define and customise random forest
customRF <- list(type = "Regression",
                 library = "randomForest",
                 loop = NULL)





customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2),
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  
  randomForest::randomForest(x, y,
                             mtry = param$mtry,
                             ntree = param$ntree, ...
  )
}





#Predict label
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)

#Predict prob
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")

customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes
customRF$oob <- function(x) {
  out <- switch(x$type,
                regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                classification =  c(1 - x$err.rate[x$ntree, "OOB"],
                                    e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]]))
  names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
  out
}

##################### should add this for importance calculations, which returns %IncMSE

customRF$varImp <- function(object, ...){
  varImp <- randomForest::importance(object, ...)
  if(object$type == "regression") {
    if("%IncMSE" %in% colnames(varImp)) {
      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
    } else {
      varImp <- data.frame(Overall = varImp[,1])
    }
  }
  else {
    retainNames <- levels(object$y)
    if(all(retainNames %in% colnames(varImp))) {
      varImp <- varImp[, retainNames]
    } else {
      varImp <- data.frame(Overall = varImp[,1])
    }
  }
  
  out <- as.data.frame(varImp, stringsAsFactors = TRUE)
  if(dim(out)[2] == 2) {
    tmp <- apply(out, 1, mean)
    out[,1] <- out[,2] <- tmp
  }
  out
}




set.seed(1234)

####### Train the model

control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=100,
                        allowParallel = TRUE,
                        search = 'random')

tunegrid <- expand.grid(.mtry=c(1:8),.ntree=c(500,1000))


################################

rmse_reg <- function(model_obj, testing = NULL, target = NULL) {
  #Calculates rmse for a regression decision tree
  #Arguments:
  # testing - test data set
  # target  - target variable (length 1 character vector)
  yhat <- predict(model_obj, newdata = testing)
  actual <- testing[[target]]
  sqrt(mean((yhat-actual)^2))
}


##################################


# load the data

df <- readRDS('Patiadataorig.rds')
df[ df == "*" ] <- NA
df$Neutral_Detergent <- as.numeric(as.character(df$Neutral_Detergent))
df$Acid_Detergent <- as.numeric(as.character(df$Acid_Detergent))
na.omit(df$F_Acid_Det, df$Neutral_De)              # Vector without NAs
as.numeric(na.omit(df$F_Acid_Det, df$Neutral_De))    # Vector without NAs 

str(df)
head(df)
df
df2 = cbind(df[19],df[23:36])
df3 = cbind(df[20],df[23:36])
df4 = cbind(df[21],df[23:36])
df5 = cbind(df[22],df[23:36])




library(caret)
set.seed(1)
trainIndex <- createDataPartition(df2$DM_g_m2, p = .7,
                                  list = FALSE,
                                  times = 1)
training <- df2[ trainIndex,]
testing <- df2[-trainIndex,]


DM <- train(DM_g_m2~.,data=training, 
            method=customRF, 
            #metric='Rsquared',
            importance = TRUE,
            tuneGrid=tunegrid, 
            trControl=control)

DM
DM$finalModel
DM$results

data.frame(model="customRF", DM$bestTune, RMSE=min(DM$results$RMSE), row.names="")
d1<- plot(DM,main="DM")
d1


d<- plot(DM,main="DM", metric="Rsquared")
d

set.seed(1)

d<- plot(varImp(DM, scale=F), xlab= "%IncMSE", ylab= "Predictor Variables",top = 10, main='DM')
d

(importance(DM$finalModel))


dm_pred <- predict(DM, testing)
head(dm_pred)


postResample(pred = dm_pred, obs = testing$DM_g_m2)

######plot(varImp(DM$finalModel)) useless plot

actuals_preds_dm1 <- data.frame(cbind(actuals=testing$DM_g_m2, predicteds=dm_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds_dm1) 
head(actuals_preds_dm1)

a<- ggplot(testing, aes(x = DM_g_m2, y = dm_pred)) +
  geom_point() +xlab("DM_orig ") + ylab('DM_pred') +
  stat_smooth(method = lm)
a

d<- ggplot(data= varImp(DM), aes(x=rownames(varImp(DM)),y= Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black")+
  geom_point(color='skyblue') + xlab(" Predictor Variables")+
  ggtitle("DM") +  ylab(" Importance (%)")+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
d



plot.data <- as.data.frame(plot(DM$finalModel))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))

options(scipen = 999)
library(ggplot2)
library(scales)
rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees (DM)")
rf.plot
remove(rf.plot, plot.data)

d2<- plot(DM1,main="DM")
d2


#attributes(DM)
#d1<- DM$finalModel
#d1

#t<- DM$resample
#t
#plot(t, )

varImp(DM$finalModel)
varImp(DM, scale=T)


##############################################


library(caret)
set.seed(1)
trainIndex <- createDataPartition(df3$Crude_Protein, p = .7,
                                  list = FALSE,
                                  times = 1)
training <- df3[ trainIndex,]
testing <- df3[-trainIndex,]

CP<- train(Crude_Protein~., data=training, 
           method=customRF, 
           #metric='Rsquared',
           importance = TRUE,
           tuneGrid=tunegrid, 
           trControl=control)
CP
CP$finalModel
CP$results


#CP$bestTune
data.frame(model="customRF", CP$bestTune, RMSE=min(CP$results$RMSE), row.names="")
c1<- plot(CP,main="CP")
c1

c2<- plot(CP,main="CP", metric="Rsquared")
c2

(importance(CP$finalModel))


set.seed(1)
c<- plot(varImp(CP, scale=F),xlab=  "%IncMSE", ylab= "Predictor Variables", top = 10, main='CP')
c

cp_pred <- predict(CP, testing)
head(cp_pred)


postResample(pred = cp_pred, obs = testing$Crude_Protein)


actuals_preds_cp1 <- data.frame(cbind(actuals=testing$Crude_Protein, predicteds=cp_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds_cp1) 
head(actuals_preds_cp1)

f<- ggplot(testing, aes(x = Crude_Protein, y = cp_pred)) +
  geom_point() +xlab("CP_orig ") + ylab('CP_pred') +
  stat_smooth(method = lm)
f


plot.data <- as.data.frame(plot(CP$finalModel))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))

options(scipen = 999)
library(ggplot2)
library(scales)
rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees (CP)")
rf.plot
remove(rf.plot, plot.data)

c<- ggplot(data= varImp(CP), aes(x=rownames(varImp(CP)),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black")+
  geom_point(color='skyblue') + xlab(" Predictor Variables")+
  ggtitle("CP") +  ylab(" Importance (%)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

c


###########################################################

library(caret)
set.seed(1)
trainIndex <- createDataPartition(df4$In_vitro_digestibility, p = .7,
                                  list = FALSE,
                                  times = 1)
training <- df4[ trainIndex,]
testing <- df4[-trainIndex,]


ID<- train(In_vitro_digestibility~., data=training, 
           method=customRF, 
           #metric='Rsquared',
           importance = TRUE,
           tuneGrid=tunegrid, 
           trControl=control)
ID
ID$finalModel
ID$results

#ID$bestTune
data.frame(model="customRF", ID$bestTune, RMSE=min(ID$results$RMSE), row.names="")
b1<- plot(ID, main="IVDMD")
b1


varImp(ID)



b2<- plot(ID,main="IVDMD", metric="Rsquared")
b2

set.seed(1)
b<- plot(varImp(ID, scale=F),xlab= "%IncMSE", ylab= "Predictor Variables", top = 10, main='IVDMD')
b

id_pred <- predict(ID, testing)
head(id_pred)


postResample(pred = id_pred, obs = testing$In_vitro_digestibility)


l<- ggplot(testing, aes(x = In_vitro_digestibility, y = id_pred)) +
  geom_point() +xlab("IVDMD_orig ") + ylab('IVDMD_pred') +
  stat_smooth(method = lm)
l


actuals_preds_id1 <- data.frame(cbind(actuals=testing$In_vitro_digestibility, predicteds=id_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds_id1) 
head(actuals_preds_id1)



plot.data <- as.data.frame(plot(ID$finalModel))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))

options(scipen = 999)
library(ggplot2)
library(scales)
rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees (IVDMD)")
rf.plot
remove(rf.plot, plot.data)


b<- ggplot(data= varImp(ID), aes(x=rownames(varImp(ID)),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black")+
  geom_point(color='skyblue') + xlab(" Predictor Variables")+
  ggtitle("IVDMD") +  ylab(" Importance (%)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))

b



###################################################################

library(caret)
set.seed(1)
trainIndex <- createDataPartition(df5$Ash, p = .7,
                                  list = FALSE,
                                  times = 1)
training <- df5[ trainIndex,]
testing <- df5[-trainIndex,]



ASH <- train(Ash~., data=training, 
             method=customRF, 
             #metric='Rsquared',
             importance = TRUE,
             tuneGrid=tunegrid, 
             trControl=control)
ASH
ASH$finalModel
ASH$results



a1<- plot(ASH, main="ASH")
a1
varImp(ASH$finalModel)
importance(ASH$finalModel)

a<- plot(varImp(ASH, scale=F), xlab= "%IncMSE", ylab= "Predictor Variables", top = 10, main='Ash')
a

a2<- plot(ASH,main="ASH", metric="Rsquared")
a2

ash_pred <- predict(ASH, testing)
head(ash_pred)

n<- ggplot(testing, aes(x = Ash, y = ash_pred)) +
  geom_point() +xlab("ASH_orig ") + ylab('ASH_pred') +
  stat_smooth(method = lm)
n

postResample(pred = ash_pred, obs = testing$Ash)


actuals_preds_ash1 <- data.frame(cbind(actuals=testing$Ash, predicteds=ash_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds_ash1) 
head(actuals_preds_ash1)


plot.data <- as.data.frame(plot(ASH$finalModel))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))

options(scipen = 999)
library(ggplot2)
library(scales)
rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees (ASH)")
rf.plot
remove(rf.plot, plot.data)



a<- ggplot(data= varImp(ASH), aes(x=rownames(varImp(ASH)),y=Overall)) +
  geom_bar(position="dodge",stat="identity",width = 0, color = "black")+
  geom_point(color='skyblue') + xlab("Predictor Variables")+
  ggtitle("ASH") + ylab(" Importance (%)")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))
a


results <- resamples(list(DM = DM, CP= CP, ASH= ASH, IDVMD= ID))
bwplot(results,  metric= "RMSE")
bwplot(results,  metric= "Rsquared")
bwplot(results,  metric= "MAE")

summary(results)

#####################################################


library(cowplot)
library(ggpubr)



ggarrange( a1,c1,b1,d1,
           ncol = 2, nrow = 2)

ggarrange( a,c,b,d,
           ncol = 2, nrow = 2)

ggarrange( Q,E,
           ncol = 2, nrow = 1)


#saveRDS(a1,b1,c1,d1, file = 'RFmodellingresults.rds')

#readRDS(file = 'RFmodellingresults.rds')
