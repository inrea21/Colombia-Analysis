setwd('D:/uni/project MSc/Patia/mosaic')


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




set.seed(1)

####### Train the model

control <- trainControl(method="repeatedcv", 
                        number=10, 
                        repeats=100,
                        allowParallel = TRUE,
                        search = 'random')

tunegrid <- expand.grid(.mtry=c(1:8),.ntree=c(500,1000))


##################################

library(raster)
library(sf)
library(mapview)
library(rgdal)

ps<- stack('clipAOI.tif')
names(ps)
names(ps) <- c("Blue", "Green", "Red", "NIR")

NLI<- stack("NLI.tif")
names(NLI)
names(NLI)<- "NLI"

SAVI<- stack("SAVI.tif")
names(SAVI)
names(SAVI)<- "SAVI"

MSR<- stack("MSR.tif")
names(MSR)
names(MSR)<- "MSR"

DVI<- stack("DVI.tif")
names(DVI)
names(DVI)<- "DVI"

RDVI<- stack("RDVI.tif")
names(RDVI)
names(RDVI)<- "RDVI"

Red<- stack("Red.tif")
names(Red)
names(Red)<- "Red"


plot(ps)

plotRGB(ps, 3 , 2 ,1,scale= 255, stretch= 'lin')

aoiBoundary <- readOGR("D:/uni/project MSc/Patia/MyProject",
                            "AOI")

NLIclip<- crop(NLI, aoiBoundary)
plot(NLIclip)

MSRclip<- crop(MSR, aoiBoundary)
plot(MSRclip)

SAVIclip<- crop(SAVI, aoiBoundary)
plot(SAVIclip, add= T)

DVIclip<- crop(DVI, aoiBoundary)
plot(DVIclip)

RDVIclip<- crop(RDVI, aoiBoundary)
plot(RDVIclip)

Redclip<- crop(Red, aoiBoundary)
plot(Redclip) 

df <- readRDS('Patiadataorig.rds')
df[ df == "*" ] <- NA
df$Neutral_Detergent <- as.numeric(as.character(df$Neutral_Detergent))
df$Acid_Detergent <- as.numeric(as.character(df$Acid_Detergent))
na.omit(df$F_Acid_Det, df$Neutral_De)              # Vector without NAs
as.numeric(na.omit(df$F_Acid_Det, df$Neutral_De))    # Vector without NAs 

str(df)
head(df)
df

###################### VI+ Bands ##################

df2 = cbind(df[19],df[23:26],df[37:52])
df3 = cbind(df[20],df[23:26],df[37:52])
df4 = cbind(df[21],df[23:26],df[37:52])
df5 = cbind(df[22],df[23:26],df[37:52])


####################### ALL Predictors #################


df2 = cbind(df[19],df[23:52])
df3 = cbind(df[20],df[23:52])
df4 = cbind(df[21],df[23:52])
df5 = cbind(df[22],df[23:52])


############################## DM###################

set.seed(1)
trainids<- createDataPartition(df2$DM_g_m2 , list= F, p= 0.7)
trainDat<- df2[trainids,]
testDat<- df2[-trainids,]
nrow(trainDat)
nrow(testDat)


DM <- train(DM_g_m2~MSR, data= trainDat, 
            method=customRF, 
            #metric='Rsquared',
            importance = TRUE,
            tuneGrid= tunegrid, 
            trControl= control)

plot(DM)

dm_pred <- predict(DM, testDat)
mean(dm_pred)


p<- predict(MSRclip, DM)

par(mfrow=c(1,1), mai=c(0.5,0.5,0.5,0.5))

plot(p, box = FALSE,
     ylab="Northing (m)", xlab= "Easting (m)", cex.axis=0.75)
title("DM (g/m²)", line = 1)


library(prettymapr)

addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.4,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotepsg = NULL, widthhint = 0.3,
            unitcategory = "metric", htin = 0.1, padin = c(0.05, 0.08),
            style = "bar", bar.cols = c("black", "white"), lwd = 0.5,
            linecol = "black", tick.cex = 1, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomleft")


######################## CP ##############################

set.seed(1)
trainids<- createDataPartition(df3$Crude_Protein , list= F, p= 0.7)
trainDat<- df3[trainids,]
testDat<- df3[-trainids,]
nrow(trainDat)
nrow(testDat)


CP <- train(Crude_Protein~NLI, data= trainDat, 
            method=customRF, 
            #metric='Rsquared',
            importance = TRUE,
            tuneGrid= tunegrid, 
            trControl= control)

plot(CP)


cp_pred <- predict(CP, testDat)
mean(cp_pred)

p1<- predict(NLIclip, CP)

plot(p1, box = FALSE,
     ylab="Northing (m)", xlab= "Easting (m)", cex.axis=0.75)
title("CP (%)", line = 1)


addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.4,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotepsg = NULL, widthhint = 0.3,
            unitcategory = "metric", htin = 0.1, padin = c(0.05, 0.08),
            style = "bar", bar.cols = c("black", "white"), lwd = 0.5,
            linecol = "black", tick.cex = 1, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomleft")




############################## IVDMD #########################

set.seed(1)
trainids<- createDataPartition(df4$In_vitro_digestibility , list= F, p= 0.7)
trainDat<- df4[trainids,]
testDat<- df4[-trainids,]
nrow(trainDat)
nrow(testDat)


ID <- train(In_vitro_digestibility~NLI, data= trainDat, 
            method=customRF, 
            #metric='Rsquared',
            importance = TRUE,
            tuneGrid= tunegrid, 
            trControl= control)

plot(ID)

id_pred <- predict(ID, testDat)
mean(id_pred)

p2<- predict(NLIclip, ID)

plot(p2, box = FALSE,
     ylab="Northing (m)", xlab= "Easting (m)", cex.axis=0.75)
title("IVDMD (%)", line = 1)

addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.4,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotepsg = NULL, widthhint = 0.3,
            unitcategory = "metric", htin = 0.1, padin = c(0.05, 0.08),
            style = "bar", bar.cols = c("black", "white"), lwd = 0.5,
            linecol = "black", tick.cex = 1, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomleft")


############################## ASH #############################

set.seed(1)
trainids<- createDataPartition(df5$Ash , list= F, p= 0.7)
trainDat<- df5[trainids,]
testDat<- df5[-trainids,]
nrow(trainDat)
nrow(testDat)


Ash <- train(Ash~Red, data= trainDat, 
            method=customRF, 
            #metric='Rsquared',
            importance = TRUE,
            tuneGrid= tunegrid, 
            trControl= control)

plot(Ash)


ash_pred <- predict(Ash, testDat)
head(ash_pred)
mean(ash_pred)


p3<- predict(Redclip, Ash)


plot(p3, box = FALSE,
     ylab="Northing (m)", xlab= "Easting (m)", cex.axis=0.75)
title("Ash (%)", line = 1)

addnortharrow(pos = "topright", padin = c(0.15, 0.15), scale = 0.4,
              lwd = 1, border = "black", cols = c("white", "black"),
              text.col = "black")

addscalebar(plotepsg = NULL, widthhint = 0.3,
            unitcategory = "metric", htin = 0.1, padin = c(0.05, 0.08),
            style = "bar", bar.cols = c("black", "white"), lwd = 0.5,
            linecol = "black", tick.cex = 1, labelpadin = 0.08, label.cex = 0.8,
            label.col = "black", pos = "bottomleft")





