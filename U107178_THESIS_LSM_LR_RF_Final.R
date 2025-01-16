
#Setting Working Directory
path="C:/Users/Binish Raj Khanal/Desktop/WD"
setwd(path)
getwd() # for checking

# Setting the personal library
.libPaths("C:/Users/Binish Raj Khanal/Desktop/WD/MyLibrary")
.libPaths() ## Press tab for options


# Define and install/load necessary packages

packages <- c(
  "RStoolbox",    # Image analysis & plotting spatial data
  "raster",       # Spatial data processing
  "plyr",         # data manipulation
  "dplyr",        # data manipulation
  "RColorBrewer", # Color
  "ggplot2",      #Plotting
  "sp",           # Spatial Data
  "caret",        # Machine Learning
  "randomForest",  # RF model
  "e1071",        # Naive Bayes
  "pROC",         # ROC Analysis
  "terra",        # Spatial data processing
  "sf",           # Spatial data
  "pscl", 
  "ROCR",        # ROC analysis
  "openxlsx",    # Exporting to Excel
  "expss",
  "car"
)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

# Import training and testing data 
list.files("./Data", pattern = "csv$", full.names = TRUE)

# 1.) Training Data

data_train <-  read.csv("./Data/TRAINING.csv", header = T)
data_train <-(na.omit(data_train))
data_train <-data.frame(data_train)  # to remove the unwelcomed attributes


# Aspect Settings for Training
data_train$ASPECT_grouped <-ifelse(data_train$ASPECT<0,-1,data_train$ASPECT)
ASPECTr<-cut(
  data_train$ASPECT_grouped,
  breaks=c(-2,seq(0,360,45)),
  right =FALSE,
  labels=c("i","a","b","c","d","e","f","g","h")
)

table(ASPECTr) 
class(ASPECTr) # double check if not a factor
ASPECTr <- factor(ASPECTr)


flags = data.frame(Reduce(cbind,lapply(levels(ASPECTr),function(x){(ASPECTr == x)*1})
))
names(flags) = levels(ASPECTr)
data_train = cbind(data_train, flags) # combine the ASPECTS with original data
summary(data_train)
data_train <-(na.omit(data_train))

# Remove the original Aspect data
data_train <- data_train[,-2] # Removing original Aspect data
data_train <- data_train[,-14] # Removing Aspect_grouped
#data_train <- data_train[,-14] # Removing Flat Areas 

# LULC setting for Training
LULCTr<-cut(
  data_train$LULC, 
  seq(1,9,1), 
  right=FALSE, 
  labels=c("l1","l2","l3","l4","l5","l6","l7","l8")
)
table(LULCTr) 
class(LULCTr) # double check if not a factor

flags = data.frame(Reduce(cbind,lapply(levels(LULCTr),function(x){(LULCTr == x)*1})
))
names(flags) = levels(LULCTr)
data_train = cbind(data_train, flags) # combine the LULC with original data

# Remove the original LULC
data_train <- data_train[,-7] # To remove original LULC
data_train <-(na.omit(data_train))

# Lithology setting for training
LithologyTr<-cut(
  data_train$LITHOLOGY, 
  seq(1,11,1), 
  right=FALSE, 
  labels=c("li1","li2","li3","li4","li5","li6","li7","li8","li9","li10")
)
table(LithologyTr) 
class(LithologyTr) # double check if not a factor

flags = data.frame(Reduce(cbind,lapply(levels(LithologyTr),function(x){(LithologyTr == x)*1})
))
names(flags) = levels(LithologyTr)
data_train = cbind(data_train, flags) # combine the LITHOLOGY with original data

# Remove the original Lithology
data_train <- data_train[,-6] # to remove LITHOLOGY


# Count the number of 1 and 0 elements with the values of dependent vector
as.data.frame(table(data_train$Training))


# Normalization
maxs <- apply(data_train, 2, max) 
mins <- apply(data_train, 2, min)
scaled_train <- as.data.frame(scale(data_train, center = mins, scale = maxs - mins))
scaled_t <-scaled_train
scaled_t$Training <- ifelse(scaled_t$Training == 1, "yes","no")


# 2.) Testing Data 

data_test <-  read.csv("./Data/TESTING.csv", header = T)
data_test <-na.omit(data_test)
data_test <-data.frame(data_test)
str(data_test)
as.data.frame(table(data_test$Testing))

# Aspect Settings for Testing
data_test$ASPECT_grouped <-ifelse(data_test$ASPECT<0,-1,data_test$ASPECT)
ASPECTe<-cut(
  data_test$ASPECT_grouped,
  breaks=c(-2,seq(0,360,45)),
  right =FALSE,
  labels=c("i","a","b","c","d","e","f","g","h")
)
table(ASPECTe) 
class(ASPECTe) # double check if not a factor
ASPECTe <- factor(ASPECTe)


flags = data.frame(Reduce(cbind,lapply(levels(ASPECTe),function(x){(ASPECTe == x)*1})
))
names(flags) = levels(ASPECTe)
data_test = cbind(data_test, flags) # combine the ASPECTS with original data
summary(data_test)
data_test <-(na.omit(data_test))

# Remove the original Aspect data
data_test <- data_test[,-2] # Removing original Aspect data
data_test <- data_test[,-14] # Removing ASPECT_grouped
#data_test <- data_test[,-14] # Removing Flat Areas

# LULC setting for Testing
LULCTe<-cut(
  data_test$LULC, 
  seq(1,9,1), 
  right=FALSE, 
  labels=c("l1","l2","l3","l4","l5","l6","l7","l8")
)
table(LULCTe) 
class(LULCTe) # double check if not a factor

flags = data.frame(Reduce(cbind,lapply(levels(LULCTe),function(x){(LULCTe == x)*1})
))
names(flags) = levels(LULCTe)
data_test = cbind(data_test, flags) # combine the LULC with original data

# Remove the original LULC
data_test <- data_test[,-7] # To remove LULC
summary(data_test)
data_test <-(na.omit(data_test))

# Lithology setting for testing
LithologyTe<-cut(
  data_test$LITHOLOGY, 
  seq(1,11,1), 
  right=FALSE, 
  labels=c("li1","li2","li3","li4","li5","li6","li7","li8","li9","li10")
)
table(LithologyTe) 
class(LithologyTe) # double check if not a factor


flags = data.frame(Reduce(cbind,lapply(levels(LithologyTe),function(x){(LithologyTe == x)*1})
))
names(flags) = levels(LithologyTe)
data_test = cbind(data_test, flags) # combine the LITHOLOGY with original data

# Remove the original Lithology
data_test <- data_test[,-6] # to remove LITHOLOGY
summary(data_test)
data_test <-(na.omit(data_test))


# Scale the data
maxs <- apply(data_test, 2, max) 
mins <- apply(data_test, 2, min)
scaled_test <- as.data.frame(scale(data_test, center = mins, scale = maxs - mins))
scaled_tst <-scaled_test
scaled_tst$Testing <- ifelse(scaled_tst$Testing == 1, "yes","no")

# RANDOM FOREST MODELING

rf_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)
# Train Random Forest with hyperparameter tuning
set.seed(1234)
rf_model <- train(
  Training ~ .,
  data = scaled_t,
  method = "rf",
  metric = "ROC",
  trControl = rf_control,
  tuneGrid = expand.grid(mtry=2),
  ntree = 300,
  nodesize = 5,
  maxnodes = 15
)

print(rf_model)
rf_importance <- varImp(rf_model, scale = T)
print(rf_importance)
plot(rf_importance, main = "Variable Importance in Random Forest")

# For Determining Mean Decrease Accuracy and Mean Decrease Gini
set.seed(1234)
rf_mod <- randomForest(
  Training ~.,
  data = scaled_t,
  ntree = 300,
  nodesize = 5,
  importance = TRUE
)
# Calculate variable importance
var_importance <- importance(rf_mod)
var_importance_df <- as.data.frame(var_importance)
var_importance_df$variable <- rownames(var_importance_df)

write.xlsx(var_importance_df,"Variable_Importance_RF.xlsx")

# LOGISTIC REGRESSION MODEL

# Converting scaled_t into a two level factor
scaled_t$Training <- as.factor(scaled_t$Training)

# Logistic Regression Model Training
lr_model <- glm(
  Training ~.,
  data = scaled_t,
  family = binomial(link = "logit")
)

summary(lr_model)
# Extracting coefficients and other details into a data frame
coefficients <- as.data.frame(coef(summary(lr_model)))
coefficients$Variable <- rownames(coefficients)
write.csv(coefficients,"Model_Summary.csv",row.names=FALSE)

# Extract Model Coefficients
coefficients_n <- summary(lr_model)$coefficients
coefficients_n <- na.omit(coefficients)

# Construct the equation
equation <- paste0(
  "logit(p) = ", round(coefficients_n[1, "Estimate"], 4), 
  paste(
    " + ", round(coefficients_n[-1, "Estimate"], 4), " * ", rownames(coefficients_n)[-1],
    collapse = ""
  )
)
cat(equation)


#SUCCESS RATE CURVE FOR RF AND LR

# Predict probabilities on the training dataset for Success Rate Curve

rf_train_predictions <- as.data.frame(predict(rf_model, scaled_t, type = "prob"))
rf_train_predictions$predict <- names(rf_train_predictions)[1:2][apply(rf_train_predictions[,1:2], 1, which.max)]
rf_train_predictions$observed <- as.factor(scaled_t$Training)
head(rf_train_predictions,2)

# RF AUC-ROC FOR SUCCESS RATE CURVE

png("Success_Rate_Curve_RF_LR.png",width=1600,height=1200,res=300)

par(pty="s")
roc_rf_train <- roc(ifelse(rf_train_predictions$observed=="yes","no-yes","yes"), 
                    as.numeric(rf_train_predictions$yes), 
                    plot = T,
                    main = "Success Rate Curve",
                    legacy.axes=T,
                    percent=T,
                    xlab="False Positive Percentage",
                    ylab="True Positive Percentage",
                    col="#2c7fb8",
                    lwd=2,
                    print.auc=T,
                    print.auc.x=45,
                    auc.polygon=F
                    )

# ADDING LR AUC ROC FOR SUCCESS RATE CURVE

# Predicting Probabilities on Training Datasets for SUCCESS RATE CURVE 

lr_train_predictions <-predict(
  lr_model, 
  scaled_t[,-1],
  type = "response"
)

# Converting probabilities to binary classes (threshold = 0.5)
lr_train_class <- ifelse(lr_train_predictions>0.5,"yes","no")

# Ensuring Training is a factor with two levels 
scaled_t$Training <- as.factor(scaled_t$Training)
levels(scaled_t$Training) <- c("no","yes")
#str(scaled_tst$Testing)

# Ensuring lr_train_predictions is numeric
lr_train_predictions <- as.numeric(lr_train_predictions)

roc_lr_train <- roc(scaled_t$Training,lr_train_predictions,
                    plot = T,
                    legacy.axes=T,
                    percent=T,
                    xlab="False Positive Percentage",
                    ylab="True Positive Percentage",
                    col="#4daf4a",
                    lwd=2,
                    print.auc=T,
                    add=T,
                    print.auc.x=45,
                    print.auc.y=40,
                    auc.polygon=F
)
legend("bottomright",legend=c("RF Model","LR Model"),
       col=c("#2c7fb8","#4daf4a"),
       lwd=2
)

# Add the grid
grid(nx=NULL,ny=NULL,col="gray",lty="dotted",lwd=0.5)

# Close the device
dev.off()

# PREDICTION RATE CURVE RF AND LR

# Predicting probabilities on the test dataset for Prediction Rate Curve
rf_test_predictions <- as.data.frame(predict(rf_model, scaled_tst, type = "prob"))
rf_test_predictions$predict <- names(rf_test_predictions)[1:2][apply(rf_test_predictions[,1:2], 1, which.max)]
rf_test_predictions$observed <- as.factor(scaled_tst$Testing)
head(rf_test_predictions,2)

# RF AUC-ROC FOR PREDICTION RATE CURVE
png("Prediction_Rate_Curve_RF_LR.png",width=1600,height=1200,res=300)
par(pty="s")
roc_rf_test <- roc(ifelse(rf_test_predictions$observed=="yes","no-yes","yes"), 
                   as.numeric(rf_test_predictions$yes),
                   plot = T,
                   main = "Prediction Rate Curve",
                   legacy.axes=T,
                   percent=T,
                   xlab="False Positive Percentage",
                   ylab="True Positive Percentage",
                   col="#2c7fb8",
                   lwd=2,
                   print.auc=T,
                   print.auc.x=45
                   )

# Predicting Probabilities on Test Datasets for PREDICTION RATE CURVE 

lr_test_predictions <-predict(
    lr_model, 
    scaled_tst[,-1],
    type = "response"
    )
  

# Converting probabilities to binary classes (threshold = 0.5)
lr_test_class <- ifelse(lr_test_predictions>0.5,"yes","no")

# LR AUC-ROC FOR PREDICTION RATE CURVE
# Ensuring Testing is a factor with two levels 
scaled_tst$Testing <- as.factor(scaled_tst$Testing)
levels(scaled_tst$Testing) <- c("no","yes")
#str(scaled_tst$Testing)

# Ensuring lr_test_predictions is numeric
lr_test_predictions <- as.numeric(lr_test_predictions)

roc_lr_test <- roc(scaled_tst$Testing,lr_test_predictions,
              plot = T,
              legacy.axes=T,
              percent=T,
              xlab="False Positive Percentage",
              ylab="True Positive Percentage",
              col="#4daf4a",
              lwd=2,
              print.auc=T,
              add=T,
              print.auc.x=45,
              print.auc.y=40,
              auc.polygon=F
              )
legend("bottomright",legend=c("RF Model","LR Model"),
       col=c("#2c7fb8","#4daf4a"),
       lwd=2
)
grid(nx=NULL,ny=NULL,col="gray",lty="dotted",lwd=0.5)

#Close the device
dev.off()

# CONFUSION MATRICES

# RF MODEL

#Confusion Matrix for Training datasets in RF Model
cmtr_rf_model <- predict(rf_model,scaled_t[,c(-1)], type="raw")
confusionMatrix(cmtr_rf_model,as.factor(scaled_t$Training))

# Confusion Matrix for Testing datasets in RF Model
cmtst_rf_model<-predict(rf_model, scaled_tst[,c(-1)], type = "raw")
confusionMatrix(cmtst_rf_model, as.factor(scaled_tst$Testing))  # using more deep tree, the accuracy linearly increases! 
#Accuracy : 0.8322

# LR MODEL

# Confusion Matrix for Training Datasets in LR Model
confusionMatrix(as.factor(lr_train_class), as.factor(scaled_t$Training))
# Confusion Matrix for Testing Datasets in LR Model
confusionMatrix(as.factor(lr_test_class),as.factor(scaled_tst$Testing))


# Produce LSM map using Training model results and Raster layers data

# Import Rasters

# load all the data
Aspect = raster("MyData/Aspect.tif")
Dist2Drain = raster("MyData/Dist2Drain.tif")
Dist2Fault = raster("MyData/Dist2Fault.tif")
Dist2Road = raster("MyData/Dist2Road.tif")
Elevation = raster("MyData/Elevation.tif")
Lithology = raster("MyData/Lithology.tif")
LULC = raster("MyData/LULC.tif")
NDVI = raster("MyData/NDVI.tif")
PlanCurvature = raster("MyData/PlanCurvature.tif")
Precipitation = raster("MyData/Precipitation.tif")
Slope = raster("MyData/Slope.tif")
SPI = raster("MyData/SPI.tif")
TWI = raster("MyData/TWI.tif")


# Resampling the datasets to common extent and resolutions

Re_Aspect <- resample(Aspect,Elevation, resample='bilinear')
Re_Dist2Drain <- resample(Dist2Drain,Elevation, resample='bilinear')
Re_Dist2Fault <- resample(Dist2Fault,Elevation, resample='bilinear')
Re_Dist2Road <- resample(Dist2Road,Elevation, resample='bilinear')
Re_Elevation <- resample(Elevation,Elevation, resample='bilinear')
Re_Lithology <- resample(Lithology,Elevation, resample='bilinear')
Re_LULC <- resample(LULC,Elevation, resample='bilinear')
Re_NDVI <- resample(NDVI,Elevation, resample='bilinear')
Re_PlanCurvature <- resample(PlanCurvature,Elevation, resample='bilinear')
Re_Precipitation <- resample(Precipitation,Elevation, resample='bilinear')
Re_Slope <- resample(Slope,Elevation, resample='bilinear')
Re_SPI <- resample(SPI,Elevation, resample='bilinear')
Re_TWI <- resample(TWI,Elevation, resample='bilinear')


# create resampled Rasters
dir.create("C:/Users/Binish Raj Khanal/Desktop/WD/ReData")  
writeRaster(Re_Aspect,"ReData/ASPECT.tif", overwrite=TRUE)
writeRaster(Re_Dist2Drain,"ReData/DIST2DRAIN.tif", overwrite=TRUE)
writeRaster(Re_Dist2Fault,"ReData/DIST2FAULT.tif", overwrite=TRUE)
writeRaster(Re_Dist2Road,"ReData/DIST2ROAD.tif", overwrite=TRUE)
writeRaster(Re_Elevation,"ReData/ELEVATION.tif", overwrite=TRUE)
writeRaster(Re_Lithology,"ReData/LITHOLOGY.tif", overwrite=TRUE)
writeRaster(Re_LULC,"ReData/LULC.tif", overwrite=TRUE)
writeRaster(Re_NDVI,"ReData/NDVI.tif", overwrite=TRUE)
writeRaster(Re_PlanCurvature,"ReData/PLANCURVATURE.tif", overwrite=TRUE)
writeRaster(Re_Precipitation,"ReData/PRECIPITATION.tif", overwrite=TRUE)
writeRaster(Re_Slope,"ReData/SLOPE.tif", overwrite=TRUE)
writeRaster(Re_SPI,"ReData/SPI.tif", overwrite=TRUE)
writeRaster(Re_TWI,"ReData/TWI.tif", overwrite=TRUE)

# Load resampled rasters
ASPECT = raster("ReData/ASPECT.tif")
DIST2DRAIN = raster("ReData/DIST2DRAIN.tif")
DIST2FAULT = raster("ReData/DIST2FAULT.tif")
DIST2ROAD = raster("ReData/DIST2ROAD.tif")
ELEVATION = raster("ReData/ELEVATION.tif")
LITHOLOGY = raster("ReData/LITHOLOGY.tif")
LULC = raster("ReData/LULC.tif")
NDVI = raster("ReData/NDVI.tif")
PLANCURVATURE = raster("ReData/PLANCURVATURE.tif")
PRECIPITATION = raster("ReData/PRECIPITATION.tif")
SLOPE = raster("ReData/SLOPE.tif")
SPI = raster("ReData/SPI.tif")
TWI = raster("ReData/TWI.tif")


# check attributes and projection and extent whether same for all or not
extent(ASPECT)
extent(DIST2DRAIN)
extent(DIST2FAULT)
extent(DIST2ROAD)
extent(ELEVATION)
extent(LITHOLOGY)
extent(LULC)
extent(NDVI)
extent(PLANCURVATURE)
extent(PRECIPITATION)
extent(SLOPE)
extent(SPI)
extent(TWI)


# stack multiple raster files
Stack_List= list.files(path = "./ReData",pattern = "tif$", full.names = TRUE)
Rasters=stack(Stack_List)
names(Rasters)


# Convert rasters to dataframe with Long-Lat
# Convert raster to dataframe with Long-Lat
Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)

Rasters.df_N <- Rasters.df[,c(-1,-2)] # remove x, y


# Dealing with Categorial data

# Aspect Settings

# Replace values less than 0 with a placeholder (e.g., -1 becomes a special case)
Rasters.df_N$ASPECT_grouped <- ifelse(Rasters.df_N$ASPECT < 0, -1, Rasters.df_N$ASPECT)
# Use cut() with seq(0, 360, 45) for the other bins
ASPECTras <- cut(
  Rasters.df_N$ASPECT_grouped, 
  breaks = c(-2, seq(0, 360, 45)),  # Extend breaks to include a range for -1
  right = FALSE, 
  labels = c("i","a", "b", "c", "d", "e", "f", "g", "h")
)

table(ASPECTras) 
class(ASPECTras) # double check if not a factor

flagsras = data.frame(Reduce(cbind,lapply(levels(ASPECTras),function(x){(ASPECTras == x)*1})
))
names(flagsras) = levels(ASPECTras)
Rasters.df_N = cbind(Rasters.df_N, flagsras) # combine the ASPECTS with original data
summary(Rasters.df_N)
Rasters.df_N <-(na.omit(Rasters.df_N))

# Remove the original Aspect data
Rasters.df_N <- Rasters.df_N[,-1] # Removing Original Aspect data
Rasters.df_N <- Rasters.df_N[,-13] # Removing ASPECT_grouped
#Rasters.df_N <- Rasters.df_N[,-13] # Removing flat areas

# LULC setting
LULCras<-cut(
  Rasters.df_N$LULC, 
  seq(1,9,1), 
  right=FALSE, 
  labels=c("l1","l2","l3","l4","l5","l6","l7","l8")
)
table(LULCras) 
class(LULCras) # double check if not a factor

flagsras = data.frame(Reduce(cbind,lapply(levels(LULCras),function(x){(LULCras == x)*1})
))
names(flagsras) = levels(LULCras)
Rasters.df_N = cbind(Rasters.df_N, flagsras) # combine the LULC with original data


# Remove the original LULC
Rasters.df_N <- Rasters.df_N[,-6] # To remove original LULC
summary(Rasters.df_N)
Rasters.df_N <-(na.omit(Rasters.df_N))

# Lithology setting
Lithologyras<-cut(
  Rasters.df_N$LITHOLOGY, 
  seq(1,11,1), 
  right=FALSE, 
  labels=c("li1","li2","li3","li4","li5","li6","li7","li8","li9","li10")
)
table(Lithologyras) 
class(Lithologyras) # double check if not a factor

flagsras = data.frame(Reduce(cbind,lapply(levels(Lithologyras),function(x){(Lithologyras == x)*1})
))
names(flagsras) = levels(Lithologyras)
Rasters.df_N = cbind(Rasters.df_N, flagsras) # combine the LITHOLOGY with original data

# Remove the original Lithology
Rasters.df_N <- Rasters.df_N[,-5] # to remove LITHOLOGY

summary(Rasters.df_N)
Rasters.df_N <-(na.omit(Rasters.df_N))
str(Rasters.df_N)

# Scale the numeric variables

maxss <- apply(Rasters.df_N, 2, max) 
minss <- apply(Rasters.df_N, 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N, center = minss, scale = maxss - minss)) # we removed the Aspect levels because it might be changed to NA!
colnames(Rasters.df_N_scaled)


# Generating Susceptibility Map from RF Model
p<-as.data.frame(predict(rf_model, Rasters.df_N_scaled, type = "prob"))
summary(p)
Rasters.df$RF_YES<-p$yes
Rasters.df$RF_NO<-p$no

x<-SpatialPointsDataFrame(as.data.frame(Rasters.df)[, c("x", "y")], data = Rasters.df)
r_ave_yes <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "RF_YES")])
proj4string(r_ave_yes)=CRS(projection(ELEVATION))

r_ave_no <- rasterFromXYZ(as.data.frame(x)[, c("x", "y", "RF_NO")])
proj4string(r_ave_no)=CRS(projection(ELEVATION))


# Plot Maps
spplot(r_ave_yes, main="Landslides Susceptibility Map using RF")
writeRaster(r_ave_yes,filename="Prediction_RF_Tunned_Landslides.tif", format="GTiff", overwrite=TRUE) 

spplot(r_ave_no, main="Non-slides Susceptibility Map using RF")
writeRaster(r_ave_no,filename="Prediction_RF_Tunned_Non_Slide.tif", format="GTiff", overwrite=TRUE) 


# Generating Susceptibility Map from LR Model

lr_map <- as.data.frame(predict(lr_model,Rasters.df_N_scaled, type="response"))
colnames(lr_map) <- "predict"
Rasters.df$LR_YES <-lr_map$predict
lr_raster <- rasterFromXYZ(as.data.frame(Rasters.df)[,c("x","y","LR_YES")])
proj4string(lr_raster)<- CRS(projection(ELEVATION))

spplot(lr_raster, main = "Landslide Susceptibility Map using Logistic Regression")
writeRaster(lr_raster, filename = "Prediction_LR_Landslide.tif", format = "GTiff",overwrite=TRUE)


# Multicollinearity Check
library(car)
train <-  read.csv("./Data/TRAINING.csv", header = T)
vif_model <- glm(
  Training ~.,
  data = train,
  family = binomial(link = "logit")
)
vif_values <- vif(vif_model)
print(vif_values)
summary(vif_model)

# ASPECT    DIST2DRAIN    DIST2FAULT     DIST2ROAD     ELEVATION     LITHOLOGY          LULC 
# 1.112526      1.772301      1.619397      1.312533      2.805439      1.386238      1.063864 
# NDVI PLANCURVATURE PRECIPITATION         SLOPE           SPI           TWI 
# 1.162232      1.713889      2.357133      2.841957      2.746686      2.117785 




