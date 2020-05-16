# Loading Packages
library("stats")
library("arm")
library("jtools")
library("broom")
library("ggstance")
library("magrittr")
library("reshape2")
library("ggplot2")
library("ggcorrplot")


### input
mdata.input <- read.csv2(file = "input/Data.csv", sep = ",", check.names = F, dec = '.')
mdata <- mdata.input


### Descriptive statistics
ls.str(mdata)
hist(mdata$claim_count, breaks = 50) 
mean(mdata$claim_count) 
var(mdata$claim_count) 
table(mdata$claim_count) 

#univariate analysis
library(psych)
describe(mdata)

### Data preparation/cleaning

# Remove 'policy_desc' column
mdata <- mdata[,!(names(mdata) %in% c('policy_desc'))]

# Renaming variables
names(mdata)[names(mdata) == "﻿policy_desc"] <- "policy_desc"
names(mdata)[names(mdata) == "cat_areacode"] <- "areacode"
names(mdata)[names(mdata) == "num_vehicleAge"] <- "vehicle_age"
names(mdata)[names(mdata) == "num_noClaimDiscountPercent"] <- "discount"
names(mdata)[names(mdata) == "cat_carBrand"] <- "car_brand"
names(mdata)[names(mdata) == "num_populationDensitykmsq"] <- "population_density"
names(mdata)[names(mdata) == "cat_Region"] <- "region"
names(mdata)[names(mdata) == "ord_vehicleHP"] <- "vehicle_hp"
names(mdata)[names(mdata) == "num_exposure"] <- "exposure"
names(mdata)[names(mdata) == "cat_fuelType"] <- "fuel_type"
names(mdata)[names(mdata) == "num_driverAge"] <- "driver_age"

# Change type of variable 'exposure'
mdata$exposure <- suppressWarnings(as.numeric(as.character(mdata$exposure)))

# Remove rows with missing values
any(!complete.cases(mdata))
mdata <- mdata[complete.cases(mdata), ]

# Remove rows where variable 'fuel_type' is NULL
mdata <- mdata[-which(mdata[,'fuel_type'] == 'NULL'), ]


# CORRELATION ------------------------------------------------------------------

cor.names <- c("spearman")

cor.df <- mdata[,!(names(mdata) %in% c('policy_desc'))]
cor.df <- lapply(cor.df, as.numeric) %>% data.frame()


# calculating correlation

for (i in cor.names) {
  all.cor <- cor(cor.df, use = 'complete.obs', method = i)
  assign(paste0("cor.", i), melt(cor(cor.df, use = 'complete.obs'), variable.factor=FALSE))
}


# Correlogram

corr <- round(all.cor, 2)

pdf(file="plots/Correlogram.pdf",width = 16, height = 12)
corr.plot1 <- ggcorrplot(corr, hc.order = TRUE, 
                         type = "lower", 
                         lab = TRUE, 
                         lab_size = 3, 
                         method="square", 
                         colors = c("tomato2", "white", "springgreen3"), 
                         title="Correlogram", 
                         ggtheme=theme_bw)
print(corr.plot1)
dev.off()



################## model  ##########################

model.stat <- matrix(0, ncol = 4, nrow = 1) %>% data.frame()
names(model.stat) <- c('RSE', 'Adjusted R-squared', 'F-Statistic', 'any-aliased')

mdata$areacode <- factor(mdata$areacode, levels=c("A","B","C","D","E","F"), labels=c(1,2,3,4,5,6))
mdata$areacode=as.integer(as.character(mdata$areacode ))

mdata$fuel_type <- factor(mdata$fuel_type, levels=c("NULL","Diesel", "Regular", "Electric"), labels=c(0,1,2,3))
mdata$fuel_type=as.integer(as.character(mdata$fuel_type))

########### Sampling ############
# Sampling (0.7 , 0.3)
set.seed(1234)
ind<- sample(2, nrow(mdata), replace=TRUE, prob=c(0.8,0.2))
trainData=mdata[ind==1,]
testData=mdata[ind==2,]

### Linear model

# MODEL 1
measurevar <- "claim_count"
lm.regressors1 <- setdiff(names(trainData), c('claim_count'))
f1 <- as.formula(paste(measurevar, paste(lm.regressors1, collapse=" + "), sep=" ~ "))
#as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
fit1 <- lm(f1, data = trainData)
summary(fit1)
model.stat.fit1 <-  model.stat
model.stat.fit1$RSE <- summary(fit1)$sigma
model.stat.fit1$`Adjusted R-squared` <- summary(fit1)$adj.r.squared
model.stat.fit1$`F-Statistic` <- summary(fit1)$fstatistic[1]
model.stat.fit1$`any-aliased` <- any(summary(fit1)$aliased)

# MODEL 2
lm.regressors2 <- setdiff(names(trainData), c('claim_count', 'region', 'car_brand', 'vehicle_hp'))
f2 <- as.formula(paste("claim_count ~ ", paste(lm.regressors2, collapse=" + ")))
fit2 <- lm(f2, data = trainData)
summary(fit2) # higher F-Statistic shows stronger relashionship between exogenic and endogenic variables
model.stat.fit2 <-  model.stat
model.stat.fit2$RSE <- summary(fit2)$sigma
model.stat.fit2$`Adjusted R-squared` <- summary(fit2)$adj.r.squared
model.stat.fit2$`F-Statistic` <- summary(fit2)$fstatistic[1]
model.stat.fit2$`any-aliased` <- any(summary(fit2)$aliased)


#See Predicted Value
pred = predict(fit1,testData)

#See Actual vs. Predicted Value
finaldata = cbind(testData,pred)
print(head(subset(finaldata, select = c(claim_count,pred))))

#Calculating RMSE
rmse <- sqrt(mean((testData$claim_count - pred)^2))
print(rmse)

#check accuracy
library(forecast)
accuracy(fit1)

### Poisson regression

# MODEL 3
pr.regressors3 <- setdiff(names(trainData), c('claim_count'))
f3 <- as.formula(paste("claim_count ~ ", paste(pr.regressors3, collapse=" + ")))
fit3 <- glm(f3, trainData, family = poisson(link = "log"))
summary(fit3)

# MODEL 4
pr.regressors4 <- setdiff(names(trainData), c('claim_count', 'region', 'car_brand', 'vehicle_hp', 'fuel_type'))
f4 <- as.formula(paste("claim_count ~ ", paste(pr.regressors4, collapse=" + ")))
fit4 <- glm(f4, trainData, family = quasipoisson(link = "log"))
summary(fit4)



#See Predicted Value
pred = predict(fit3,testData)

#See Actual vs. Predicted Value
finaldata = cbind(testData,pred)
print(head(subset(finaldata, select = c(claim_count,pred))))

#Calculating RMSE
rmse <- sqrt(mean((testData$claim_count - pred)^2))
print(rmse)

#check accuracy
library(forecast)
accuracy(fit4)

# Comparing The Models
coef1 = coef(fit3)
coef2 = coef(fit4)
se.coef1 = se.coef(fit3)
se.coef2 = se.coef(fit4)
models.both <- cbind(coef1, se.coef1, coef2, se.coef2, exponent=exp(coef1))
models.both

# Comparing The Models
coef3 = coef(fit1)
coef4 = coef(fit2)
se.coef3 = se.coef(fit1)
se.coef4 = se.coef(fit2)
models.both <- cbind(coef1, se.coef1, coef2, se.coef2, coef3, se.coef4, coef3, se.coef4, exponent=exp(coef1))
models.both

# Predicting From The Model
#modeling.data <- mdata[,setdiff(names(mdata), c('claim_count'))]
#predict(fit3, newdata = modeling.data, type = "response")
#table(round(predict(fit3, newdata = modeling.data, type = "response"), 0))



# Visualizing
#plot_summs(fit4, scale = TRUE, exp = TRUE)
#plot_summs(fit3, fit4, scale = TRUE, exp = TRUE)




