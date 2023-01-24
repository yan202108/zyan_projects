rm(list = ls())
setwd("D:/BigDataAnalytics/Project")
# libraries 
#install.packages("magrittr") 
#install.packages("tidyverse")
#install.packages("randomForest")
#install.packages("jtools") 
#install.packages("klaR")
# install.packages("kknn")
library(kknn)
library(caret)
library(randomForest)
library(magrittr) # needs to be run to use %>%
library(tidyverse)
library(adabag)
library(rpart) 
library(caret)
library(klaR)
library(jtools)

pokemon.df <- read.csv("Pokemon.csv")
combat.df <- read.csv("combats.csv")

#FIND MISSING VALUES AND CALCULATE  WIN PERCENTAGE******************************

table(is.na(pokemon.df))
table(is.na(combat.df))


#MERGING DATAFRAMS POKEMON AND NUMBEROFWINS WITH RESULT**************************

pokemon.df = pokemon.df %>% 
  rename(
    PokemonId = X.
  )

# How Fast is each Pokemon Type?
ggplot(pokemon.df, aes(x=Type.1  , y= Speed)) + geom_boxplot(position = 'dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(pokemon.df, aes(x=Type.2  , y= Speed)) + geom_boxplot(position = 'dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Are Legendary Pokemons Faster than Non-Legendary?
ggplot(pokemon.df, aes(Legendary, Speed,  fill = Legendary)) + geom_boxplot(position = 'dodge')


#Dummify categorical variables**************************************************

unique(pokemon.df$Type.1) 
unique(pokemon.df$Type.2)

#install.packages('fastDummies')
library('fastDummies')

pokeTransform.df <- pokemon.df

# merge two data frames by ID
pokeTransform.df[pokeTransform.df==""] <- NA

#rename Type.1 with Type
pokeTransform.df = pokeTransform.df %>% 
  rename(
    Type = Type.1
  )

#create categorical variable for Type(Type1) total cols after running this will be 33
pokeTransform.df <- dummy_cols(pokeTransform.df, select_columns = 'Type')

#rename back Type.1 
pokeTransform.df = pokeTransform.df %>% 
  rename(
    Type.1 = Type
  )

#replace NA with 0 with type2
pokeTransform.df$Type.2[is.na(pokeTransform.df$Type.2)] = 0


#skipping NA values from assigning type Type2 covers all the categorical values tot cols 33 
pokeTransform.df$Type_Poison <- ifelse(pokeTransform.df$Type.2 == "Poison",1, pokeTransform.df$Type_Poison)
pokeTransform.df$Type_Bug <- ifelse(pokeTransform.df$Type.2 == "Bug",1,pokeTransform.df$Type_Bug)
pokeTransform.df$Type_Dark <- ifelse(pokeTransform.df$Type.2 == "Dark",1,pokeTransform.df$Type_Dark)
pokeTransform.df$Type_Flying <- ifelse(pokeTransform.df$Type.2 == "Flying",1,pokeTransform.df$Type_Flying)
pokeTransform.df$Type_Dragon <- ifelse(pokeTransform.df$Type.2 == "Dragon",1,pokeTransform.df$Type_Dragon)
pokeTransform.df$Type_Ground <- ifelse(pokeTransform.df$Type.2 == "Ground",1,pokeTransform.df$Type_Ground)
pokeTransform.df$Type_Fairy <- ifelse(pokeTransform.df$Type.2 == "Fairy",1,pokeTransform.df$Type_Fairy)
pokeTransform.df$Type_Grass <- ifelse(pokeTransform.df$Type.2 == "Grass",1,pokeTransform.df$Type_Grass)
pokeTransform.df$Type_Fighting <- ifelse(pokeTransform.df$Type.2 == "Fighting",1,pokeTransform.df$Type_Fighting)
pokeTransform.df$Type_Psychic <- ifelse(pokeTransform.df$Type.2 == "Psychic",1,pokeTransform.df$Type_Psychic)
pokeTransform.df$Type_Steel <- ifelse(pokeTransform.df$Type.2 == "Steel",1,pokeTransform.df$Type_Steel)
pokeTransform.df$Type_Ice <- ifelse(pokeTransform.df$Type.2 == "Ice",1,pokeTransform.df$Type_Ice)
pokeTransform.df$Type_Rock <- ifelse(pokeTransform.df$Type.2 == "Rock",1,pokeTransform.df$Type_Rock)
pokeTransform.df$Type_Water <- ifelse(pokeTransform.df$Type.2 == "Water",1,pokeTransform.df$Type_Water)
pokeTransform.df$Type_Electric <- ifelse(pokeTransform.df$Type.2 == "Electric",1,pokeTransform.df$Type_Electric)
pokeTransform.df$Type_Fire <- ifelse(pokeTransform.df$Type.2 == "Fire",1,pokeTransform.df$Type_Fire)
pokeTransform.df$Type_Ghost <- ifelse(pokeTransform.df$Type.2 == "Ghost",1,pokeTransform.df$Type_Ghost)
pokeTransform.df$Type_Normal <- ifelse(pokeTransform.df$Type.2 == "Normal",1,pokeTransform.df$Type_Normal)

#Categorize__Legendary

pokeTransform.df$Legendary <- ifelse(pokeTransform.df$Legendary == "True",1,0)


#Final Data Preparation for Modelling for Machine Learning**********************
pokefight <- combat.df

# Create 'difference' features for stats
pokefight$diff_HP <- pokeTransform.df$HP[combat.df$First_pokemon] - pokeTransform.df$HP[combat.df$Second_pokemon]
pokefight$diff_Attack <- pokeTransform.df$Attack[combat.df$First_pokemon] - pokeTransform.df$Attack[combat.df$Second_pokemon]
pokefight$diff_Defense <- pokeTransform.df$Defense[combat.df$First_pokemon] - pokeTransform.df$Defense[combat.df$Second_pokemon]
pokefight$diff_Sp..Atk <- pokeTransform.df$Sp..Atk[combat.df$First_pokemon] - pokeTransform.df$Sp..Atk[combat.df$Second_pokemon]
pokefight$diff_Sp..Def <-  pokeTransform.df$Sp..Def[combat.df$First_pokemon] -  pokeTransform.df$Sp..Def[combat.df$Second_pokemon]
pokefight$diff_Speed <-  pokeTransform.df$Speed[combat.df$First_pokemon] -  pokeTransform.df$Speed[combat.df$Second_pokemon]

# Create 'difference' features for types
pokefight$Type_Poison <- pokeTransform.df$Type_Poison[combat.df$First_pokemon] - pokeTransform.df$Type_Poison[combat.df$Second_pokemon]
pokefight$Type_Bug <- pokeTransform.df$Type_Bug[combat.df$First_pokemon] - pokeTransform.df$Type_Bug[combat.df$Second_pokemon]
pokefight$Type_Dark <- pokeTransform.df$Type_Dark[combat.df$First_pokemon] - pokeTransform.df$Type_Dark[combat.df$Second_pokemon]
pokefight$Type_Flying <- pokeTransform.df$Type_Flying[combat.df$First_pokemon] - pokeTransform.df$Type_Flying[combat.df$Second_pokemon]
pokefight$Type_Dragon <- pokeTransform.df$Type_Dragon[combat.df$First_pokemon] - pokeTransform.df$Type_Dragon[combat.df$Second_pokemon]
pokefight$Type_Ground <-pokeTransform.df$Type_Ground[combat.df$First_pokemon] - pokeTransform.df$Type_Ground[combat.df$Second_pokemon]
pokefight$Type_Fairy <- pokeTransform.df$Type_Fairy[combat.df$First_pokemon] - pokeTransform.df$Type_Fairy[combat.df$Second_pokemon]
pokefight$Type_Grass <- pokeTransform.df$Type_Grass[combat.df$First_pokemon] - pokeTransform.df$Type_Grass[combat.df$Second_pokemon]
pokefight$Type_Fighting <- pokeTransform.df$Type_Fighting[combat.df$First_pokemon] - pokeTransform.df$Type_Fighting[combat.df$Second_pokemon]
pokefight$Type_Psychic <- pokeTransform.df$Type_Psychic[combat.df$First_pokemon] - pokeTransform.df$Type_Psychic[combat.df$Second_pokemon]
pokefight$Type_Steel <- pokeTransform.df$Type_Steel[combat.df$First_pokemon] - pokeTransform.df$Type_Steel[combat.df$Second_pokemon]
pokefight$Type_Ice <- pokeTransform.df$Type_Ice[combat.df$First_pokemon] - pokeTransform.df$Type_Ice[combat.df$Second_pokemon]
pokefight$Type_Rock <- pokeTransform.df$Type_Rock[combat.df$First_pokemon] - pokeTransform.df$Type_Rock[combat.df$Second_pokemon]
pokefight$Type_Water <- pokeTransform.df$Type_Water[combat.df$First_pokemon] - pokeTransform.df$Type_Water[combat.df$Second_pokemon]
pokefight$Type_Electric <- pokeTransform.df$Type_Electric[combat.df$First_pokemon] - pokeTransform.df$Type_Electric[combat.df$Second_pokemon]
pokefight$Type_Fire <- pokeTransform.df$Type_Fire[combat.df$First_pokemon] - pokeTransform.df$Type_Fire[combat.df$Second_pokemon]
pokefight$Type_Ghost <- pokeTransform.df$Type_Ghost[combat.df$First_pokemon] - pokeTransform.df$Type_Ghost[combat.df$Second_pokemon]
pokefight$Type_Normal <- pokeTransform.df$Type_Normal[combat.df$First_pokemon] - pokeTransform.df$Type_Normal[combat.df$Second_pokemon]

pokefight$Legendary <- pokeTransform.df$Legendary[combat.df$First_pokemon] - pokeTransform.df$Legendary[combat.df$Second_pokemon]

pokefight$Result[pokefight$Winner == pokefight$First_pokemon] <- 'Win'
pokefight$Result[pokefight$Winner == pokefight$Second_pokemon] <- 'Loss'

# Create binary variable of Result ('Win/Lose' to numeric variable '1/0')
pokefight$binaryResult[pokefight$Result == "Win"] <- 1
pokefight$binaryResult[pokefight$Result == "Loss"] <- 0

#Converting required type and Legendary into Factors 0/1
pokefight$Legendary <-as.factor(pokefight$Legendary)

pokefight$Type_Poison <- as.factor(pokefight$Type_Poison)
pokefight$Type_Bug <- as.factor(pokefight$Type_Bug)
pokefight$Type_Dark <- as.factor(pokefight$Type_Dark)
pokefight$Type_Flying <- as.factor(pokefight$Type_Flying)
pokefight$Type_Dragon <- as.factor(pokefight$Type_Dragon)
pokefight$Type_Ground <- as.factor(pokefight$Type_Ground)
pokefight$Type_Fairy <- as.factor(pokefight$Type_Fairy)
pokefight$Type_Grass <- as.factor(pokefight$Type_Grass)
pokefight$Type_Fighting <- as.factor(pokefight$Type_Fighting)
pokefight$Type_Psychic <- as.factor(pokefight$Type_Psychic)
pokefight$Type_Steel <- as.factor(pokefight$Type_Steel)
pokefight$Type_Ice <- as.factor(pokefight$Type_Ice)
pokefight$Type_Rock <- as.factor(pokefight$Type_Rock)
pokefight$Type_Water <- as.factor(pokefight$Type_Water)
pokefight$Type_Electric <- as.factor(pokefight$Type_Electric)
pokefight$Type_Fire <- as.factor(pokefight$Type_Fire)
pokefight$Type_Ghost <- as.factor(pokefight$Type_Ghost)
pokefight$Type_Normal <- as.factor(pokefight$Type_Normal)

#'data.frame':	50000 obs. of  29 variables:
str(pokefight)

# Does Speed help distinguish Win/Loss?
ggplot(pokefight, aes((diff_Speed), fill = Result)) + geom_density(alpha = 0.5)


#Variable Selection and Test Valid Data Prep**************************************


pokefight <- as.data.frame(pokefight)

selectedcols <- colnames(pokefight[c("diff_HP","diff_Attack","diff_Defense","diff_Sp..Atk","diff_Sp..Def","diff_Speed",
                                     "Legendary","Type_Poison","Type_Bug","Type_Dark","Type_Flying","Type_Dragon","Type_Ground",
                                     "Type_Fairy","Type_Grass","Type_Fighting","Type_Psychic","Type_Steel","Type_Ice",
                                     "Type_Rock","Type_Water","Type_Electric","Type_Fire","Type_Ghost","Type_Normal","binaryResult")])

# select variables for regression
selected.var <- c(selectedcols)

# partition data
set.seed(123)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(pokefight)[1]), dim(pokefight)[1]*0.7)  
train.df <- pokefight[train.index, selected.var]
valid.df <- pokefight[-train.index, selected.var]

#logistic regression *************************************************************

# use glm() (general linear model) with family = "binomial" to fit a logistic regression.

logit.reg <- glm(binaryResult ~ ., data = train.df, family = "binomial") 
options(scipen=999) # remove scientific notation
summ(logit.reg, digits=5)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# Use Confusion Matrix to evaluate performance
# install.packages("caret") # Package installation is required for the first time, it takes some time!
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5, 1, 0)), 
                as.factor(valid.df$binaryResult))

#Logistic with Backward Variable Selection********************************************
# Try a Backward Variable Selection
pc.lm.step <- step(logit.reg, direction = "backward")
summary(pc.lm.step)  # Which variables did it drop?

pc.lg.step.prep <- predict(pc.lm.step, valid.df, type = "response")

#Run pca 1st, if works use PCA result to try logistic and RF

confusionMatrix(as.factor(ifelse(pc.lg.step.prep > 0.5, 1, 0)), 
                as.factor(valid.df$binaryResult))


#Random forest*************************************************************************


pc_rf <- randomForest(binaryResult ~ ., data = train.df, ntree = 100, 
                      mtry = 4, nodesize = 5, importance = TRUE)  
summary(pc_rf)
#head(rf$votes,20)

## Plot forest by prediction errors
plot(pc_rf)
legend("top", colnames(pc_rf$err.rate),cex=0.8,fill=1:3)


## variable importance plot
varImpPlot(pc_rf, type = 1)

## accuracy
pc_rf.pred <- predict(pc_rf, valid.df)
library(caret)
confusionMatrix(as.factor(ifelse(pc_rf.pred > 0.5, 1, 0)),
                as.factor(valid.df$binaryResult))
#KNNN *****************************************************************************

set.seed(123)
pc_kknn.train <- train.kknn(binaryResult ~ ., data = train.df, kmax = 25,
                            distance = 2,
                            kernel = c("rectangular", "triangular", "epanechnikov"))
plot(pc_kknn.train)

pc_kknn.train

pc_kknn.pred <- predict(pc_kknn.train, newdata = valid.df)
confusionMatrix(as.factor(ifelse(pc_kknn.pred  > 0.5, 1, 0)),
                as.factor(valid.df$binaryResult))

#run naive bayes****************************************************************************************
library(e1071)

pc_nb <- naiveBayes(binaryResult ~. , data = train.df)
pc_nb

## predict probabilities: Training
pc_nb_pred.prob <- predict(pc_nb, newdata = train.df, type = "raw")
## predict class membership
nb.pred.class <- predict(pc_nb, newdata = train.df)
confusionMatrix(factor(nb.pred.class), factor(train.df$binaryResult))

## predict probabilities: Validation
pred.prob <- predict(pc_nb, newdata = valid.df, type = "raw")
## predict class membership
nb.pred.class <- predict(pc_nb, newdata = valid.df)
confusionMatrix(factor(nb.pred.class), factor(valid.df$binaryResult))


#Neural Net Plot****************************************************************************************

library(neuralnet)
library(nnet)
library(caret)

nn2<-nnet(binaryResult~ diff_HP+diff_Attack+diff_Defense+diff_Sp..Atk+diff_Sp..Def+diff_Speed
          +Legendary+Type_Poison+Type_Bug+Type_Dark+Type_Flying+Type_Dragon+Type_Ground
          +Type_Fairy+Type_Grass+Type_Fighting+Type_Psychic+Type_Steel+Type_Ice
          +Type_Rock+Type_Water+Type_Electric+Type_Fire+Type_Ghost+Type_Normal,size=10,data=train.df)

library(NeuralNetTools)
plotnet(nn2)
neuralweights(nn2)
olden(nn2)


#Ensamble Selection and Test Valid Data Prep********************************************
pokefight <- as.data.frame(pokefight)

# transform binaryResult into categorical variable
pokefight$binaryResult <- factor(pokefight$binaryResult,levels=c(0,1),labels = c("X0", "X1"))
# Make class levels valid R variable names 
levels(pokefight$binaryResult) <- make.names(levels(factor(pokefight$binaryResult)))


# partition data
set.seed(123)  # set seed for reproducing the partition
train.index <- sample(c(1:dim(pokefight)[1]), dim(pokefight)[1]*0.7)
train.df <- pokefight[train.index, ]
valid.df <- pokefight[-train.index,]


#****************************************************************************************
#
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

predictors<-c("diff_HP","diff_Attack","diff_Defense","diff_Sp..Atk","diff_Sp..Def","diff_Speed",
              "Legendary","Type_Poison","Type_Bug","Type_Dark","Type_Flying","Type_Dragon","Type_Ground",
              "Type_Fairy","Type_Grass","Type_Fighting","Type_Psychic","Type_Steel","Type_Ice",
              "Type_Rock","Type_Water","Type_Electric","Type_Fire","Type_Ghost","Type_Normal")

outcomeName<-c("binaryResult")


#Training a Logistic regression model*********************************************


model_lr<-train(train.df[,predictors],train.df$binaryResult,method='glm',
                trControl=fitControl,tuneLength=3)
#Predicting using logistic model
valid.df$pred_lr<-predict(object = model_lr,valid.df[,predictors])
valid.df$pred_lr.prob<-predict(object = model_lr,valid.df[,predictors],type="prob")
#Checking the accuracy of the logistic model
confusionMatrix(valid.df$binaryResult,valid.df$pred_lr)


#Training a Naive Bayes model****************************************************

model_nb<-train(train.df[,predictors],train.df$binaryResult,method='nb',
                trControl=fitControl,tuneLength=3)
#Predicting using Naive Bayes model
valid.df$pred_nb<-predict(object = model_nb,valid.df[,predictors])
valid.df$pred_nb.prob<-predict(object = model_nb,valid.df[,predictors],type="prob")
#Checking the accuracy of the Naive Bayes model
confusionMatrix(valid.df$binaryResult,valid.df$pred_nb)


#Training a random forest model****************************************************

model_rf<-train(train.df[,predictors],train.df$binaryResult,method='rf',
                trControl=fitControl,tuneLength=3)
#Predicting using random forest model
valid.df$pred_rf<-predict(object = model_rf,valid.df[,predictors])
valid.df$pred_rf.prob<-predict(object = model_rf,valid.df[,predictors],type="prob")
#Checking the accuracy of the random forest model
confusionMatrix(valid.df$binaryResult,valid.df$pred_rf)

#Training a Neural Net model****************************************************
model_nn<-train(train.df[,predictors],train.df$binaryResult,method='nnet',
                trControl=fitControl,tuneLength=3)
#Predicting using neural net model
valid.df$pred_nn<-predict(object = model_nn,valid.df[,predictors])
valid.df$pred_nn.prob<-predict(object = model_nn,valid.df[,predictors],type="prob")
#Checking the accuracy of the random forest model
confusionMatrix(valid.df$binaryResult,valid.df$pred_nn)

# Life chart***********************************************************************
# install.packages("gains")
library(gains)
valid.df$binaryResult.n = ifelse(valid.df$binaryResult == "X1", 1, 0)
valid.df$pred_rf.n = ifelse(valid.df$pred_rf == "X1", 1, 0)
valid.df$pred_lr.n = ifelse(valid.df$pred_lr == "X1", 1, 0)
valid.df$pred_nb.n = ifelse(valid.df$pred_nb == "X1", 1, 0)
valid.df$pred_nn.n = ifelse(valid.df$pred_nb == "X1", 1, 0)

# Life chart: the main idea
lift.df <- valid.df$pred_rf.prob$X1
lift.df <- data.frame(lift.df, valid.df[,c("pred_rf.n","binaryResult.n")])
colnames(lift.df) <- c("pred_rf.prob.1","pred_rf.n","binaryResult.n")
head(lift.df)
attach(lift.df)
lift.df <- lift.df[order(-pred_rf.prob.1),]
detach(lift.df)
lift.df$binaryResult.n.cum<-cumsum(lift.df$binaryResult.n)
lift.df[1:30,]
lift.df[1980:2000,]


# Gains: Random Forest
gain.rf <- gains(valid.df$binaryResult.n, valid.df$pred_rf.prob$X1, groups=10)
# Gains: Logistic Regression
gain.lr <- gains(valid.df$binaryResult.n, valid.df$pred_lr.prob$X1, groups=10)
# Gains: Naive Bayes
gain.nb <- gains(valid.df$binaryResult.n, valid.df$pred_nb.prob$X1, groups=10)
# Gains: Neural Net
gain.nn <- gains(valid.df$binaryResult.n, valid.df$pred_nn.prob$X1, groups=10)


# Plot lift charts
plot(c(0, gain.rf$cume.pct.of.total*sum(valid.df$binaryResult.n)) ~ c(0, gain.rf$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="red")
par(new=TRUE)
plot(c(0, gain.lr$cume.pct.of.total*sum(valid.df$binaryResult.n)) ~ c(0, gain.lr$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="green")
par(new=TRUE)
plot(c(0, gain.nb$cume.pct.of.total*sum(valid.df$binaryResult.n)) ~ c(0, gain.nb$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="blue")
par(new=TRUE)
plot(c(0, gain.nn$cume.pct.of.total*sum(valid.df$binaryResult.n)) ~ c(0, gain.nn$cume.obs), 
     xlab = "# cases", ylab = "Cumulative", type="l", col="blue")
par(new=TRUE)
lines(c(0,sum(valid.df$binaryResult.n))~c(0,dim(valid.df)[1]), col="gray", lty=2)


# compute deciles and plot decile-wise chart
par(mfrow=c(1,3))
dec.rf <- gain.rf$mean.resp/mean(valid.df$binaryResult.n)
barplot(dec.rf, names.arg = gain.rf$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Random Forest")
dec.lr <- gain.lr$mean.resp/mean(valid.df$binaryResult.n)
barplot(dec.lr, names.arg = gain.lr$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Logistic Regression")
dec.nb <- gain.nb$mean.resp/mean(valid.df$binaryResult.n)
barplot(dec.nb, names.arg = gain.nb$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Naive Bayes")
dec.nn <- gain.nb$mean.resp/mean(valid.df$binaryResult.n)
barplot(dec.nn, names.arg = gain.nn$depth, ylim = c(0,9), 
        xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise: Neural Net")

# ROC
# install.packages("pROC")
library(pROC)
roc.rf <- roc(valid.df$binaryResult.n, valid.df$pred_rf.prob$X1)
roc.lr <- roc(valid.df$binaryResult.n, valid.df$pred_lr.prob$X1)
roc.nb <- roc(valid.df$binaryResult.n, valid.df$pred_nb.prob$X1)
roc.nn <- roc(valid.df$binaryResult.n, valid.df$pred_nn.prob$X1)

par(mfcol=c(1,1))
plot(roc.rf,col="red")
par(new=TRUE)
plot(roc.lr,col="green")
par(new=TRUE)
plot(roc.nb,col="blue")
par(new=TRUE)
plot(roc.nn,col="yellow")

auc(roc.rf)
auc(roc.lr)
auc(roc.nb)
auc(roc.nn)

# Ensemble using Averaging*************************************
# Taking average of predicted probabilities
valid.df$pred_avg<-(valid.df$pred_rf.prob$X1+valid.df$pred_lr.prob$X1+valid.df$pred_nb.prob$X1+valid.df$pred_nn.prob$X1)/4
#Splitting into binary classes at 0.5
valid.df$pred_class<-as.factor(ifelse(valid.df$pred_avg>0.5,'X1','X0'))
ensemble.averaging<-confusionMatrix(valid.df$binaryResult,valid.df$pred_class)

# Ensemble using Majority Voting
valid.df$pred_majority<-as.factor(ifelse(valid.df$pred_rf=='X1' & valid.df$pred_nn=='X1','X1',
                                         ifelse(valid.df$pred_rf=='X1' & valid.df$pred_lr=='X1','X1',
                                                ifelse(valid.df$pred_nn=='X1' & valid.df$pred_lr=='X1','X1','X0'))))
ensemble.voting<-confusionMatrix(valid.df$binaryResult,valid.df$pred_majority)


# Ensemble using Weighted Average____________________________________________________
# Taking weighted average of predictions
valid.df$pred_weighted_avg<-(valid.df$pred_rf.prob$X1*0.50)+(valid.df$pred_lr.prob$X1*0.25)+(valid.df$pred_nb.prob$X1*0.5)+(valid.df$pred_nn.prob$X1*0.5)
#Splitting into binary classes at 0.5
valid.df$pred_weighted_avg<-as.factor(ifelse(valid.df$pred_weighted_avg>0.5,'X1','X0'))
ensemble.weighted<-confusionMatrix(valid.df$binaryResult,valid.df$pred_weighted_avg)

con_rf<-confusionMatrix(valid.df$binaryResult,valid.df$pred_rf)
con_lr<-confusionMatrix(valid.df$binaryResult,valid.df$pred_lr)
con_nb<-confusionMatrix(valid.df$binaryResult,valid.df$pred_nb)
con_nn<-confusionMatrix(valid.df$binaryResult,valid.df$pred_nn)

c1<-rbind("Averaging","Voting","Random Forest", 
          "Logistic Regress", "Naive Bayes","Neural Net")
c2<-rbind(ensemble.averaging$overall[1],ensemble.voting$overall[1],con_rf$overall[1],
          con_lr$overall[1],con_nb$overall[1], con_nn$overall[1])
D1<-cbind(c1,c2)
D1


