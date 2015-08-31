library(ggplot2)
library(party)
library(rpart)

extractFeature <- function(data) {
  
  features <- c("Pclass", "Age", "SibSp", "Parch", "Fare", "Cat")
  fea <- data[, features]
  
  fea$Title = sapply(as.character(data$Name), function(x) strsplit(x,'[.,]')[[1]][2])
  fea$Title = gsub(' ', '', fea$Title)
  #print(class(fea$Title))
  fea$Title[fea$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  fea$Title[fea$Title %in% c('Dona', 'Lady', 'the Countess', 'theCountess', 'Jonkheer')] <- 'Lady'
  fea$Title[fea$Title %in% c('Mme', 'Ms')] <- 'Mrs'
  fea$Title[fea$Title %in% c('Mlle')] <- 'Miss'
  fea$Title = as.factor(fea$Title)
  print(summary(fea$Title))
  
  #predict age
  predict_age <- rpart(Age ~ Title, data = fea[!is.na(fea$Age), ], method = "anova")
  fea$Age[is.na(fea$Age)] <- predict(predict_age, fea[is.na(fea$Age), ])
    
  #predict fare
  predict_fare <- rpart(Fare ~ Pclass, data = fea[!is.na(fea$Fare) & fea$Fare != 0, ], method = "anova")
  fea$Fare[is.na(fea$Fare) | fea$Fare == 0] <- predict(predict_fare, fea[is.na(fea$Fare) | fea$Fare == 0, ])
  
  fea$Gender <- 0
  fea$Gender <- as.numeric(sapply(data$Sex, function(x) {as.character(x) == as.character("male")}))
  
  fea$Mother <- 0
  fea$Mother[fea$Gender == 0 & fea$Parch > 0 & fea$Age > 18 & fea$Title != 'Miss'] <- 1
  
  fea$Child <- 0
  fea$Child[fea$Parch > 0 & fea$Parch <= 18] <- 1
  
  data$Embarked <- as.character(data$Embarked)
  data$Embarked[data$Embarked != "S" & data$Embarked != "C" & data$Embarked != "Q"] <- which.max(table(data$Embarked))
  fea$Port_C = as.numeric(sapply(data$Embarked, function(x) {as.character(x) == as.character("C")}))
  fea$Port_Q = as.numeric(sapply(data$Embarked, function(x) {as.character(x) == as.character("Q")}))
  fea$Port_S = as.numeric(sapply(data$Embarked, function(x) {as.character(x) == as.character("S")}))
  
  fea$Surname <- sapply(as.character(data$Name), function(x) strsplit(x, '[.,]')[[1]][1]);
  family_id <- paste0(fea$FamilySize, fea$Surname)
  fea$Family_id <- as.factor(family_id)
  family_table <- data.frame(table(fea$Family_id))
  SmallFamily <- family_table$Var1[family_table$Freq <= 2]
  family_id[family_id %in% SmallFamily] <- "small"
  fea$Family_id2 <- as.factor(family_id)
  #print(levels(fea$Family_id2))
    
  fea$Deck <- sapply(as.character(data$Cabin), function(x) strsplit(x, NULL)[[1]][1]);
  #print(class(fea$Deck))
  fea$Deck[is.na(fea$Deck)] <- "EMPTY"
  fea$Deck <- as.factor(fea$Deck)
  
  fea$CabinNum <- sapply(as.character(data$Cabin), function(x) strsplit(x, NULL)[[1]][2]);
  fea$CabinNum <- as.numeric(fea$CabinNum)
  num <- subset(fea$CabinNum, !is.na(fea$CabinNum))
  CabinNumCluster <- kmeans(num, 3)
  #print(fea$CabinNum)
  #print(summary(CabinNumCluster$cluster))
  fea$CabinPos[!is.na(fea$CabinNum)] <- CabinNumCluster$cluster;
  print(class(fea$CabinPos))
  fea$CabinPos <- as.factor(fea$CabinPos)
  levels(fea$CabinPos) <- c("Front", "Middle", "End")
  fea$CabinNum <- NULL
  
  print(names(fea))
  return (fea)
}

set.seed(1)
train <- read.csv("input/train.csv", header = T)
test <- read.csv("input/test.csv", header = T)
train$Cat <- "train"
test$Cat <- "test"

test$Survived <- NA
full <- rbind(train, test)

full <- extractFeature(full)
feaTrain <- full[full$Cat == "train", ]
feaTest <- full[full$Cat == "test", ]

#rf <- randomForest(feaTrain, as.factor(train$Survived), ntree = 100, importance = TRUE)

feaTrain <- data.frame(Survived = as.factor(train$Survived), feaTrain)

rf <- cforest(feaTrain$Survived ~ Pclass + Age + SibSp + Parch + Fare + Gender + Title + Mother + Child +
                Port_S + Port_Q+ Port_C + Family_id2 + CabinPos + Deck, data = feaTrain, 
                controls=cforest_unbiased(ntree=200, mtry=3))
pre <- predict(rf, newdata = feaTest, OOB = TRUE, type = "response")

pre <- predict(rf, newdata = feaTest)

out <- data.frame(test$PassengerId, pre)

names(out) <- cbind("PassengerId", "Survived")

write.csv(out, file = "Survived.csv", row.names = FALSE)

pre_train <- predict(rf, newdata = feaTrain)

out_train <- data.frame(train$Survived, pre_train)

names(out_train) <- cbind("Survived", "pre")
print(sum(out_train$Survived == out_train$pre) / length(out_train$Survived))

