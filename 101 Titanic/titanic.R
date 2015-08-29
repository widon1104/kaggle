library(randomForest)
library(ggplot2)
library(rpart)

extractFeature <- function(data) {
  
  features <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")
  fea <- data[, features]
  
  #predict age
  predict_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = fea[!is.na(fea$Age), ], method = "anova")
  fea$Age[is.na(fea$Age)] <- predict(predict_age, fea[is.na(fea$Age), ])
    
  #predict fare
  predict_fare <- rpart(Fare ~ Pclass + Sex + Age + SibSp + Parch, data = fea[!is.na(fea$Fare) & fea$Fare != 0, ], method = "anova")
  fea$Fare[is.na(fea$Fare) | fea$Fare == 0] <- predict(predict_fare, fea[is.na(fea$Fare) | fea$Fare == 0, ])
  
  return (fea)
}

set.seed(1)
train <- read.csv("input/train.csv", header = T)
test <- read.csv("input/test.csv", header = T)

feaTrain <- extractFeature(train)
feaTest <- extractFeature(test)

#level should be the same
#levels(feaTest$Embarked) <- levels(feaTrain$Embarked)

rf <- randomForest(feaTrain, as.factor(train$Survived), ntree = 100, importance = TRUE)

pre <- predict(rf, newdata = feaTest, type = "class")

out <- data.frame(test$PassengerId, pre)

names(out) <- cbind("PassengerId", "Survived")

write.csv(out, file = "Survived.csv", row.names = FALSE)

pre_train <- predict(rf, newdata = feaTrain)

out_train <- data.frame(train$Survived, pre_train)

names(out_train) <- cbind("Survived", "pre")
print(sum(out_train$Survived == out_train$pre) / length(out_train$Survived))

imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), importance = imp[, 1])

p <- ggplot(featureImportance, aes(x = reorder(Feature, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "#53cfff") + 
  coord_flip() + 
  theme_light(base_size = 20) +
  xlab("") +
  ylab("Importance") +
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title = element_text(size = 18))

ggsave("2_feature_importance.png", p)
