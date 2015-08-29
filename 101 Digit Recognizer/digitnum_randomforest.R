library(randomForest)

set.seed(0)

numTrain <- 42000
numTrees <- 200

train <- read.csv("train.csv")

rows <- sample(1:nrow(train), numTrain)
labels <- as.factor(train[rows, 1])

print(head(labels))

train <- train[rows, -1]

gc()
print(memory.size())
print(memory.limit())

rf <- randomForest(train, labels, ntree = numTrees)
rm(train)

test <- read.csv("test.csv")
pre <- predict(rf, newdata = test)

print(head(pre))

predictions <- data.frame(ImageId = 1:nrow(test), Label = pre)

write.csv(predictions, "predict.csv", row.names = FALSE)
