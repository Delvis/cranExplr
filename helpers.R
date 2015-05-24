cranid <- readRDS("cranid.rds")
require(caret)
# require(AppliedPredictiveModeling)
# transparentTheme(trans = .4)
# featurePlot(x = cranid[, 1:8],
#             y = cranid$Ancestry2,
#             plot = "ellipse",
#             ## Add a key at the top
#             auto.key = list(columns = 3))

set.seed(408)
cranid <- subset(cranid, select = -c(Ancestry, Sex, OL))
namekeeper <- levels(cranid$Ancestry2)
levels(cranid$Ancestry2) <- make.names(levels(cranid$Ancestry2))
cranid[, 1:29] <- sapply(cranid[, 1:29], as.numeric)
inTraining <- createDataPartition(cranid$Ancestry2, p = .75, list = FALSE)
training <- cranid[ inTraining,]
testing  <- cranid[-inTraining,]

#5k fold Cross-validation
fitControl <- trainControl(method = "cv", number = 5, repeats = 2)

set.seed(333)
fdaFit1 <- train(Ancestry2 ~ . , data = training, method = "fda", trControl = fitControl)
testPred <- predict(fdaFit1, testing)
confusionMatrix(testPred, testing$Ancestry2)


# saveRDS(fdaFit1, "model.rds")
