library(xgboost)
set.seed(1)

# -------------------------------------------------
num.class <- length(levels(final.train$fault_severity)) + 1

# -------------------------------------------------
feature.names <- names(final.train)
feature.names <- feature.names[-c(1, 3)]
# feature.names <- feature.names[-c(1, 55)]
feature.names <- feature.names[-c(55)]
head(feature.names)
feature.formula <- formula(paste('fault_severity ~ ', paste(feature.names, collapse = ' + '), sep = ''))



# -------------------------------------------------
dfull <- xgb.DMatrix(data.matrix(final.train[, feature.names]), label = final.train$fault_severity)
dtest <- data.matrix(final.test[, feature.names])
results <- list()
for (i in 1:500)
{
  print(i)
  print(paste0("Started: ", Sys.time()))
  
  # -------------------------------------------------
  indexes <- sample(seq_len(nrow(final.train)), floor(nrow(final.train)*0.85))
  dtrain <- xgb.DMatrix(data.matrix(final.train[indexes, feature.names]), label = final.train[indexes, 'fault_severity'])
  dvalid <- xgb.DMatrix(data.matrix(final.train[-indexes, feature.names]), label = final.train[-indexes, 'fault_severity'])
  watchlist <- list(eval = dvalid, train = dtrain)
#   # -------------------------------------------------
#   indexes <- sample(seq_len(nrow(final.train)), floor(nrow(final.train)*0.85))
#   data <- sparse.model.matrix(feature.formula, data = final.train[indexes, ])
#   sparseMatrixColNamesTrain <- colnames(data)
#   dtrain <- xgb.DMatrix(data, label = final.train[indexes, 'fault_severity'])
#   rm(data)
#   dvalid <- xgb.DMatrix(sparse.model.matrix(feature.formula, data = final.train[-indexes, ]),
#                         label = final.train[-indexes, 'fault_severity'])
#   watchlist <- list(eval = dvalid, train = dtrain)
#   
#   dtest_cv <- final.test[, feature.names]
#   dtest_cv$fault_severity = 0
#   dtest_cv[is.na(dtest_cv)]<- 0
#   dtest <- sparse.model.matrix(feature.formula, data = dtest_cv)
  
  
  # -------------------------------------------------
  params <- list(booster = "gbtree", objective = "multi:softprob", # multi:softprob   multi:softmax
                 max_depth = floor(runif(1, min=6, max=9)), eta = round(runif(1, min=0.01, max=0.15), 3),
                 colsample_bytree = 0.75, subsample = 0.95)
  model1 <- xgb.train(params = params, data = dtrain,
                      nrounds = 10001, early.stop.round = 100, maximize = F,
                      eval_metric = 'mlogloss', num_class = num.class, # merror mlogloss ndcg5
                      watchlist = watchlist, print.every.n = 50)
  # feature.importance.1 <- xgb.importance(feature.names, model = model1)
  # feature.importance.1 <- xgb.importance(sparseMatrixColNamesTrain, model = model1)


  print(paste0("Model created: ", Sys.time()))
  
  pred.valid <- predict(model1, dvalid)
  zzz <- as.data.frame(t(matrix(pred.valid, nrow=num.class)))
  valid.mlogloss <- MultiLogLoss(final.train[-indexes, 'fault_severity'], as.matrix(zzz[2:4]))
  print(paste0('mlogloss: ', valid.mlogloss))
  
  if (valid.mlogloss < 0.52) {
    pred.test <- predict(model1, dtest)
    results <- c(results, list(pred.test))

    print(paste0("Predicted: ", Sys.time()))
  }
}  


results.mean <- unlist(c(results[1]))
for(index in 2:length(results)) {
  results.mean <- cbind(results.mean, unlist(c(results[index])))
}
results.mean <- rowMeans(results.mean)

submission <- as.data.frame(t(matrix(results.mean, nrow=num.class)))
submission[, 1] <- final.test$id
colnames(submission) <- c('id', paste('predict_', levels(final.train$fault_severity), sep = ""))
submissionName <- paste0("results/ensemble_xg_full_bag_", format(Sys.time(), "%H_%M_%S"), '_6-10_001-015_55_95')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)







pred <- predict(model1, dtest)
predictions <- as.data.frame(t(matrix(pred, nrow=num.class)))
predictions[, 1] <- final.test$id
colnames(predictions) <- c('id', paste('predict_', levels(final.train$fault_severity), sep = ""))

write.csv(predictions, 'results/third_6_01_95_95.csv', sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)

