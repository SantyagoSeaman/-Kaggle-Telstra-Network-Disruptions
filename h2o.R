library(h2o)
library(data.table)
library(Metrics)
library(MLmetrics)
h2o.init(nthreads=-1, min_mem_size = "2G", max_mem_size = "8G")
#h2o.shutdown(F)


feature.names <- names(final.train)
feature.names <- feature.names[-c(1, 3)]
head(feature.names)
feature.formula <- formula(paste('fault_severity ~ ', paste(feature.names, collapse = ' + '), sep = ''))

trainFullHex <- as.h2o(final.train[, c(feature.names, 'fault_severity')], destination_frame="train.full.hex")

predictionsList <- list()
for(i in 1:20)
{
  indexes <- sample(seq_len(nrow(final.train)), floor(nrow(final.train)*0.95))
  trainHex <- as.h2o(final.train[indexes, c(feature.names, 'fault_severity')], destination_frame="train.hex")
  #summary(trainHex)
  validHex <- as.h2o(final.train[-indexes, c(feature.names, 'fault_severity')], destination_frame="valid.hex")
  testHex <- as.h2o(final.test[, feature.names], destination_frame="test.hex")

  rfHex <- h2o.randomForest(x = feature.names, y = "fault_severity",
                            training_frame = trainHex, model_id = "rfStarter.hex",
                            ntrees = 200,
                            sample_rate = 0.9,
                            mtries = 50,
                            max_depth = 8,
                            nfolds = 5,
                            stopping_rounds = 20)

  gbmHex <- h2o.gbm(x = feature.names,
                    y = "fault_severity", training_frame=trainHex, model_id="gbmStarter.hex",
                    distribution="AUTO",
                    nfolds = 5,
                    stopping_rounds = 50,
                    seed = 123,
                    ntrees = 1000,
                    max_depth = 6,
                    #min_rows = 10,
                    #learn_rate = 0.1,
                    sample_rate = 0.9,
                    col_sample_rate = 0.9)

  #predictionsList <- c(predictionsList, h2o.predict(rfHex, testHex))
  #predictionsList <- c(predictionsList, h2o.predict(gbmHex, testHex))
  
  zzz <- as.data.frame(h2o.predict(rfHex, trainHex))
  MultiLogLoss(final.train[indexes, 'fault_severity'], as.matrix(zzz[2:4]))
  
  zzz <- as.data.frame(h2o.predict(rfHex, validHex))
  MultiLogLoss(final.train[-indexes, 'fault_severity'], as.matrix(zzz[2:4]))
  
  zzz <- as.data.frame(h2o.predict(rfHex, testHex))

}


zzz <- as.data.frame(h2o.predict(gbmHex, trainHex))
MultiLogLoss(final.train[indexes, 'fault_severity'], as.matrix(zzz[2:4]))

zzz <- as.data.frame(h2o.predict(gbmHex, validHex))
MultiLogLoss(final.train[-indexes, 'fault_severity'], as.matrix(zzz[2:4]))


zzz$predict <- final.test$id
submission <- zzz
colnames(submission) <- c('id', paste('predict_', levels(final.train$fault_severity), sep = ""))
submissionName <- paste0("results/h2o_rf_", format(Sys.time(), "%H_%M_%S"), '_200_09_50_8')
submissionFile <- paste0(submissionName, ".csv")
write.csv(submission, submissionFile, sep=",", dec=".", col.names=TRUE, row.names=FALSE, quote = FALSE)

  