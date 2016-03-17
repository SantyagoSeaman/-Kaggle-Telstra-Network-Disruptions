#library(data.table)
library(xgboost)
library(Matrix)
library(methods)
#library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
library(stringr)
#library(reshape2)
options(scipen=999)


orig.event_type <- read.csv("input/event_type.csv", stringsAsFactors = T)
orig.log_feature <- read.csv("input/log_feature.csv", stringsAsFactors = T)
orig.resource_type <- read.csv("input/resource_type.csv", stringsAsFactors = T)
orig.severity_type <- read.csv("input/severity_type.csv", stringsAsFactors = T)


orig.train <- read.csv("input/train.csv", stringsAsFactors = F)
orig.train$fault_severity <- factor(as.character(orig.train$fault_severity))
orig.test <- read.csv("input/test.csv", stringsAsFactors = F)
orig.test$fault_severity <- factor(NA, levels = levels(orig.train$fault_severity))
orig.sample_submission <- read.csv("input/sample_submission.csv", stringsAsFactors = F)


# ---------------------------------------------------
# setdiff(orig.train$id, orig.resource_type$id)
# setdiff(orig.test$id, orig.resource_type$id)
# 
# setdiff(orig.train$id, orig.event_type$id)
# setdiff(orig.test$id, orig.event_type$id)
# 
# setdiff(orig.train$id, orig.log_feature$id)
# setdiff(orig.test$id, orig.log_feature$id)
# 
# nrow(orig.test[!(orig.test$id %in% orig.event_type$id), ])
# nrow(orig.test[!(orig.test$id %in% orig.log_feature$id), ])


# ---------------------------------------------------
merged <- rbind(orig.train, orig.test)
merged$location <- as.factor(merged$location)
merged.rows <- nrow(merged)

# ---------------------------------------------------
merged <- merge(merged, orig.severity_type, by = 'id')

# ---------------------------------------------------
event_types <- levels(orig.event_type$event_type)
event_types.df <- as.data.frame(matrix(ncol=length(event_types)+1, nrow=merged.rows))
colnames(event_types.df) <- c('id', event_types)
event_types.df[, ] <- 0
event_types.df$id <- merged$id

for(i in 1:nrow(orig.event_type)) {
  cur <- orig.event_type[i, ]
  event_types.df[event_types.df$id == cur$id, as.character(cur$event_type)] <- 1
  if (i %% 1000 == 0) {
    print(i)
  }
}

merged <- cbind(merged, event_types.df[, -1])

# ---------------------------------------------------
log_features <- levels(orig.log_feature$log_feature)
log_features.df <- as.data.frame(matrix(ncol=length(log_features)+1, nrow=merged.rows))
colnames(log_features.df) <- c('id', log_features)
log_features.df[, ] <- 0
log_features.df$id <- merged$id

for(i in 1:nrow(orig.log_feature)) {
  cur <- orig.log_feature[i, ]
  log_features.df[log_features.df$id == cur$id, as.character(cur$log_feature)] <- cur$volume
  if (i %% 1000 == 0) {
    print(i)
  }
}

merged <- cbind(merged, log_features.df[, -1])

# ---------------------------------------------------
resource_types <- levels(orig.resource_type$resource_type)
resource_types.df <- as.data.frame(matrix(ncol=length(resource_types)+1, nrow=merged.rows))
colnames(resource_types.df) <- c('id', resource_types)
resource_types.df[, ] <- 0
resource_types.df$id <- merged$id

for(i in 1:nrow(orig.resource_type)) {
  cur <- orig.resource_type[i, ]
  resource_types.df[resource_types.df$id == cur$id, as.character(cur$resource_type)] <- 1
  if (i %% 1000 == 0) {
    print(i)
  }
}

merged <- cbind(merged, resource_types.df[, -1])

# ---------------------------------------------------
severity_types <- levels(orig.severity_type$severity_type)
severity_types.df <- as.data.frame(matrix(ncol=length(severity_types)+1, nrow=merged.rows))
colnames(severity_types.df) <- c('id', severity_types)
severity_types.df[, ] <- 0
severity_types.df$id <- merged$id

for(i in 1:nrow(orig.severity_type)) {
  cur <- orig.severity_type[i, ]
  severity_types.df[severity_types.df$id == cur$id, as.character(cur$severity_type)] <- 1
  if (i %% 1000 == 0) {
    print(i)
  }
}

merged <- cbind(merged, severity_types.df[, -1])

# ---------------------------------------------------
# Split

full.col.names <- colnames(merged)
full.col.names <- gsub(' ', '_', full.col.names)
colnames(merged) <- full.col.names

final.train <- merged[!is.na(merged$fault_severity), ]
final.test <- merged[is.na(merged$fault_severity), ]

