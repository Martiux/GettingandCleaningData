run_analysis <- function(){
  
  # LIBRARIES
  library(data.table);
  library(stringr)
  
  # ESTABLISH PATHS
  path.data <- paste0(getwd(), "/c3_data/UCI HAR Dataset/");
  
  # GET DATA
  test_X <- as.data.table(read.table(paste0(path.data, "test/X_test.txt")));
  test_Y <- as.data.table(read.table(paste0(path.data, "test/y_test.txt")));
  test_S <- as.data.table(read.table(paste0(path.data, "test/subject_test.txt")));
  train_X <- as.data.table(read.table(paste0(path.data, "train/X_train.txt")));
  train_Y <- as.data.table(read.table(paste0(path.data, "train/y_train.txt")));
  train_S <- as.data.table(read.table(paste0(path.data, "train/subject_train.txt")));
  
  data.F <- as.data.table(read.table(paste0(path.data, "features.txt")));
  data.A <- as.data.table(read.table(paste0(path.data, "activity_labels.txt")));
  

  # LOOK FOR INDEX ON VARIABLES NAMES WITH "MEAN" AND "STD"
  pos.mean <- as.vector(regexpr("mean", data.F$V2));
  pos.mean <- which(pos.mean > 0);
  pos.meanFreq <- as.vector(regexpr("meanFreq", data.F$V2));
  pos.meanFreq <- which(pos.meanFreq > 0);
  pos.mean <- setdiff(pos.mean, pos.meanFreq);
  pos.std <- as.vector(regexpr("std", data.F$V2));
  pos.std <- which(pos.std > 0);
  idx <- sort(c(pos.mean, pos.std));
  
  # SET DESCRIPTIVE NAMES TO DATA
  setnames(data.F, 1:2, c("VAR", "DESC"));
  setnames(data.A, 1:2, c("ACT", "DESC"));
  
  # SET DECRIPTIVE VARIABLE NAMES
#   setnames(test_X, 1:ncol(test_X), as.character(data.F$DESC));
#   setnames(train_X, 1:ncol(train_X), as.character(data.F$DESC));
  data.F.desc <- str_replace_all(as.character(data.F$DESC), "[[:punct:]]", "");
  setnames(test_X, 1:ncol(test_X), as.character(data.F.desc));
  setnames(train_X, 1:ncol(train_X), as.character(data.F.desc));
  
  # PRINT DIMENSION OF DATA TABLES
  print(dim(test_X));
  print(dim(test_Y));
  print(dim(train_X));
  print(dim(train_Y));

  # CREATE DESCRIPTIVE NAMES FOR ACTIVITY AND TYPE (TEST OR TRAIN)
  setkey(data.A, ACT);
  # FOR TEST
  setnames(test_Y, 1, "ACT");
  setkey(test_Y, ACT);  
  DescActTest <- merge(test_Y, data.A);
  IdTest <- cbind(TYPE=rep("TEST", nrow(test_X)), DescActTest, test_S);
  setnames(IdTest, 4, "SUBJ");
  # FOR TRAIN
  setnames(train_Y, 1, "ACT");
  setkey(train_Y, ACT);  
  DescActTrain <- merge(train_Y, data.A);
  IdTrain <- cbind(TYPE=rep("TRAIN", nrow(train_X)), DescActTrain, train_S);
  setnames(IdTrain, 4, "SUBJ");
  
  # SELECT VARIABLES WITH MEAN AND STD; ADD ID
  # FOR TEST
  TestMeanStd <- test_X[, idx, with=FALSE];
  TestMeanStd <- cbind(IdTest, TestMeanStd);  
  # FOR TRAIN
  TrainMeanStd <- train_X[, idx, with=FALSE];
  TrainMeanStd <- cbind(IdTrain, TrainMeanStd);
  
  # JOIN TEST AND TRAIN IN ONE TIDY TABLE
  TestTrain <- rbind(TestMeanStd, TrainMeanStd);
  print(head(TestTrain[, 1:5, with=FALSE]));
  print(tail(TestTrain[, 1:5, with=FALSE]));
  
  
  # GET MEANS OF FEATURES GROUPED BY ACTIVITY AND SUBJECT
  TestTrainMean <- TestTrain[, list(mean(tBodyAccmeanX), mean(tBodyAccmeanY), mean(tBodyAccmeanZ), mean(tBodyAccstdX), mean(tBodyAccstdY), mean(tBodyAccstdZ), mean(tGravityAccmeanX), mean(tGravityAccmeanY), mean(tGravityAccmeanZ), mean(tGravityAccstdX), mean(tGravityAccstdY), mean(tGravityAccstdZ), mean(tBodyAccJerkmeanX), mean(tBodyAccJerkmeanY), mean(tBodyAccJerkmeanZ), mean(tBodyAccJerkstdX), mean(tBodyAccJerkstdY), mean(tBodyAccJerkstdZ), mean(tBodyGyromeanX), mean(tBodyGyromeanY), mean(tBodyGyromeanZ), mean(tBodyGyrostdX), mean(tBodyGyrostdY), mean(tBodyGyrostdZ), mean(tBodyGyroJerkmeanX), mean(tBodyGyroJerkmeanY), mean(tBodyGyroJerkmeanZ), mean(tBodyGyroJerkstdX), mean(tBodyGyroJerkstdY), mean(tBodyGyroJerkstdZ), mean(tBodyAccMagmean), mean(tBodyAccMagstd), mean(tGravityAccMagmean), mean(tGravityAccMagstd), mean(tBodyAccJerkMagmean), mean(tBodyAccJerkMagstd), mean(tBodyGyroMagmean), mean(tBodyGyroMagstd), mean(tBodyGyroJerkMagmean), mean(tBodyGyroJerkMagstd), mean(fBodyAccmeanX), mean(fBodyAccmeanY), mean(fBodyAccmeanZ), mean(fBodyAccstdX), mean(fBodyAccstdY), mean(fBodyAccstdZ), mean(fBodyAccJerkmeanX), mean(fBodyAccJerkmeanY), mean(fBodyAccJerkmeanZ), mean(fBodyAccJerkstdX), mean(fBodyAccJerkstdY), mean(fBodyAccJerkstdZ), mean(fBodyGyromeanX), mean(fBodyGyromeanY), mean(fBodyGyromeanZ), mean(fBodyGyrostdX), mean(fBodyGyrostdY), mean(fBodyGyrostdZ), mean(fBodyAccMagmean), mean(fBodyAccMagstd), mean(fBodyBodyAccJerkMagmean), mean(fBodyBodyAccJerkMagstd), mean(fBodyBodyGyroMagmean), mean(fBodyBodyGyroMagstd), mean(fBodyBodyGyroJerkMagmean), mean(fBodyBodyGyroJerkMagstd)), by=list(DESC, SUBJ)];
                                                                  
  setnames(TestTrainMean, 3:ncol(TestTrainMean), paste0("M", data.F.desc[idx]));
  print(head(TestTrainMean));
  print(dim(TestTrainMean)); 
  print(dim(TestTrain));


}