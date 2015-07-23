require(dplyr)
require(plyr)
require(LaF)

# This function takes the directory where the directory named "UCI HAR Dataset" resides as the input
# and generate a dataset file named  run_analysis_summary_dataset.csv
#
run_analysis <- function (directory) {
  # Define frequently used string literals
  uci_har_dataset <- "UCI HAR Dataset"
  train <- "train"
  test <- "test"
  column_types <- rep("string", times=561)
  column_widths <- rep(16, times=561)

  # Define features data frame containing mean and std by using the given features.txt file
  feats <- read.csv(file.path(directory,uci_har_dataset,"features.txt"), header=FALSE, sep=" ")
  feats <- mutate(feats, V2 = as.character(V2))
  feats <- filter(feats, grepl('-mean|-std', V2))
  feats$V2 <- gsub("\\(\\)","",feats$V2)
  feats$V2 <- gsub("([a-z])([A-Z])","\\1.\\2",feats$V2)
  feats$V2 <- gsub("Acc","Acceleration",feats$V2)
  feats$V2 <- gsub("Freq","Frequency",feats$V2)
  feats$V2 <- gsub("Gyro","Gyroscope",feats$V2)
  feats$V2 <- gsub("Mag","Magnitude",feats$V2)
  feats$V2 <- gsub("^t","Time",feats$V2)
  feats$V2 <- gsub("^f","Frequency",feats$V2)
  feats$V2 <- gsub("mean","Mean",feats$V2)
  feats$V2 <- gsub("std","Standard.Deviation",feats$V2)
  feats$V2 <- gsub("-",".",feats$V2)
  
  # Define activity-label data frame by using the given activity_lables.txt file
  actlab <- read.csv(file.path(directory,uci_har_dataset,"activity_labels.txt"), header=FALSE, sep=" ")
  actlab$V2 <- as.character(actlab$V2)
  names(actlab) <- c("activity_num", "activity_label")

  # Define data frame containing mean and std based on the given x_train.txt file
  xtrain <- laf_open_fwf(file.path(directory,uci_har_dataset,train,"X_train.txt"), 
                         column_types = column_types, 
                         column_widths = column_widths)
  meanstdtrain <- next_block(xtrain, columns = feats$V1, nrow = 8000)
  meanstdtrain <- as.matrix(meanstdtrain)
  class(meanstdtrain) <- "numeric"
  meanstdtrain <- as.data.frame(meanstdtrain)
  names(meanstdtrain) <- feats$V2
  
  # Define data frames containing activity and subject 
  # based on the given y_train.txt and subject_train.txt files
  ytrain <- read.csv(file.path(directory,uci_har_dataset,train,"y_train.txt"), header=FALSE)  
  names(ytrain) <- "activity"
  subjecttrain <- read.csv(file.path(directory,uci_har_dataset,train,"subject_train.txt"), header=FALSE)  
  names(subjecttrain) <- "subject"

  # Define data frame containing mean and std based on the given x_test.txt file
  xtest <- laf_open_fwf(file.path(directory,uci_har_dataset,test,"X_test.txt"), 
                        column_types = column_types, 
                        column_widths = column_widths)
  meanstdtest <- next_block(xtest, columns = feats$V1, nrow = 3000)
  meanstdtest <- as.matrix(meanstdtest)
  class(meanstdtest) <- "numeric"
  meanstdtest <- as.data.frame(meanstdtest)
  names(meanstdtest) <- feats$V2
  
  # Define data frames containing activity and subject 
  # based on the given y_test.txt and subject_test.txt files  
  ytest <- read.csv(file.path(directory,uci_har_dataset,test,"y_test.txt"), header=FALSE)
  ytest <- as.data.frame(ytest)
  names(ytest) <- "activity"
  subjecttest <- read.csv(file.path(directory,uci_har_dataset,test,"subject_test.txt"), header=FALSE)  
  names(subjecttest) <- "subject"

  # Consolidate train and test data frames
  meanstd <- rbind(meanstdtrain, meanstdtest)
  activity <- rbind(ytrain, ytest)
  subject <- rbind(subjecttrain, subjecttest)
  meanstdactsub <- cbind(meanstd, activity, subject)
  meanstdactsub <- mutate(meanstdactsub, subact = activity + 10 * subject)
  meanstdactsub <- arrange(meanstdactsub, subject, activity)
  
  # Create summary data frames
  meanstdactsubg <- group_by(meanstdactsub, subact, add=TRUE)
  meanstdactsubs <- summarise_each(meanstdactsubg, funs(mean), matches("Mean|Standard.Deviation"))
  meanstdactsubs <- as.data.frame(meanstdactsubs)
  meanstdactsubs <- mutate(meanstdactsubs, Subject = as.integer(subact / 10), Activity = subact - Subject * 10)
  meanstdactsubs <- select(meanstdactsubs, -subact)
  meanstdactsubs$Activity <- mapvalues(meanstdactsubs$Activity, from=actlab$activity_num, to=actlab$activity_label)
  meanstdactsubs <- meanstdactsubs[,c(ncol(meanstdactsubs)-1, ncol(meanstdactsubs), 1:(ncol(meanstdactsubs)-2))]

  # Output the summary data from into files
  write.table(meanstdactsubs, file = file.path(directory,"run_analysis_summary_dataset.txt"), row.names=FALSE)
}




