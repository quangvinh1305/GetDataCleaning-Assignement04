run_analysis <- function(){
  
  #1. Get the data
  ## Download and create file if file is not exists
  if (require('plyr')){
    print("plyr is loaded correctly")
  } else {
    print("trying to install plyr")
    install.packages("plyr")
    if(require('plyr')){
      print("plyr installed and loaded")
    } else {
      stop("could not install plyr")
    }
  }
  if (!file.exists('./data')){
    dir.create('./data')
  }
  
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  uhi_har_zip_file <- './data/UCI_HAR_Dataset.zip'
  if (!file.exists('./data/UCI HAR Dataset') && !file.exists(uhi_har_zip_file)){
    download.file(fileUrl, destfile = uhi_har_zip_file, method='curl')
    unzip(zipfile = uhi_har_zip_file, exdir = './data')
    unlink(uhi_har_zip_file)
  }
  path_uci_har <- file.path("./data" , "UCI HAR Dataset")
  #2. Read data from the targeted files
  ## Read the Activity files
  dataActivityTest  <- read.table(file.path(path_uci_har, "test" , "y_test.txt" ),header = FALSE)
  dataActivityTrain <- read.table(file.path(path_uci_har, "train", "y_train.txt"),header = FALSE)
  ## Read the Subject files
  dataSubjectTrain <- read.table(file.path(path_uci_har, "train", "subject_train.txt"),header = FALSE)
  dataSubjectTest  <- read.table(file.path(path_uci_har, "test" , "subject_test.txt"),header = FALSE)
  ## Read Fearures files
  dataFeaturesTest  <- read.table(file.path(path_uci_har, "test" , "X_test.txt" ),header = FALSE)
  dataFeaturesTrain <- read.table(file.path(path_uci_har, "train", "X_train.txt"),header = FALSE)
  #3. Merges the training and the test sets to create one data set
  ## Combine subject train and subject test data by row
  dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
  ## Combine active train and active test data by row
  dataActivity<- rbind(dataActivityTrain, dataActivityTest)
  ## Combine feature train and feature test data by row
  dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)
  ## Set name for every column when combine data
  names(dataSubject)<-c("subject")
  names(dataActivity)<- c("activity")
  dataFeaturesNames <- read.table(file.path(path_uci_har, "features.txt"),head=FALSE)
  names(dataFeatures)<- dataFeaturesNames$V2
  ## 1. Merge columns to get the data frame Data for all data
  dataCombine <- cbind(dataSubject, dataActivity)
  Data <- cbind(dataFeatures, dataCombine)
  #2. Extracts only the mean and standard deviation
  subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
  selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
  Data<-subset(Data,select=selectedNames)
  #3. Uses descriptive activity names to name the activities in the data set
  activityLabels <- read.table(file.path(path_uci_har, "activity_labels.txt"),header = FALSE)
  #4. Appropriately labels the data set with descriptive variable names
  names(Data)<-gsub("^t", "Time", names(Data))
  names(Data)<-gsub("^f", "Frequency", names(Data))
  names(Data)<-gsub("Acc", "Accelerometer", names(Data))
  names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
  names(Data)<-gsub("Mag", "Magnitude", names(Data))
  names(Data)<-gsub("BodyBody", "Body", names(Data))
  #5. From the data set in step 4, creates a second, 
  #independent tidy data set with the average 
  #of each variable for each activity and each subject.
  Data2<-aggregate(. ~subject + activity, Data, mean)
  Data2<-Data2[order(Data2$subject,Data2$activity),]
  write.table(Data2, file = "tidydata.txt",row.name=FALSE)
}