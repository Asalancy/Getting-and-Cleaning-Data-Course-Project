Getting_and_Cleaning_Data_Course_Project_Script<-function(directory) {

      # Set the appropriate directory for source files
      setwd(directory)
      
      # Read in the training data: x, y, and subjects
      trainData_x <- read.table("./train/X_train.txt")
      trainData_y <- read.table("./train/y_train.txt")
      trainData_Subject <- read.table("./train/subject_train.txt")

      # Read in the test data: x, y, and subjects
      testData_x <- read.table("./test/X_test.txt")
      testData_y <- read.table("./test/y_test.txt")
      testData_Subject <- read.table("./test/subject_test.txt")

      # Read in the appropriate labels and set the activity IDs and names
      features <- read.table("./features.txt")
      activity_labels <- read.table("./activity_labels.txt")
      names(activity_labels) = c("ActivityID","Activityname")
      
      # Bind together the training and test data: x, y, and subjects
      totalData <- rbind(trainData_x,testData_x) 
      total_labels <- rbind(trainData_y,testData_y) 
      total_Subjects <- rbind(trainData_Subject,testData_Subject) 
      
      # Assign names to the data set and label sets
      names(totalData) = features[[2]]
      names(total_labels) = "ActivityID"
      names(total_Subjects) = "Subjects"
      
      # Identify the variable names with "mean" or "std" 
      meansAndstdsFound <- grep("mean|std",names(totalData)) 

      # Create the data set with only the means and stds using the vector meansAndstdsFound
      meansAndstdsData <- totalData[meansAndstdsFound]
      
      activities <- merge(activity_labels,total_labels,"ActivityID")
      
      # Add the activity type to the dataset
      meansAndstdsData$activities<-activities[[2]]
      # Add the subjects to the dataset
      meansAndstdsData$subject<-total_Subjects[[1]]
      
      # Substitute more meaningful names - some features are labeled "BodyBody", have simplified here to just "Body"
      names(meansAndstdsData)<-gsub("tBodyAccJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyGyroJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyGyroMag-","Body acceleration signal in time domain applied to Fast Fourrier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyGyroJerk-","Body acceleration jerk signal in time domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tGravityAccMag-","Gravity acceleration signal in time domain applied to Fast Fourier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyBodyGyroJerkMag-","Body acceleration jerk signal in frequency domain applied to Fast Fourier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyBodyAccJerkMag-","Body acceleration jerk signal in frequency domain applied to Fast Fourrier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyBodyGyroMag-","Body acceleration signal in frequency domain applied to Fast Fourier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyAccJerk-","Body acceleration jerk signal in frequency domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyAccMag-","Body acceleration signal in frequency domain applied to Fast Fourier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyAccMag-","Body acceleration signal in time domain applied to Fast Fourier Transform",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyAccJerk-","Body acceleration jerk signal in time domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyAcc-","Body acceleration signal in time domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tGravityAcc-","Gravity acceleration signal in time domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("tBodyGyro-","Body acceleration signal in time domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyAcc-","Body acceleration signal in frequency domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("fBodyGyro-","Body acceleration signal in frequency domain",names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("mean()", " mean ", names(meansAndstdsData))
      names(meansAndstdsData)<-gsub("std()", " SD ", names(meansAndstdsData))
      
      # Scrub any remaining special characters
      names(meansAndstdsData)<-gsub("[()-]","",names(meansAndstdsData))
      
      # Create the dataset of the averages
      meansAndstdsDataAverages<-aggregate(meansAndstdsData[,1:79],list(activities = meansAndstdsData$activities, subjects=meansAndstdsData$subject),mean, na.rm=TRUE)

      # Write the results
      write.table(meansAndstdsData, "tidy_data.txt")
      write.table(meansAndstdsDataAverages,"tidy_data_averages.txt",row.name=FALSE)
}