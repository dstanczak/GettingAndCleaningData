#This script defines function called run_analysis which does the following:
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.
#Function does not need any parameters list to execute. Just need to be run from the
#working directory where "UCI HAR Dataset" is placed.

run_analysis <- function(){
    
    #Read the test data
    testdata<-read.table("./UCI HAR Dataset/test/X_test.txt")
    testlabels<-read.table("./UCI HAR Dataset/test/y_test.txt")
    testsubj<-read.table("./UCI HAR Dataset/test/subject_test.txt")
    
    #Read the training data
    traindata<-read.table("./UCI HAR Dataset/train/X_train.txt")
    trainlabels<-read.table("./UCI HAR Dataset/train/y_train.txt")
    trainsubj<-read.table("./UCI HAR Dataset/train/subject_train.txt")
    
    #Merge test nad training data
    fulldata<-rbind(testdata,traindata)
    fullsubj<-rbind(testsubj,trainsubj)
    fulllabels<-rbind(testlabels,trainlabels)
    
    #Read and set the cnames for merged data
    vars<-read.table("./UCI HAR Dataset/features.txt")
    names(fulldata)<-vars[,2]
    names(fullsubj)<-c("subject")
    names(fulllabels)<-c("activity")
    
    #Keep only mean and std features
    keep<-vector()
    for (i in 1:nrow(vars)){
       if(   length(grep("mean()",as.character(vars[i,2]),fixed=TRUE))>0
          || length(grep("std()",as.character(vars[i,2]),fixed=TRUE))>0){
           keep<-c(keep,as.character(vars[i,2]))
       }
    }
    data<-fulldata[as.character(keep)]
  
    #Merge feature data with subject id and activity id
    data<-cbind(fullsubj,fulllabels,data)
    
    #Set activity names instead of ids in the data set
    actlabels<-read.table("./UCI HAR Dataset/activity_labels.txt")
    for (i in 1:nrow(data)){
        data[i,2]<-as.character(actlabels[data[i,2],2])
    }
    
    #creates a second, independent tidy data set with the average 
    #of each variable for each activity and each subject
    
    aggdata<-aggregate(data[,3:ncol(data)],by=list(subject=data$subject,activity=data$activity),mean)
    #View(data)
    #View(aggdata)
    
    #save the results
    write.table(aggdata,file="aggdata.txt",row.name=FALSE)
    
}
