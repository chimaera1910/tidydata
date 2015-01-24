# Read the data into R and check column names and classes.  
LData <- read.table('UCI HAR Dataset/features.txt', 
                    header=FALSE, col.names=c('id', 'CName'), 
                    colClasses = c('numeric', 'character'))

## Step.1 Merge training and testing data set using the LData created in the previous step.



  # Load LData
  result <- read.table(data, header=FALSE, col.names=LData$CName, 
                       colClasses = rep("numeric", nrow(LData)))
  # Load labels
  result_label <- read.table(labels, header=FALSE, col.names=c('label'), 
                             colClasses = c('numeric'))
  # Load subjects
  result_subject <- read.table(subject_file, header=FALSE, 
                               col.names=c('subject'), 
                               colClasses = c('numeric') )
  # merge labels and LData for data set.
  result$label <- result_label$label
  result$subject <- result_subject$subject
  result  

# Load training data set
train <- loadData('train')
test <- loadData('test')

# merge train and test data
alldata <- rbind(train, test)

## Step.2 Only keep the required mean() and std() measurements
requireLData <- grepl("mean\\(\\)",LData$CName) | 
  grepl("std\\(\\)",LData$CName )
requireCols <- LData[requireLData,]$id
requireData <- alldata[, requireCols]

# append label and subject since they are required in the final result
requireData$label <- alldata$label
requireData$subject <- alldata$subject

## Step. 3 & 4.
# Load activity labels

# Join activity labels with the data
requireData <- merge(requireData, activity_labels, by.x = 'label', by.y = 'id')
# drop the numeric label column since we now have the activity_label column
requireData <- requireData[, !(names(requireData) %in% c('label'))]

## Step. 5, calculate mean of each subject and each activity_label
library(reshape2)

meltD <- melt(requireData, id = c('subject', 'activity_label'))
result <- dcast(meltD, subject + activity_label ~ variable, mean)

# function to add a prefix
addPrefix <- function(x, prefix) {
  paste(prefix, x, sep="")
}

# set names for the columns
headerNames <- gsub("\\.+", ".", names(result))
headerNames <- gsub("\\.$", "", headerNames)
headerNames <- sapply(headerNames, addPrefix, "mean.of.")
headerNames[1] <- 'subject'
headerNames[2] <- 'activity'

names(result) <- headerNames

# write the data to a tidy data set as txt file for submission into coursera site.
write.table(result, "finaltidydataset.txt", row.names=FALSE)
