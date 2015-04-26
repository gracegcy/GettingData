## 1. Merge the training and test data sets for x(data), y(lable) and subject.

X_merged <- rbind(X_train, X_test)
y_merged <- rbind(y_train, y_test)
subject_merged <- rbind(subject_train, subject_test)

## 2. Find the featurs for mean and std using rowSums which is faster than apply()
## http://stackoverflow.com/questions/25667941/r-efficiently-grep-characters-in-rows-of-large-data-frame
charMean <- "mean()"
rowsMean <- which(
  rowSums(
    `dim<-`(grepl(charMean, as.matrix(features), fixed=TRUE), dim(features))
  ) > 0
)

charStd <- "std()"
rowsStd <- which(
  rowSums(
    `dim<-`(grepl(charStd, as.matrix(features), fixed=TRUE), dim(features))
  ) > 0
)

featureMerged <- c(rowsStd, rowsMean)
featureMerged <- sort(featureMerged)

X_merged_sub <- X_merged[, featureMerged]

## 3. replace lable ID with lable names
y_merged_1 <- with(y_merged, V1==1)
y_merged[which(y_merged_1),] <- "WALKING"
y_merged_2 <- with(y_merged, V1==2)
y_merged[which(y_merged_2),] <- "WALKING_UPSTAIRS"
y_merged_3 <- with(y_merged, V1==3)
y_merged[which(y_merged_3),] <- "WALKING_DOWNSTAIRS"
y_merged_4 <- with(y_merged, V1==4)
y_merged[which(y_merged_4),] <- "SITTING"
y_merged_5 <- with(y_merged, V1==5)
y_merged[which(y_merged_5),] <- "STANDING"
y_merged_6 <- with(y_merged, V1==6)
y_merged[which(y_merged_6),] <- "LAYING"

## Append activity lables and subject ID to the X_merged file
X_mergedAll <- cbind(X_merged_sub, subject_merged, y_merged)

## 4. Appropriately labels the data set with descriptive variable names. 
reNameFunc <- function(x) {
  current <- names(X_mergedAll_Copy)[x]
  name <- as.numeric(substr(current, 2, nchar(current)))
  names(X_mergedAll_Copy)[x] <- as.character(features[name, 2])
  return (names(X_mergedAll_Copy)[x] )
}

for (i in 1:66)
 reNameFunc(i)

names(X_mergedAll_Copy)[67] <- "Subject ID"
names(X_mergedAll_Copy)[68] <- "Activity"

##5. creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
tidyData <- aggregate(X_mergedAll_Copy[,1:66], list(SubjectID=X_mergedAll_Copy$SubjectID, Activity=X_mergedAll_Copy$Activity), mean)
## export data
write.table(tidyData, "tidy.txt", sep="\t", row.name=FALSE)




