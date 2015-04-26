README
========================================================

This is a README document telling how the script run_Analysis.R works. 

1. Merge the training and test data sets for x(data), y(lable) and subject.

```r
X_train <- read.table("X_train.txt", quote="\"")
X_test <- read.table("X_test.txt", quote="\"")
y_train <- read.table("y_train.txt", quote="\"")
y_test <- read.table("y_test.txt", quote="\"")
subject_train <- read.table("subject_train.txt", quote="\"")
subject_test <- read.table("subject_test.txt", quote="\"")
X_merged <- rbind(X_train, X_test)
y_merged <- rbind(y_train, y_test)
subject_merged <- rbind(subject_train, subject_test)
```

2. Find the featurs for mean and std using rowSums() which is faster than apply()

```r
features <- read.table("features.txt", quote="\"")
charMean <- "mean()" #find those names containing mean()
rowsMean <- which(
  rowSums(
    `dim<-`(grepl(charMean, as.matrix(features), fixed=TRUE), dim(features))
  ) > 0
)

charStd <- "std()" #find those names containing std()
rowsStd <- which(
  rowSums(
    `dim<-`(grepl(charStd, as.matrix(features), fixed=TRUE), dim(features))
  ) > 0
)

featureMerged <- c(rowsStd, rowsMean)
featureMerged <- sort(featureMerged)

X_merged_sub <- X_merged[, featureMerged] # merge two lists
```


3. replace lable ID with lable names

```r
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
X_mergedAll_Copy <- cbind(X_merged_sub, subject_merged, y_merged)
```

## 4. Appropriately labels the data set with descriptive variable names. 

```r
reNameFunc <- function(x) {
  current <- names(X_mergedAll_Copy)[x]
  name <- as.numeric(substr(current, 2, nchar(current)))
  names(X_mergedAll_Copy)[x] <- as.character(features[name, 2])
  return (names(X_mergedAll_Copy)[x] )
}

for (i in 1:66)
 #reNameFunc(i)

names(X_mergedAll_Copy)[67] <- "SubjectID"
names(X_mergedAll_Copy)[68] <- "Activity"
```

## 5. creates a second, independent tidy data set 

```r
## with the average of each variable for each activity and each subject.
tidyData <- aggregate(X_mergedAll_Copy[,1:66], list(SubjectID=X_mergedAll_Copy$SubjectID, Activity=X_mergedAll_Copy$Activity), mean)
## export data
write.table(tidyData, "tidy.txt", sep="\t", row.name=FALSE)
```

## 6. Create codebook

```r
tidy <- read.table("tidy.txt", quote="\"")
varNames <- names(tidy)
description <- varNames
description <- gsub("Acc", "accelerometer ", description)
description <- gsub("Gyro", "gyroscope ", description)
description <- gsub("fBody", "Frequency of the Body ", description)
description <- gsub("fGravity", "Frequency of the Gravity ", description)
description <- gsub("tBody", "Time of the Body ", description)
description <- gsub("tGravity", "Time of the Gravity ", description)
description <- gsub("Jerk", "Jerk Signials ", description)
description <- gsub("Mag", "Magnitude of Signials ", description)
description <- gsub("X", "in X Direction ", description)
description <- gsub("Y", "in Y Direction ", description)
description <- gsub("Z", "in Z Direction ", description)
description <- gsub("mean...", "by average ", description)
description <- gsub("std...", "by standard deviation ", description)
outputStrings <- paste("* ", varNames, "\n\n", description,"\n", sep="")
head(outputStrings)
```

```
## [1] "* V1\n\nV1\n" "* V2\n\nV2\n" "* V3\n\nV3\n" "* V4\n\nV4\n"
## [5] "* V5\n\nV5\n" "* V6\n\nV6\n"
```

```r
write.table(outputStrings, "codebook.txt", quote=FALSE, row.names = FALSE, col.names=FALSE)
```
