# run_analysis.R

library(dplyr)
#reading data
subject_test <- read.table("./test/subject_test.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_train <- read.table("./train/subject_train.txt")
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
featrues <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

# Merge test and train data
subject <- rbind(subject_test, subject_train)
X <- rbind(X_test, X_train)
y <- rbind(y_test, y_train)
rm(X_test, X_train, y_test, y_train, subject_test, subject_train)

# Extract measurement on mean and standard deviation only
extract_log <- grepl("mean[()]|std", featrues[,2])
new_X <- X[,extract_log]

# Uses descriptive activity names to name the activities in the data set
new_X <- mutate(new_X, activity_name = activity_labels[,2][as.matrix(y)])

# Labels the data set with descriptive variable names
names(new_X) <- c(as.character(featrues[,2][extract_log]), "activity_name")

# Creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
# Adding subjects names
new_X <- mutate(new_X, subject_name = as.matrix(subject))
new_X <- group_by(new_X, subject_name, activity_name)
new_X <- summarize_all(new_y, mean)

write.table(new_X, "./MeanTable.txt",row.name=FALSE)



