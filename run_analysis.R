# Coursera Assignment - Data cleaning with R

library(data.table)

# load all needed files
testx <- fread(".\\test\\X_test.txt")
testy <- fread(".\\test\\y_test.txt")
testsub <- fread(".\\test\\subject_test.txt")

trainx <- fread(".\\train\\X_train.txt")
trainy <- fread(".\\train\\y_train.txt")
trainsub <- fread(".\\train\\subject_train.txt")

features <- fread(".\\features.txt") # variable labels for test/train
activityLabels <- fread(".\\activity_labels.txt") # value labels for activity

# do some renaming to avoid confusion later
setnames(testy, c("V1"), c("activity"))
setnames(trainy, c("V1"), c("activity"))
setnames(testsub, c("V1"), c("subject"))
setnames(trainsub, c("V1"), c("subject"))

#--------------------------------------------------------
# 5 Steps of the assignment

# Step 1: Merges the training and the test sets to create
# one data set.

## Cbind all test/train data and then Rbind test and train
test <- cbind(testsub, testx, testy)
train <- cbind(trainsub, trainx, trainy)
dt <- rbind(train, test)

# Step 4: Appropriately labels the data set with
# descriptive variable names.

setnames(dt,
         names(dt[,2:562]),
         features$V2)

# Step 2: Extracts only the measurements on the mean and
# standard deviation for each measurement.

# get the indices first
cols <- c(1, # first variable is subject
          grep("std\\(\\)|mean\\(\\)", names(dt)),
          ncol(dt)) # last variable is activity
# subset and order dt
dt <- dt[order(subject, activity),
         ..cols] # fast subsetting with data.table
colnames(dt) <- gsub("\\(*\\)", "", colnames(dt)) # remove paranthesis from colnames
colnames(dt) <- gsub("\\-", "_", colnames(dt)) # replace dash with underscore

# 3. Uses descriptive activity names to name the activities
# in the data set

# Here I update the activity variable and transform it into
# a factor with labels according to the included Codebook.
dt[, activity := factor(activity,
                        levels = ..activityLabels$V1,
                        labels = ..activityLabels$V2)]

# 5. From the data set in step 4, creates a second, 
# independent tidy data set with the average of each 
# variable for each activity and each subject.

# get the name of all sd an mean variables
cols <- names(dt)[-c(1, ncol(dt))]

dt2 <- dt[, lapply(.SD, mean), # apply mean function
          by = .(subject, activity), # grouped by subject + activity
          .SDcols = cols] # to this columns


# I prefer long format over wide format.
# To get wide alternatively:
# dt3 <- dcast(dt2,
#              subject ~ activity,
#              value.var = names(dt2)[-(1:2)])

# Clean up
rm(list=setdiff(ls(), "dt2"))
# save tidy data as csv
write.table(dt2, file = "tidyData.txt", row.names = F)

