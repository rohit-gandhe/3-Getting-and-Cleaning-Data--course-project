require(plyr)

# Directories and files
uci_hard_dir <- "UCI\ HAR\ Dataset"
feature_file <- paste(uci_hard_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_hard_dir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_dir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_dir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_dir, "/test/subject_test.txt", sep = "")

# Load raw data
features <- read.table(feature_file, colClasses = c("character"))
activity_labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
x_train <- read.table(x_train_file)
y_train <- read.table(y_train_file)
subject_train <- read.table(subject_train_file)
x_test <- read.table(x_test_file)
y_test <- read.table(y_test_file)
subject_test <- read.table(subject_test_file)

# Merge the training and the test sets to create one data set
# Bind sensor data
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

#Label columns
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels

#Extract the measurements on the mean and standard deviation for each measurement.
sensor_data_mean_and_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]

# Use descriptive activity names to name the activities in the data set

sensor_data_mean_and_std <- join(sensor_data_mean_and_std, activity_labels, by = "ActivityId", match = "first")
sensor_data_mean_and_std <- sensor_data_mean_and_std[,-1]

# label the data set with descriptive names

# Remove parentheses
names(sensor_data_mean_and_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_and_std), perl = TRUE)
# Make valid names
names(sensor_data_mean_and_std) <- make.names(names(sensor_data_mean_and_std))
# Make cleane names
names(sensor_data_mean_and_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_and_std))
names(sensor_data_mean_and_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_and_std))

# Create a second, tidy data set with the average of each variable for each activity and each subject

sensor_avg_by_act_sub = ddply(sensor_data_mean_and_std, c("Subject","Activity"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "Tidy.txt", row.name=FALSE, sep = "\t")