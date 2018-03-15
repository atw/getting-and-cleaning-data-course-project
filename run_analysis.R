library(dplyr)

DATA_DIR <- "data"
DATASET_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
DATASET_FILES <- file.path(
    DATA_DIR,
    c(
        "activity_labels.txt",
        "features.txt",
        "subject_test.txt",
        "subject_train.txt",
        "X_test.txt",
        "X_train.txt",
        "y_test.txt",
        "y_train.txt"
    )
)
names(DATASET_FILES) <- basename(DATASET_FILES)
DATASET_SUMMARY_FILE <- file.path(DATA_DIR, "summary.txt")

# dataset_exist function checks if files from the UCI HAR Dataset exist
# in data directory
dataset_exist <- function(files = DATASET_FILES, dir = DATA_DIR) {
    file.exists(dir) & all(basename(files) %in% dir(dir))
}

# download_dataset function downloads, unzips & saves the UCI HAR Dataset files
download_dataset <- function(dataset_url = DATASET_URL, dest_dir = DATA_DIR) {
    if (!dataset_exist()) {
        # create temporary file for the zip file
        temp <- tempfile()
        # Create data directory if it doesn't exist
        if (!file.exists(dest_dir)) { dir.create(dest_dir) }
        message("Downloading UCI HAR Dataset")
        download.file(dataset_url, destfile = temp, mode = "wb")
        # unzip the dataset
        unzip(temp, overwrite = TRUE, junkpaths = TRUE, exdir = DATA_DIR)
        # delete the zip file
        unlink(temp)
    } else {
        message("UCI HAR Dataset found in data directory")
    }
}

# combine_data_for_means_and_std function does the following:
# - loads the training and test sets
# - extracts only the measurements on the mean and standard deviation for each
#   measurement
# - appropriately labels the data set with descriptive variable names
# - merges the training and the test sets to create one data set
# - uses descriptive activity names to name the activities in the data set
combine_data_for_means_and_std <- function(dataset = DATASET_FILES) {
    message("Combining data")

    # Read variables labels
    features <- read.table(
        dataset["features.txt"],
        col.names = c("id", "name")
    )
    # Format the variable names by removing the non alphabetic or numeric
    # characters
    normalized_feature_names <- gsub("[^A-Za-z0-9]", "", features$name)
    # Start Mean and Std in variable names with uppercase
    normalized_feature_names <- sub("mean", "Mean", normalized_feature_names)
    normalized_feature_names <- sub("std", "Std", normalized_feature_names)

    # Create logical vector which represents only the variables for measurements
    # on the mean and standard deviation
    selected_features_logical <- grepl("(mean|std)\\(\\)", features$name)

    # Based on the logical vector above create a vector for column classes.
    # Variables for mean and standard deviation get numeric and the other
    # variables get NULL. The NULL value will allow skipping the columns we are
    # not interested in.
    selected_features_classes <- sapply(selected_features_logical, function(i) {
        if (i) "numeric"
        else "NULL"
    })

    # Read the X_test and X_train data which containes variables with various
    # measurements from the phone sensors.
    # Apply the descriptive column names using the col.names attribute.
    # Read only the variables for measurements on the mean and standard
    # deviation using NULL values in the colClasses attribute
    X_test <- read.table(
        dataset["X_test.txt"],
        colClasses = selected_features_classes,
        col.names = normalized_feature_names
    )
    X_train <- read.table(
        dataset["X_train.txt"],
        colClasses = selected_features_classes,
        col.names = normalized_feature_names
    )

    # Read the y_test and y_train data which contains information about activity
    # type
    y_test <- read.table(
        dataset["y_test.txt"],
        colClasses = "factor",
        col.names = "activity"
    )
    y_train <- read.table(
        dataset["y_train.txt"],
        colClasses = "factor",
        col.names = "activity"
    )

    # Read the subject_test and subject_train data which contains information
    # about subject id
    subject_test <- read.table(
        dataset["subject_test.txt"],
        col.names = "subject"
    )
    subject_train <- read.table(
        dataset["subject_train.txt"],
        col.names = "subject"
    )

    # Create table with complete test data by combining the subject, activity
    # and measurements data
    data_test <- cbind(subject_test, y_test, X_test)

    # Create table with complete train data by combining the subject, activity
    # and measurements data
    data_train <- cbind(subject_train, y_train, X_train)

    # Combine the test and train data
    data_combined <- rbind(data_test, data_train)

    # Read activity labels
    activity_labels <- read.table(
        dataset["activity_labels.txt"],
        col.names = c("id", "name")
    )

    # Set the levels of activity variable to the activity labels to get
    # descriptive names instead of numbers
    levels(data_combined$activity) <- activity_labels$name

    data_combined
}

# summarise_means function groups data frame by given variables and summarises
# the data by creating average of each variable
summarise_means <- function(data, ...) {
    message("Creatting data summary")
    data %>%
        group_by(...) %>%
        summarise_all(mean)
}

# write_data_file function creates file with given data
write_data_file <- function(data, dest = DATASET_SUMMARY_FILE) {
    message("Writting data into ", dest, " file")
    write.table(data, file=dest, row.names = FALSE)
}

download_dataset()
data_combined <- combine_data_for_means_and_std()
data_combined_means_summary <- summarise_means(data_combined, subject, activity)
write_data_file(data_combined_means_summary)
