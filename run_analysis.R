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

dataset_exist <- function(files = DATASET_FILES, dir = DATA_DIR) {
    file.exists(dir) & all(basename(files) %in% dir(dir))
}

download_dataset <- function(dataset_url = DATASET_URL, dest_dir = DATA_DIR) {
    temp <- tempfile()
    if (!dataset_exist()) {
        dir.create(dest_dir)
        message("Downloading UCI HAR Dataset")
        download.file(dataset_url, destfile = temp, mode = "wb")
        unzip(temp, overwrite = TRUE, junkpaths = TRUE, exdir = DATA_DIR)
        unlink(temp)
    } else {
        message("UCI HAR Dataset found in data directory")
    }
}

download_dataset()
