CodeBook
================

# Data

The dataset in this analysis contains acceleration and velocity data
collected from inertial sensors on a Samsung Galaxy S II smartphone.

Here is the dataset information, as described by the [UCI Machine
Learning Repository
website](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)
where the data was obtained:

The experiments have been carried out with a group of 30 volunteers
within an age bracket of 19-48 years. Each person performed six
activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING,
STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the
waist. Using its embedded accelerometer and gyroscope, we captured
3-axial linear acceleration and 3-axial angular velocity at a constant
rate of 50Hz. The experiments have been video-recorded to label the data
manually. The obtained dataset has been randomly partitioned into two
sets, where 70% of the volunteers was selected for generating the
training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by
applying noise filters and then sampled in fixed-width sliding windows
of 2.56 sec and 50% overlap (128 readings/window). The sensor
acceleration signal, which has gravitational and body motion components,
was separated using a Butterworth low-pass filter into body acceleration
and gravity. The gravitational force is assumed to have only low
frequency components, therefore a filter with 0.3 Hz cutoff frequency
was used. From each window, a vector of features was obtained by
calculating variables from the time and frequency domain.

# Variables

The following information was copied directly from the `features.txt`
file

The features selected for this database come from the accelerometer and
gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain
signals (prefix ‘t’ to denote time) were captured at a constant rate of
50 Hz. Then they were filtered using a median filter and a 3rd order low
pass Butterworth filter with a corner frequency of 20 Hz to remove
noise. Similarly, the acceleration signal was then separated into body
and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ)
using another low pass Butterworth filter with a corner frequency of 0.3
Hz.

Subsequently, the body linear acceleration and angular velocity were
derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and
tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional
signals were calculated using the Euclidean norm (tBodyAccMag,
tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these
signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ,
fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to
indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for
each pattern:  
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions.

  - tBodyAcc-XYZ
  - tGravityAcc-XYZ
  - tBodyAccJerk-XYZ
  - tBodyGyro-XYZ
  - tBodyGyroJerk-XYZ
  - tBodyAccMag
  - tGravityAccMag
  - tBodyAccJerkMag
  - tBodyGyroMag
  - tBodyGyroJerkMag
  - fBodyAcc-XYZ
  - fBodyAccJerk-XYZ
  - fBodyGyro-XYZ
  - fBodyAccMag
  - fBodyAccJerkMag
  - fBodyGyroMag
  - fBodyGyroJerkMag

The set of variables that were estimated from these signals are:

  - mean(): Mean value
  - std(): Standard deviation
  - mad(): Median absolute deviation
  - max(): Largest value in array
  - min(): Smallest value in array
  - sma(): Signal magnitude area
  - energy(): Energy measure. Sum of the squares divided by the number
    of values.
  - iqr(): Interquartile range
  - entropy(): Signal entropy
  - arCoeff(): Autorregresion coefficients with Burg order equal to 4
  - correlation(): correlation coefficient between two signals
  - maxInds(): index of the frequency component with largest magnitude
  - meanFreq(): Weighted average of the frequency components to obtain a
    mean frequency
  - skewness(): skewness of the frequency domain signal
  - kurtosis(): kurtosis of the frequency domain signal
  - bandsEnergy(): Energy of a frequency interval within the 64 bins of
    the FFT of each window.
  - angle(): Angle between to vectors.

Additional vectors obtained by averaging the signals in a signal window
sample. These are used on the angle() variable:

  - gravityMean
  - tBodyAccMean
  - tBodyAccJerkMean
  - tBodyGyroMean
  - tBodyGyroJerkMean

# Transformations

### Importing

Although not a transformation, the importing code chunk below is
included for completeness.

``` r
library(readr); library(plyr); library(magrittr); library(dplyr)
```

``` r
subject_test <- read.table("./test/subject_test.txt", col.names = "SubID")
subject_train <- read.table("./train/subject_train.txt", col.names = "SubID")

x_test <- read.table("./test/X_test.txt", header = F)
x_train <- read.table("./train/X_train.txt", header = F)

y_test <- read.table("./test/y_test.txt", col.names = "Condition")
y_train <- read.table("./train/y_train.txt", col.names = "Condition")
```

### Change Factor Levels

The factor levels were converted from numeric to descriptive character
values with the `mapvalues()` function.

``` r
# CHANGE FACTOR LEVELS FROM NUMERIC TO CHAR
y_test$Condition <- 
    mapvalues(x = y_test$Condition, 
              from = c(1, 2, 3, 4, 5, 6), 
              to = c("Walking", "Walking Upstairs", "Walking Downstairs",
                     "Sitting", "Standing", "Laying"))

y_train$Condition <- 
    mapvalues(x = y_train$Condition, 
              from = c(1, 2, 3, 4, 5, 6), 
              to = c("Walking", "Walking Upstairs", "Walkiing Downstairs",
                     "Sitting", "Standing", "Laying"))
```

### Features List Cleaning

The `features.txt` file was imported and cleaned so that the numbers
were separated from the characters and then deleted.

``` r
# IMPORT FEATURES LIST AND SEPARATE NUMERIC VALUES FROM CHAR VALUES
features <- read.delim("./features.txt", head = F, string = F)
features <- features %>% tidyr::separate(V1, into = c("n", "c"), sep = " ")
features <- (unname(unlist(features[,2])))

# MAP FEATURES TO COLUMN NAMES
colnames(x_test) <- features
colnames(x_train) <- features
```

### Merge Datasets

A `Type` column was added to the data so that, even after the data is
combined, it can be (re)split into `Test` and `Train` datasets (unneeded
objects were removed from the environment with the `rm()` function).

``` r
# ADD TEST/TRAIN COLUMN, COMBINE DATASETS
data_test <- cbind(subject_test, y_test, x_test)
data_test$Type <- "Test"
data_test <- data_test[c(1,564,2:563)]
rm(subject_test, y_test, x_test)

data_train <- cbind(subject_train, y_train, x_train)
data_train$Type <- "Train"
data_train <- data_train[c(1,564,2:563)]
rm(subject_train, y_train, x_train, features)

data <- rbind(data_train, data_test)
rm(data_train, data_test)
```

### Select Mean and Std Measurements

The dataset was filtered to only include columns that contain the
characters `mean()` or `std()`.

``` r
# SELECT ONLY THE MEAN() AND STD() COLUMNS 
data_mean_std <- 
data %>% select(SubID, Type, Condition, contains("mean()"), contains("std()"))
```

### Summarize Data

The data was grouped by `SubID` and `Condition`, and the mean was
computed for all columns.

``` r
# SUMMARISE DATA WHICH IS GROUPED BY SUBID AND CONDITION
data_summarized <- 
data_mean_std %>% 
    select(!Type) %>% 
    group_by(SubID, Condition) %>% 
    summarise_all(mean)

head(data_summarized)[1:5]
```

    ## # A tibble: 6 x 5
    ## # Groups:   SubID [1]
    ##   SubID Condition        `tBodyAcc-mean()-… `tBodyAcc-mean()-… `tBodyAcc-mean()…
    ##   <int> <chr>                         <dbl>              <dbl>             <dbl>
    ## 1     1 Laying                        0.222           -0.0405            -0.113 
    ## 2     1 Sitting                       0.261           -0.00131           -0.105 
    ## 3     1 Standing                      0.279           -0.0161            -0.111 
    ## 4     1 Walkiing Downst…              0.289           -0.00992           -0.108 
    ## 5     1 Walking                       0.277           -0.0174            -0.111 
    ## 6     1 Walking Upstairs              0.255           -0.0240            -0.0973
