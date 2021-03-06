# README.md

This Script starts with the assumption that the Samsung data is available in the working directory in an unzipped UCI CHAR Dataset folder.

The script includes some comments to facilitate you to understand it.

You need to have installed the library plyr on your machine.

First of all is the lecture of the files into data frames from giving files
- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

I took these files and put them into data frames tables.

############################################################
# 1 Create one data set merging train data and test data from: 
############################################################
- x_train from the file  'train/X_train.txt': Training set.
- y_train from the file  'train/y_train.txt'
- subject_train from the file  'train/subject_train.txt'
- x_test from the file 'test/X_test.txt'
- y_test from the file 'test/y_test.txt'
- subject_test from the file 'test/subject_test.txt'

I change de X for x to avoid errors on writting the names, joining rows to total tables, begining whith train and then test


x_total <- rbind(x_train, x_test)
y_total <- rbind(y_train, y_test)
subject_total <- rbind(subject_train, subject_test)

Then I paste in a new table both totals
table_total <- cbind(x_total, y_total)

For remove characteres like "-", "(" and ")" I used the gsub command, "-" was replaced by "_" and "(" and ")" by nothing. All was stored in a variable named features_clean to put as head (names)  for x_total data.

###################################################
# 2 select only the mean and the standard deviation
###################################################

I used the grep function and store the columns I need after in the variable only_mean_std.
I created a new table named measurements_mean_std that included the data solicitated.

##################################
# 3 use descriptive activity names
##################################

I replace the numbers in the data frame y_total by its equivalent in the activity_labels and put it description in the data frame y_total.
Later I rename the head (name) of the column with the word "activity".

##############################################
# 4 labels the data set with appropiated names
##############################################

I need to rename the head (name) of the colum in the subject_total with the word "subject".
Now I have all the data frames with its columns names so only need to paste in order in a new data frame that I called table_total

###########################################################
# 5 create a new data set of each activity and each subject
###########################################################

I used the function aggregate to extract the data solicitate with the subject and activity order by subject and then by activity.
After I put this new table in a file called "tidydataset.txt" 

#####
note: I have prepared another set of data frames to load all files from inertial signals only removing the "#"
#####



#####################
# About the Experiment:
#####################
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 


For more information about this dataset contact: activityrecognition@smartlab.ws
License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
