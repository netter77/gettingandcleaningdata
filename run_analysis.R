{
  # Fichero run_analysis.R creado por Ignacio Gonzálvez para Getting and Cleaning Data
  #  igonzalvez@gmail.com
  #
  # Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
  #  This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
  # Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
  
  
  library(plyr)
  
# Reading files into tables

features = read.table("./features.txt") # List of all features.
#
activity_labels=read.table("./activity_labels.txt") # Links the class labels with their activity name.

####################
### Files from TRAIN
####################

x_train = read.table("./train/X_train.txt")  # Train set.
y_train = read.table("./train/y_train.txt")  # Train labels.
subject_train = read.table("./train/subject_train.txt") # Each row identifies the subject who performed the activity for each window sample. 
                                                        # Its range is from 1 to 30. 

###################
### Files from TEST
###################

x_test = read.table("./test/X_test.txt") # Test set.
y_test = read.table("./test/y_test.txt") # Test labels.
subject_test = read.table("./test/subject_test.txt") # Each row identifies the subject who performed the activity for each window sample. 
                                                    # Its range is from 1 to 30. 



# end of reading files


########################################################
# 1 Create one data set merging train data and test data
########################################################




 

# join rows to the table, begining whith train and then test


x_total <- rbind(x_train, x_test)
y_total <- rbind(y_train, y_test)
subject_total <- rbind(subject_train, subject_test)
table_total <- cbind(x_total, y_total)


# remove invalid names caracter
  cadena <- features$V2
  cadena_clean1 <- gsub("\\(","", cadena)
  cadena_clean2 <- gsub("\\)","", cadena_clean1)
  cadena_clean <- gsub("-","_", cadena_clean2)

features_clean <- cadena_clean # list of all features without caracteres "-","(" and ")"
                                             # need to add the name of the subject column, not listed in features

names(x_total) <- features_clean
##
###################################################
# 2 select only the mean and the standard deviation
###################################################


only_mean_std <- grep("-(mean|std)\\(\\)", features[, 2])

measurements_mean_std <- x_total[, only_mean_std]


##################################
# 3 use descriptive activity names
##################################

y_total[,1] <- activity_labels[y_total[, 1], 2] #using data frame y_total to show data in data frame activity_labels

names(y_total) <- c("activity")


##############################################
# 4 labels the data set with appropiated names
##############################################

##
# rename the column name of subject_total
names(subject_total) <- "subject"

# join columns to a table called total_table
table_total <- cbind(x_total, y_total, subject_total)

###########################################################
# 5 create a new data set of each activity and each subject
###########################################################

new_table <- aggregate( . ~subject + activity, data=table_total, FUN=mean)
new_table <- new_table[order(new_table$subject, new_table$activity),]


write.table(new_table, file = "./tidydataset.txt",row.name=FALSE)



#
# If you want, remove # to load Files from inertial signals
#total_acc_x_train = read.table("./train/Inertial Signals/total_acc_x_train.txt") # The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
# Every row shows a 128 element vector. 

#body_acc_x_train = read.table("./train/Inertial Signals/body_acc_x_train.txt")   # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#body_gyro_x_train = read.table("./train/Inertial Signals/body_gyro_x_train.txt") # The angular velocity vector measured by the gyroscope for each window sample. 
# The units are radians/second. 
#
#
#total_acc_y_train = read.table("./train/Inertial Signals/total_acc_y_train.txt") # The acceleration signal from the smartphone accelerometer Y axis in standard gravity units 'g'. 
# Every row shows a 128 element vector.
#body_acc_y_train = read.table("./train/Inertial Signals/body_acc_y_train.txt")   # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#body_gyro_y_train = read.table("./train/Inertial Signals/body_gyro_y_train.txt") # The angular velocity vector measured by the gyroscope for each window sample. 
# The units are radians/second. 
#
#
#total_acc_z_train = read.table("./train/Inertial Signals/total_acc_z_train.txt") # The acceleration signal from the smartphone accelerometer Z axis in standard gravity units 'g'. 
# Every row shows a 128 element vector.
#body_acc_z_train = read.table("./train/Inertial Signals/body_acc_z_train.txt")   # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#body_gyro_z_train = read.table("./train/Inertial Signals/body_gyro_z_train.txt") # The angular velocity vector measured by the gyroscope for each window sample. 
# If you want, remove # to load Files from inertial signals 
#total_acc_x_test = read.table("./test/Inertial Signals/total_acc_x_test.txt") # The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. 
# Every row shows a 128 element vector. 

#body_acc_x_test = read.table("./test/Inertial Signals/body_acc_x_test.txt")   # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#body_gyro_x_test = read.table("./test/Inertial Signals/body_gyro_x_test.txt") # The angular velocity vector measured by the gyroscope for each window sample. 
# The units are radians/second. 
#
#
#total_acc_y_test = read.table("./test/Inertial Signals/total_acc_y_test.txt") # The acceleration signal from the smartphone accelerometer Y axis in standard gravity units 'g'. 
# Every row shows a 128 element vector.
#body_acc_y_test = read.table("./test/Inertial Signals/body_acc_y_test.txt")   # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#body_gyro_y_test = read.table("./test/Inertial Signals/body_gyro_y_test.txt") # The angular velocity vector measured by the gyroscope for each window sample. 
# The units are radians/second. 
#
#
#total_acc_z_test = read.table("./test/Inertial Signals/total_acc_z_test.txt") # The acceleration signal from the smartphone accelerometer Z axis in standard gravity units 'g'. 
# Every row shows a 128 element vector.
#body_acc_z_test = read.table("./test/Inertial Signals/body_acc_z_test.txt")   # The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
#body_gyro_z_test = read.table("./test/Inertial Signals/body_gyro_z_test.txt") # The angular velocity vector measured by the gyroscope for each window sample. 
# The units are radians/second.
# if you want to merge all the tables with inertial signals remove the #
#x_train_total_big <- cbind(subject_train,  x_train, total_acc_x_train, body_acc_x_train, body_gyro_x_train, total_acc_y_train, body_acc_y_train, body_gyro_y_train, total_acc_z_train, body_acc_z_train, body_gyro_z_train)
#x_test_total_big  <- cbind(subject_test,    x_test, total_acc_x_test,  body_acc_x_test,  body_gyro_x_test,  total_acc_y_test,  body_acc_y_test,  body_gyro_y_test,  total_acc_z_test,  body_acc_z_test,  body_gyro_z_test)
#x_total_big <- rbind(x_train_total_big, x_test_total_big)
#y_total_big <- rbind(y_train, y_test)
#table_total_big <- cbind(x_total_big, y_total_big)
#names(table_total_big) <- c(1:1715)


}
