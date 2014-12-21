This file is regarding the variables, the data, and any transformations I have performed to clean up the data.  
* The place where the data was obtained:  
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones      
The data for the project:  
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
* The run_analysis.R script performs the following steps to clean the data:   
 1. Read X_train.txt, y_train.txt and subject_train.txt from the "./UCI HAR Dataset/train" folder and store them in *trainData*, *trainLabel* and *trainSubject* variables respectively.       
 2. Read X_test.txt, y_test.txt and subject_test.txt from the "./UCI HAR Dataset/test" folder and store them in *testData*, *testLabel* and *testsubject* variables respectively.  
 3. Concatenate *testData* to *trainData* to generate a 10299x561 data frame, *joinData*; concatenate *testLabel* to *trainLabel* to generate a 10299x1 data frame, *joinLabel*; concatenate *testSubject* to *trainSubject* to generate a 10299x1 data frame, *joinSubject*.  
 4. Read the features.txt file from the "/UCI HAR Dataset" folder and store the data in the variable *features*. To only extract the measurements on the mean and standard deviation, we get a subset of *joinData* with the 66 corresponding columns.  
 5. Clean the column names of the subset by removing the "()".   
 6. Read the activity_labels.txt file from the "./UCI HAR Dataset"" folder and store the data in the variable *activity*.  
derscore and capitalize the letter immediately after the underscore.  
 7. Transform the values of *joinLabel* with respect to the *activity* data frame.  
 8. Combine the *joinSubject*, *joinLabel* and *joinData* by column to get a cleaned 10299x68 data frame, *cleanedData*. Name the first two columns as "subject" and "activity". The "subject" column contains integers that range from 1 to 30; the "activity" column contains 6 kinds of activity names; the last 66 columns contain measurements that range from -1 to 1.  
 10. Write the *cleanedData* out to "merged_data.txt" file.  
 11. Last, generate a second tidy data set with the average of each measurement for each activity and each subject. Initialize the *result* data frame and we get a 180x68 data frame.
 12. Write the *result* out to "data_with_means.txt" file. 