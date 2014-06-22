GettingandCleaningData
======================
    
# Project Course
    
* The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.
   
   
* The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

* The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

* Check the Codebook.md  file for further details about this dataset. 


   
## I create one R script called run_analysis.R that does the following: 

    1. Merges the training and the test sets to create one data set.
    2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    3. Uses descriptive activity names to name the activities in the data set
    4. Appropriately labels the data set with descriptive variable names. 
    5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


### Exactly

* Get libraries 
* EStablish paths 
* Get data 
* Loook for index on variables "mean" and "std" 
* Set descriptive names to data
* Set descriptive variable names   
* Print dimensions of data tables
* Create descriptive names for activity and type "test" or "train" 
* Select variables with "mean" and "std" and add "id"
* Join "test" and "train" in one tidy table
* Get means of features gruoped by activity and subject
* Write tidy data how txt file.

* Check the Run_analysis. R file for further details. 

