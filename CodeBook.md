
Feature Description 
===================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

Finally 2 set of variables were estimated from these signals. They are :

mean : Mean value
std : Standard deviation

The complete list of features are as follows :
tBodyAcc_mean_X          
tBodyAcc_mean_Y           
tBodyAcc_mean_Z           
tBodyAcc_std_X            
tBodyAcc_std_Y           
tBodyAcc_std_Z            
tGravityAcc_mean_X        
tGravityAcc_mean_Y        
tGravityAcc_mean_Z       
tGravityAcc_std_X         
tGravityAcc_std_Y         
tGravityAcc_std_Z         
tBodyAccJerk_mean_X      
tBodyAccJerk_mean_Y       
tBodyAccJerk_mean_Z       
tBodyAccJerk_std_X        
tBodyAccJerk_std_Y       
tBodyAccJerk_std_Z        
tBodyGyro_mean_X          
tBodyGyro_mean_Y          
tBodyGyro_mean_Z         
tBodyGyro_std_X           
tBodyGyro_std_Y           
tBodyGyro_std_Z           
tBodyGyroJerk_mean_X     
tBodyGyroJerk_mean_Y      
tBodyGyroJerk_mean_Z      
tBodyGyroJerk_std_X       
tBodyGyroJerk_std_Y      
tBodyGyroJerk_std_Z       
tBodyAccMag_mean          
tBodyAccMag_std           
tGravityAccMag_mean      
tGravityAccMag_std        
tBodyAccJerkMag_mean      
tBodyAccJerkMag_std       
tBodyGyroMag_mean        
tBodyGyroMag_std          
tBodyGyroJerkMag_mean     
tBodyGyroJerkMag_std      
fBodyAcc_mean_X          
fBodyAcc_mean_Y           
fBodyAcc_mean_Z           
fBodyAcc_std_X            
fBodyAcc_std_Y           
fBodyAcc_std_Z            
fBodyAccJerk_mean_X       
fBodyAccJerk_mean_Y       
fBodyAccJerk_mean_Z      
fBodyAccJerk_std_X        
fBodyAccJerk_std_Y        
fBodyAccJerk_std_Z        
fBodyGyro_mean_X         
fBodyGyro_mean_Y          
fBodyGyro_mean_Z          
fBodyGyro_std_X           
fBodyGyro_std_Y          
fBodyGyro_std_Z           
fBodyAccMag_mean          
fBodyAccMag_std           
fBodyBodyAccJerkMag_mean 
fBodyBodyAccJerkMag_std   
fBodyBodyGyroMag_mean     
fBodyBodyGyroMag_std      
fBodyBodyGyroJerkMag_mean
fBodyBodyGyroJerkMag_std
