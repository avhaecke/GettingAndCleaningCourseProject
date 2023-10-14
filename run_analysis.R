library('dplyr')
library('stringr')

#
# Step 0 : read data
#

# Select mean and std deviation measurements
features_description = read.table(file = "./features.txt", col.names=c("index", "name"))
features_names_vec = features_description$name
columns_index_to_select = str_detect(features_names_vec, '-std\\(\\)|-mean\\(\\)')

# Extract names and index to keep
list_of_names_to_select = features_description$name[columns_index_to_select]
list_of_index_to_select = features_description$index[columns_index_to_select]

# id of participant in activity (different from test)
test_subject = read.table(file = "./test/subject_test.txt", col.names=c("subject_id"))

# The 561-feature vector with time and frequency domain variables (description in features.txt)
feature_vector_test = read.table(file = "./test/X_test.txt")

# id of participant in activity (different from test)
train_subject = read.table(file = "./train/subject_train.txt", col.names=c("subject_id"))

# The 561-feature vector with time and frequency domain variables (description in features.txt)
feature_vector_train = read.table(file = "./train/X_train.txt")

# Step 1 : 
# Merges the training and the test sets to create one data set
#

# Merge the measurements
test_subject = bind_rows(test_subject, train_subject)
feature_vector_test = bind_rows(feature_vector_test, feature_vector_train)

# The activity label (from 1 to 6 - description in activity_labels.txt)
test_activity = read.table(file = "./test/y_test.txt", col.names = c("activity"))
train_activity = read.table(file = "./train/y_train.txt", col.names = c("activity"))

# Merge the activity labels
test_activity = bind_rows(test_activity, train_activity)

#
# Step 2 :
# Extracts only the measurements on the mean and standard deviation for each measurement
#
feature_vector_test_mean_std = feature_vector_test %>% select(list_of_index_to_select)
colnames(feature_vector_test_mean_std) = list_of_names_to_select

# Read activity labels
activity_desc = read.table(file = "./activity_labels.txt", col.names = c("activity_index", "activity_name"))

# Complete activity with name
activity_name = activity_desc[test_activity$activity,]$activity_name
test_activity_with_name = test_activity %>% mutate(activity_name = activity_name)

# Measurement on the mean and standard deviation with a name for the activity
test_feature_activity = bind_cols(test_subject, test_activity_with_name, feature_vector_test_mean_std)

#
# Step 4 :
# Clean names to label the data set with descriptive variable names
#
features_names_vec = colnames(test_feature_activity)
features_names_vec2 = gsub("[(][)]","",features_names_vec,)
features_names_vec3 = gsub("[,-]","_",features_names_vec2,)
colnames(test_feature_activity) = features_names_vec3

first_tidy_dataset = test_feature_activity

#
# Step 5 :
# From the data set in step 4 namely test_feature_activity,
# This second data set is an independent tidy data set with the average of each variable for each activity and each subject
#

# Compute mean and create a 2d data set
activity_mean = test_feature_activity %>% mutate(activity_name_test_id = paste(activity_name, subject_id)) %>% group_by(activity_name_test_id) %>% select(!activity_name) %>% summarise(across(everything(),mean))
name_and_id = str_split_fixed(activity_mean$activity_name_test_id, ' ', 2)
activity_mean = activity_mean %>% mutate(activity_name = name_and_id[,1]) %>% select(!activity_name_test_id) %>% relocate(activity_name)

second_tidy_dataset = activity_mean

#
# Step 6 :
# write 2d data set
#
write.table(file = "./second_dataset.txt", activity_mean, row.name=FALSE)

