### Introduction

**run_analysis.R** contains a function named **run_analysis()** which tidies data from:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


Note:

  * outputs 2 tidy data set files in space-delimited *.txt format:

    ** one with means and standard deviations of various measures
    
    ** one with averages of the means and standard deviations

  * the function includes diagnostic prints so the user can
    track the progress of the tidying

  * Important Notes:

    ** it is assumed that the working directory includes a folder named
          **UCI HAR Dataset**
       that has the same the directory structure as
           https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

    ** it is assumed that the input files are in the working directory

    ** it is assumed that the following supplemental R packages are installed:
      plyr, dplyr



### Processing Flow

1. Preliminary set-up:
- activate supplemental R libraries

2. Input and reformat **features.txt**:
- this file has the names of the measures found in the Test and raining data sets
- only measures with **mean()** and **std()** are retained 
- names with **BodyBody** are excluded as the documentation provided with the data sets was not adequate to understand what these contained  
- an extra column is added that will be used to create column names in the tidy data sets

3. Input and reformat **activity_labels.txt**:
- this file maps the activity codes to activity names
- the activity names will be added to the tidy data sets
- the activities are changed to lower case and underscores are removed

4. & 5. Input and reformat **test/Y_test.txt** and **train/Y_train.txt**:
- these files contain the activity codes for each trial of the experiment
- the activity names are added via a merge()
- a temprary extra column is added to all resorting to the original order after the merge

6. & 7. Input and refomat **test/subject_test.txt** and **train/subject_train.txt**:
- these files contain the subject codes for each trial of the experiment (that is, it identifies the study participant associated with the given trial)
- the subject codes are needed for the second output file which requires averaging by subject code and activity

8. & 9. Input and reformat **test/X_test.txt** and **train/X_train.txt**:
- these contain the Test and Training data sets
- the files are combined with the inputs previously mentioned above in order to remove unneeded columns, add new column names and add subject codes

10. Concatenate the Test and Training data and output the result to a space-delimited txt file named **smartphone_data_tidy_1.txt**.

11. Take averages of all non-factor variables by subject code and activity and output the result to a space-delimited txt file named **smartphone_data_tidy_2.txt**.


