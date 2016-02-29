
#
#   run_analysis()
#
#   - takes smartphone study data sets as input
#     the input files are expected to be in the working directory
#
#   - outputs 2 tidy data set files in space-delimited *.txt format:
#
#     > one with means and standard deviations of variaous measures
#     > one with averages of the means and standrd deviations
#
#   - the function includes diagnostic prints so the user can
#     track the progress of the tidying
#
#
#   - !!! Important Notes:
#
#     > it is assumed that the working directory includes a folder named
#           "UCI HAR Dataset"
#        that includes has the same the directory structure as
#            https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
#
#     > it is assumed that the input files are in the working directory
#
#     > it is assumed that the following supplemental R packages are installed:
#       plyr, dplyr
# 
run_analysis <- function() {
        
        # ============================================================
        #
        # preliminary set-up
        #
        # ============================================================
        
        #
        # clear out working directory
        #
        #rm(list=c("activitynames",
        #          "alldata",
        #          "alldataavg",
        #          "features",
        #          "featureskeepcolumns",
        #          "featureskeepnames",
        #          "test",
        #          "testactivities",
        #          "testactivitiesplus",
        #          "testsubjects",
        #          "train",
        #          "trainactivities",
        #          "trainactivitiesplus",
        #          "trainsubjects"))
        #     > as a precaution this starts with a houskeeping step that cleans out objects
        #       from the working directory
        
        #
        # activate packages that will be needed
        #
        library(dplyr)
        library(plyr)
        
        
        #
        # print inital messages
        #
        print("INITIATING DATA TIDYING...", quote=FALSE);
        print(" ", quote=FALSE);
        print(" ", quote=FALSE)
         
        
        # ============================================================
        #
        # input feature name file, select the measures of interest & add new names 
        # for the meaures
        #
        # ============================================================
        
        #
        # input features file
        #
        features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE, sep = "", fill=TRUE, col.names=c("featurecode", "featurename") ) 
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : features', quote=FALSE );
        print( c('CLASS        : ', class(features)), quote=FALSE );
        print( c('ROWS         : ', nrow(features)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(features)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(features))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # preliminary trim to capture columns of interest
        #
        features <- features[grepl("std[[:punct:]]|mean[[:punct:]]", features$featurename), ]
        
        #
        # drop undocumented BodyBody columns 
        # and
        # make a vector of the columns to keep
        #
        features <- features[grep("BodyBody", features$featurename, invert=TRUE),]
        
        #
        # copy the feature names to a new column column
        #
        features <- cbind(features, featurenamenew=features$featurename)
        
        #
        # change the new column names
        #
        features$featurenamenew <- gsub("-", "", features$featurenamenew);
        features$featurenamenew <- sub("BodyAcc", "BodyAcceleration", features$featurenamenew);
        features$featurenamenew <- sub("GravityAcc", "GravitationalAcceleration", features$featurenamenew);
        features$featurenamenew <- sub("^t", "", features$featurenamenew);
        features$featurenamenew <- sub("mean", "Mean", features$featurenamenew);
        features$featurenamenew <- sub("std", "Std", features$featurenamenew);
        features$featurenamenew <- sub("MagMean", "MeanN", features$featurenamenew);
        features$featurenamenew <- sub("MagStd", "StdN", features$featurenamenew);
        features$featurenamenew <- sub("Gyro", "Angularvelocity", features$featurenamenew);
        features$featurenamenew <- sub("^f", "Transformed", features$featurenamenew);
        features$featurenamenew <- gsub("[[:punct:]]", "", features$featurenamenew)
        
        #
        # make a vector of the columns positions of the columns to keep
        #
        featureskeepcolumns <- features[, 1]
        
        #
        # make a vector of the names of the columns to keep
        #
        featureskeepnames <- features[, "featurenamenew"]
        
        #
        # convert to character values
        #
        featureskeepnames <- sub("", "", featureskeepnames)
       
        #
        # print message
        #
        print('FEATURE NAME PRE-PROCESSING COMPLETE w/generated vectors:', quote=FALSE);
        print('featureskeepcolumns', quote=FALSE);
        print('featureskeepnames', quote=FALSE);
        print(' ', quote=FALSE)
        

        # ============================================================
        #
        # input activity names file and reformat values
        #
        # ============================================================
        
        #
        # read in Activity Labels
        #
        activitynames <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "", fill=TRUE, col.names = c('activitycode', 'activityname') )
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : activitynames', quote=FALSE );
        print( c('CLASS        : ', class(activitynames)), quote=FALSE );
        print( c('ROWS         : ', nrow(activitynames)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(activitynames)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(activitynames))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # change activity names to lower case & remove "_"
        #
        activitynames[,2] <- sub("_", " ", tolower(activitynames[,2])) 
        
        
        # ============================================================
        #
        # input and reformat TEST activities file
        #
        # ============================================================
        
        #
        # read in the list of test activities
        #
        testactivities <- read.table(nrows=-1, "./UCI HAR Dataset/test/Y_test.txt", header = FALSE, sep = "", fill=TRUE, col.names="activitycode")
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : testactivities', quote=FALSE );
        print( c('CLASS        : ', class(testactivities)), quote=FALSE );
        print( c('ROWS         : ', nrow(testactivities)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(testactivities)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(testactivities))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # add test_id column = for use in re-sorting
        #
        testactivities <- cbind(testid=c( 1:nrow(testactivities) ), testactivities)
        
        #
        # merge the activity file and the activity name file,
        #
        # keeping all entries from the activity file
        #
        testactivitiesplus <- merge(testactivities, activitynames, by.x = "activitycode", by.y = "activitycode", all.x=TRUE)
        
        #
        # re-sort to restore original ordering
        #
        testactivitiesplus <- testactivitiesplus[order(as.numeric(testactivitiesplus$testid)), ]
        
        #
        # print message
        #
        print('ACTIVITY NAME PRE-PROCESSING COMPLETE w/generated vector:', quote=FALSE);
        print('testactivitiesplus ', quote=FALSE);
        print(' ', quote=FALSE)
        
        
        # ============================================================
        #
        # input and reformat the TRAINing activities file
        #
        # ============================================================
        
        #
        # read in the list of train activities
        #
        trainactivities <- read.table(nrows=-1, "./UCI HAR Dataset/train/Y_train.txt", header = FALSE, sep = "", fill=TRUE, col.names="activitycode")
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : trainactivities', quote=FALSE );
        print( c('CLASS        : ', class(trainactivities)), quote=FALSE );
        print( c('ROWS         : ', nrow(trainactivities)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(trainactivities)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(trainactivities))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # add train_id column = for use in re-sorting
        #
        trainactivities <- cbind(trainid=c( 1:nrow(trainactivities) ), trainactivities)
        
        #
        # merge the activity file and the activity name file,
        #
        # keeping all entries from the activity file
        #
        trainactivitiesplus <- merge(trainactivities, activitynames, by.x = "activitycode", by.y = "activitycode", all.x=TRUE)
        
        #
        # re-sort to restore original ordering
        #
        trainactivitiesplus <- trainactivitiesplus[order(as.numeric(trainactivitiesplus$trainid)), ]

        #
        # print message
        #
        print('ACTIVITY NAME PRE-PROCESSING COMPLETE w/generated vector:', quote=FALSE);
        print('trainactivitiesplus ', quote=FALSE);
        print(' ', quote=FALSE)
        
        
        # ============================================================
        #
        # input TEST subject code file & reformat
        #
        # ============================================================
        
        #
        # input subject ID file
        #
        testsubjects <- read.table(nrows=-1, "./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "", fill=TRUE, col.names="subjectcode")
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : testsubjects', quote=FALSE );
        print( c('CLASS        : ', class(testsubjects)), quote=FALSE );
        print( c('ROWS         : ', nrow(testsubjects)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(testsubjects)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(testsubjects))), quote=FALSE );
        print(" ", quote=FALSE)

        #
        # add test_id column = for use in re-sorting after grouping
        #
        #testsubjects <- cbind(testid=c( 1:nrow(testsubjects) ), testsubjects)
        
        #
        # create groups for each subject
        #
        #testsubjects <- group_by(testsubjects, subjectcode)
        
        #
        # re-sort
        #
        #testsubjects <- testsubjects[order(as.numeric(testsubjects$testid)), ]
        
        #
        # add trial number within each subject
        #
        #testsubjects <- cbind(subjectcode=testsubjects[,2],
        #        ddply(testsubjects, "subjectcode", transform, subjecttrialnumber=1:length(subjectcode) ) )

        #
        # remove extra columns 
        #
        #testsubjects <- testsubjects[,c(1,4)]
        
        #
        # rename trial number column
        #
        #names(testsubjects[2]) <- c("subjecttrialnumber")

        
        # ============================================================
        #
        # input TRAINing subject code file
        #
        # ============================================================
        
        #
        # input subject ID file
        #
        trainsubjects <- read.table(nrows=-1, "./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "", fill=TRUE, col.names="subjectcode")
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : trainsubjects', quote=FALSE );
        print( c('CLASS        : ', class(trainsubjects)), quote=FALSE );
        print( c('ROWS         : ', nrow(trainsubjects)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(trainsubjects)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(trainsubjects))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # add test_id column = for use in re-sorting after grouping
        #
        #trainsubjects <- cbind(testid=c( 1:nrow(trainsubjects) ), trainsubjects)
        
        #
        # create groups for each subject
        #
        #trainsubjects <- group_by(trainsubjects, subjectcode)
        
        #
        # add trial number within each subject
        #
        #trainsubjects <- cbind(subjectcode=trainsubjects[,2],
        #                       ddply(trainsubjects, "subjectcode", transform, subjecttrialnumber=1:length(subjectcode) ) )
        
        #
        # remove extra columns 
        #
        #trainsubjects <- trainsubjects[,c(1,4)]
        
        #
        # rename trial number column
        #
        #names(trainsubjects[2]) <- c("subjecttrialnumber")

#print(trainsubjects)  
        
        
        # ============================================================
        #
        # input TEST data file
        #
        # ============================================================
        
        #
        # input test file
        #
        test <- read.table(nrows=-1, "./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "", fill=TRUE)
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : test', quote=FALSE );
        print( c('CLASS        : ', class(test)), quote=FALSE );
        print( c('ROWS         : ', nrow(test)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(test)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(test))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # select only the columns of interest
        #
        test <- test[,c(featureskeepcolumns)]
        
        #
        # rename the columns
        #
        names(test) <- c(featureskeepnames)

        #
        # add the activity name & the subject code
        #
        #test <- cbind(subjectcode=testsubjects$subjectcode, 
        #             subjecttrialnumber=testsubjects$subjecttrialnumber, 
        #             activity=testactivitiesplus$activityname, test)
        
        
        #
        # add the activity name & the subject code
        #
        test <- cbind(subjectcode=testsubjects$subjectcode,
                     activity=testactivitiesplus$activityname, test)
        
        #
        # print message
        #
        print("test DATA FRAME HAS BEEN REFORMATED", quote=FALSE)
        print(" ", quote=FALSE)


        # ============================================================
        #
        # input TRAINing data file
        #
        # ============================================================
        
        #
        # input train file
        #
        train <- read.table(nrows=-1, "./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "", fill=TRUE)
        
        #
        # print diagnostics
        #
        print( 'INPUT NAME   : train', quote=FALSE );
        print( c('CLASS        : ', class(train)), quote=FALSE );
        print( c('ROWS         : ', nrow(train)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(train)), quote=FALSE );
        print( c('COUNT OF NAs : ', sum(is.na(train))), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # select only the columns of interest
        #
        train <- train[,c(featureskeepcolumns)]
        
        #
        # rename the columns
        #
        names(train) <- c(featureskeepnames)
        
        #
        # add the activity name  & the subject code
        #
        #train <- cbind(subjectcode=trainsubjects$subjectcode, 
        #               subjecttrialnumber=testsubjects$subjecttrialnumber, 
        #               activity=trainactivitiesplus$activityname, train)
 
        #
        # add the activity name
        #
        train <- cbind(subjectcode=trainsubjects$subjectcode, 
                       activity=trainactivitiesplus$activityname, train)

        #
        # print message
        #
        print("train DATA FRAME HAS BEEN REFORMATED", quote=FALSE)
        print(' ', quote=FALSE)    

        
        # ============================================================
        #
        # bind the TEST and TRAINing data & perfom housekeeping
        #
        # ============================================================
        
        #
        # concatenate the TEST and TRAIN data
        #
        alldata <- rbind(train, test)

        #
        # print diagnostics
        #
        print('train AND test DATA HAVE BEEN CONCATENATED:', quote=FALSE );
        print(   'NAME         : alldata', quote=FALSE );
        print( c('CLASS        : ', class(alldata)), quote=FALSE );
        print( c('ROWS         : ', nrow(alldata)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(alldata)), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # housekeeping
        #
        rm(test, train)

        #
        # write data to permanent file
        #
        #write.table(alldata, file="./smartphone_data_tidy_1.csv", sep=",", 
        #           row.names = FALSE, col.names = TRUE, dec = ".")
        write.table(alldata, file="./smartphone_data_tidy_1.txt", 
                    row.names = FALSE, col.names = TRUE, dec = ".")

        #
        # print message
        #
        print("TIDY FILE 1 HAS BEEN OUTPUT:", quote=FALSE)
        print(c("DIRECTORY:", getwd() ), quote=FALSE)
        #print(  "FILE     : smartphone_data_tidy_1.csv", quote=FALSE)
        print(  "FILE     : smartphone_data_tidy_1.csv", quote=FALSE)
        print(' ', quote=FALSE)  
       
#print( alldata[,c(1,2)] )
       
         
        # ============================================================
        #
        # create the second tidy file
        #
        # ============================================================
        
        #
        # get avgs by subjectcode and activity
        #
        alldataavg <- unique(ddply(alldata, c("subjectcode", "activity" ), summarize, 
                   avgTransformedBodyAngularvelocityStdZ=ave(TransformedBodyAngularvelocityStdZ), 
                   avgTransformedBodyAngularvelocityStdY=ave(TransformedBodyAngularvelocityStdY), 
                   avgTransformedBodyAngularvelocityStdX=ave(TransformedBodyAngularvelocityStdX), 
                   avgTransformedBodyAngularvelocityMeanZ=ave(TransformedBodyAngularvelocityMeanZ), 
                   avgTransformedBodyAngularvelocityMeanY=ave(TransformedBodyAngularvelocityMeanY), 
                   avgTransformedBodyAngularvelocityMeanX=ave(TransformedBodyAngularvelocityMeanX), 
                   avgTransformedBodyAccelerationStdZ=ave(TransformedBodyAccelerationStdZ), 
                   avgTransformedBodyAccelerationStdY=ave(TransformedBodyAccelerationStdY), 
                   avgTransformedBodyAccelerationStdX=ave(TransformedBodyAccelerationStdX), 
                   avgTransformedBodyAccelerationStdN=ave(TransformedBodyAccelerationStdN), 
                   avgTransformedBodyAccelerationMeanZ=ave(TransformedBodyAccelerationMeanZ), 
                   avgTransformedBodyAccelerationMeanY=ave(TransformedBodyAccelerationMeanY), 
                   avgTransformedBodyAccelerationMeanX=ave(TransformedBodyAccelerationMeanX), 
                   avgTransformedBodyAccelerationMeanN=ave(TransformedBodyAccelerationMeanN), 
                   avgTransformedBodyAccelerationJerkStdZ=ave(TransformedBodyAccelerationJerkStdZ), 
                   avgTransformedBodyAccelerationJerkStdY=ave(TransformedBodyAccelerationJerkStdY), 
                   avgTransformedBodyAccelerationJerkStdX=ave(TransformedBodyAccelerationJerkStdX), 
                   avgTransformedBodyAccelerationJerkMeanZ=ave(TransformedBodyAccelerationJerkMeanZ), 
                   avgTransformedBodyAccelerationJerkMeanY=ave(TransformedBodyAccelerationJerkMeanY), 
                   avgTransformedBodyAccelerationJerkMeanX=ave(TransformedBodyAccelerationJerkMeanX), 
                   avgGravitationalAccelerationStdZ=ave(GravitationalAccelerationStdZ), 
                   avgGravitationalAccelerationStdY=ave(GravitationalAccelerationStdY), 
                   avgGravitationalAccelerationStdX=ave(GravitationalAccelerationStdX), 
                   avgGravitationalAccelerationStdN=ave(GravitationalAccelerationStdN), 
                   avgGravitationalAccelerationMeanZ=ave(GravitationalAccelerationMeanZ), 
                   avgGravitationalAccelerationMeanY=ave(GravitationalAccelerationMeanY), 
                   avgGravitationalAccelerationMeanX=ave(GravitationalAccelerationMeanX), 
                   avgGravitationalAccelerationMeanN=ave(GravitationalAccelerationMeanN), 
                   avgBodyAngularvelocityStdZ=ave(BodyAngularvelocityStdZ), 
                   avgBodyAngularvelocityStdY=ave(BodyAngularvelocityStdY), 
                   avgBodyAngularvelocityStdX=ave(BodyAngularvelocityStdX), 
                   avgBodyAngularvelocityStdN=ave(BodyAngularvelocityStdN), 
                   avgBodyAngularvelocityMeanZ=ave(BodyAngularvelocityMeanZ), 
                   avgBodyAngularvelocityMeanY=ave(BodyAngularvelocityMeanY), 
                   avgBodyAngularvelocityMeanX=ave(BodyAngularvelocityMeanX), 
                   avgBodyAngularvelocityMeanN=ave(BodyAngularvelocityMeanN), 
                   avgBodyAngularvelocityJerkStdZ=ave(BodyAngularvelocityJerkStdZ), 
                   avgBodyAngularvelocityJerkStdY=ave(BodyAngularvelocityJerkStdY), 
                   avgBodyAngularvelocityJerkStdX=ave(BodyAngularvelocityJerkStdX), 
                   avgBodyAngularvelocityJerkStdN=ave(BodyAngularvelocityJerkStdN), 
                   avgBodyAngularvelocityJerkMeanZ=ave(BodyAngularvelocityJerkMeanZ), 
                   avgBodyAngularvelocityJerkMeanY=ave(BodyAngularvelocityJerkMeanY), 
                   avgBodyAngularvelocityJerkMeanX=ave(BodyAngularvelocityJerkMeanX), 
                   avgBodyAngularvelocityJerkMeanN=ave(BodyAngularvelocityJerkMeanN), 
                   avgBodyAccelerationStdZ=ave(BodyAccelerationStdZ), 
                   avgBodyAccelerationStdY=ave(BodyAccelerationStdY), 
                   avgBodyAccelerationStdX=ave(BodyAccelerationStdX), 
                   avgBodyAccelerationStdN=ave(BodyAccelerationStdN), 
                   avgBodyAccelerationMeanZ=ave(BodyAccelerationMeanZ), 
                   avgBodyAccelerationMeanY=ave(BodyAccelerationMeanY), 
                   avgBodyAccelerationMeanX=ave(BodyAccelerationMeanX), 
                   avgBodyAccelerationMeanN=ave(BodyAccelerationMeanN), 
                   avgBodyAccelerationJerkStdZ=ave(BodyAccelerationJerkStdZ), 
                   avgBodyAccelerationJerkStdY=ave(BodyAccelerationJerkStdY), 
                   avgBodyAccelerationJerkStdX=ave(BodyAccelerationJerkStdX), 
                   avgBodyAccelerationJerkStdN=ave(BodyAccelerationJerkStdN), 
                   avgBodyAccelerationJerkMeanZ=ave(BodyAccelerationJerkMeanZ), 
                   avgBodyAccelerationJerkMeanY=ave(BodyAccelerationJerkMeanY), 
                   avgBodyAccelerationJerkMeanX=ave(BodyAccelerationJerkMeanX), 
                   avgBodyAccelerationJerkMeanN=ave(BodyAccelerationJerkMeanN)))
 
        #
        # print diagnostics
        #
        print('AVERAGES OF COMBINED DATA HAVE BEEN CALCULATED:', quote=FALSE );
        print(   'NAME         : alldataavg', quote=FALSE );
        print( c('CLASS        : ', class(alldataavg)), quote=FALSE );
        print( c('ROWS         : ', nrow(alldataavg)), quote=FALSE );
        print( c('COLUMNS      : ', ncol(alldataavg)), quote=FALSE );
        print(" ", quote=FALSE)
        
        #
        # write data to permanent file
        #
        #write.table(alldataavg, file="./smartphone_data_tidy_2.csv", sep=",", 
        #            row.names = FALSE, col.names = TRUE, dec = ".")
        write.table(alldataavg, file="./smartphone_data_tidy_2.txt", 
                    row.names = FALSE, col.names = TRUE, dec = ".")
        
        #
        # print message
        #
        print("TIDY FILE 2 HAS BEEN OUTPUT:", quote=FALSE)
        print(c("DIRECTORY:", getwd() ), quote=FALSE)
        print(  "FILE     : smartphone_data_tidy_2.txt", quote=FALSE)
        #print(  "FILE     : smartphone_data_tidy_2.csv", quote=FALSE)
        print(' ', quote=FALSE)  
        
        
        # ============================================================
        #
        # end run
        #
        # ============================================================
        
        #
        # print message
        #
        print("...THE TIDYING IS COMPLETE.", quote=FALSE)       
                
}

