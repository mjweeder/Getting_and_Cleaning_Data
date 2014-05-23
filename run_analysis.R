
# Getting and Cleaning Class Project
# Author: Ron Collins
#

#input text files all ready unzipped
X_test <- read.table("C:/Users/Ron Collins/Documents/Cousara/Getting and Cleaning Data/Class Project/X_test.txt",header=F,sep="",quote="")

y_test <- read.table("C:/Users/Ron Collins/Documents/Cousara/Getting and Cleaning Data/Class Project/y_test.txt",header=F,sep="",quote="")

X_train <- read.table("C:/Users/Ron Collins/Documents/Cousara/Getting and Cleaning Data/Class Project/X_train.txt",header=F,sep="",quote="")

y_train <- read.table("C:/Users/Ron Collins/Documents/Cousara/Getting and Cleaning Data/Class Project/y_train.txt",header=F,sep="",quote="")

subject_train <- read.table("~/Cousara/Getting and Cleaning Data/Class Project/subject_train.txt", quote="\"")

subject_test <- read.table("~/Cousara/Getting and Cleaning Data/Class Project/subject_test.txt", quote="\"")

features <- read.table("~/Cousara/Getting and Cleaning Data/Class Project/features.txt", quote="\"")

activity_labels <- read.table("~/Cousara/Getting and Cleaning Data/Class Project/activity_labels.txt", quote="\"")

#Add column name to dataframe X_test and X_train
dataframe_col_names <- features[,2]

names(X_test)<-dataframe_col_names #add column names to X_test

names(X_train)<-dataframe_col_names #add column names to X_test

# Add activity column to dataFrames
X_test <- cbind(y_test,X_test)
names(X_test)[1] = "activity"

X_train <- cbind(y_train,X_train)
names(X_train)[1] = "activity"


#add subject lable colume to dataframe X_test and X train
X_test <- cbind(subject_test,X_test)
X_train <- cbind(subject_train,X_train)

names(X_test)[1] = "subject" # change first col name to subject
names(X_train)[1] = "subject"  # change first col name to subject

#create vector for colums in dataframe with mean or std values
mean_vector_1 <- c(1,2,3, 4, 5,6,7,8,43,44,45,46,47,48,83,84,85,86,87,88,123,124,125,126,127,128)
mean_vector_2 <- c(163,164,165,166,167,168,203,204,216,217,229,230,242,243,255,256)
mean_vector_3 <- c(268,269,270,271,272,273,347,348,349,350,351,352,426,427,428,429,430,431)
mean_vector_4 <- c(505,506,531,532,544,545)


mean_vector <- c(mean_vector_1, mean_vector_2, mean_vector_3, mean_vector_4)

#combine X_test and X_train dataframes
X_comb <- rbind(X_test, X_train)

# remove dataframes not needed
rm(X_test, X_train, subject_test, subject_Train)

# order dataframe
X_comb <- X_comb[ order(X_comb[,1], X_comb[,2]), ]

#remove all columns not contain mean or standard deviation
X_comb_mean <- X_comb[,mean_vector]

# change modes
activity_labels[,2] <- as.character(activity_labels[,2])
X_comb_mean[,2] <- as.character(X_comb_mean[,2])

for( i in 1:length(X_comb_mean[,1])){
    if(X_comb_mean[i,2] == activity_labels[1,1] ) X_comb_mean[i,2] <- activity_labels[1,2]
    
    if(X_comb_mean[i,2] == activity_labels[2,1] ) X_comb_mean[i,2] <- activity_labels[2,2]
    
    if(X_comb_mean[i,2] == activity_labels[3,1] ) X_comb_mean[i,2] <- activity_labels[3,2]
    
    if(X_comb_mean[i,2] == activity_labels[4,1] ) X_comb_mean[i,2] <- activity_labels[4,2]
    
    if(X_comb_mean[i,2] == activity_labels[5,1] ) X_comb_mean[i,2] <- activity_labels[5,2]
    
    if(X_comb_mean[i,2] == activity_labels[6,1] ) X_comb_mean[i,2] <- activity_labels[6,2]    
}

# write cvs file
write.csv(X_comb_mean, file = "MyData.csv")


############################################################################33
############################# Create second tidy data set

meanTidyData <- matrix(,nrow = 180, ncol = 66)

k <- 1

#  The mean is calcualed for each of the columns for each activity for each subject in a vector
# without the subject or activity
for (i in 1:30){
    for( j in 1:6){
        selectionVector <- X_comb_mean[,1] == i & X_comb_mean[,2]== activity_labels[j,2]
        meanData <- X_comb_mean[selectionVector,]
        colMeansResults <- colMeans(meanData[,c(-1,-2)]) 
        newRow <- c(i,j)
        # matrice created by combinig two vectors one with subject and activity; second with means
        meanTidyData[k,] <- c(newRow,colMeansResults)
        k <- k + 1
        
    }
}

# MeanTidyData matrix is converted to dataframe and the activity is converted to character
meanTidyData <- data.frame(meanTidyData)
meanTidyData[,2] <- as.character(meanTidyData[,2])

# activity labels are added to dataframe
for( i in 1:length(meanTidyData[,1])){
    if(meanTidyData[i,2] == activity_labels[1,1] ) meanTidyData[i,2] <- activity_labels[1,2]
    
    if(meanTidyData[i,2] == activity_labels[2,1] ) meanTidyData[i,2] <- activity_labels[2,2]
    
    if(meanTidyData[i,2] == activity_labels[3,1] ) meanTidyData[i,2] <- activity_labels[3,2]
    
    if(meanTidyData[i,2] == activity_labels[4,1] ) meanTidyData[i,2] <- activity_labels[4,2]
    
    if(meanTidyData[i,2] == activity_labels[5,1] ) meanTidyData[i,2] <- activity_labels[5,2]
    
    if(meanTidyData[i,2] == activity_labels[6,1] ) meanTidyData[i,2] <- activity_labels[6,2]    
}

# recover dataframe column names from earlier data frame
dataframe_col_names <-colnames(meanData[,])

# add recovered dataframe column names to calcualted dataframe
names(meanTidyData)<-dataframe_col_names

# Write csv file meanTidyData
write.csv(meanTidyData, file = "MyDataMean.csv")
