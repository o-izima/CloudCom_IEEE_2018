require(dplyr)
require(glmnet)
require(randomForest)
require(readr)

#this script filters based on the TCP value in the tcpsck column
#it also automates model building

# Reading the file
df <- read_csv('XY.csv')
unique.tcpsck.values <- unique(df['tcpsck'])
unique.tcpsck.values <- unique.tcpsck.values[[1]]
df.list <- list()

# Making dataframes for distinct values of tcpsck
# We iterate through each unique value of tcpsck and filter out the
# rows with that tcpsck value and construct a dataframe out of it, and
# append the dataframe to df.list
for (i in 1:length(unique.tcpsck.values)) {
  df.list[[i]] <- filter(df, tcpsck == unique.tcpsck.values[i])
}

# Filtering further on a minimum occurrence of 500
# Iterate through each dataframe in df.list and check if it has at
# least 500 rows in it. If so, append to filtered.df.list
filter.vector <- c()
for (i in 1:length(df.list)) {
  filter.vector <- append(filter.vector, dim(df.list[[i]])[1] >= 500)
}
filtered.df.list <- df.list[filter.vector]

# Building the baseline Linear Regression models
# Create three lists to hold the different Linear Regression models,
# one for each of the target variables.
no.audio.played.models <- list()
no.rtp.pkts.models <- list()
disp.frames.models <- list()

# Iterate through each dataframe in filtered.df.list, and construct the
# three models as required and append them to the corresponding lists.
for (i in 1:length(filtered.df.list)) {
  no.audio.played.models[[i]] <- lm(noAudioPlayed ~ .,
                                    data = select(filtered.df.list[[i]],
                                                  -c(NoRTPPkts, DispFrames)))
  no.rtp.pkts.models[[i]] <- lm(NoRTPPkts ~ .,
                                data = select(filtered.df.list[[i]],
                                              -c(noAudioPlayed, DispFrames)))
  disp.frames.models[[i]] <- lm(DispFrames ~ .,
                                data = select(filtered.df.list[[i]],
                                              -c(noAudioPlayed, NoRTPPkts)))
}

# Building the elasticnet models - similar as above
# no.audio.played.elasticnet.models <- list()
# no.rtp.pkts.elasticnet.models <- list()
# disp.frames.elasticnet.models <- list()

# for (i in 1:length(filtered.df.list)) {
#   no.audio.played.elasticnet.models[[i]] <- 
#     glmnet(select(filtered.df.list[[i]], -noAudioPlayed),
#            filtered.df.list[[i]]$noAudioPlayed,
#            family="gaussian", alpha=.5)
#   no.rtp.pkts.elasticnet.models[[i]] <- 
#     glmnet(select(filtered.df.list[[i]], -NoRTPPkts),
#            filtered.df.list[[i]]$NoRTPPkts,
#            family="gaussian", alpha=.5)
#   disp.frames.elasticnet.models[[i]] <- 
#     glmnet(select(filtered.df.list[[i]], -DispFrames),
#            filtered.df.list[[i]]$DispFrames,
#            family="gaussian", alpha=.5)
# }

# Building the random forest models - similar as linear regression
no.audio.played.rf.models <- list()
no.rtp.pkts.rf.models <- list()
disp.frames.rf.models <- list()

for (i in 1:length(filtered.df.list)) {
  no.audio.played.rf.models[[i]] <- randomForest(
    noAudioPlayed ~ .,
    data = select(filtered.df.list[[i]], -c(NoRTPPkts, DispFrames)))
  no.rtp.pkts.rf.models[[i]] <- randomForest(NoRTPPkts ~ .,
                                             data = select(filtered.df.list[[i]], -c(noAudioPlayed, DispFrames)))
  disp.frames.rf.models[[i]] <- randomForest(DispFrames ~ .,
                                             data = select(filtered.df.list[[i]], -c(noAudioPlayed, NoRTPPkts)))
}

# Each item in the list is a trained model, that can be accessed and
# used to predict further.