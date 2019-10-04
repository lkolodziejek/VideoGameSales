################################
#          VideoGamesSales Capstone project 2 for online course HarvardX: PH125.9x
#          Author: Lukasz Kolodziejek
#          Date: 25.09.2019
################################

################################
# Analysis
################################

################################
# Data ingestion
################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")

#Enables parallel calculation to better leverage multiple PC cores
split <- detectCores()
split
cl <- makePSOCKcluster(split)
registerDoParallel(cl)


# Dataset comes from Kaggle: www.kaggle.com
# Kaggle VideoGames Sales:
# https://www.kaggle.com/rush4ratio/video-game-sales-with-ratings/download
# As access to file on Kaggle requires authorization for purpose of this project dataset copy has been created on Author's GitHub:
# https://github.com/lkolodziejek/VideoGameSales/blob/master/Video_Games_Sales_as_at_22_Dec_2016.csv

dl <- tempfile()
download.file("https://github.com/lkolodziejek/VideoGameSales/blob/master/Video_Games_Sales_as_at_22_Dec_2016.csv", dl)
games <- fread("Video_Games_Sales_as_at_22_Dec_2016.csv", header=TRUE)

################################
# Data wrangling
################################

games[games==""] <- NA #Fill missing values with NA
games$Year_of_Release <- as.numeric(as.character(games$Year_of_Release))
games$User_Score <- as.numeric(as.character(games$User_Score))*10 #Normalize User_Score to same scale as Critic_Score
games$Genre <- as.factor(games$Genre)
games$Publisher <- as.factor(games$Publisher)
games$Developer <- as.factor(games$Developer)
games$Rating <- as.factor(games$Rating)
games$Platform <- as.factor(games$Platform)
games <- games[-which(games$Year_of_Release>2016)] #As data after 2016 is not complete, remove it

################################
# Data exploration
################################

summary(games)

dim(games)

head(games)

gamesRelease <- games %>% select(Year_of_Release) %>%
  group_by(Year_of_Release)%>% 
  summarise(Count=n())

gamesRelease %>% ggplot(aes(Year_of_Release, Count))+geom_line()+scale_x_continuous(limits=c(1980,2016))+ggtitle("Number of games released per year")

gamesSales <- games %>% select(Year_of_Release,NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(NorthAmerica=sum(NA_Sales), EU=sum(EU_Sales), Japan=sum(JP_Sales), Other=sum(Other_Sales))

gamesSales <- gamesSales %>%
  select(Year_of_Release, NorthAmerica, EU, Japan, Other) %>%
  gather(key = "Market", value = "Sales", -Year_of_Release)

gamesSales %>% ggplot(aes(Year_of_Release, Sales, color=Market))+geom_line()+scale_x_continuous(limits=c(1980,2016))+ggtitle("Sales volume per market")

#gamesUserScore <- games %>% select(User_Score) %>%
# group_by(User_Score)%>% 
#  summarise(Count=n())

games %>% ggplot(aes(User_Score)) + geom_histogram(binwidth = 5)+scale_x_continuous(limits=c(0,100))

games %>% ggplot(aes(Critic_Score)) + geom_histogram(binwidth = 5)+scale_x_continuous(limits=c(0,100))

# Top 10 games with highest sales

games %>% 
  group_by(Name) %>%
  summarize(sumGlobalSales = sum(Global_Sales)) %>%
  arrange(desc(sumGlobalSales)) %>%
  top_n(10)

# Top 10 best rated games by users
games %>% 
  group_by(Name) %>%
  summarize(meanUserScore = mean(User_Score)) %>%
  arrange(desc(meanUserScore)) %>%
  top_n(10)

# Top 10 best rates games by critics
games %>% 
  group_by(Name) %>%
  summarize(meanCriticScore = mean(Critic_Score)) %>%
  arrange(desc(meanCriticScore)) %>%
  top_n(10)

# Correlation between User & Critics rating

games %>%
  group_by(Name) %>%
  summarize(meanUserScore = mean(User_Score), meanCriticScore = mean(Critic_Score)) %>%
  ggplot(aes(meanCriticScore, meanUserScore)) +geom_bin2d() + scale_fill_gradient(
    low = "red",
    high = "yellow") 

cor(games$User_Score, games$Critic_Score, use="complete.obs")

################################
# Looking for correlation with Global Sales
################################

# Sales per year

games %>% select(Year_of_Release, Global_Sales) %>%
  group_by(Year_of_Release)%>% 
  summarise(globalSales = sum(Global_Sales), count=n(), salesPerGameReleased=globalSales/count) %>% 
  ggplot(aes(Year_of_Release, salesPerGameReleased))+geom_line()+scale_x_continuous(limits=c(1980,2016))+ggtitle("Sales per game released in time")

# Genres generating highest sales

genresBySales <- games %>% 
  group_by(Genre) %>%
  summarize(globalSales = sum(Global_Sales), count=n(), salesPerGameReleased=globalSales/count) %>%
  arrange(desc(salesPerGameReleased))

genresBySales <- genresBySales[-which(is.na(genresBySales$Genre)),] #Remove from data NA category of games
games$Genre <- factor(games$Genre, genresBySales$Genre) #Reorder factors to display in descending order

games %>% 
  group_by(Genre) %>%
  summarize(globalSales = sum(Global_Sales), count=n(), salesPerGameReleased=globalSales/count) %>%
  arrange(desc(salesPerGameReleased)) %>%
  ggplot(aes(Genre, salesPerGameReleased)) +geom_bar(stat="identity")+ggtitle("Sales per game released depending on Genre")


# Top 10 platform for which games sales was highest

games %>% 
  group_by(Platform) %>%
  summarize(globalSales = sum(Global_Sales), count=n(), salesPerPlatform=globalSales/count) %>%
  arrange(desc(salesPerPlatform)) 

# Correlation between Global Sales and NA_Sales

games %>%
  group_by(Name) %>%
  ggplot(aes(NA_Sales, Global_Sales)) +geom_bin2d() + scale_fill_gradient(
    low = "red",
    high = "yellow") 

# Correlation between Global Sales and User_Count

games %>%
  group_by(Name) %>%
  ggplot(aes(User_Count, Global_Sales)) +geom_bin2d() + scale_fill_gradient(
    low = "red",
    high = "yellow") 

games <- na.omit(games)
games <- subset(games, select = -c(NA_Sales, EU_Sales, JP_Sales, Other_Sales, User_Count))


################################
# Models
################################

set.seed(1, sample.kind="Rounding")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

validation_index <- createDataPartition(y = games$Global_Sales, times = 1, p = 0.2, list = FALSE)
train <- games[-validation_index,]
validation <- games[validation_index,]


# Simple mean 

mu <- mean(train$Global_Sales)

model_0_rmse <- RMSE(train$Global_Sales, mu)
model_0_rmse 
rmse_results <- tibble(method = "Model 0 - Simple mean", RMSE = model_0_rmse)

# LM 

train_lm <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                       method = "lm", data = train, na.action = na.exclude)

getTrainPerf(train_lm)$TrainRMSE
varImp(train_lm) 


model_1_rmse <- getTrainPerf(train_lm)$TrainRMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 1 - Linear model",
                                     RMSE = model_1_rmse ))


# GLM 

train_glm <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                   method = "glm", data = train, na.action = na.exclude)
train_glm$results
getTrainPerf(train_glm)$TrainRMSE
varImp(train_glm)

model_2_rmse <- getTrainPerf(train_glm)$TrainRMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 2 - Generalized Linear model",
                                     RMSE = model_2_rmse ))

# SVM 

train_svm <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                   method = "svmLinear", data = train, na.action = na.exclude)
getTrainPerf(train_svm)$TrainRMSE
train_svm$results
varImp(train_svm) 

model_3_rmse <- getTrainPerf(train_svm)$TrainRMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 3 - Support Vector Machines with Linear Kernel model",
                                     RMSE = model_3_rmse ))

# knn n

train_knn <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                   method = "knn", data = train, na.action = na.exclude)
getTrainPerf(train_knn)$TrainRMSE
train_knn$results
varImp(train_knn)

model_4_rmse <- getTrainPerf(train_knn)$TrainRMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 4 - k-Nearest Neighbors model",
                                     RMSE = model_4_rmse ))

# rf 

train_rf <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                  method = "rf", data = train, na.action = na.exclude, ntree=50, metric="RMSE", trControl = trainControl(method = "repeatedcv", number=10,repeats=3))
getTrainPerf(train_rf)$TrainRMSE
train_rf$results

model_5_rmse <- getTrainPerf(train_rf)$TrainRMSE
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 5 - Random Forest",
                                     RMSE = model_5_rmse ))

# Tuned RF

mtry = c(5:15)
rf_grid <- data.frame(mtry)
rf_grid
tuning_results <- data.frame(matrix(ncol = length(mtry)+1, nrow=0))
colnames(tuning_results) <- c("ntree", mtry)

modellist <- list()
for (numtree in seq(5,100,by=5)) {
  train_trf <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                     method = "rf", data = train, na.action = na.exclude, ntree=numtree, tuneGrid=rf_grid, metric="RMSE", trControl = trainControl(method = "repeatedcv", number=10,repeats=3))
  tuning_results[nrow(tuning_results) + 1,] = c(numtree, train_trf$results$RMSE)
  key <- toString(numtree)
  modellist[[key]] <- train_trf
}

tuning_results  

# Make dataframe tidy for better presentation
tr <- gather(tuning_results, "mtry", "RMSE", 2:(length(mtry)+1))

# Assign levels to mtry to display mtry legend in right order
tr$mtry <- factor(tr$mtry, levels=mtry)

tr %>% ggplot(aes(ntree, RMSE))+geom_line(aes(color=mtry))+ggtitle("RMSE depending on ntree and mtry parameters")

min(tr$RMSE)
model_6_rmse <- min(tr$RMSE)
tr[which.min(tr$RMSE),]


rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Model 6 - Random Forest After Tuning model",
                                     RMSE = model_6_rmse ))

################################
# Results
################################

rmse_results

#As the best model with are selecting Model 6 - Random Forest After Tuning with 

#Let's calculate validation RMSE

optimal_mtry <- as.numeric(as.character(tr[which.min(tr$RMSE),2]))
optimal_mtry
optimal_ntree <- as.numeric(as.character(tr[which.min(tr$RMSE),1]))
optimal_ntree
optimal_rf_grid <- data.frame(mtry = optimal_mtry)
train_final <- train(Global_Sales ~ Platform + User_Score + Critic_Score + Critic_Count + Genre + Year_of_Release + Rating,
                     method = "rf", data = train, na.action = na.exclude, ntree = optimal_ntree, tuneGrid = optimal_rf_grid, metric="RMSE", trControl = trainControl(method = "repeatedcv", number=10,repeats=3))

predicted <- predict(train_final, newdata = validation)
RMSE(validation$Global_Sales, predicted)


################################
# Conclusion
################################

stopCluster(cl) #stops cluster created for parallel processing