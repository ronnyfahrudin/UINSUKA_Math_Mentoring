# Decision Tree Machine learning 
#install.packages('ROSE')
#install.packages(c('ISLR','tree','dplyr))
#install.packages("rpart.plot")	
#install.packages('caret')
# memanggil library yang dibutuhkan 
setwd('E:/Karir/Mentoring dan Pemateri/Uinsuka') # setting directory folder
require(tree)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(ROSE)
# membaca datasets
df <- read.csv('churn_analysis.csv')
# head
head(df)

# kasaran data
str(df)

#tail
tail(df)

# descriptive statistics
summary(df)

# check null valus

na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# chect target variable
count_churn <- df %>%
  group_by(Churn) %>%
  summarise(counts = n())

# ploting target variable
ggplot(count_churn, aes(x = Churn, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)

# prosentase
pie(count_churn$counts, 
    labels = paste0(round(100 * count_churn$counts/sum(count_churn$counts), 2), "%"))



# menghapus kolom yang tidak diperlukan
# klom ny : UpdateAt, customerID
df_1 <- df[, !(names(df) %in% c('UpdatedAt','customerID'))]


# fixing imbalance dataset
#df_1 <- ovun.sample(Churn~., data=df,
                                  #p=0.5, seed=1,
                                  #method="over")$data

count_churn <- df_1 %>%
  group_by(Churn) %>%
  summarise(counts = n())
ggplot(count_churn, aes(x = Churn, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3)




# Encoding datasets
# gender, SeniorCitizen, Partner
# PhoneServise, StreamingTV
# InternetService, PaperlessBilling

# membuat function yang values nya yes no
encod_data <- function(columns) {
  outputs <- factor(columns,
                     levels = c('No','Yes'),
                     labels = c(0,1))
  return(outputs)
}
# memulai encoding 
summary(df_1)

df_1$gender <- factor(df_1$gender,
                levels = c('Female','Male'),
                labels = c(0,1))
df_1$SeniorCitizen <- encod_data(df_1$SeniorCitizen)
df_1$Partner <- encod_data(df_1$Partner)
df_1$PhoneService <- encod_data(df_1$PhoneService)
df_1$StreamingTV <- encod_data(df_1$StreamingTV)
df_1$InternetService <- encod_data(df_1$InternetService)
df_1$PaperlessBilling <- encod_data(df_1$PaperlessBilling)
df_1$Churn <- encod_data(df_1$Churn)


# melihat all
head(df_1)

# modelling with DecisionTree

# Memecah ke train dan test set 
# train 70% dan test 30%
View(df_1)

split <- sort(sample(nrow(df_1), nrow(df_1)*.7))
train <- df_1[split,]
test <- df_1[-split,]

# training datanya
res_train <- rpart(Churn~., train, method = 'class')
rpart.plot(res_train)

# testing
p <- predict(res_train, test, type = 'class')
table(test[,11], p)


# confusion matrix report
print(confusionMatrix(test[,11],p))


# roc - AUC
library(pROC)
roc_qda=roc(response=test[,11], 
            predictor= factor(p,
                              ordered = TRUE), plot=TRUE)
plot(roc_qda, col="red", lwd=3, main="ROC curve QDA")
auc_qda<-auc(roc_qda)

roc(response=test[,11], 
    predictor= factor(p,
                      ordered = TRUE), plot=TRUE)
