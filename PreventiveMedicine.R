#-------------------------------------------------------------------------------
# Loading the required libraries
library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(stringr)
library(tm)
library(syuzhet) 
library(SnowballC)


#-------------------------------------------------------------------------------
# Load the text files into R

cat("Read data")

train_text <- do.call(rbind,strsplit(readLines('training_text'),'||',fixed=T))
train_text <- as.data.table(train_text)
train_text <- train_text[-1,]
colnames(train_text) <- c("ID", "Text")
train_text$ID <- as.numeric(train_text$ID)

test_text <- do.call(rbind,strsplit(readLines('test_text'),'||',fixed=T))
test_text <- as.data.table(test_text)
test_text <- test_text[-1,]
colnames(test_text) <- c("ID", "Text")
test_text$ID <- as.numeric(test_text$ID)

train <- fread("training_variants", sep=",", stringsAsFactors = T)
test <- fread("test_variants", sep=",", stringsAsFactors = T)
train <- merge(train,train_text,by="ID")
test <- merge(test,test_text,by="ID")
rm(test_text,train_text);gc()
test$Class <- -1
data1 <- rbind(train,test)

#-------------------------------------------------------------------------------
# Grouping the factor levels 

library(forcats)
data1$Gene = fct_lump(data1$Gene,20,other_level = 'Others',ties.method = 'max')
data1$Variation = fct_lump(data1$Variation,20,other_level = 'V_Others',ties.method = 'max')
data1$Class =  as.factor(data1$Class)
rm(train,test);gc()
data1$Class = as.factor(data1$Class)
str(data1)

#-------------------------------------------------------------------------------
# TF-IDF and Natural Language Processing

cat("TF-IDF")
data = data1[Class!= -1,]
table(data1$Class)
txt <- Corpus(VectorSource(data$Text))
txt <- tm_map(txt, stripWhitespace)
txt <- tm_map(txt, content_transformer(tolower))
txt <- tm_map(txt, removePunctuation)
txt <- tm_map(txt, removeWords, stopwords("english"))
txt <- tm_map(txt, stemDocument, language="english")
txt <- tm_map(txt, removeNumbers)
txt <- tm_map(txt,function(x)iconv(x, "latin1", "ASCII", sub=""))
dtm <- DocumentTermMatrix(txt, control = list(weighting = weightTfIdf))
dtm <- removeSparseTerms(dtm, 0.95)
data1 <- cbind(data, as.matrix(dtm))
data1 = data1[,-c(5)]
table(data1$Class)

data1$Class = as.numeric(data1$Class)
str(train)
data1$Class = as.factor(data1$Class)
levels(data1$Class) = c(1,2,3,4,5,6,7,8,9)

#-------------------------------------------------------------------------------
# dividing the data into train and test 


ind = sample(2, nrow(data1),replace = T,prob = c(0.7,0.3))
train =  data1[ind == 1,]
test =  data1[ind == 2,]
class(train$Class)
table(data1$Class)


#-------------------------------------------------------------------------------
# Building the classifier

library(e1071)
library(caret)


classifier = svm(Class~., data = train)
predicted = predict(classifier,   newdata =  test)
pred_file = cbind(test,predicted)
cm1 = table(test$Class,predicted)
write.table(pred_file[,c(1:4,3508,5:3507)],paste0('prediction_file_All_Folds.csv'),sep = ',',row.names = TRUE,append = TRUE)
sum(diag(cm1))/sum(cm1)

#-------------------------------------------------------------------------------
#K-Fold cross validation

folds = createFolds(train$Class, k = 9)

cv = lapply(folds, function(x){
  training_set = train[-x,]
  test_set = train[x,]
  classifier_local = svm(Class~., data = training_set)
  class_predicted = predict(classifier_local,   newdata =  test_set)
  pred_file = cbind(test_set,class_predicted)
  write.table(pred_file[,c(1:4,3508,5:3507)],paste0('prediction_file_All_Folds.csv'),sep = ',',row.names = FALSE,append = TRUE)
  cm = table(test_set$Class,class_predicted)
  write.table(cm,paste0('confusion_matrix_All_Folds.csv'),sep = ',',append = TRUE)
  accuracy = sum(diag(cm))/sum(cm)
  return(accuracy)
})

#-------------------------------------------------------------------------------
final_accuracy = mean(as.numeric(cv))

error_rate = 1-final_accuracy






