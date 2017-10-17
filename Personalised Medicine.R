
# Loading the required libraries
library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(tm)
library(e1071)
library(caret)
library(SnowballC)
library(MLmetrics)


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


classifier = svm(Class~., data = train,probability = TRUE)
predicted = predict(classifier,   newdata =  test,probability = TRUE)
predicted1 = data.frame(attr(predicted, "probabilities"))
predicted1 = cbind(test$ID,predicted1)
colnames(predicted1) = c('ID','class2','class3','class4','class1','class5','class6','class7','class9','class8')
predicted1 = predicted1[,c('ID','class1','class2','class3','class4','class5','class6','class7','class8','class9')]
cm1 = table(test$Class,predicted)
write.table(predicted1,paste0('prediction_file.csv'),sep = ',',row.names = FALSE)
accuracy = sum(diag(cm1))/sum(cm1)

#-------------------------------------------------------------------------------
#K-Fold cross validation
# The value of k can be anything between 2 and 10 for this instance i have considered 2 for the sake of experimentation
folds = createFolds(train$Class, k = 2)

cv = lapply(folds, function(x){
  training_set = train[-x,]
  test_set = train[x,]
  classifier_local = svm(Class~., data = training_set,probability = TRUE)
  class_predicted = predict(classifier_local,   newdata =  test_set,probability = TRUE)
  class_predicted1 = data.frame(attr(class_predicted, "probabilities"))
  pred_file = cbind(test_set$ID,class_predicted1)
  colnames(pred_file) = c('ID','class2','class3','class4','class5','class7','class1','class9','class8','class6')
  pred_file = pred_file[,c('ID','class1','class2','class3','class4','class5','class6','class7','class8','class9')]
  write.table(pred_file,paste0('prediction_file_K-FOLD.csv'),sep = ',',row.names = FALSE,append = TRUE)
  cm = table(test_set$Class,class_predicted)
  cm
  accuracy = sum(diag(cm))/sum(cm)
  return(accuracy)
})

#-------------------------------------------------------------------------------
final_accuracy = mean(as.numeric(cv))

error_rate = 1-final_accuracy
