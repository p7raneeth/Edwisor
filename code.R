#---------------------------------------------
# Read data from csv file

data1 = read.csv('TextClassification_Data.csv',stringsAsFactors = F)
install.packages('FSelector')
library(tm)
library(FSelector)
library(rJava)
# data1 = data1

txt1 <- Corpus(VectorSource(data1$SUMMARY))
txt1 <- tm_map(txt1, stripWhitespace)
txt1 <- tm_map(txt1, content_transformer(tolower))
txt1 <- tm_map(txt1, removePunctuation)
txt1 <- tm_map(txt1, removeWords, stopwords("english"))
txt1 <- tm_map(txt1, stemDocument, language="english")
txt1 <- tm_map(txt1, removeNumbers)
txt1 <- tm_map(txt1,function(x)iconv(x, "latin1", "ASCII", sub=""))
dtm1 <- DocumentTermMatrix(txt1, control = list(weighting = weightTfIdf))
dtm1 <- removeSparseTerms(dtm1, 0.95)
data3 <- cbind(data2, as.matrix(dtm1))

names(data3)

dt4 = data3[,-c(1,2,3,7,78)]

dt4$categories =  factor(as.character(dt4$categories))
dt4$sub_categories =  factor(as.character(dt4$sub_categories))


str(dt4$sub_categories)
levels(train$previous_appointment)[2] = "YES"

imp_var = random.forest.importance(categories+sub_categories~., data = dt4)


rownames(imp_var)
important_variables<-rownames(imp_var2345)[1:150]

dt5<-dt4[,important_variables]
data = cbind(dt5,dt4$categories)


data$sub_categories

data$sub_categories =  factor(as.character(data$sub_categories))
data$categories =  factor(as.character(data$categories))
data$previous_appointment =  factor(as.character(data$previous_appointment))


levels(data$previous_appointment)
# [8] = "MEDICATION RELATED"
levels(data)
str()
str(data$categories)
[5] = "MISCELLANEOUS"

names(data)[151] = "categories"

ind = sample(2, nrow(data),replace = T,prob = c(0.75,0.25))
train =  data[ind == 1,]
test =  data[ind == 2,]


train$previous_appointment =  factor(as.character(train$previous_appointment))


library(e1071)
library(caret)

levels(test$previous_appointment)[2] = "NO"
str(test$categories)

model_cat = svm(categories~., train)                    

class_predicted = predict(model_cat,test)

str(test)




pred_file = cbind(train$categories,class_predicted)

cm = data.frame(cm)

cm = table(test$categories,class_predicted)
accuracy = sum(diag(cm))/sum(cm)
summary(model)
